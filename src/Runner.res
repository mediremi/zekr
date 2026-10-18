// Runner - Test execution and reporting

open Types

// Environment variable access for test filtering
@val @scope("process") external env: Dict.t<string> = "env"

let getEnvVar = (name: string): option<string> => {
  env->Dict.get(name)
}

// Test filtering configuration
let filterPattern = ref((None: option<string>))
let skipPattern = ref((None: option<string>))

let setFilterPattern = (pattern: option<string>): unit => {
  filterPattern := pattern
}

let setSkipPattern = (pattern: option<string>): unit => {
  skipPattern := pattern
}

let initFilterFromEnv = (): unit => {
  filterPattern := getEnvVar("ZEKR_FILTER")
  skipPattern := getEnvVar("ZEKR_SKIP")
}

let matchesPattern = (name: string, pattern: string): bool => {
  let lowerName = String.toLowerCase(name)
  let lowerPattern = String.toLowerCase(pattern)
  String.includes(lowerName, lowerPattern)
}

let shouldRunTest = (suiteName: string, testName: string): bool => {
  let fullName = suiteName ++ " " ++ testName

  let skipped = switch skipPattern.contents {
  | Some(pattern) => matchesPattern(fullName, pattern)
  | None => false
  }

  if skipped {
    false
  } else {
    switch filterPattern.contents {
    | Some(pattern) => matchesPattern(fullName, pattern)
    | None => true
    }
  }
}

module NodeUtil = {
  @module("util") external inspect: 'a => string = "inspect"
}

// How ReScript compiles an exception, e.g. `Failure("boom")` is
// {RE_EXN_ID: "Failure", _1: "boom", Error: new Error()}, and one with an
// inline record, `Bad({code: 3})`, is {RE_EXN_ID: Bad, code: 3, Error: ...}
type rescriptException = {
  @as("RE_EXN_ID") id: string,
  @as("Error") error: Nullable.t<JsExn.t>,
}
external asRescriptException: exn => rescriptException = "%identity"
external asDict: exn => dict<unknown> = "%identity"

// The directory zekr's own modules are in, to leave its frames out of stacks
let ownDirectory: string = %raw(`new URL(".", import.meta.url).href`)

// Where a test threw, from a stack: its first frame in the project's own
// code, rather than in zekr, Node or a dependency (e.g. the ReScript runtime's
// JsError.throwWithMessage)
let thrownAt = (stack: option<string>): option<string> =>
  stack
  ->Option.getOr("")
  ->String.split("\n")
  ->Array.map(String.trim)
  ->Array.find(line =>
    String.startsWith(line, "at ") &&
    !String.includes(line, ownDirectory) &&
    !String.includes(line, "/node_modules/") &&
    !String.includes(line, "node:")
  )

// How a test (or a hook, the thrower) that threw an exception is reported: a JS
// error's name and message, or a ReScript exception's name and payload, and
// where it was thrown
let exceptionFailure = (~thrower="Test", exn: exn): testResult => {
  let (description, stack) = switch exn {
  | JsExn(err) =>
    let name = JsExn.name(err)->Option.getOr("Error")
    switch JsExn.message(err) {
    | Some(message) => (`${name}: ${message}`, JsExn.stack(err))
    // Not an Error, e.g. a thrown string
    | None => (NodeUtil.inspect(err), None)
    }
  | _ =>
    let {id, error} = asRescriptException(exn)
    let payload =
      asDict(exn)
      ->Dict.toArray
      ->Array.filter(((key, _value)) => key != "RE_EXN_ID" && key != "Error")
    let positional = payload->Array.every(((key, _value)) => /^_\d+$/->RegExp.test(key))
    let description = switch payload {
    | [] => id
    | _ if positional =>
      `${id}(${payload->Array.map(((_key, value)) => NodeUtil.inspect(value))->Array.join(", ")})`
    | _ => `${id}(${NodeUtil.inspect(Dict.fromArray(payload))})`
    }
    (description, error->Nullable.toOption->Option.flatMap(JsExn.stack))
  }
  let location = switch thrownAt(stack) {
  | Some(frame) => `\n    ${frame}`
  | None => ""
  }
  Fail(`${thrower} threw an exception: ${description}${location}`)
}

// Runs a synchronous test, so that an exception it throws fails that test
// rather than stopping the run (runWithTimeout does the same for async tests)
let runSafely = (run: unit => testResult): testResult =>
  try run() catch {
  | exn => exceptionFailure(exn)
  }

// Runs a hook, so that an exception it throws is a failure, of the tests it was
// for, rather than stopping the run
let runHook = (hook: option<unit => unit>, ~name: string): testResult =>
  switch hook {
  | None => Pass
  | Some(fn) =>
    try {
      fn()
      Pass
    } catch {
    | exn => exceptionFailure(exn, ~thrower=`${name} hook`)
    }
  }

// The result of a promise, or Fail(message) if it takes more than ms
let failAfter = async (result: promise<testResult>, ms: int, ~message: string): testResult => {
  let timer = ref(None)
  let timeout = Promise.make((resolve, _reject) => {
    timer := Some(setTimeout(() => resolve(Fail(message)), ms))
  })
  let result = await Promise.race([result, timeout])
  timer.contents->Option.forEach(clearTimeout)
  result
}

// How long an async hook can take, in milliseconds: beforeAll and afterAll,
// and beforeEach and afterEach when their test has no timeout. Ten seconds,
// ava's default.
let hookTimeout = ref(10000)

let setHookTimeout = (ms: int): unit => {
  hookTimeout := ms
}

// runHook, for an async hook, which fails if it takes longer than `timeout`
// (or hookTimeout) rather than leaving the file hanging
let runAsyncHook = async (
  hook: option<unit => promise<unit>>,
  ~name: string,
  ~timeout: option<int>=?,
): testResult =>
  switch hook {
  | None => Pass
  | Some(fn) =>
    let run = async () =>
      try {
        await fn()
        Pass
      } catch {
      | exn => exceptionFailure(exn, ~thrower=`${name} hook`)
      }
    let ms = timeout->Option.getOr(hookTimeout.contents)
    await failAfter(run(), ms, ~message=`${name} hook timed out after ${Int.toString(ms)}ms`)
  }

// A test's result once its afterEach hook has run: a failing afterEach fails
// the test, and adds to the test's own failure
let withAfterEach = (result: testResult, afterEach: testResult): testResult =>
  switch (result, afterEach) {
  | (Pass, _) => afterEach
  | (Fail(message), Fail(hookMessage)) => Fail(`${message}\n    ${hookMessage}`)
  | (Fail(_), Pass) => result
  }

// Runs a test between its suite's beforeEach and afterEach hooks, as ava does:
// a failed beforeAll fails the test, and a failed beforeEach fails it without
// running it. afterEach still runs then, to clean up after beforeEach.
let runTest = (testCase: testCase, ~hooks: option<hooks>, ~beforeAll: testResult): testResult =>
  switch beforeAll {
  | Fail(_) => beforeAll
  | Pass =>
    let result = switch runHook(hooks->Option.flatMap(h => h.beforeEach), ~name="beforeEach") {
    | Pass => runSafely(testCase.run)
    | failure => failure
    }
    withAfterEach(result, runHook(hooks->Option.flatMap(h => h.afterEach), ~name="afterEach"))
  }

// Whether a test runs by its mode: when any test is `only`, just those do
let runsByMode = (mode: testMode, ~hasOnly: bool): bool =>
  switch mode {
  | Skip => false
  | Only => true
  | Normal => !hasOnly
  }

// A suite's hooks, unless none of its tests run: then, as in ava, its hooks
// don't either, so that a suite nobody selected can't fail the run
let hooksIfAnyRuns = (hooks: option<'hooks>, tests: array<'test>, runs: 'test => bool) =>
  tests->Array.some(runs) ? hooks : None

let runSuite = (testSuite: testSuite): unit => {
  Console.log(`\n ${Colors.suite(`Running test suite: ${testSuite.name}`)}`)
  Console.log(Colors.dimmed("=" ++ String.repeat("-", String.length(testSuite.name) + 23)))

  let passed = ref(0)
  let failed = ref(0)
  let skipped = ref(0)

  // Check if there are any "Only" tests
  let hasOnly = testSuite.tests->Array.some(tc => tc.mode == Only)

  let hooks = hooksIfAnyRuns(testSuite.hooks, testSuite.tests, (tc: testCase) =>
    runsByMode(tc.mode, ~hasOnly)
  )

  // Run beforeAll hook if present
  let beforeAll = runHook(hooks->Option.flatMap(h => h.beforeAll), ~name="beforeAll")

  testSuite.tests->Array.forEach(testCase => {
    if runsByMode(testCase.mode, ~hasOnly) {
      switch runTest(testCase, ~hooks, ~beforeAll) {
      | Pass => {
          Console.log(`  ${Colors.pass("✓")} ${testCase.name}`)
          passed := passed.contents + 1
        }
      | Fail(message) => {
          Console.log(`  ${Colors.fail("✗")} ${testCase.name}`)
          Console.log(`    ${Colors.fail(message)}`)
          failed := failed.contents + 1
        }
      }
    } else {
      Console.log(
        `  ${Colors.skip("○")} ${Colors.skip(testCase.name)} ${Colors.dimmed(
            "(skipped)",
          )}`,
      )
      skipped := skipped.contents + 1
    }
  })

  // Run afterAll hook if present
  switch runHook(hooks->Option.flatMap(h => h.afterAll), ~name="afterAll") {
  | Pass => ()
  | Fail(message) => {
      Console.log(`  ${Colors.fail("✗")} afterAll`)
      Console.log(`    ${Colors.fail(message)}`)
      failed := failed.contents + 1
    }
  }

  Console.log("")
  let skipMsg = if skipped.contents > 0 {
    `, ${Colors.skip(Int.toString(skipped.contents) ++ " skipped")}`
  } else {
    ""
  }
  Console.log(
    `Results: ${Colors.pass(Int.toString(passed.contents) ++ " passed")}, ${Colors.fail(
        Int.toString(failed.contents) ++ " failed",
      )}${skipMsg}`,
  )

  if failed.contents > 0 {
    Console.log(Colors.fail(" Some tests failed"))
  } else {
    Console.log(Colors.pass(" All tests passed!"))
  }
}

// Helper to run async test with timeout and error handling
let runWithTimeout = async (run: unit => promise<testResult>, timeout: option<int>): testResult => {
  let testPromise = async () => {
    try {
      await run()
    } catch {
    | exn => exceptionFailure(exn)
    }
  }

  switch timeout {
  | None => await testPromise()
  | Some(ms) =>
    await failAfter(testPromise(), ms, ~message=`Test timed out after ${Int.toString(ms)}ms`)
  }
}

// runTest, for an async test and hooks
let runAsyncTest = async (
  testCase: asyncTestCase,
  ~hooks: option<asyncHooks>,
  ~beforeAll: testResult,
): testResult =>
  switch beforeAll {
  | Fail(_) => beforeAll
  | Pass =>
    let result = switch await runAsyncHook(
      hooks->Option.flatMap(h => h.beforeEach),
      ~name="beforeEach",
      ~timeout=?testCase.timeout,
    ) {
    | Pass => await runWithTimeout(testCase.run, testCase.timeout)
    | failure => failure
    }
    withAfterEach(
      result,
      await runAsyncHook(
        hooks->Option.flatMap(h => h.afterEach),
        ~name="afterEach",
        ~timeout=?testCase.timeout,
      ),
    )
  }

let printTotal = (~passed: int, ~failed: int, ~skipped: int, ~filtered: int): unit => {
  Console.log("\n" ++ Colors.dimmed(String.repeat("=", 50)))
  let totalSkipMsg = if skipped > 0 {
    `, ${Colors.skip(Int.toString(skipped) ++ " skipped")}`
  } else {
    ""
  }
  let totalFilterMsg = if filtered > 0 {
    `, ${Colors.dimmed(Int.toString(filtered) ++ " filtered")}`
  } else {
    ""
  }
  Console.log(
    `Total: ${Colors.pass(Int.toString(passed) ++ " passed")}, ${Colors.fail(
        Int.toString(failed) ++ " failed",
      )}${totalSkipMsg}${totalFilterMsg}`,
  )
}

let finishExit = (~failed: int): unit => {
  if failed > 0 {
    Console.log(Colors.fail(" Some tests failed\n"))
    %raw(`process.exit(1)`)
  } else {
    Console.log(Colors.pass(" All tests passed!\n"))
    %raw(`process.exit(0)`)
  }
}

let runSuiteAccumulate = (testSuite: testSuite, ~hasOnly: bool): (int, int, int, int) => {
  Console.log(`\n ${Colors.suite(testSuite.name)}`)
  Console.log(Colors.dimmed("-" ++ String.repeat("-", String.length(testSuite.name) + 3)))

  let hooks = hooksIfAnyRuns(testSuite.hooks, testSuite.tests, (tc: testCase) =>
    shouldRunTest(testSuite.name, tc.name) && runsByMode(tc.mode, ~hasOnly)
  )
  let beforeAll = runHook(hooks->Option.flatMap(h => h.beforeAll), ~name="beforeAll")

  let suitePassed = ref(0)
  let suiteFailed = ref(0)
  let suiteSkipped = ref(0)
  let suiteFiltered = ref(0)

  testSuite.tests->Array.forEach(testCase => {
    let passesFilter = shouldRunTest(testSuite.name, testCase.name)
    let shouldRun = passesFilter && runsByMode(testCase.mode, ~hasOnly)

    if !passesFilter {
      suiteFiltered := suiteFiltered.contents + 1
    } else if shouldRun {
      switch runTest(testCase, ~hooks, ~beforeAll) {
      | Pass => {
          Console.log(`   ${Colors.pass("✓")} ${testCase.name}`)
          suitePassed := suitePassed.contents + 1
        }
      | Fail(message) => {
          Console.log(`   ${Colors.fail("✗")} ${testCase.name}`)
          Console.log(`     ${Colors.fail(message)}`)
          suiteFailed := suiteFailed.contents + 1
        }
      }
    } else {
      Console.log(
        `   ${Colors.skip("○")} ${Colors.skip(testCase.name)} ${Colors.dimmed("(skipped)")}`,
      )
      suiteSkipped := suiteSkipped.contents + 1
    }
  })

  switch runHook(hooks->Option.flatMap(h => h.afterAll), ~name="afterAll") {
  | Pass => ()
  | Fail(message) => {
      Console.log(`   ${Colors.fail("✗")} afterAll`)
      Console.log(`     ${Colors.fail(message)}`)
      suiteFailed := suiteFailed.contents + 1
    }
  }

  let skipMsg = if suiteSkipped.contents > 0 {
    `, ${Colors.skip(Int.toString(suiteSkipped.contents) ++ " skipped")}`
  } else {
    ""
  }
  Console.log(
    `  ${Colors.pass(Int.toString(suitePassed.contents) ++ " passed")}, ${Colors.fail(
        Int.toString(suiteFailed.contents) ++ " failed",
      )}${skipMsg}`,
  )

  (suitePassed.contents, suiteFailed.contents, suiteSkipped.contents, suiteFiltered.contents)
}

let runAsyncSuiteAccumulate = async (
  asyncSuite: asyncTestSuite,
  ~hasOnly: bool,
): (int, int, int, int) => {
  Console.log(`\n ${Colors.suite(asyncSuite.name)}`)
  Console.log(Colors.dimmed("-" ++ String.repeat("-", String.length(asyncSuite.name) + 3)))

  let hooks = hooksIfAnyRuns(asyncSuite.hooks, asyncSuite.tests, (tc: asyncTestCase) =>
    shouldRunTest(asyncSuite.name, tc.name) && runsByMode(tc.mode, ~hasOnly)
  )
  let beforeAll = await runAsyncHook(hooks->Option.flatMap(h => h.beforeAll), ~name="beforeAll")

  let suitePassed = ref(0)
  let suiteFailed = ref(0)
  let suiteSkipped = ref(0)
  let suiteFiltered = ref(0)

  for j in 0 to Array.length(asyncSuite.tests) - 1 {
    let testCase = asyncSuite.tests->Array.getUnsafe(j)
    let passesFilter = shouldRunTest(asyncSuite.name, testCase.name)
    let shouldRun = passesFilter && runsByMode(testCase.mode, ~hasOnly)

    if !passesFilter {
      suiteFiltered := suiteFiltered.contents + 1
    } else if shouldRun {
      let result = await runAsyncTest(testCase, ~hooks, ~beforeAll)
      switch result {
      | Pass => {
          Console.log(`   ${Colors.pass("✓")} ${testCase.name}`)
          suitePassed := suitePassed.contents + 1
        }
      | Fail(message) => {
          Console.log(`   ${Colors.fail("✗")} ${testCase.name}`)
          Console.log(`     ${Colors.fail(message)}`)
          suiteFailed := suiteFailed.contents + 1
        }
      }
    } else {
      Console.log(
        `   ${Colors.skip("○")} ${Colors.skip(testCase.name)} ${Colors.dimmed("(skipped)")}`,
      )
      suiteSkipped := suiteSkipped.contents + 1
    }
  }

  switch await runAsyncHook(hooks->Option.flatMap(h => h.afterAll), ~name="afterAll") {
  | Pass => ()
  | Fail(message) => {
      Console.log(`   ${Colors.fail("✗")} afterAll`)
      Console.log(`     ${Colors.fail(message)}`)
      suiteFailed := suiteFailed.contents + 1
    }
  }

  let skipMsg = if suiteSkipped.contents > 0 {
    `, ${Colors.skip(Int.toString(suiteSkipped.contents) ++ " skipped")}`
  } else {
    ""
  }
  Console.log(
    `  ${Colors.pass(Int.toString(suitePassed.contents) ++ " passed")}, ${Colors.fail(
        Int.toString(suiteFailed.contents) ++ " failed",
      )}${skipMsg}`,
  )

  (suitePassed.contents, suiteFailed.contents, suiteSkipped.contents, suiteFiltered.contents)
}

let runSuites = (suites: array<testSuite>): unit => {
  initFilterFromEnv()

  Console.log(`\n ${Colors.suite("Running all test suites")}`)
  Console.log(Colors.dimmed("========================\n"))

  switch filterPattern.contents {
  | Some(p) => Console.log(Colors.dimmed(`Filter: "${p}"`))
  | None => ()
  }
  switch skipPattern.contents {
  | Some(p) => Console.log(Colors.dimmed(`Skip: "${p}"`))
  | None => ()
  }

  let totalPassed = ref(0)
  let totalFailed = ref(0)
  let totalSkipped = ref(0)
  let totalFiltered = ref(0)

  let hasOnly = suites->Array.some(s => s.tests->Array.some(tc => tc.mode == Only))

  suites->Array.forEach(testSuite => {
    let (p, f, s, fl) = runSuiteAccumulate(testSuite, ~hasOnly)
    totalPassed := totalPassed.contents + p
    totalFailed := totalFailed.contents + f
    totalSkipped := totalSkipped.contents + s
    totalFiltered := totalFiltered.contents + fl
  })

  printTotal(
    ~passed=totalPassed.contents,
    ~failed=totalFailed.contents,
    ~skipped=totalSkipped.contents,
    ~filtered=totalFiltered.contents,
  )
  finishExit(~failed=totalFailed.contents)
}

let runAsyncSuite = async (asyncSuite: asyncTestSuite): unit => {
  Console.log(`\n ${Colors.suite(`Running async test suite: ${asyncSuite.name}`)}`)
  Console.log(Colors.dimmed("=" ++ String.repeat("-", String.length(asyncSuite.name) + 29)))

  let passed = ref(0)
  let failed = ref(0)
  let skipped = ref(0)

  // Check if there are any "Only" tests
  let hasOnly = asyncSuite.tests->Array.some(tc => tc.mode == Only)

  let hooks = hooksIfAnyRuns(asyncSuite.hooks, asyncSuite.tests, (tc: asyncTestCase) =>
    runsByMode(tc.mode, ~hasOnly)
  )

  // Run beforeAll hook if present
  let beforeAll = await runAsyncHook(hooks->Option.flatMap(h => h.beforeAll), ~name="beforeAll")

  for i in 0 to Array.length(asyncSuite.tests) - 1 {
    let testCase = asyncSuite.tests->Array.getUnsafe(i)

    if runsByMode(testCase.mode, ~hasOnly) {
      let result = await runAsyncTest(testCase, ~hooks, ~beforeAll)
      switch result {
      | Pass => {
          Console.log(`  ${Colors.pass("✓")} ${testCase.name}`)
          passed := passed.contents + 1
        }
      | Fail(message) => {
          Console.log(`  ${Colors.fail("✗")} ${testCase.name}`)
          Console.log(`    ${Colors.fail(message)}`)
          failed := failed.contents + 1
        }
      }
    } else {
      Console.log(
        `  ${Colors.skip("○")} ${Colors.skip(testCase.name)} ${Colors.dimmed(
            "(skipped)",
          )}`,
      )
      skipped := skipped.contents + 1
    }
  }

  // Run afterAll hook if present
  switch await runAsyncHook(hooks->Option.flatMap(h => h.afterAll), ~name="afterAll") {
  | Pass => ()
  | Fail(message) => {
      Console.log(`  ${Colors.fail("✗")} afterAll`)
      Console.log(`    ${Colors.fail(message)}`)
      failed := failed.contents + 1
    }
  }

  Console.log("")
  let skipMsg = if skipped.contents > 0 {
    `, ${Colors.skip(Int.toString(skipped.contents) ++ " skipped")}`
  } else {
    ""
  }
  Console.log(
    `Results: ${Colors.pass(Int.toString(passed.contents) ++ " passed")}, ${Colors.fail(
        Int.toString(failed.contents) ++ " failed",
      )}${skipMsg}`,
  )

  if failed.contents > 0 {
    Console.log(Colors.fail(" Some tests failed"))
  } else {
    Console.log(Colors.pass(" All tests passed!"))
  }
}

let runAsyncSuites = async (suites: array<asyncTestSuite>): unit => {
  initFilterFromEnv()

  Console.log(`\n ${Colors.suite("Running all async test suites")}`)
  Console.log(Colors.dimmed("==============================\n"))

  switch filterPattern.contents {
  | Some(p) => Console.log(Colors.dimmed(`Filter: "${p}"`))
  | None => ()
  }
  switch skipPattern.contents {
  | Some(p) => Console.log(Colors.dimmed(`Skip: "${p}"`))
  | None => ()
  }

  let totalPassed = ref(0)
  let totalFailed = ref(0)
  let totalSkipped = ref(0)
  let totalFiltered = ref(0)

  let hasOnly = suites->Array.some(s => s.tests->Array.some(tc => tc.mode == Only))

  for i in 0 to Array.length(suites) - 1 {
    let asyncSuite = suites->Array.getUnsafe(i)
    let (p, f, s, fl) = await runAsyncSuiteAccumulate(asyncSuite, ~hasOnly)
    totalPassed := totalPassed.contents + p
    totalFailed := totalFailed.contents + f
    totalSkipped := totalSkipped.contents + s
    totalFiltered := totalFiltered.contents + fl
  }

  printTotal(
    ~passed=totalPassed.contents,
    ~failed=totalFailed.contents,
    ~skipped=totalSkipped.contents,
    ~filtered=totalFiltered.contents,
  )
  finishExit(~failed=totalFailed.contents)
}

let run = async (): unit => {
  initFilterFromEnv()
  let (syncSuites, asyncSuites) = Registry.snapshot()

  Console.log(`\n ${Colors.suite("Running all test suites")}`)
  Console.log(Colors.dimmed("========================\n"))

  switch filterPattern.contents {
  | Some(p) => Console.log(Colors.dimmed(`Filter: "${p}"`))
  | None => ()
  }
  switch skipPattern.contents {
  | Some(p) => Console.log(Colors.dimmed(`Skip: "${p}"`))
  | None => ()
  }

  let totalPassed = ref(0)
  let totalFailed = ref(0)
  let totalSkipped = ref(0)
  let totalFiltered = ref(0)

  let hasOnly =
    syncSuites->Array.some(s => s.tests->Array.some(tc => tc.mode == Only)) ||
      asyncSuites->Array.some(s => s.tests->Array.some(tc => tc.mode == Only))

  syncSuites->Array.forEach(suite => {
    let (p, f, s, fl) = runSuiteAccumulate(suite, ~hasOnly)
    totalPassed := totalPassed.contents + p
    totalFailed := totalFailed.contents + f
    totalSkipped := totalSkipped.contents + s
    totalFiltered := totalFiltered.contents + fl
  })

  for i in 0 to Array.length(asyncSuites) - 1 {
    let suite = asyncSuites->Array.getUnsafe(i)
    let (p, f, s, fl) = await runAsyncSuiteAccumulate(suite, ~hasOnly)
    totalPassed := totalPassed.contents + p
    totalFailed := totalFailed.contents + f
    totalSkipped := totalSkipped.contents + s
    totalFiltered := totalFiltered.contents + fl
  }

  printTotal(
    ~passed=totalPassed.contents,
    ~failed=totalFailed.contents,
    ~skipped=totalSkipped.contents,
    ~filtered=totalFiltered.contents,
  )
  finishExit(~failed=totalFailed.contents)
}

// Watch mode
module NodeChildProcess = {
  type spawnResult = {status: Nullable.t<int>}
  @module("child_process")
  external spawnSync: (string, array<string>, {"stdio": string}) => spawnResult = "spawnSync"
}

let watchMode = (
  ~testCommand: string,
  ~watchPaths: array<string>,
  ~buildCommand: option<string>=?,
): unit => {
  let lastRun = ref(Date.now())
  let debounceMs = 100.0

  let runTests = () => {
    Console.log("\n" ++ Colors.dimmed(String.repeat("=", 50)))
    Console.log(Colors.suite(" File change detected, re-running tests..."))
    Console.log(Colors.dimmed(String.repeat("=", 50)))

    // Run build command if provided
    switch buildCommand {
    | Some(cmd) => {
        let parts = cmd->String.split(" ")
        let command = parts->Array.get(0)->Option.getOr("echo")
        let args = parts->Array.slice(~start=1)
        let _ = NodeChildProcess.spawnSync(command, args, {"stdio": "inherit"})
      }
    | None => ()
    }

    // Run test command
    let parts = testCommand->String.split(" ")
    let command = parts->Array.get(0)->Option.getOr("node")
    let args = parts->Array.slice(~start=1)
    let _ = NodeChildProcess.spawnSync(command, args, {"stdio": "inherit"})
  }

  let onChange = (_eventType: string, _filename: string) => {
    let now = Date.now()
    if now -. lastRun.contents > debounceMs {
      lastRun := now
      runTests()
    }
  }

  Console.log(Colors.suite("\n Watch mode started"))
  Console.log(Colors.dimmed(" Watching for file changes..."))
  Console.log(Colors.dimmed(" Paths: " ++ watchPaths->Array.join(", ")))
  Console.log(Colors.dimmed(" Press Ctrl+C to stop\n"))

  // Initial test run
  runTests()

  // Watch each path
  watchPaths->Array.forEach(path => {
    if Snapshot.NodeFs.existsSync(path) {
      Snapshot.NodeFs.watch(path, {"recursive": true}, onChange)
    } else {
      Console.log(Colors.fail(" Warning: Path \"" ++ path ++ "\" does not exist"))
    }
  })
}
