open Types

exception Invalid(string, int)
exception Bad({field: string, code: int})

let syncHelperTests = Suite.make(
  "Runner accumulate helpers (sync)",
  [
    Test.make("runSuiteAccumulate counts pass, fail, and skip", () => {
      Runner.setFilterPattern(None)
      Runner.setSkipPattern(None)
      let suite: testSuite = {
        name: "Sample",
        tests: [
          {name: "p", run: () => Pass, mode: Normal},
          {name: "f", run: () => Fail("boom"), mode: Normal},
          {name: "s", run: () => Pass, mode: Skip},
        ],
        hooks: None,
      }
      let (passed, failed, skipped, filtered) = Runner.runSuiteAccumulate(suite, ~hasOnly=false)
      if passed == 1 && failed == 1 && skipped == 1 && filtered == 0 {
        Pass
      } else {
        Fail("Expected (1,1,1,0)")
      }
    }),
    Test.make("runSafely turns an exception into a failure", () => {
      switch Runner.runSafely(() => JsError.throwWithMessage("boom")) {
      | Fail(msg) if String.includes(msg, "boom") => Pass
      | _ => Fail("Expected a failure with the exception's message")
      }
    }),
    Test.make("names a ReScript exception, with its payload", () =>
      switch Runner.runSafely(() => throw(Invalid("age", -1))) {
      | Fail(msg) => Assert.contains(msg, "Invalid('age', -1)")
      | Pass => Fail("Expected Fail")
      }
    ),
    Test.make("names a ReScript exception, with an inline record payload", () =>
      switch Runner.runSafely(() => throw(Bad({field: "age", code: 3}))) {
      | Fail(msg) => Assert.contains(msg, "Bad({ field: 'age', code: 3 })")
      | Pass => Fail("Expected Fail")
      }
    ),
    Test.make("names a ReScript exception without a payload", () =>
      switch Runner.runSafely(() => throw(Not_found)) {
      | Fail(msg) => Assert.contains(msg, "Test threw an exception: Not_found")
      | Pass => Fail("Expected Fail")
      }
    ),
    Test.make("gives a JS error's name", () =>
      switch Runner.runSafely(() => JsError.TypeError.throwWithMessage("x is undefined")) {
      | Fail(msg) => Assert.contains(msg, "TypeError: x is undefined")
      | Pass => Fail("Expected Fail")
      }
    ),
    Test.make("says where the exception was thrown, outside zekr", () =>
      switch (
        Runner.runSafely(() => throw(Not_found)),
        Runner.runSafely(() => JsError.throwWithMessage("boom")),
      ) {
      | (Fail(rescript), Fail(js)) =>
        Assert.combineResults([
          Assert.contains(rescript, "RunnerHelpers.test.js:"),
          Assert.contains(js, "RunnerHelpers.test.js:"),
          Assert.isFalse(String.includes(js, "/src/Runner.js")),
        ])
      | _ => Fail("Expected two failures")
      }
    ),
    Test.make("runSuiteAccumulate runs the tests after one that throws", () => {
      Runner.setFilterPattern(None)
      Runner.setSkipPattern(None)
      let afterEachRuns = ref(0)
      let suite: testSuite = {
        name: "Throwing",
        tests: [
          {name: "throws", run: () => JsError.throwWithMessage("boom"), mode: Normal},
          {name: "p", run: () => Pass, mode: Normal},
        ],
        hooks: Some({
          beforeAll: None,
          afterAll: None,
          beforeEach: None,
          afterEach: Some(() => afterEachRuns := afterEachRuns.contents + 1),
        }),
      }
      let (passed, failed, _skipped, _filtered) = Runner.runSuiteAccumulate(suite, ~hasOnly=false)
      if passed == 1 && failed == 1 && afterEachRuns.contents == 2 {
        Pass
      } else {
        Fail("Expected 1 passed, 1 failed and afterEach to run twice")
      }
    }),
    Test.make("runSuite runs the tests after one that throws", () => {
      let ran = ref(false)
      Runner.runSuite({
        name: "Throwing",
        tests: [
          {name: "throws", run: () => JsError.throwWithMessage("boom"), mode: Normal},
          {
            name: "p",
            run: () => {
              ran := true
              Pass
            },
            mode: Normal,
          },
        ],
        hooks: None,
      })
      Assert.isTrue(ran.contents, ~message="Expected the test after the throwing one to run")
    }),
    Test.make("a beforeEach that throws fails its test, which doesn't run", () => {
      Runner.setFilterPattern(None)
      Runner.setSkipPattern(None)
      let ran = ref(false)
      let afterEachRuns = ref(0)
      let suite: testSuite = {
        name: "Throwing beforeEach",
        tests: [
          {
            name: "t",
            run: () => {
              ran := true
              Pass
            },
            mode: Normal,
          },
        ],
        hooks: Some({
          beforeAll: None,
          afterAll: None,
          beforeEach: Some(() => JsError.throwWithMessage("no db")),
          afterEach: Some(() => afterEachRuns := afterEachRuns.contents + 1),
        }),
      }
      let (passed, failed, _skipped, _filtered) = Runner.runSuiteAccumulate(suite, ~hasOnly=false)
      let afterEachRan = afterEachRuns.contents == 1
      Assert.combineResults([
        Assert.equal((passed, failed), (0, 1)),
        Assert.isFalse(ran.contents, ~message="Expected the test not to run"),
        Assert.isTrue(afterEachRan, ~message="Expected afterEach to clean up"),
        switch Runner.runTest(
          suite.tests->Array.getUnsafe(0),
          ~hooks=suite.hooks,
          ~beforeAll=Pass,
        ) {
        | Fail(msg) => Assert.contains(msg, "beforeEach hook threw an exception: Error: no db")
        | Pass => Fail("Expected Fail")
        },
      ])
    }),
    Test.make("an afterEach that throws fails its test", () => {
      Runner.setFilterPattern(None)
      Runner.setSkipPattern(None)
      let suite: testSuite = {
        name: "Throwing afterEach",
        tests: [
          {name: "p", run: () => Pass, mode: Normal},
          {name: "f", run: () => Fail("boom"), mode: Normal},
        ],
        hooks: Some({
          beforeAll: None,
          afterAll: None,
          beforeEach: None,
          afterEach: Some(() => throw(Not_found)),
        }),
      }
      let (passed, failed, _skipped, _filtered) = Runner.runSuiteAccumulate(suite, ~hasOnly=false)
      let hooks = suite.hooks
      Assert.combineResults([
        Assert.equal((passed, failed), (0, 2)),
        switch Runner.runTest(suite.tests->Array.getUnsafe(0), ~hooks, ~beforeAll=Pass) {
        | Fail(msg) => Assert.contains(msg, "afterEach hook threw an exception: Not_found")
        | Pass => Fail("Expected Fail")
        },
        // The test's own failure comes first
        switch Runner.runTest(suite.tests->Array.getUnsafe(1), ~hooks, ~beforeAll=Pass) {
        | Fail(msg) => Assert.isTrue(String.startsWith(msg, "boom\n    afterEach hook threw"))
        | Pass => Fail("Expected Fail")
        },
      ])
    }),
    Test.make("a beforeAll that throws fails every test in the suite", () => {
      Runner.setFilterPattern(None)
      Runner.setSkipPattern(None)
      let ran = ref(0)
      let run = () => {
        ran := ran.contents + 1
        Pass
      }
      let suite: testSuite = {
        name: "Throwing beforeAll",
        tests: [
          {name: "a", run, mode: Normal},
          {name: "b", run, mode: Normal},
          {name: "s", run, mode: Skip},
        ],
        hooks: Some({
          beforeAll: Some(() => throw(Invalid("port", 0))),
          afterAll: None,
          beforeEach: None,
          afterEach: None,
        }),
      }
      let (passed, failed, skipped, _filtered) = Runner.runSuiteAccumulate(suite, ~hasOnly=false)
      Assert.combineResults([
        Assert.equal((passed, failed, skipped), (0, 2, 1)),
        Assert.equal(ran.contents, 0, ~message="Expected no test to run"),
        switch Runner.runTest(
          suite.tests->Array.getUnsafe(0),
          ~hooks=suite.hooks,
          ~beforeAll=Runner.runHook(
            suite.hooks->Option.flatMap(h => h.beforeAll),
            ~name="beforeAll",
          ),
        ) {
        | Fail(msg) => Assert.contains(msg, "Invalid('port', 0)")
        | Pass => Fail("Expected Fail")
        },
      ])
    }),
    Test.make("an afterAll that throws counts as a failure", () => {
      Runner.setFilterPattern(None)
      Runner.setSkipPattern(None)
      let suite: testSuite = {
        name: "Throwing afterAll",
        tests: [{name: "p", run: () => Pass, mode: Normal}],
        hooks: Some({
          beforeAll: None,
          afterAll: Some(() => JsError.throwWithMessage("still open")),
          beforeEach: None,
          afterEach: None,
        }),
      }
      let (passed, failed, _skipped, _filtered) = Runner.runSuiteAccumulate(suite, ~hasOnly=false)
      Assert.equal((passed, failed), (1, 1))
    }),
    Test.make("a suite none of whose tests run doesn't run its hooks", () => {
      let hookRuns = ref(0)
      let hook = () => {
        hookRuns := hookRuns.contents + 1
        JsError.throwWithMessage("boom")
      }
      let suite: testSuite = {
        name: "Unselected",
        tests: [
          {name: "a", run: () => Pass, mode: Normal},
          {name: "s", run: () => Pass, mode: Skip},
        ],
        hooks: Some({
          beforeAll: Some(hook),
          afterAll: Some(hook),
          beforeEach: Some(hook),
          afterEach: Some(hook),
        }),
      }
      Runner.setSkipPattern(None)
      Runner.setFilterPattern(Some("something else"))
      let filtered = Runner.runSuiteAccumulate(suite, ~hasOnly=false)
      Runner.setFilterPattern(None)
      // Another suite has a test with `only`
      let otherOnly = Runner.runSuiteAccumulate(suite, ~hasOnly=true)
      Runner.runSuite({...suite, tests: [{name: "s", run: () => Pass, mode: Skip}]})
      Assert.combineResults([
        Assert.equal(filtered, (0, 0, 0, 2)),
        Assert.equal(otherOnly, (0, 0, 2, 0)),
        Assert.equal(hookRuns.contents, 0, ~message="Expected no hook to run"),
      ])
    }),
    Test.make("runSuite carries on past hooks that throw", () => {
      let ran = ref(false)
      Runner.runSuite({
        name: "Throwing hooks",
        tests: [
          {
            name: "p",
            run: () => {
              ran := true
              Pass
            },
            mode: Normal,
          },
        ],
        hooks: Some({
          beforeAll: None,
          afterAll: Some(() => JsError.throwWithMessage("boom")),
          beforeEach: None,
          afterEach: Some(() => JsError.throwWithMessage("boom")),
        }),
      })
      Assert.isTrue(ran.contents)
    }),
  ],
)

let asyncHelperTests = Suite.async(
  "Runner accumulate helpers (async)",
  [
    Test.async("runAsyncSuiteAccumulate counts pass and fail", async () => {
      Runner.setFilterPattern(None)
      Runner.setSkipPattern(None)
      let suite: asyncTestSuite = {
        name: "SampleAsync",
        tests: [
          {name: "p", run: async () => Pass, mode: Normal, timeout: None},
          {name: "f", run: async () => Fail("boom"), mode: Normal, timeout: None},
        ],
        hooks: None,
      }
      let (passed, failed, skipped, filtered) = await Runner.runAsyncSuiteAccumulate(
        suite,
        ~hasOnly=false,
      )
      if passed == 1 && failed == 1 && skipped == 0 && filtered == 0 {
        Pass
      } else {
        Fail("Expected (1,1,0,0)")
      }
    }),
    Test.async("runAsyncSuiteAccumulate fails the tests whose hooks reject", async () => {
      Runner.setFilterPattern(None)
      Runner.setSkipPattern(None)
      let ran = ref(false)
      let beforeEachRuns = ref(0)
      let suite: asyncTestSuite = {
        name: "Rejecting hooks",
        tests: [
          {
            name: "skipped by beforeEach",
            run: async () => {
              ran := true
              Pass
            },
            mode: Normal,
            timeout: None,
          },
          {name: "failed by afterEach", run: async () => Pass, mode: Normal, timeout: None},
        ],
        hooks: Some({
          beforeAll: None,
          afterAll: Some(async () => JsError.throwWithMessage("still open")),
          beforeEach: Some(
            async () => {
              beforeEachRuns := beforeEachRuns.contents + 1
              if beforeEachRuns.contents == 1 {
                JsError.throwWithMessage("no db")
              }
            },
          ),
          afterEach: Some(
            async () =>
              if beforeEachRuns.contents == 2 {
                throw(Not_found)
              },
          ),
        }),
      }
      let (passed, failed, _skipped, _filtered) = await Runner.runAsyncSuiteAccumulate(
        suite,
        ~hasOnly=false,
      )
      // Two failed tests, and afterAll
      Assert.combineResults([
        Assert.equal((passed, failed), (0, 3)),
        Assert.isFalse(ran.contents, ~message="Expected the first test not to run"),
      ])
    }),
    Test.async("a rejected beforeAll fails every test in an async suite", async () => {
      Runner.setFilterPattern(None)
      Runner.setSkipPattern(None)
      let ran = ref(false)
      let run = async () => {
        ran := true
        Pass
      }
      let suite: asyncTestSuite = {
        name: "Rejecting beforeAll",
        tests: [
          {name: "a", run, mode: Normal, timeout: None},
          {name: "b", run, mode: Normal, timeout: None},
        ],
        hooks: Some({
          beforeAll: Some(async () => JsError.throwWithMessage("no db")),
          afterAll: None,
          beforeEach: None,
          afterEach: None,
        }),
      }
      let (passed, failed, _skipped, _filtered) = await Runner.runAsyncSuiteAccumulate(
        suite,
        ~hasOnly=false,
      )
      let result = await Runner.runAsyncTest(
        suite.tests->Array.getUnsafe(0),
        ~hooks=suite.hooks,
        ~beforeAll=await Runner.runAsyncHook(
          suite.hooks->Option.flatMap(h => h.beforeAll),
          ~name="beforeAll",
        ),
      )
      Assert.combineResults([
        Assert.equal((passed, failed), (0, 2)),
        Assert.isFalse(ran.contents, ~message="Expected no test to run"),
        switch result {
        | Fail(msg) => Assert.contains(msg, "beforeAll hook threw an exception: Error: no db")
        | Pass => Fail("Expected Fail")
        },
      ])
    }),
    Test.async("an async suite none of whose tests run doesn't run its hooks", async () => {
      let hookRuns = ref(0)
      let hook = async () => {
        hookRuns := hookRuns.contents + 1
        JsError.throwWithMessage("boom")
      }
      let suite: asyncTestSuite = {
        name: "Unselected",
        tests: [
          {name: "a", run: async () => Pass, mode: Normal, timeout: None},
          {name: "s", run: async () => Pass, mode: Skip, timeout: None},
        ],
        hooks: Some({
          beforeAll: Some(hook),
          afterAll: Some(hook),
          beforeEach: Some(hook),
          afterEach: Some(hook),
        }),
      }
      Runner.setSkipPattern(None)
      Runner.setFilterPattern(Some("something else"))
      let filtered = await Runner.runAsyncSuiteAccumulate(suite, ~hasOnly=false)
      Runner.setFilterPattern(None)
      // Another suite has a test with `only`
      let otherOnly = await Runner.runAsyncSuiteAccumulate(suite, ~hasOnly=true)
      await Runner.runAsyncSuite({
        ...suite,
        tests: [{name: "s", run: async () => Pass, mode: Skip, timeout: None}],
      })
      Assert.combineResults([
        Assert.equal(filtered, (0, 0, 0, 2)),
        Assert.equal(otherOnly, (0, 0, 2, 0)),
        Assert.equal(hookRuns.contents, 0, ~message="Expected no hook to run"),
      ])
    }),
    Test.async("an async hook that never settles times out, and fails its tests", async () => {
      Runner.setFilterPattern(None)
      Runner.setSkipPattern(None)
      let never = () => Promise.make((_resolve, _reject) => ())
      let hookFailure = async (hooks: asyncHooks, ~timeout=?) =>
        switch await Runner.runAsyncTest(
          {name: "t", run: async () => Pass, mode: Normal, timeout},
          ~hooks=Some(hooks),
          ~beforeAll=Pass,
        ) {
        | Fail(message) => message
        | Pass => "Pass"
        }
      let noHooks = {beforeAll: None, afterAll: None, beforeEach: None, afterEach: None}
      let previous = Runner.hookTimeout.contents
      Runner.setHookTimeout(20)
      let (passed, failed, _skipped, _filtered) = await Runner.runAsyncSuiteAccumulate(
        {
          name: "Hanging hooks",
          tests: [{name: "t", run: async () => Pass, mode: Normal, timeout: None}],
          hooks: Some({...noHooks, beforeAll: Some(never), afterAll: Some(never)}),
        },
        ~hasOnly=false,
      )
      let beforeAll = await Runner.runAsyncHook(Some(never), ~name="beforeAll")
      // beforeEach and afterEach take their test's timeout
      let beforeEach = await hookFailure({...noHooks, beforeEach: Some(never)}, ~timeout=30)
      Runner.setHookTimeout(previous)
      let afterEach = await hookFailure({...noHooks, afterEach: Some(never)}, ~timeout=30)
      Assert.combineResults([
        // The test, and afterAll
        Assert.equal((passed, failed), (0, 2)),
        Assert.equal(beforeAll, Fail("beforeAll hook timed out after 20ms")),
        Assert.equal(beforeEach, "beforeEach hook timed out after 30ms"),
        Assert.equal(afterEach, "afterEach hook timed out after 30ms"),
      ])
    }),
    Test.async("runAsyncSuite carries on past hooks that reject", async () => {
      let ran = ref(false)
      await Runner.runAsyncSuite({
        name: "Rejecting hooks",
        tests: [
          {
            name: "p",
            run: async () => {
              ran := true
              Pass
            },
            mode: Normal,
            timeout: None,
          },
        ],
        hooks: Some({
          beforeAll: None,
          afterAll: Some(async () => JsError.throwWithMessage("boom")),
          beforeEach: None,
          afterEach: Some(async () => JsError.throwWithMessage("boom")),
        }),
      })
      Assert.isTrue(ran.contents)
    }),
  ],
)


