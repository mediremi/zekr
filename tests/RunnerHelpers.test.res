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
  ],
)


