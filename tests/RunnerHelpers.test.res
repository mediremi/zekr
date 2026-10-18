open Types

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

