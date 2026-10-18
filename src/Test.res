// Test - Test case factory functions

open Types

let make = (name: string, run: unit => testResult): testCase => {
  {name, run, mode: Normal}
}

let skip = (name: string, run: unit => testResult): testCase => {
  {name, run, mode: Skip}
}

let only = (name: string, run: unit => testResult): testCase => {
  {name, run, mode: Only}
}

let async = (
  name: string,
  run: unit => promise<testResult>,
  ~timeout: option<int>=?,
): asyncTestCase => {
  {name, run, mode: Normal, timeout}
}

let asyncSkip = (
  name: string,
  run: unit => promise<testResult>,
  ~timeout: option<int>=?,
): asyncTestCase => {
  {name, run, mode: Skip, timeout}
}

let asyncOnly = (
  name: string,
  run: unit => promise<testResult>,
  ~timeout: option<int>=?,
): asyncTestCase => {
  {name, run, mode: Only, timeout}
}

// Fuzz tests: tests of a property that should hold for any value of a fuzzer.
// The property runs on `runs` random values, and when one fails, it's shrunk
// to a simpler value that fails too (see Shrink).

@val @scope("process") external env: Dict.t<string> = "env"

// The seed of this run's fuzz tests: ZEKR_SEED, or one at random. Each test's
// values come from this seed and the test's name, so it reproduces them
// whichever tests run.
let runSeed: ref<option<result<float, string>>> = ref(None)

let seed = (): result<float, string> =>
  switch runSeed.contents {
  | Some(seed) => seed
  | None => {
      let seed = switch env->Dict.get("ZEKR_SEED") {
      | None => Ok(Prng.randomSeed())
      | Some(text) =>
        switch Prng.parseSeed(text) {
        | Some(seed) => Ok(seed)
        | None => Error(`ZEKR_SEED must be a whole number from 0 to 4294967295, not "${text}"`)
        }
      }
      runSeed := Some(seed)
      seed
    }
  }

let plural = (count: int, noun: string): string =>
  `${Int.toString(count)} ${noun}${count == 1 ? "" : "s"}`

// How many values the fuzzer can fail to make (see Fuzz.filter) for each
// one a test needs, before the test fails
let maxDiscards = 10

// The fuzz test loop, for sync tests and async ones
module FuzzLoop = (E: Shrink.Effect) => {
  module Shrinker = Shrink.Make(E)

  // How far the loop has got: values tested, and values the fuzzer couldn't
  // make (e.g. ones Fuzz.filter rejected), which are discarded
  type progress = {tested: int, discarded: int}

  let run = (
    ~name: string,
    ~fuzzer: Fuzz.t<'a>,
    ~check: 'a => E.t<testResult>,
    ~runs: int,
    ~maxShrinks: int,
  ): E.t<testResult> =>
    switch seed() {
    | Error(message) => E.return(Fail(message))
    | Ok(seed) => {
        let prng = Prng.make(Prng.seedFor(seed, name))
        let seedText = Float.toString(seed)

        let shrink = (~tested, ~run, ~value, ~failure) =>
          Shrinker.shrink(
            ~fuzzer,
            ~check=value =>
              check(value)->E.flatMap(result =>
                switch result {
                | Pass => E.return(None)
                | Fail(failure) => E.return(Some(failure))
                }
              ),
            ~maxShrinks,
            {run, value, failure},
          )->E.flatMap(({state, steps}) =>
            E.return(
              Fail(
                `Failed after ${plural(tested, "run")}, shrunk in ${plural(
                    steps,
                    "step",
                  )}. Seed ${seedText}.\n` ++
                `       Given: ${Assert.indent(Assert.show(state.value))}\n` ++
                `       ${state.failure}\n` ++
                `       Reproduce it with ZEKR_SEED=${seedText} or zekr --seed ${seedText}`,
              ),
            )
          )

        E.loop({tested: 0, discarded: 0}, progress =>
          if progress.tested >= runs {
            E.return(Error(Pass))
          } else {
            let choices = Choices.random(prng)
            switch Fuzz.generate(fuzzer, choices) {
            // Another value is drawn, so that a filter that rejects a run of
            // values doesn't fail the test, unless it rejects most of them
            | exception Choices.Reject(reason) =>
              E.return(
                progress.discarded >= maxDiscards * runs
                  ? Error(Fail(`The fuzzer couldn't make a value: ${reason}`))
                  : Ok({...progress, discarded: progress.discarded + 1}),
              )
            | exception exn => E.return(Error(Runner.exceptionFailure(~thrower="The fuzzer", exn)))
            | value => {
                let tested = progress.tested + 1
                check(value)->E.flatMap(result =>
                  switch result {
                  | Pass => E.return(Ok({...progress, tested}))
                  | Fail(failure) =>
                    shrink(~tested, ~run=Choices.recorded(choices), ~value, ~failure)->E.flatMap(
                      result => E.return(Error(result)),
                    )
                  }
                )
              }
            }
          }
        )
      }
    }
}

module SyncFuzzLoop = FuzzLoop(Shrink.Sync)
module AsyncFuzzLoop = FuzzLoop(Shrink.Async)

let fuzz = (
  name: string,
  fuzzer: Fuzz.t<'a>,
  property: 'a => testResult,
  ~runs: int=100,
  ~maxShrinks: int=10000,
): testCase => {
  name,
  mode: Normal,
  run: () =>
    SyncFuzzLoop.run(
      ~name,
      ~fuzzer,
      // An exception the property throws fails it, as for a test
      ~check=value => Runner.runSafely(() => property(value)),
      ~runs,
      ~maxShrinks,
    ),
}

let fuzzAsync = (
  name: string,
  fuzzer: Fuzz.t<'a>,
  property: 'a => promise<testResult>,
  ~runs: int=100,
  ~maxShrinks: int=10000,
  ~timeout: option<int>=?,
): asyncTestCase => {
  name,
  mode: Normal,
  // The timeout is for each run of the property, so that one that takes too
  // long fails like any other, and is shrunk and reported with the seed
  timeout: None,
  run: () =>
    AsyncFuzzLoop.run(
      ~name,
      ~fuzzer,
      ~check=value => Runner.runWithTimeout(() => property(value), timeout),
      ~runs,
      ~maxShrinks,
    ),
}
