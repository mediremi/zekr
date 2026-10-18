open Types

// Whether a property holds for 1000 of a fuzzer's values
let holds = (fuzzer: Fuzz.t<'a>, property: 'a => bool): testResult =>
  switch Fuzz.examples(fuzzer, ~count=1000)->Array.find(value => !property(value)) {
  | Some(value) => Fail(`Doesn't hold for ${Assert.indent(Assert.show(value))}`)
  | None => Pass
  }

let isNegativeZero = (x: float) => x == 0. && 1. /. x < 0.

let fuzzerTests = Suite.make(
  "Fuzz fuzzers",
  [
    Test.make("int is a 32-bit int", () =>
      holds(Fuzz.int, n => Int.toFloat(n) == Int.toFloat(n)->Float.toInt->Int.toFloat)
    ),
    Test.make("intRange stays in its range", () =>
      holds(
        Fuzz.tuple2(Fuzz.int, Fuzz.int)->Fuzz.andThen(
          ((a, b)) => Fuzz.intRange(a, b)->Fuzz.map(n => (a, b, n)),
        ),
        ((a, b, n)) => n >= Math.Int.min(a, b) && n <= Math.Int.max(a, b),
      )
    ),
    Test.make("float is finite, and never -0", () =>
      holds(Fuzz.float, x => Float.isFinite(x) && !isNegativeZero(x))
    ),
    Test.make("floatRange stays in its range", () =>
      holds(
        Fuzz.tuple2(Fuzz.float, Fuzz.float)->Fuzz.andThen(
          ((a, b)) => Fuzz.floatRange(a, b)->Fuzz.map(x => (a, b, x)),
        ),
        ((a, b, x)) => x >= Math.min(a, b) && x <= Math.max(a, b),
      )
    ),
    Test.make("array stays within its lengths", () =>
      holds(
        Fuzz.tuple2(Fuzz.intRange(0, 10), Fuzz.intRange(0, 10))->Fuzz.andThen(
          ((a, b)) =>
            Fuzz.array(Fuzz.bool, ~minLength=a, ~maxLength=b)->Fuzz.map(items => (a, b, items)),
        ),
        ((a, b, items)) =>
          Array.length(items) >= Math.Int.min(a, b) && Array.length(items) <= Math.Int.max(a, b),
      )
    ),
    Test.make("asciiString is up to 10 printable ASCII characters", () =>
      holds(Fuzz.asciiString, string => /^[ -~]{0,10}$/->RegExp.test(string))
    ),
    Test.make("string is up to 10 code units, with no lone surrogates", () =>
      holds(Fuzz.string, string => String.length(string) <= 10 && !RegExp.test(/\p{Cs}/u, string))
    ),
    Test.make("char is one code point", () =>
      holds(Fuzz.char, char => Array.length(Array.fromString(char)) == 1)
    ),
    Test.make("oneOfValues is one of the values", () =>
      holds(Fuzz.oneOfValues(["a", "b", "c"]), value => ["a", "b", "c"]->Array.includes(value))
    ),
    Test.make("frequency is one of the fuzzers, by weight", () => {
      let values = Fuzz.examples(
        Fuzz.frequency([(0.5, Fuzz.constant("a")), (1.5, Fuzz.constant("b"))]),
        ~count=1000,
      )
      let a = values->Array.filter(value => value == "a")->Array.length
      Assert.isTrue(a > 200 && a < 300, ~message=`Expected about 250 "a"s, got ${Int.toString(a)}`)
    }),
    Test.make("recursive values are at most depth deep", () =>
      holds(
        Fuzz.recursive([Fuzz.constant(0)], depth => [depth->Fuzz.map(n => n + 1)], ~depth=3),
        depth => depth <= 3,
      )
    ),
  ],
)

let exampleTests = Suite.make(
  "Fuzz examples",
  [
    Test.make("examples gives count values", () =>
      Assert.equal(Array.length(Fuzz.examples(Fuzz.int, ~count=25)), 25)
    ),
    Test.make("labelExamples gives an example of each label and combination", () =>
      Assert.equal(
        Fuzz.labelExamples(
          Fuzz.intRange(1, 20),
          [("fizz", n => mod(n, 3) == 0), ("buzz", n => mod(n, 5) == 0), ("big", n => n > 100)],
          ~count=1000,
        )->Array.map(((labels, example)) => (labels, example->Option.isSome)),
        [(["fizz"], true), (["buzz"], true), (["big"], false), (["fizz", "buzz"], true)],
      )
    ),
  ],
)

let seedTests = Suite.make(
  "Fuzz seeds",
  [
    Test.make("the same seed gives the same values", () =>
      Assert.combineResults([
        Assert.equal(Fuzz.examples(Fuzz.float, ~seed=7.), Fuzz.examples(Fuzz.float, ~seed=7.)),
        Assert.notEqual(Fuzz.examples(Fuzz.float, ~seed=7.), Fuzz.examples(Fuzz.float, ~seed=8.)),
      ])
    ),
    Test.make("a seed is a whole number from 0 to 2^32 - 1", () =>
      Assert.equal(
        ["0", "4294967295", "4294967296", "-1", "1.5", "seed", ""]->Array.map(Prng.parseSeed),
        [Some(0.), Some(4294967295.), None, None, None, None, None],
      )
    ),
    Test.make("replaying a value's choices makes it again", () => {
      let choices = Choices.random(Prng.make(1.))
      let value = Fuzz.generate(Fuzz.list(Fuzz.string), choices)
      Assert.equal(
        Fuzz.generate(Fuzz.list(Fuzz.string), Choices.replay(Choices.recorded(choices))),
        value,
      )
    }),
    Test.make("replaying too few choices rejects", () =>
      Assert.throws(() => Fuzz.generate(Fuzz.int, Choices.replay([])))
    ),
  ],
)

// Runs a fuzz test with the run's seed set to `seed`
let runWithSeed = (seed: float, testCase: testCase): testResult => {
  let previous = Test.runSeed.contents
  Test.runSeed := Some(Ok(seed))
  let result = testCase.run()
  Test.runSeed := previous
  result
}

let runAsyncWithSeed = async (seed: float, testCase: asyncTestCase): testResult => {
  let previous = Test.runSeed.contents
  Test.runSeed := Some(Ok(seed))
  let result = await testCase.run()
  Test.runSeed := previous
  result
}

// Whether a failed fuzz test's shrunk value is `expected`
let shrunkTo = (result: testResult, expected: 'a): testResult =>
  switch result {
  | Fail(message) => Assert.contains(message, `Given: ${Assert.indent(Assert.show(expected))}\n`)
  | Pass => Fail("Expected the fuzz test to fail")
  }

let sum = list => list->List.reduce(0., (sum, n) => sum +. Int.toFloat(n))

let seeds = [1., 2., 3., 42., 4000000000.]

let shrinkingTests = Suite.make(
  "Test.fuzz shrinking",
  [
    Test.make("a list whose sum is over 1000 shrinks to [1001]", () =>
      seeds
      ->Array.map(seed =>
        Test.fuzz("sum", Fuzz.list(Fuzz.int), list => Assert.isTrue(sum(list) <= 1000.))
        ->runWithSeed(seed, _)
        ->shrunkTo(list{1001})
      )
      ->Assert.combineResults
    ),
    Test.make("a pair of ints neither of which is 0 shrinks to (1, 1)", () =>
      Test.fuzz("pair", Fuzz.tuple2(Fuzz.int, Fuzz.int), ((a, b)) =>
        Assert.isTrue(a == 0 || b == 0)
      )
      ->runWithSeed(7., _)
      ->shrunkTo((1, 1))
    ),
    Test.make("a pair of ranged ints whose sum is over 1000 shrinks to (1, 1000)", () =>
      Test.fuzz("pair", Fuzz.tuple2(Fuzz.intRange(0, 1000), Fuzz.intRange(0, 1000)), ((a, b)) =>
        Assert.isTrue(a + b <= 1000)
      )
      ->runWithSeed(7., _)
      ->shrunkTo((1, 1000))
    ),
    Test.make("shrunk values stay in their fuzzer's range", () =>
      Assert.combineResults([
        Test.fuzz("intRange", Fuzz.intRange(10, 20000), n => Assert.isTrue(n < 15))
        ->runWithSeed(7., _)
        ->shrunkTo(15),
        Test.fuzz("floatRange", Fuzz.floatRange(1., 5.), x => Assert.isTrue(x == 1.))
        ->runWithSeed(7., _)
        ->shrunkTo(5.),
        Test.fuzz("array", Fuzz.array(Fuzz.int, ~minLength=2, ~maxLength=5), items =>
          Assert.equal(items, [0, 0])
        )
        ->runWithSeed(7., _)
        ->shrunkTo([0, 1]),
      ])
    ),
    Test.make("values shrink through andThen", () =>
      Test.fuzz("andThen", Fuzz.bool->Fuzz.andThen(bool => Fuzz.list(Fuzz.constant(bool))), list =>
        Assert.isTrue(List.length(list) < 3)
      )
      ->runWithSeed(7., _)
      ->shrunkTo(list{false, false, false})
    ),
    Test.make("values made with Fuzz.make shrink", () =>
      Test.fuzz(
        "make",
        Fuzz.make(({draw}) => (draw(Fuzz.asciiString), draw(Fuzz.intRange(0, 100)))),
        ((name, age)) => Assert.isTrue(String.length(name) < 2 || age < 18),
      )
      ->runWithSeed(7., _)
      ->shrunkTo(("  ", 18))
    ),
    Test.make("a property that throws fails, and its value shrinks", () =>
      switch Test.fuzz("throws", Fuzz.int, n =>
        n > 5 ? JsError.throwWithMessage("too big") : Pass
      )->runWithSeed(7., _) {
      | Fail(message) as result =>
        Assert.combineResults([
          result->shrunkTo(6),
          Assert.contains(message, "Test threw an exception: Error: too big"),
        ])
      | Pass => Fail("Expected the fuzz test to fail")
      }
    ),
    Test.make("the failure says how to reproduce it, with the assertion's message", () =>
      switch Test.fuzz("message", Fuzz.intRange(0, 100), n =>
        Assert.isTrue(n < 50, ~message="too big")
      )->runWithSeed(42., _) {
      | Fail(message) =>
        Assert.matches(
          message,
          /^Failed after \d+ runs?, shrunk in \d+ steps?\. Seed 42\.\n {7}Given: 50\n {7}too big\n {7}Reproduce it with ZEKR_SEED=42 or zekr --seed 42$/,
        )
      | Pass => Fail("Expected the fuzz test to fail")
      }
    ),
    Test.make("shrinking stops after maxShrinks runs of the test", () => {
      let runs = ref(0)
      let result = Test.fuzz(
        "maxShrinks",
        Fuzz.list(Fuzz.int),
        list => {
          runs := runs.contents + 1
          Assert.isTrue(sum(list) <= 1000.)
        },
        ~maxShrinks=3,
      )->runWithSeed(1., _)
      // The test's own runs until the first failure, and 3 while shrinking
      switch result {
      | Fail(message) =>
        switch /Failed after (\d+) runs?/->RegExp.exec(message) {
        | Some(match) =>
          let failedAfter =
            match
            ->RegExp.Result.matches
            ->Array.get(0)
            ->Option.flatMap(x => x)
            ->Option.flatMap(Int.fromString(_))
            ->Option.getOr(0)
          Assert.equal(runs.contents, failedAfter + 3)
        | None => Fail("Expected the number of runs")
        }
      | Pass => Fail("Expected the fuzz test to fail")
      }
    }),
    Test.make("a fuzzer that can't make a value fails its test", () =>
      switch Test.fuzz("filter", Fuzz.int->Fuzz.filter(_ => false), _ => Pass)->runWithSeed(1., _) {
      | Fail(message) =>
        Assert.equal(message, "The fuzzer couldn't make a value: Too many values were filtered out")
      | Pass => Fail("Expected the fuzz test to fail")
      }
    ),
    Test.make("a filter that rejects a run of values doesn't fail its test", () =>
      // 16 ints in a row aren't multiples of 3 about once in 650 values
      Array.fromInitializer(~length=50, Int.toFloat)
      ->Array.map(seed =>
        Test.fuzz("filter", Fuzz.int->Fuzz.filter(n => mod(n, 3) == 0), _ => Pass)->runWithSeed(
          seed,
          _,
        )
      )
      ->Array.map(result => Assert.equal(result, Pass))
      ->Assert.combineResults
    ),
    Test.make("a fuzz test that passes runs 100 times, or ~runs", () => {
      let runs = ref(0)
      let count = _ => {
        runs := runs.contents + 1
        Pass
      }
      let byDefault = Test.fuzz("runs", Fuzz.int, count)->runWithSeed(1., _)
      let defaultRuns = runs.contents
      runs := 0
      let given = Test.fuzz("runs", Fuzz.int, count, ~runs=7)->runWithSeed(1., _)
      Assert.combineResults([
        Assert.equal((byDefault, defaultRuns), (Pass, 100)),
        Assert.equal((given, runs.contents), (Pass, 7)),
      ])
    }),
  ],
)

let asyncShrinkingTests = Suite.async(
  "Test.fuzzAsync",
  [
    Test.async("a failing async property's value shrinks", async () => {
      let result = await Test.fuzzAsync("sum", Fuzz.list(Fuzz.int), async list =>
        Assert.isTrue(sum(list) <= 1000.)
      )->runAsyncWithSeed(1., _)
      result->shrunkTo(list{1001})
    }),
    Test.async("a property that times out fails, and its value shrinks", async () => {
      let testCase = Test.fuzzAsync(
        "timeout",
        Fuzz.intRange(0, 1000),
        n => n >= 10 ? Promise.make((_resolve, _reject) => ()) : Promise.resolve(Pass),
        ~timeout=5,
      )
      let previous = Test.runSeed.contents
      Test.runSeed := Some(Ok(7.))
      // As the runner runs it
      let result = await Runner.runAsyncTest(testCase, ~hooks=None, ~beforeAll=Pass)
      Test.runSeed := previous
      switch result {
      | Fail(message) =>
        Assert.combineResults([
          result->shrunkTo(10),
          Assert.contains(message, "Test timed out after 5ms"),
          Assert.contains(message, "Reproduce it with ZEKR_SEED=7"),
        ])
      | Pass => Fail("Expected the fuzz test to fail")
      }
    }),
    Test.async("a passing async property passes", async () => {
      let result = await Test.fuzzAsync("passes", Fuzz.int, async _ => Pass)->runAsyncWithSeed(
        1.,
        _,
      )
      Assert.equal(result, Pass)
    }),
  ],
)

// Collects the values a fuzz test is given
let valuesOf = (seed: float, name: string, fuzzer: Fuzz.t<'a>): array<'a> => {
  let values = []
  let _ = Test.fuzz(name, fuzzer, value => {
    values->Array.push(value)
    Pass
  })->runWithSeed(seed, _)
  values
}

let fuzzSeedTests = Suite.make(
  "Test.fuzz seeds",
  [
    Test.make("the same seed gives the same values", () =>
      Assert.combineResults([
        Assert.equal(valuesOf(42., "a", Fuzz.string), valuesOf(42., "a", Fuzz.string)),
        Assert.notEqual(valuesOf(42., "a", Fuzz.string), valuesOf(43., "a", Fuzz.string)),
      ])
    ),
    Test.make("each test's values depend on its name", () =>
      Assert.notEqual(valuesOf(42., "a", Fuzz.int), valuesOf(42., "b", Fuzz.int))
    ),
  ],
)
