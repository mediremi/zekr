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
