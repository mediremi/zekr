// Fuzz - Fuzzers, the random values of fuzz tests (see Test.fuzz).
//
// A fuzzer makes its value from random choices, which a fuzz test records.
// When a value fails a test, the test shrinks it by editing the recorded
// choices and making the value again, so a shrunk value is always one the
// fuzzer could have made: it stays in its fuzzer's range and passes its
// filters. Shrinking works through map and andThen without any help.
//
// This is elm-test 2's design; the fuzzers here are ported from its Fuzz.
//
//   let user = Fuzz.make(({draw}) => {
//     name: draw(Fuzz.string),
//     age: draw(Fuzz.option(Fuzz.intRange(0, 130))),
//   })
//
//   Test.fuzz("a user survives a JSON round trip", user, user =>
//     Assert.equal(user->User.toJson->User.fromJson, Ok(user))
//   )

// A fuzzer of values of type 'a
type t<'a>

// Basic fuzzers

// An int from -(2^31 - 1) to 2^31 - 1, mostly small ones. Shrinks towards 0,
// and towards positive values.
let int: t<int>

// An int from lo to hi, inclusive (in either order). Shrinks towards 0, or
// towards the bound closest to 0 if 0 is out of range.
let intRange: (int, int) => t<int>

// A finite float: never NaN, Infinity or -0. Prefers whole numbers and simple
// fractions, and shrinks towards 0 and towards whole numbers.
let float: t<float>

// A float from lo to hi, inclusive (in either order), which must be finite.
// Tries the bounds (and 0 when it's in range) more often than other values.
// Shrinks towards 0 or the bound closest to it, but not towards nice values.
let floatRange: (float, float) => t<float>

// Shrinks towards false
let bool: t<bool>

// One Unicode character, as a string: mostly ASCII, then whitespace,
// combining diacritics, emoji and any other code point that isn't a
// surrogate. Shrinks towards " ".
let char: t<string>

// Up to 10 printable ASCII characters (from " " to "~"). Shrinks towards "".
let asciiString: t<string>

// A string of `char`s, up to 10 UTF-16 code units long (an emoji is 2).
// Shrinks towards "".
let string: t<string>

// Always the same value
let constant: 'a => t<'a>

// Collections and wrappers

// An array of items, 0 to 32 long by default. Shrinks towards fewer items,
// and simpler ones.
let array: (t<'a>, ~minLength: int=?, ~maxLength: int=?) => t<array<'a>>

// A list of items, like array
let list: (t<'a>, ~minLength: int=?, ~maxLength: int=?) => t<list<'a>>

// None a quarter of the time. Shrinks towards None.
let option: t<'a> => t<option<'a>>

// Error a quarter of the time, from the first fuzzer for Ok values and the
// second for Error ones. Shrinks towards Error.
let result: (t<'a>, t<'e>) => t<result<'a, 'e>>

let tuple2: (t<'a>, t<'b>) => t<('a, 'b)>

// Choosing

// One of the fuzzers, each as likely as the others. Shrinks towards the first.
let oneOf: array<t<'a>> => t<'a>

// One of the values, each as likely as the others. Shrinks towards the first.
let oneOfValues: array<'a> => t<'a>

// One of the fuzzers, as likely as its weight, e.g. [(1., Fuzz.constant(None)),
// (3., ...)] for None a quarter of the time. Weights can't be negative, and
// one must be more than 0. Shrinks towards the first fuzzer.
let frequency: array<(float, t<'a>)> => t<'a>

// Combinators

let map: (t<'a>, 'a => 'b) => t<'b>

let map2: (t<'a>, t<'b>, ('a, 'b) => 'c) => t<'c>

let map3: (t<'a>, t<'b>, t<'c>, ('a, 'b, 'c) => 'd) => t<'d>

// A fuzzer that depends on another's value, e.g. an index into an array:
//
//   Fuzz.array(Fuzz.int, ~minLength=1)->Fuzz.andThen(items =>
//     Fuzz.intRange(0, Array.length(items) - 1)->Fuzz.map(index => (items, index))
//   )
let andThen: (t<'a>, 'a => t<'b>) => t<'b>

// Only the values for which keep is true. When it keeps none of 16 in a row,
// the fuzz test discards the value and draws another, and it fails once it
// has discarded 10 times as many values as it needs. A filter slows tests
// down, so prefer map when you can: an even int is
// Fuzz.int->Fuzz.map(n => n * 2), not a filter.
let filter: (t<'a>, 'a => bool) => t<'a>

// A fuzzer made when it's first used, for a fuzzer that refers to itself
let lazy_: (unit => t<'a>) => t<'a>

// A fuzzer of a recursive type: base values, and recursive ones made from
// smaller values, up to `depth` levels deep (4 by default). Shrinks towards
// the first base fuzzer. For example, binary trees:
//
//   let tree = Fuzz.recursive([Fuzz.constant(Leaf)], tree => [
//     Fuzz.map2(tree, tree, (left, right) => Node(left, right)),
//   ])
//
// Each level multiplies the size of the values, so keep depth small when
// values have many children, e.g. from Fuzz.array.
let recursive: (array<t<'a>>, t<'a> => array<t<'a>>, ~depth: int=?) => t<'a>

// Makes a value by drawing from other fuzzers, in the order it draws them.
// Easier to read than nested andThens for a record:
//
//   let point = Fuzz.make(({draw}) => {x: draw(Fuzz.int), y: draw(Fuzz.int)})
//
// The value shrinks like one made with andThen.
type draw = {draw: 'a. t<'a> => 'a}
let make: (draw => 'a) => t<'a>

// Examples

// A few of the fuzzer's values, to see what it makes, e.g. in a REPL. The
// seed is count, unless given. Values the fuzzer couldn't make are left out.
let examples: (t<'a>, ~count: int=?, ~seed: float=?) => array<'a>

// One example for each label (a predicate on the fuzzer's values) out of
// `count` values (100 by default), or None if there was none, and one for
// each combination of labels found:
//
//   Fuzz.labelExamples(Fuzz.intRange(1, 20), [
//     ("fizz", n => mod(n, 3) == 0),
//     ("buzz", n => mod(n, 5) == 0),
//   ])
//   == [(["fizz"], Some(3)), (["buzz"], Some(10)), (["fizz", "buzz"], Some(15))]
let labelExamples: (
  t<'a>,
  array<(string, 'a => bool)>,
  ~count: int=?,
  ~seed: float=?,
) => array<(array<string>, option<'a>)>

// For fuzz tests and Shrink

// The fuzzer's value from these choices. Throws Choices.Reject when it can't
// make one.
let generate: (t<'a>, Choices.t) => 'a

// How the float fuzzers lay out a float in choices, for Shrink to simplify it
module FloatBits: {
  let isFractional: float => bool
  let getExponent: float => int
  let setExponent: (int, (float, float)) => (float, float)
  let getMantissa: (float, float) => float
  let setMantissa: (float, (float, float)) => (float, float)
}
