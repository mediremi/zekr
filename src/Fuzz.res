// Fuzz - Fuzzers, the random values of fuzz tests. Ported from elm-test 2's
// Fuzz: a fuzzer makes its value from choices (see Choices), and lays them out
// so that smaller choices make simpler values, for shrinking.

type t<'a> = Choices.t => 'a

let generate = (fuzzer: t<'a>, choices: Choices.t): 'a => fuzzer(choices)

// A fuzzer that can't make a value, e.g. a oneOf of nothing. Tests using it fail
// with the reason.
let invalid = (reason: string): t<'a> => _ => throw(Choices.Reject(reason))

// Combinators

let constant = (value: 'a): t<'a> => _ => value

let map = (fuzzer: t<'a>, f: 'a => 'b): t<'b> => choices => f(fuzzer(choices))

let map2 = (a: t<'a>, b: t<'b>, f: ('a, 'b) => 'c): t<'c> =>
  choices => {
    let x = a(choices)
    let y = b(choices)
    f(x, y)
  }

let map3 = (a: t<'a>, b: t<'b>, c: t<'c>, f: ('a, 'b, 'c) => 'd): t<'d> =>
  choices => {
    let x = a(choices)
    let y = b(choices)
    let z = c(choices)
    f(x, y, z)
  }

let tuple2 = (a: t<'a>, b: t<'b>): t<('a, 'b)> => map2(a, b, (x, y) => (x, y))

let andThen = (fuzzer: t<'a>, f: 'a => t<'b>): t<'b> => choices => f(fuzzer(choices))(choices)

// As in elm-test, a value is tried 16 times before the fuzzer gives up
let filter = (fuzzer: t<'a>, keep: 'a => bool): t<'a> =>
  choices => {
    let rec attempt = tries =>
      if tries > 15 {
        throw(Choices.Reject("Too many values were filtered out"))
      } else {
        let value = fuzzer(choices)
        keep(value) ? value : attempt(tries + 1)
      }
    attempt(0)
  }

let lazy_ = (makeFuzzer: unit => t<'a>): t<'a> => choices => makeFuzzer()(choices)

type draw = {draw: 'a. t<'a> => 'a}

let make = (f: draw => 'a): t<'a> => choices => f({draw: fuzzer => fuzzer(choices)})

// Choices

// A whole number from 0 to max (at most 2^32 - 1), shrinking towards 0
let uniformInt = (max: float): t<float> => choices => Choices.uniform(choices, max)

let int32 = uniformInt(4294967295.)

let forcedChoice = (value: float): t<float> => choices => Choices.forced(choices, value)

// True with probability p, shrinking towards false
let weightedBool = (p: float): t<bool> =>
  if p <= 0. {
    forcedChoice(0.)->map(_ => false)
  } else if p >= 1. {
    forcedChoice(1.)->map(_ => true)
  } else {
    choices =>
      Choices.draw(choices, ~max=1., ~generate=prng => Prng.float(prng) <= p ? 1. : 0.) == 1.
  }

// One of the fuzzers, chosen by weight (which must be more than 0), as one
// choice: its index, so it shrinks towards the first
let weighted = (fuzzers: array<(float, t<'a>)>): t<'a> => {
  let total = fuzzers->Array.reduce(0., (sum, (weight, _)) => sum +. weight)
  let pick = prng => {
    let target = ref(Prng.float(prng) *. total)
    let index = ref(0)
    while (
      index.contents < Array.length(fuzzers) - 1 &&
        target.contents >= Pair.first(fuzzers->Array.getUnsafe(index.contents))
    ) {
      target := target.contents -. Pair.first(fuzzers->Array.getUnsafe(index.contents))
      index := index.contents + 1
    }
    Int.toFloat(index.contents)
  }
  choices => {
    let index = Choices.draw(choices, ~max=Int.toFloat(Array.length(fuzzers) - 1), ~generate=pick)
    Pair.second(fuzzers->Array.getUnsafe(Float.toInt(index)))(choices)
  }
}

let intFrequency = (fuzzers: array<(int, t<'a>)>): t<'a> =>
  weighted(fuzzers->Array.map(((weight, fuzzer)) => (Int.toFloat(weight), fuzzer)))

// Choosing

let oneOf = (fuzzers: array<t<'a>>): t<'a> =>
  switch Array.length(fuzzers) {
  | 0 => invalid("Fuzz.oneOf: there must be at least one fuzzer")
  | length =>
    uniformInt(Int.toFloat(length - 1))->andThen(index =>
      fuzzers->Array.getUnsafe(Float.toInt(index))
    )
  }

let oneOfValues = (values: array<'a>): t<'a> => oneOf(values->Array.map(constant))

// Numbers

// Ints are drawn from 0..2^bits - 1, for these numbers of bits by weight:
// mostly small, sometimes huge
let intPreferences = [(4, 4), (8, 8), (2, 16), (1, 32)]

let intBits = (bits: int): t<float> => uniformInt(2. ** Int.toFloat(bits) -. 1.)

// The lowest bit is the sign, so values shrink towards 0 and positive ones
let int: t<int> =
  intPreferences
  ->Array.map(((weight, bits)) => (weight, intBits(bits)))
  ->intFrequency
  ->map(n => {
    let magnitude = Math.floor(n /. 2.)
    Float.toInt(Float.mod(n, 2.) == 1. ? -.magnitude : magnitude)
  })

// A whole number from 0 to max, uniform up to 255 and above that preferring
// small values like int
let natural = (max: float): t<float> =>
  if max <= 255. {
    uniformInt(max)
  } else {
    let size = max +. 1.
    // The int preferences that fit: whose bits are at most the number of bits
    // size needs, rounded up to a power of 2
    let maxBits = 2. ** Math.ceil(Math.log2(Math.ceil(Math.log2(size))))
    let preferences = intPreferences->Array.filter(((_, bits)) => Int.toFloat(bits) <= maxBits)
    preferences
    ->Array.map(((weight, bits)) => (weight, intBits(bits)))
    ->intFrequency
    ->map(n => Float.mod(n, size))
  }

let rec intRange = (lo: int, hi: int): t<int> =>
  if hi < lo {
    intRange(hi, lo)
  } else if lo == hi {
    constant(lo)
  } else if lo >= 0 {
    natural(Int.toFloat(hi) -. Int.toFloat(lo))->map(n => Float.toInt(Int.toFloat(lo) +. n))
  } else if hi <= 0 {
    natural(Int.toFloat(hi) -. Int.toFloat(lo))->map(n => Float.toInt(Int.toFloat(hi) -. n))
  } else {
    // Each half shrinks towards 0, and positive values come first
    oneOf([intRange(0, hi), intRange(lo, -1)])
  }

let bool = oneOfValues([false, true])

// How the float fuzzer lays a float out in choices, from elm-test's
// Fuzz.Float, which took it from Hypothesis: two 32-bit choices, hi and lo,
// where simpler floats have lexicographically smaller choices
module FloatBits = {
  let twoTo32 = 4294967296.

  // A choice's 32 bits as an int
  let toInt = Prng.toInt32

  let unsigned = Prng.unsigned

  let keepBits = (count: int, n: int): int => count >= 32 ? n : n &&& (1 << count) - 1

  let reverseByte = (byte: int): int => {
    let result = ref(0)
    for i in 0 to 7 {
      result := result.contents ||| (byte >>> i &&& 1) << 7 - i
    }
    result.contents
  }

  let reverseByteTable = Array.fromInitializer(~length=256, reverseByte)

  let reverse32Bits = (n: int): int => {
    let byte = shift => reverseByteTable->Array.getUnsafe(n >>> shift &&& 0xFF)
    byte(0) << 24 ||| byte(8) << 16 ||| byte(16) << 8 ||| byte(24)
  }

  // The lowest `count` bits of n, reversed
  let reverseNBits = (count: int, n: int): int => count == 0 ? 0 : reverse32Bits(n) >>> 32 - count

  // Reverses a 52-bit mantissa split into its high 20 and low 32 bits
  let reverse52Bits = ((hi20: int, lo32: int)): (int, int) => {
    let reversedLo32 = reverse32Bits(lo32)
    let reversedHi20 = reverseNBits(20, hi20)
    (keepBits(20, reversedLo32 >>> 12), keepBits(12, reversedLo32) << 20 ||| reversedHi20)
  }

  // Whether hi's top bit says the float has a fractional part
  let isFractional = (hi: float): bool => hi >= 2147483648.

  let getExponent = (hi: float): int => toInt(hi) >>> 20 &&& 0x7FF

  let setExponent = (exponent: int, (hi: float, lo: float)): (float, float) => (
    unsigned(toInt(hi) &&& 0x800FFFFF ||| (exponent &&& 0x7FF) << 20),
    lo,
  )

  let getMantissa = (hi: float, lo: float): float =>
    Int.toFloat(toInt(hi) &&& 0xFFFFF) *. twoTo32 +. lo

  let setMantissa = (mantissa: float, (hi: float, _lo: float)): (float, float) => (
    unsigned(toInt(hi) &&& 0xFFF00000 ||| toInt(Math.floor(mantissa /. twoTo32)) &&& 0xFFFFF),
    Float.mod(mantissa, twoTo32),
  )

  // The float with these IEEE 754 bits
  let view = DataView.fromBuffer(ArrayBuffer.make(8))
  let fromBits = (hi: float, lo: float): float => {
    view->DataView.setUint32(0, toInt(hi))
    view->DataView.setUint32(4, toInt(lo))
    view->DataView.getFloat64(0)
  }

  let exponentBias = 1023
  let maxExponent = 0x7FF

  // Exponents ordered from simplest: 0 and up (1, 2, 4, ...), then the
  // negative ones (1/2, 1/4, ...), and last the one for infinities and NaN
  let exponentKey = (exponent: int): float => {
    let unbiased = exponent - exponentBias
    if exponent == maxExponent {
      Float.Constants.positiveInfinity
    } else if unbiased < 0 {
      Int.toFloat(10000 - unbiased)
    } else {
      Int.toFloat(unbiased)
    }
  }

  let exponentMapping =
    Array.fromInitializer(~length=maxExponent + 1, exponent => exponent)->Array.toSorted((a, b) =>
      Float.compare(exponentKey(a), exponentKey(b))
    )

  // Reverses the mantissa's fractional bits, so that fractions with fewer
  // bits, like 1/2 and 1/4, come first
  let reorderMantissa = (unbiasedExponent: int, (hi: int, lo: int)): (int, int) =>
    if unbiasedExponent <= 0 {
      reverse52Bits((hi, lo))
    } else if unbiasedExponent <= 51 {
      let fractionalBits = 52 - unbiasedExponent
      let fractionalHi = fractionalBits >= 32 ? keepBits(fractionalBits - 32, hi) : 0
      let fractionalLo = fractionalBits < 32 ? keepBits(fractionalBits, lo) : lo
      let reversedHi = fractionalBits >= 32 ? reverseNBits(fractionalBits - 32, fractionalHi) : 0
      let reversedLo =
        fractionalBits < 32 ? reverseNBits(fractionalBits, lo) : reverseNBits(32, fractionalLo)
      (hi ^^^ fractionalHi ||| reversedHi, lo ^^^ fractionalLo ||| reversedLo)
    } else {
      (hi, lo)
    }

  // The float that the choices hi and lo stand for: a whole number when hi's
  // top bit is 0, and otherwise a float with a reordered exponent and mantissa
  let fromChoices = (hi: float, lo: float): float =>
    if isFractional(hi) {
      let exponent = exponentMapping->Array.getUnsafe(getExponent(hi))
      let (mantissaHi, mantissaLo) = reorderMantissa(
        exponent - exponentBias,
        (toInt(hi) &&& 0xFFFFF, toInt(lo)),
      )
      fromBits(unsigned(exponent << 20 ||| mantissaHi), unsigned(mantissaLo))
    } else {
      getMantissa(hi, lo)
    }
}

// A finite float, preferring whole numbers and simple fractions. Never -0.
let wellShrinkingFloat: t<float> = map3(int32, int32, bool, (hi, lo, negative) => {
  let value = FloatBits.fromChoices(hi, lo)
  negative && value != 0. ? -.value : value
})->filter(Float.isFinite)

// 0 on its own as well, as wellShrinkingFloat needs at least 3 choices
let float = intFrequency([(1, constant(0.)), (5, wellShrinkingFloat)])

// A float from 0 (inclusive) to 1 (exclusive), shrinking towards 0
let percentage: t<float> = intFrequency([
  (1, constant(0.)),
  (1, constant(1. -. 2. ** -52.)),
  (
    4,
    map2(uniformInt(0xFFFFF->Int.toFloat), int32, (hi, lo) =>
      (hi *. FloatBits.twoTo32 +. lo) /. 4503599627370495.
    ),
  ),
])

// A float from lo to hi, uniform rather than preferring simple values
let scaledFloat = (lo: float, hi: float): t<float> =>
  percentage->map(p => Math.min(hi, p *. (hi -. lo) +. lo))

let rec floatRange = (lo: float, hi: float): t<float> =>
  if !Float.isFinite(lo) || !Float.isFinite(hi) {
    invalid("Fuzz.floatRange: the bounds must be finite")
  } else if hi < lo {
    floatRange(hi, lo)
  } else if lo == hi {
    constant(lo)
  } else if lo >= 0. {
    intFrequency([(1, constant(lo)), (1, constant(hi)), (4, scaledFloat(lo, hi))])
  } else if hi <= 0. {
    intFrequency([
      (1, constant(lo)),
      (1, constant(hi)),
      // Shrinking towards 0
      (4, scaledFloat(-.hi, -.lo)->map(x => -.x)),
    ])
  } else {
    // Each half shrinks towards 0, and positive values come first
    intFrequency([
      (1, constant(0.)),
      (2, constant(lo)),
      (2, constant(hi)),
      (4, scaledFloat(0., -.lo)->map(x => x == 0. ? 0. : -.x)),
      (4, scaledFloat(0., hi)),
    ])
  }

// Strings

let asciiChar: t<string> = intRange(32, 126)->map(String.fromCharCode)

let char: t<string> = intFrequency([
  (5, asciiChar),
  (2, oneOfValues([" ", "\t", "\n"])),
  // Combining circumflex, tilde and diaeresis
  (1, oneOfValues([0x0302, 0x0303, 0x0308])->map(String.fromCodePoint)),
  (1, oneOfValues(["🌈", "❤", "🔥"])),
  // Anything but the surrogates, 0xD800 to 0xDFFF, which are only valid in
  // pairs
  (1, intRange(0, 0x10FFFF)->filter(n => n < 0xD800 || n > 0xDFFF)->map(String.fromCodePoint)),
])

// Collections

// Each item is preceded by a choice of whether there's another, so deleting
// an item's choices deletes the item
let array = (item: t<'a>, ~minLength: int=0, ~maxLength: int=32): t<array<'a>> => {
  let (minLength, maxLength) =
    minLength > maxLength ? (maxLength, minLength) : (minLength, maxLength)
  if maxLength <= 0 {
    constant([])
  } else {
    // From Hypothesis, for a list of average length about the average of
    // minLength and maxLength
    let average = Int.toFloat(minLength + maxLength) /. 2.
    let another = weightedBool(1. -. 1. /. (1. +. average))
    choices => {
      let items = []
      let continue = ref(true)
      while continue.contents {
        let length = Array.length(items)
        let more = if length < minLength {
          forcedChoice(1.)(choices) == 1.
        } else if length == maxLength {
          forcedChoice(0.)(choices) == 1.
        } else {
          another(choices)
        }
        if more {
          items->Array.push(item(choices))
        } else {
          continue := false
        }
      }
      items
    }
  }
}

let list = (item: t<'a>, ~minLength: int=0, ~maxLength: int=32): t<list<'a>> =>
  array(item, ~minLength, ~maxLength)->map(List.fromArray)

let asciiString: t<string> = array(asciiChar, ~maxLength=10)->map(chars => chars->Array.join(""))

// Up to 10 UTF-16 code units, like elm-test's: an emoji is 2
let string: t<string> =
  array(char, ~maxLength=10)
  ->map(chars => chars->Array.join(""))
  ->filter(string => String.length(string) <= 10)

let option = (fuzzer: t<'a>): t<option<'a>> =>
  intFrequency([(1, constant(None)), (3, fuzzer->map(value => Some(value)))])

let result = (ok: t<'a>, error: t<'e>): t<result<'a, 'e>> =>
  intFrequency([(1, error->map(e => Error(e))), (3, ok->map(value => Ok(value)))])

// Unlike elm-test's, which picks with a percentage for weights that aren't
// whole numbers, giving the first fuzzer a sixth of the time more than its
// share, this is one choice whatever the weights
let frequency = (fuzzers: array<(float, t<'a>)>): t<'a> =>
  if fuzzers->Array.some(((weight, _)) => weight < 0. || !Float.isFinite(weight)) {
    invalid("Fuzz.frequency: weights must be finite, and can't be negative")
  } else {
    switch fuzzers->Array.filter(((weight, _)) => weight > 0.) {
    | [] => invalid("Fuzz.frequency: at least one weight must be more than 0")
    | fuzzers => weighted(fuzzers)
    }
  }

let recursive = (base: array<t<'a>>, recurse: t<'a> => array<t<'a>>, ~depth: int=4): t<'a> => {
  let rec atDepth = depth =>
    depth <= 0 ? oneOf(base) : oneOf(Array.concat(base, recurse(atDepth(depth - 1))))
  atDepth(depth)
}

// Examples

let examples = (fuzzer: t<'a>, ~count: int=10, ~seed: option<float>=?): array<'a> => {
  let prng = Prng.make(seed->Option.getOr(Int.toFloat(count)))
  Array.fromInitializer(~length=count, _ =>
    try Some(fuzzer(Choices.random(prng))) catch {
    | Choices.Reject(_) => None
    }
  )->Array.filterMap(value => value)
}

let labelExamples = (
  fuzzer: t<'a>,
  labels: array<(string, 'a => bool)>,
  ~count: int=100,
  ~seed: option<float>=?,
): array<(array<string>, option<'a>)> => {
  // The first example of each combination of labels, in the order found
  let found: array<(array<string>, 'a)> = []
  examples(fuzzer, ~count, ~seed?)->Array.forEach(value => {
    let matching =
      labels->Array.filterMap(((label, predicate)) => predicate(value) ? Some(label) : None)
    if (
      Array.length(matching) > 0 &&
        !(found->Array.some(((combination, _)) => combination == matching))
    ) {
      found->Array.push((matching, value))
    }
  })
  let single = labels->Array.filterMap(((label, _)) =>
    switch found->Array.find(((combination, _)) => combination == [label]) {
    | Some((_, example)) => Some(([label], Some(example)))
    // Found, but only along with other labels
    | None if found->Array.some(((combination, _)) => combination->Array.includes(label)) => None
    | None => Some(([label], None))
    }
  )
  let combinations =
    found
    ->Array.filter(((combination, _)) => Array.length(combination) > 1)
    ->Array.map(((combination, example)) => (combination, Some(example)))
  Array.concat(single, combinations)
}
