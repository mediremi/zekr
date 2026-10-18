// Prng - A seedable pseudo-random number generator for fuzz tests: sfc32,
// seeded with splitmix32. Both work on 32-bit ints, so neither needs BigInt.
//
// A seed is a whole number from 0 to 2^32 - 1. It's a float, as ReScript's
// int stops at 2^31 - 1.

type t = {mutable a: int, mutable b: int, mutable c: int, mutable d: int}

let twoTo32 = 4294967296.

// An int's 32 bits read as an unsigned number (not with n >>> 0, which the
// compiler drops, as it types the result as an int)
let unsigned = (n: int): float => n < 0 ? Int.toFloat(n) +. twoTo32 : Int.toFloat(n)

// A seed's 32 bits as an int (the conversion wraps, keeping the bits)
let toInt32 = (seed: float): int => Float.toInt(seed)

// splitmix32: turns a seed into a sequence of well mixed 32-bit ints, to
// fill sfc32's state with
let splitmix32 = (state: ref<int>): int => {
  state := state.contents + 0x9e3779b9
  let z = state.contents
  let z = Math.Int.imul(z ^^^ z >>> 16, 0x85ebca6b)
  let z = Math.Int.imul(z ^^^ z >>> 13, 0xc2b2ae35)
  z ^^^ z >>> 16
}

// sfc32's next 32 bits
let nextInt32 = (t: t): int => {
  let result = t.a + t.b + t.d
  t.d = t.d + 1
  t.a = t.b ^^^ t.b >>> 9
  t.b = t.c + (t.c << 3)
  t.c = t.c << 21 ||| t.c >>> 11
  t.c = t.c + result
  result
}

let make = (seed: float): t => {
  let state = ref(toInt32(seed))
  let a = splitmix32(state)
  let b = splitmix32(state)
  let c = splitmix32(state)
  let d = splitmix32(state)
  let t = {a, b, c, d}
  // sfc32's first few numbers are still close to its seed
  for _ in 1 to 12 {
    let _ = nextInt32(t)
  }
  t
}

// A whole number from 0 to 2^32 - 1
let next = (t: t): float => unsigned(nextInt32(t))

// A number from 0 (inclusive) to 1 (exclusive)
let float = (t: t): float => next(t) /. twoTo32

// A whole number from 0 to max inclusive (max is at most 2^32 - 1), each as
// likely as the others: draws that would favour the low numbers are redrawn
let uniform = (t: t, max: float): float => {
  let size = max +. 1.
  let limit = twoTo32 -. Float.mod(twoTo32, size)
  let draw = ref(next(t))
  while draw.contents >= limit {
    draw := next(t)
  }
  Float.mod(draw.contents, size)
}

// A seed for one test, from the run's seed and the test's name, so a test gets
// the same values whichever other tests run alongside it
let seedFor = (seed: float, name: string): float => {
  // FNV-1a over the name's UTF-16 code units, starting from the mixed seed
  // (not the seed itself, or seed 1 with "b" would hash like seed 2 with "a")
  let hash = ref(splitmix32(ref(toInt32(seed))) ^^^ 0x811c9dc5)
  for i in 0 to String.length(name) - 1 {
    let code = name->String.charCodeAtUnsafe(i)
    hash := Math.Int.imul(hash.contents ^^^ code, 0x01000193)
  }
  unsigned(splitmix32(hash))
}

// A seed at random, for a run that wasn't given one
let randomSeed = (): float => Math.floor(Math.random() *. twoTo32)

// A seed written as text, e.g. in ZEKR_SEED, if it's a whole number from 0 to
// 2^32 - 1
let parseSeed = (text: string): option<float> =>
  if /^\d+$/->RegExp.test(String.trim(text)) {
    let seed = Float.parseFloat(text)
    seed < twoTo32 ? Some(seed) : None
  } else {
    None
  }
