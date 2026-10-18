// Choices - The random choices a fuzzer makes, as whole numbers from 0 to
// 2^32 - 1, recorded so that its value can be made again by replaying them.
// A failing value is shrunk by replaying edited choices.

// A fuzzer can't make a value from these choices. Replaying throws it when
// the choices run out or one is out of range, and Fuzz.filter when too many
// values were filtered out.
exception Reject(string)

type source = Random(Prng.t) | Replay(array<float>)

type t = {source: source, recorded: array<float>}

let random = (prng: Prng.t): t => {source: Random(prng), recorded: []}

let replay = (choices: array<float>): t => {source: Replay(choices), recorded: []}

// The choices made so far
let recorded = (t: t): array<float> => t.recorded->Array.copy

// The next choice, from 0 to max: a new one from generate, or the next
// recorded one
let draw = (t: t, ~max: float, ~generate: Prng.t => float): float => {
  let choice = switch t.source {
  | Random(prng) => generate(prng)
  | Replay(choices) =>
    switch choices->Array.get(Array.length(t.recorded)) {
    | None => throw(Reject("ran out of choices"))
    | Some(choice) if choice > max => throw(Reject("a choice was out of range"))
    | Some(choice) => choice
    }
  }
  t.recorded->Array.push(choice)
  choice
}

// A choice from 0 to max, each as likely as the others
let uniform = (t: t, max: float): float => draw(t, ~max, ~generate=prng => Prng.uniform(prng, max))

// A choice that can only be `value`. It's still recorded, so that a value's
// choices are laid out the same whether or not a choice was forced, e.g.
// Fuzz.array records a 1 before every item, even the ones it must have.
let forced = (t: t, value: float): float => {
  switch t.source {
  | Random(_) => ()
  | Replay(choices) =>
    switch choices->Array.get(Array.length(t.recorded)) {
    | None => throw(Reject("ran out of choices"))
    | Some(choice) if choice != value => throw(Reject("a forced choice was changed"))
    | Some(_) => ()
    }
  }
  t.recorded->Array.push(value)
  value
}
