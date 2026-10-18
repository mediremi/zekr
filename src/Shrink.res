// Shrink - Simplifies a fuzz test's failing value, by editing the choices it
// was made from (see Choices) and keeping each edit that makes smaller choices
// whose value still fails. Choices are smaller when there are fewer of them,
// or as many but the first that differs is smaller (shortlex order).
//
// A port of elm-test 2's Simplify and Simplify.Cmd. The test can be sync or
// async, so the shrinker is written once for an Effect, Sync or Async.

module type Effect = {
  type t<'a>
  let return: 'a => t<'a>
  let flatMap: (t<'a>, 'a => t<'b>) => t<'b>
  // Runs step from init until it returns Error(result), without growing the
  // stack (a recursive flatMap would, in Sync)
  let loop: ('s, 's => t<result<'s, 'r>>) => t<'r>
}

module Sync: Effect with type t<'a> = 'a = {
  type t<'a> = 'a
  let return = value => value
  let flatMap = (value, f) => f(value)
  let loop = (init, step) => {
    let state = ref(Ok(init))
    let finished = ref(None)
    while finished.contents->Option.isNone {
      switch state.contents {
      | Ok(current) => state := step(current)
      | Error(done) => finished := Some(done)
      }
    }
    finished.contents->Option.getUnsafe
  }
}

module Async: Effect with type t<'a> = promise<'a> = {
  type t<'a> = promise<'a>
  let return = value => Promise.resolve(value)
  let flatMap = (promise, f) => promise->Promise.then(f)
  let loop = async (init, step) => {
    let state = ref(Ok(init))
    let finished = ref(None)
    while finished.contents->Option.isNone {
      switch state.contents {
      | Ok(current) => state := (await step(current))
      | Error(done) => finished := Some(done)
      }
    }
    finished.contents->Option.getUnsafe
  }
}

// Editing choices. Choices never go below 0, and edits out of bounds are
// ignored, as in elm-test's RandomRun.

type chunk = {size: int, start: int}

let compare = (a: array<float>, b: array<float>): int =>
  if Array.length(a) != Array.length(b) {
    Array.length(a) < Array.length(b) ? -1 : 1
  } else {
    let result = ref(0)
    let i = ref(0)
    while result.contents == 0 && i.contents < Array.length(a) {
      let x = a->Array.getUnsafe(i.contents)
      let y = b->Array.getUnsafe(i.contents)
      result := if x < y {
          -1
        } else if x > y {
          1
        } else {
          0
        }
      i := i.contents + 1
    }
    result.contents
  }

let set = (run: array<float>, index: int, value: float): array<float> =>
  if index < 0 || index >= Array.length(run) {
    run
  } else {
    let copy = run->Array.copy
    copy->Array.setUnsafe(index, Math.max(0., value))
    copy
  }

let update = (run: array<float>, index: int, f: float => float): array<float> =>
  switch run->Array.get(index) {
  | Some(value) => set(run, index, f(value))
  | None => run
  }

let inBounds = (run: array<float>, chunk: chunk): bool =>
  chunk.start + chunk.size <= Array.length(run)

let deleteChunk = (run: array<float>, chunk: chunk): array<float> =>
  inBounds(run, chunk)
    ? Array.concat(
        run->Array.slice(~start=0, ~end=chunk.start),
        run->Array.slice(~start=chunk.start + chunk.size),
      )
    : run

let replaceChunkWithZero = (run: array<float>, chunk: chunk): array<float> =>
  inBounds(run, chunk)
    ? run->Array.mapWithIndex((value, index) =>
        index >= chunk.start && index < chunk.start + chunk.size ? 0. : value
      )
    : run

let sortChunk = (run: array<float>, chunk: chunk): array<float> =>
  if inBounds(run, chunk) {
    let sorted =
      run
      ->Array.slice(~start=chunk.start, ~end=chunk.start + chunk.size)
      ->Array.toSorted(Float.compare)
    Array.concat(
      Array.concat(run->Array.slice(~start=0, ~end=chunk.start), sorted),
      run->Array.slice(~start=chunk.start + chunk.size),
    )
  } else {
    run
  }

// Swaps two chunks of the same size. If they overlap, the left chunk's
// original values win.
let swapChunks = (run: array<float>, left: chunk, right: chunk): option<array<float>> =>
  if inBounds(run, left) && inBounds(run, right) {
    let copy = run->Array.copy
    for i in 0 to left.size - 1 {
      copy->Array.setUnsafe(right.start + i, run->Array.getUnsafe(left.start + i))
    }
    for i in 0 to right.size - 1 {
      copy->Array.setUnsafe(left.start + i, run->Array.getUnsafe(right.start + i))
    }
    Some(copy)
  } else {
    None
  }

// The commands, from Simplify.Cmd: each is one way of editing choices, tried
// in this order, and only while the choices are at least minLength long

type command =
  | DeleteChunkAndMaybeDecrementPrevious(chunk)
  | ReplaceChunkWithZero(chunk)
  | SortChunk(chunk)
  | MinimizeFloat(int)
  | MinimizeChoice(int)
  | RedistributeChoicesAndMaybeIncrement(int, int)
  | DecrementTogether(int, int, float)
  | SwapChunkWithNeighbour(chunk)

type step = {command: command, minLength: int}

// Chunks of sizes 1 (if allowed), 2, 3, 4 and 8, largest first and, for each
// size, from the end of the choices (chunks of 3 are common: a list's "one
// more" choice, then an int's bucket and value)
let chunkSteps = (length: int, ~allowSize1: bool, toCommand: chunk => command): array<step> => {
  let steps = []
  [1, 2, 3, 4, 8]
  ->Array.filter(size => allowSize1 || size > 1)
  ->Array.forEach(size =>
    for start in 0 to length - size {
      steps->Array.push({command: toCommand({size, start}), minLength: start + size})
    }
  )
  steps->Array.toReversed
}

// Floats are three choices, hi, lo and a sign of 0 or 1 (see Fuzz.FloatBits)
let minimizeFloatSteps = (run: array<float>): array<step> => {
  let steps = []
  for index in 0 to Array.length(run) - 3 {
    if run->Array.getUnsafe(index + 2) <= 1. {
      steps->Array.push({command: MinimizeFloat(index), minLength: index + 3})
    }
  }
  steps
}

let redistributeSteps = (length: int): array<step> => {
  let steps = []
  for offset in 1 to 3 {
    for left in length - 1 - offset downto 0 {
      steps->Array.push({
        command: RedistributeChoicesAndMaybeIncrement(left, left + offset),
        minLength: left + offset + 1,
      })
    }
  }
  steps
}

// An int is two choices, a bucket and its value, so neighbouring ints'
// values are 2 apart; the sign bit halves values, so decrementing by 2 helps
let decrementTogetherSteps = (length: int): array<step> => {
  let maxOffset = length < 512 ? 4 : 2
  let steps = []
  for left in 0 to length - 2 {
    for offset in 1 to Math.Int.min(maxOffset, length - left - 1) {
      [4., 2., 1.]->Array.forEach(by =>
        steps->Array.push({
          command: DecrementTogether(left, left + offset, by),
          minLength: left + offset + 1,
        })
      )
    }
  }
  steps
}

let stepsFor = (run: array<float>): array<step> => {
  let length = Array.length(run)
  [
    chunkSteps(length, ~allowSize1=true, chunk => DeleteChunkAndMaybeDecrementPrevious(chunk)),
    // Zeroing 1 choice is part of MinimizeChoice
    chunkSteps(length, ~allowSize1=false, chunk => ReplaceChunkWithZero(chunk)),
    run->Array.filterMapWithIndex((value, index) =>
      value > 0. ? Some({command: MinimizeChoice(index), minLength: index + 1}) : None
    ),
    minimizeFloatSteps(run),
    chunkSteps(length, ~allowSize1=false, chunk => SortChunk(chunk)),
    redistributeSteps(length),
    decrementTogetherSteps(length),
    // Swapping single choices is part of RedistributeChoicesAndMaybeIncrement
    chunkSteps(length, ~allowSize1=false, chunk => SwapChunkWithNeighbour(chunk))->Array.map(step =>
      switch step.command {
      | SwapChunkWithNeighbour(chunk) => {...step, minLength: step.minLength + chunk.size}
      | _ => step
      }
    ),
  ]->Array.flat
}

// The smallest failing value found so far, with its choices and failure
type state<'a> = {run: array<float>, value: 'a, failure: string}

// The smallest failing value, and how many edits it took to find it
type shrunk<'a> = {state: state<'a>, steps: int}

module Make = (E: Effect) => {
  // check(value) is Some(failure) if the value fails the test
  let shrink = (
    ~fuzzer: Fuzz.t<'a>,
    ~check: 'a => E.t<option<string>>,
    ~maxShrinks: int,
    initial: state<'a>,
  ): E.t<shrunk<'a>> => {
    // How many times the test has run, and how many commands made the
    // choices smaller
    let attempts = ref(0)
    let steps = ref(0)

    // Replays the choices, and keeps their value if it still fails and its
    // choices are smaller. Returns whether it kept it.
    let keepIfBetter = (run: array<float>, state: state<'a>): E.t<(bool, state<'a>)> =>
      if compare(run, state.run) == 0 || attempts.contents >= maxShrinks {
        E.return((false, state))
      } else {
        let choices = Choices.replay(run)
        switch Fuzz.generate(fuzzer, choices) {
        | exception _ => E.return((false, state))
        | value =>
          // Only the choices the fuzzer used, as the rest made no difference
          let used = Choices.recorded(choices)
          if compare(used, state.run) >= 0 {
            E.return((false, state))
          } else {
            attempts := attempts.contents + 1
            check(value)->E.flatMap(failure =>
              switch failure {
              | Some(failure) => E.return((true, {run: used, value, failure}))
              | None => E.return((false, state))
              }
            )
          }
        }
      }

    // Tries low, then binary searches for the smallest value from low to high
    // that updateRun can put in the choices and still fail
    let binarySearch = (
      ~low: float,
      ~high: float,
      ~updateRun: (float, array<float>) => array<float>,
      state: state<'a>,
    ): E.t<(bool, state<'a>)> =>
      keepIfBetter(updateRun(low, state.run), state)->E.flatMap(((improved, afterLow)) =>
        if improved {
          E.return((true, afterLow))
        } else {
          E.loop((low, high, state), ((low, high, current)) =>
            if low +. 1. < high {
              let mid = low +. Math.round((high -. low) /. 2.)
              keepIfBetter(updateRun(mid, current.run), current)->E.flatMap(
                ((improved, next)) => E.return(Ok(improved ? (low, mid, next) : (mid, high, next))),
              )
            } else {
              E.return(Error((current !== state, current)))
            }
          )
        }
      )

    let andThen = (attempt: E.t<(bool, state<'a>)>, next: state<'a> => E.t<(bool, state<'a>)>) =>
      attempt->E.flatMap(((improvedFirst, state)) =>
        next(state)->E.flatMap(((improvedNext, state)) =>
          E.return((improvedFirst || improvedNext, state))
        )
      )

    let run = (command: command, state: state<'a>): E.t<(bool, state<'a>)> =>
      switch command {
      | DeleteChunkAndMaybeDecrementPrevious(chunk) => {
          let deleted = deleteChunk(state.run, chunk)
          // e.g. for a list's last item, which can't be deleted without
          // turning the "one more" choice before it from 1 to 0
          let decremented = update(deleted, chunk.start - 1, x => x -. 1.)
          keepIfBetter(decremented, state)->E.flatMap(((improved, next)) =>
            improved || decremented == deleted
              ? E.return((improved, next))
              : keepIfBetter(deleted, state)
          )
        }

      | ReplaceChunkWithZero(chunk) => keepIfBetter(replaceChunkWithZero(state.run, chunk), state)

      | SortChunk(chunk) => keepIfBetter(sortChunk(state.run, chunk), state)

      | SwapChunkWithNeighbour(chunk) =>
        switch swapChunks(state.run, chunk, {...chunk, start: chunk.start + chunk.size}) {
        | Some(swapped) => keepIfBetter(swapped, state)
        | None => E.return((false, state))
        }

      | MinimizeChoice(index) =>
        switch state.run->Array.get(index) {
        | Some(value) if value > 0. =>
          binarySearch(
            ~low=0.,
            ~high=value,
            ~updateRun=(value, run) => set(run, index, value),
            state,
          )
        | _ => E.return((false, state))
        }

      | MinimizeFloat(index) =>
        // The exponent, then the mantissa, of a fractional float
        let pair = state =>
          switch (state.run->Array.get(index), state.run->Array.get(index + 1)) {
          | (Some(hi), Some(lo)) => Some((hi, lo))
          | _ => None
          }
        let setPair = (run, (hi, lo)) => run->set(index, hi)->set(index + 1, lo)
        switch pair(state) {
        | Some((hi, _)) if Fuzz.FloatBits.isFractional(hi) =>
          let minimizeExponent = state =>
            switch pair(state) {
            | Some((hi, lo)) =>
              binarySearch(
                ~low=0.,
                ~high=Int.toFloat(Fuzz.FloatBits.getExponent(hi)),
                ~updateRun=(exponent, run) =>
                  setPair(run, Fuzz.FloatBits.setExponent(Float.toInt(exponent), (hi, lo))),
                state,
              )
            | None => E.return((false, state))
            }
          let minimizeMantissa = state =>
            switch pair(state) {
            | Some((hi, lo)) =>
              binarySearch(
                ~low=0.,
                ~high=Fuzz.FloatBits.getMantissa(hi, lo),
                ~updateRun=(mantissa, run) =>
                  setPair(run, Fuzz.FloatBits.setMantissa(mantissa, (hi, lo))),
                state,
              )
            | None => E.return((false, state))
            }
          minimizeExponent(state)->andThen(minimizeMantissa)
        | _ => E.return((false, state))
        }

      | DecrementTogether(left, right, by) =>
        keepIfBetter(update(update(state.run, left, x => x -. by), right, x => x -. by), state)

      | RedistributeChoicesAndMaybeIncrement(left, right) =>
        // Swaps the two choices if they're out of order, then moves as much as
        // it can from the left one to the right one, keeping their sum
        switch (state.run->Array.get(left), state.run->Array.get(right)) {
        | (Some(leftValue), Some(rightValue)) => {
            let (swapped, leftValue, rightValue) =
              leftValue > rightValue
                ? (state.run->set(left, rightValue)->set(right, leftValue), rightValue, leftValue)
                : (state.run, leftValue, rightValue)
            keepIfBetter(swapped, state)->E.flatMap(((swapImproved, afterSwap)) => {
              let redistribute = run =>
                binarySearch(
                  ~low=0.,
                  ~high=leftValue,
                  ~updateRun=(value, run) =>
                    run->set(left, value)->set(right, rightValue +. leftValue -. value),
                  {...afterSwap, run},
                )->E.flatMap(((improved, redistributed)) =>
                  // run may not fail (when it's incremented), but if the
                  // search improved on it, the result does
                  E.return(
                    improved && compare(redistributed.run, afterSwap.run) < 0
                      ? (true, redistributed)
                      : (false, afterSwap),
                  )
                )
              redistribute(afterSwap.run)->E.flatMap(((improved, next)) =>
                if improved {
                  E.return((true, next))
                } else {
                  // e.g. ints 236 and 255 can't become 0 and 491 while 491 is
                  // in the right int's 0..255 bucket, so also try the next
                  // bucket up: the choice before the right one
                  redistribute(update(afterSwap.run, right - 1, x => x +. 1.))->E.flatMap(
                    ((improved, next)) =>
                      improved ? E.return((true, next)) : E.return((swapImproved, afterSwap)),
                  )
                }
              )
            })
          }
        | _ => E.return((false, state))
        }
      }

    // Runs every command once, in order
    let pass = (state: state<'a>): E.t<state<'a>> =>
      E.loop((stepsFor(state.run), 0, state), ((commands, index, state)) =>
        switch commands->Array.get(index) {
        | None => E.return(Error(state))
        | Some({minLength}) if minLength > Array.length(state.run) =>
          E.return(Ok((commands, index + 1, state)))
        | Some({command}) =>
          run(command, state)->E.flatMap(((_, next)) => {
            if compare(next.run, state.run) < 0 {
              steps := steps.contents + 1
            }
            E.return(Ok((commands, index + 1, next)))
          })
        }
      )

    // Passes until one makes no difference, or the test has run maxShrinks
    // times
    E.loop(initial, state =>
      if Array.length(state.run) == 0 || attempts.contents >= maxShrinks {
        E.return(Error(state))
      } else {
        pass(state)->E.flatMap(next =>
          E.return(compare(next.run, state.run) == 0 ? Error(next) : Ok(next))
        )
      }
    )->E.flatMap(state => E.return({state, steps: steps.contents}))
  }
}
