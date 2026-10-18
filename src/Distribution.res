// Distribution - What share of a fuzz test's values have some property, to
// report or check, e.g. that at least 10% of generated lists are longer than
// 10 items. Ported from elm-test 2's Test.Distribution; see
// https://martin.janiczek.cz/2025/05/01/elm-test-distributions.html

// How common a label should be among a fuzz test's values
type expected =
  // At least this percentage (from 0 to 100) of values
  | AtLeast(float)
  // None of them
  | Zero
  // At least one of them
  | MoreThanZero

type t<'a> =
  // Labels to count, and print a table of with the test's result
  | Report(array<(string, 'a => bool)>)
  // Labels to check, failing the test when a label isn't as common as expected
  | Expect(array<(expected, string, 'a => bool)>)

let report = (labels: array<(string, 'a => bool)>): t<'a> => Report(labels)

let expect = (expectations: array<(expected, string, 'a => bool)>): t<'a> => Expect(expectations)

let labels = (distribution: t<'a>): array<(string, 'a => bool)> =>
  switch distribution {
  | Report(labels) => labels
  | Expect(expectations) => expectations->Array.map(((_, label, predicate)) => (label, predicate))
  }

// How many values had each combination of labels: one entry for each set of
// labels a value had, including none
type counts = array<(array<string>, ref<int>)>

let emptyCounts = (distribution: t<'a>): counts =>
  distribution->labels->Array.map(((label, _)) => ([label], ref(0)))

let count = (counts: counts, distribution: t<'a>, value: 'a): unit => {
  let matching =
    distribution
    ->labels
    ->Array.filterMap(((label, predicate)) => predicate(value) ? Some(label) : None)
  switch counts->Array.find(((labels, _)) => labels == matching) {
  | Some((_, count)) => count := count.contents + 1
  | None => counts->Array.push((matching, ref(1)))
  }
}

// The counts with each label's own count including the combinations it's in
let includeCombinations = (counts: counts): array<(array<string>, int)> =>
  counts->Array.map(((labels, count)) =>
    switch labels {
    | [label] => (
        labels,
        counts->Array.reduce(count.contents, (sum, (combination, count)) =>
          Array.length(combination) > 1 && combination->Array.includes(label)
            ? sum + count.contents
            : sum
        ),
      )
    | _ => (labels, count.contents)
    }
  )

let countOf = (counts: array<(array<string>, int)>, label: string): int =>
  counts
  ->Array.find(((labels, _)) => labels == [label])
  ->Option.mapOr(0, ((_, count)) => count)

// The checks allow a false positive (a wrong verdict) once in `certainty`
// runs, as in elm-test and QuickCheck
let certainty = 1e9

// A label passes once it's likely to be at least `tolerance` times as common
// as expected, so a label that's only just common enough doesn't take forever
let tolerance = 0.9

// Acklam's approximation of the inverse of the standard normal distribution's
// cumulative distribution function, good enough for a certainty up to 1e9:
// https://web.archive.org/web/20151110174102/http://home.online.no/~pjacklam/notes/invnorm/
let inverseNormal = (p: float): float => {
  let a1 = -39.69683028665376
  let a2 = 220.9460984245205
  let a3 = -275.9285104469687
  let a4 = 138.357751867269
  let a5 = -30.66479806614716
  let a6 = 2.506628277459239
  let b1 = -54.47609879822406
  let b2 = 161.5858368580409
  let b3 = -155.6989798598866
  let b4 = 66.80131188771972
  let b5 = -13.28068155288572
  let c1 = -0.007784894002430293
  let c2 = -0.3223964580411365
  let c3 = -2.400758277161838
  let c4 = -2.549732539343734
  let c5 = 4.374664141464968
  let c6 = 2.938163982698783
  let d1 = 0.007784695709041462
  let d2 = 0.3224671290700398
  let d3 = 2.445134137142996
  let d4 = 3.754408661907416
  let pLow = 0.02425
  let pHigh = 1. -. pLow
  let tail = q =>
    (((((c1 *. q +. c2) *. q +. c3) *. q +. c4) *. q +. c5) *. q +. c6) /.
      ((((d1 *. q +. d2) *. q +. d3) *. q +. d4) *. q +. 1.)

  if p < 0. || p > 1. {
    Float.Constants.nan
  } else if p == 0. {
    Float.Constants.negativeInfinity
  } else if p == 1. {
    Float.Constants.positiveInfinity
  } else if p < pLow {
    tail(Math.sqrt(-2. *. Math.log(p)))
  } else if p <= pHigh {
    let q = p -. 0.5
    let r = q *. q
    (((((a1 *. r +. a2) *. r +. a3) *. r +. a4) *. r +. a5) *. r +. a6) *.
    q /.
    (((((b1 *. r +. b2) *. r +. b3) *. r +. b4) *. r +. b5) *. r +. 1.)
  } else {
    -.tail(Math.sqrt(-2. *. Math.log(1. -. p)))
  }
}

// The Wilson score interval's bound, for z, on the share of values with a label
// when `seen` of `total` values had it:
// https://en.wikipedia.org/wiki/Binomial_proportion_confidence_interval#Wilson_score_interval
let wilson = (~seen: int, ~total: int, z: float): float => {
  let n = Int.toFloat(total)
  let p = Int.toFloat(seen) /. n
  let zz = z *. z
  (p +. zz /. (2. *. n) +. z *. Math.sqrt(p *. (1. -. p) /. n +. zz /. (4. *. n *. n))) /.
    (1. +. zz /. n)
}

let falsePositive = 1. /. certainty

// Whether the label is surely common enough: share is from 0 to 1
let sufficientlyCovered = (~seen: int, ~total: int, share: float): bool =>
  wilson(~seen, ~total, inverseNormal(falsePositive /. 2.)) >= tolerance *. share

// Whether the label is surely not common enough. Every value having it is
// always enough, though rounding puts the bound just under 1 then.
let insufficientlyCovered = (~seen: int, ~total: int, share: float): bool =>
  seen < total && wilson(~seen, ~total, inverseNormal(1. -. falsePositive /. 2.)) < share

type verdict = Covered | NotCovered(string) | Undecided

let formatPercentage = (percentage: float): string =>
  `${(Math.round(percentage *. 1000.) /. 1000.)->Float.toFixed(~digits=3)}%`

let describe = (expected: expected): string =>
  switch expected {
  | AtLeast(percentage) => `at least ${formatPercentage(percentage)}`
  | Zero => "exactly 0%"
  | MoreThanZero => "more than 0%"
  }

// How many values MoreThanZero looks through for one with its label, before
// it fails
let moreThanZeroValues = 10000

// Whether each expected label is as common as it should be, after `total`
// values, or Undecided when it takes more values to tell. AtLeast is decided
// by the Wilson score interval; Zero fails as soon as a value has the label;
// and MoreThanZero passes as soon as one does, or fails after
// moreThanZeroValues values without one.
let check = (
  expectations: array<(expected, string, 'a => bool)>,
  counts: counts,
  ~total: int,
): verdict => {
  let counts = includeCombinations(counts)
  let notCovered = (expected, label, seen) => NotCovered(
    `Distribution of label "${label}" was insufficient:\n` ++
    `       expected:  ${describe(expected)}\n` ++
    `       got:       ${formatPercentage(Int.toFloat(seen) *. 100. /. Int.toFloat(total))}\n` ++
    `       (Generated ${Int.toString(total)} values.)`,
  )
  let insufficient = expectations->Array.find(((expected, label, _)) => {
    let seen = countOf(counts, label)
    switch expected {
    | AtLeast(percentage) => insufficientlyCovered(~seen, ~total, percentage /. 100.)
    | Zero => seen > 0
    | MoreThanZero => seen == 0 && total >= moreThanZeroValues
    }
  })
  let allCovered = expectations->Array.every(((expected, label, _)) => {
    let seen = countOf(counts, label)
    switch expected {
    | AtLeast(percentage) => sufficientlyCovered(~seen, ~total, percentage /. 100.)
    | Zero => true
    | MoreThanZero => seen > 0
    }
  })
  switch insufficient {
  | Some((expected, label, _)) => notCovered(expected, label, countOf(counts, label))
  | None => allCovered ? Covered : Undecided
  }
}

// A table of how common each label was, e.g.
//
//   Distribution report:
//   ====================
//     short:  70.0%  (70x)  █████████████████████░░░░░░░░░
//     long:   30.0%  (30x)  █████████░░░░░░░░░░░░░░░░░░░░░
let table = (counts: counts, ~total: int): string => {
  let counts = includeCombinations(counts)
  let rows =
    counts
    // A label never seen on its own is in the table as part of a combination
    ->Array.filter(((labels, count)) =>
      switch labels {
      | [label] =>
        count > 0 ||
          !(
            counts->Array.some(((other, _)) =>
              Array.length(other) > 1 && other->Array.includes(label)
            )
          )
      | _ => true
      }
    )
    ->Array.toSorted(((_, a), (_, b)) => Int.compare(b, a))
  let (base, combinations) = rows->Array.reduce(([], []), ((base, combinations), row) => {
    let (labels, _) = row
    Array.length(labels) > 1 ? combinations->Array.push(row) : base->Array.push(row)
    (base, combinations)
  })
  let name = labels => Array.length(labels) == 0 ? "<uncategorized>" : labels->Array.join(", ")
  let percentage = count =>
    `${(Math.round(Int.toFloat(count) /. Int.toFloat(total) *. 1000.) /. 10.)->Float.toString}%`
  let width = f =>
    rows->Array.reduce(0, (widest, row) => Math.Int.max(widest, String.length(f(row))))
  let nameWidth = width(((labels, _)) => name(labels))
  let percentageWidth = width(((_, count)) => percentage(count))
  let countWidth = width(((_, count)) => Int.toString(count))
  let bars = 30
  let render = ((labels, count)) => {
    let filled = Float.toInt(
      Math.round(Int.toFloat(count) /. Int.toFloat(total) *. Int.toFloat(bars)),
    )
    `  ${(name(labels) ++ ":")->String.padEnd(nameWidth + 1, " ")}  ${percentage(
        count,
      )->String.padStart(percentageWidth, " ")}  (${Int.toString(count)->String.padStart(
        countWidth,
        " ",
      )}x)  ${String.repeat("█", filled)}${String.repeat("░", bars - filled)}`
  }
  let combinationsTable = switch combinations {
  | [] => ""
  | _ =>
    "\nCombinations (included in the above base counts):\n" ++
    combinations->Array.map(render)->Array.join("\n")
  }
  `Distribution report:\n====================\n${base
    ->Array.map(render)
    ->Array.join("\n")}${combinationsTable}`
}
