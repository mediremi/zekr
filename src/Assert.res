// Assert - Assertion functions for test verification

open Types

module NodeUtil = {
  @module("util")
  external inspect: ('a, {"depth": int, "compact": bool}) => string = "inspect"
}

// How a value appears in a failure message. String.make prints records,
// variants and objects as [object Object]; util.inspect shows their contents,
// with each field and array item on its own line so diffs mark just the ones
// that differ.
let show = (value: 'a): string => NodeUtil.inspect(value, {"depth": 20, "compact": false})

// Indents every line of a failure message's detail after the first
let indent = (text: string): string => text->String.replaceAll("\n", "\n       ")

type diffLine = Same(string) | Removed(string) | Added(string)

// The lines of `actual` and `expected` as a diff: the lines they share, and
// the ones only in actual (Removed) or only in expected (Added)
let diffLines = (actual: string, expected: string): array<diffLine> => {
  let a = actual->String.split("\n")
  let b = expected->String.split("\n")
  let n = Array.length(a)
  let m = Array.length(b)

  // common(i, j): how many lines a[i:] and b[j:] have in common, in order
  let table = Array.fromInitializer(~length=n + 1, _ => Array.make(~length=m + 1, 0))
  let common = (i, j) => table->Array.getUnsafe(i)->Array.getUnsafe(j)
  for i in n - 1 downto 0 {
    for j in m - 1 downto 0 {
      table
      ->Array.getUnsafe(i)
      ->Array.setUnsafe(
        j,
        a->Array.getUnsafe(i) == b->Array.getUnsafe(j)
          ? common(i + 1, j + 1) + 1
          : max(common(i + 1, j), common(i, j + 1)),
      )
    }
  }

  let lines = []
  let i = ref(0)
  let j = ref(0)
  while i.contents < n || j.contents < m {
    if (
      i.contents < n &&
      j.contents < m &&
      a->Array.getUnsafe(i.contents) == b->Array.getUnsafe(j.contents)
    ) {
      lines->Array.push(Same(a->Array.getUnsafe(i.contents)))
      i := i.contents + 1
      j := j.contents + 1
    } else if (
      j.contents >= m ||
        (i.contents < n && common(i.contents + 1, j.contents) >= common(i.contents, j.contents + 1))
    ) {
      lines->Array.push(Removed(a->Array.getUnsafe(i.contents)))
      i := i.contents + 1
    } else {
      lines->Array.push(Added(b->Array.getUnsafe(j.contents)))
      j := j.contents + 1
    }
  }
  lines
}

// How many unchanged lines are shown either side of a change
let diffContext = 3

// A diff of `actual` and `expected`, with - for lines only in actual and +
// for lines only in expected. Long runs of unchanged lines are folded into …
let diff = (actual: string, expected: string): string => {
  let lines = diffLines(actual, expected)
  let isChange = index =>
    switch lines->Array.get(index) {
    | Some(Removed(_) | Added(_)) => true
    | Some(Same(_)) | None => false
    }
  // Whether line `index` is within diffContext lines of a change
  let nearChange = index => {
    let near = ref(false)
    for offset in -diffContext to diffContext {
      if isChange(index + offset) {
        near := true
      }
    }
    near.contents
  }

  let shown = []
  lines->Array.forEachWithIndex((line, index) =>
    switch line {
    | Removed(text) => shown->Array.push(Colors.fail("- " ++ text))
    | Added(text) => shown->Array.push(Colors.pass("+ " ++ text))
    | Same(text) if nearChange(index) => shown->Array.push(Colors.dimmed("  " ++ text))
    | Same(_) =>
      // One … for each run of folded lines
      if nearChange(index - 1) || index == 0 {
        shown->Array.push(Colors.dimmed("  …"))
      }
    }
  )
  shown->Array.join("\n       ")
}

let equal = (actual: 'a, expected: 'a, ~message: option<string>=?): testResult => {
  if actual == expected {
    Pass
  } else {
    let msg = switch message {
    | Some(m) => m
    | None =>
      `Assertion failed\n` ++
      `       ${Colors.pass("+ expected")} ${Colors.fail("- actual")}\n` ++
      `       ${diff(show(actual), show(expected))}`
    }
    Fail(msg)
  }
}

let notEqual = (actual: 'a, expected: 'a, ~message: option<string>=?): testResult => {
  if actual != expected {
    Pass
  } else {
    let msg = switch message {
    | Some(m) => m
    | None =>
      `Assertion failed: values should not be equal\n` ++
      `       Both values: ${indent(show(actual))}`
    }
    Fail(msg)
  }
}

let isTrue = (condition: bool, ~message: option<string>=?): testResult => {
  if condition {
    Pass
  } else {
    let msg = switch message {
    | Some(m) => m
    | None => "Expected true, got false"
    }
    Fail(msg)
  }
}

let isFalse = (condition: bool, ~message: option<string>=?): testResult => {
  if !condition {
    Pass
  } else {
    let msg = switch message {
    | Some(m) => m
    | None => "Expected false, got true"
    }
    Fail(msg)
  }
}

let greaterThan = (actual: 'a, expected: 'a, ~message: option<string>=?): testResult => {
  if actual > expected {
    Pass
  } else {
    let msg = switch message {
    | Some(m) => m
    | None =>
      `Assertion failed: expected actual > threshold\n` ++
      `       actual:    ${indent(show(actual))}\n` ++
      `       threshold: ${indent(show(expected))}`
    }
    Fail(msg)
  }
}

let lessThan = (actual: 'a, expected: 'a, ~message: option<string>=?): testResult => {
  if actual < expected {
    Pass
  } else {
    let msg = switch message {
    | Some(m) => m
    | None =>
      `Assertion failed: expected actual < threshold\n` ++
      `       actual:    ${indent(show(actual))}\n` ++
      `       threshold: ${indent(show(expected))}`
    }
    Fail(msg)
  }
}

let greaterThanOrEqual = (
  actual: 'a,
  expected: 'a,
  ~message: option<string>=?,
): testResult => {
  if actual >= expected {
    Pass
  } else {
    let msg = switch message {
    | Some(m) => m
    | None =>
      `Assertion failed: expected actual >= threshold\n` ++
      `       actual:    ${indent(show(actual))}\n` ++
      `       threshold: ${indent(show(expected))}`
    }
    Fail(msg)
  }
}

let lessThanOrEqual = (actual: 'a, expected: 'a, ~message: option<string>=?): testResult => {
  if actual <= expected {
    Pass
  } else {
    let msg = switch message {
    | Some(m) => m
    | None =>
      `Assertion failed: expected actual <= threshold\n` ++
      `       actual:    ${indent(show(actual))}\n` ++
      `       threshold: ${indent(show(expected))}`
    }
    Fail(msg)
  }
}

let contains = (haystack: string, needle: string, ~message: option<string>=?): testResult => {
  if String.includes(haystack, needle) {
    Pass
  } else {
    let msg = switch message {
    | Some(m) => m
    | None =>
      `Assertion failed: string does not contain substring\n` ++
      `       string:    "${haystack}"\n` ++
      `       expected:  "${needle}"`
    }
    Fail(msg)
  }
}

let arrayContains = (arr: array<'a>, item: 'a, ~message: option<string>=?): testResult => {
  if Array.includes(arr, item) {
    Pass
  } else {
    let msg = switch message {
    | Some(m) => m
    | None =>
      `Assertion failed: array does not contain item\n` ++
      `       expected to contain: ${indent(show(item))}`
    }
    Fail(msg)
  }
}

let matches = (str: string, pattern: RegExp.t, ~message: option<string>=?): testResult => {
  if RegExp.test(pattern, str) {
    Pass
  } else {
    let msg = switch message {
    | Some(m) => m
    | None => `Expected "${str}" to match pattern`
    }
    Fail(msg)
  }
}

let some = (opt: option<'a>, ~message: option<string>=?): testResult => {
  switch opt {
  | Some(_) => Pass
  | None => {
      let msg = switch message {
      | Some(m) => m
      | None => "Expected Some, got None"
      }
      Fail(msg)
    }
  }
}

let none = (opt: option<'a>, ~message: option<string>=?): testResult => {
  switch opt {
  | None => Pass
  | Some(_) => {
      let msg = switch message {
      | Some(m) => m
      | None => "Expected None, got Some"
      }
      Fail(msg)
    }
  }
}

let ok = (result: result<'a, 'e>, ~message: option<string>=?): testResult => {
  switch result {
  | Ok(_) => Pass
  | Error(_) => {
      let msg = switch message {
      | Some(m) => m
      | None => "Expected Ok, got Error"
      }
      Fail(msg)
    }
  }
}

let error = (result: result<'a, 'e>, ~message: option<string>=?): testResult => {
  switch result {
  | Error(_) => Pass
  | Ok(_) => {
      let msg = switch message {
      | Some(m) => m
      | None => "Expected Error, got Ok"
      }
      Fail(msg)
    }
  }
}

let throws = (fn: unit => 'a, ~message: option<string>=?): testResult => {
  try {
    let _ = fn()
    let msg = switch message {
    | Some(m) => m
    | None => "Expected function to throw"
    }
    Fail(msg)
  } catch {
  | _ => Pass
  }
}

let combineResults = (results: array<testResult>): testResult => {
  let failures = results->Array.filter(r =>
    switch r {
    | Fail(_) => true
    | Pass => false
    }
  )

  if Array.length(failures) > 0 {
    failures->Array.get(0)->Option.getOr(Pass)
  } else {
    Pass
  }
}

let combineAsyncResults = async (results: array<promise<testResult>>): testResult => {
  let resolvedResults = await Promise.all(results)
  let failures = resolvedResults->Array.filter(r =>
    switch r {
    | Fail(_) => true
    | Pass => false
    }
  )

  if Array.length(failures) > 0 {
    failures->Array.get(0)->Option.getOr(Pass)
  } else {
    Pass
  }
}
