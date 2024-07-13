//// Side effects are not encouraged when using worm because it can
//// lead to undesirable and confusing behavior. Specifically, functions
//// passed to `worm.persist` are guaranteed to be run at least once
//// but since no concurrency primitives are used to block competing
//// processes, they may be run multiple times before the result is
//// cached for the first time.
//// 
//// Ideally all functions should be pure and not take any parameters.

import gleeunit/should
import simplifile
import temporary
import worm

/// Anti-pattern
/// 
/// Functions that employ the `use <- worm.persist()` pattern should
/// not take any parameters as `worm.persist` only caches the result
/// of the initial function run and will ignore any parameters passed
/// to the function after that.
fn cached_side_effect(filepath: String, value: String) -> String {
  use <- worm.persist()

  let assert Ok(_) = "Hello world!" |> simplifile.append(to: filepath)

  value
}

pub fn side_effect_test() {
  use file <- temporary.create(temporary.file())

  let first = cached_side_effect(file, "First")
  let second = cached_side_effect(file, "Second")

  first
  |> should.equal("First")

  first
  |> should.equal(second)

  simplifile.read(file)
  |> should.equal(Ok("Hello world!"))
}

/// Anti-pattern
/// 
/// Returning `Nil` from a function passed to `worm.persist` is not
/// encouraged since it will only be useful when there are side
/// effects which can be problematic as explained above.
fn cached_nil_return_with_side_effect(filepath: String) -> Nil {
  use <- worm.persist()

  let assert Ok(_) = "Goodbye planet!" |> simplifile.append(to: filepath)

  Nil
}

pub fn nil_return_test() {
  use file <- temporary.create(temporary.file())

  cached_nil_return_with_side_effect(file)
  cached_nil_return_with_side_effect(file)

  simplifile.read(file)
  |> should.equal(Ok("Goodbye planet!"))
}
