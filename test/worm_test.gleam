import gleam/int
import gleam/regex
import gleeunit
import gleeunit/should
import simplifile
import temporary
import worm

pub fn main() {
  gleeunit.main()
}

fn random_int() {
  int.random(100)
}

fn cached_int() {
  use <- worm.persist()
  random_int()
}

pub fn simple_test() {
  let n = cached_int()
  let m = cached_int()

  n |> should.equal(m)
}

type RegexCache {
  RegexCache(greeting: regex.Regex, name: regex.Regex)
}

fn regexes() -> RegexCache {
  use <- worm.persist()
  let assert Ok(greeting) = regex.from_string("^\\w+")
  let assert Ok(name) = regex.from_string("\\w+$")
  RegexCache(greeting, name)
}

pub fn readme_example_test() {
  let RegexCache(greeting: initial, ..) = regexes()
  "Hello, Gleam!"
  |> regex.scan(with: initial)
  |> should.equal([regex.Match("Hello", [])])
}

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
