import gleam/int
import gleam/regex
import gleeunit
import gleeunit/should
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
