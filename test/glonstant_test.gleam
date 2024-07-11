import gleam/int
import gleeunit
import gleeunit/should
import glonstant

pub fn main() {
  gleeunit.main()
}

fn random_int() {
  int.random(100)
}

pub fn simple_test() {
  let n = glonstant.persist("test1", random_int)
  let m = glonstant.persist("test1", random_int)

  n
  |> should.equal(m)
}
