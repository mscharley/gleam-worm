# worm 🪱

[![Package Version](https://img.shields.io/hexpm/v/worm)](https://hex.pm/packages/worm)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/worm/)

```sh
gleam add worm
```
```gleam
import gleam/regex
import worm

type RegexCache {
  RegexCache(greeting: regex.Regex, name: regex.Regex)
}

fn regexes() -> RegexCache {
  use <- worm.persist()
  let assert Ok(greeting) = regex.from_string("^\\w+")
  let assert Ok(name) = regex.from_string("\\w+$")
  RegexCache(greeting, name)
}

pub fn main() {
  let RegexCache(greeting: initial, ..) = regexes()

  "Hello, Gleam!"
  |> regex.scan(with: initial) // [regex.Match("Hello", [])]

  Nil
}
```

While this library was initial concieved of as a cache for regex compilation, it can be used for any deterministic calculation which you only want to run once and reuse many times.

Please read the documentation before using this library as it is not the best solution for many situations. API documentation can be found at <https://hexdocs.pm/worm/worm.html>.

[persistent_term]: https://www.erlang.org/doc/apps/erts/persistent_term.html
