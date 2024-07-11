# alpaca

[![Package Version](https://img.shields.io/hexpm/v/alpaca)](https://hex.pm/packages/alpaca)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/alpaca/)

```sh
gleam add alpaca
```
```gleam
import gleam/regex
import alpaca

pub type RegexCache {
  RegexCache(greeting: regex.Regex, name: regex.Regex)
}

fn regexes() -> RegexCache {
  use <- alpaca.persist("my_app")
  RegexCache(regex.from_string("^\\w+"), regex.from_string("\\w+$"))
}

pub fn main() {
  let RegexCache(initial, name) = regexes()
  "Hello, Gleam!" |> regex.scan(initial) // [Match("Hello, _)]
  "Hello, Gleam!" |> regex.scan(name) // []
}
```

While this library was initial concieved of as a cache for regex compilation, it can be used for any deterministic calculation which you only want to run once and reuse many times.

If you are using this in a library then it is recommended to stick to one big cache for your library wherever possible. The [`persistent_term`][persistent_term] package used to facilitate the cache on the Erlang target gets slower the more individual terms are stored. Hence, a single large object stored in a single term and destructured in Gleam is the preferred approach as much as reasonable.

API documentation can be found at <https://hexdocs.pm/alpaca>.

[persistent_term]: https://www.erlang.org/doc/apps/erts/persistent_term.html
