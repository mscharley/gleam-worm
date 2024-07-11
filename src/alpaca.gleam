/// Persist the result of a callback.
///
/// The generator function is expected to be deterministic and pure. Your generator function will be called
/// at least once, but may be called more than once due to race conditions during the initial filling of
/// the cache. After the cache is filled, your generator function will never be called again. If your generator
/// produces side effects then those side effects will be extremely unreliable.
///
/// If you are using this in a library then it is recommended to stick to one big cache for your library
/// wherever possible. The [`persistent_term`](https://www.erlang.org/doc/apps/erts/persistent_term.html)
/// package used to facilitate the cache on the Erlang target gets slower the more individual terms are
/// stored. Hence, a single large object stored in a single term and destructured in Gleam is the preferred
/// approach as much as reasonable.
///
/// ## Examples
///
/// ```gleam
/// pub type RegexCache {
///   RegexCache(greeting: regex.Regex, name: regex.Regex)
/// }
///
/// fn regexes() -> RegexCache {
///   use <- alpaca.persist()
///   RegexCache(regex.from_string("^\\w+"), regex.from_string("\\w+$"))
/// }
/// ```
@external(erlang, "alpaca_ffi", "persist")
@external(javascript, "./alpaca_ffi.mjs", "persist")
pub fn persist(generator: fn() -> a) -> a
