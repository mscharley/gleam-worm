/// Persist a value with a given name. The name must be globally unique, so the current recommendation is to
/// include your library or module name as a prefix on the name.
///
/// The generator function is expected to be deterministic and pure. Your generator function will be called
/// at least once, but may be called more than once due to race conditions during the initial filling of
/// the cache. After the cache is filled, your generator function will never be called again. If your generator
/// produces side effects then those side effects will be extremely unreliable.
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
