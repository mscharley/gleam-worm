@external(erlang, "glonstant_ffi", "persist")
@external(javascript, "./glonstant_ffi.mjs", "persist")
pub fn persist(name: String, generator: fn() -> a) -> a
