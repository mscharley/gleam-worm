-module(elephant_ffi).
-compile([no_auto_import]).

-export([persist/2]).

persist(Name, Gen) ->
	case catch persistent_term:get(Name) of
		{ 'EXIT', { badarg, _ } } -> 
			Value = Gen(),
			persistent_term:put(Name, { ok, Value }),
			Value;
		{ ok, Value } -> Value
	end.

