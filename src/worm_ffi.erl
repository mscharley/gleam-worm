-module(worm_ffi).
-compile([no_auto_import]).

-export([persist/1]).

persist(Gen) ->
	{ _, GenModule } = erlang:fun_info(Gen, module),
	{ _, GenName } = erlang:fun_info(Gen, name),
	Name = { ?MODULE, GenModule, GenName },
	case catch persistent_term:get(Name) of
		{ 'EXIT', { badarg, _ } } -> 
			Value = Gen(),
			try
				persistent_term:get(Name)
			catch
				error:badarg:_ ->
					persistent_term:put(Name, { ok, Value }),
					Value
			end;
		{ ok, Value } -> Value
	end.

