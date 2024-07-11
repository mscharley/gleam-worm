-module(alpaca_ffi).
-compile([no_auto_import]).

-export([persist/1]).

persist(Gen) ->
	{ _, GenModule } = erlang:fun_info(Gen, module),
	{ _, GenName } = erlang:fun_info(Gen, name),
	Name = { ?MODULE, GenModule, GenName },
	case catch persistent_term:get(Name) of
		{ 'EXIT', { badarg, _ } } -> 
			Value = Gen(),
			case catch persistent_term:get(Name) of
				{ 'EXIT', { badarg, _ } } ->
					persistent_term:put(Name, { ok, Value }),
					Value;
				_ -> Value
			end;
		{ ok, Value } -> Value
	end.

