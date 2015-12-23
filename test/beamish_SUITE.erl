%%% ==========================================================================
%%% @doc beamish unit testing.
%%% @end
%%% @private
%%% ==========================================================================

-module(beamish_SUITE).

%%% ==========================================================================
%%% Includes
%%% ==========================================================================

-include_lib("common_test/include/ct.hrl").

%%% ==========================================================================
%%% Exports
%%% ==========================================================================

%%% Test cases
-export([
         exported_types/1
        ]).

%%% Common test callbacks
-export([suite/0, all/0,
         init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).


%%% ==========================================================================
%%% Common Test Callbacks
%%% ==========================================================================

suite() ->
    [{timetrap, {seconds, 30}}].


all() ->
    [
     exported_types
    ].


init_per_suite(Config) ->
    Config.


end_per_suite(_Config) ->
    ok.


init_per_testcase(_TestCase, Config) ->
    Config.


end_per_testcase(_TestCase, _Config) ->
    ok.



%%% ==========================================================================
%%% Test Case Functions
%%% ==========================================================================

exported_types(Config) ->
    %% We're going to generate some fake types with different arities.
    %% Then we'll generate a module and dynamically compile it.
    ArityLo = 0, ArityHi = 5,
    Types = generate_types("test_type", ArityLo, ArityHi),
    ModName = export_type_test_1,
    {Mod, Beam} = generate_types_mod(ModName, Types),
    Mod = ModName,
    ct:log("beam_lib:chunks() -> ~p~n", [beam_lib:chunks(Beam, [abstract_code])]),
    {ok, {ModName, ExpTypes}} = beamish:exported_types(Beam),
    ct:log("ExpTypes: ~p~n", [ExpTypes]),
    SortedTypes = lists:sort(Types),
    SortedTypes = lists:sort(ExpTypes),
    Config.

%%% ==========================================================================
%%% Internal Functions
%%% ==========================================================================
-spec generate_types(BaseTypeName, ArityLo, ArityHi) -> Result when
      BaseTypeName :: string(), ArityLo :: non_neg_integer(),
      ArityHi :: non_neg_integer(), Result :: [{atom(), non_neg_integer()}].
generate_types(BaseTypeName, ArityLo, ArityHi) ->
    [{list_to_atom(BaseTypeName ++ "_" ++ integer_to_list(Arity)), Arity} ||
     Arity <- lists:seq(ArityLo, ArityHi)].

-spec generate_types_mod(ModName, Types) -> Result when
      ModName :: string(), Types :: list(), Result :: {atom(), binary()}.

generate_types_mod(ModName, Types) ->
    S = generate_module_attr(ModName) ++
        generate_export_type_attr(Types) ++
        string:join(generate_typespecs(Types), ""),
    ct:log("Generated module: ~s~n", [S]),
    dynamic_compile:from_string(S, [debug_info]).

%% -> ["A","B","C","D",...]
mkarglist(N) when N =< 26 ->
    [[$A + I] || I <- lists:seq(0, N - 1)].

generate_module_attr(ModName) ->
    "-module(" ++ atom_to_list(ModName) ++ ").\n".

generate_export_type_attr(Types) ->
    "-export_type([\n" ++ generate_export_types(Types) ++ "\n]).\n".

generate_typespecs(Types) ->
    [generate_typespec(Type, Arity) || {Type, Arity} <- Types].

generate_typespec(Type, Arity) ->
    ArgList = mkarglist(Arity),
    "-type " ++ atom_to_list(Type) ++
    "(" ++ string:join(ArgList, ",") ++ ")" ++
    " :: " ++
    case Arity of
        0 ->
            "any()";
        _ ->
            "{" ++ string:join(ArgList, ",") ++ "}"
    end ++ ".\n".

%% -> "type0/0,type1/1,type2/2,..."
generate_export_types(Types) ->
    L = [atom_to_list(Type) ++ "/" ++ integer_to_list(Arity) ||
         {Type, Arity} <- Types],
    string:join(L, ",").

