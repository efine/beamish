%%--------------------------------------------------------------------
%% @doc
%% This is a simple module that provides useful BEAM-related utility
%% functions that at some time one or more of us wished we had.
%%
%% Like one that will tell us which types a BEAM exports, so we can
%% us them in our function specs.
%%
%% @end
%%--------------------------------------------------------------------
-module(beamish).

-export([
         exported_types/1
        ]).

-type beam_spec() :: module() | file:filename() | binary().
-type type_spec() :: {Type :: atom(), Arity :: non_neg_integer()}.

%%--------------------------------------------------------------------
%% @doc
%% Return the module name and a list of types that were exported by a
%% BEAM file, presumably using `-export_type'.
%%
%% If there is no abstract code in the BEAM file, return the module
%% name and an empty list.
%% @end
%%--------------------------------------------------------------------
-spec exported_types(Beam) -> Result when
      Beam :: beam_spec(),
      Result :: {ok, {Mod :: module(), Types :: [type_spec()]}} |
                {error, beam_lib, Reason :: term()}.
exported_types(Beam) when is_list(Beam) orelse
                          is_atom(Beam) orelse
                          is_binary(Beam) ->
    case beam_lib:chunks(Beam, [abstract_code]) of
        {ok, {Mod, ChunkData}} ->
            case proplists:get_value(abstract_code, ChunkData) of
                {_AbstVsn, Forms} ->
                    ExpTypeLists = [Form || {attribute, _, export_type, Form} <- Forms],
                    SetOfTypes = gb_sets:union([gb_sets:from_list(ExpTypes) ||
                                                ExpTypes <- ExpTypeLists]),
                    {ok, {Mod, gb_sets:to_list(SetOfTypes)}};
                no_abstract_code ->
                    {ok, {Mod, []}}
            end;
        Err ->
            Err
    end.
