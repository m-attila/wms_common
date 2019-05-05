%%%-------------------------------------------------------------------
%%% @author Attila Makra
%%% @copyright (C) 2019, OTP Bank Nyrt.
%%% @doc
%%% Proplist handling function.
%%% @end
%%% Created : 05. May 2019 11:43
%%%-------------------------------------------------------------------
-module(wms_common_proplist).
-author("Attila Makra").

%% API
-export([get_proplist_value/3,
         proplist_to_map/1,
         proplist_to_map/2]).

-spec get_proplist_value([{term(), term() | [term()]}], [term()], term()) ->
  term().
get_proplist_value([], _, Default) ->
  Default;
get_proplist_value(Proplist, [Key], Default) ->
  proplists:get_value(Key, Proplist, Default);
get_proplist_value(Proplist, [Key | Rest], Default) ->
  case proplists:get_value(Key, Proplist, undefined) of
    V when is_list(V) ->
      get_proplist_value(V, Rest, Default);
    _ ->
      Default
  end;
get_proplist_value(Proplist, Key, Default) ->
  proplists:get_value(Key, Proplist, Default).

-spec proplist_to_map([{term(), term() | [term()]}]) ->
  map().
proplist_to_map(Proplist) ->
  to_map(Proplist, [], #{}).

-spec proplist_to_map([{term(), term() | [term()]}], term() | [term()]) ->
  map().
proplist_to_map(Proplist, PrefixKeys) when is_list(PrefixKeys) ->
  Map = proplist_to_map(Proplist),
  maps:fold(
    fun(K, V, NewMap) ->
      NewMap#{PrefixKeys ++ K => V}
    end, #{}, Map);
proplist_to_map(Proplist, PrefixKey) ->
  proplist_to_map(Proplist, [PrefixKey]).

%%====================================================================
%% Internal functions
%%====================================================================

-spec to_map([{term(), term() | [term()]}], [term()], map()) ->
  map().
to_map([], _, Map) ->
  Map;
to_map([{Key, Value} | Rest], KeyPath, Map) when is_list(Value) ->
  ValuePath = [Key | KeyPath],

  NewMap = case Value of
             [{_KeyV, _ValueV} | _RestV] ->
               to_map(Value, ValuePath, Map);
             _ ->
               Map#{lists:reverse(ValuePath) => Value}
           end,
  to_map(Rest, KeyPath, NewMap);
to_map([{Key, Value} | Rest], KeyPath, Map) ->
  ValuePath = [Key | KeyPath],
  to_map(Rest, KeyPath, Map#{lists:reverse(ValuePath) => Value}).
