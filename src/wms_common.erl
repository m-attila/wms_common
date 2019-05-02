-module(wms_common).

-include("wms_common.hrl").

%% API exports
-export([timestamp/0,
         elapsed/3,
         elapsed/2,
         compare/2,
         convert/2,
         add/2, get_proplist_value/3, proplist_to_map/1, proplist_to_map/2]).

%%====================================================================
%% API functions
%%====================================================================

%% -----------------------------------------------------------------------------
%% Time handling functions
%% -----------------------------------------------------------------------------
%% @doc
%%
%%-------------------------------------------------------------------
%%
%% ### Function
%% timestamp/0
%% ###### Purpose
%% Generate current timestamp.
%% ###### Arguments
%%
%% ###### Returns
%%
%%-------------------------------------------------------------------
%%
%% @end
-spec timestamp() ->
  {atom(), integer()}.
timestamp() ->
  {native, erlang:system_time()}.

%% @doc
%%
%%-------------------------------------------------------------------
%%
%% ### Function
%% convert/2
%% ###### Purpose
%% Convert timestamp for given time unit
%% ###### Arguments
%%
%% ###### Returns
%%
%%-------------------------------------------------------------------
%%
%% @end
-spec convert(timestamp(), time_unit()) ->
  timestamp().
convert({Unit, _} = Timestamp, Unit) ->
  Timestamp;
convert({FromUnit, Value}, ToUnit) ->
  {ToUnit, erlang:convert_time_unit(Value, FromUnit, ToUnit)}.

%% @doc
%%
%%-------------------------------------------------------------------
%%
%% ### Function
%% elapsed/3
%% ###### Purpose
%% Calculate elapsed time from timestamps and convert to given time unit.
%% ###### Arguments
%%
%% ###### Returns
%%
%%-------------------------------------------------------------------
%%
%% @end
-spec elapsed(timestamp(), timestamp(), time_unit()) ->
  timestamp().
elapsed({EUnit, Earlier}, {LUnit, Later}, TimeUnit) ->
  {TimeUnit,
   erlang:convert_time_unit(Later, LUnit, TimeUnit) -
     erlang:convert_time_unit(Earlier, EUnit, TimeUnit)}.

%% @doc
%%
%%-------------------------------------------------------------------
%%
%% ### Function
%% elapsed/2
%% ###### Purpose
%% Calculate elapsed time by using current timestamp, and convert to given
%% time unit.
%% ###### Arguments
%%
%% ###### Returns
%%
%%-------------------------------------------------------------------
%%
%% @end

-spec elapsed(timestamp(), time_unit()) ->
  timestamp().
elapsed(Earlier, TimeUnit) ->
  elapsed(Earlier, timestamp(), TimeUnit).

%% @doc
%%
%%-------------------------------------------------------------------
%%
%% ### Function
%% compare/2
%% ###### Purpose
%% Compare timestamps T1, T2
%% ###### Arguments
%%
%% ###### Returns
%% -1 if T1 < T2
%% 0 if T1 = T2
%% 1 if T1 > T2
%%-------------------------------------------------------------------
%%
%% @end

-spec compare(timestamp(), timestamp()) ->
  -1 | 0 | 1.
compare({Unit, Value1}, {Unit, Value2}) ->
  sgn(Value1 - Value2);
compare({Unit1, Value1}, {Unit2, Value2}) ->
  sgn(erlang:convert_time_unit(Value1, Unit1, native) -
        erlang:convert_time_unit(Value2, Unit2, native)).


%% @doc
%%
%%-------------------------------------------------------------------
%%
%% ### Function
%% add/2
%% ###### Purpose
%% Add timestamp and offset
%% ###### Arguments
%%
%% ###### Returns
%%
%%-------------------------------------------------------------------
%%
%% @end
-spec add(timestamp(), timestamp()) ->
  timestamp().
add({Unit, Value}, {Unit, OffsetValue}) ->
  {Unit, Value + OffsetValue};
add({Unit1, _} = Timestamp, Offset) ->
  ConvertedOffset = convert(Offset, Unit1),
  add(Timestamp, ConvertedOffset).

%% @doc
%%
%%-------------------------------------------------------------------
%%
%% ### Function
%% get_value/3
%% ###### Purpose
%% Return value from embedded property list.
%% ###### Arguments
%% * Proplist - property list
%% * Keys - list of keys
%% * Default - default value.
%% ###### Returns
%% Value in proplist or default value if does not found.
%%-------------------------------------------------------------------
%%
%% @end
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
-spec sgn(integer()) ->
  integer().
sgn(0) ->
  0;
sgn(Number) ->
  Number div abs(Number).

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