%%%-------------------------------------------------------------------
%%% @author Attila Makra
%%% @copyright (C) 2019, Attila Makra.
%%% @doc
%%% Timestamp functions
%%% @end
%%% Created : 05. May 2019 11:36
%%%-------------------------------------------------------------------
-module(wms_common_timestamp).
-author("Attila Makra").

-include("wms_common.hrl").

%% API
-export([timestamp/0,
         convert/2,
         elapsed/3,
         compare/2,
         add/2]).

-spec timestamp() ->
  {atom(), integer()}.
timestamp() ->
  {native, erlang:system_time()}.

-spec convert(timestamp(), time_unit()) ->
  timestamp().
convert({Unit, _} = Timestamp, Unit) ->
  Timestamp;
convert({FromUnit, Value}, ToUnit) ->
  {ToUnit, erlang:convert_time_unit(Value, FromUnit, ToUnit)}.

-spec elapsed(timestamp(), timestamp(), time_unit()) ->
  timestamp().
elapsed({EUnit, Earlier}, {LUnit, Later}, TimeUnit) ->
  {TimeUnit,
   erlang:convert_time_unit(Later, LUnit, TimeUnit) -
     erlang:convert_time_unit(Earlier, EUnit, TimeUnit)}.

-spec compare(timestamp(), timestamp()) ->
  -1 | 0 | 1.
compare({Unit, Value1}, {Unit, Value2}) ->
  sgn(Value1 - Value2);
compare({Unit1, Value1}, {Unit2, Value2}) ->
  sgn(erlang:convert_time_unit(Value1, Unit1, native) -
        erlang:convert_time_unit(Value2, Unit2, native)).

-spec add(timestamp(), timestamp()) ->
  timestamp().
add({Unit, Value}, {Unit, OffsetValue}) ->
  {Unit, Value + OffsetValue};
add({Unit1, _} = Timestamp, Offset) ->
  ConvertedOffset = convert(Offset, Unit1),
  add(Timestamp, ConvertedOffset).

%%====================================================================
%% Internal functions
%%====================================================================
-spec sgn(integer()) ->
  integer().
sgn(0) ->
  0;
sgn(Number) ->
  Number div abs(Number).

