-module(wms_common).

-include("wms_common.hrl").

%% API exports
-export([timestamp/0,
         elapsed/3,
         elapsed/2,
         compare/2,
         convert/2]).

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

-spec compare(timestamp(), timestamp()) ->
  -1 | 0 | 1.
compare({Unit, Value1}, {Unit, Value2}) ->
  sgn(Value1 - Value2);
compare({Unit1, Value1}, {Unit2, Value2}) ->
  sgn(erlang:convert_time_unit(Value1, Unit1, native) -
        erlang:convert_time_unit(Value2, Unit2, native)).

%%====================================================================
%% Internal functions
%%====================================================================
-spec sgn(integer()) ->
  integer().
sgn(0) ->
  0;
sgn(Number) ->
  Number div abs(Number).
