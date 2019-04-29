-module(wms_common).

-include("wms_common.hrl").

%% API exports
-export([timestamp/0,
         elapsed/3,
         elapsed/2]).

%%====================================================================
%% API functions
%%====================================================================

%% -----------------------------------------------------------------------------
%% Time handling functions
%% -----------------------------------------------------------------------------
-spec timestamp() ->
  {atom(), integer()}.
timestamp() ->
  {native, erlang:system_time()}.

-spec elapsed(timestamp(), timestamp(), time_unit()) ->
  timestamp().
elapsed({EUnit, Earlier}, {LUnit, Later}, TimeUnit) ->
  {TimeUnit,
   erlang:convert_time_unit(Later, LUnit, TimeUnit) -
     erlang:convert_time_unit(Earlier, EUnit, TimeUnit)}.

-spec elapsed(timestamp(), time_unit()) ->
  timestamp().
elapsed(Earlier, TimeUnit) ->
  elapsed(Earlier, timestamp(), TimeUnit).

%%====================================================================
%% Internal functions
%%====================================================================
