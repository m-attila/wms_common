-module(wms_common).

-include("wms_common.hrl").

%% API exports
-export([timestamp/0,
         elapsed/3,
         elapsed/2,
         compare/2,
         convert/2,
         add/2,
         get_proplist_value/3,
         proplist_to_map/1,
         proplist_to_map/2,
         fork/4,
         get_hostname/1,
         get_hostname/0, add_host/1]).

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
  wms_common_timestamp:timestamp().

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
convert(Timestamp, ToUnit) ->
  wms_common_timestamp:convert(Timestamp, ToUnit).

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
elapsed(Earlier, Later, TimeUnit) ->
  wms_common_timestamp:elapsed(Earlier, Later, TimeUnit).

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
compare(T1, T2) ->
  wms_common_timestamp:compare(T1, T2).

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
add(T1, T2) ->
  wms_common_timestamp:add(T1, T2).

%% -----------------------------------------------------------------------------
%% Proplist handling functions
%% -----------------------------------------------------------------------------

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
get_proplist_value(Proplist, KeyOrKeys, Default) ->
  wms_common_proplist:get_proplist_value(Proplist, KeyOrKeys, Default).

-spec proplist_to_map([{term(), term() | [term()]}]) ->
  map().
proplist_to_map(Proplist) ->
  wms_common_proplist:proplist_to_map(Proplist).

-spec proplist_to_map([{term(), term() | [term()]}], term() | [term()]) ->
  map().
proplist_to_map(Proplist, PrefixKeys) ->
  wms_common_proplist:proplist_to_map(Proplist, PrefixKeys).

%% -----------------------------------------------------------------------------
%% Process handling functions
%% -----------------------------------------------------------------------------
-spec fork(fun(), [term()], [term()], pos_integer()) ->
  [fork_retval()].
fork(Fun, ExtraArgs, Elements, Timeout) ->
  wms_common_fork:fork(Fun, ExtraArgs, Elements, Timeout).

%% -----------------------------------------------------------------------------
%% Host functions
%% -----------------------------------------------------------------------------

-spec get_hostname() ->
  string().
get_hostname() ->
  get_hostname(node()).

-spec get_hostname(node()) ->
  string().
get_hostname(Node) when is_atom(Node) ->
  get_hostname(atom_to_list(Node));
get_hostname(Node) ->
  lists:nth(2, re:split(Node,
                        "@", [{return, list}, {parts, 2}])).

-spec add_host(string()) ->
  string().
add_host(Name) ->
  Name ++ "@" ++ get_hostname().

