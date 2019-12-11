%%%-------------------------------------------------------------------
%%% @author Attila Makra
%%% @copyright (C) 2019, Attila Makra.
%%% @doc
%%%
%%% @end
%%% Created : 08. Nov 2019 20:23
%%%-------------------------------------------------------------------
-module(wms_common_id_test).
-author("Attila Makra").

-include_lib("eunit/include/eunit.hrl").

generate_unique_id_test() ->
  ShortID = wms_common_id:generate_unique_id(8),
  ?assert(byte_size(ShortID) >= 8),
  LongID = wms_common_id:generate_unique_id(128),
  ?assert(byte_size(LongID) >= 128).

