%%%-------------------------------------------------------------------
%%% @author Attila Makra
%%% @copyright (C) 2019, OTP Bank Nyrt.
%%% @doc
%%%
%%% @end
%%% Created : 29. Apr 2019 20:25
%%%-------------------------------------------------------------------
-module(wms_common_test).
-author("Attila Makra").

-include("wms_common.hrl").
-include_lib("eunit/include/eunit.hrl").

elapsed_test() ->
  Earlier = wms_common:timestamp(),
  timer:sleep(500),

  Min = 480,
  Max = 520,

  {millisecond, ElapsedMsec} = wms_common:elapsed(Earlier, millisecond),
  ?assert(ElapsedMsec >= Min andalso ElapsedMsec =< Max),

  Now = wms_common:timestamp(),

  {millisecond, ElapsedMsec1} = wms_common:elapsed(Earlier, Now, millisecond),
  ?assert(ElapsedMsec1 >= Min andalso ElapsedMsec1 =< Max).

