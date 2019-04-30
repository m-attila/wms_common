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
  ?assert(ElapsedMsec1 >= Min andalso ElapsedMsec1 =< Max),

  {microsecond, ElapsedMicro} = wms_common:elapsed(Earlier, Now, microsecond),
  ?assert(ElapsedMicro >= Min * 1000 andalso ElapsedMicro =< Max * 1000).

compare_test() ->
  Earlier = wms_common:timestamp(),
  timer:sleep(500),
  Later = wms_common:timestamp(),

  % same time unit

  ?assertEqual(-1, wms_common:compare(Earlier, Later)),
  ?assertEqual(1, wms_common:compare(Later, Earlier)),
  ?assertEqual(0, wms_common:compare(Earlier, Earlier)),

  % different time unit
  LaterMsec = wms_common:convert(Later, millisecond),

  ?assertEqual(-1, wms_common:compare(Earlier, LaterMsec)),
  ?assertEqual(1, wms_common:compare(LaterMsec, Earlier)),
  ?assertEqual(0, wms_common:compare(Earlier, Earlier)).

convert_test() ->
  Timestamp = {millisecond, 1000},
  ?assertEqual(Timestamp, wms_common:convert(Timestamp, millisecond)),

  Expected = {second, 1},
  ?assertEqual(Expected, wms_common:convert(Timestamp, second)).




