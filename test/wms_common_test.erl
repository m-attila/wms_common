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

add_test() ->
  ?assertEqual({millisecond, 1000}, wms_common:add({millisecond, 100},
                                                   {millisecond, 900})),
  ?assertEqual({second, 2}, wms_common:add({second, 1},
                                           {millisecond, 1000})).

get_proplist_value_test() ->
  % empty proplist
  ?assertEqual(default, wms_common:get_proplist_value([], key, default)),
  ?assertEqual(default, wms_common:get_proplist_value([], [key], default)),
  ?assertEqual(default, wms_common:get_proplist_value([], [key1, key2], default)),

  % key not found
  ?assertEqual(default, wms_common:get_proplist_value([{main, value}],
                                                      key, default)),
  ?assertEqual(default, wms_common:get_proplist_value([{main, value}],
                                                      [key], default)),
  ?assertEqual(default, wms_common:get_proplist_value([{main, value}],
                                                      [key1, key2], default)),
  ?assertEqual(default, wms_common:get_proplist_value([{main, [{sub, value}]}],
                                                      [key1, key2], default)),

  % key found
  ?assertEqual(value, wms_common:get_proplist_value([{main, value}],
                                                    main, default)),
  ?assertEqual(value, wms_common:get_proplist_value([{main, value}],
                                                    [main], default)),
  ?assertEqual(value, wms_common:get_proplist_value([{main, [{sub, value}]}],
                                                    [main, sub], default)),
  ok.

proplist_to_map_test() ->
  % proplist is empty
  ?assertEqual(#{}, wms_common:proplist_to_map([])),

  % proplist is not empty
  List =
    [
      {mode, test},
      {nodes, [
        {database, [n1@node, n2@node]},
        {tools, [t1@node, t2@node]},
        {application, [
          {app1, [na11@node, na12@node]},
          {app2, [na21@node, na22@node]},
          {timeout, 12}
        ]}
      ]},
      {version, "1.0.0"}
    ],
  Expected = #{
               [mode] => test,
               [nodes, database] => [n1@node, n2@node],
               [nodes, tools] => [t1@node, t2@node],
               [nodes, application, app1] => [na11@node, na12@node],
               [nodes, application, app2] => [na21@node, na22@node],
               [nodes, application, timeout] => 12,
               [version] => "1.0.0"
             },
  ?assertEqual(Expected, wms_common:proplist_to_map(List)),

  % keys with prefix-1
  Expected1 = #{
                [myapp, mode] => test,
                [myapp, nodes, database] => [n1@node, n2@node],
                [myapp, nodes, tools] => [t1@node, t2@node],
                [myapp, nodes, application, app1] => [na11@node, na12@node],
                [myapp, nodes, application, app2] => [na21@node, na22@node],
                [myapp, nodes, application, timeout] => 12,
                [myapp, version] => "1.0.0"
              },
  ?assertEqual(Expected1, wms_common:proplist_to_map(List, myapp)),
% keys with prefix-1
  Expected2 = #{
                [myapp, test, mode] => test,
                [myapp, test, nodes, database] => [n1@node, n2@node],
                [myapp, test, nodes, tools] => [t1@node, t2@node],
                [myapp, test, nodes, application, app1] => [na11@node, na12@node],
                [myapp, test, nodes, application, app2] => [na21@node, na22@node],
                [myapp, test, nodes, application, timeout] => 12,
                [myapp, test, version] => "1.0.0"
              },
  ?assertEqual(Expected2, wms_common:proplist_to_map(List, [myapp, test])).

host_test() ->
  ?assertEqual("nohost", wms_common:get_hostname()),
  ?assertEqual("makraat", wms_common:get_hostname("wms1@makraat")),
  ?assertEqual("wms1@nohost", wms_common:add_host("wms1")).