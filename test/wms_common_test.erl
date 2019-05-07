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

-define(CHK(Exp, Literal, Op),
  ?assertEqual(Exp,
               wms_common:eval_operation(Literal, Op))).

-define(CHK(Exp, Literal1, Op, Literal2),
  ?assertEqual(Exp,
               wms_common:eval_operation(Literal1, Op, Literal2))).

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

eval_op_test() ->
  test_eval_op_one_arg(),
  test_eval_op_two_arg().

test_eval_op_one_arg() ->
  ?CHK(x, x, nop),

  ?CHK(true, false, '!'),
  ?CHK(false, true, '!'),
  ?CHK({error, {'!', 12}}, 12, '!'),

  ?CHK({e1, []}, [e1], {'--', head}),
  ?CHK({e1, [e2]}, [e1, e2], {'--', head}),
  ?CHK({error, {{'--', head}, {[]}}}, [], {'--', head}),

  ?CHK({e1, []}, [e1], {'--', tail}),
  ?CHK({e2, [e1]}, [e1, e2], {'--', tail}),
  ?CHK({error, {{'--', tail}, {[]}}}, [], {'--', tail}),

  ?CHK({e1, [e1]}, [e1], {'?', head}),
  ?CHK({e1, [e1, e2]}, [e1, e2], {'?', head}),
  ?CHK({error, {{'?', head}, {[]}}}, [], {'?', head}),

  ?CHK({e1, [e1]}, [e1], {'?', tail}),
  ?CHK({e2, [e1, e2]}, [e1, e2], {'?', tail}),
  ?CHK({error, {{'?', tail}, {[]}}}, [], {'?', tail}),

  ?CHK({error, {{'?', tail}, no_list}}, no_list, {'?', tail}).


test_eval_op_two_arg() ->
  test_eval_op_two_base(),
  test_eval_op_two_date(),
  test_eval_op_two_time(),
  test_eval_op_two_list(),
  ok.

test_eval_op_two_base() ->
  % plus operator
  ?CHK("abcd", "ab", '+', "cd"),
  ?CHK(20 + 30, 20, '+', 30),
  ?CHK(20.1 + 30.2, 20.1, '+', 30.2),
  ?CHK(true, true, '+', true),
  ?CHK(true, true, '+', false),
  ?CHK(true, false, '+', true),
  ?CHK(false, false, '+', false),
  ?CHK(<<"abcd">>, <<"ab">>, '+', <<"cd">>),
  ?CHK({error, {'+', {x, y}}}, x, '+', y),

  % minus operator
  ?CHK("ab", "abcd", '-', "cd"),
  ?CHK(20 - 30, 20, '-', 30),
  ?CHK(20.1 - 30.2, 20.1, '-', 30.2),
  ?CHK(true, true, '-', false),
  ?CHK(false, true, '-', true),
  ?CHK(true, false, '-', true),
  ?CHK(false, false, '-', false),
  ?CHK({error, {'-', {x, y}}}, x, '-', y),

  % multiple operator
  ?CHK("Bc", "aBc", '*', "cBd"),
  ?CHK(20 * 30, 20, '*', 30),
  ?CHK(20.1 * 30.2, 20.1, '*', 30.2),
  ?CHK(true, true, '*', true),
  ?CHK(false, true, '*', false),
  ?CHK(false, false, '*', true),
  ?CHK(false, false, '*', false),
  ?CHK({error, {'*', {x, y}}}, x, '*', y),

  % divisor operator
  ?CHK(20 div 30, 20, '/', 30),
  ?CHK(20.1 / 30.2, 20.1, '/', 30.2),
  ?CHK({error, {'/', {x, y}}}, x, '/', y),

  % invalid operator
  ?CHK({error, {invalid, eval_operation, '?', 10}}, 10, '?'),

  ok.

test_eval_op_two_date() ->

  % date mod plus
  ?CHK({2000, 1, 2}, {1999, 12, 31}, {'+', day}, 2),
  ?CHK({2000, 2, 29}, {1999, 12, 31}, {'+', month}, 2),
  ?CHK({2001, 2, 28}, {2000, 12, 31}, {'+', month}, 2),
  ?CHK({2001, 12, 31}, {1999, 12, 31}, {'+', year}, 2),
  ?CHK({error, {{'+', year}, {{1999, 12, 31}, "apple"}}},
       {1999, 12, 31}, {'+', year}, "apple"),
  ?CHK({error, {{'+', year}, {"abc", 2}}}, "abc", {'+', year}, 2),


  % date mod minus
  ?CHK({1999, 12, 31}, {2000, 1, 2}, {'-', day}, 2),
  ?CHK({1999, 11, 30}, {2000, 1, 31}, {'-', month}, 2),
  ?CHK({2000, 12, 28}, {2001, 2, 28}, {'-', month}, 2),
  ?CHK({1999, 12, 31}, {2001, 12, 31}, {'-', year}, 2),
  ?CHK({error, {invalid, eval_operation, {'-', xx}, {{2001, 12, 31}, 2}}},
       {2001, 12, 31}, {'-', xx}, 2),
  ?CHK({error, {{'-', year}, {{2001, 12, 31}, 2.2}}},
       {2001, 12, 31}, {'-', year}, 2.2),
  ?CHK({error, {{'-', year}, {{2001, 12, 31}, "apple"}}},
       {2001, 12, 31}, {'-', year}, "apple"),

  % date set
  ?CHK({2000, 2, 29}, {2000, 2, 1}, {set, 'day'}, 'end'),
  ?CHK({2000, 2, 11}, {2000, 2, 1}, {set, 'day'}, 11),
  ?CHK({error, {{set, day}, {{2000, 2, 1}, "apple"}}},
       {2000, 2, 1}, {set, 'day'}, "apple"),
  ?CHK({error, {{set, day}, {"abc", 11}}}, "abc", {set, 'day'}, 11),

  ?CHK({2000, 12, 29}, {2000, 2, 29}, {set, 'month'}, 'end'),
  ?CHK({2000, 11, 30}, {2000, 1, 30}, {set, 'month'}, 11),
  ?CHK({error, {invalid_date, {2000, 11, 31}}},
       {2000, 1, 31}, {set, 'month'}, 11),
  ?CHK({error, {{set, day}, {{2000, 2, 1}, "apple"}}},
       {2000, 2, 1}, {set, 'day'}, "apple"),

  ?CHK({error, {{set, year}, {{2000, 2, 1}, 'end'}}},
       {2000, 2, 1}, {set, 'year'}, 'end'),
  ?CHK({2003, 2, 1}, {2000, 2, 1}, {set, 'year'}, 2003),

  % datetime mod plus
  Time = {12, 13, 14},

  ?CHK({{2000, 1, 2}, Time}, {{1999, 12, 31}, Time}, {'+', day}, 2),
  ?CHK({{2000, 2, 29}, Time}, {{1999, 12, 31}, Time}, {'+', month}, 2),
  ?CHK({{2001, 2, 28}, Time}, {{2000, 12, 31}, Time}, {'+', month}, 2),
  ?CHK({{2001, 12, 31}, Time}, {{1999, 12, 31}, Time}, {'+', year}, 2),
  ?CHK({error, {{'+', year}, {{1999, 12, 31}, "apple"}}},
       {1999, 12, 31}, {'+', year}, "apple"),

  % date mod minus
  ?CHK({{1999, 12, 31}, Time}, {{2000, 1, 2}, Time}, {'-', day}, 2),
  ?CHK({{1999, 11, 30}, Time}, {{2000, 1, 31}, Time}, {'-', month}, 2),
  ?CHK({{2000, 12, 28}, Time}, {{2001, 2, 28}, Time}, {'-', month}, 2),
  ?CHK({{1999, 12, 31}, Time}, {{2001, 12, 31}, Time}, {'-', year}, 2),
  ?CHK({error, {invalid, eval_operation, {'-', xx}, {{{2001, 12, 31}, Time}, 2}}},
       {{2001, 12, 31}, Time}, {'-', xx}, 2),
  ?CHK({error, {{'-', year}, {{{2001, 12, 31}, Time}, 2.2}}},
       {{2001, 12, 31}, Time}, {'-', year}, 2.2),
  ?CHK({error, {{'-', year}, {{{2001, 12, 31}, Time}, "apple"}}},
       {{2001, 12, 31}, Time}, {'-', year}, "apple"),

  % date set
  ?CHK({{2000, 2, 29}, Time}, {{2000, 2, 1}, Time}, {set, 'day'}, 'end'),
  ?CHK({{2000, 2, 11}, Time}, {{2000, 2, 1}, Time}, {set, 'day'}, 11),
  ?CHK({error, {{set, day}, {{{2000, 2, 1}, Time}, "apple"}}},
       {{2000, 2, 1}, Time}, {set, 'day'}, "apple"),

  ?CHK({{2000, 12, 29}, Time}, {{2000, 2, 29}, Time}, {set, 'month'}, 'end'),
  ?CHK({{2000, 11, 30}, Time}, {{2000, 1, 30}, Time}, {set, 'month'}, 11),
  ?CHK({error, {invalid_date, {{2000, 11, 31}, Time}}},
       {{2000, 1, 31}, Time}, {set, 'month'}, 11),
  ?CHK({error, {{set, day}, {{{2000, 2, 1}, Time}, "apple"}}},
       {{2000, 2, 1}, Time}, {set, 'day'}, "apple"),

  ?CHK({error, {{set, year}, {{{2000, 2, 1}, Time}, 'end'}}},
       {{2000, 2, 1}, Time}, {set, 'year'}, 'end'),
  ?CHK({{2003, 2, 1}, Time}, {{2000, 2, 1}, Time}, {set, 'year'}, 2003),

  ok.

test_eval_op_two_time() ->
  % time mod plus
  ?CHK({0, 0, 9}, {23, 59, 59}, {'+', 'second'}, 10),
  ?CHK({0, 9, 59}, {23, 59, 59}, {'+', 'minute'}, 10),
  ?CHK({9, 59, 59}, {23, 59, 59}, {'+', 'hour'}, 10),
  ?CHK({error, {{'+', second}, {{23, 59, 59}, 1.1}}},
       {23, 59, 59}, {'+', 'second'}, 1.1),
  ?CHK({error, {invalid, eval_operation, {'+', badop}, {{23, 59, 59}, 1}}},
       {23, 59, 59}, {'+', 'badop'}, 1),
  ?CHK({error, {{'+', hour}, {"apple", 10}}}, "apple", {'+', 'hour'}, 10),

  % time mod minus
  ?CHK({23, 59, 59}, {0, 0, 9}, {'-', 'second'}, 10),
  ?CHK({23, 59, 59}, {0, 9, 59}, {'-', 'minute'}, 10),
  ?CHK({23, 59, 59}, {9, 59, 59}, {'-', 'hour'}, 10),
  ?CHK({error, {{'-', second}, {{23, 59, 59}, 1.1}}},
       {23, 59, 59}, {'-', 'second'}, 1.1),
  ?CHK({error, {invalid, eval_operation, {'-', badop}, {{23, 59, 59}, 1}}},
       {23, 59, 59}, {'-', 'badop'}, 1),

  % time set
  ?CHK({10, 0, 25}, {10, 0, 0}, {'set', 'second'}, 25),
  ?CHK({10, 25, 0}, {10, 0, 0}, {'set', 'minute'}, 25),
  ?CHK({5, 0, 0}, {10, 0, 0}, {'set', 'hour'}, 5),
  ?CHK({error, {invalid_time, {10, 0, 60}}}, {10, 0, 0}, {'set', 'second'}, 60),
  ?CHK({error, {invalid_time, {10, 0, x}}}, {10, 0, 0}, {'set', 'second'}, x),
  ?CHK({error, {invalid_time, {10, 60, 0}}}, {10, 0, 0}, {'set', 'minute'}, 60),
  ?CHK({error, {invalid_time, {25, 0, 0}}}, {10, 0, 0}, {'set', 'hour'}, 25),
  ?CHK({error, {{set, hour}, {"apple", 5}}}, "apple", {'set', 'hour'}, 5),

  % datetime mod plus
  DateF = {2000, 12, 31},
  DateT = {2001, 1, 1},
  ?CHK({DateT, {0, 0, 9}}, {DateF, {23, 59, 59}}, {'+', 'second'}, 10),
  ?CHK({DateT, {0, 9, 59}}, {DateF, {23, 59, 59}}, {'+', 'minute'}, 10),
  ?CHK({DateT, {9, 59, 59}}, {DateF, {23, 59, 59}}, {'+', 'hour'}, 10),
  ?CHK({error, {{'+', second}, {{DateF, {23, 59, 59}}, 1.1}}},
       {DateF, {23, 59, 59}}, {'+', 'second'}, 1.1),
  ?CHK({error, {invalid, eval_operation,
                {'+', badop},
                {{DateF, {23, 59, 59}}, 1}}},
       {DateF, {23, 59, 59}}, {'+', 'badop'}, 1),

  % datetime mod minus
  DateF1 = {2001, 1, 1},
  DateT1 = {2000, 12, 31},
  ?CHK({DateT1, {23, 59, 59}}, {DateF1, {0, 0, 9}}, {'-', 'second'}, 10),
  ?CHK({DateT1, {23, 59, 59}}, {DateF1, {0, 9, 59}}, {'-', 'minute'}, 10),
  ?CHK({DateT1, {23, 59, 59}}, {DateF1, {9, 59, 59}}, {'-', 'hour'}, 10),
  ?CHK({error, {{'-', second}, {{DateF1, {23, 59, 59}}, 1.1}}},
       {DateF1, {23, 59, 59}}, {'-', 'second'}, 1.1),
  ?CHK({error, {invalid, eval_operation, {'-', badop},
                {{DateF1, {23, 59, 59}}, 1}}},
       {DateF1, {23, 59, 59}}, {'-', 'badop'}, 1),

  % datetime set
  Date = {2001, 1, 1},
  ?CHK({Date, {10, 0, 25}}, {Date, {10, 0, 0}}, {'set', 'second'}, 25),
  ?CHK({Date, {10, 25, 0}}, {Date, {10, 0, 0}}, {'set', 'minute'}, 25),
  ?CHK({Date, {5, 0, 0}}, {Date, {10, 0, 0}}, {'set', 'hour'}, 5),
  ?CHK({error, {invalid_time, {Date, {10, 0, 60}}}}, {Date, {10, 0, 0}},
       {'set', 'second'}, 60),
  ?CHK({error, {invalid_time, {Date, {10, 0, x}}}}, {Date, {10, 0, 0}},
       {'set', 'second'}, x),
  ?CHK({error, {invalid_time, {Date, {10, 60, 0}}}}, {Date, {10, 0, 0}},
       {'set', 'minute'}, 60),
  ?CHK({error, {invalid_time, {Date, {25, 0, 0}}}}, {Date, {10, 0, 0}},
       {'set', 'hour'}, 25).

test_eval_op_two_list() ->
  ?CHK([1, 2, 3], [2, 3], {'++', head}, 1),
  ?CHK([1, 2, 3], [1, 2], {'++', tail}, 3),
  ?CHK({error, {invalid, eval_operation, {'++', other}, {[1, 2], 3}}}, [1, 2],
       {'++', other}, 3),
  ?CHK({error, {{'++', tail}, {x, 3}}}, x, {'++', tail}, 3),
  ok.