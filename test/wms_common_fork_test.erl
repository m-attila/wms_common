%%%-------------------------------------------------------------------
%%% @author Attila Makra
%%% @copyright (C) 2019, OTP Bank Nyrt.
%%% @doc
%%% Unit tests for wms_common_fork
%%% @end
%%% Created : 05. May 2019 10:33
%%%-------------------------------------------------------------------
-module(wms_common_fork_test).
-author("Attila Makra").

-include_lib("eunit/include/eunit.hrl").

positive_test() ->
  Timeout = 1000,
  Fun =
    fun(X) ->
      timer:sleep(rand:uniform(300)),
      {value, X}
    end,

  % no task
  ?assertEqual([], wms_common:fork(Fun, [], [], Timeout)),

  ElementCount = 1000,

  Elements = [X || X <- lists:seq(1, ElementCount)],
  Expected = [{ok, {value, X}} || X <- lists:seq(1, ElementCount)],
  % simple tasks
  ?assertEqual(Expected, wms_common_fork:fork(Fun, [], Elements, Timeout)),

  % extra args
  Fun1 =
    fun(X, Y) ->
      timer:sleep(rand:uniform(300)),
      {value, X, Y}
    end,
  Expected1 = [{ok, {value, X, extra}} || X <- lists:seq(1, ElementCount)],
  % simple tasks
  ?assertEqual(Expected1, wms_common_fork:fork(Fun1, [extra], Elements, Timeout)),

  ok.

error_test() ->
  Timeout = 1000,

  % throw exception

  FunThrow =
    fun(X) when X rem 2 == 0 ->
      timer:sleep(rand:uniform(300)),
      {value, X};
       (X) ->
         timer:sleep(rand:uniform(300)),
         throw({bad_number, X})
    end,

  ElementCount = 1000,

  Elements = [X || X <- lists:seq(1, ElementCount)],
  Expected = [case X rem 2 == 0 of
                true ->
                  {ok, {value, X}};
                false ->
                  {throw, {bad_number, X}}
              end
              || X <- lists:seq(1, ElementCount)],
  ?assertEqual(Expected, wms_common_fork:fork(FunThrow, [], Elements, Timeout)),

  % error occurred

  FunError =
    fun(X) when X rem 2 == 0 ->
      timer:sleep(rand:uniform(300)),
      {value, X};
       (X) ->
         timer:sleep(rand:uniform(300)),
         X div 0
    end,

  Expected1 = [case X rem 2 == 0 of
                 true ->
                   {ok, {value, X}};
                 false ->
                   {error, badarith}
               end
               || X <- lists:seq(1, ElementCount)],
  ?assertEqual(Expected1, wms_common_fork:fork(FunError, [], Elements, Timeout)),
  ok.

timeout_test() ->
  Timeout = 100,

  FunThrow =
    fun(X) when X rem 2 == 0 ->
      timer:sleep(rand:uniform(Timeout div 2)),
      {value, X};
       (X) ->
         % it will be timeouted.
         timer:sleep(Timeout * 2 + rand:uniform(Timeout div 2)),
         {value, X}
    end,

  ElementCount = 1000,

  Elements = [X || X <- lists:seq(1, ElementCount)],
  Expected = [case X rem 2 == 0 of
                true ->
                  {ok, {value, X}};
                false ->
                  {error, timeout}
              end
              || X <- lists:seq(1, ElementCount)],
  ?assertEqual(Expected, wms_common_fork:fork(FunThrow, [], Elements, Timeout)),

  ok.

foreign_process_down_test() ->
  Timeout = 1200,
  Fun =
    fun(X) ->
      timer:sleep(800 + rand:uniform(200)),
      {value, X}
    end,


  ElementCount = 1000,

  Elements = [X || X <- lists:seq(1, ElementCount)],
  Expected = [{ok, {value, X}} || X <- lists:seq(1, ElementCount)],

  % starts monitored, but foreign process
  spawn_monitor(fun() -> timer:sleep(300), exit(message) end),
  % simple tasks
  ?assertEqual(Expected, wms_common_fork:fork(Fun, [], Elements, Timeout)).