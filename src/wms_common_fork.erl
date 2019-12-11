%%%-------------------------------------------------------------------
%%% @author Attila Makra
%%% @copyright (C) 2019, Attila Makra.
%%% @doc
%%% Forking tast to processes
%%% @end
%%% Created : 05. May 2019 08:54
%%%-------------------------------------------------------------------
-module(wms_common_fork).
-author("Attila Makra").

-include("wms_common.hrl")
.
-export([fork/4]).

%% =============================================================================
%% Types
%% =============================================================================

-type pid_store() :: #{pid() => {integer(), reference()}}.

%% =============================================================================
%% API functions
%% =============================================================================

-spec fork(fun(), [term()], [term()], pos_integer()) ->
  [fork_retval()].
fork(Fun, ExtraArgs, Elements, Timeout) ->
  Arguments = create_args(ExtraArgs, Elements, 1, []),
  Pids = spawn_workers(Fun, Arguments, #{}),
  receive_replies(Pids, Timeout, length(Arguments)).

%% =============================================================================
%% Private functions
%% =============================================================================

-spec create_args([term()], [term()], integer(), [{integer(), [term()]}]) ->
  [{integer(), [term()]}].
create_args(_, [], _, Accu) ->
  % in parallel execution cannot be guarantee order of execution but
  % it try to keep arguments order.
  lists:reverse(Accu);
create_args(ExtraArgs, [H | T], ID, Accu) ->
  Arg = {ID, [H | ExtraArgs]},
  create_args(ExtraArgs, T, ID + 1, [Arg | Accu]).

-spec spawn_workers(fun(), [{integer(), [term()]}], pid_store()) ->
  pid_store().
spawn_workers(_, [], Pids) ->
  Pids;
spawn_workers(Fun, [{ID, Argument} | RestArguments], Pids) ->
  {Pid, Ref} = spawn_monitor(
    fun() ->
      Result =
        try
          {ID, {ok, apply(Fun, Argument)}}
        catch
          Class:Reason ->
            {ID, {Class, Reason}}
        end,
      exit(Result)
    end),
  spawn_workers(Fun, RestArguments, Pids#{Pid => {ID, Ref}}).

-spec receive_replies(pid_store(), pos_integer(), pos_integer()) ->
  [fork_retval()].
receive_replies(Pids, Timeout, Length) ->
  receive_replies(Pids, Timeout, 0, Length, []).

-spec receive_replies(pid_store(), pos_integer(), non_neg_integer(),
                      pos_integer(), [{integer(), fork_retval()}]) ->
                       [fork_retval()].
receive_replies(_, _, RepCount, WorkerCnt, Replies) when RepCount =:= WorkerCnt ->
  lists:map(
    fun({_, Reply}) ->
      Reply
    end,
    lists:sort(Replies));
receive_replies(Pids, Timeout, RepCount, WorkerCnt, Replies) ->
  {NewPids, NewRepCnt, NewReplies} =
    receive
      {'DOWN', Ref, process, Pid, Reply} ->
        process_reply(Pids, Pid,
                      RepCount,
                      Replies, Reply,
                      Ref)
    after
      Timeout ->
        {Pids, WorkerCnt, process_timeout(Pids, Replies)}
    end,
  receive_replies(NewPids, Timeout, NewRepCnt, WorkerCnt, NewReplies).

-spec process_reply(pid_store(), pid(), non_neg_integer(),
                    [{integer(), fork_retval()}],
                    {integer(), fork_retval()} | term(), reference()) ->
                     {pid_store(), non_neg_integer(), [{integer(), fork_retval()}]}.
process_reply(Pids, Pid, RepCount, Replies, Reply, Ref) ->
  erlang:demonitor(Ref, [info, flush]),
  case maps:take(Pid, Pids) of
    {_, RestPids} ->
      {RestPids, RepCount + 1, [Reply | Replies]};
    error ->
      {Pids, RepCount, Replies}
  end.

-spec process_timeout(pid_store(), [{integer(), fork_retval()}]) ->
  [{integer(), fork_retval()}].
process_timeout(Pids, Replies) ->
  maps:fold(
    fun(_, {ID, Ref}, Accu) ->
      erlang:demonitor(Ref, [info, flush]),
      [{ID, {error, timeout}} | Accu]
    end, Replies, Pids).