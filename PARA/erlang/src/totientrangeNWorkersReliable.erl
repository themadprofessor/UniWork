%%%-------------------------------------------------------------------
%%% @author Stuart Reilly 2258082
%%%-------------------------------------------------------------------
-module(totientrangeNWorkersReliable).

-behaviour(gen_server).

-export([start/0, runner/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-record(totientrangeNWorkersReliable_state, {remaining_workers, workers,
  current_sum, timestamp}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

runner(Lower, Upper, Num) ->
  start(),
  server ! {range, Lower, Upper, Num}.

start() ->
  gen_server:start_link({local, server}, ?MODULE, [], []).

init([]) ->
  io:format("Server: Started~n"),
  {ok, #totientrangeNWorkersReliable_state{ remaining_workers = 0,
    workers = [], current_sum = 0 }}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(finished, State) ->
  lists:foreach(fun gen_server:stop/1,
    State#totientrangeNWorkersReliable_state.workers),
  gen_server:stop(server);

handle_info({worker_done, Res, Name}, State) ->
  if
    State#totientrangeNWorkersReliable_state.remaining_workers == 1 ->
      io:format("Server: Sum of totients: ~p~n",
        [State#totientrangeNWorkersReliable_state.current_sum + Res]),
      {_, S, US} = State#totientrangeNWorkersReliable_state.timestamp,
      printElapsed(S, US),
      NewState = #totientrangeNWorkersReliable_state{
        current_sum = 0,
        remaining_workers = 0,
        workers = []
      };
    true -> {
      NewState = #totientrangeNWorkersReliable_state{
        current_sum =
          State#totientrangeNWorkersReliable_state.current_sum + Res,
        remaining_workers =
          State#totientrangeNWorkersReliable_state.remaining_workers-1,
        workers = lists:delete(Name,
          State#totientrangeNWorkersReliable_state.workers),
        timestamp = State#totientrangeNWorkersReliable_state.timestamp
      }
    }
  end,
  {noreply, NewState};

handle_info({range, Lower, Upper, Num}, _State) ->
  Inc = ((Upper - Lower) div Num)+1,
  NewState = #totientrangeNWorkersReliable_state{
    remaining_workers = Num,
    workers = build_worker_list(Num, Inc),
    current_sum = 0,
    timestamp = os:timestamp()
  },
  {noreply, NewState}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
spawn_worker(Name, Lower, Upper) ->
  watcher:start(Name),
  Name ! {range, Lower, Upper},
  Name.

build_worker_list(0, _Inc) -> [];
build_worker_list(N, Inc) ->
  [spawn_worker(workerName(N), ((N-1) * Inc) + 1, N * Inc) |
    build_worker_list(N-1, Inc)].

workerName(Num) ->
  list_to_atom( "worker" ++ integer_to_list( Num )).

printElapsed(S,US) ->
  {_, S2, US2} = os:timestamp(),
  %% Adjust Seconds if completion Microsecs > start Microsecs
  if
    US2-US < 0 ->
      S3 = S2-1,
      US3 = US2+1000000;
    true ->
      S3 = S2,
      US3 = US2
  end,
  io:format("Time taken in Secs, MicroSecs ~p ~p~n",[S3-S,US3-US]).