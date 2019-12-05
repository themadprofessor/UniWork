%%%-------------------------------------------------------------------
%%% @author stuart
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(totientrangeWorker).

-behaviour(gen_server).

-export([start/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).


-record(totientrangeWorker_state, {name}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start(Name) ->
  gen_server:start({local, Name}, ?MODULE, [Name], []).

init([Name]) ->
  {ok, #totientrangeWorker_state{name = Name}}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(finished, State) ->
  gen_server:stop(State#totientrangeWorker_state.name);

handle_info({range, Lower, Upper}, State) ->
  io:format("~p: Computing range: ~p ~p~n", [State#totientrangeWorker_state.name, Lower, Upper]),
  Res = sumTotient(Lower, Upper),
  server ! {worker_done, Res, State#totientrangeWorker_state.name}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% Phil Trinder 20/10/2018

%% This program calculates the sum of the totients between a lower and an 
%% upper limit. It is based on earlier work by: Nathan Charles, 
%% Hans-Wolfgang Loidl and Colin Runciman

%% The comments provide (executable) Haskell specifications of the functions

%% hcf x 0 = x
%% hcf x y = hcf y (rem x y)

hcf(X,0) -> X;
hcf(X,Y) -> hcf(Y,X rem Y).

%% relprime x y = hcf x y == 1

relprime(X,Y) ->
  V = hcf(X,Y),
  if
    V == 1
      -> true;
    true
      -> false
  end.

%%euler n = length (filter (relprime n) (mkList n))

euler(N) ->
  RelprimeN = fun(Y) -> relprime(N,Y) end,
  length (lists:filter(RelprimeN,(lists:seq(1,N)))).

%%sumTotient lower upper = sum (map euler [lower, lower+1 .. upper])

sumTotient(Lower,Upper) ->
  lists:sum(lists:map(fun euler/1,lists:seq(Lower, Upper))).
