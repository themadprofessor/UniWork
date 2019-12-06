%%%-------------------------------------------------------------------
%%% @author stuart
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(watcher).

-behaviour(supervisor).

-export([start/1, init/1]).

start(Name) ->
  supervisor:start_link({local, list_to_atom(lists:join(["sup_", atom_to_list(Name)], ""))}, ?MODULE, [Name]).

init([Name]) ->
  AChild = #{id => Name,
    start => {totientrangeWorker, start, [Name]},
    restart => transient,
    shutdown => 2000,
    type => worker},

  io:format("sup_~p: Started~n", [Name]),
  {ok, {#{strategy => one_for_one,
    intensity => 5,
    period => 30},
    [AChild]}
  }.
