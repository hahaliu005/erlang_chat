-module(room_manager).
-behaviour(supervisor).
-include("const.hrl").

-export([start_link/0, create/1]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

create(_Name) ->
  %% todo
  %% create a new player
  supervisor:start_child(?MODULE, [_Name]).

init([]) ->
	Procs = [{undefined, {player, start_link, []}, temporary, 2000, worker, []}],
	{ok, {{simple_one_for_one, 1, 5}, Procs}}.
