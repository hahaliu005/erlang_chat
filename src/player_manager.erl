-module(player_manager).
-behaviour(supervisor).
-include("const.hrl").

-export([start_link/0, create/1]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

create(Name) ->
  %% create a new player connection
  case ets:match_object(?ETS_PLAYER, #player_info{name=Name,_='_'}) of
    [#player_info{}] ->
      {error, name_exist};
    [] ->
      {ok, Pid} = supervisor:start_child(?MODULE, [Name]),
      PlayerInfo = #player_info{
                      pid=Pid,
                      name=Name
                     },
      ets:insert(?ETS_PLAYER, PlayerInfo),
      {ok, Pid}
  end.

init([]) ->
	Procs = [{undefined, {player, start_link, []}, temporary, 2000, worker, []}],
	{ok, {{simple_one_for_one, 1, 5}, Procs}}.
