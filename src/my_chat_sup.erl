-module(my_chat_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,start_child/0]).

%% Supervisor callbacks
-export([init/1]).

-include("const.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  ?ERROR("~p start_link~n",[?MODULE]),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child() ->
  Childs = [
            ?CHILD(id_pool, worker),
            ?CHILD(player_manager, supervisor),
            ?CHILD(room_manager, supervisor),
            ?CHILD(connection_manager, worker),
            ?CHILD(connection_sup, supervisor)
           ],
  lists:foreach(fun(Child) ->
                    supervisor:start_child(?MODULE,Child)
                end, Childs).

init_ets_table() ->
  lists:foreach(fun(Table) ->
                    ets:new(Table, [ordered_set, public, named_table, {keypos, 1}])
                end, ?ETS_TABLES).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  init_ets_table(),
  ?ERROR("~p init~n", [?MODULE]),
  {ok, { {one_for_one, 5, 10}, []} }.
