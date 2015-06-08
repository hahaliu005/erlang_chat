-module(connection_sup).
-behaviour(supervisor).
-include("const.hrl").

-export([start_link/0, start_connection/1]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_connection(Socket) ->
  supervisor:start_child(?MODULE,[Socket]).


init([]) ->
  {ok, {?SIMPLE_ONE_FOR_ONE, ?SUP_CHILD(connection)}}.
