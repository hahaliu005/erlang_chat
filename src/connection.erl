-module(connection).
-behaviour(gen_server).
-include("const.hrl").

%% API.
-export([start_link/1]).

%% gen_server.
-export([init/1, set_socket/2]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
          socket
}).

%% API.

start_link(Socket) ->
	gen_server:start_link(?MODULE, [Socket], [{timeout,infinity}]).

%% gen_server.

init([Socket]) ->
  {ok, #state{socket=Socket}}.

set_socket(ConnPid, Socket) ->
  gen_server:cast(ConnPid, {set_socket, Socket}).

keep_alive_or_close(Keep, State) ->
  if
    Keep /= keep_alive -> gen_tcp:close(State#state.socket),
                          {stop, normal, State};
    true -> {noreply, State}
  end.

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast({set_socket, Socket}, State) ->
  inet:setopts(Socket, [{active, once}, {packet, 0}, binary]),
  keep_alive_or_close(keep_alive, State#state{socket = Socket});
handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({tcp, Socket, Data}, State) ->
  inet:setopts(Socket, [{active, once}, {packet, 0}, binary]),
  ?ERROR("Data:~p~n", [Data]),
  {noreply, State};
handle_info(_Info, State) ->
  ?ERROR("connection:handle_info: Info:~p ~n", [_Info]),
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
