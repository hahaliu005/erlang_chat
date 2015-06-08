-module(connection_manager).
-behaviour(gen_server).
-include("const.hrl").

%% API.
-export([start_link/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
          socket
}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	Result = gen_server:start_link({local, ?MODULE}, ?MODULE, [], [{timeout,infinity}]),
  loop_accept(),
  Result.

%% gen_server.

init([]) ->
  Options = [binary, {packet, 0}, {reuseaddr, true},
             {backlog, 1024}, {active, false}],
  {ok, ListenSocket} = gen_tcp:listen(?LISTEN_PORT, Options),
  {ok, #state{socket=ListenSocket}}.

loop_accept() ->
  gen_server:cast(?MODULE, accept_loop).

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast(accept_loop, State = #state{socket=ListenSocket}) ->
  case (catch gen_tcp:accept(ListenSocket)) of
    {ok, Socket} -> 
      {ok, ConnPid} = connection_sup:start_connection(Socket),
      ets:insert(?ETS_CONN, {ConnPid, 1}),
      gen_tcp:controlling_process(Socket, ConnPid),
      connection:set_socket(ConnPid, Socket);
    {error, Reason} -> 
      ?ERROR("error accept failed:~p~n", [Reason]);
    {'EXIT', Reason} -> 
      ?ERROR("exit accept failed:~p~n", [Reason])
  end,
  gen_server:cast(?MODULE, accept_loop),
  {noreply, State};

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
