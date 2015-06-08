-module(player).
-behaviour(gen_server).

%% API.
-export([start_link/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
          name
}).

%% API.

start_link(Name) ->
	gen_server:start_link(?MODULE, [Name], [{timeout,infinity}]).

%% gen_server.

init([Name]) ->
	{ok, #state{name=Name}}.

handle_call(get_name, _From, State = #state{name=Name}) ->
  {reply,Name, State};

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
