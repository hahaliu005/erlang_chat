-module(id_pool).
-behaviour(gen_server).
-include("const.hrl").

%% API.
-export([start_link/0,gen_id/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
          id=1
}).

-define(DEFAULT_ID, 1).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], [{timeout,infinity}]).

%% gen_server.

init([]) ->
  ?ERROR("id_pool init",[]),
  %% save the max id to ets is for pretect the process crash
  Id = case ets:lookup(?ETS_ID_POOL, max_id) of
         [] ->
           ets:insert(?ETS_ID_POOL, {max_id, ?DEFAULT_ID}),
           ?DEFAULT_ID;
         [{max_id, MaxId}] ->
           MaxId
       end,
  {ok, #state{id=Id}}.

gen_id() ->
  gen_server:call(?MODULE, gen_id).

handle_call(gen_id, _From, #state{id=Id})->
  NewId = Id + 1,
  ets:insert(?ETS_ID_POOL, {max_id, NewId}),
  {reply, NewId, #state{id=NewId}};

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
  ?ERROR("handle_info:~p~n",[_Info]),
	{noreply, State}.

terminate(_Reason, _State) ->
  ?ERROR("id_pool terminate",[]),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
