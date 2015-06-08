-module(websocket_handler).
-include("const.hrl").

-export([init/2]).
-export([websocket_handle/3]).
-export([websocket_info/3]).

init(Req, Opts) ->
  ets:insert(?ETS_WEBSOCKET, {id_pool:gen_id(),self()}),
	{cowboy_websocket, Req, Opts}.

websocket_handle({text, Msg}, Req, State) ->
	{reply, {text, << "That's what she said! ", Msg/binary >>}, Req, State};
websocket_handle(_Data, Req, State) ->
	{ok, Req, State}.

websocket_info({timeout, _Ref, Msg}, Req, State) ->
  ?ERROR("1111"),
	{reply, {text, Msg}, Req, State};
websocket_info("info msg", Req, State) ->
  ?ERROR("2222"),
	{reply, {text, list_to_binary("hahahah")}, Req, State};
websocket_info(_Info, Req, State) ->
  ?ERROR("3333"),
	{ok, Req, State}.
