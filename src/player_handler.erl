-module(player_handler).
-include("const.hrl").

-export([init/2]).

init(Req, Opts) ->
  error_logger:info_msg("body_qs:~p~n", [cowboy_req:headers(Req)]),
  error_logger:info_msg("Req:~p~n", [Req]),
  error_logger:info_msg("qs:~p~n", [cowboy_req:parse_qs(Req)]),
  BodyQs = cowboy_req:body_qs(Req),
  Handler = case lists:keyfind(<<"action">>, 1, BodyQs) of
              {<<"action">>, H} ->
                H;
              _ ->
                <<"index">>
            end,
  handler(Handler, Req, Opts).

handler(<<"create_room">>, Req, _Opts) ->
  %% to create room
  Req,
  ok;
handler(<<"enter_room">>, Req, _Opts) ->
  %% to enter room
  Req,
  ok;
handler(_, Req, Opts) ->
  Req2 = cowboy_req:reply(200,
                          [{<<"content-type">>, <<"text/plain">>}],
                          <<"Hello Erlang!">>,
                          Req),
  {ok, Req2, Opts}.
