-module(hello_handler).

-export([init/2]).

-record(state, {
}).

init(Req, _Opts) ->
  error_logger:info_msg("Req:~p  _Opts:~p~n",[Req,_Opts]),
  Result = cowboy_req:reply(200, 
                                [{<<"content-type">>,<<"text/plain">>}],
                                <<"Hello Erlang!">>,
                                Req),
  error_logger:info_msg("Result:~p~n",[Result]),
	{ok, Result, #state{}}.
