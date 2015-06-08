-module(my_chat_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, start/0]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  error_logger:info_msg("start_http2222"),
  %% add the app dir
  code:add_path(element(2, file:get_cwd())),
  Dispatch = cowboy_router:compile([
                                    {'_', [
                                           {"/", cowboy_static, {priv_file, my_chat, "index.html"}},
                                           {"/player", player_handler, []},
                                           {"/room", room_handler, []},
                                           {"/websocket", websocket_handler, []},
                                           {"/static/[...]", cowboy_static, {priv_dir, my_chat, "static"}}
                                          ]
                                    }
                                   ]),
  Result = cowboy:start_http(my_http_listener, 100, [{port, 8080}],
                             [{env, [{dispatch, Dispatch}]}]
                            ),
  error_logger:info_msg("start_http:~p~n",[Result]),
  my_chat_sup:start_link().

stop(_State) ->
    ok.

start() ->
  Apps = [ranch,crypto,cowlib,cowboy,my_chat],
  lists:foreach(fun(App) ->
                    Result =case catch application:start(App) of
                             ok ->
                                ok;
                              _Other ->
                                _Other
                            end,
                    error_logger:info_msg("start app:~p reason:~p~n",[App,Result])
                end,Apps),
  my_chat_sup:start_child().
