-module(websocket_handler).
-include("const.hrl").

-compile(export_all).
-export([init/2]).
-export([websocket_handle/3]).
-export([websocket_info/3]).

init(Req, Opts) ->
  self() ! <<"connect_socket">>,
	{cowboy_websocket, Req, Opts}.

websocket_handle({text, Msg}, Req, State) ->
  ?ERROR("websocket_handle:text:~p~n", [Msg]),
  [{BMethod, BParam} | _]= jsx:decode(Msg),
  HandleText = {BMethod, BParam},
  ?ERROR("The HandleText:~p~n", [HandleText]),
  handle_text(HandleText, Req, State);
websocket_handle(_Data, Req, State) ->
  ?ERROR("websocket_handle"),
	{ok, Req, State}.

websocket_info({timeout, _Ref, Msg}, Req, State) ->
  ?ERROR("1111"),
	{reply, {text, Msg}, Req, State};
websocket_info({<<"text">>, Msg}, Req, State) ->
  ?ERROR("####:~p~n",[Msg]),
  Reply = reply(msg, true, Msg, ""),
	{reply, {text, Reply}, Req, State};
websocket_info(<<"connect_socket">>, Req, State) ->
  Reply = reply(connect_socket, true, "", ""),
  {reply, {text, Reply}, Req, State};
websocket_info(_Info, Req, State) ->
  ?ERROR("3333:~p~n", [_Info]),
	{ok, Req, State}.

handle_text({<<"create_room">>, Param}, Req, State) ->
  PlayerName = get_option(<<"player_name">>, Param),
  RoomName = get_option(<<"room_name">>, Param),
  ?ERROR("fffff"),
  Reply = case create_room(PlayerName, RoomName, self()) of
            {error, Result} ->
              reply(create_room, false, "", Result);
            {ok, _} ->
              reply(create_room, true, "", "")
          end,
  {reply, {text, Reply}, Req, State};
handle_text({<<"room_list">>, _Param}, Req, State) ->
  {ok, Names} = room_list(),
  ?ERROR("Names:~p~n", [Names]),
  Reply = reply(room_list, true, Names, ""),
  {reply, {text,Reply}, Req, State};
handle_text({<<"join_room">>, Param}, Req, State) ->
  PlayerName = get_option(<<"player_name">>, Param),
  RoomName = get_option(<<"room_name">>, Param),
  Reply = case join_room(PlayerName, RoomName) of
            {error, Result} ->
              reply(join_room, false, "", Result);
            {ok, _} ->
              reply(join_room, true, "", "")
          end,
  {reply, {text,Reply}, Req, State};
handle_text({<<"player_list">>, Param}, Req, State) ->
  RoomName = get_option(<<"room_name">>, Param),
  {ok, Names}= player_list(RoomName),
  Reply = reply(player_list, true, Names, ""),
  {reply, {text,Reply}, Req, State};
handle_text({<<"send_msg_to_room">>, Param}, Req, State) ->
  FromPlayerName = get_option(<<"from_player_name">>, Param),
  ToRoomName = get_option(<<"to_room_name">>, Param),
  Msg = get_option(<<"msg">>, Param),
  Reply = case send_msg_to_room(FromPlayerName, ToRoomName, Msg) of
            {error, Result} ->
              reply(send_msg_to_room, false, "", Result);
            {ok, _} ->
              reply(send_msg_to_room, true, "", "")
          end,
  {reply, {text,Reply}, Req, State};
handle_text({<<"send_msg_to_player">>, Param}, Req, State) ->
  FromPlayerName = get_option(<<"from_player_name">>, Param),
  ToPlayerName = get_option(<<"to_player_name">>, Param),
  Msg = get_option(<<"msg">>, Param),
  Reply = case send_msg_to_player(FromPlayerName, ToPlayerName, Msg) of
            {error, Result} ->
              reply(send_msg_to_player, false, "", Result);
            {ok, _} ->
              reply(send_msg_to_player, true, "", "")
          end,
  {reply, {text,Reply}, Req, State};
handle_text({_Method, _Param}, Req, State) ->
  Reply = reply(undefined, false, "", no_method),
  {reply, {text,Reply}, Req, State}.

room_list() ->
  Names = lists:map(fun(#room_info{name=RName}) ->
                        RName
            end, ets:tab2list(?ETS_ROOM)),
  {ok, Names}.

join_room(PlayerName, RoomName) ->
  PlayerInfo = #player_info{name=PlayerName, room_name=RoomName, pid=self()},
  RoomInfo = #room_info{name=RoomName, creator_name=PlayerName},
  add_player(PlayerInfo),
  add_room(RoomInfo),
  {ok, PlayerName}.

player_list(RoomName) ->
  Names = lists:map(fun(#player_info{name=PName}) ->
                PName
            end,ets:match_object(?ETS_PLAYER, #player_info{room_name=RoomName,_='_'})),
  {ok, Names}.

send_msg_to_room(FromPlayerName, ToRoomName, Msg) ->
  case ets:match_object(?ETS_PLAYER, #player_info{name=FromPlayerName,_='_'}) of
    [#player_info{room_name=ToRoomName}] ->
      case ets:match_object(?ETS_PLAYER, #player_info{room_name=ToRoomName,_='_'}) of
        [] ->
          {error, no_player_to_send};
        PlayerInfoList ->
          lists:foreach(fun(#player_info{pid=Pid}) ->
                            send_msg(FromPlayerName, Pid, Msg)
                        end, PlayerInfoList)
      end;
    [#player_info{}] ->
      {error, not_in_room};
    _ ->
      {error, player_not_exist}
  end.

send_msg_to_player(FromPlayerName, ToPlayerName, Msg) ->
  case {ets:match_object(?ETS_PLAYER, #player_info{name=FromPlayerName, _='_'}), ets:match_object(?ETS_PLAYER, #player_info{name=ToPlayerName, _='_'})} of
    {[#player_info{name=FromPlayerName}], [#player_info{name=FromPlayerName}]} ->
      {error, can_not_send_to_self};
    {[#player_info{name=FromPlayerName}], [#player_info{name=ToPlayerName, pid=ToPid}]} ->
      send_msg(FromPlayerName, ToPid, Msg);
    _ ->
      {error, player_not_exist}
  end.

send_msg(FromPlayerName, Pid, Msg) ->
  TotalMsg = list_to_binary(binary_to_list(FromPlayerName) ++ ">>" ++ binary_to_list(Msg)),
  Pid ! {<<"text">>, TotalMsg}.

reply(From, Status, Data, ErrorMsg) ->
  Reply = [
           {<<"from">>, From},
           {<<"status">>, Status},
           {<<"data">>, Data},
           {<<"error_msg">>, misc:term_to_binary(ErrorMsg)}
          ],
  jsx:encode(Reply).

get_option(Key, List) ->
  {_, Value} = lists:keyfind(Key, 1, List),
  Value.

get_player_info(PlayerName) ->
  case ets:lookup(?ETS_PLAYER, PlayerName) of
    [] ->
      false;
    [{PlayerName, Info}] ->
      Info
  end.

get_room_info(RoomName) ->
  case ets:lookup(?ETS_ROOM, RoomName) of
    [] ->
      false;
    [{RoomName, Info}] ->
      Info
  end.

add_player(PlayerInfo = #player_info{name=PlayerName}) ->
  ets:insert(?ETS_PLAYER, {PlayerName, PlayerInfo}).

add_room(RoomInfo = #room_info{name=RoomName}) ->
  ets:insert(?ETS_ROOM, {RoomName, RoomInfo}).
