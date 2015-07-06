-define(ERROR(Format, Msg), error_logger:info_msg(Format, Msg)).
-define(ERROR(Format), error_logger:info_msg(Format)).

-define(ETS_ID_POOL, id_pool).
-define(ETS_PLAYER, player).
-define(ETS_ROOM, room).
-define(ETS_CONN, connection).
-define(ETS_WEBSOCKET, websocket).

-define(ETS_TABLES, [
                     ?ETS_ID_POOL,
                     ?ETS_PLAYER,
                     ?ETS_CONN,
                     ?ETS_WEBSOCKET,
                     ?ETS_ROOM
                    ]).

-define(SIMPLE_ONE_FOR_ONE, {simple_one_for_one, 5, 10}).
-define(ONE_FOR_ONE, {one_for_one, 5, 10}).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(CHILD(I, Type, Params), {I, {I, start_link, Params}, permanent, 5000, Type, [I]}).
-define(SUP_CHILD(Type), [{undefined, {Type, start_link, []}, temporary, 2000, worker, []}]).

-record(player_info,{
          name,
          room_name,
          pid
         }).

-record(room_info,{
          name,
          creator_name
         }).

%% the listen port
-define(LISTEN_PORT, 8081).
