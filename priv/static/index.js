function connect_socket()
{
  wsHost = "ws://" + window.location.host + "/websocket"
  websocket = new WebSocket(wsHost);
  showScreen('<b>Connecting to: ' +  wsHost + '</b>');
  websocket.onopen = function(evt) { onOpen(evt) };
  websocket.onclose = function(evt) { onClose(evt) };
  websocket.onmessage = function(evt) { onMessage(evt) };
  websocket.onerror = function(evt) { onError(evt) };
};

function disconnect() {
  websocket.close();
};

function toggle_connection(){
  if(websocket.readyState == websocket.OPEN){
    disconnect();
  } else {
    connect();
  };
};

function sendTxt(text) {
  if(websocket.readyState == websocket.OPEN){
    txtStr = JSON.stringify(text)
    websocket.send(txtStr);
    showScreen('sending: ' + txtStr);
  } else {
    showScreen('websocket is not connected');
  };
};

function onOpen(evt) {
  showScreen('<span style="color: green;">CONNECTED </span>');
  $("#connected").fadeIn('slow');
  $("#content").fadeIn('slow');
};

function onClose(evt) {
  showScreen('<span style="color: red;">DISCONNECTED </span>');
};

function onMessage(evt) {
  console.log(evt)
  var data = JSON.parse(evt.data)
  console.log(data);
  if (data['status'] == false) {
    showScreen('<span style="color: red;">RESPONSE: ' + evt.data+ '</span>');
  }else{
    switch(data['from']){
      case 'create_room':
        showScreen('<span style="color: red;">create: ' + evt.data+ '</span>');
      break;
      case 'msg':
        showScreen('<span style="color: red;">msg: ' + data['data']+ '</span>');
      break;
      case 'room_list':
        pageListRoom(data['data'])
      break;
      case 'connect_socket':
        roomList()
      break;
      default:
        showScreen('<span style="color: red;">default: ' + evt.data+ '</span>');
    }
  }
};

function pageListRoom(rooms){
  for(r in rooms){
    $('#room_list').append('<span onclick=chooseRoom(this) >' + rooms[r] + '</span>')
  }
}

function chooseRoom(ts){
  var roomName = $(ts).text()
  $('input[name=room_name]').attr('value', roomName)
}

function onError(evt) {
  showScreen('<span style="color: red;">ERROR: ' + evt.data+ '</span>');
};

function showScreen(txt) {
  $('#output').prepend('<p>' + txt + '</p');
};

function clearScreen()
{
  $('#output').html("");
};

function initLoginPage(){
  pageStr = " \
  <input type='text' name='player_name' value='' /> \
  <input type='text' name='room_name' value='' /> \
  <input type='button' onclick='form_join_room(this)' name='start' value='join' /> \
  <div id='room_list'  ></div>"
  $(pageStr).appendTo('#main')
}

function form_join_room(ts){
  var playerName = $(ts).siblings('input[name=player_name]').val()
  var roomName = $(ts).siblings('input[name=room_name]').val()
  joinRoom(playerName, roomName)
}

function createRoom(playerName, roomName){
  var text = {}
  text['create_room'] = {}
  text['create_room']["player_name"] = playerName
  text['create_room']["room_name"] = roomName
  sendTxt(text)
}

function roomList(){
  var text = {}
  text['room_list'] = ""
  sendTxt(text)
}

function joinRoom(playerName, roomName){
  var text = {}
  text['join_room'] = {}
  text['join_room']["player_name"] = playerName
  text['join_room']["room_name"] = roomName
  sendTxt(text)
}

function playerList(roomName){
  var text = {}
  text['player_list'] = {}
  text['plaery_list']["player_name"] = playerName
  text['plaery_list']["room_name"] = roomName
  sendTxt(text)
}

function sendMsgToRoom(fromPlayerName, toRoomName, msg){
  var text = {}
  text['send_msg_to_room'] = {}
  text['send_msg_to_room']["from_player_name"] = fromPlayerName
  text['send_msg_to_room']["to_room_name"] = toRoomName
  text['send_msg_to_room']["msg"] = msg
  sendTxt(text)
}

function sendMsgToPlayer(fromPlayerName, toPlayerName, msg){
  var text = {}
  text['send_msg_to_player'] = {}
  text['send_msg_to_player']["from_player_name"] = fromPlayerName
  text['send_msg_to_player']["to_player_name"] = toPlayerName
  text['send_msg_to_player']["msg"] = msg
  sendTxt(text)
}

$(document).ready(function(){
  connect_socket()
  initLoginPage()
})
