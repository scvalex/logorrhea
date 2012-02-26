var http = require('http');
var WebSocketServer = require('websocket').server;

var server = http.createServer();
server.listen(9999, '0.0.0.0');

var echo = new WebSocketServer({
    httpServer: server
});

// WebSocket server
echo.on('request', function(request) {
  var conn = request.accept(null, request.origin);

  conn.on('message', function(message) {
    console.log("Got: ", message);
    if (message.type === 'utf8') {
      try {
        var msgObj = JSON.parse(message.utf8Data,
                                function(key, value) {
                                  return value;
                                });
        if (msgObj['event'] === 'ping') {
          msgObj['event'] = 'pong';
        } else if (msgObj['event'] === 'connect') {
          /* msgObj = {event: "connect.error", data: {reason: "Meh"}}; */
          msgObj = {event: "connect.ok", data: {}};
        } else if (msgObj['event'] === 'disconnect') {
          msgObj = {event: "disconnect.ok", data: {}};
        } else if (msgObj['event'] === 'list_channels') {
          msgObj = {event: 'list_channels.ok',
                    data: { channels: ["stats", "comptech"] }};
        } else if (msgObj['event'] === 'list_users') {
          if (msgObj['data']['channel'] === 'stats') {
            var users = ['aif', 'nh2', 'exFalso'];
          } else {
            var users = ['scvalex', 'rostayob'];
          }
          msgObj = {event: 'list_users.ok',
                    data: {users: users,
                           channel: msgObj['data']['channel']}};
        }
        var replyStr = JSON.stringify(msgObj);
      } catch (err) {
        console.error("WARNING: received non-JSON message: " + message.utf8Data);
        var replyStr = message.utf8Data;
      }
      conn.sendUTF(replyStr);
    } else {
      console.error("ERROR: received non-utf8 data, ignoring");
    }
  });

  conn.on('close', function(connection) {
  });
});
