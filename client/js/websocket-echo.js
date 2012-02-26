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
        } else if (msgObj['event'] === 'list_conversations') {
          if (msgObj['data']['channel'] === 'stats') {
            var convs = [
              {
                tag: 'client_development',
                topic: 'Why is client development so easy?',
                users: ['scvalex', 'nh2'],
                messages: [
                  { user: 'nh2', message: "Can you tell me why our client development runs so super smoothly?" },
                  { user: 'scvalex', message: "Because we're using MVVM." }
                ]
              },
              {
                tag: 'server_development',
                topic: 'Is it OK to have more LANGUAGE pragmas than lines of code?',
                users: ['ex_falso', 'rostayob'],
                messages: [
                  { user: 'ex_falso', message: "That looks a little bit strange to me..." },
                  { user: 'rostayob', message: "It's OK, have that all the time!" }
                ]
              }
            ];
          } else {
            var convs = [
              {
                tag: "scifi_rocks",
                topic: "Am I the only one who sees the corollary between scifi and comptech",
                users: ["nh2", "exFalso"],
                messages: []
              }
            ];
          }
          msgObj = {event: "list_conversations.ok",
                    data: {conversations: convs,
                           channel: msgObj['data']['channel']}};
        }
        var replyStr = JSON.stringify(msgObj);
      } catch (err) {
        console.error("WARNING: received non-JSON message: " + message.utf8Data);
        console.log("  ", err);
        var replyStr = message.utf8Data;
      }
      console.log("Sending back ", replyStr);
      conn.sendUTF(replyStr);
    } else {
      console.error("ERROR: received non-utf8 data, ignoring");
    }
  });

  conn.on('close', function(connection) {
  });
});
