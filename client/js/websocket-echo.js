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
    if (message.type === 'utf8') {
      try {
        var msgObj = JSON.parse(message.utf8Data,
                                function(key, value) {
                                  return value;
                                });
        if (msgObj['event'] === 'ping') {
          msgObj['event'] = 'pong';
        } else if (msgObj['event'] === 'connect') {
          msgObj = {event: "connect.ok", data: null};
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
