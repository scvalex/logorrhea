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
      conn.sendUTF(message.utf8Data);
    } else {
      console.error("ERROR: received non-utf8 data, ignoring");
    }
  });

  conn.on('close', function(connection) {
  });
});
