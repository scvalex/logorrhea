describe('websocket-json-events test suite', function () {
  it('event ping/pong', function () {

    /* Initialise and open the WebSocket. */
    runs(function () {
      this.connectionOpen = false;
      this.lastMessage = null;

      this.sock = new FancyWebSocket('ws://localhost:9999/echo');

      var self = this;
      this.sock.bind('open', function(_) {
        console.log('open');
        self.connectionOpen = true;
      });
      this.sock.bind('pong', function(msg) {
        console.log('message', msg);
        self.lastMessage = msg;
      });
    });

    /* Wait for the WebSocket to open. */
    waitsFor(function() {
      return this.connectionOpen;
    }, "WebSocket took too long to open", 500);

    runs(function () {
      expect(this.connectionOpen).toEqual(true);
    });

    /* Send a message and wait for the echo. */
    runs(function () {
      this.sock.send('ping', 'Hello, WebSocket!');
    });

    waitsFor(function () {
      return (this.lastMessage != null);
    }, "Waited too much for a reply", 500);

    runs(function () {
      expect(this.lastMessage).toEqual('Hello, WebSocket!');
    });
  });
});
