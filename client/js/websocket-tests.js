describe('async example test suite', function () {
  it('open websocket connection', function () {

    /* Initialise and open the WebSocket. */
    runs(function () {
      this.connectionOpen = false;
      this.lastMessage = null;

      var url = 'ws://localhost:9999/echo';
      if (typeof WebSocket != 'undefined') {
        this.sock = new WebSocket(url);
      } else if (typeof MozWebSocket != 'undefined') {
        this.sock = new MozWebSocket(url);
      } else {
        console.log("Unsupported browser: I don't know how to open WebSockets.");
        throw "unspported browser";
      }

      var self = this;
      this.sock.onopen = function() {
        console.log('open');
        self.connectionOpen = true;
      };
      this.sock.onmessage = function(e) {
        console.log('message', e.data);
        self.lastMessage = e.data;
      };
      this.sock.onclose = function() {
        console.log('close');
        self.connectionOpen = false;
      };
      this.sock.onerror = function(e) {
        console.log('ERROR: ' + e);
      };
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
      this.sock.send('Hello, WebSocket!');
    });

    waitsFor(function () {
      return (this.lastMessage != null);
    });

    runs(function () {
      expect(this.lastMessage).toEqual('Hello, WebSocket!');
    });

    /* Close the WebSocket. */
    runs(function () {
      this.sock.close();
    });

    waitsFor(function() {
      return !this.connectionOpen;
    }, "WebSocket took too long to close", 500);
  });
});
