describe('async example test suite', function () {
  it('open websocket connection', function () {

    /* Initialise and open the WebSocket. */
    runs(function () {
      this.connectionOpen = false;
      this.lastMessage = null;
      this.sock = new WebSocket('ws://localhost:9999/echo');

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
      meh = this.lastMessage;
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
