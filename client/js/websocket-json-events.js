/* Adapted from https://gist.github.com/299789 */

/* Ismael Celis 2010
Simplified WebSocket events dispatcher (no channels, no users)

var socket = new FancyWebSocket();

// bind to server events
socket.bind('some_event', function(data){
  alert(data.name + ' says: ' + data.message)
});

// broadcast events to all connected users
socket.send( 'some_event', {name: 'ismael', message : 'Hello world'} );

WebSocket onopen/onclose events are mapped to the 'open'/'close' event channels.

The server has to send websocket data of form
  JSON.stringify({ event: EVENT_NAME, data: DATA })
*/

define(['jsschema', 'schemas'], function(_, schemas) {
  return {
    FancyWebSocket: function(url) {
      if (typeof WebSocket !== 'undefined') {
        var conn = new WebSocket(url);
      } else if (typeof MozWebSocket !== 'undefined') {
        var conn = new MozWebSocket(url);
      } else {
        throw 'WebSocket seems to not be supported by this browser!';
      }

      var callbacks = {};
      var defaultCallback = function(_) { };

      this.bind = function(event_name, callback) {
        callbacks[event_name] = callbacks[event_name] || [];
        callbacks[event_name].push(callback);
        return this; // chainable
      };

      this.bindMethod = function(method_name, okCallback, errorCallback) {
        this.bind(method_name + ".ok", okCallback);
        this.bind(method_name + ".error", errorCallback);
      };

      this.bindDefault = function(callback) {
        defaultCallback = callback;
      }

      this.send = function(event_name, event_data) {
        var payload = JSON.stringify({event:event_name, data: event_data});
        conn.send(payload); // <= send JSON data to socket server
        return this;
      };

      this.close = function() {
        conn.close();
      }

      this.inject = function (event_name, message) {
        console.log("Receiving injected message from socket on channel '" + event_name +"': ", message);
        dispatch(event_name, message);
      }

      // dispatch to the right handlers
      conn.onmessage = function(evt) {
        var json = JSON.parse(evt.data);

        // not an event, throw exception
        if (!jsschema.valid(schemas.event, json))
          throw new Error("received object that is not an event, ignoring: " + JSON.stringify(evt.data));

        var eventName = json.event;
        var eventData = json.data;

        // not a defined schema: warn but permit
        var eventSchema = schemas.events[eventName];
        if (eventSchema === undefined)
          console.error("Warning: received object without defined schema on event '" + eventName + "': ", eventData, " received JSON: ", json);
        else if (!jsschema.valid(eventSchema, eventData))
          console.error("Warning: received object ", eventData, " does not mach declared schema ", eventSchema, "for event '" + eventName + "'; received JSON: ", json);

        dispatch(eventName, eventData);
      };

      conn.onclose = function() {
        dispatch('close',null);
      };

      conn.onopen = function() {
        dispatch('open',null);
      };

      var dispatch = function(event_name, message) {
        var chain = callbacks[event_name];
        if (typeof chain == 'undefined') {
          if (typeof defaultCallback != 'undefined') {
            defaultCallback(event_name, message);
          }
          return; // no callbacks for this event
        }
        for(var i = 0; i < chain.length; i++) {
          chain[i](message);
        }
      }
    }

  }

});
