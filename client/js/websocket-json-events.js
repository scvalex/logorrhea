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

var FancyWebSocket = function(url){
  if (typeof WebSocket !== 'undefined')
    var conn = new WebSocket(url);
  else if (typeof MozWebSocket !== 'undefined')
    var conn = new MozWebSocket(url);
  else
    throw 'WebSocket seems to not be supported by this browser!';

  var callbacks = {};

  this.bind = function(event_name, callback){
    callbacks[event_name] = callbacks[event_name] || [];
    callbacks[event_name].push(callback);
    return this;// chainable
  };

  this.send = function(event_name, event_data){
    var payload = JSON.stringify({event:event_name, data: event_data});
    conn.send( payload ); // <= send JSON data to socket server
    return this;
  };

  // dispatch to the right handlers
  conn.onmessage = function(evt){
    var json = JSON.parse(evt.data)
    dispatch(json.event, json.data)
  };

  conn.onclose = function(){dispatch('close',null)}
  conn.onopen = function(){dispatch('open',null)}

  var dispatch = function(event_name, message){
    var chain = callbacks[event_name];
    if(typeof chain == 'undefined') return; // no callbacks for this event
    for(var i = 0; i < chain.length; i++){
      chain[i]( message )
    }
  }
};
