require(['jquery', 'knockout', 'websocket-json-events', 'jsschema', 'schemas'],
        function($, _, _, _, schemas) {

  function ConversationsModel() {
    var self = this;

    self.username = ko.observable();
    self.channels = ko.observableArray([]);
    self.users = ko.observableArray(['scvalex', 'ex_falso', 'rostayob', 'nh2']);
    self.conversations = ko.observableArray([
      { name: 'client_development', topic: 'Why is client development so easy?', users: ['scvalex', 'nh2'] },
      { name: 'server_development', topic: 'Is it OK to have more LANGUAGE pragmas than lines of code?', users: ['ex_falso', 'rostayob'] }
    ]);

    self.connect = function() {
      connectInternal();
    };

    self.disconnect = function() {
      disconnectInternal();
    };

    self.channelClicked = function(channelName) {
      browseChannelInternal(channelName);
    }
  }

  function connectInternal() {
    $('#loginBox').addClass('hidden');
    $('#loggedInBox').removeClass('hidden');

    console.log("Connecting as " + conversationsModel.username());

    socket = new FancyWebSocket('ws://localhost:9999/echo');
    socket.bindSchema = function(eventName, callback) {
      this.bind(eventName, function(data) {
        return jsschema.check(schemas[data.event], callback(data));
      });
    };
    socket.bindSchemaMethod = function(eventName, okCallback, errorCallback) {
      this.bindMethod(
        eventName,
        function(data) {
          console.log("schema-check ", eventName, data);
          if (schemas[data.event] !== undefined)
            return jsschema.check(schemas[data.event], okCallback(data));
          else {
            console.log("Warning: received object without schema for event type '" + eventName + "'", data);
            return okCallback(data);
          }
        },
        // Dont schema-check errors (yet)
        errorCallback
      );
    }

    socket.bind(
      'open',
      function(_) {
        console.log('socket open');
        socket.send('connect',
                    JSON.stringify({user: conversationsModel.username()}));
      });

    socket.bind(
      'close',
      function(_) {
        console.log('socket closed');
        $("#connectionStatusLabel").text("Disconnected");
      });

    socket.bindDefault(function(event, data) {
      console.log("Unexpected message: " + event + "(" + data + ")");
    });

    socket.bindSchemaMethod(
      'connect',
      function(_) {
        console.log("connected ok");
        $("#disconnectButton").removeClass("hidden");
        $('#channelsBox').removeClass('hidden');
        $("#connectionStatusLabel").text("Connected").addClass("hidden");
        socket.send('list_channels', {});
      },
      function(err) {
        console.log("failed to connect: ", err);
      });

    socket.bindSchemaMethod(
      'list_channels',
      function(data) {
        conversationsModel.channels(data['channels']);
      },
      function(err) {
        console.log("failed to get channels: ", err);
      });
  }

  function disconnectInternal() {
    $("#disconnectButton").addClass("hidden");

    socket.bindSchemaMethod(
      'disconnect',
      function(_) {
        console.log(socket);
        socket.close();
        $("#connectionStatusLabel").removeClass("hidden");
      },
      undefined);

    socket.send('disconnect', {});
  }

  function browseChannelInternal(channel) {
    console.log("Channel clicked: " + channel);
  }

  $(function() {
    console.log("hello logorrhea");

    var conversationsModel = new ConversationsModel();
    window.conversationsModel = conversationsModel;
    ko.applyBindings(conversationsModel);

    socket = null;

    $("#usernameInput")[0].focus();
  });
});
