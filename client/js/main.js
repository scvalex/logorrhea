require(['jquery', 'knockout', 'websocket-json-events'],
        function($, _, websocket_json_events) {

  function ConversationsModel() {
    var self = this;

    self.username = ko.observable("");
    self.channels = ko.observableArray([]);
    self.channel = ko.observable("");
    self.users = ko.observableArray([]);
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

    socket = new websocket_json_events.FancyWebSocket('ws://localhost:9999/echo');

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

    socket.bindMethod(
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

    socket.bindMethod(
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

    socket.bindMethod(
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
    conversationsModel.channel(channel);
    $("#channelBox").removeClass("hidden");
    socket.bindMethod(
      'list_users',
      function(users) {
        console.log("Users on ", conversationsModel.channel(),
                    " are ", users['users']);
        conversationsModel.users(users['users']);
        $("#usersBox").removeClass('hidden');
      },
      undefined);

    socket.send('list_users', {"channel": conversationsModel.channel()});
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
