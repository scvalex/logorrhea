// TODO make it a module
require(['jquery', 'knockout', 'websocket-json-events'],
        function($, _, websocket_json_events) {

  var status = {
    unknown: 'unknown',
    connecting: 'connecting',
    connected: 'connected',
    disconnected: 'disconnected'
  };

  var statusLabels = {
    unknown: '<<unknown>>',
    connecting: 'Connecting...',
    connected: 'Connected',
    disconnected: 'Disconnected'
  };

  // TODO put it into the model
  socket = null;

  function ConversationsModel() {
    var self = this;

    self.connectionStatus = ko.observable(status.disconnected);
    self.connectionStatusText = ko.computed(function() {
      return statusLabels[self.connectionStatus];
    });
    self.isConnected = ko.computed(function() {
      return self.connectionStatus() == status.connected;
    });

    self.username = ko.observable("");
    self.channels = ko.observableArray([]);
    self.channel = ko.observable("");
    self.channelSelected = ko.computed(function() {
      return (self.channel().length > 0);
    });
    self.users = ko.observableArray([]);
    self.usersReceived = ko.observable(false);
    self.conversations = ko.observableArray([]);
    self.conversation = ko.observable({topic: "", messages: []});

    self.connect = function() {
      connectInternal();
    };

    self.disconnect = function() {
      disconnectInternal();
    };

    self.channelClicked = function(channelName) {
      browseChannelInternal(channelName);
    }

    self.conversationClicked = function(conversation) {
      conversationClickedInternal(conversation.tag);
    }
  }

  function connectInternal() {
    console.log("Connecting as " + conversationsModel.username());
    conversationsModel.connectionStatus(status.connecting);

    socket = new websocket_json_events.FancyWebSocket('ws://localhost:9999/echo');

    socket.bind(
      'open',
      function(_) {
        console.log('socket open');
        socket.send('connect', { user: conversationsModel.username() });
      });

    socket.bind(
      'close',
      function(_) {
        console.log('socket closed');
        conversationsModel.connectionStatus(status.disconnected);
      });

    socket.bindDefault(function(event, data) {
      console.log("Unexpected message: " + event + "(" + data + ")");
    });

    socket.bindMethod(
      'connect',
      function(_) {
        console.log("connected ok");
        conversationsModel.connectionStatus(status.connected);
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
    socket.bindMethod(
      'disconnect',
      function(_) {
        console.log(socket);
        socket.close();
      },
      function(_) {
        /* whoosh */
      });

    socket.send('disconnect', {});
  }

  function browseChannelInternal(channel) {
    console.log("Channel clicked: " + channel);
    conversationsModel.channel(channel);
    socket.bindMethod(
      'list_users',
      function(users) {
        console.log("Users on ", conversationsModel.channel(),
                    " are ", users['users']);
        conversationsModel.users(users['users']);
        conversationsModel.usersReceived(true);
      },
      function(err) {
        console.log("failed to get users: ", err);
      });

    socket.bindMethod(
      'list_conversations',
      function(conversations) {
        console.log("Conversations in ", conversationsModel.channel(),
                    " are ", conversations['conversations']);
        conversationsModel.conversations(conversations['conversations']);
        $("#conversationsBox").removeClass('hidden');
      },
      function(err) {
        console.log("failed to get conversations: ", err);
      });

    socket.send('list_users',
                {"channel": conversationsModel.channel()});
    socket.send('list_conversations',
                {"channel": conversationsModel.channel()});
  }

  function conversationClickedInternal(tag) {
    console.log("Conversation clicked: ", tag);
  }

  $(function() {
    console.log("hello logorrhea");

    var conversationsModel = new ConversationsModel();
    window.conversationsModel = conversationsModel;
    ko.applyBindings(conversationsModel);

    $("#usernameInput")[0].focus();
  });
});
