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
      if (self.connectionStatus() == status.connected)
        $('.hero-unit').slideUp();
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
    self.conversationsReceived = ko.observable(false);
    self.conversation = ko.observable({topic: "", messages: []});
    self.conversationSelected = ko.computed(function() {
      return (typeof self.conversation().tag !== "undefined");
    });

    self.currentMessage = ko.observable("");

    self.conversationsDict = ko.computed(function() {
      return objectArrayToDict(self.conversations(), 'tag');
    });

    self.connect = function() {
      connectInternal();
    };

    self.disconnect = function() {
      disconnectInternal();
    };

    self.channelClicked = function(channelName) {

      socket.send('join',
                  { channel: channelName });

    }

    self.conversationClicked = function(conversation) {
      conversationClickedInternal(conversation.tag);
    }

    self.postInCurrent = function(_) {
      self.post(self.channel(), self.conversation()['tag'],
                self.currentMessage());
      self.currentMessage("");
    }

    self.post = function(channel, tag, message) {
      console.log("Sending ", channel,
                  " in conversation ", tag,
                  " in channel ", message);
      socket.send('send_conversation',
                  { channel: channel, tag: tag, message: message });
    }
  }

  function connectInternal() {
    console.log("Connecting as " + conversationsModel.username());
    conversationsModel.connectionStatus(status.connecting);

    socket = new websocket_json_events.FancyWebSocket('ws://0.0.0.0:9999');
    // For debuging / event injection
    window.socket = socket;

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
      console.error("Warning: Received unexpected event '" + event + "' with data: ", data);
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

    socket.bind('join.ok', function(joinOkEvent) {
      // TODO rename
      browseChannelInternal(joinOkEvent.channel);
    });


    socket.bindMethod(
      'list_channels',
      function(data) {
        conversationsModel.channels(data['channels']);
      },
      function(err) {
        console.log("failed to get channels: ", err);
      });

    socket.bind(
      'receive_conversation',
      function(receiveConversationEvent) {
        var e = receiveConversationEvent;

        console.log("BEFORE", conversationsModel.conversations());

        // Hack
        e.tag = '#whatislecturea';

        if (conversationsModel.conversations().length == 0) {
          console.log('adding');
          // conversationsModel.conversations.push({ tag: e.tag, topic: "What is Lecturea!", users: ['nh2', 'scvalex', 'rostayob', 'exfalso'], messages: [] });
          conversationsModel.conversations([{ tag: e.tag, topic: "What is Lecturea!", users: ['nh2', 'scvalex', 'rostayob', 'exfalso'], messages: [] }]);
        }
        conversationsModel.conversationsReceived(true);

        // TODO we can currently be in only one channel, so ignore e.channel

        // TODO conversation creation
        // TODO user "creation"

        console.log("dict ", conversationsModel.conversationsDict(), "tag: ", e.tag, e);
        // var conv = conversationsModel.conversationsDict()[e.tag];
        var conv = conversationsModel.conversationsDict()[e.tag];
        console.log("receive_conversation", e, conv);

        // TODO extract !conv handling, share with conversationClickedInternal
        if (!conv) {
          console.error("Warning: Could not find conversation for receive_conversation event: ", e);
          return;
        }

        // Update conversation
        // TODO check if we have to update 'conversations'
        conv.messages.push({ user: e.user, message: e.message });
        conversationsModel.conversation(conv);
        console.log("new conv:", conv);



      });
  }

  function disconnectInternal() {
    socket.bindMethod(
      'disconnect',
      function(_) {
        console.log("Socket disconnected");
        socket.close();
      },
      function(_) {
        /* whoosh */
      });

    conversationsModel.conversation({topic: "", messages: []});
    conversationsModel.conversationsReceived(false);
    conversationsModel.conversations([]);
    conversationsModel.usersReceived(false);
    conversationsModel.users([]);
    conversationsModel.channel("");
    conversationsModel.channels([]);

    socket.send('disconnect', {});
  }

  function browseChannelInternal(channel) {
    console.log("Channel clicked: " + channel);

    conversationsModel.channel(channel);

    socket.bindMethod(
      'list_users',
      function(usersEvent) {
        console.log("Users on ", conversationsModel.channel(),
                    " are ", usersEvent.users);
        conversationsModel.users(usersEvent.users);
        conversationsModel.usersReceived(true);
      },
      function(err) {
        console.log("failed to get users: ", err);
      });

    socket.bindMethod(
      'list_conversations',
      function(conversationsEvent) {
        var conversationsArray = conversationsEvent.conversations;

        console.log("Conversations in ", conversationsModel.channel(),
                    " are ", conversationsArray);

        conversationsModel.conversations(conversationsArray);
        conversationsModel.conversationsReceived(true);
      },
      function(err) {
        console.log("failed to get conversations: ", err);
      });

    socket.send('list_users', { "channel": channel });
    socket.send('list_conversations', { "channel": channel });
  }

  function conversationClickedInternal(tag) {
    console.log("Conversation clicked: ", tag);

    var conv = conversationsModel.conversationsDict()[tag];
    if (!conv || !conv.tag)
      console.error("Warning: Could not find conversation for tag '"
                    + tag + "' in conversation dict: ", conversationsDict());
    else
      conversationsModel.conversation(conv);
  }

  $(function() {
    console.log("hello logorrhea");

    var conversationsModel = new ConversationsModel();
    window.conversationsModel = conversationsModel;
    ko.applyBindings(conversationsModel);

    $("#usernameInput").focus();
  });

  /* Turns an array of object into a "dictionary" for fast access,
     using the given member of each object as the key.
     Example:

        objectArrayToDict([{ user: 'nh2', age: 20 }], 'user')
        ==
        { 'nh2': { user: 'nh2', age: 20 } }
  */
  function objectArrayToDict(array, keyMemberName) {
    var dict = {};
    ko.utils.arrayForEach(array, function(obj) {
      dict[obj[keyMemberName]] = obj;
    });
    return dict;
  }
});
