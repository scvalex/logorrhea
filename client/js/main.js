require(["jquery", "knockout", "websocket-json-events"], function($) {

  function ConversationsModel() {
    var self = this;

    self.username = ko.observable();
    self.users = ko.observableArray(['scvalex', 'ex_falso', 'rostayob', 'nh2']);
    self.conversations = ko.observableArray([
      { name: 'client_development', topic: 'Why is client development so easy?', users: ['scvalex', 'nh2'] },
      { name: 'server_development', topic: 'Is it OK to have more LANGUAGE pragmas than lines of code?', users: ['ex_falso', 'rostayob'] }
    ]);

    self.connect = function() {
      $('#loginBox').addClass('hidden');
      $('#mainBox').removeClass('hidden');
      connectInternal();
    };
  }

  function connectInternal() {
    console.log("Connecting as " + conversationsModel.username());

    var socket = new FancyWebSocket('ws://localhost:9999/echo');

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
        $("#connectingLabel").text("Disconnected");
      });

    socket.bindDefault(function(event, data) {
      console.log("Unexpected message: " + event + "(" + data + ")");
    });

    socket.bindMethod(
      'connect',
      function(_) {
        console.log("connected ok");
        $("#connectingLabel").text("Connected");
      },
      function(err) {
        console.log("failed to connect: " + reason);
      });
  }

  $(function() {
    console.log("hello logorrhea");

    var conversationsModel = new ConversationsModel();
    window.conversationsModel = conversationsModel;
    ko.applyBindings(conversationsModel);

    $("#usernameInput")[0].focus();
  });
});
