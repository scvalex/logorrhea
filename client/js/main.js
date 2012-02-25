require(["jquery", "knockout", "websocket-json-events"], function($) {

  function ConversationsModel() {
    var self = this;

    self.users = ko.observableArray(['scvalex', 'ex_falso', 'rostayob', 'nh2']);
    self.conversations = ko.observableArray([
      { name: 'client_development', topic: 'Why is client development so easy?', users: ['scvalex', 'nh2'] },
      { name: 'server_development', topic: 'Is it OK to have more LANGUAGE pragmas than lines of code?', users: ['ex_falso', 'rostayob'] }
    ]);
  }

  $(function() {
    console.log("hello logorrhea");

    var conversationsModel = new ConversationsModel();
    window.conversationsModel = conversationsModel;
    ko.applyBindings(conversationsModel);

    var socket = new FancyWebSocket('ws://nh2.me:9999/echo');

    socket.bind('open', function() {
      console.log('open');
      socket.send('channel1', JSON.stringify({something: 'value'}));
    });
    socket.bind('channel1', function(data){
      console.log("on channel 1: " + data);
    });
  });
});
