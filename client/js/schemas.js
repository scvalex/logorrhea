// TODO make it a require.js module

var schema = jsschema.schema;
var required = jsschema.required;
var optional = jsschema.optional;
var repeated = jsschema.repeated;

var eventSchema = function(dataSchema) {
  return schema(function() {
    this.event = required('string'),
    // TODO check if optional is all right (vs jsschema, null and {} in API spec)
    this.data = optional(dataSchema)
  });
};

var conversation_schema = schema(function() {
  this.hash  = required('string');
  this.topic = required('string');
});


var schemas = {

  'connect': eventSchema(schema(function() {
  })),

  'connect.ok': eventSchema(schema(function() {
  })),


  'disconnect': eventSchema(schema(function() {
  })),

  'disconnect.ok': eventSchema(schema(function() {
  })),


  'list_channels': eventSchema(schema(function() {
  })),

  'list_channels.ok': eventSchema(schema(function() {
    this.channels = repeated('string');
  })),


  'list_users': eventSchema(schema(function() {
    this.channel = required('string');
  })),

  'list_users.ok': eventSchema(schema(function() {
    this.channel  = required('string');
    this.users    = repeated('string');
  })),


  'list_coversations': eventSchema(schema(function() {
    this.channel = required('string');
  })),

  'list_coversations.ok': eventSchema(schema(function() {
    this.channel       = required('string');
    this.conversations = repeated(conversation_schema);
  })),


  'join': eventSchema(schema(function() {
    this.channel = required('string');
  })),

  'join.ok': eventSchema(schema(function() {
    this.channel = required('string');
  })),


  'send_channel': eventSchema(schema(function() {
    this.channel = required('string');
    this.message = required('string');
  })),

  'send_channel.ok': eventSchema(schema(function() {
  })),


  'send_conversation': eventSchema(schema(function() {
    this.channel      = required('string');
    this.conversation = required('string');
    this.message      = required('string');
  })),

  'send_conversation.ok': eventSchema(schema(function() {
  })),


  'receive_channel': eventSchema(schema(function() {
    this.channel = required('string');
    this.user    = required('string');
    this.message = required('string');
  })),


  'receive_conversation': eventSchema(schema(function() {
    this.channel      = required('string');
    this.conversation = required('string');
    this.user         = required('string');
    this.message      = required('string');
  }))

};
