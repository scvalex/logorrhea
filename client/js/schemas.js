define(['jsschema'], function() {

  var schema = jsschema.schema;
  var required = jsschema.required;
  var optional = jsschema.optional;
  var repeated = jsschema.repeated;

  var eventSchema = schema(function() {
    this.event = required('string');
    // keep data optional, it could be of any type or null
  });

  var makeSpecificEventSchema = function(dataSchema) {
    return schema(function() {
      this.event = required('string');
      // TODO check if optional is all right (vs jsschema, null and {} in API spec)
      this.data = optional(dataSchema);
    });
  };

  var conversation_schema = schema(function() {
    this.tag   = required('string');
    this.topic = required('string');
    this.users = repeated('string');
  });


  var eventSchemas = {

    'connect': schema(function() {
    }),

    'connect.ok': schema(function() {
    }),


    // Note that this currently is for both server and client isssued disconnect
    'disconnect': schema(function() {
    }),

    'disconnect.ok': schema(function() {
    }),


    'list_channels': schema(function() {
    }),

    'list_channels.ok': schema(function() {
      this.channels = repeated('string');
    }),


    'list_users': schema(function() {
      this.channel = required('string');
    }),

    'list_users.ok': schema(function() {
      this.channel  = required('string');
      this.users    = repeated('string');
    }),


    'list_conversations': schema(function() {
      this.channel = required('string');
    }),

    'list_conversations.ok': schema(function() {
      this.channel       = required('string');
      this.conversations = repeated(conversation_schema);
    }),


    'join': schema(function() {
      this.channel = required('string');
    }),

    'join.ok': schema(function() {
      this.channel = required('string');
    }),


    'send_channel': schema(function() {
      this.channel = required('string');
      this.message = required('string');
    }),

    'send_channel.ok': schema(function() {
    }),


    'send_conversation': schema(function() {
      this.channel      = required('string');
      this.conversation = required('string');
      this.message      = required('string');
    }),

    'send_conversation.ok': schema(function() {
    }),


    'receive_channel': schema(function() {
      this.channel = required('string');
      this.user    = required('string');
      this.message = required('string');
    }),


    'receive_conversation': schema(function() {
      this.channel      = required('string');
      this.conversation = required('string');
      this.user         = required('string');
      this.message      = required('string');
    })

  };

  var errableMethods = ['connect', 'disconnect', 'list_channels',
                        'list_users', 'list_conversations', 'join',
                        'send_channel', 'send_conversation'];
  for (methodName in errableMethods) {
    eventSchemas[errableMethods[methodName] + ".error"] = schema(function() {
      this.reason = required('string');
    });
  }

  return {
    event: eventSchema,
    makeSpecificEventSchema: makeSpecificEventSchema,
    events: eventSchemas,
  };

});
