// TODO make it a require.js module

var schema = jsschema.schema;
var required = jsschema.required;
var optional = jsschema.optional;

var eventSchema = function(dataSchema) {
  return schema(function() {
    this.event = required('string'),
    // TODO check if optional is all right (vs jsschema, null and {} in API spec)
    this.data = optional(dataSchema)
  });
};

var schemas = {
  receive_channel: eventSchema(schema(function() {
    this.channel = required('string');
    this.user    = required('string');
    this.message = required('string');
  }))
};

