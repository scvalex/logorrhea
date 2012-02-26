// Use _ for jsschema module to use global jsschema object
require(['jsschema', 'schemas'], function(_, schemas) {

  var expectSchemaPass = function (schema, object) {
    // TODO move this into jsschema?
    if (!schema)
      throw new Error("schema evaluates to false!")
    return expect(jsschema.check(schema, object)).toEqual(object);
  }

  var expectSchemaFail = function (schema, object) {
    // TODO move this into jsschema?
    if (!schema)
      throw new Error("schema evaluates to false!")
    expect(function() {
      jsschema.check(schema, object);
    }).toThrow();
  }

  describe('API implementation', function () {

    it('checks full event schemas', function () {

      runs(function () {

        var event_receive_channel_ok_1 = {
          event: 'receive_channel',
          data: {
            channel: 'haskell',
            user: 'scvalex',
            message: 'Yes, you can just open a new conversation.'
          }
        };

        var event_receive_channel_fail_1 = {
          event: 'receive_channel',
          data: {
            channel: 'haskell',
            user: 20,
            message: 'Yes, you can just open a new conversation.'
          }
        };

        expectSchemaPass(schemas.makeSpecificEventSchema(schemas.events.receive_channel), event_receive_channel_ok_1);
        expectSchemaFail(schemas.makeSpecificEventSchema(schemas.events.receive_channel), event_receive_channel_fail_1);
      });

    });

    it('checks the data part of schemas', function () {

      runs(function () {

        var receive_channel_ok_1 = {
          channel: 'haskell',
          user: 'scvalex',
          message: 'Yes, you can just open a new conversation.'
        }

        var receive_channel_fail_1 = {
          channel: 'haskell',
          user: 'nh2',
          message: null
        }

        var receive_channel_fail_2 = {
          user: 'nh2',
          message: 'message'
        }

        expectSchemaPass(schemas.events.receive_channel, receive_channel_ok_1);
        expectSchemaFail(schemas.events.receive_channel, receive_channel_fail_1);
        expectSchemaFail(schemas.events.receive_channel, receive_channel_fail_2);
      });

    });
  });

});
