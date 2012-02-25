var expectSchemaPass = function (schema, object) {
  return expect(jsschema.check(schema, object)).toEqual(object);
}

var expectSchemaFail = function (schema, object) {
  expect(function() {
    jsschema.check(schema, object);
  }).toThrow();
}

var receive_channel_ok_1 = {
  event: 'receive_channel',
  data: {
    channel: 'haskell',
    user: 'scvalex',
    message: 'Yes, you can just open a new conversation.'
  }
};

var receive_channel_fail_1 = {
  event: 'receive_channel',
  data: {
    channel: 'haskell',
    user: 20,
    message: 'Yes, you can just open a new conversation.'
  }
};

describe('API implementation', function () {
  it('checks schemas', function () {

    runs(function () {

      expectSchemaPass(schemas.receive_channel, receive_channel_ok_1);

      expectSchemaFail(schemas.receive_channel, receive_channel_fail_1);
    });
  });
});
