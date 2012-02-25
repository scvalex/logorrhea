describe('exmaple test suite', function() {
  it('should increment a variable', function () {
    var foo = 0;
    foo++;
    expect(foo).toEqual(1);
  });
});

describe('async example test suite', function () {
  it('should increment a variable', function () {
    runs(function () {
      this.foo = 0;
      var that = this;
      setTimeout(function () {
        that.foo++;
      }, 250);
    });

    runs(function () {
      expect(this.foo).toEqual(0);
    });

    waits(500);

    runs(function () {
      expect(this.foo).toEqual(1);
    });
  });
});
