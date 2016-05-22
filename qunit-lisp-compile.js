QUnit.assert.testcmp = function (a, b){
  this.same(L.cmps(a), b, "compiling " + a);
};