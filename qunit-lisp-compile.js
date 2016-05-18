QUnit.assert.testcmp = function (a, b){
  this.same(L.dat(L.lispCompile(L.st(a))), b, "compiling " + a);
};