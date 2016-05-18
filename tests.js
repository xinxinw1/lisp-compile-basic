QUnit.test('Compiler', function (assert){
  assert.testevl("(js-var 'a)", "\"a\"");
  assert.testevl("(js-var '-)", "\"sub\"");
  assert.testevl("(js-var 'a-b)", "\"aB\"");
  assert.testevl("(js-var 'what*is)", "\"whatmulis\"");
  assert.testevl("(js-var '*)", "\"mul\"");
  assert.testevl("(js-var '*hey*)", "\"HEY\"");
  
  assert.testcmp("1", "1;");
  assert.testcmp("test", "test;");
  assert.testcmp("*", "mul;");
  assert.testcmp("\"test\"", "\"test\";");
  assert.testcmp("()", "[];");
  assert.testevl("(make-in-line '(1 2 3))", "<lin {data (\"1\" \", \" \"2\" \", \" \"3\")}>");
  assert.testcmp("(random-function 1 2 3)", "randomFunction(1, 2, 3);");
  
  assert.testcmp("(arr 1 2 3)", "[1, 2, 3];");
  assert.testcmp("(arr (do 1 2) 2 3)", "[(1, 2), 2, 3];");
  assert.testcmp("(do 1 2 3)", "1;\n2;\n3;");
  assert.testcmp("(do (do 1 2) (do 3 4))", "1;\n2;\n3;\n4;");
  assert.testcmp("(do)", "[];");
  assert.testcmp("(arr (do 1 2))", "[(1, 2)];");
  assert.testcmp("'(1 2 3)", "[1, [2, [3, []]]];");
  assert.testcmp("(while t)", "while (t);");
  assert.testcmp("(while t 1 2 3)", "while (t){\n  1;\n  2;\n  3;\n}");
  assert.testcmp("(while t (do 1 2 3))", "while (t){\n  1;\n  2;\n  3;\n}");
  
  assert.testcmp("(loop 2 3 4)", "for (2; 3; 4);");
  assert.testcmp("(loop 2 3 4 5 6 7)", "for (2; 3; 4){\n  5;\n  6;\n  7;\n}");
});
