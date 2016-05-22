QUnit.test('ret-obj', function (assert){
  assert.testevl("(make-ret-obj 'test 3)", "<ret {ret-type test flags {} data 3}>");
  assert.testevl("(make-ret-obj 'test 3 {a t})", "<ret {ret-type test flags {a t} data 3}>");
  assert.testevl("(get-ret-type (make-ret-obj 'test 3))", "test");
  assert.testevl("(get-ret-flag (make-ret-obj 'test 3 {a t}) 'a)", "t");
  assert.testevl("(ret-obj 'test (set-ret-flag 'a t) 3)", "<ret {ret-type test flags {a t} data 3}>");
  assert.testevl("(ret-obj 'test (fobj (pass-ret-flags (make-ret-obj 'test 3 {a t}))))", "<ret {ret-type test flags {a t} data 3}>");
});

QUnit.test('js-var', function (assert){
  assert.testevl("(js-var 'a)", "\"a\"");
  assert.testevl("(js-var '-)", "\"sub\"");
  assert.testevl("(js-var 'a-b)", "\"aB\"");
  assert.testevl("(js-var 'what*is)", "\"whatmulis\"");
  assert.testevl("(js-var '*)", "\"mul\"");
  assert.testevl("(js-var '*hey*)", "\"HEY\"");
  
  assert.testevl("(not-nil 'err a nil a)", "err");
  assert.testevl("(not-nil 'err a 5 a)", "5");
  
  assert.testevl("(not-neg-one -1 'err)", "err");
  assert.testevl("(not-neg-one 5 'err)", "5");
});

QUnit.test('Atoms', function (assert){
  assert.testcmp("1", "1;");
  assert.testcmp("test", "test;");
  assert.testcmp("*", "mul;");
  assert.testcmp("\"test\"", "\"test\";");
  assert.testcmp("()", "[];");
});

QUnit.test('Inline', function (assert){
  assert.testevl("(make-in-line '(1 2 3))", "<lin {data (\"1\" \", \" \"2\" \", \" \"3\")}>");
  assert.testcmp("(random-function 1 2 3)", "randomFunction(1, 2, 3);");
  assert.testcmp("(arr (random-function 1 2 3))", "[randomFunction(1, 2, 3)];");
  assert.testcmp("(arr 1 2 3)", "[1, 2, 3];");
});

QUnit.test('do', function (assert){
  assert.testcmp("(arr (do 1 2) 2 3)", "[(1, 2), 2, 3];");
  assert.testcmp("(do 1 2 3)", "1;\n2;\n3;");
  assert.testcmp("(do (do 1 2) (do 3 4))", "1;\n2;\n3;\n4;");
  assert.testcmp("(do)", "[];");
  assert.testcmp("(arr (do 1 2))", "[(1, 2)];");
  assert.testcmp("(do 1 () 3)", "1;\n3;");
  assert.testcmp("(do 1 ())", "1;\n[];");
});

QUnit.test('Quote', function (assert){
  assert.testcmp("'(1 2 3)", "[1, [2, [3, []]]];");
});

QUnit.test('loops', function (assert){
  assert.testcmp("(while t)", "while (t);");
  assert.testcmp("(while t 1)", "while (t)1;");
  assert.testcmp("(while t 1 2 3)", "while (t){\n  1;\n  2;\n  3;\n}");
  assert.testcmp("(while t (do 1 2 3))", "while (t){\n  1;\n  2;\n  3;\n}");
  
  assert.testcmp("(loop 2 3 4)", "for (2; 3; 4);");
  assert.testcmp("(loop 2 3 4 5)", "for (2; 3; 4)5;");
  assert.testcmp("(loop 2 3 4 5 6 7)", "for (2; 3; 4){\n  5;\n  6;\n  7;\n}");
});

QUnit.test('js-add', function (assert){
  //assert.testevl("(mcx1 (mcx1 '(make-comp-bin test \"+\")))", "(do (def comp-js-test (a b) (ret-obj 'test-obj (lin (comp-and-pip 'test-left a) \"+\" (comp-and-pip 'test-right b)))) (set-function-compile-fn js-test comp-js-test))");
  
  assert.testcmp("(js-add 1 2)", "1+2;");
  assert.testcmp("(js-add (js-sub 1 2) 3)", "1-2+3;");
  assert.testcmp("(js-add 1 (js-sub 2 3))", "1+2-3;");
  assert.testcmp("(js-add (js-add 1 2) 3)", "1+2+3;");
  assert.testcmp("(js-add 1 (js-add 2 3))", "1+2+3;");
  assert.testcmp("(js-add (do 1 2) 3)", "(1, 2)+3;");
});

QUnit.test('js-sub', function (assert){
  assert.testcmp("(js-sub 1 2)", "1-2;");
  assert.testcmp("(js-sub (js-add 1 2) 3)", "1+2-3;");
  assert.testcmp("(js-sub 1 (js-add 2 3))", "1-(2+3);");
  assert.testcmp("(js-sub (js-sub 1 2) 3)", "1-2-3;");
  assert.testcmp("(js-sub 1 (js-sub 2 3))", "1-(2-3);");
  
  assert.testcmp("(arr (js-add 1 2) (js-sub 1 2))", "[1+2, 1-2];");
});

QUnit.test('js-mul', function (assert){
  assert.testcmp("(js-mul 1 2)", "1*2;");
  assert.testcmp("(js-mul (js-add 1 2) 3)", "(1+2)*3;");
  assert.testcmp("(js-mul 1 (js-add 2 3))", "1*(2+3);");
  assert.testcmp("(js-mul (js-div 1 2) 3)", "1/2*3;");
  assert.testcmp("(js-mul 1 (js-div 2 3))", "1*2/3;");
  assert.testcmp("(js-mul (js-mul 1 2) 3)", "1*2*3;");
  assert.testcmp("(js-mul 1 (js-mul 2 3))", "1*2*3;");
});

QUnit.test('js-div', function (assert){
  assert.testcmp("(js-div 1 2)", "1/2;");
  assert.testcmp("(js-div (js-add 1 2) 3)", "(1+2)/3;");
  assert.testcmp("(js-div 1 (js-add 2 3))", "1/(2+3);");
  assert.testcmp("(js-div (js-mul 1 2) 3)", "1*2/3;");
  assert.testcmp("(js-div 1 (js-mul 2 3))", "1/(2*3);");
  assert.testcmp("(js-div (js-div 1 2) 3)", "1/2/3;");
  assert.testcmp("(js-div 1 (js-div 2 3))", "1/(2/3);");
});

QUnit.test('js-set', function (assert){
  assert.testcmp("(js-set a 2)", "a = 2;");
  assert.testcmp("(js-set (js-add 1 2) 3)", "1+2 = 3;");
  assert.testcmp("(js-set 1 (js-add 2 3))", "1 = 2+3;");
  assert.testcmp("(js-set (js-set 1 2) 3)", "(1 = 2) = 3;");
  assert.testcmp("(js-set 1 (js-set 2 3))", "1 = 2 = 3;");
  
  assert.testcmp("(arr (js-set 1 2))", "[1 = 2];");
});

QUnit.test('if', function (assert){
  assert.testcmp("(if 1 2)", "if (1)2;");
  assert.testcmp("(if 1 2 3)", "if (1)2;\nelse 3;");
  assert.testcmp("(if 1 (do 2 3))", "if (1){\n  2;\n  3;\n}");
  assert.testcmp("(if 1 (do 2 3) 3)", "if (1){\n  2;\n  3;\n} else 3;");
  assert.testcmp("(if 1 (do 2 3) (do 3 4))", "if (1){\n  2;\n  3;\n} else {\n  3;\n  4;\n}");
  assert.testcmp("(if 1 (do 2 3) 3 4)", "if (1){\n  2;\n  3;\n} else if (3)4;");
  assert.testcmp("(if 1 (do 2 3) 3 (do 4 5))", "if (1){\n  2;\n  3;\n} else if (3){\n  4;\n  5;\n}");
  assert.testcmp("(if 1 (do 2 3) (if 3 (do 4 5)))", "if (1){\n  2;\n  3;\n} else if (3){\n  4;\n  5;\n}");
  
  assert.testcmp("(while t (if 1 2 3))", "while (t){\n  if (1)2;\n  else 3;\n}");
  
  assert.testcmp("(if 1 (if 2 3 4) 2)", "if (1){\n  if (2)3;\n  else 4;\n} else 2;");
  
  assert.testcmp("(arr (if 1 2 3))", "[1?2:3];");
  assert.testcmp("(arr (if 1 2 3 4 5))", "[1?2:3?4:5];");
  assert.testcmp("(arr (if (if 1 2 3) 2 3 4 5))", "[1?2:3?2:3?4:5];");
  assert.testcmp("(arr (if (do 1 2) 2 3))", "[(1, 2)?2:3];");
  assert.testcmp("(arr (if (js-set 1 2) 2 3))", "[(1 = 2)?2:3];");
});

QUnit.test('return', function (assert){
  assert.testcmp("(return 3)", "return 3;");
  assert.testcmp("(return (if 1))", "return 1;");
  assert.testcmp("(return (if 1 2))", "if (1)return 2;\nreturn [];");
  assert.testcmp("(return (if 1 2 3))", "if (1)return 2;\nreturn 3;");
  assert.testcmp("(return (if 1 2 3 4 5))", "if (1)return 2;\nif (3)return 4;\nreturn 5;");
  
  assert.testcmp("(return (if 1 (if 1 2 3) 4))", "if (1){\n  if (1)return 2;\n  return 3;\n}\nreturn 4;");
  
  assert.testcmp("(return (if 1 (do 1 2) 4))", "if (1){\n  1;\n  return 2;\n}\nreturn 4;");
  
  assert.testcmp("(return (if 1 (return 1) 4))", "if (1)return 1;\nreturn 4;");
  
  assert.testcmp("(return (do 1 2 3))", "1;\n2;\nreturn 3;");
  assert.testcmp("(do (return 1) 2)", "return 1;\n2;");
  
  assert.throws(function (){
    L.cmps("(if (return 1) 2 3)");
  });
  
  assert.testcmp("(if 1 (return (do 1 2 3)))", "if (1){\n  1;\n  2;\n  return 3;\n}");
  
});
