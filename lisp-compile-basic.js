/***** Lisp Compile 0.1 *****/

/* require tools 4.6.1 */
/* require ajax 4.5.1 */
/* require prec-math 4.3.2 */
/* require lisp-tools 0.1 */
/* require lisp-parse 0.1 */
/* require lisp-exec 0.1.1 */

(function (win, udf){
  ////// Import //////
  
  var udfp = $.udfp;
  
  ////// JS functions //////
  
  L.evlf($.libdir + "/lisp-format/lisp-format.lisp");
  L.evlf($.libdir + "/lisp-compile-basic/lisp-compile-basic.lisp");
  //L.jcal("compile", L.st($.get($.libdir + "/lisp-compile-basic/lisp-cmp-core.lisp")));
  
  // s is a lisp string
  // returns a lisp string
  function lispCompile(s){
    return L.jcal("compile", s);
  }
  
  ////// Object exposure //////
  
  $.att(L, {
    lispCompile: lispCompile
  });
  
  ////// Testing //////
  
  
  
})(window);
