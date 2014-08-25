/***** Lisp to JS Basic Compiler Devel *****/

/* require tools >= 3.1 */
/* require lisp-tools */
/* require lisp-parse */ // cmps uses this
/* require lisp-exec */

(function (win, udf){
  
  //
  
  var rts;
  function cmp(a, ret){
    if (udfp(ret)){
      rts = [];
      var c = cmp1(a);
      if (nilp(c))return "";
      return c.t;
    }                                                          
    $.L.psh(ret, rts);
    var r = cmp1(a);
    $.L.pop(rts);
    return r;
  }
  
  function cmp1(a){
    if (atmp(a)){
      if (nilp(a))return chkrt("[]", "atm");
      if (nump(a))return chkrt(a, "atm");
      if (symp(a))return chkrt(jvar(a), "atm");
      if (strp(a))return chkrt($.dsp(rp(a)), "atm");
      if (rgxp(a))return chkrt($.str(a), "atm");
      return cmp1([]);
    }
    var o = car(a);
    if (atmp(o)){
      if (symp(o))return cprc(o, cdr(a));
      return cmp1(cons("js-cal", a));
    }
    return cmp1(cons("js-cal", a));
  }
  
  function cprc(o, a){
    if (beg(o, "js-")){
      switch (sli(o, 3)){
        case "cal": return ccal(a);
        case "do": return cdo(a);
        case "while": return cwhi(a);
        default: err(cprc, "Unknown o = $1", o);
      }
    }
    return cmp1(lisd("js-cal", o, a));
  }
  
  //// Compile all ////
  
  function cpa(a, ret){
    return map(function (x){
      return inpos(ret, cmp(x));
    }, a);
  }
  
  //// Blocks ////
  
  
  /*
  Block format:
  (while 1 2 (while 3 4 5) 6)
  -> (2 {ret "while" str "while (3){4; 5)})
  */
  
  function mblk(a, ret){
    return map(function (x){
      return cmp(x, ret);
    }, a);
  }
  
  function mpar(a){
    return "(" + jjoi(cpa(a, "inln"), ", ") + ")";
  }
  
  function chkbra(a){
    if (len(a) == "1" && !objp(car(a))){
      return car(a);
    }
    return "{\n" + jjoi(map(function (x){
      if ($.strp(x))return x;
      if ($.objp(x))return x.str;
      err(chkbra, "Unknown type of x = $1", x);
    }, a)) + "}\n";
  }
  
  //// Return ////
  
  function chkrt(a, cr){
    return a;
  }
  
  //// Procedures ////
  
  function ccal(a){
    return rt("atm", inpos("refee", cmp(car(a))) + mpar(cdr(a)));
  }
  
  function cdo(a){
    var bd = cpa(a, "do");
    return rt("do", lns(bd));
  }
  
  function cwhi(a){
    var s = "while (" + cmp(car(a), "bot") + ")";
    var bd = cpa(cdr(a), "lop");
    if (brap(bd))return rt("lop", lns(lis(s+"{", ind(bd), "}")));
    return rt("lop", s+car(bd));
  }
  
  function lns(a){
    return tg("lines", lns2(a));
  }
  
  function lns2(a){
    if (nilp(a))return [];
    var o = car(a);
    if (lnsp(o))return app(rp(o), lns2(cdr(a)));
    if (rtp(o)){
      if (inlp(o))return cons(rp(o).str+";", lns2(cdr(a)));
      return cons(rp(o).str, lns2(cdr(a)));
    }
    return cons(o, lns2(cdr(a)));
  }
  
  function ind(a){
    return tg("ind", lns2(a));
  }
  
  function rt(ret, str){
    return tg("ret", {ret: ret, str: str});
  }
  
  function isa(x, a){
    return typ(a) === x;
  }
  
  function lnsp(a){
    return isa("lines", a);
  }
  
  function rtp(a){
    return isa("ret", a);
  }
  
  function indp(a){
    return isa("ind", a);
  }
  
  function brap(a){
    return gt(len(a), "1") || objp(car(a));
  }
  
  ////// Compile from str //////
  
  function cmps(a){
    return cmp(prs(a));
  }
  
  ////// Object exposure //////
  
  $.att({
    cmp: cmp,
    cmps: cmps
  }, L);
  
  ////// Testing //////
  
  
  
})(window);
