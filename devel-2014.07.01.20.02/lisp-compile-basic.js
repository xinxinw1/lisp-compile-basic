/***** Lisp to JS Basic Compiler Devel *****/

/* require tools >= 3.1 */
/* require lisp-tools */
/* require lisp-parse */ // cmps uses this
/* require lisp-exec */

(function (win, udf){
  ////// Import //////
  
  var typ = L.typ;
  var tg = L.tg;
  var rp = L.rp;
  
  var nilp = L.nilp;
  var lisp = L.lisp;
  var atmp = L.atmp;
  var synp = L.synp;
  var symp = L.symp;
  var nump = L.nump;
  var objp = L.objp;
  var rgxp = L.rgxp;
  var udfp = L.udfp;
  var strp = L.strp;
  var arrp = L.arrp;
  
  var map = L.map;
  var mapn = L.mapn;
  
  var len = L.len;
  
  var sli = L.sli;
  
  var joi = L.joi;
  var app = L.app;
  
  var beg = L.beg;
  
  var car = L.car;
  var cdr = L.cdr;
  var cons = L.cons;
  
  var caar = L.caar;
  var cadr = L.cadr;
  var cdar = L.cdar;
  var cddr = L.cddr;
  
  var lis = L.lis;
  var lisd = L.lisd;
  var nth = L.nth;
  var ncdr = L.ncdr;
  
  var prs = L.prs;
  var evl = L.evl;
  var apl = L.apl;
  
  var err = L.err;
  
  
  ////// Compiler //////
  
  function cmpln(a){
    return dolns(rp(cmp(a)).str);
  }
  
  function cmp(a){
    return cmp1(a);
  }
  
  var indlvl = 0;
  function dolns(a){
    if (lnsp(a))return jjoi(mapn(function (a){
      return dolns(a);
    }, rp(a)));
    if (indp(a))return jjoi(mapn(function (a){
      indlvl += 2;
      var r = dolns(a);
      indlvl -= 2;
      return r;
    }, rp(a)));
    if (synp(a))return $.nof(indlvl, " ")+a+"\n";
    err(dolns, "What?");
  }
  
  function jjoi(a, x){
    return rp(joi(a, x));
  }
  
  function cmp1(a){
    if (atmp(a)){
      if (nump(a))return rt("atm", a);
      err(cmp1, "Unknown atom a = $1", a);
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
        case "cal": err(cprc, "cal unsupported");
        case "blk": return cblk(a);
        default: err(cprc, "Unknown o = $1", o);
      }
    }
    return cmp1(lisd("js-cal", o, a));
  }
  
  //// Compile all ////
  
  function cpa(a){
    return map(function (x){
      return cmp1(x);
    }, a);
  }
  
  //// Blocks ////
  
  
  
  //// Return ////
  
  //// Procedures ////
  
  function cblk(a){
    var bd = cpa(a);
    if (len(a) === "0")return rt("blk", "{}");
    return rt("blk", lns(lis("{", ind(bd), "}")));
  }
  
  function lns(a){
    return tg("lines", lns2(a));
  }
  
  function lns2(a){
    if (nilp(a))return [];
    var o = car(a);
    if (lnsp(o))return app(rp(o), lns2(cdr(a)));
    if (rtp(o)){
      if (rp(o).ret === "atm")return cons(rp(o).str+";", lns2(cdr(a)));
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
  
  /*function brap(a){
    return gt(len(a), "1") || objp(car(a));
  }*/
  
  ////// Compile from str //////
  
  function cmps(a){
    return cmpln(prs(a));
  }
  
  ////// Object exposure //////
  
  $.att({
    cmp: cmp,
    cmps: cmps
  }, L);
  
  ////// Testing //////
  
  
  
})(window);
