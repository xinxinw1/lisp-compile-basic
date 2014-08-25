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
  
  var inp = L.inp;
  
  var al = L.al;
  
  var map = L.map;
  var mapn = L.mapn;
  var has = L.has;
  
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
  
  var rts;
  function cmp(a, ret){
    if (udfp(ret)){
      rts = [];
      return cmp1(a);
    }
    $.L.psh(ret, rts);
    //al(rts);
    var r = cmp1(a);
    $.L.pop(rts);
    return r;
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
  
  function cpa(a, ret){
    return map(function (a){
      return cmp(a, ret);
    }, a);
  }
  
  function cpalas(a, ret){
    if (nilp(a))return [];
    if (atmp(a))err(cpalas, "Can't cmp improper list a = $1", a);
    if (nilp(cdr(a)))return lis(cmp(car(a), ret+"las"));
    return cons(cmp(car(a), ret), cpalas(cdr(a), ret));
  }
  
  //// Blocks ////
  
  
  
  //// Return ////
  
  //// Procedures ////
  
  function cblk(a){
    var bd = cpalas(a, "blk");
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
    if (rtp(o))return cons(rp(o).str, lns2(cdr(a)));
    return cons(o, lns2(cdr(a)));
  }
  
  var blks = [
    "blk", "blklas"
  ];
  function blkp(rts){
    if (nilp(rts))return true;
    return $.has(car(rts), blks);
  }
  
  var rets = ["blklas"];
  var ends = ["blk2las"];
  function retp(rts){
    if (nilp(rts))return false;
    //al(has(car(rts), rets))
    if ($.has(car(rts), rets))return true;
    if ($.has(car(rts), ends))return isret(cdr(rts));
  }
  
  function ind(a){
    return tg("ind", lns2(a));
  }
  
  function rt(ret, str){
    if (blkp(rts)){
      if (retp(rts))return rt1(ret, "return " + str + ";", true);
      return rt1(ret, blkbrc(str, ret) + ";");
    }
    al(rts);
    al(blkp(rts));
    return brc(str, ret, car(rts));
  }
  
  function rtblk(ret, str){
    if (!blkp(rts))err(rt, "Can't return blk $1 to a non-blk pos $2", ret, car(rts));
    return rt1(ret, str);
  }
  
  function brc(a, cr, ret){
    if (!hasprec(cr, ret))return "(" + a + ")";
    return a;
  }
  
  function blkbrc(a, cr){
    if (inp(cr, "fn", "rfn", "obj"))return "(" + a + ")";
    return a;
  }
  
  function rt1(ret, str, r){
    if (r === udf)r = false;
    return tg("ret", {ret: ret, str: str, r: r});
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
