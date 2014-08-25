;;;; Compiler ;;;;

;;; Compile lines ;;;

(def cmpln (a)
  (dolns (. (rp (cmp a)) str)))

(var *indlvl* 0)
(var *indup* 2)

(mac wind a
  `(dyn *indlvl* (+ *indlvl* *indup*)
      ,@a))

(def indent (a)
  (app (nof *indlvl* " ") a))

(def dolns (a)
  (case a
    lns? (joi (map dolns (rp a)))
    ind? (joi (map [wind (dolns _)] (rp a)))
    [or (syn? _) (str? _)] (indent (app a "\n"))
    (err dolns "What? a = $1" a)))

;;; Main compile ;;;

(def cmp (a)
  (if (atm? a)
    (case a
      nil? (cmp 'nil)
      num? (rt 'atm a)
      sym? (rt 'atm (jvar a))
      str? (rt 'atm (dsp a))
      (err cmp "Unknown atom a = $1" a))
    (let b (car a)
      (if (atm? b)
            (if (sym? b) (cprc b (cdr a))
                (cmp (cons 'js-cal a)))
          (cmp (cons 'js-cal a))))))

(def cprc (p a)
  (if (beg p 'js-)
        (case (sli p 3)
          'cal (ccal a)
          'blk (cblk a)
          'blk2 (cblk2 a)
          'whi (cwhi a)
          (err cprc "Unknown p = $1" p))
      (cmp (lisd 'js-cal p a))))

;;; Positions ;;;

(var *rts* nil)
(def cmppos (a ret)
  (sta *rts* ret
    (let r (rp (cmp a))
       (if (blkrt? (. r ret)) (rtblk (. r ret) (. r str))
           (rtinl (. r ret) (. r str))))))

(def rtblk (ret str)
  (if (inblk?) (rt ret str)
      (err rtblk "Can't return blk $1 to a non-blk pos $2" ret (car *rts*))))

(def rtinl (ret str)
  (if (inblk?)
        (if (inret?) (rt ret (app "return " str ";") t)
            (rt ret (app (blkbrc ret str) ";")))
      (brc ret str)))

(var *blkpss* nil)

(def inblk? ()
  (has (car *rts*) *blkpss*))

(var *blkrts* nil)

(def blkrt? (a)
  (has a *blkrts*))

(var *rets* nil)
(var *ends* nil)

(def addblk (a . opt)
  (psh a *blkpss*)
  (if (has 'las opt)
        (let alas (app a 'las)
          (psh alas *blkpss*)
          (if (has 'ret opt) (psh alas *rets*))
          (if (has 'end opt) (psh alas *ends*)))
      (do (if (has 'ret opt) (psh a *rets*))
          (if (has 'end opt) (psh a *ends*))))
  nil)

(def addblkrt (a . opt)
  (psh a *blkrts*))

(def inret? ()
  (inret1? *rts*))

(def inret1? (rts)
  (if (no rts) nil
      (has (car rts) *rets*) t
      (has (car rts) *ends*) (inret1? (cdr rts))
      nil))

(var *preclis*
  '((bot)
    inln
    refee
    atm))

(def prec (a)
  (let n (posm a *preclis*)
    (if (is n -1) (err prec "Unknown a = $1" a)
        n)))

(def hasprec (a b)
  (>= (prec a) (prec b)))

(def posm (x a (o n 0))
  (if (no a) -1
      (lis? (car a))
        (let p (pos x (car a))
          (if (isn p -1) n
              (posm x (cdr a) (+ n 1))))
      (is (car a) x) n
      (posm x (cdr a) (+ n 1))))

(def brc (cr a)
  (if (hasprec cr (car *rts*)) a
      (app "(" a ")")))

(def blkbrc (cr a)
  (if (in cr 'fn 'rfn 'obj) (app "(" a ")")
      a))

;;; Compile all ;;;

(def cpa (a ret)
  (map [cmppos _ ret] a))

(def cpalas (a ret)
  (if (no a) nil
      (atm? a) (err cpalas "Can't cmp improper list a = $1" a)
      (no (cdr a)) (lis (cmppos (car a) (app ret 'las)))
      (cons (cmppos (car a) ret) (cpalas (cdr a) ret))))

;;; Procedures ;;;

(def jvar? (a)
  (has #"^[a-zA-Z$_][a-zA-Z0-9$_]*$" a))

(def var? (a)
  (has #"^[a-zA-Z$_][a-zA-Z0-9$_?-]*$" a))

(def jvar (a)
  (if (jvar? a) a
      (var? a)
        (let s ""
          (ind i a
            (case (a i)
              "-" (do (+= s (upp (a (+ i 1))))
                      (++ i))
              "?" (+= s "p")
              (+= s (a i))))
          s)
      (err jvar "Can't coerce a = $1 to jvar" a)))

(def ccal (a)
  (rt 'atm (app (cmppos (car a) 'refee) (mpar (cdr a)))))

(def mpar (a)
  (app "(" (joi (cpa a 'inln) ", ") ")"))

(def cblk (a)
  (let bd (cpalas a 'blk)
    (if (is (len a) 0) (rt 'blk "{}")
        (rt 'blk (lns (lis "{" (idn bd) "}"))))))

(addblk 'blk 'las 'ret)
(addblkrt 'blk)

(def cblk2 (a)
  (let bd (cpalas a 'blk2)
    (if (is (len a) 0) (rt 'blk2 "{}")
        (rt 'blk2 (lns (lis "{" (idn bd) "}"))))))

(addblk 'blk2 'las 'end)
(addblkrt 'blk2)

(def cwhi (a)
  (with (tes (cmppos (car a) 'bot)
         bd (cpa (cdr a) 'lop))
    (if (is (len bd) 0) (rt 'whi (str "while (" tes ");"))
        (and (is (len bd) 1) (not (needbrace (car bd))))
          (rt 'whi (str "while (" tes ")" (. (rp (car bd)) str)))
        (rt 'whi (lns (lis (str "while (" tes "){") (idn bd) "}"))))))

(var *brarts* nil)

(over addblkrt (a . opt)
  (if (has 'bra opt) (psh a *brarts*))
  (apl sup (cons a opt)))

(def needbrace (a)
  (assert (rt? a))
  (has (. (rp a) ret) *brarts*))

(addblkrt 'whi 'bra)
(addblk 'lop)

;;; Properties ;;;

;;; Types ;;;

(def lns (a)
  (tg 'lines (lns2 a)))

(def lns2 (a)
  (if (no a) nil
      (case (car a)
        lns? (app (rp (car a) (lns2 (cdr a))))
        rt? (cons (. (rp (car a)) str) (lns2 (cdr a)))
        (cons (car a) (lns2 (cdr a))))))

(def idn (a)
  (tg 'ind (lns2 a)))

(def rt (ret str)
  (tg 'ret {ret ret str str}))

(def isa (a x)
  (is (typ a) x))

(def lns? (a)
  (isa a 'lines))

(def rt? (a)
  (isa a 'ret))

(def ind? (a)
  (isa a 'ind))

;;; Compile from str ;;;

(def cmps (a)
  (cmpln (prs a)))

(def cmpp (a)
  (pr (cmps a)))
