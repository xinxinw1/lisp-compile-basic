;;;; Compiler ;;;;

#|
Examples:

(prn (cmp '(js-loop (set x 3) (lt x 5) (pp x) (js-def hey (a b c) (js-if test 1 2 3 4 5)) 3 4 5)))

|#

;;; Main compile ;;;

(def cmp (a)
  (proc (cmp1 a)))

(def cmp1 (a)
  (if (atm? a)
    (case a
      nil? (send 'nil)
      num? (call num a)
      sym? (if (smset? a) (send (xsmcal a))
               (call sym a))
      str? (call str a)
      (err cmp1 "Unknown atom a = $1" a))
    (cmp1l (car a) (cdr a))))

(def cmp1l (e a)
  (if (atm? e)
    (if (sym? e)
      (if (smset? e) (send (cons (xsmcal e) a))
          (mset? e) (send (xmcal e a))
          ;(sset? e) (xspc e a)
          (cprc e @a))
      (call cal e @a))
    (cmp1ll (car e) (cdr e) a)))

(def cmp1ll (e a b)
  (if (atm? e)
    (if (sym? e)
      (if (smset? e) (send (cons (cons (xsmcal e) a) b))
          (mset? e) (send (cons (xmcal e a) b))
          ;(sset? e) (call cal (xspc e a) @b)
          (mmset? e) (send (xmmcal e a b))
          (call cal (cprc e @a) @b))
      (call cal e @a @b))
    (call cal e @a @b)))

(def send (a)
  (cmp1 a))

(mac call (p . a)
  ;(al "res = $1" `(send (lis ',(app 'js- p) ,@a)))
  `(send (lis ',(app 'js- p) ,@a)))

;;; Procedures ;;;

(var *prcs* {})

(var *curropt* {})

(mac opt (nm val)
  `(do (= (. *curropt* ,nm) ,val)
       nil))

(def opsfr1 (ob a)
  `(if (ohas (. ,ob opt) ',a) (opt ,a (. ,ob opt ,a))))

(mac opsfr (ob . a)
  `(do ,@(map [opsfr1 ob _] a)))

(mac cpops (ops . a)
  `(let #r (do ,@a)
     (opsfr #r ,@ops)
     #r))

(mac defprc (nm ag . bd)
  `(= (. *prcs* ,nm)
      (fn ,ag
        (blk ,nm
          (mwith ((chan (a) `(retfr ,,nm (send ,a)))
                  (pass (p . a)
                    `(retfr ,,nm (call ,p ,@a))))
            (dyn *curropt* {}
              (rt ',nm (do ,@bd) *curropt*)))))))

(def cprc (p . a)
  (if (beg p 'js-)
        (let f (*prcs* (sli p 3))
          (if (no f) (err cprc "Unknown p = $1" p)
              (f @a)))
      (call cal p @a)))

;;; Macros ;;;

;(mkoacc spec s)
(mkoacc macs m)
(mkoacc smacs sm)
(mkoacc mmacs mm)

(def xmcal (e a)
  ((mref e) @a))

(def xsmcal (a)
  ((smref a)))

(def xmmcal (e a b)
  ((mmref e) a b))

(mac xmac (nm ag . bd)
  `(mput ',(app 'js- nm) (fn ,ag ,@bd)))

(xmac exe (a)
  (evl (cons 'do a)))

(xmac mac (nm ag . bd)
  (mput nm (evl `(fn ,ag ,@bd)))
  nil)

;;; Places ;;;

; *ps* = places
(var *ps* nil)

(mac stapla (p . a)
  `(sta *ps* ,p ,@a))

(mac wpla (p a)
  `(stapla ,p (place ,a *ps*)))

(mac cpla (p a)
  `(wpla ,p (send ,a)))

(def place (a ps)
  a)

(def cpa (p a)
  (map [cpla p _] a))

(mac defpla (a . opt)
  nil)

(mac defrt (a . opt)
  nil)

;;; JS Places ;;;

(var *blkpla* nil)
(var *blkrts* nil)

(def inblk? (ps)
  (has (car ps) *blkpla*))

(def blk? (a)
  (has (. a tp) *blkrts*))

(var *retpla* nil)
(var *endpla* nil)

(def inret? (ps)
  (if (no ps) nil
      (has (car ps) *retpla*) t
      (has (car ps) *endpla*) (inret? (cdr ps))
      nil))

(def ret? (a)
  (. a opt ret))

(def thr? (a)
  (. a opt thr))

(def brk? (a)
  (. a opt brk))

(def exi? (a)
  (or (ret? a) (thr? a) (brk? a)))

(def bra? (a)
  (. a opt bra))

(def needbra? (a)
  (unless (is (. a typ) 'rt) (err needbra? "a = $1 must be a rt" a))
  (and (ohas (. a opt) 'bra)
       (bra? a)))

(def mkbra (a)
  (lns "{" (ind 2 a) "}"))

(def chkbra (a)
  (if (needbra? a) (mkbra a)
      a))

; a should always be a rt
(def place (a ps)
  (if (inblk? ps)
        (if (no (blk? a))
              (if (no (inret? ps))
                    (do (when (is (. a tp) 'fn)
                          (= a (mapdat [lin "(" _ ")"] a)))
                        (mapdat [lin _ ";"] a))
                  (rt (. a tp) (lin "return " (. a dat) ";")
                      (owith (. a opt) 'ret t)))
            a)
      a))

(mac defpla (a . opt)
  `(with (#a ',a #opt '(,@opt))
     (if (has 'blk #opt) (psh #a *blkpla*))
     (if (has 'ret #opt) (psh #a *retpla*))
     (if (has 'end #opt) (psh #a *endpla*))
     nil))

(mac defrt (a . opt)
  `(with (#a ',a #opt '(,@opt))
     (if (has 'blk #opt) (psh #a *blkrts*))
     nil))

;;; JS Procedures ;;;

(defprc num (a)
  (str a))

(defprc sym (a)
  (jvar a))

(defprc str (a)
  (dsp a))

(def jvar? (a)
  (has #"^[a-zA-Z$_][a-zA-Z0-9$_]*$" a))

(def var? (a)
  (has #"^[a-zA-Z$_][a-zA-Z0-9$_?-]*$" a))

(def jvar (a)
  (if (jvar? a) (str a)
      (var? a)
        (let s ""
          (idx i a
            (case (a i)
              '- (do (app= s (upp (a (+ i 1))))
                      (++ i))
              '? (app= s "p")
              (app= s (a i))))
          s)
      (err jvar "Can't coerce a = $1 to jvar" a)))

(defprc cal (nm . ag)
  (lin (cpla 'refee nm) (mpar ag)))

(def mpar (a)
  (lin "(" (btwa (cpa 'inln a) ", ") ")"))

(defprc do a
  (if (no a) (chan nil)
      (no (cdr a)) (chan (car a))
      (let fst (cpla 'do (car a))
        (if (redun? fst) (pass do @(cdr a))
            (do (opt bra t)
                (lns fst (cdo1 @(cdr a))))))))

(def cdo1 a
  (if (no (cdr a))
        (cpops (ret thr brk)
          (cpla 'dolas (car a)))
      (let fst (cpla 'do (car a))
        (if (redun? fst) (cdo1 @(cdr a))
            (lns fst (cdo1 @(cdr a)))))))

(defpla do blk)
(defpla dolas blk end)
(defrt do blk)

(def redun? (a)
  ;(al "a = $1" a)
  ;(al "orig = $1" (. a opt orig))
  (and (is (. a tp) 'sym) (is (. a opt orig) "nil")))

(defprc whi (ts . bd)
  (opt bra t)
  (lin "while (" (cpla 'bot ts) ")"
       (if (no bd) ";"
           (chkbra (wpla 'lop (call do @bd))))))

(defpla lop blk)
(defrt whi blk)

(defprc loop (st p up . bd)
  (opt bra t)
  (lin "for (" (cpla 'forbeg st) "; "
               (cpla 'bot p) "; "
               (cpla 'bot up) ")"
       (if (no bd) ";"
           (chkbra (wpla 'lop (call do @bd))))))

(defrt loop blk)

(defprc if a
  (if (no a) (chan nil)
      (no (cdr a)) (chan (car a))
      (do (opt bra t)
          (cif1 a))))

(def cif1 (a)
  (if (no a) (cpla 'if nil)
      (no (cdr a)) (cpla 'if (car a))
      (with (ts (cpla 'bot (car a))
             yes (cpla 'if (cadr a)))
        (opsfr yes ret thr brk)
        (if (exi? yes)
              (lns (lin "if (" ts ")" (chkbra yes))
                   (cif1 (cddr a)))
            (needbra? yes)
              (lin "if (" ts ")" (chkbra yes) " " (celif (cddr a)))
            (lns (lin "if (" ts ")" (chkbra yes))
                 (celif (cddr a)))))))

(def celif (a)
  (if (no a) nil
      (no (cdr a)) (lin "else " (chkbra (cpla 'if (car a))))
      (with (ts (cpla 'bot (car a))
             yes (cpla 'if (cadr a)))
        (if (needbra? yes)
              (lin "else if (" ts ")" (chkbra yes) " " (celif (cddr a)))
            (lns (lin "else if (" ts ")" (chkbra yes))
                 (celif (cddr a)))))))

(defpla if blk end)
(defrt if blk)

(defprc ret (a)
  (cpops (ret thr brk bra) 
    (cpla 'ret a)))

(defpla ret blk ret)
(defrt ret blk)

(defprc nret (a)
  (cpops (ret thr brk bra)
    (cpla 'nret a)))

(defpla nret blk)
(defrt nret blk)

(defprc fn (ag . bd)
  (lns (lin "function " (mpar ag) "{")
       (ind 2 (wpla 'blk (call do @bd)))
       "}"))

#|(defprc blk a
  (opt bra t)
  (if (no a) "{}"
      (lns "{" (ind 2 (inpla 'blk (cdo a))) "}")))|#

(defpla blk blk ret)
(defrt fn)

(defprc def (nm ag . bd)
  (opt bra t)
  (lns (lin "function " (call sym nm) (mpar ag) "{")
       (ind 2 (wpla 'blk (call do @bd)))
       "}"))

(defrt def blk)

;;; Lines ;;;

(def mklnobj (typ ob)
  (app ob {typ typ}))

(def lin a
  (mklnobj 'lin {dat a}))

(def lns a
  (mklnobj 'lns {dat a}))

(def lvl a
  (mklnobj 'lvl {dat a}))

(def lvlind (n . a)
  (mklnobj 'lvlind {dat a n n}))

(def ind (n . a)
  (mklnobj 'ind {dat a n n}))

(def wind (n . a)
  (mklnobj 'wind {dat a n n}))

(def rt (tp a (o opt {}))
  (mklnobj 'rt {dat a tp tp opt (app {orig a} opt)}))

(def mapdat (f a)
  (mklnobj (. a typ) (app a {dat (f (. a dat))})))

(over dsp (a)
  (sup (trans a)))

(def trans (a)
  (case a
    obj? (case (. a typ)
           'lin `(lin ,@(trans (. a dat)))
           'lns `(lns ,@(trans (. a dat)))
           'ind `(ind ,(. a n) ,@(trans (. a dat)))
           'rt  `(rt ,(. a tp) ,(trans (. a dat))))
    lis? (map trans a)
    a))

;;; Output lines ;;;

(var *indlvl* 0)
(var *begline* t)
(var *linepos* 0)

(def emit (a)
  ;(al "a = $1 | *indlvl* = $2 | *begline* = $3 | *linepos* = $4" a *indlvl* *begline* *linepos*)
  (when *begline*
    (pr (nof *indlvl* " "))
    (+= *linepos* *indlvl*))
  (pr a)
  (+= *linepos* (len a))
  (= *begline* nil))

(def newln ()
  (pr "\n")
  (= *linepos* 0)
  (= *begline* t))

(def freshln ()
  (unless *begline* (newln)))

(def resetln ()
  (= *indlvl* 0)
  (= *linepos* 0)
  (= *begline* t)
  nil)

;;; Process lines ;;;

(def proc (a)
  (resetln)
  (tostr (proclin (lin a))))

(def proclin (a)
  (each x (rflat (. a dat))
    (unless (no x) (proclin1 x))))

(def proclin1 (a)
  (case a
    obj? (case (. a typ)
           'lin (proclin a)
           'lns (proclns a)
           'lvl (dyn *indlvl* *linepos*
                  (proclns a))
           'ind (procind a)
           'lvlind (dyn *indlvl* *linepos*
                     (procind a))
           'wind (procwind a)
           'rt (proclin1 (. a dat))
           (err proclin1 "Unknown type a = $1" a))
    syn? (emit (str a))
    str? (emit a)
    (err proclin1 "Unknown type a = $1" a)))

(def proclns (a)
  (proclnslis (rflat (. a dat))))

(def proclnslis (a)
  (if (no a) nil
      (no (car a)) (proclnslis (cdr a))
      (do (proclns1 (car a))
          (proclnslis2 (cdr a)))))
          
(def proclnslis2 (a)
  (if (no a) nil
      (no (car a)) (proclnslis2 (cdr a))
      (do (newln)
          (proclns1 (car a))
          (proclnslis2 (cdr a)))))

(def proclns1 (a)
  (case a
    obj? (case (. a typ)
           'lin (proclin a)
           'lns (proclns a)
           'lvl (proclns a)
           'ind (procind a)
           'lvlind (procind a)
           'wind (procwind a)
           'rt (proclns1 (. a dat))
           (err proclns1 "Unknown type a = $1" a))
    syn? (emit (str a))
    str? (emit a)
    (err proclns1 "Unknown type a = $1" a)))

(def procind (a)
  (dyn *indlvl* (+ *indlvl* (. a n))
    (proclns (lns (. a dat)))))

(def procwind (a)
  (dyn *indlvl* (. a n)
    (proclns (lns (. a dat)))))

(def rflat (a)
  (if (no a) nil
      (atm? (car a)) (cons (car a) (rflat (cdr a)))
      (app (rflat (car a)) (rflat (cdr a)))))

;;; Compile from str ;;;

(def cmps (a)
  (cmp (prs a)))

(def cmpp (a)
  (prn (cmps a)))
