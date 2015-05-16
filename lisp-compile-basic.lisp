;;;; Compiler ;;;;

#|
Examples:

(prn (compproc '(js-loop (set x 3) (lt x 5) (pp x) (js-def hey (a b c) (js-if test 1 2 3 4 5)) 3 4 5)))

|#

(import Format lin lns ind flns lvl note proc)

;;; Main compile ;;;

(def compproc (a)
  (proc (comp a)))

; numbers are sent to the call js procedure
; symbols are sent to sym
; strings are sent to str
; nil is sent to sym as 'nil
; the call procedure is used for calls that aren't
;   defined as procedures or macros

(def comp (a)
  (if (atm? a)
    (casetyp a
      nil (comp 'nil)
      num (call num a)
      sym (if (smset? a) (comp (xsmcall a))
              (call sym a))
      str (call str a)
      (err comp "Unknown atom a = $1" a))
    (compl (car a) (cdr a))))

(def compl (e a)
  (if (atm? e)
    (if (sym? e)
      (if (smset? e) (comp (cons (xsmcall e) a))
          (mset? e) (comp (xmcall e a))
          (sset? e) (xscall e a)
          (cprc e @a))
      (call call e @a))
    (compll (car e) (cdr e) a)))

(def compll (e a b)
  (if (atm? e)
    (if (sym? e)
      (if (mmset? e) (comp (xmmcall e a b))
          (call call (cons e a) @b))
      (call call (cons e a) @b))
    (call call (cons e a) @b)))

#|
(comp a) -> compile a
(call p @a) -> call jsprc p with args a
(chan a) -> cancel current compilation and (comp a) instead
(pass p @a) -> cancel current compilation and (call p @a) instead
|#

(mac mkcall (nm . a)
  `(lis ',(app 'js- nm) ,@a))

(mac call (nm . a)
  ;(al "res = $1" `(comp (lis ',(app 'js- p) ,@a)))
  `(comp (mkcall ,nm ,@a)))

;;; Procedures ;;;

(var *prcs* {})
(var *curropts* {})

; set option nm to val
(mac setopt (nm val)
  `(do (= (. *curropts* ,nm) ,val)
       nil))

(def useoptsfr1 (ob a)
  `(if (ohas (. ,ob opt) ',a) (setopt ,a (. ,ob opt ,a))))

; use options in a from rt obj ob
(mac useoptsfr (ob . a)
  `(do ,@(map [useoptsfr1 ob _] a)))

; copy options in ops from rt obj in (do @a) and return rt obj
(mac useopts (ops . a)
  `(let #r (do ,@a)
     (useoptsfr #r ,@ops)
     #r))

(deferr chan (a) "Outside of procedure.")
(deferr pass (p . a) "Outside of procedure.")

; define procedure
(mac defprc (nm ag . bd)
  `(= (. *prcs* ,nm)
      (fn ,ag
        (dynblock ,nm
          (dynmwith ((chan (a) `(retfrom ,,nm (comp ,a)))
                     (pass (p . a)
                       `(retfrom ,,nm (call ,p ,@a))))
            (dyn *curropts* {}
              (rt ',nm (do ,@bd) *curropts*)))))))

(def rt (tp a (o opt {}))
  (note a {tp tp opt (app {orig a} opt)}))

(def cprc (nm . a)
  (if (beg nm 'js-)
        (let f (*prcs* (sli nm 3))
          (if (no f) (err cprc "Unknown nm = $1" nm)
              (f @a)))
      (call call nm @a)))

;;; Macros ;;;

(mkoacc spec s)
(mkoacc macs m)
(mkoacc smacs sm)
(mkoacc mmacs mm)

; mmacs can be used to optimize exprs like
;   ((dtfn test) a x y z)
;   to ((. a test) x y z)
;   or ((combine f g) a b c)
;   to (f (g a b c))

(def xscall (e a)
  ((sref e) @a))

(def xmcall (e a)
  ((mref e) @a))

(def xsmcall (a)
  ((smref a)))

(def xmmcall (e a b)
  ((mmref e) a b))

; (xmac name ...) should be the same thing as running
;   (js-mac js-name ...) in the compiler
(mac xmac (nm ag . bd)
  `(mput ',(app 'js- nm) (fn ,ag ,@bd)))

(xmac exe a
  (geval `(do ,@a)))

(xmac mac (nm ag . bd)
  (mput nm (eval `(fn ,ag ,@bd)))
  nil)

(xmac dmac (nm)
  (mdel nm)
  nil)

(xmac rmac (fr to)
  (mren fr to)
  nil)

(xmac smac (nm . bd)
  (smput nm (eval `(fn () ,@bd)))
  nil)

(xmac dsmac (nm)
  (smdel nm)
  nil)

(xmac rsmac (fr to)
  (smren fr to)
  nil)

(xmac mmac (nm ag1 ag2 . bd)
  (mmput nm (eval `(fn (,ag1 ,ag2) ,@bd)))
  nil)

(xmac dsmac (nm)
  (mmdel nm)
  nil)

(xmac rsmac (fr to)
  (mmren fr to)
  nil)

(mac xsmac (nm . bd)
  `(smput ',(app 'js- nm) (fn () ,@bd)))

(xsmac nil
  `(arr))

; special procedures are not compiled again after they are run
; this means the return value should already be compiled
(mac xspec (nm ag . bd)
  `(sput ',(app 'js- nm) (fn ,ag ,@bd)))

(xspec cdo1 (a . bd)
  (let r (comp a)
    (comp `(do ,@bd))
    r))

(xspec mblock a
  (mlay)
  (let r (comp `(do ,@a))
    (mulay)
    r))

(xspec smblock a
  (smlay)
  (let r (comp `(do ,@a))
    (smulay)
    r))

(xspec mmblock a
  (mmlay)
  (let r (comp `(do ,@a))
    (mmulay)
    r))

(mac xmmac (nm ag1 ag2 . bd)
  `(mmput ',(app 'js- nm) (fn (,ag1 ,ag2) ,@bd)))

; ((dtfn a b c) x 1 2 3)
; -> ((. x a b c) 1 2 3)
(xmmac dtfn a (x . args)
  `((. ,x ,@a) ,@args))

; ((combine a b c) 1 2 3)
; -> (a (b (c 1 2 3)))
(xmmac combine fs args
  (foldr lis `(,(las fs) ,@args) (but fs)))

;;; Places ;;;

; *ps* = places
(var *ps* nil)

(mac stapla (p . a)
  `(sta *ps* ,p ,@a))

(mac w/pla (p a)
  `(stapla ,p (place ,a)))

(mac comppla (p a)
  `(w/pla ,p (comp ,a)))

(mac callpla (p nm . a)
  `(w/pla ,p (call ,nm ,@a)))

(def currpla () (car *ps*))

(def getps () *ps*)

; does any operations (such as adding parens, order of operations)
;   needed to put a into the env defined by *ps*;
;   should be overridden for the target language
(def place (a) a)

; compile all in list a in place p
(def compall (p a) (map [comppla p _] a))

; define place; should be overridden to define options that
;   will be used by place
(mac defpla (a . opt) nil)

; define rt (return object)
(mac defrt (a . opt) nil)

;;; JS Places ;;;

; block places include the top level, inside a function, a while, an if...
; a block rt is anything that needs to be placed inside a block

(var *blkpla* nil)
(var *blkrts* nil)

(def inblk? ()
  (or (no (currpla))
      (has (currpla) *blkpla*)))

; an place or rt is considered inline unless it is defined as blk
(def blk? (a) (has (. a tp) *blkrts*))

; return places include the end of a function and inside
;   a ret expression
; an end place is a place that carries over returns, but isn't a
;   ret place itself

(var *retpla* nil)
(var *endpla* nil)

(def inret? ((o ps (getps)))
  (if (no ps) nil
      (has (car ps) *retpla*) t
      (has (car ps) *endpla*) (inret? (cdr ps))
      nil))

; the ret rt option signals whether the code inside always returns
; the thr option signals whether the code always throws
; the brk option signals whether the code always breaks
; exi? says whether the code always exits, whether by ret, thr, or brk

(def ret? (a) (. a opt ret))
(def thr? (a) (. a opt thr))
(def brk? (a) (. a opt brk))

(def exi? (a)
  (or (ret? a) (thr? a) (brk? a)))

; the bra option says whether the code needs braces
(def bra? (a)
  (. a opt bra))

(def needbra? (a)
  ;(bug a (. a opt) (ohas (. a opt) 'bra) (bra? a))
  (ifnot (is (typ a) 'note) (err needbra? "a = $1 must be a rt" a))
  (and (ohas (. a opt) 'bra)
       (bra? a)))

(def mkbra (a)
  (lns "{" (ind 2 a) "}"))

; add bracket if a needs it
(def chkbra (a)
  (if (needbra? a) (mkbra a)
      a))

; *reqbrac* is a list of lists
; each list is of the form (rt pl) which specifies that
;   when a rt obj of type rt is placed in a place pl, it requires brackets
(var *reqbrac* nil)

; ex. (defbra doln inln)
(mac defbra (rt pl)
  `(push '(,rt ,pl) *reqbrac*))

; ex. (reqbrac? 'doln 'inln)
(def reqbrac? (rt pl)
  (has [iso _ (lis rt pl)] *reqbrac*))

; a should always be a rt
(def place (a)
  ;(bugm "place" a (inblk?) (blk? a) (inret?))
  (if (inblk?)
        (if (blk? a) a
            (no (inret?))
              (do (when (is (. a tp) 'fn)
                    (= a (calldat [lin "(" _ ")"] a)))
                  (calldat [lin _ ";"] a))
            (rt (. a tp) (lin "return " (dat a) ";")
                (owith (. a opt) 'ret t)))
      (blk? a) (err place "Can't place blk $1 inside inline place $2"
                          a (currpla))
      (reqbrac? (. a tp) (currpla))
        (calldat [lin "(" _ ")"] a)
      a))

; define whether your place is a blk or a ret or end
(mac defpla (a . opt)
  `(with (#a ',a #opt '(,@opt))
     (if (has 'blk #opt) (push #a *blkpla*))
     (if (has 'ret #opt) (push #a *retpla*))
     (if (has 'end #opt) (push #a *endpla*))
     nil))

; define whether your rt is a blk
(mac defrt (a . opt)
  `(with (#a ',a #opt '(,@opt))
     (if (has 'blk #opt) (push #a *blkrts*))
     nil))

;;; JS Procedures ;;;

(defprc num (a)
  (str a))

(defprc sym (a)
  (jvar a))

(defprc str (a)
  (dsp a))

; is a already a js variable
(def jvar? (a)
  (has #"^[a-zA-Z$_][a-zA-Z0-9$_]*$" a))

; is a suitable for conversion?
(def var? (a)
  (has #"^\*?[a-zA-Z$_*/+-^=!][a-zA-Z0-9$_*/+-^=!?-]*\*?$" a))

; convert lisp sym to js variable
; todo: *var* -> VAR
; * -> mul
; / -> div
; + -> add
; - -> sub
; ^ -> pow
; ! -> bang
; ? -> p
; a-test -> aTest
(def jvar (a)
  (if (jvar? a) (str a)
      (var? a)
        (let s ""
          (index i a
            (case (a i)
              '- (if (is i 0) (app= s "sub")
                   (do (app= s (upp (a (+ i 1))))
                       (++ i)))
              '* (app= s "mul")
              '/ (app= s "div")
              '+ (app= s "add")
              '^ (app= s "pow")
              '! (app= s "bang")
              '? (app= s "p")
              (app= s (a i))))
          s)
      (err jvar "Can't coerce a = $1 to jvar" a)))

(defprc call (nm . ag)
  (lin (comppla 'refee nm) (mpar ag)))

(def mpar (a)
  (lin "(" @(btwa (compall 'inln a) ", ") ")"))

(defprc arr a
  (lin "[" @(btwa (compall 'inln a) ", ") "]"))

(defprc qt (a)
  ;(bug a)
  (casetyp a
    nil (chan nil)
    num (pass num a)
    sym (pass str (str a))
    str (pass str a)
    lis (cqtlis a)
    (err qt "Unknown obj a = $1" a)))

(def cqtlis (a)
  (if (no a) (chan nil)
      (pass arr (mkcall qt (car a)) (mkcall qt (cdr a)))))

#|(def clis (a)
  (if (no a) (chan nil)
      (lin "[" (callpla 'inln qt (car a)) ", " (callpla 'inln qt (cdr a)) "]")))|#

(defprc do a
  (if (no a) (chan nil)
      (no (cdr a)) (chan (car a))
      (inblk?) (cdo @a)
      (pass doln @a)))

(def cdo a
  (let fst (comppla 'do (car a))
    (if (redun? fst) (pass do @(cdr a))
        (do (setopt bra t)
            (lns fst (cdo2 @(cdr a)))))))

; same as cdo but don't pass to do and process last one differently
(def cdo2 a
  (if (no (cdr a))
        (useopts (ret thr brk)
          (comppla 'dolas (car a)))
      (let fst (comppla 'do (car a))
        (if (redun? fst) (cdo2 @(cdr a))
            (lns fst (cdo2 @(cdr a)))))))

(defpla do blk)
(defpla dolas blk end)
(defrt do blk)

(defprc doln a
  (if (no a) (chan nil)
      (no (cdr a)) (chan (car a))
      (cdoln @a)))

(def cdoln a
  (let fst (comppla 'doln (car a))
    (if (redun? fst) (pass do @(cdr a))
        (lin fst ", " (cdoln2 @(cdr a))))))

; same as cdoln but don't pass to do
(def cdoln2 a
  (let fst (comppla 'doln (car a))
    (if (no (cdr a)) fst
        (redun? fst) (cdoln2 @(cdr a))
        (lin fst ", " (cdoln2 @(cdr a))))))

; putting a doln rt into an inln pla requires brackets
(defbra doln inln)

(def redun? (a)
  (bug a)
  ;(al "orig = $1" (. a opt orig))
  (and (is (. a tp) 'sym) (is (. a opt orig) "nil")))

(defprc whi (ts . bd)
  (setopt bra t)
  (lin "while (" (comppla 'bot ts) ")"
       (if (no bd) ";"
           (chkbra (callpla 'lop do @bd)))))

(defpla lop blk)
(defrt whi blk)

(defprc loop (st p up . bd)
  (setopt bra t)
  (lin "for (" (comppla 'forbeg st) "; "
               (comppla 'bot p) "; "
               (comppla 'bot up) ")"
       (if (no bd) ";"
           (chkbra (callpla 'lop do @bd)))))

(defrt loop blk)

(defprc if a
  (if (no a) (chan nil)
      (no (cdr a)) (chan (car a))
      (inblk?) (do (setopt bra t)
                   (cif1 @a))
      (pass ifln @a)))

(def cif1 a
  (if (no a) (comppla 'if nil)
      (no (cdr a)) (comppla 'if (car a))
      (with (ts (comppla 'bot (car a))
             yes (comppla 'if (cadr a)))
        (useoptsfr yes ret thr brk)
        (if (exi? yes)
              (lns (lin "if (" ts ")" (chkbra yes))
                   (cif1 @(cddr a)))
            (needbra? yes)
              (lin "if (" ts ")" (chkbra yes) " " (celif @(cddr a)))
            (lns (lin "if (" ts ")" (chkbra yes))
                 (celif @(cddr a)))))))

(def celif a
  (if (no a) nil
      (no (cdr a)) (lin "else " (chkbra (comppla 'if (car a))))
      (with (ts (comppla 'bot (car a))
             yes (comppla 'if (cadr a)))
        (if (needbra? yes)
              (lin "else if (" ts ")" (chkbra yes) " " @(celif (cddr a)))
            (lns (lin "else if (" ts ")" (chkbra yes))
                 (celif @(cddr a)))))))

(defpla if blk end)
(defrt if blk)

(defprc ifln a
  (if (no a) (chan nil)
      (no (cdr a)) (chan (car a))
      (cifln2 @a)))

(def cifln2 (ts yes nop . rst)
  (lvl (lin (comppla 'iflntest ts) "?"
            (comppla 'iflnyes yes) ":"
            (if (no rst) (comppla 'iflnno nop)))
       (if rst (cifln2 @rst))))

(defprc ret (a)
  (useopts (ret thr brk bra) 
    (comppla 'ret a)))

(defpla ret blk ret)
(defrt ret blk)

(defprc nret (a)
  (useopts (ret thr brk bra)
    (comppla 'nret a)))

(defpla nret blk)
(defrt nret blk)

(defprc fn (ag . bd)
  (lns (lin "function " (mpar ag) "{")
       (ind 2 (callpla 'blk do @bd))
       "}"))

#|(defprc blk a
  (setopt bra t)
  (if (no a) "{}"
      (lns "{" (ind 2 (inpla 'blk (cdo a))) "}")))|#

(defpla blk blk ret)
(defrt fn)

(defprc def (nm ag . bd)
  (setopt bra t)
  (lns (lin "function " (callpla 'fnnm sym nm) (mpar ag) "{")
       (ind 2 (callpla 'blk do @bd))
       "}"))

(defrt def blk)


;;; Compile from str ;;;

(def compprocstr (a)
  (compproc (prs a)))

(def compprocprn (a)
  (prn (compprocstr a)))


#|(over dsp (a)
  (sup (trans a)))

(def trans (a)
  (case a
    obj? (case (typ a)
           'lin `(lin ,@(trans (dat a)))
           'lns `(lns ,@(trans (dat a)))
           'ind `(ind ,(. a n) ,@(trans (dat a)))
           'note  `(rt ,(. a tp) ,(trans (dat a))))
    lis? (map trans a)
    a))|#
