;;;; Compiler ;;;;

;;; Main compile ;;;

(def cmp1 (a)
  (if (atm? a)
    (case a
      nil? (cmp1 'nil)
      num? (rt 'atm (str a))
      sym? (rt 'atm (jvar a))
      str? (rt 'atm (dsp a))
      (err cmp1 "Unknown atom a = $1" a))
    (let b (car a)
      (if (atm? b)
            (if (sym? b) (cprc b (cdr a))
                (cmp1 (cons 'js-cal a)))
          (cmp1 (cons 'js-cal a))))))

(def cprc (p a)
  (if (beg p 'js-)
        (case (sli p 3)
          'cal (ccal a)
          'blk (cblk a)
          'blk2 (cblk2 a)
          'whi (cwhi a)
          (err cprc "Unknown p = $1" p))
      (cmp1 (lisd 'js-cal p a))))

;;; Places ;;;

; *ps* = places
(var *ps* nil)
(def cmppla (a p)
  (sta *ps* p
    (place (cmp a) *ps*)))

(def place (a ps)
  a)

;;; Types ;;;

(def lns a
  (tg 'lns {a a}))

(def lin a
  (tg 'lin {a a}))

(def ind (n . a)
  (tg 'ind {n n a a}))

(def wind (n . a)
  (tg 'wind {n n a a}))

(def rt (tp a)
  (tg 'rt {tp tp a a}))

(def geta (a)
  (. (rp a) a))

(def getn (a)
  (. (rp a) n))

(def gettp (a)
  (. (rp a) tp))

(def isa (a x)
  (is (typ a) x))

(def lns? (a)
  (isa a 'lns))

(def lin? (a)
  (isa a 'lin))

(def ind? (a)
  (isa a 'ind))

(def wind? (a)
  (isa a 'wind))

(def rt? (a)
  (isa a 'rt))

;;; Convert to macro ;;;

#|(def cnv (a)
  (case a
    lin? `(line ,@(cnvl (geta a)))
    lns? `(lines ,@(cnvl (geta a)))
    ind? `(+ind ,(getn a) ,@(cnvl (geta a)))
    syn? (str a)
    str? a
    (err cnv "Unknown a = $1" a)))

; convert lis
(def cnvl (a)
  (map cnv (flata a)))|#

;;; Compile ;;;

(def cmp (a)
  (evll (cmp1 a)))

(var *indlvl* 0)
(var *begline* t)
(var *linepos* 0)

(def emit (a)
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
  (= *begline* t))

#|(def indent (a)
  (str (nof *indlvl* " ") a))|#

(def proc (a)
  (resetln)
  (proclin1 a)
  nil)
    
(def proclin (a)
  (map proclin1 (flata (geta a))))

(def proclin1 (a)
  (case a
    lin? (proclin a)
    lns? (dyn *indlvl* *linepos*
           (proclns a))
    ind? (dyn *indlvl* *linepos*
           (procind a))
    wind? (procwind a)
    syn? (emit (str a))
    str? (emit a)
    (err proclin1 "proclin1 a = $1 | tp = $2" a (typ a))))

(def proclns (a)
  (let a (flata (geta a))
    (proclns1 (car a))
    (each x (cdr a)
      (newln)
      (proclns1 x))))

(def proclns1 (a)
  (case a
    lin? (proclin a)
    lns? (proclns a)
    ind? (procind a)
    wind? (procwind a)
    syn? (emit (str a))
    str? (emit a)
    (err proclns1 "proclns1 a = $1 | tp = $2" a (typ a))))

(def procind (a)
  (dyn *indlvl* (+ *indlvl* (getn a))
    (proclns (lns (geta a)))))

(def procwind (a)
  (dyn *indlvl* (getn a)
    (proclns (lns (geta a)))))
  
#|(def proc (a)
  (case a
    lin? (proclin a)
    lns? (joi (map dolns (flata (geta a))))
    ind? (+ind (getn a) (dolns (lns (geta a))))
    rt?  (doln (geta a))
    syn? (str a)
    str? a
    (err dolns "Unknown a = $1" a)))
  
(def mklin (a (o s ""))
  (if (no a) s
      (mklin (cdr a) (mklin1 (car a) s))))

(def mklin1 (a s)
  (case a
    lin? (mklin (flata (geta a)) s)
    lns? (mklvllns (flata (geta a)) s)
    ind? (err mklin1 "Can't indent inside line")
    rt?  (mklin1 (geta a) s)
    syn? (mklin1 (str a) s)
    str? (app s a)
    (err mklin1 "Unknown a = $1" a)))

(def mklvllns (a (o s ""))
  (if (no a) s
      (case (car a)
        lin? (mklin (mklin1 a s)
        lns? (mklvllns (flata (geta a)) s)
        

(def dolns (a)
  (case a
    lns? (joi (map dolns (flata (geta a))))
    ind? (+ind (getn a) (dolns (lns (geta a))))
    rt?  (dolns (geta a))
    lin? (indent (app (doln a) "\n"))
    syn? (indent (app (doln a) "\n"))
    str? (indent (app (doln a) "\n"))
    (err dolns2 "Unknown a = $1" a)))|#

#|(def evllns2 (a)
  (case a
    lns? (evllns a)
    ind? (evlind a)
    syn? (indent (app a "\n"))
    str? (indent (app a "\n"))
    (err evllns2 "Unknown a = $1" a)))|#

(def flata (a)
  (if (no a) nil
      (atm? (car a)) (cons (car a) (flata (cdr a)))
      (app (flata (car a)) (flata (cdr a)))))

#|(def lines a
  (joi (map [str (indent _) "\n"] a)))|#

;;; Compile from str ;;;

(def cmps (a)
  (cmpln (prs a)))

(def cmpp (a)
  (pr (cmps a)))
