;;;; Compiler ;;;;

#|
Examples:

(prn (compile "(1 2 3 4 5)"))

|#

(import Format lin lns ind flns lvl note proc)

;;; Main compile ;;;

(def compile (a)
  (proc (comp (prs a))))

; numbers are sent to the call js procedure
; symbols are sent to sym
; strings are sent to str
; nil is sent to sym as 'nil
; the call procedure is used for calls that aren't
;   defined as procedures or macros

(var *atom-compile-table* {})

(mac set-atom-compile-fn (type f)
  `(= (*atom-compile-table* ',type) ,f))

(mac no-nil (err-expr v object . rst)
  `(let #rand ,object
     (if (nil? #rand) ,err-expr
         (let ,v #rand
           ,@rst))))

; (comp a) -> Format object (lin, lns, etc)
; it should actually be a "placed Return object" (ie. (place (ret-obj 'if (lin "a" "b"))) )
(def comp (a)
  (if (atm? a)
    (if (and (sym? a) (symbol-macro-set? a))
          (comp (symbol-macro-call a))
        (no-nil (err comp "Unknown atom a = $1" a)
          f (*atom-compile-table* (typ a))
          (f a)))
    (comp-list (car a) (cdr a))))

(def comp-list (e a)
  (if (atm? e)
    (if (sym? e)
      (if (symbol-macro-set? e) (comp (cons (symbol-macro-call e) a))
          (macro-set? e) (comp (macro-call e a))
          (comp-apply-fn e a))
      (comp-apply-atom e a))
    (comp-list-of-list (car e) (cdr e) a)))

(def comp-list-of-list (e a b)
  (if (and (atm? e) (sym? e) (macro-macro-set? e))
        (comp (macro-macro-call e a b))
      (comp-apply-unknown (comp-list e a) b)))

(mac defstub (v cmd . pairs)
  (let genfn (defstub-mk-gen-def v cmd)
    `(do ,@(map genfn (grp pairs 2)))))

(def defstub-mk-gen-def (v cmd)
  (fn ((nm ag))
    `(def ,nm ,ag
       (let ,v (stf ,(str nm " | " (join (mapi (str _ " = $" i) ag 1) " | ")) ,@ag)
         ,cmd))))

(defstub s (comp s)
  comp-apply-unknown (e a)
  comp-apply-atom (e a)
  comp-apply-fn (e a))

(var *function-compile-table* {})

(mac set-function-compile-fn (type f)
  `(= (*function-compile-table* ',type) ,f))

(def comp-apply-fn (nm args)
  (no-nil (err comp-apply-fn "Unknown function nm = $1" nm)
    f (*function-compile-table* nm)
    (f @args)))

;;; Places ;;;

(var *places* nil)

(def curr-place () (car *places*))

(def comp-in-place (p a)
  (sta *places* p
    (comp a)))

(def place (ret-type format-obj)
  format-obj)

;;; Macros ;;;

(with-object-accessors spec spec-)
(with-object-accessors macs macro-)
(with-object-accessors smacs symbol-macro-)
(with-object-accessors mmacs macro-macro-)

; mmacs can be used to optimize exprs like
;   ((dtfn test) a x y z)
;   to ((. a test) x y z)
;   or ((combine f g) a b c)
;   to (f (g a b c))

(def spec-call (e a)
  ((spec-ref e) @a))

(def macro-call (e a)
  ((macro-ref e) @a))

(def symbol-macro-call (a)
  ((symbol-macro-ref a)))

(def macro-macro-call (e a b)
  ((macro-macro-ref e) a b))

(mac cmpr-mac (nm ag . bd)
  `(macro-put ',nm (fn ,ag ,@bd)))

;;; JS Specific ;;;

(var *block-places* nil)

(def block-place? ()
  (or (no (curr-place))
      (has (curr-place) *block-places*)))

(def add-block-place (a)
  (push a *block-places*))

(var *return-places* nil)
(var *end-places* nil)

(def return-place? ((o places *places*))
  (if (no places) nil
      (has (car places) *return-places*) t
      (has (car places) *end-places*) (return-place? (cdr places))
      nil))

(var *block-types* nil)

(def block-type? (a)
  (has a *block-types*))

(def add-block-type (a)
  (push a *block-types*))

; *require-bracket-pairs* is a list of lists
; each list is of the form (ret-type place) which specifies that
;   when an obj of return type ret-type is placed in place, it requires brackets
(var *require-bracket-pairs* nil)

(def add-require-bracket-pair (pair)
  (push pair *require-bracket-pairs*))

(def requires-brackets? (ret-type place)
  ;(bug ret-type place *require-bracket-pairs*)
  (has [iso _ (lis ret-type place)] *require-bracket-pairs*))

(def redundant? (a)
  ;(bug a)
  ;(al "orig = $1" (. a opt orig))
  (and (is (. a tp) 'sym) (is (. a opt orig) "nil")))

(def needs-brackets-when-placing? (ret-type place)
  nil)

(def with-braces (a)
  (lns "{" (ind 2 a) "}"))

(def place (ret-type format-obj)
  (if (block-place?)
        (if (block-type? ret-type) format-obj
            (return-place?) (lin "return " format-obj ";")
            (lin format-obj ";"))
      (block-type? ret-type)
        (err place "Can't place obj $1 of block type $2 inside inline place $3"
                   format-obj ret-type (curr-place))
      (requires-brackets? ret-type (curr-place))
        (lin "(" format-obj ")")
      format-obj))

; is a already a js variable
(def js-var? (a)
  (has #"^[a-zA-Z$_][a-zA-Z0-9$_]*$" a))

; is a suitable for conversion?
(def convertible-var? (a)
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
(def js-var (a)
  (if (js-var? a) (str a)
      (convertible-var? a)
        (if (and (>= (len a) 2) (is (a 0) '*) (is (las a) '*))
              (upp (js-var (sli a 1 (- (len a) 1))))
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
              s))
      (err js-var "Can't coerce a = $1 to js var" a)))

;;; Compiling Functions ;;;

(set-atom-compile-fn nil [comp `(arr)])
(set-atom-compile-fn num [place 'atom (str _)])
(set-atom-compile-fn str [place 'atom (dsp _)])
(set-atom-compile-fn sym [place 'atom (js-var _)])

(def comp-qt (a)
  (casetyp a
    nil (comp nil)
    num (comp a)
    sym (comp (str a))
    str (comp a)
    lis (comp `(lis ,@(map aqt a)))
    (err comp-qt "Unknown obj a = $1" a)))

(set-function-compile-fn qt comp-qt)

(def comp-lis a
  (if (no a) (comp nil)
      (comp `(arr ,(car a) (lis ,@(cdr a))))))

(set-function-compile-fn lis comp-lis)

(def comp-arr a
  (place 'arr-obj (lin "[" @(btwa (map [comp-in-place 'in-line _] a) ", ") "]")))

(set-function-compile-fn arr comp-arr)

(def comp-do a
  (if (no a) (comp nil)
      (no (cdr a)) (comp (car a))
      (no (block-place?)) (comp-doln @a)
      (comp-do-always @a)))

(def comp-do-always a
  (if (no (cdr a)) (comp-in-place 'do-last (car a))
      (place 'do-obj (lns (comp-in-place 'do (car a)) (comp-do-always @(cdr a))))))

(def comp-doln a
  (if (no a) (comp nil)
      (no (cdr a)) (comp (car a))
      (place 'do-line-obj (lin (comp-in-place 'do-line (car a)) ", " (comp-doln @(cdr a))))))

(add-block-place 'do)
(add-block-place 'do-last)
(add-block-type 'do-obj)

(add-require-bracket-pair '(do-line-obj in-line))

(set-function-compile-fn do comp-do)

(def comp-while (ts . bd)
  (place 'loop-obj
    (lin "while (" (comp-in-place 'in-paren ts) ")"
      (if (no bd) ";"
          (with-braces (comp-in-place 'loop-body `(do ,@bd)))))))

(add-block-place 'loop-body)
(add-block-type 'loop-obj)

(set-function-compile-fn while comp-while)

(def comp-loop (start pred update . bd)
  (place 'loop-obj
    (lin "for (" (comp-in-place 'loop-beg start) "; "
                 (comp-in-place 'in-paren pred) "; "
                 (comp-in-place 'in-paren update) ")"
         (if (no bd) ";"
             (with-braces (comp-in-place 'loop-body `(do ,@bd)))))))

(set-function-compile-fn loop comp-loop)