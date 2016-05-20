;;;; Compiler ;;;;

#|
Examples:

(prn (compile "(1 2 3 4 5)"))

|#

(import Format lin lns ind flns lvl note proc)

;;; Main compile ;;;

(def compile (a)
  (proc (bug (pip 'root (comp (prs a))))))

; numbers are sent to the call js procedure
; symbols are sent to sym
; strings are sent to str
; nil is sent to sym as 'nil
; the call procedure is used for calls that aren't
;   defined as procedures or macros

(var *atom-compile-table* {})

(mac set-atom-compile-fn1 (type f)
  `(= (*atom-compile-table* ',type) ,f))

(by 2 set-atom-compile-fn set-atom-compile-fn1)

(mac not-nil (err-expr v object . rst)
  (once object
    `(if (nil? ,object) ,err-expr
         (let ,v ,object ,@rst))))

; (comp a) -> Return object
(def comp (a)
  (if (atm? a)
    (if (and (sym? a) (symbol-macro-set? a))
          (comp (symbol-macro-call a))
        (not-nil (err comp "Unknown atom a = $1" a)
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
  (not-nil (if (is nm 'call) (err comp-apply-fn "Unknown function nm = $1" nm)
              (comp `(call ,nm ,@args)))
    f (*function-compile-table* nm)
    (f @args)))

;;; Return object ;;;

(def ret-obj (ret-type format-obj (o opts {}))
  (mkdat 'ret format-obj (app {ret-type ret-type} opts)))

(def get-ret-type (ret-obj)
  (ret-obj 'ret-type))

;;; Places ;;;

(var *places* nil)

(def curr-place () (car *places*))

(mac in-place (p . bd)
  `(sta *places* ,p ,@bd))

; pip = place in place
(mac pip (p a)
  `(in-place ,p
     (place ,a)))

(def comp-and-pip (p a)
  (pip p (comp a)))

(def place (ret-type format-obj)
  format-obj)

;;; Macros ;;;

(with-object-accessors
  spec spec-
  macs macro-
  smacs symbol-macro-
  mmacs macro-macro-
)

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

(def add-block-place1 (a)
  (push a *block-places*))

(by 1 add-block-place add-block-place1)

(add-block-place 'root)

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

(def add-block-type1 (a)
  (push a *block-types*))

(by 1 add-block-type add-block-type1)

; *require-bracket-pairs* is a list of lists
; each list is of the form (ret-type place) which specifies that
;   when an obj of return type ret-type is placed in place, it requires brackets
; this should be used to override the precedence table if necessary
(var *require-bracket-pairs* nil)

(def add-require-bracket-pair (pair)
  (push pair *require-bracket-pairs*))

; this table contains both return types and places
; a return type needs brackets when being placed into a place
;   when the position of the type is lower than the position of the place
(var *precedence-table* '(
  in-paren loop-beg do-line
  do-line-obj
  in-line set-right
  set-obj
  add-left add-right sub-left set-left
  add-obj sub-obj
  mul-left mul-right div-left sub-right
  mul-obj div-obj
  div-right fn-being-called
  atom-obj nil-obj arr-obj fn-call-obj
))

(mac not-neg-one (object err-expr)
  (once object
    `(if (is ,object -1) ,err-expr
         ,object)))

(def get-precedence (a)
  (or (pos a *precedence-table*)
    (err get-precedence "Item $1 doesn't exist in the precedence table" a)))

(def requires-brackets? (ret-type place)
  ;(bug ret-type place *require-bracket-pairs*)
  (or (has [iso _ (lis ret-type place)] *require-bracket-pairs*)
      (< (get-precedence ret-type) (get-precedence place))))

(var *skippable-in-do-types* nil)

(def skippable-in-do? (ret-obj)
  (has (get-ret-type ret-obj) *skippable-in-do-types*))

(def add-skippable-in-do-type (a)
  (push a *skippable-in-do-types*))

(def needs-brackets-when-placing? (ret-type place)
  nil)

; *require-brace-pairs* is a list of lists
; each list is of the form (ret-type place) which specifies that
;   when an obj of return type ret-type is placed in place, it requires braces

(var *require-brace-pairs* nil)

(def add-require-brace-pair (pair)
  (push pair *require-brace-pairs*))

(def requires-braces? (ret-type place)
  ;(bug ret-type place *require-bracket-pairs*)
  (has [iso _ (lis ret-type place)] *require-brace-pairs*))

(def with-braces (a)
  (lns "{" (ind 2 a) "}"))

(def with-braces-if-needed (ret-type place a)
  (if (requires-braces? ret-type place) (with-braces a) a))

(def list-with-braces-if-needed (ret-type place a)
  (let r (requires-braces? ret-type place)
    (lis r (if r (with-braces a) a))))

; a must be a Return object
(def place-with-braces-if-needed (a)
  (with-braces-if-needed (get-ret-type a) (curr-place) (place a)))

(def list-place-with-braces-if-needed (a)
  (list-with-braces-if-needed (get-ret-type a) (curr-place) (place a)))

; (place return-obj) -> Format obj
(def place (return-obj)
  (with (ret-type (get-ret-type return-obj) format-obj (dat return-obj))
    ;(bug ret-type format-obj (block-place?) (block-type? ret-type))
    (if (block-place?)
          (if (block-type? ret-type) format-obj
              (return-place?) (lin "return " format-obj ";")
              (lin format-obj ";"))
        (block-type? ret-type)
          (err place "Can't place obj $1 of block type $2 inside inline place $3"
                     format-obj ret-type (curr-place))
        (requires-brackets? ret-type (curr-place))
          (lin "(" format-obj ")")
        format-obj)))

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

(set-atom-compile-fn
  nil [ret-obj 'nil-obj (dat (comp `(arr)))]
  num [ret-obj 'atom-obj (str _)]
  str [ret-obj 'atom-obj (dsp _)]
  sym [ret-obj 'atom-obj (js-var _)]
)

(add-skippable-in-do-type 'nil-obj)

;(add-skippable-in-do-type 'atom-obj)

(def comp-qt (a)
  (casetyp a
    nil (comp nil)
    num (comp a)
    sym (comp (str a))
    str (comp a)
    lis (comp `(lis ,@(map aqt a)))
    (err comp-qt "Unknown obj a = $1" a)))

(set-function-compile-fn qt comp-qt)

(def make-in-line (a)
  (lin @(btwa (map [comp-and-pip 'in-line _] a) ", ")))

(def comp-call (nm . args)
  (ret-obj 'fn-call-obj
    (lin (comp-and-pip 'fn-being-called nm) "(" (make-in-line args) ")")))

(set-function-compile-fn call comp-call)

(mac make-comp-bin1 (nm op)
  `(do (def ,(app 'comp-js- nm) (a b)
         (ret-obj ',(app nm '-obj)
            (lin (comp-and-pip ',(app nm '-left) a) ,op (comp-and-pip ',(app nm '-right) b))))
       (set-function-compile-fn ,(app 'js- nm) ,(app 'comp-js- nm))))

(by 2 make-comp-bin make-comp-bin1)

(make-comp-bin
  add "+"
  sub "-"
  mul "*"
  div "/"
  set " = "
)

(def comp-lis a
  (if (no a) (comp nil)
      (comp `(arr ,(car a) (lis ,@(cdr a))))))

(set-function-compile-fn lis comp-lis)

(def comp-arr a
  (ret-obj 'arr-obj (lin "[" (make-in-line a) "]")))

(set-function-compile-fn arr comp-arr)
;(add-skippable-in-do-type 'arr-obj)

(def comp-do a
  (if (no a) (comp nil)
      (no (cdr a)) (comp (car a))
      (block-place?) (comp-do-block @a)
      (comp-do-line @a)))

(def comp-do-block a
  (let fst-obj (in-place 'do (comp (car a)))
    (if (skippable-in-do? fst-obj) (comp-do @(cdr a))
        (ret-obj 'do-obj
          (lns (pip 'do fst-obj)
               (pip 'do-last (comp-do @(cdr a))))))))

(def comp-do-line a
  (let fst-obj (in-place 'do-line (comp (car a)))
    (if (skippable-in-do? fst-obj) (comp-do @(cdr a))
        (ret-obj 'do-line-obj
          (lin (pip 'do-line (comp (car a))) ", "
               (pip 'do-line (comp-do @(cdr a))))))))

(add-block-place 'do 'do-last)
(add-block-type 'do-obj)

;(add-require-bracket-pair '(do-line-obj in-line))

(set-function-compile-fn do comp-do)

(def comp-while (ts . bd)
  (ret-obj 'loop-obj
    (lin "while (" (comp-and-pip 'in-paren ts) ")"
      (if (no bd) ";"
          (in-place 'loop-body (place-with-braces-if-needed (comp-do @bd)))))))

(add-block-place 'loop-body)
(add-block-type 'loop-obj)

(set-function-compile-fn while comp-while)

(def comp-loop (start ts update . bd)
  (ret-obj 'loop-obj
    (lin "for (" (comp-and-pip 'loop-beg start) "; "
                 (comp-and-pip 'in-paren ts) "; "
                 (comp-and-pip 'in-paren update) ")"
         (if (no bd) ";"
             (in-place 'loop-body (place-with-braces-if-needed (comp-do @bd)))))))

(add-require-brace-pair '(do-obj loop-body))

(set-function-compile-fn loop comp-loop)

(def comp-if a
  (if (no a) (comp nil)
      (no (cdr a)) (comp (car a))
      (block-place?) (comp-if-block @a)
      (comp-if-line @a)))

(def comp-if-block (ts true-expr . rst)
  (ret-obj 'if-obj
    (lin "if (" (comp-and-pip 'in-paren ts) ")"
         (let (needed-braces format-obj) (in-place 'if-body (list-place-with-braces-if-needed (comp true-expr)))
           (if (no rst) format-obj
               (let result (in-place 'else-body (place-with-braces-if-needed (comp-if @rst)))
                 (if needed-braces (lin format-obj " else " result)
                     (lns format-obj (lin "else " result)))))))))

(add-require-brace-pair '(do-obj if-body))
(add-require-brace-pair '(do-obj else-body))

(add-block-place 'if-body 'else-body)
(add-block-type 'if-obj)

(set-function-compile-fn if comp-if)
