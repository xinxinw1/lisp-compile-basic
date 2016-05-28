;;;; Compiler ;;;;

#|
Examples:

(prn (compile "(1 2 3 4 5)"))

|#

(import Format lin lns ind flns lvl note proc)

;;; Main compile ;;;

(def compile (a)
  (proc (bug (cip-and-fobj 'root (prs a)))))

; numbers are sent to the call js procedure
; symbols are sent to sym
; strings are sent to str
; nil is sent to sym as 'nil
; the call procedure is used for calls that aren't
;   defined as procedures or macros

(var *atom-compile-table* {})

(macby set-atom-compile-fn (type f)
  `(= (*atom-compile-table* ',type) ,f))

(mac not-nil (err-expr v object . rst)
  (once object
    `(if (nil? ,object) ,err-expr
         (let ,v ,object ,@rst))))

; (comp a) -> Return object which has already been placed
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

(def make-ret-obj (ret-type format-obj (o flags {}))
  (mkdat 'ret format-obj {ret-type ret-type flags flags}))

; fobj = get format object
(def fobj (a-ret-obj)
  (dat a-ret-obj))

(def get-ret-type (a-ret-obj)
  (a-ret-obj 'ret-type))

(def get-ret-flags (a-ret-obj)
  (a-ret-obj 'flags))

(def get-ret-flag (a-ret-obj flag)
  (a-ret-obj 'flags flag))

(mac ret-obj (ret-type . bd)
  `(let #flags {}
     (let #format-obj (fwiths ((set-ret-flag (a x) (do (= (#flags a) x) nil))
                               (set-ret-flags (a) (do (app= #flags a) nil))
                               (pass-ret-flags (ret-obj) (do (set-ret-flags (get-ret-flags ret-obj)) ret-obj)))
                        ,@bd)
       (make-ret-obj ,ret-type #format-obj #flags))))

(mac place-ret-obj (ret-type . bd)
  `(place (ret-obj ,ret-type ,@bd)))

;;; Places ;;;

(var *places* nil)

(def curr-place () (car *places*))

(mac in-place (p . bd)
  `(sta *places* ,p ,@bd))

; compile in place
(def cip (p a)
  (in-place p (comp a)))

(def cip-and-fobj (p a)
  (fobj (cip p a)))

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

(defby add-block-place (a)
  (push a *block-places*))

(add-block-place 'root)

(var *return-places* nil)
(var *end-places* nil)

(defby add-return-place (a)
  (push a *return-places*))

(defby add-end-place (a)
  (push a *end-places*))

(def return-place? ((o places *places*))
  (if (no places) nil
      (has (car places) *return-places*) t
      (has (car places) *end-places*) (return-place? (cdr places))
      nil))

(var *block-types* nil)

(def block-type? (a)
  (has a *block-types*))

(defby add-block-type (a)
  (push a *block-types*))

; *require-bracket-pairs* is a list of lists
; each list is of the form (ret-type place) which specifies that
;   when an obj of return type ret-type is placed in place, it requires brackets
; this should be used to override the precedence table if necessary
(var *require-bracket-pairs* nil)

(defby add-require-bracket-pair (pair)
  (push pair *require-bracket-pairs*))

; this table contains both return types and places
; a return type needs brackets when being placed into a place
;   when the position of the type is lower than the position of the place
(var *precedence-table* '(
  in-paren loop-beg do-line func-name
  do-line-obj
  in-line if-line-yes if-line-no set-right
  set-obj
  if-line-test
  if-line-obj
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

(var *brace-places* nil)

(defby add-brace-place (a)
  (push a *brace-places*))

; *no-need-brace-pairs* is a list of lists
; each list is of the form (ret-type place) which specifies that
;   when an obj of return type ret-type is placed in place, it doesn't requires braces

(var *no-need-brace-pairs* nil)

(def add-no-need-brace-pair (a)
  (push a *no-need-brace-pairs*))

(def requires-braces? (ret-type place)
  ;(bug ret-type place *require-bracket-pairs*)
  (and (has place *brace-places*)
       (no (has [iso _ (lis ret-type place)] *no-need-brace-pairs*))))

(def with-braces (a)
  (lns "{" (ind 2 a) "}"))

(def with-braces-if-needed (ret-type place a)
  (if (requires-braces? ret-type place) (with-braces a) a))

(def list-with-braces-if-needed (ret-type place a)
  (let r (requires-braces? ret-type place)
    (lis (if r (with-braces a) a) r)))

; a must be a Return object
(def place-with-braces-if-needed (a)
  (with-braces-if-needed (get-ret-type a) (curr-place) (place a)))

(def list-place-with-braces-if-needed (a)
  (list-with-braces-if-needed (get-ret-type a) (curr-place) (place a)))

; (place return-obj) -> return obj
(def place (return-obj)
  (with (ret-type (get-ret-type return-obj)
         format-obj (fobj return-obj)
         flags (get-ret-flags return-obj))
    (ret-obj ret-type
      (set-ret-flags flags)
      (if (block-place?)
            (let temp-format-obj (if (block-type? ret-type) format-obj
                                     (return-place?)
                                       (do (set-ret-flag 'return t)
                                           (lin "return " format-obj ";"))
                                     (lin format-obj ";"))
              (if (and (requires-braces? ret-type (curr-place))
                       (get-ret-flag return-obj 'needs-braces))
                    (with-braces temp-format-obj)
                  temp-format-obj))
          (block-type? ret-type)
            (err place "Can't place obj $1 of block type $2 inside inline place $3"
                       format-obj ret-type (curr-place))
          (if (requires-brackets? ret-type (curr-place))
                (lin "(" format-obj ")")
              format-obj)))))

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
  nil [ret-obj 'nil-obj (fobj (pass-ret-flags (comp `(arr))))]
  num [place-ret-obj 'atom-obj (str _)]
  str [place-ret-obj 'atom-obj (dsp _)]
  sym [place-ret-obj 'atom-obj (js-var _)]
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
  (lin @(btwa (map [cip-and-fobj 'in-line _] a) ", ")))

(def comp-call (nm . args)
  (place-ret-obj 'fn-call-obj
    (lin (cip-and-fobj 'fn-being-called nm) "(" (make-in-line args) ")")))

(set-function-compile-fn call comp-call)

(macby make-comp-bin (nm op)
  `(do (def ,(app 'comp-js- nm) (a b)
         (place-ret-obj ',(app nm '-obj)
            (lin (cip-and-fobj ',(app nm '-left) a) ,op (cip-and-fobj ',(app nm '-right) b))))
       (set-function-compile-fn ,(app 'js- nm) ,(app 'comp-js- nm))))

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
  (place-ret-obj 'arr-obj (lin "[" (make-in-line a) "]")))

(set-function-compile-fn arr comp-arr)
;(add-skippable-in-do-type 'arr-obj)

(def comp-do a
  (if (no a) (comp nil)
      (no (cdr a)) (comp (car a))
      (block-place?) (comp-do-block @a)
      (comp-do-line @a)))

(def comp-do-block a
  (let fst-obj (cip 'do (car a))
    (if (skippable-in-do? fst-obj) (comp-do @(cdr a))
        (place-ret-obj 'do-obj
          (set-ret-flag 'needs-braces t)
          (lns (fobj fst-obj)
               (let rst-obj (in-place 'do-last (comp-do @(cdr a)))
                 (if (get-ret-flag rst-obj 'return) (set-ret-flag 'return t))
                 (fobj rst-obj)))))))

(def comp-do-line a
  (let fst-obj (cip 'do-line (car a))
    (if (skippable-in-do? fst-obj) (comp-do @(cdr a))
        (place-ret-obj 'do-line-obj
          (lin (fobj fst-obj) ", "
               (fobj (in-place 'do-line (comp-do @(cdr a)))))))))

(add-block-place 'do 'do-last)
(add-block-type 'do-obj)

(add-end-place 'do-last)

(set-function-compile-fn do comp-do)

(def comp-while (ts . bd)
  (place-ret-obj 'loop-obj
    (set-ret-flag 'needs-braces t)
    (lin "while (" (cip-and-fobj 'in-paren ts) ")"
      (if (no bd) ";"
          (fobj (in-place 'loop-body (comp-do @bd)))))))

(add-block-place 'loop-body)
(add-block-type 'loop-obj)

(set-function-compile-fn while comp-while)

(def comp-loop (start ts update . bd)
  (place-ret-obj 'loop-obj
    (set-ret-flag 'needs-braces t)
    (lin "for (" (cip-and-fobj 'loop-beg start) "; "
                 (cip-and-fobj 'in-paren ts) "; "
                 (cip-and-fobj 'in-paren update) ")"
         (if (no bd) ";"
             (fobj (in-place 'loop-body (comp-do @bd)))))))

(add-brace-place 'loop-body)

(set-function-compile-fn loop comp-loop)

(def comp-if a
  (if (no a) (comp nil)
      (no (cdr a)) (comp (car a))
      (block-place?) (comp-if-block @a)
      (comp-if-line @a)))

(def comp-if-block (ts true-expr . rst)
  (place-ret-obj 'if-obj
    (set-ret-flag 'needs-braces t)
    (withs (test-lin (lin "if (" (cip-and-fobj 'in-paren ts) ")")
            true-obj (cip 'if-body true-expr)
            true-format-obj (fobj true-obj)
            returns (get-ret-flag true-obj 'return))
      (if (get-ret-flag true-obj 'return)
            (withs (rest-obj (in-place 'else-after-body (comp-if @rst))
                    rest-format-obj (fobj rest-obj))
              (if (get-ret-flag rest-obj 'return)
                    (do (set-ret-flag 'return t)
                        (lns (lin test-lin true-format-obj) rest-format-obj))
                  (no rst)
                    (lin test-lin true-format-obj)
                  (lns (lin test-lin true-format-obj) rest-format-obj)))
          (no rst)
            (lin test-lin true-format-obj)
          (withs (rest-obj (in-place 'else-body (comp-if-else-block @rst))
                  rest-format-obj (fobj rest-obj))
            (if (get-ret-flag true-obj 'needs-braces)
                  (lin test-lin true-format-obj " else " rest-format-obj)
                (lns (lin test-lin true-format-obj) (lin "else " rest-format-obj))))))))

(def comp-if-else-block (ts true-expr . rst)
  (if (no true-expr) (comp ts)
      (place-ret-obj 'if-obj
        (withs (test-lin (lin "if (" (cip-and-fobj 'in-paren ts) ")")
                true-obj (cip 'if-body true-expr)
                true-format-obj (fobj true-obj))
          (if (no rst)
                (lin test-lin true-format-obj)
              (withs (rest-obj (in-place 'else-body (comp-if-else-block @rst))
                      rest-format-obj (fobj rest-obj))
                (if (get-ret-flag true-obj 'needs-braces)
                      (lin test-lin true-format-obj " else " rest-format-obj)
                    (lns (lin test-lin true-format-obj) (lin "else " rest-format-obj)))))))))

(def comp-if-line (ts true-expr . rst)
  (place-ret-obj 'if-line-obj
    (lin (cip-and-fobj 'if-line-test ts) "?"
         (cip-and-fobj 'if-line-yes true-expr) ":"
         (fobj (in-place 'if-line-no (comp-if @rst))))))

(add-brace-place 'if-body 'else-body)

(add-no-need-brace-pair '(if-obj else-body))

(add-block-place 'if-body 'else-body 'else-after-body)
(add-block-type 'if-obj)

(add-end-place 'if-body 'else-body 'else-after-body)

(set-function-compile-fn if comp-if)

(def comp-return (a)
  (place-ret-obj 'return-obj
    (fobj (pass-ret-flags (cip 'return a)))))

(add-block-place 'return)
(add-block-type 'return-obj)

(add-return-place 'return)

(set-function-compile-fn return comp-return)

(def comp-def (nm ag . bd)
  (place-ret-obj 'def-obj
    (lin "function " (cip-and-fobj 'func-name nm) "(" (make-in-line ag) ")"
         (with-braces (fobj (in-place 'func-body (comp-do @bd)))))))

(add-block-place 'func-body)
(add-block-type 'def-obj)
(add-return-place 'func-body)

(set-function-compile-fn def comp-def)
