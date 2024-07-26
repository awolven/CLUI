; -*- Mode: Lisp -*-
;; ---------------------------------------------------------------------------
;;     Title: CLEX-2 --- another scanner generator for Common Lisp
;;   Created: 2012-11-05
;;    Author: Gilbert Baumann <gilbert@bauhh.de>
;;   License: MIT style (see below)
;; ---------------------------------------------------------------------------
;;  (c) copyright 1997-2016 by Gilbert Baumann

;; *** NOT FOR REDISTRIBUTION ***

(defpackage :clex2
  (:nicknames :clex-deriv)
  (:use :common-lisp)
  (:export
   #:lexer
   #:lexing
   #:parse-re

   #:begin
   ;; #:start
   ;; #:end
   #:lex-warn
   #:lex-error
   ;;
   #||
   #:start-line-number
   #:end-line-number
   #:start-column
   #:end-column
   ||#
   #:with-input-from-lexer
   ;;
   #:scan
   #:scanner
   #:with-scan
   #:scan-case
   #:scan-to-strings
   ;;
   #:match
   #:matcher
   #:with-match
   #:match-case
   #:match-to-strings
   ;;
   #:transducer                         ;reserved
   #:transduce-string                   ;reserved
   ;;
   #:test-lexer
   #:show-dfa
   )
  (:shadow #:step))

(in-package :clex2)

(eval-when (:compile-toplevel)
  (declaim (optimize (speed 3) (safety 1))))

;;;; -- Changes -------------------------------------------------------------------------------

;; 2024-05-12   GB      - Support for :READ-SEQUENCE and :UNREAD-SEQUENCE scanner options.
;;                      - SCAN, SCANNER, WITH-SCAN, SCAN-CASE, SCAN-TO-STRINGS
;;                      - MATCH, MATCHNER, WITH-MATCH, MATCH-CASE, MATCH-TO-STRINGS

;; 2022-10-12   GB      - We did away with RE-* and use RE-** always further
;;                        min=-1 is the not yet expanded iteration for the POSIX
;;                        "empty longer than zero" rule.
;;                        RE-FIX-PREFER-EMPTY-OVER-NO-MATCH: gone

;; 2021-11-06   GB      - (:interactive-p ...) option to lexer
;;                      - (:sloc mode) new option
;;                        Also PARSED-LEXER-SLOC-MODE.
;;                        And :CHARACTER-POSITION sloc mode

;; 2021-11-04   GB      - (:syntax ...) option of lexer definitions
;;                      - :SYNTAX option for MATCH et al.
;;                      - SRE-RENAME-GROUPS: Have a :synax option, too
;;                      - :EXTENDED is the default syntax for MATCH et al.
;;                      - PARSE-RE: :start/:end args
;;                      - BASIC: :initial is the default

;; 2021-11-03   GB      - Changed the signature of WITH-MATCH.
;;                      - INTERN-$N -- new function
;;                      - TAG-START-REG, TAG-END-REG -- intern in our own package.
;;                      - Borrowed FORM-ALL-$N-SYMBOL-MAP from DE.BAUHH.PARSE
;;                      - The tagmap is modified to use integers for $n to be
;;                        matched in COMPILE-RULE-ACTION for actual user
;;                        variables named so.
;;                      - LEXING, SCAN, MATCH, WITH-SCAN, WITH-MATCH all
;;                        accpet :START/:END now.
;;                      - $$ is no longer exported
;;                      - BAG is no longer exported

;; 2021-08-02   GB      LEXER-AUX: bind *count-lines-p* and *count-lines-p*

;; 2021-07-18   GB      :dot-includes-newline-p option to LEXER and LEXING.

;; 2016-06-16   GB      *use-underflow-sentinel-p* depreciated
;;                      *direct-string-lexing-p* depreciated
;;                      Template could also scan strings directly.
;;

;; 2012-11-11   GB      Initial version

;;;; -- Ideas -----------------------------------------------------------------

;; For in-lexer reads, we would need to craft a gray stream, which uses our
;; buffer. That is the only way, we could keep SLOC character based. We won't
;; need that when doing it FILE-POSITION based.

;; --

;; A parser would need to have essentially.
;;
;;     SLOC-NOTE object sloc
;;     SLOC-COMBINE sloc-1 .. sloc-n
;;
;; and nothing more. On top of the VAL-STACK, we would have a SLOC-STACK. But
;; I fear, we would need to have a proper SLOC data type then, as this would
;; not only involve positions, but also a resource this is read from. And
;; there might be some include mechanism, and some macro mechanism.
;; Refactoring tools may also want to keep track of comments.

;; --

;; We could keep SLOC as character positions and leave it to the reporting
;; functions to show these as line numbers and/or context. There could be a
;; simple cache for some pivot positions, perhaps at buffer reads. The
;; rationale is, that there is no point in reporting errors quick, and
;; tracking line and column numbers while scanning is relatively expensive.

;; (:sloc { nil | :line | :line-column | :file-position | :character-position })

;;;

;; (* (or #\space #\newline
;;        (delimited '"/*" (* t) '"*/")
;;        (delimited '"//" (* t) (or #\newline :eof))))

;; - Use &WHOLE for $$

;; - Use (:start var), (:end var), (:buffer var)?

;; - Or $1.start $1.end $1.buffer


;;;; -- Overview --------------------------------------------------------------

;; This is a scanner generator for Common Lisp. Like in lex(1) scanner
;; definitions are a list of rules, which are a regular expression to match
;; and then the action to take. CLEX can cope with large character sets. You
;; can address submatches and have the whole arsenal of Boolean operations
;; on regular languages at your disposal. Trailing context is correct
;; always.

;; Regular expression could be expressed in both a traditional syntax as
;; well as in an s-expression based notation, both notations could be freely
;; mixed.

;; CLEX compiles to open code dispatch, which is quite fast. Scanners
;; usually process each character at about ten clock cycles.

;; A simple scanner could look like this:

#+(OR)
(defun foo-lexer (input)
  (lexer (input)
    ;; macros
    (= idstart "[A-Za-z_]")
    (= idchar  (or idstart "[0-9]"))
    ;; rules
    (:integer -> "[0-9]+"                  => (parse-integer $$))
    ;;                   v submatch
    (:integer -> "0x" (= d "[0-9A-Fa-f]+") => (parse-integer d :radix 16))
    ;;                                                       ^ addressing
    (:ident   -> (and idstart (* idchar))  => (intern $$))
    ;; ignore white space
    (         -> (or #\space #\newline))))

;; > (test-lexer #'foo-lexer "100 0xFF FOO")
;;   :INTEGER 100 
;;   :INTEGER 255
;;   :IDENT FOO
;; ;No value

;; For details refer to the documentation at http://bauhh.de/clex/


;;;; -- Usage ---------------------------------------------------------------------------------

;; (LEXER (input-var)
;;   clause*)

;; clause := (keyword? [-> sre... [=> action...]]*)
;;         | (:MACRO name expansion)            ;defines a macro
;;         | (= name expansion)                 ;dito
;;         | (:SIMPLE-TOKENS string...)
;;         | (:IN context-sepc clause...)
;;         | (:EOF action...)
;;         | (sre action...)
;;         | 
;;
;; sre := #\char
;;      | "string"                              ;parsed as regular expression in FLEX syntax
;;      | '"string"                             ;literal string
;;      | T                                     ;matches any character, even #\newline
;;      | :EOF                                  ;matches end of file
;;      | :BOL                                  ;matches beginning of line
;;      | :EOL                                  ;matches end of line
;;      | name                                  ;macro invokation
;;      | (RANGE from to)
;;      | (<= from to)
;;      | (AND sre...)
;;      | (OR sre...)
;;      | (* sre)
;;      | (+ sre)
;;      | (? sre)
;;      | (INTERSECTION sre sre)
;;      | (DIFFERENCE sre sre)  | (- sre sre)
;;      | (** min max sre)
;;      | (= var sre...)
;;      | (CI sre)
;;      | (CS sre)
;;      | (DELIMITED prefix stuff suffix)
;;      | (CONTAINS matter)
;;      | (DIGIT radix?)
;;      | (FOLLOWED-BY sre context)             ;only in tpl sre
;;
;; action := Common Lisp form with additional local macros/functions:
;;      | var                                   ;a variable previously introduced by (= var ...)
;;      | (BUFFER var)
;;      | (START var)
;;      | (END var)
;;      | (LINE-START-NUMBER)                   ;1-based
;;      | (LINE-END-NUMBER)                     ;1-based
;;      | (START-COLUMN)
;;      | (END-COLUMN)
;;      | (BEGIN &optional context)
;;      | (RETURN value)                        ;makes the lexer return, not calling RETURN,
;;                                              ; will make it continue scanning.
;;


;;;; -- Design Decisions ----------------------------------------------------------------------

;; - We are not as baroque as flex(1); what we do not have and why we think
;;   this is not useful:

;;    - We have no input stack. I have never seen that tokens may span
;;      across multiple input sources. When you need to handle include
;;      files, consider spawning another lexer. Our stuff is reentrant.

;;    - We have no REJECT. It is expensive and it really smells like you
;;      need a transducer rather.

;;    - Besides using handwritten code or another lexer/parser combo
;;      perhaps, we do not support to manipulate the buffer at will. See
;;      above.

;;;; -- Pit falls -----------------------------------------------------------------------------

;; - WITH-INPUT-FROM-LEXER fails to update the reading pointer, if the body
;;   does not exit normally. However, if we just had a buffer underflow real
;;   input may have been read from the underlying stream. So do not rely on
;;   the fact, that exiting abnormally would some how undo harm.

;; - We lexically analyze the action bodies, to draw conclusions about which
;;   features are used and which submatches are actually used. So do not
;;   hide the fact, that you use any of the locally defined entities.


;;;; -- Implementation Notes ------------------------------------------------------------------

;; This is one large macro, perhaps the largest macro I have ever
;; written. It DOES NOT use gensyms, for the most part because it would been
;; a very cumbersome thing to do. And I do not consider this a bug, since as
;; long as the user of the LEXER macro does not mess with addressing things
;; by "CLEX::" no harm is done. Common Lisp is not about establishing fences
;; and walls, but more about to give the progammer a lot of rope to play
;; with. Or put otherwise: Just don't mess with my package. End of story.


;;; Fixes
#+SBCL (declaim (notinline parsed-lexer-p)) ;Dammit, I didn't ask for that!


;;;; -- Implementation ------------------------------------------------------------------------

;;; Parameters for the generated scanner

(defparameter +end-of-file-sentinel+ -1
  "Sentinel to indicate end of file, this must NOT be a valid character code.")

(defparameter +beginning-of-file-sentinel+ -2
  "Another sentinel to use for the very beginning of the input.")

(defvar *template-optimize-settings*
  '(declare (optimize (speed 3) (safety 0)))
  "Compiler optimization settings to use in auto generated code in the
   scanner template; this does not apply to user code.")

(defvar *string-type* 'simple-string)

;; Options bound from the LEXER macro

(defvar *count-lines-p*)                ;Whether we count lines, bound in LEXER
(defvar *count-columns-p*)              ;Whether we count columns, bound in LEXER
(defvar *template-closure-p*)

(defvar *track-file-position-p*
  nil
  "Whether to track file positions for tokens reads. File positions
  would be reported as additional values. Can be tweaked by
  the :TRACK-FILE-POSITION-P scanner option, which defaults to NIL.")

;;; Debugging

(defvar *dfa* nil
  "For debugging purposes, the last generated DFA is stored here. Use
   e.g. SHOW-DFA or PLOT-DFA to peek at it.")

(defvar *expansion* nil
  "For debugging purposes, the last expansion of the LEXER macro.")

(defvar *lexer* nil
  "For debugging purposes, the parsed lexer definition.")

(defvar *re-dfa-scanner-verbose* nil)

(defvar *dfa0*)

;; For Emacs:

;; (put 'lexer 'lisp-indent-function 1)
;; (put 'isum-case 'lisp-indent-function 1)
;; (put 'map-dfa-transitions 'lisp-indent-function 1)
;; (put 'map-dfa-states 'lisp-indent-function 1)


;;;; -- Design Decisions ----------------------------------------------------------------------

;; 1. Would we demand explicit capture of the whole shebang?

;; 2. Is the default, that strings are parsed as REs correct? What about
;;    S-Expr-heavy code?
;;
;;    Perhaps (lexer (input)
;;              (:sexp-only-p t)
;;              (:syntax :extended)
;;              ...)

;; 5. Would we care about equivalent classes? Perhaps with these huge sets,
;;    that XML names.

;; 6. T as redefinable macro?

;; ======


;;;; -- Some Infrastructure -------------------------------------------------------------------

(defmacro define-constant (&rest xs)
  ;; Common Lisps defconst is borken.
  `(defparameter ,@xs))

(defmacro memo ((hash-table key) &rest forms)
  "Short hand for looking something up in a hash table and generate it if
   not found."
  (let ((ht (gensym "HT."))
        (gkey (gensym "KEY."))
        (val (gensym "VAL."))
        (foundp (gensym "FOUNDP.")))
    `(let ((,gkey ,key)
           (,ht ,hash-table))
       (multiple-value-bind (,val ,foundp) (gethash ,gkey ,ht)
         (if ,foundp
             ,val
             (setf (gethash ,gkey ,ht)
                   (progn ,@forms)))))))

(defun note (fmt &rest args)
  (format t "~&~<;; ~@;~?~:>" (list fmt args)))

;;; Compiler warnings

(defvar *compiler-context-chain* nil)

(defun compiler-warn (form format &rest args)
  "Issue a warning about the form /form/. When possible implementation
specific means are used to point the finger at the form, so that eventually
SLIME or whatever IDE could highlight the context. When not available or not
within the compiler, a normal warning is issued.

Obviously this does not work if /form/ is a symbol or a number.

To narrow down the context you could setup a next best context with
COMPILER-DESCEND."
  ;; Any adaption to other Common Lisp implementations is welcome.
  #+CCL
  (let ((context (some #'ccl::nx-source-note (cons form *compiler-context-chain*))))
    (if context
        (let ((ccl::*nx-current-note* context))
          (ccl::nx1-whine :program-error (apply #'format nil format args)))
        (warn "~@<~? ~_---> ~S~:>" format args form)))
  #-CCL
  (warn "~@<~? ~_---> ~S~:>" format args form))

(defmacro compiler-descend (form &body body)
  `(let ((*compiler-context-chain*
          (if (consp ,form)
              (cons ,form *compiler-context-chain*)
              *compiler-context-chain*)))
     ,@body))

;; Outside HANDLE-WHINING a COMPILER-WHINE is an error. Inside it is a
;; warning. But when HANDLE-WHINING detected that COMPILER-WHINE was
;; invoked, it whines itself just returning NIL. So these things nest! The
;; intention is, that we report as many problems as we could find without
;; dying right away. The outermost COMPILER-WHINE then would turn into an
;; error.

;; COMPILER-WARN

;;     A warning about something unusual, like dead code detected; but
;;     nothing on the line of syntactic or semantic error.

;; COMPILER-WHINE

;;     The programmer really is in error. We carry on, but the topmost
;;     WITH-WHINING will fail.

;; COMPILER-ERROR

;;     We cannot carry on and quit doing things right away.


;;;; -- Interval Sums -------------------------------------------------------------------------

;; To support large character sets, we need an implemention of a set of
;; characters. Traditional scanner generators would at some place just
;; enumerate the alphabet \Sigma, which is not feasible with large character
;; sets like Unicode.

;; We handle all transitions in the automaton as a set of of the codes of
;; characters, expressed by an ISUM. The representation of such a set is
;; best defined by the ISUM-MEMBER function, but here is an overview to get
;; the idea:

;;     NIL          is the empty set
;;     (a b)        is the set [a, b)
;;     (a b c d)    is the set [a, b) u [c, d)
;;     (nil)        is everything
;;     (nil a b)    is everything but [a, b)

;; An ISUM is a sequence of stricly monotonic increasing integers. The idea
;; is that when you sweep a pointer over the list at each element found the
;; membership in the set changes. Like (1 10 12 15). You start outside the
;; set, find 1 and say "above or equal 1 is in the set" and then find 10 and
;; say "above or equal 10 is not in the set" and so on. This way it is very
;; easy to implement Boolean operations on sets.

(define-constant +isum-nothing+ nil
  "The empty set.")

(define-constant +isum-everything+ '(nil)
  "The set that contains every integer.")

(define-constant +isum-any-character+ (list 0 char-code-limit)
  "The set that contains every character by its code point.")

(define-constant +isum-ascii-printable+ (list 32 127))

(define-constant +ascii-ctype+
    '(("alnum"   (or (<= #.(code-char 48) #.(code-char 57))
                     (<= #.(code-char 65) #.(code-char 90))
                     (<= #.(code-char 97) #.(code-char 122))))
      ("alpha"   (or (<= #.(code-char 65) #.(code-char 90))
                     (<= #.(code-char 97) #.(code-char 122))))
      ("blank"   (or #\tab #\space))
      ("cntrl"   (or (<= #.(code-char 0) #.(code-char 31))
                     #.(code-char 127)))
      ("digit"   (<= #.(code-char 48) #.(code-char 57)))
      ("graph"   (<= #.(code-char 33) #.(code-char 126)))
      ("lower"   (<= #.(code-char 97) #.(code-char 122)))
      ("print"   (<= #.(code-char 32) #.(code-char 126)))
      ("punct"   (or (<= #.(code-char 33) #.(code-char 47))
                     (<= #.(code-char 58) #.(code-char 64))
                     (<= #.(code-char 91) #.(code-char 96))
                     (<= #.(code-char 123) #.(code-char 126))))
      ("space"   (or (<= #.(code-char 9) #.(code-char 13))
                     #\space))
      ("upper"   (<= #.(code-char 65) #.(code-char 90)))
      ("xdigit"  (or (<= #.(code-char 48) #.(code-char 57))
                     (<= #.(code-char 65) #.(code-char 70))
                     (<= #.(code-char 97) #.(code-char 102))))
      ("space"   (or (<= #.(code-char 9) #.(code-char 13))
                     #\space))
      ("<"       :bow)
      (">"       :eow)))

(defun isum-singleton (x)
  "Returns the ISUM, that contains only /x/."
  (list x (1+ x)))

(defun isum-range (from below)
  "Returns the ISUM, that contains every code point that is in [from, below)"
  (list from below))

(defun set-isum (xs)
  (let ((r +isum-nothing+))
    (loop for x in xs do (setf r (isum-union r (isum-singleton x))))
    r))

;;; Membership

(defun isum-member (x isum)
  "Determines, whether /x/ is member of the ISUM /isum/. Returns non-NIL if so and NIL otherwise."
  (declare (type fixnum x)
           (optimize (speed 3) (safety 0)))
  (loop for i of-type fixnum from 0
     for y in isum
     when (and y (< x (the fixnum y))) return (oddp i)
     finally (return (oddp i))))

;;; Boolean operation on ISUMs

(defmacro isum-op (op A B)
  "Combine the sets A and B by the Boolean operator op, which should be a
valid argument to the BOOLE function. An integer x is member of the
resulting set iff
     (logbitp 0 (boole op (if (isum-member x A) 1 0) (if (isum-member x B) 1 0)))
 is non-NIL. That way e.g. boole-ior denotes the union."
  `((lambda (A B)
      (declare (optimize (speed 3) (safety 0)))
      (let* ((Ain 0)
             (Bin 0)
             (Cin 0)
             (s nil)
             (res (cons nil nil))
             (resf res))
        (declare (type fixnum Ain Bin Cin)
                 (type cons res))
        ;; Get rid of an initial NIL, which indicates a complemented set.
        (when (and A (null (car A)))
          (pop A) (setq Ain (- 1 Ain)))
        (when (and B (null (car B)))
          (pop B) (setq Bin (- 1 Bin)))
        ;; Now traverse A and B in paralell and generate the resulting sequence.
        (loop
           (when (/= Cin
                     (the fixnum (ldb (byte 1 0) (the fixnum (boole ,op (the fixnum Ain) (the fixnum Bin))))))
             (setf resf (setf (cdr resf) (cons s nil)))
             (setf Cin (the fixnum (- 1 Cin))))
           (cond ((null A)
                  (cond ((null B)
                         (return))
                        (t
                         (setq s (pop B))
                         (setq Bin (the fixnum (- 1 Bin))))))
                 ((null B)
                  (setq s (pop A)) (setq Ain (the fixnum (- 1 Ain))))
                 ((< (the fixnum (car A)) (the fixnum (car B)))
                  (setq s (pop A)) (setq Ain (the fixnum (- 1 Ain))))
                 ((< (the fixnum (car B)) (the fixnum (car A)))
                  (setq s (pop B)) (setq Bin (the fixnum (- 1 Bin))))
                 (t
                  (setq s (pop A)) (setq Ain (the fixnum (- 1 Ain)))
                  (pop B) (setq Bin (the fixnum (- 1 Bin)))) ))
        (cdr res)))
    ,A ,B))

;;; Boiler plate

;; Now we could define interesting set operations in terms of ISUM-OP.

(defun isum-union (a b)         (isum-op boole-ior a b))
(defun isum-intersection (a b)  (isum-op boole-and a b))
(defun isum-difference (a b)    (isum-op boole-andc2 a b))
(defun isum-complement (a)      (isum-op boole-c1 a nil))

;;; Misc

(defun isum-witness (isum)
  "Returns a witness of the set /isum/, which must be non empty."
  (cond ((null isum)
         (error "The empty set has no witnesses."))
        ((null (car isum))
         (if (cdr isum)
             (1- (cadr isum))
             42))                       ;you get it
        (t
         (car isum))))

(defun isum-empty-p (isum)
  (null isum))


;;;; -- Regular Expressions -------------------------------------------------------------------

;; For speed, we define regular expressions as structures. The
;; individual types inherit from another. Common Lisp implementations
;; are pretty good at compiling TYPECASEs for structure types. We even
;; could have used CLOS, but then I am abit old-skool here.

;; The important thing is, that all regular expressions are
;; interned. Two regular expressions, which are equal actually are
;; also #'EQ.

;; The regular expressions of arity zero are preallocated and are
;; never consed otherwise. There really is only one +EPSILON+.

;; For speed we implement our own hash table. Mind you: It has a fixed
;; size, but that seemed good enough for me.

#-CMU
(declaim (inline re-name re-lhs re-rhs re-%vars re-%stripped))

(defstruct (re (:constructor make-re (name)))
  (name 0 :type fixnum)
  (%stripped nil :type (or null re))
  (%nullable :unknown)
  (%vars nil)
  (%has-pretext 0 :type fixnum))

(defstruct (re-epsilon (:include re) (:constructor make-re-epsilon (name))))
(defstruct (re-null (:include re) (:constructor make-re-null (name))))
(defstruct (re-not-null (:include re) (:constructor make-re-not-null (name))))

(defstruct (re-unop (:include re) (:constructor make-re-unop (name lhs)))
  (lhs nil :type re))

(defstruct (re-not (:include re-unop) (:constructor make-re-not (name lhs))))

(defstruct (re-** (:include re) (:constructor make-re-** (name min max sub)))
  min max
  (sub nil :type re))

(defstruct (re-binop (:include re-unop) (:constructor make-re-binop (name lhs rhs)))
  (rhs nil :type re))

(defstruct (re-and (:include re-binop) (:constructor make-re-and (name lhs rhs))))
(defstruct (re-or (:include re-binop) (:constructor make-re-or (name lhs rhs))))
(defstruct (re-intersection (:include re-binop) (:constructor make-re-intersection (name lhs rhs))))

(defstruct (re-vector (:include re) (:constructor make-re-vector (name args)))
  args)

(defstruct (re-set (:include re) (:constructor make-re-set (name args)))
  args)

(defstruct (re-setq (:include re) (:constructor make-re-setq (name args)))
  args)

(defstruct (re-grep (:include re-binop) (:constructor make-re-grep (name lhs rhs))))

(defstruct (re-conf (:include re-vector) (:constructor make-re-conf (name args))))
(defstruct (re-pretext (:include re-unop) (:constructor make-re-pretext (name lhs))))
(defstruct (re-epitext (:include re-unop) (:constructor make-re-epitext (name lhs))))

;; short hands

(defmacro lhs (x) `(re-unop-lhs ,x))
(defmacro rhs (x) `(re-binop-rhs ,x))
(defmacro re-isum (x) `(re-set-args ,x))
(defmacro re-subst (x) `(re-setq-args ,x))
(defmacro re-vector-elements (x) `(re-vector-args ,x))

(defconstant +re-hash-size+ 8009)
;;(defconstant +re-hash-size+ 107)
;;(defconstant +re-hash-size+ 503)

(defstruct (re-hash (:constructor %make-re-hash))
  (vector (make-array +re-hash-size+ :initial-element nil)
          :type simple-vector)
  (next-name 10 :type fixnum)
  (var-hash (make-hash-table :test #'eq))
  (next-var -100 :type fixnum))

(defmethod print-object ((object re-hash) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "next-name = ~D, var-hash = ~D"
            (re-hash-next-name object)
            (re-hash-var-hash object))))

#+SBCL
'(declaim (sb-ext:freeze-type re re-epsilon re-null re-not-null 
                             re-unop re-not re-binop re-and
                             re-or re-intersection re-vector re-set re-setq
                             re-**
                             re-grep re-conf re-pretext re-epitext
                             re-hash))

;; Zero-ary regular expressions.

(defparameter +epsilon+  (make-re-epsilon 0))
(defparameter +null+     (make-re-null 1))
(defparameter +not-null+ (make-re-not-null 2))

(defun make-re-hash ()
  (%make-re-hash))

(defparameter *re-hash*
  (make-re-hash))

(defun clear ()
  (setf *re-hash* (make-re-hash))
  (values))

;;; Low level constructors for regular expression nodes

;; Don't use these directly, rather use RE-AND, RE-OR and so on.

(defun var-name (x)
  (or (gethash x (re-hash-var-hash *re-hash*))
      (setf (gethash x (re-hash-var-hash *re-hash*))
            (decf (re-hash-next-var *re-hash*)))))

(declaim (inline var<))
(defun var< (x y)
  (< (if (integerp x) x (var-name x))
     (if (integerp y) y (var-name y))))

(declaim (inline borrowing-cons))
(defun borrowing-cons (cons car cdr)
  (if (and (eq (car cons) car)
           (eq (cdr cons) cdr))
      cons
      (cons car cdr)))

(defun borrowing-mapcar (fun list)
  (and list
       (borrowing-cons list (funcall fun (car list)) (borrowing-mapcar fun (cdr list)))))

(defun %re-merge-vars (vs1 vs2)
  (let (v1 v2)
    (cond ((null vs1) vs2)
          ((null vs2) vs1)
          ((eql (car (setf v1 (car vs1)))
                (car (setf v2 (car vs2))))
           (cond ((eql (cadr v1) (cadr v2))
                  (borrowing-cons vs1
                                  (car vs1) (%re-merge-vars (cdr vs1) (cdr vs2))))
                 ((null (cadr v1))
                  (borrowing-cons vs1
                                  (car vs1) (%re-merge-vars (cdr vs1) vs2)))
                 ((null (cadr v2))
                  (borrowing-cons vs2
                                  (car vs2) (%re-merge-vars vs1 (cdr vs2))))
                 ((< (cadr v1) (cadr v2))
                  (borrowing-cons vs1
                                  (car vs1) (%re-merge-vars (cdr vs1) vs2)))
                 (t
                  (borrowing-cons vs2
                                  (car vs2) (%re-merge-vars vs1 (cdr vs2))))))
          ((var< (caar vs1) (caar vs2))
           (borrowing-cons vs1 (car vs1) (%re-merge-vars (cdr vs1) vs2)))
          (t
           (borrowing-cons vs2 (car vs2) (%re-merge-vars vs1 (cdr vs2)))))))

(defmacro re-cons-aux (&key hash tester cons)
  `(let ((hash 5831))
     (declare (type (unsigned-byte 24) hash)
              (optimize (speed 3) (safety 0)))
     (labels ((hash-step (x)
                (declare (type fixnum x))
                (setf hash (logand #xFFFFFF (logxor x (* 33 hash))))))
       (declare (inline hash-step))
       ,hash
       (setf hash (mod hash +re-hash-size+))
       (let* ((hash-table *re-hash*)
              (vec (re-hash-vector hash-table)))
         (loop for q in (svref vec hash) do
              (let ((item q))
                (when (,tester item)
                  (return item)))
              finally
              (return
                (let ((res ,cons))
                  (push res (svref vec hash))
                  res)))))))

(defmacro re-cons-binop (key type constructor x y)
  `(re-cons-aux :hash (progn
                        (hash-step (re-name ,y))
                        (hash-step (re-name ,x))
                        (hash-step ,key))
                :tester (lambda (item)
                          (and (typep item ',type)
                               (eq (lhs item) ,x)
                               (eq (rhs item) ,y)))
                :cons (let ((r (,constructor (incf (re-hash-next-name hash-table)) x y)))
                        (setf (re-%has-pretext r)
                              (the fixnum (logior (the fixnum (re-%has-pretext x))
                                                  (the fixnum (re-%has-pretext y)))))
                        (setf (re-%vars r)
                              (%re-merge-vars (re-%vars x) (re-%vars y)))
                        r)))

(defun re-cons-and (x y)
  "Low level regular expression cons, should only be called via RE-AND."
  (re-cons-binop 1 re-and make-re-and x y))

(defun re-cons-or (x y)
  "Low level regular expression cons, should only be called via RE-OR."
  (assert (not (typep y 're-or)))
  (re-cons-binop 2 re-or make-re-or x y))

(defun re-cons-intersection (x y)
  "Low level regular expression cons, should only be called via RE-INTERSECTION."
  (re-cons-binop 3 re-intersection make-re-intersection x y))

(defmacro re-cons-unop (key type constructor x &optional (flags 0))
  `(re-cons-aux :hash (progn
                        (hash-step (re-name ,x))
                        (hash-step ,key))
                :tester (lambda (item)
                          (and (typep item ',type)
                               (eq (lhs item) ,x)))
                :cons (let ((r (,constructor (incf (re-hash-next-name hash-table)) ,x)))
                        (setf (re-%vars r) (re-%vars ,x))
                        (setf (re-%has-pretext r) (logior ,flags (re-%has-pretext ,x)))
                        r)))

(defun re-cons-not (x)
  "Low level regular expression cons, should only be called via RE-NOT."
  (re-cons-aux :hash (progn
                       (hash-step (re-name x))
                       (hash-step 4))
               :tester (lambda (item)
                         (and (typep item 're-not) (eq (lhs item) x)))
               :cons (let ((r (make-re-not (incf (re-hash-next-name hash-table)) x)))
                       (setf (re-%vars r) (re-%vars x))
                       (setf (re-%has-pretext r) (re-%has-pretext x))
                       r)))

(defun re-cons-grep (x y)
  (re-cons-binop 9 re-grep make-re-grep x y))

(defun re-cons-isum (isum)
  "Low level regular expression cons, should only be called via RE-SET."
  (re-cons-aux :hash (progn
                       (hash-step 0)
                       (loop for x in isum do
                            (hash-step (the fixnum (or x 0)))))
               :tester (lambda (item)
                         (and (typep item 're-set)
                              (do ((p (re-isum item) (cdr p))
                                   (q isum (cdr q)))
                                  ((or (null p) (null q)) (eq p q))
                                (unless (eql (the fixnum (car p)) (the fixnum (car q))) ;???
                                  (return nil)))))
               :cons (make-re-set (incf (re-hash-next-name hash-table)) isum)))

(defun re-cons-vector (vector)
  "Low level regular expression cons, should only be called via RE-VECTOR."
  (re-cons-aux :hash (progn
                       (hash-step 6)
                       (loop for x in vector do (hash-step (the fixnum (re-name x)))))
               :tester (lambda (item)
                         (and (typep item 're-vector)
                              (do ((p (re-vector-elements item) (cdr p))
                                   (q vector (cdr q)))
                                  ((or (null p) (null q)) (eq p q))
                                (unless (eq (car p) (car q))
                                  (return nil)))))
               :cons (let ((r (make-re-vector (incf (re-hash-next-name hash-table)) vector)))
                       (let ((vars nil)
                             (has-pretext 0))
                         (declare (type fixnum has-pretext))
                         (loop for item in vector do
                               (setf vars (%re-merge-vars vars (re-%vars item)))
                               (setf has-pretext (logior has-pretext (re-%has-pretext item))))
                         (setf (re-%vars r) vars
                               (re-%has-pretext r) has-pretext))
                       r)))

(defun re-cons-** (min max sub)
  "Low level regular expression cons, should only be called via RE-**."
  (re-cons-aux :hash (progn
                       (hash-step (or min 0))
                       (hash-step (or max 0))
                       (hash-step (re-name sub))
                       (hash-step 7))
               :tester (lambda (item)
                         (and (typep item 're-**)
                              (eql (the (or fixnum null) (re-**-min item)) (the (or fixnum null) min))
                              (eql (the (or fixnum null) (re-**-max item)) (the (or fixnum null) max))
                              (eq (re-**-sub item) sub)))
               :cons (let ((r (make-re-** (incf (re-hash-next-name hash-table)) min max sub)))
                       (setf (re-%vars r) (re-%vars sub))
                       (setf (re-%has-pretext r) (re-%has-pretext sub))
                       r)))

(defun re-cons-setq (vars)
  "Low level regular expression cons, should only be called via RE-SETQ. Note that /vars/ must be sorted."
  (re-cons-aux :hash (progn
                       (hash-step (sxhash vars)))
               :tester (lambda (item)
                         (and (typep item 're-setq)
                              (equal (re-setq-args item) vars)))
               :cons (let ((r (make-re-setq (incf (re-hash-next-name hash-table)) vars)))
                       (setf (re-%vars r) vars)
                       (setf (re-%has-pretext r) 0)
                       r)))

;;; Printer

(defmethod print-object ((object re) stream)
  (print-unreadable-object (object stream :type nil :identity nil)
    (format stream "RE ~S" (re-sre object))))


;;;; -- Regular Expression Constructors -------------------------------------------------------

;; These constructors implement all of the following identities:

;;            r & r = r
;;            r & s = s & r
;;      (r & s) & t = r & (s & t)
;;        \null & r = \null
;;       !\null & r = r

;;          (r s) t = r (s t)
;;          \null r = \null
;;          r \null = \null
;;           \eps r = r
;;           r \eps = r

;;            r + r = r
;;            r + s = s + r
;;      (r + s) + t = t + (s + t)

;;       !\null + r = !\null
;;        \null + r = r

;;              r** = r*
;;           (and)* = (and)
;;            (or)* = (and)
;;              !!r = r

;;; RE-SET

(defun re-set (isum)
  (if (equal isum +isum-nothing+)
      +null+
      (re-cons-isum isum)))

(defun re-char (c)
  (re-set (isum-singleton (if (characterp c) (char-code c) c))))

(declaim (inline re-stripped))
(defun re-stripped (re)
  "The regular expression /re/ without any SETQ nodes."
  (or (re-%stripped re)
      (if (null (re-%vars re))
          re
          (setf (re-%stripped re)
                (re-stripped-1 re)))))

;;; RE-OR

;; Note: RE-AND is no function and short circuits.

(define-compiler-macro re-or (&rest terms)
  (cond ((null terms) nil)
        ((null (cdr terms)) (car terms))
        (t (reduce #'(lambda (x y) `(re-or/2 ,x ,y)) terms))))

(declaim (inline re-or/2))
(defun re-or/2 (x y)
  (cond ((eq x +null+)  y)
        ((eq y +null+)  x)
        ((re-or-p y)    (locally
                            (declare (notinline re-or/2))
                          (re-or (re-or x (lhs y)) (rhs y))))
        ((eq x y) x)
        ;; Look for a canceling term
        ((let ((y* (re-stripped y)))
           (do ((q x (lhs q)))
               ((not (re-or-p q)) (eq (re-stripped q) y*))
             (when (eq (re-stripped (rhs q)) y*)
               (return t))))
         x)
        ((and (re-set-p x) (re-set-p y))
         (re-set (isum-union (re-isum x) (re-isum y))))
        (t
         (re-cons-or x y))))

(defun re-or (&rest terms)
  (cond ((null terms) +null+)
        ((null (cdr terms)) (car terms))
        (t (reduce #'re-or/2 terms))))

(defun re-or* (terms)
  (cond ((null terms) +null+)
        ((null (cdr terms)) (car terms))
        (t (reduce #'re-or/2 terms))))

;;; RE-AND

;; Note: RE-AND is no function and short circuits.

(defmacro re-and (&rest terms)
  (cond ((null terms) '+epsilon+)
        ((null (cdr terms)) (car terms))
        ((null (cddr terms))
         ;; short circuit
         (destructuring-bind (x y) terms
           (let ((a (gensym "A."))
                 (b (gensym "B.")))
             `(let ((,a ,x))
                (cond ((eq ,a +null+) +null+)
                      (t
                       (let ((,b ,y))
                         (cond ((eq ,a +epsilon+) ,b)
                               ((eq ,b +null+) +null+)
                               (t (re-and/2 ,a ,b))))))))))
        (t
         `(re-and ,(car terms) (re-and ,@(cdr terms))))))

(defun re-and/2 (x y)
  (cond ((eq x +epsilon+) y)
        ((eq y +epsilon+) x)
        ((eq x +null+) +null+)
        ((eq y +null+) +null+)
        ((and (eq x +not-null+) (eq y +not-null+)) +not-null+)

        ((and (re-setq-p x) (re-or-p y))
         (re-or (re-and x (lhs y)) (re-and x (rhs y))))

        ((and (re-or-p x) (re-setq-p y))
         (re-or (re-and (lhs x) y) (re-and (rhs x) y)))

        ;; Concatenation is associative only, when there are no tag assignments.
        ((and (re-and-p x)
              (or (re-setq-free-p x) (re-setq-free-p y)))
         (re-and (lhs x) (re-and (rhs x) y)))

        ((and (re-setq-p x) (re-setq-p y))
         (re-merge-setqs y x))

        ((and (re-and-p x) (re-setq-p (lhs x)))
         (re-and (lhs x) (re-and (rhs x) y)))

        ((and (re-setq-p x) (re-and-p y) (re-setq-p (lhs y)))
         (re-and (re-merge-setqs (lhs y) x) (rhs y)))

        ;; ### This makes 7 tests fail. Why? I mean, this ought to be fine.
        ;; ((re-or-p y) (re-or (re-and x (lhs y)) (re-and x (rhs y))))
        ;; ((re-or-p x) (re-or (re-and (lhs x) y) (re-and (rhs x) y)))

        ;; This only works for PCRE
        ;; ((re-and-p x) (re-and (lhs x) (re-and (rhs x) y)))

        ;; Hmm.
        ((and (re-and-p x) (re-setq-p (rhs x)))
         ;; (r \phi) s => r (\phi s)
         (re-and (lhs x) (re-and (rhs x) y)))
        
        (t
         (re-cons-and x y))))

(defun re-and* (xs)
  (cond ((= (length xs) 0) +epsilon+)
        ((= (length xs) 1) (car xs))
        ((> (length xs) 2) (re-and (car xs) (re-and* (cdr xs))))
        (t                 (re-and (first xs) (second xs)))))

;;; RE-SETQ

(defun re-setq (&rest vars)
  ;; /vars/ _must_ be sorted.
  (setf vars (mapcar (lambda (x)
                       (when (and (consp x) (second x) (null (third x)))
                         (error "barf"))
                       (cond ((atom x) (list x nil 'p))
                             ((second x) (list (first x) (second x) (third x)))
                             (t (list (first x) (second x) (or (third x) 'p)))))
                     vars))
  (re-cons-setq vars))

(defun re-setq* (vars)
  (let ((res +epsilon+))
    (loop for v in vars do (setf res (re-and res (re-setq v))))
    res))

(defun re-setq-free-p (re)
  "Is the regular expression /re/ free of SETQ nodes?"
  (eq (re-stripped re) re))             ;sic

;;;

(defun re-intersection (x y)
  (cond ((eq x +null+) +null+)
        ((eq y +null+) +null+)
        ((eq x +not-null+) y)
        ((eq y +not-null+) x)
        ((and (eq (re-stripped x) +epsilon+) (re-set-p (re-stripped y))) +null+)
        ((and (eq (re-stripped y) +epsilon+) (re-set-p (re-stripped x))) +null+)
        ;; The next two don't work with :PRETEXT
        ;; ((and (eq x +epsilon+) (eq +null+ (re-nullable y))) +null+)
        ;; ((and (eq y +epsilon+) (eq +null+ (re-nullable x))) +null+)
        ((eq x y) x)
        ((and (or (eq x +epsilon+) (re-setq-p x))
              (or (eq y +epsilon+) (re-setq-p y)))
         (re-and x y))                  ;conflicts?
        ((and (re-and-p y)
              (re-setq-p (lhs y)))
         (re-and (lhs y) (re-intersection x (rhs y))))
        ((and (re-and-p x)
              (re-setq-p (lhs x)))
         (re-and (lhs x) (re-intersection (rhs x) y)))
        ;; ???
        ;; ((eq (re-stripped x) (re-stripped y)) x)
        ((let ((y* (re-stripped y)))
           (do ((q x (lhs q)))
               ((not (re-intersection-p q)) (eq (re-stripped q) y*))
             (when (eq (re-stripped (rhs q)) y*)
               (return t))))
         x)
        ;;
        #+NIL
        ((or (eql x +epsilon+) (re-setq-p x))
         (let ((n (re-nullable y)))
           (re-intersection x n)))
        #+NIL
        ((or (eql y +epsilon+) (re-setq-p y))
         (let ((n (re-nullable x)))
           (re-intersection n y)))
        ;;
        ((typep y 're-intersection)
         (re-intersection (re-intersection x (lhs y)) (rhs y)))
        ;; Look for a canceling term
        ((let* ((y* (re-stripped y))
                (not-y* (re-not y*)))
           (do ((q x (lhs q)))
               ((not (typep q 're-intersection))
                (when (eq (re-stripped q) not-y*)
                  (return-from re-intersection +null+))
                (eq (progn 're-stripped q) y*) ;<--- ### not correct
                nil)
             (when (eq (re-stripped (rhs q)) not-y*)
               (return-from re-intersection +null+))
             ;; ### this is _not_ correct; it kills SETQs in RE-NULLABLE
             '(when (eq (progn 're-stripped (rhs q)) y*)
               (return t))
             ))
         (warn "cancel: ~S ~S" x y)     ;<--- ### what to do about this?
         x)
        ;; (not x) + x ?
        ((and (typep x 're-set)
              (typep y 're-set))
         (re-set (isum-intersection (re-isum x) (re-isum y))))
        #||
        ;;
        ((and (typep (re-stripped x) 're-set)
              (typep (re-stripped y) '(or re-epsilon re-setq)))
         +null+)
        ((and (typep (re-stripped x) '(or re-epsilon re-setq))
              (typep (re-stripped y) 're-set))
         +null+)
        ((and (typep (re-stripped x) 're-epsilon)
              (not (re-nullable-p y)))
         +null+)
        ((and (typep (re-stripped y) 're-epsilon)
              (not (re-nullable-p x)))
         +null+)
        ||#
        ;; re-larger-p
        (t
         (re-cons-intersection x y))))

(defun re-intersection* (xs)
  (cond ((null xs)         +not-null+)
        ((null (cdr xs))   (car xs))
        ((null (cddr xs))  (re-intersection (car xs) (cadr xs)))
        (t                 (re-intersection (car xs) (re-intersection* (cdr xs))))))

(defun re-vector (&rest xs)
  (re-vector* xs))

(defun re-vector* (xs)
  (re-cons-vector xs))

(defun re-not (x)
  (cond ((typep x 're-not)              (lhs x))
        ((eq x +null+)                  +not-null+)
        ((eq x +not-null+)              +null+)
        ;;((typep x 're-or)               (re-intersection (re-not (lhs x)) (re-not (rhs x))))
        ;;((typep x 're-intersection)     (re-or (re-not (lhs x)) (re-not (rhs x))))
        (t                              (re-cons-not x))))

(defun re-grep (x y)
  ;; x is a vector of solutions we cling to
  ;; y is the thing to respawn
  (assert (re-vector-p x))
  (let ((xs (remove +null+ (re-vector-elements x))))
    (cond ((eq y +null+)
           (re-or* xs))
          (t
           (re-cons-grep (re-vector* xs) y)))))

(defun re-** (min max sub)
  (cond ((and (<= min 0) (eql max 0))  +epsilon+)
        ((and (eql min 1) (eql max 1))  sub)
        ;;((and (eql min 0) (eql max 1))  (re-or sub +epsilon+))
        ((eq sub +epsilon+)             +epsilon+)
        ((eq sub +null+)
         (if (<= min 0) +epsilon+ +null+))
        ((eq sub +not-null+)            +not-null+)
        (t
         (re-cons-** min max sub))))

'(defun re-** (min max sub)
  (cond ((and (<= min 0) (eql max 0))   +epsilon+)
        ((and (eql min 1) (eql max 1))  sub)
        ;; ((and (eql min 0) (eql max 1))  sub) ;;(re-or sub +epsilon+))
        ;; ((and (eql min 0) (eql max 1))  (re-or sub +epsilon+))
        ((eq sub +epsilon+)             +epsilon+)
        ((eq sub +null+)
         (if (<= min 0) +epsilon+ +null+))
        ((eq sub +not-null+)            +not-null+)
        (t
         (re-cons-** min max sub))))

;;; Tag assignments

(defun re-stripped-1 (re)
  (etypecase re
    (re-set            re)
    (re-and            (re-and (re-stripped (lhs re)) (re-stripped (rhs re))))
    (re-or             (re-or (re-stripped (lhs re)) (re-stripped (rhs re))))
    (re-conf           (re-conf* (mapcar #'re-stripped (re-vector-elements re))))
    (re-vector         (re-vector* (mapcar #'re-stripped (re-vector-elements re))))
    (re-intersection   (re-intersection (re-stripped (lhs re)) (re-stripped (rhs re))))
    (re-not            (re-not (re-stripped (lhs re))))
    (re-grep           (re-grep (re-stripped (lhs re)) (re-stripped (rhs re))))
    (re-setq           +epsilon+)
    (re-**             (re-** (re-**-min re) (re-**-max re) (re-stripped (re-**-sub re))))
    (re-null           re)
    (re-not-null       re)
    (re-epsilon        re)
    (re-epitext        (re-epitext (re-stripped (lhs re))))
    (re-pretext        (re-pretext (re-stripped (lhs re))))))

(defun re-all-variables (re)
  (re-%vars re))

(defun re-merge-setqs (then first)
  (cond ((eq then +epsilon+) first)
        ((eq first +epsilon+) then)
        (t (re-cons-setq (re-merge-setqs-1 (re-subst then) (re-subst first))))))

(defun re-merge-setqs-1 (then first)
  (cond ((null then) first)
        ((null first) then)
        ((var< (caar then) (caar first))
         (borrowing-cons then (car then) (re-merge-setqs-1 (cdr then) first)))
        ((var< (caar first) (caar then))
         (borrowing-cons first (car first) (re-merge-setqs-1 then (cdr first))))
        (t
         ;; then wins
         (borrowing-cons then (car then) (re-merge-setqs-1 (cdr then) (cdr first))))))


;;;; -- Derivative ----------------------------------------------------------------------------

(defparameter *k* nil
  "When non-null specifies the register to use for newly set variables.")

(defvar *new-vars-collector* nil
  "When bound to a CONS cell, all newly introduced variables are collected in its CAR.")

(defun re-nullable (re)
  (let ((%nullable (re-%nullable re)))
    (case %nullable
      (:unknown
       (let ((n (re-nullable-aux re)))
         (cond ((re-setq-free-p n)
                (setf (re-%nullable re) n))
               (t
                (setf (re-%nullable re) :compute)))
         n))
      (:compute
       (re-nullable-aux re))
      (otherwise
       %nullable))))

(defun re-nullable-aux (re)
  (etypecase re
    (re-epsilon         +epsilon+)
    (re-set             +null+)
    (re-and             (re-and (re-nullable (lhs re)) (re-nullable (rhs re))))
    (re-or              (let ((lhs* (re-nullable (lhs re))))
                          (cond ((or (typep lhs* 're-epsilon)
                                     (typep lhs* 're-setq))
                                 lhs*)
                                (t
                                 (re-or lhs* (re-nullable (rhs re)))))))
    (re-intersection    (re-intersection (re-nullable (lhs re)) (re-nullable (rhs re))))
    (re-not             (if (re-nullable-p (lhs re))
                            +null+
                            +epsilon+))
    (re-grep            (re-nullable (lhs re)))
    (re-conf            (re-nullable/conf re))
    (re-vector          (re-nullable (re-or* (re-vector-elements re))))
    (re-setq            #+(or)
                        (re-cons-setq (loop for x in (re-subst re)
                                            collect
                                            (cond ((and *k* (null (second x)))
                                                   (let ((old
                                                          (find-if (lambda (y)
                                                                     (and (eql (first y) (first x))
                                                                          (equal (third y) (third x))))
                                                                   (car *new-vars-collector*))))
                                                     (if old
                                                         (list (first old) (second old) (third old))
                                                         (let ((new (list (first x)
                                                                          (funcall *k*)
                                                                          (third x))))
                                                           (and *new-vars-collector*
                                                                (push new (car *new-vars-collector*)))
                                                           (list (first new) (second new) (third new))))))
                                                  (t
                                                   x))))
                        (re-cons-setq
                        (borrowing-mapcar (lambda (x)
                                            (cond ((and *k* (null (second x)))
                                                   (let ((old
                                                          (find-if (lambda (y)
                                                                     (and (eql (first y) (first x))
                                                                          (equal (third y) (third x))))
                                                                   (car *new-vars-collector*))))
                                                     (if old
                                                         (list (first old) (second old) (third old))
                                                         (let ((new (list (first x)
                                                                          (funcall *k*)
                                                                          (third x))))
                                                           (and *new-vars-collector*
                                                                (push new (car *new-vars-collector*)))
                                                           (list (first new) (second new) (third new))))))
                                                  (t
                                                   x)))
                                          (re-subst re))))
    (re-null            +null+)
    (re-not-null        +epsilon+)
    (re-**              (let ((min (re-**-min re))
                              (sub (re-**-sub re)))
                          (cond ((= min 0) +epsilon+)
                                ((> min 0) (re-nullable sub))
                                ((= -1 min)
                                 ;; Empty Match Longer than no Match
                                 ;;
                                 ;; POSIX has this rule that an empty match is preferred over no match at
                                 ;; all. That is, when facing an iteration the solution "iterate once
                                 ;; even if it's empty" is preferred over the solution to have r* match
                                 ;; \epsilon without diving into 'r'.
                                 ;;
                                 (re-or (re-nullable sub) +epsilon+))
                                (t (assert nil)))))
    (re-pretext         (re-nullable (lhs re)))
    (re-epitext         (re-or (re-nullable (lhs re))
                               re) )))

(defun re-nullable-p (re)
  (let ((%nullable (re-%nullable re)))
    (case %nullable
      (:unknown
       (not (eq +null+ (re-nullable re))))
      (:compute
       t)
      (otherwise
       (not (eq %nullable +null+))))))

(defun re-deriv (a re)
  (let* ((*k* (re-next-k re)))
    (cond ((eql a +end-of-file-sentinel+)
           ;; Hack, see comment about EOF handling. Basically it says, there
           ;; never is anything beyond EOF.
           (print(re-nullable (re-deriv-1 a re))))
          (t
           (re-deriv-1 a re)))))

(defvar *nest* nil)
(defvar *derive-count* 0)

(defun re-deriv-2 (a re)
  (re-deriv-1 a re))

(defun re-deriv-1 (a re)
  (declare (type re re))
  (incf *derive-count*)
  (etypecase re
    (re-and            (re-or (re-and (re-deriv-1 a (lhs re)) (rhs re))
                              (re-and (re-derive-epitext a (lhs re))
                                      (re-deriv-1 a (rhs re)))))
    (re-setq           +null+)
    (re-set            (if (isum-member a (re-isum re))
                           +epsilon+
                           +null+))
    (re-or             (re-or (re-deriv-1 a (lhs re))
                              (re-deriv-1 a (rhs re))))
    (re-**             (let ((min (re-**-min re))
                             (max (re-**-max re))
                             (inner (re-**-sub re)))
                         (cond ((and (integerp max) (<= max 0))
                                +null+)
                               (t
                                (re-and (re-deriv-1 a inner)
                                        (if (and (integerp max) (eql max 1))
                                            +epsilon+
                                            (re-** (if (integerp min) (max 0 (1- min)))
                                                   (if (integerp max) (max 0 (1- max)))
                                                   inner)))))))
    (re-epsilon        +null+)
    (re-conf           (re-deriv/conf a re))
    (re-intersection   (re-intersection (re-deriv-1 a (lhs re)) (re-deriv-1 a (rhs re))))
    (re-pretext        +null+)          ;<--------
    (re-epitext        +null+)
    (re-null           +null+)
    (re-not-null       +not-null+)
    (re-vector         (re-vector* (loop for x in (re-vector-elements re)
                                         collect (re-deriv-1 a x))))
    (re-not            (re-not (re-deriv-1 a (lhs re))))
    (re-grep           (grep-derive a re))))

'(defun re-deriv-1 (a re)
  (declare (type re re))
  (incf *derive-count*)
  (etypecase re
    (re-and            (re-or (re-and (re-deriv-1 a (lhs re)) (rhs re))
                              (re-and (re-derive-epitext a (lhs re))
                                      (re-deriv-1 a (rhs re)))))
    (re-setq           +null+)
    (re-set            (if (isum-member a (re-isum re))
                           +epsilon+
                           +null+))
    (re-or             (re-or (re-deriv-1 a (lhs re))
                              (re-deriv-1 a (rhs re))))
    (re-**             (let ((min (re-**-min re))
                             (max (re-**-max re))
                             (sub (re-**-sub re)))
                         (cond ((and max (<= max 0))
                                +null+)
                               ((eql max 1) 
                                +epsilon+)
                               ((eql min 0)
                                (let ((s (re-deriv-1 a sub)))
                                  (if (eq +epsilon+ (re-stripped s))
                                      +epsilon+
                                      (re-and s
                                              (re-** (max 0 (1- min)) (and max (1- max)) sub)))))
                               (t
                                (re-and (re-deriv-1 a sub)
                                        (re-** (max 0 (1- min)) (and max (1- max)) sub))))))
    (re-epsilon        +null+)
    (re-conf           (re-deriv/conf a re))
    (re-intersection   (re-intersection (re-deriv-1 a (lhs re)) (re-deriv-1 a (rhs re))))
    (re-pretext        +null+)          ;<--------
    (re-epitext        +null+)
    (re-null           +null+)
    (re-not-null       +not-null+)
    (re-vector         (re-vector* (loop for x in (re-vector-elements re)
                                         collect (re-deriv-1 a x))))
    (re-not            (re-not (re-deriv-1 a (lhs re))))
    (re-grep           (grep-derive a re))))

(defun grep-derive (a re)
  (let ((dl (re-deriv-1 a (lhs re))))
    (let ((cand (find-if #'(lambda (x) 
                             (not (eq +null+ (elide-epitext (re-nullable x)))))
                         (re-vector-elements dl))))
      (cond ((not (null cand))
             ;; Once any of the machines can match, we cling to it and don't grep
             ;; anymore. The trouble is: With epitext, we cannot tell anymore :-(
             cand)
            (t
             (grep-advance re dl))))))

(defun grep-advance (re dre)
  (let ((nx (remove +null+ (append (re-vector-elements dre)
                                   (list (grep-spawn (rhs re)))))))
    ;; cancel equal terms.
    (setf nx (reverse (loop for q on (reverse nx)
                            unless (find (re-stripped (car q)) (cdr q) :key #'re-stripped)
                            collect (car q))))
    (re-grep (re-vector* nx)
             (rhs re))))
    
(defun grep-spawn (re)
  re)

;;;;

(defun re-next-k (re)
  ;; this now assumes that all variables are prepended
  ;; It uses most of our time!
  (let ((set (re-k-set re)))
    (lambda ()
      (let* ((y 0)
             (x (loop for i from y until (not (logbitp i set)) finally (return i))))
        (setf set (logior set (ash 1 x)))
        (setf y (1+ x))
        x))))

(defun re-k-set (re)
  (let ((res 0))
    (loop for x in (re-all-variables re) do
         (when (integerp (second x))
           (setf res (logior res (ash 1 (the fixnum (second x))))))
         (when (integerp (first x))
           (setf res (logior res (ash 1 (the fixnum (first x)))))))
    res))


;;;; -- Character Classes ---------------------------------------------------------------------

(defun re-derivate-classes (re)
  (list* (isum-singleton +end-of-file-sentinel+)
         (isum-singleton +beginning-of-file-sentinel+)
         (isum-classes (re-first re))))

(defun re-first (re)
  (let ((res nil))
    (labels ((walk (re firstp)
               (etypecase re
                 (re-epsilon                  nil)
                 (re-null                     nil)
                 (re-not-null                 nil)
                 (re-set                      (when firstp
                                                (pushnew (re-isum re) res :test #'equal)))
                 (re-and                      (walk (lhs re) firstp)
                                              (walk (rhs re) (and firstp (re-nullable-p (lhs re)))))
                 (re-vector                   (loop for x in (re-vector-elements re) do (walk x firstp)))
                 ((or re-or re-intersection  re-grep)  (walk (lhs re) firstp) (walk (rhs re) firstp))
                 (re-not                      (walk (lhs re) firstp))
                 (re-**                       (walk (re-**-sub re) firstp))
                 (re-setq                     nil)
                 (re-epitext                  (walk (lhs re) firstp))
                 (re-pretext                  (walk (lhs re) t)))))
      (walk re t)
      res)))

(defun isum-classes (sets)
  ;; 'sets' is list of isums
  (let ((domain +isum-any-character+)) ; Just the characters, %EOF is
                                       ; handled otherwise, see note about
                                       ; EOF handling and trailing context.
    (let ((res (list domain))
          new)
      (loop for n in sets do
           (setf new nil)
           (loop for e in res do
                (let ((a (isum-intersection e n)))
                  (unless (isum-empty-p a)
                    (push a new)))
                (let ((a (isum-difference e n)))
                  (unless (isum-empty-p a)
                    (push a new))) )
           (setf res new))
      res)))


;;;; -- SRE -----------------------------------------------------------------------------------

;; Symbolic Regular Expressions

;; sre -> string
;;     -> character
;;     -> T                     any character
;;     -> (RANGE low high)
;;     -> (QUOTE string)
;;     -> (AND r1 .. rn)
;;     -> (OR r1 .. rn)
;;     -> (VECTOR r1 .. rn)
;;     -> (* r...)
;;     -> (? r...)
;;     -> (INTERSECTION r1 .. rn)
;;     -> (DIFFERNCE r s)
;;     -> (** min max s)
;;     -> (= var r...)
;;     -> (CI r...)
;;     -> (CS r...)
;;     -> (DELIMITED prefix matter stffix)

(defvar *sre-macros* nil
  "A-List of macro definitions to recognize in SRE-RE.")

(defvar *sre-seen-yet* nil
  "Used in SRE-RE to detect recursive macro definitions.")

(defvar *sre-case-insensitive-p* nil
  "Whether SRE-RE currently is case-insensitive. Please only bind this variable.")

(defun sre-re (sre)
  "Converts a symbolic regular expression to an RE struct."
  (%sre-re sre))

(defvar *scanning-mode-p*
  t)

(defvar *pretext-hack* nil)

(defun %sre-re (sre &aux it)
  "Converts a symbolic regular expression to an RE struct."
  (compiler-descend sre
    (cond ((stringp sre)
           (re-and* (map 'list #'%sre-re sre)))
          ((characterp sre)
           (if *sre-case-insensitive-p*
               (re-or (re-set (isum-singleton (char-code (char-upcase sre))))
                      (re-set (isum-singleton (char-code (char-downcase sre)))))
               (re-set (isum-singleton (char-code sre)))))
          ((integerp sre)
           (if (and (>= sre 0) (< sre char-code-limit))
               (re-set (isum-singleton sre))
               (progn
                 (compiler-warn sre "Character code out of range [0,#x~X)" char-code-limit)
                 +null+)))
          ((eql 't sre)
           (re-set (isum-range 0 char-code-limit)))
          ((eql :eol sre)
           (%sre-re '(:epitext (or %eof #\newline))))
          ((eql :bol sre)
           (%sre-re '(:pretext (* (and (* t) (or #\Newline %bof))))))
          ((eql :bof sre)
           (%sre-re (if *scanning-mode-p*
                        '(:pretext %bof)
                        '(:pretext ""))))
          ((member sre '(:bol :eol))
           (error "uh?"))
          ((eql :bow sre)
           (if *scanning-mode-p*
               (%sre-re '(and
                          (:pretext (or %bof
                                     (and
                                      (* t) (- t (<= #\a #\z) (<= #\A #\Z) (<= #\0 #\9) #\_))))
                          (:epitext (or (<= #\a #\z) (<= #\A #\Z) (<= #\0 #\9) #\_))
                          ))
               (%sre-re '(and
                          (:pretext (or "" (and (* t) (- t (<= #\a #\z) (<= #\A #\Z) (<= #\0 #\9) #\_))))
                          (:epitext (or (<= #\a #\z) (<= #\A #\Z) (<= #\0 #\9) #\_))
                          ))))
          ((eql :eow sre)
           (if *scanning-mode-p*
               (%sre-re '(and
                          (:pretext (and (or %bof t) (* t) (or (<= #\a #\z) (<= #\A #\Z) (<= #\0 #\9) #\_)))
                          (:epitext (or %eof (- t (<= #\a #\z) (<= #\A #\Z) (<= #\0 #\9) #\_)))
                          ;; (:epitext (or #\space %eof))
                          ))
               (%sre-re '(and
                          (:pretext (and (* t) (or (<= #\a #\z) (<= #\A #\Z) (<= #\0 #\9) #\_)))
                          (:epitext (or %eof (- t (<= #\a #\z) (<= #\A #\Z) (<= #\0 #\9) #\_)))
                          ;; (:epitext (or #\space %eof))
                          ))))
          ((eql :nwb sre)
           ;; not a word boundary
           (%sre-re '(or
                     (and
                      (:pretext (or "" (and (* t) (- t (<= #\a #\z) (<= #\A #\Z) (<= #\0 #\9) #\_))))
                      (:epitext (or %eof (- t (<= #\a #\z) (<= #\A #\Z) (<= #\0 #\9) #\_))))
                     (and
                      (:pretext (and (* t) (or (<= #\a #\z) (<= #\A #\Z) (<= #\0 #\9) #\_))) 
                      (:epitext (or (<= #\a #\z) (<= #\A #\Z) (<= #\0 #\9) #\_))))))
                      
          ((eql '%eof sre) (re-set (isum-singleton +end-of-file-sentinel+)))
          ((eql '%bof sre) (re-set (isum-singleton +beginning-of-file-sentinel+)))
          ((eql :eof sre)  (%sre-re '(:epitext %eof)))
          ((setf it (assoc sre *sre-macros*))
           (cond ((member sre *sre-seen-yet*)
                  (error "Recursive SRE macro ~S encountered." sre))
                 (t
                  (let ((*sre-seen-yet* (cons sre *sre-seen-yet*)))
                    (%sre-re (cdr it))))))
          ((symbolp sre)
           (compiler-warn sre "Undefined SRE macro ~S." sre)
           +null+)
          ((atom sre)
           (compiler-warn sre "Bad SRE.")
           +null+)
          ((not (symbolp (car sre)))
           (compiler-warn sre "Bad SRE.")
           +null+)
          (t
           (macrolet ((frob (&rest clauses)
                        `(cond
                           ,@(loop for (funs params . body) in clauses collect
                                  `((or ,@(loop for fun in (if (listp funs) funs (list funs))
                                             collect `(string-equal (car sre) ',fun)))
                                    ;; ### how to catch destructuring-bind error?
                                    (destructuring-bind ,params (cdr sre)
                                      ,@body)))
                           (t
                            (compiler-warn sre "Unknown symbolic regular expression operator ~S."
                                           (car sre))
                            +null+))))
             (frob
              (RE (string)
                  (parse-re string))
              ((RANGE <=) (from to)
               (and (stringp from) (= 1 (length from)) (setf from (char from 0)))
               (and (stringp to) (= 1 (length to)) (setf to (char to 0)))
               (cond ((and (integerp from) (integerp to))
                      (let (res)
                        (unless (< -1 from char-code-limit)
                          (compiler-warn sre "Lower bound character code out of range [0,#x~X)" char-code-limit)
                          (setf res +null+))
                        (unless (< -1 to char-code-limit)
                          (compiler-warn sre "Upper bound character code out of range [0,#x~X)" char-code-limit)
                          (setf res +null+))
                        (when (> from to)
                          (compiler-warn sre "Lower bound cannot be larger than upper bound.")
                          (setf res +null+))
                        (or res
                            (re-set (isum-range from (1+ to))))))
                     ((not (and (characterp from) (characterp to)))
                      (compiler-warn sre "Arguments to ~S must be either both chracters or both integers." 'range)
                      +null+)
                     ((char> from to)
                      (compiler-warn sre "Lower bound cannot be larger than upper bound.")
                      +null+)
                     (*sre-case-insensitive-p*
                      (re-or (re-set (isum-range (char-code (char-downcase from)) (1+ (char-code (char-downcase to)))))
                             (re-set (isum-range (char-code (char-upcase from)) (1+ (char-code (char-upcase to)))))))
                     (t
                      (re-set (isum-range (char-code from) (1+ (char-code to)))))))
              ;;
              ((AND) (&rest more)       (re-and* (mapcar #'%sre-re more)))
              ((OR) (&rest more)        (re-or* (mapcar #'%sre-re more)))
              ((VECTOR) (&rest more)    (re-vector* (mapcar #'%sre-re more)))
              ((CONF) (&rest more)      (re-conf* (mapcar #'%sre-re more)))
              ;; Iterations
              ((*) (&rest more)         (%sre-re `(** 0 nil ,@more)))
              ((+) (&rest more)         (re-** 1 nil (re-prepend-setqs (re-and* (mapcar #'%sre-re more)))))
              ;;((?) (&rest more)         (%sre-re `(or (and ,@more) (and))))
              ((?) (&rest more)         (%sre-re `(** 0 1 ,@more)))
              ((**) (min max &rest more)
               (cond ((not (typep min '(integer 0 *)))
                      (compiler-warn sre "The minimum iteration count must be a non-negative integer.")
                      +null+)
                     ((not (typep max '(or (integer 0 *) (member t * nil))))
                      (compiler-warn sre "The maximium iteration count must be a non-negative integer or T or *.")
                      +null+)
                     ((and (integerp max) (< max min))
                      (compiler-warn sre "The maximium iteration count must not be smaller than ~
                                         the minimum iteration count.")
                      +null+)
                     ((and (= 0 min) (eql 1 max))
                      (%sre-re `(or (and ,@more) (and))))
                     ((= 0 min)
                      (setq max (subst nil '* max))
                      (re-** -1 max (re-prepend-setqs (re-and* (mapcar #'%sre-re more)))))
                     (t
                      (setq max (subst nil '* max))
                      (re-** min max (re-prepend-setqs (re-and* (mapcar #'%sre-re more)))))))
              ;;
              #||
              ((CNOT) (sub)             ;### disallow
               (let ((sub (%sre-re sub)))
                 (assert (typep sub 're-set))
                 (re-set (isum-complement (re-isum sub)))))
              ;;
              ((NOT) (&rest more)       ;### disallow
               (re-not (re-and* (mapcar #'%sre-re more))))
              ;;
              ((INTERSECTION) (first &rest more)
               (re-intersection* (mapcar #'%sre-re (cons first more))))
              ||#
              ;;
              ((DIFFERENCE -) (domain &rest subtrahends)
               (let ((a (%sre-re domain))
                     (b (%sre-re `(or ,@subtrahends))))
                 ;; ### handle in RE itself.
                 (cond ((and (re-set-p a) (re-set-p b))
                        (re-set (isum-difference (re-isum a) (re-isum b))))
                       (t
                        (re-intersection a (re-not b)))))) ;### rather (- (* t) b)?
              ;;
              
              ;;
              ((=) (id &rest more)
               (re-and (re-and (re-setq (tag-start-reg id))
                               (re-and* (mapcar #'%sre-re more)))
                       (re-setq (tag-end-reg id))))
              ;;
              ((SETQ) (&rest more)
               ;; We intentionally use RE-AND here to let RE-MERGE-SETQS sort
               ;; our tag list.
               (let ((res +epsilon+))
                 (loop for v in more do
                      (setq res (re-and (re-setq v) res)))
                 res))
              ;;
              ((CI) (&rest more)
               (let ((*sre-case-insensitive-p* t))
                 (re-and* (mapcar #'%sre-re more))))
              ;;
              ((CS) (&rest more)
               (let ((*sre-case-insensitive-p* nil))
                 (re-and* (mapcar #'%sre-re more))))
              ;;
              ((:pretext) (&rest more)
               (if *pretext-hack*
                   (re-pretext (%sre-re `(and t ,@more)))
                   (re-pretext (%sre-re `(and ,@more)))))
              ((:epitext) (&rest more) (re-epitext (%sre-re `(and ,@more))))
              ;; short hands
              (DELIMITED (prefix matter suffix)
                         (%sre-re `(and ,prefix (- ,matter (and (* t) ,suffix (* t))) ,suffix)))
              (CONTAINS (matter)
                        (%sre-re `(and (* t) ,matter (* t))))))))))

(defun create-grep (x)
  (if (re-nullable-p (elide-epitext x))
      x
      (re-grep (re-vector* (list x)) x)))

(defun re-sre (re)
  "Converts a regular expression (RE) to an equivalent symbolic regular expression (SRE)."
  (etypecase re
    (re-epsilon         "")
    (re-null            '(or))
    (re-not-null        '(intersection))
    (re-set
     (let* ((charset  (isum-intersection (re-isum re) (isum-range 0 char-code-limit)))
            (bof?     (isum-member +beginning-of-file-sentinel+ (re-isum re)))
            (eof?     (isum-member +end-of-file-sentinel+ (re-isum re)))
            (charset-sre-list
             (append
              (cond ((equal charset +isum-any-character+)
                     (list 't))
                    ((and charset (null (car charset)))
                     ;; complement
                     (list `(- t ,(re-sre (make-re-set 0 (cdr charset))))))
                    ;; Heuristic, a set which is not everything, but still includes NUL is a complement
                    ((isum-member 0 charset)
                     (list `(- t ,(re-sre (make-re-set 0 (isum-difference +isum-any-character+ charset))))))
                    (t
                     (loop for (from below) on charset by #'cddr collect
                           (cond ((= below (1+ from))
                                  (kode-char from))
                                 (t
                                  `(<= ,(kode-char from) ,(kode-char (1- below))))))))
              (if bof? (list '%bof))
              (if eof? (list '%eof)))))
       (cond ((= 0 (length charset-sre-list)) '(or))
             ((= 1 (length charset-sre-list)) (car charset-sre-list))
             (t `(or ,@charset-sre-list)))))
    ;;
    (re-or              (let ((x (re-sre (lhs re)))
                              (y (re-sre (rhs re))))
                          (cond ((and (consp x) (eq 'or (car x)))
                                 (cons 'or (append (cdr x) (list y))))
                                (t
                                 (list 'or x y)))))
    (re-intersection    (let ((x (re-sre (lhs re)))
                              (y (re-sre (rhs re))))
                          (cond ((and (consp x) (eq 'intersection (car x)))
                                 (cons 'intersection (append (cdr x) (list y))))
                                (t
                                 (list 'intersection x y)))))
    (re-not             (list 'not (re-sre (lhs re))))
    (re-grep            (list 'grep (re-sre (lhs re)) (re-sre (rhs re))))
    (re-conf            (cons 'conf (mapcar #'re-sre (re-vector-elements re))))
    (re-vector          (cons 'vector (mapcar #'re-sre (re-vector-elements re))))
    (re-setq            `(setq ,@(re-subst re)))
    (re-and             (cond ((eq re +epsilon+)
                               "")
                              (t
                               (let ((x (re-sre (lhs re)))
                                     (y (re-sre (rhs re))))
                                 (cond ((and (or (characterp x) (stringp x))
                                             (or (characterp y) (stringp y)))
                                        (concatenate 'string (string x) (string y)))
                                       ((and (consp y) (eq (car y) 'and))
                                        (cond ((and (or (characterp x) (stringp x))
                                                    (or (characterp (second y)) (stringp (second y))))
                                               (list* 'and (concatenate 'string (string x) (string (second y)))
                                                      (cddr y)))
                                              (t
                                               (list* 'and x (cdr y)))))
                                       (t
                                        (list 'and x y)))))))
    (re-**              (list '** (re-**-min re) (re-**-max re) (re-sre (re-**-sub re))))
    (re-pretext         `(:pretext ,(re-sre (lhs re))))
    (re-epitext         `(:epitext ,(re-sre (lhs re)))) ))

(defun kode-char (code)
  (cond ((eql code +end-of-file-sentinel+) '%eof)
        ((eql code +beginning-of-file-sentinel+) '%bof)
        (t (code-char code))))


;;;; -- Traditional Syntax Parser -------------------------------------------------------------

(defvar *default-re-syntax*
  :extended)

(defun parse-ere (string &optional basicp)
  (parse-re string :syntax (if basicp :basic :extended)))


;;;; -- DFA construction ----------------------------------------------------------------------

(defstruct dfa
  ;; Vector of all states
  (states #() :type simple-vector)
  ;; List of all output registers as seen with RE-SETQs
  (output-registers nil :type (or (member :unspecified) list)) 
  ;; Whether this DFA dispatches on a condition as used by lexers.
  (has-condition-dispatch nil)
  ;; Whether this DFA dispatches on an extra single character lookbehind
  ;; (which also may be the %BOF sentinel).
  (has-lookahead nil))

(defstruct dfa-state
  (transitions nil :type list)
  re)

(defstruct transition
  (from 0 :type fixnum)
  sigma                                 ;either an ISUM or ':EXIT
  (to 0 :type fixnum)
  (subst nil :type list))

(defun dfa-copy (dfa)
  (dfa-deep-copy dfa))

(defun dfa-deep-copy (dfa)
  (setq dfa (copy-dfa dfa))
  (setf (dfa-states dfa)
        (map 'vector (lambda (q)
                       (setq q (copy-dfa-state q))
                       (setf (dfa-state-transitions q)
                             (mapcar #'copy-transition (dfa-state-transitions q)))
                       q)
             (dfa-states dfa)))
  dfa)

(defun dfa-all-states (dfa)
  (loop for i below (length (dfa-states dfa)) collect i))

(defun dfa-n-states (dfa)
  (length (dfa-states dfa)))

(defun dfa-state (dfa i)
  (if (dfa-state-p i) i (elt (dfa-states dfa) i)))

(defun map-dfa-states (dfa fun)
  (map nil fun (dfa-states dfa)))

(defun map-dfa-transitions (dfa fun)
  "Applies the function /fun/ to all the transitions of the DFA /dfa/."
  (map-dfa-states dfa (lambda (q) (map nil fun (dfa-state-transitions q)))))

(defun dfa-transit (dfa q code)
  ;; -> q' subst ; or NIL
  (let ((tr (find code (dfa-state-transitions (aref (dfa-states dfa) q))
                  :test #'sigma-member :key #'transition-sigma)))
    (and tr
         (values (transition-to tr)
                 (transition-subst tr)))))

(defun sigma-member (item sigma)
  (if (symbolp sigma)
      (eq item sigma)
      (and (integerp item) (isum-member item sigma))))

(defun dfa-state-predecessors (dfa q &aux res)
  "Returns a list of all transitions in the DFA /dfa/, which lead to the state /q/."
  (let ((i (if (integerp q) q (position q (dfa-states dfa)))))
    (map-dfa-transitions dfa (lambda (tr)
                               (when (eql (transition-to tr) i)
                                 (push tr res))))
    res))

(defun dfa-state-successors (dfa q &aux res)
  "Returns a list of all states in the DFA /dfa/, which are directly reached from state /q/."
  (let ((i (if (integerp q) q (position q (dfa-states dfa)))))
    (map-dfa-transitions dfa (lambda (tr)
                               (when (eql (transition-from tr) i)
                                 (pushnew (transition-to tr) res))))
    res))

(defun dfa-state-exit-transition (dfa state)
  (find :exit (dfa-state-transitions (if (integerp state) (aref (dfa-states dfa) state) state))
        :key #'transition-sigma))

(defun dfa-all-registers (dfa &aux res)
  (loop for q across (dfa-states dfa) do
       (loop for tr in (dfa-state-transitions q) do
            (loop for (d . s) in (transition-subst tr) do
                 (pushnew d res)
                 (and (atom s) (pushnew s res)))))
  (remove 'cur-bol (remove 'cur-line (remove 'p res))))

#+(or)
(defun dfa-all-output-registers (dfa &aux res)
  (loop for q below (length (dfa-states dfa)) do
       (let ((exit (dfa-state-exit-transition dfa q)))
         (when exit
           (loop for ds in (transition-subst exit) do
                (when (not (integerp (car ds)))
                  (pushnew (car ds) res))))))
  res)

#-(or)
(defun dfa-all-output-registers (dfa)
  (dfa-output-registers dfa))

(defun subst-compose (first then)
  "Given two substitions /first/ and /then/ given as a-lists, returns the composition."
  (cond ((null first) then)
        ((null then) first)
        (t
         (let ((res nil))
           (loop for v in then do
                (let ((it (assoc (cdr v) first :test #'eq)))
                  (if it
                      (push (cons (car v) (cdr it)) res)
                      (push v res))))
           (loop for v in first do
                (unless (assoc (car v) then :test #'eq)
                  (push v res)))
           res))))

(defvar *state-count* 0
  "Purely for statistics: The total number of states generated.")

(defvar *re*)

(defun re-dfa (re &key (output-registers :unspecified)
                       (has-condition-dispatch (error "bust"))
                       (has-lookahead (error "bust")))
  ;; State 0 always is the initial state
  ;; State 1 always is the fail state (+null+)
  ;; State 2 always is the imaginary exit state (:EXIT transitions use these).
  (setf re (con-init re))
  (setq *re* re)
  ;;
  (let ((todo nil)
        (starts nil)
        (state-vector (make-array 0 :adjustable t :fill-pointer 0))
        (my-names (make-hash-table :test #'eq))
        (alter-ego-hash (make-hash-table :test #'eq)))
    (labels
        ((add-transition* (from sigma to &optional subst)
           (let ((state (aref state-vector from)))
             (push (make-transition :from from :sigma sigma :to to :subst subst)
                   (dfa-state-transitions state))))
         ;;
         (add-transition (from sigma to &optional subst)
           (add-transition* (name from) sigma (name to) subst))
         ;;
         (add-exit-transition (from &optional subst)
           (add-transition* (name from) :exit 2 subst))
         ;;
         (name (re)
           (or (gethash re my-names)
               (prog1
                   (setf (gethash re my-names) (length state-vector))
                 (when (re-p re)
                   (push re (gethash (re-stripped re) alter-ego-hash))
                   (push re todo))
                 (vector-push-extend (make-dfa-state :re re) state-vector))))
         ;;
         (alter-ego (re)
           ;; -> re* setqs
           (cond ((gethash re my-names)
                  (values re nil))
                 (t
                  (let* ((r2 (re-stripped re))
                         (candidates (gethash r2 alter-ego-hash)))
                    (cond ((member re candidates) (values re nil))
                          (t
                           (dolist (alter-ego candidates)
                             (multiple-value-bind (can-do-p subst)
                                 (reducible re alter-ego)
                               (when can-do-p
                                 (return-from alter-ego
                                   (values alter-ego subst)))))))))))
         ;;
         (collect-vars-2 (re)
           ;; ### this returns multiple assignments in case of INTERSECTIONS
           (mapcar (lambda (v)
                     (cons (first v) (or (second v) (third v))))
                   (re-all-variables re))) )
      ;; (push (name re) starts)           ;make the start state state 0
      (name 'q0) (pop todo)
      (name +null+)  (pop todo)          ;make the fail state state 1
      (name nil) (pop todo)              ;make the exit state 2
      (push (name re) starts)
      ;;
      ;; ----
      (do () ((null todo))
        (let ((re (pop todo)))
          (dolist (a (re-derivate-classes re))
            (let* ((*new-vars-collector* (list nil))
                   (re-next (con-deriv (isum-witness a) re))
                   (new-vars (car *new-vars-collector*))
                   (all-vars (re-all-variables re-next))
                   (setq
                    (loop for v in new-vars
                          when (member (second v) all-vars :key #'second)
                          collect (cons (second v) (third v)))))
              (setf setq (remove-duplicates setq :key #'car))
              (multiple-value-bind (alter-ego subst) (alter-ego re-next)
                (when alter-ego
                  (setq re-next alter-ego)
                  (setq setq (subst-compose setq subst)))
                ;; Do not bother to add transitions to the fail state
                (unless (con-jam-p re-next)
                  (add-transition re a re-next setq)))))
          ;; Add an exit transition, if needed
          (let* ((*k* (re-next-k re))
                 (*new-vars-collector* (list nil))
                 (n (con-nullable re)))
            (unless (eq +null+ n)
              (let* ((new-vars (car *new-vars-collector*))
                     (setq (remove-duplicates (mapcar #'(lambda (v)
                                                          (cons (second v) (third v)))
                                                      new-vars)
                                              :key #'car))
                     (collect (remove-duplicates (collect-vars-2 n) :key #'car)))
                (let ((su (loop for ds in (subst-compose setq collect)
                             when (find (car ds) collect :key #'first)
                             collect ds)))
                  (add-exit-transition re su)))))))
      ;; ----
      (let ((dfa (make-dfa :states (copy-seq state-vector) ;coerce it to a simple vector
                           :output-registers output-registers
                           :has-condition-dispatch has-condition-dispatch
                           :has-lookahead has-lookahead
                           )))
        ;; add pseudo dispatch
        (loop for q in starts do
             (add-transition* 0 (isum-singleton q) q nil))
        ;;
        (multiple-value-prog1
            dfa
          (when *re-dfa-scanner-verbose*
            (format t "~&;; ~%")
            (format t "~&;; Statistics:~%")
            (format t "~&;; ~5D states.~%" (length (dfa-states dfa)))
            (format t "~&;; ~5D transitions.~%"
                    (loop for q across state-vector sum (1+ (length (dfa-state-transitions q)))))))))))

(defun reducible (new old)
  "-> successp ; subst
   Given the two REs /new/ and /old/ returns a substitution on the tags, which
   when applied to /new/ would make it equal to /old/. If any such
   substition exists."
  ;; Note that this operation is not reflexive.
  (let ((subst nil))                ;map from old registers to new registers
    (labels ((walk-tag (n o)
               (cond ((or (null (cadr n)) (null (cadr o)))
                      ;; yet about to be assigned tags must be equal.
                      (equal n o))
                     ((and (eql (car n) (car o)) ;tags are equal
                           (or (not (eq (car n) 'cat))
                               (equal (third n) (third o)))) ;values are equal [**1]
                      (let ((o* (cdr (assoc (second o) subst))))
                        (cond ((null o*)
                               (push (cons (second o) (second n)) subst)
                               t)
                              (t
                               (eql o* (second n))))))))
             ;;
             (walk (new old)
               (and (eq (type-of new) (type-of old))
                    (etypecase new
                      (RE-SETQ
                       (do ((p (re-subst new) (cdr p))
                            (q (re-subst old) (cdr q)))
                           ((or (null p) (null q)
                                (not (walk-tag (car p) (car q))))
                            (and (null p) (null q)))))
                      ((or RE-EPSILON RE-NULL RE-NOT-NULL)
                       t)
                      (RE-SET
                       (eq new old))
                      (RE-BINOP
                       (and (walk (lhs new) (lhs old))
                            (walk (rhs new) (rhs old))))
                      (RE-UNOP
                       (walk (lhs new) (lhs old)))
                      (RE-VECTOR
                       (do ((p (re-vector-elements new) (cdr p))
                            (q (re-vector-elements old) (cdr q)))
                           ((or (null p) (null q)
                                (not (walk (car p) (car q))))
                            (and (null p) (null q)))))
                      (RE-**
                       (and (eql (re-**-min new) (re-**-min old))
                            (eql (re-**-max new) (re-**-max old))
                            (walk (re-**-sub new) (re-**-sub old))))))))
      (declare (inline walk-tag))
      (values
       (walk new old)
       (delete-if (lambda (x) (eql (car x) (cdr x))) subst))))

  ;; [1] Actually this requirement could be dropped. But: We have it here,
  ;; so that CAT when assigned always turns out to be assigned to a
  ;; constant.
  )


;;;; ------------------------------------------------------------------------------------------

(defvar *outrule-empty-match-p* t
  "Whether to intersect the overall resulting expression
  with (or (and (+ t) (? :eof)) :eof). Must be set to non-NIL for
  scanners.")

;;;; -- Pretext and Epitext -------------------------------------------------------------------

(defun re-pretext (sub)
  (cond ((eq +null+ sub) +null+)
        (t
         (re-cons-unop 6 re-pretext make-re-pretext sub 1))))

(defun re-epitext (sub)
  ;; Hmm, rather it is that if 'sub' is nullable, we cave in and return +epsilon+.
  ;; (print (list sub (re-nullable-p sub)))
  (typecase sub
    (re-epsilon sub)
    (re-setq    sub)
    (re-null    sub)
    (t          (re-cons-unop 7 re-epitext make-re-epitext sub 2))))


;;;

(defun re-derive-pretext (a re)
  "Derives all RE-PRETEXT nodes in `re', but leaves everything else intact."
  (cond ((zerop (logand 1 (re-%has-pretext re)))
         re)
        (t
         (when (characterp a) (setq a (char-code a)))
         (etypecase re
           (re-pretext          (re-pretext (re-derive-pretext a (re-deriv-1 a (lhs re)))))
           (re-epitext          re)
           ;;
           (re-and              (re-and (re-derive-pretext a (lhs re)) (re-derive-pretext a (rhs re))))
           (re-setq             re)
           (re-set              re)
           (re-or               (re-or (re-derive-pretext a (lhs re)) (re-derive-pretext a (rhs re))))
           (re-intersection     (re-intersection (re-derive-pretext a (lhs re)) (re-derive-pretext a (rhs re))))
           (re-epsilon          re)
           (re-null             re)
           (re-not-null         re)
           ;;
           (re-conf             (re-conf* (mapcar (lambda (s) (re-derive-pretext a s)) (re-vector-elements re))))
           ;;
           (re-vector           (re-vector* (mapcar (lambda (s) (re-derive-pretext a s)) (re-vector-elements re))))
           (re-not              (re-not (re-derive-pretext a (lhs re))))
           (re-**               (re-** (re-**-min re) (re-**-max re) (re-derive-pretext a (re-**-sub re))))
           ;; ???
           (re-grep             (re-grep (re-derive-pretext a (lhs re)) (re-derive-pretext a (rhs re)))) 
           ))))

(defun re-derive-epitext (a re)
  (cond ((zerop (logand 2 (re-%has-pretext re)))
         (re-nullable re))
        (t
         (etypecase re
           (re-epsilon          +epsilon+)
           (re-setq             (re-nullable re))
           (re-set              +null+)
           (re-and              (re-and (re-derive-epitext a (lhs re))
                                        (re-derive-epitext a (rhs re))))
           (re-or               (re-or (re-derive-epitext a (lhs re))
                                       (re-derive-epitext a (rhs re))))
           (re-pretext          (re-derive-epitext a (lhs re)))
           (re-epitext          (re-or (re-epitext (re-deriv-1 a (lhs re)) )
                                       (re-nullable (lhs re))))
           (re-vector           (re-vector* (mapcar (lambda (r) (re-derive-epitext a r)) (re-vector-elements re))))
           (re-null             +null+)
           (re-not-null         +epsilon+)
           (re-**               (let ((min (re-**-min re))
                                      (sub (re-**-sub re)))
                                  (if (and (integerp min) (> min 0))
                                      (re-derive-epitext a sub)
                                      +epsilon+)))
           (re-intersection     (re-intersection (re-derive-epitext a (lhs re))
                                                 (re-derive-epitext a (rhs re))))
           (re-not              (if (re-nullable-p (lhs re))
                                    +null+
                                    +epsilon+))))))

(defun elide-epitext (re)
  (if (zerop (logand 2 (re-%has-pretext re)))
      re
      (etypecase re
        (re-pretext             re)
        (re-epitext             +null+)
        ;;
        (re-and                 (re-and (elide-epitext (lhs re)) (elide-epitext (rhs re))))
        (re-setq                re)
        (re-set                 re)
        (re-or                  (re-or (elide-epitext (lhs re)) (elide-epitext (rhs re))))
        (re-intersection        (re-intersection (elide-epitext (lhs re)) (elide-epitext (rhs re))))
        (re-epsilon             re)
        (re-null                re)
        (re-not-null            re)
        ;;
        (re-conf                (re-conf* (mapcar (lambda (s) (elide-epitext s))
                                                  (re-vector-elements re))))
        ;;
        (re-vector              (re-vector* (mapcar (lambda (s) (elide-epitext s))
                                                    (re-vector-elements re))))
        (re-not                 (re-not (elide-epitext (lhs re))))
        (re-**                  (re-** (re-**-min re) (re-**-max re) (elide-epitext (re-**-sub re))))
        ;; ???
        (re-grep                (re-grep (elide-epitext (lhs re)) (elide-epitext (rhs re)))) )))

;;;; -- Configuration I -----------------------------------------------------------------------

(defun re-cons-conf (conf)
  "Low level regular expression cons, should only be called via RE-CONF."
  (re-cons-aux :hash (progn
                       (hash-step 6)
                       (loop for x in conf do (hash-step (the fixnum (re-name x)))))
               :tester (lambda (item)
                         (and (typep item 're-conf)
                              (do ((p (re-vector-elements item) (cdr p))
                                   (q conf (cdr q)))
                                  ((or (null p) (null q)) (eq p q))
                                (unless (eq (car p) (car q))
                                  (return nil)))))
               :cons (let ((r (make-re-conf (incf (re-hash-next-name hash-table)) conf)))
                       (let ((vars nil)
                             (has-pretext 0))
                         (declare (type fixnum has-pretext))
                         (loop for item in conf do
                               (setf vars (%re-merge-vars vars (re-%vars item)))
                               (setf has-pretext (logior has-pretext (re-%has-pretext item))))
                         (setf (re-%vars r) vars
                               (re-%has-pretext r) has-pretext))
                       r)))

(defun re-conf (re &optional (stash (re-vector)))
  (re-conf* (list re stash)))

(defun re-conf* (xs)
  (cond #-NIL
        ((and (= 2 (length xs))
              (eq +null+ (car xs))
              (re-vector-p (cadr xs))
              (null (re-vector-elements (cadr xs))))
         +null+)
        ;; Ok. A configuration with a +null+ remaining RE and no epitext at the
        ;; stash, is as good as the stash.
        #-NIL
        ((and (= 2 (length xs))
              ;; (eq +null+ (car xs))
              (eq (elide-epitext (car xs)) (car xs))
              (re-vector-p (cadr xs))
              (every (lambda (s) (eq (elide-epitext s) s)) (re-vector-elements (cadr xs))))
         (re-or* (cons (car xs) (re-vector-elements (cadr xs)))))
        (t
         (re-cons-conf xs))))


;;;; -- Configurations ------------------------------------------------------------------------

(defun con-init (re)
  (let* (
         (re
          (cond ((parsed-lexer-p re)
                 (con-init/lexer re))
                (t
                 (let ((re (re re)))
                 (if *scanning-mode-p*
                     (re-conf
                      (re-and (re-or (re-set (isum-singleton +beginning-of-file-sentinel+))
                                     (sre-re 't))
                              re)
                      (re-vector))
                     (create-grep (re-conf re (re-vector)))))))))
    (re-vector re +null+ +null+)))

(defun con-deriv (a re &optional (*k* (re-next-k re)))
  (if (characterp a) (setq a (char-code a)))
  (destructuring-bind (re stash eof?) (re-vector-elements re)
    (let ((re*
           (if (eq +epsilon+ eof?)
               +null+
               (re-derive-pretext a (re-deriv-1 a re)))))
      (cond ((and (re-vector-p re*)
                  (not (find +null+ (re-vector-elements re*) :test-not 'eq)))
             (setq re* +null+)))
      (re-vector re*
                 (re-or (re-nullable re) stash)
                 (re-or eof? (if (eql a +end-of-file-sentinel+) +epsilon+ +null+))
                 ))))

(defun con-nullable (re)
  (destructuring-bind (re stash eof?) (re-vector-elements re)
    (declare (ignore eof?))
    (re-or (re-nullable re) stash)))

(defun con-jam-p (re)
  (destructuring-bind (re stash eof?) (re-vector-elements re)
    (declare (ignorable stash eof?))
    (eq +null+ re)))

(defun re (thing)
  (cond ((re-p thing) thing)
        ((stringp thing) (sre-re (parse-re thing)))
        ((listp thing) (sre-re thing))
        (t (error "I have no idea how to turn ~S into a regular expression." thing))))

(defun re-deriv/conf (a re)
  (destructuring-bind (re stash) (re-vector-elements re)
    (let ((dre (re-deriv-1 a re))
          (stash*
           (remove +null+
                   (cons (re-derive-epitext a (re-nullable re))
                         (mapcan (lambda (s)
                                   (let ((s* (re-derive-epitext a s)))
                                     (and (not (eq s s*)) (list s*))))
                                 (re-vector-elements stash))))))
      (re-conf dre (re-vector* stash*)))))

(defun re-nullable/conf (re)
  (destructuring-bind (re stash) (re-vector-elements re)
    (declare (ignorable re))
    (loop for s in (progn ;;cons (re-nullable re)
                         (re-vector-elements stash))
          do
          (let ((s* (elide-epitext s)))
            (unless (eq s* +null+)
              (return s*)))
          finally (return +null+))))


;;;; -- Data Flow Analysis --------------------------------------------------------------------

(defun dataflow-analysis (dfa &key (direction :forward)
                                gen
                                kill
                                (initial-value nil)
                                (transfer-function
                                 #'(lambda (new old)
                                     (union (funcall gen new)
                                            (set-difference old (funcall kill new) :test #'equal)
                                            :test #'equal)))
                                (meet #'(lambda (x y) (union x y :test #'equal)))
                                (compare #'(lambda (x y) (set-equal x y :test #'equal))))
  ;; Our blocks are the transitions
  (let ((succ-hash (make-hash-table :test #'eq))
        (pred-hash (make-hash-table :test #'eq))
        (all-blocks nil))
    (labels ((succs (b)
               (memo (succ-hash b)
                     (dfa-state-transitions (dfa-state dfa (transition-to b)))))
             (preds (b)
               (gethash b pred-hash))
             (all-blocks ()
               (or all-blocks
                   (setf all-blocks
                         (let ((yet nil) (yet-q nil))
                           (labels ((walk (q)
                                      (unless (member q yet-q)
                                        (push q yet-q)
                                        (loop for tr in (dfa-state-transitions q) do
                                             (push tr yet)
                                             (walk (dfa-state dfa (transition-to tr)))))))
                             (walk (dfa-state dfa 0))
                             (if (eq direction :backward)
                                 yet
                                 (reverse yet))))))))
      (let ((in (make-hash-table :test #'eq))
            (out (make-hash-table :test #'eq)))
        (ecase direction
          (:backward
           ;;
           (loop for b in (all-blocks) do
                (setf (gethash b in) initial-value))
           ;;
           (let ((changep t))
             (loop while changep do
                  (setf changep nil)
                  (loop for b in (all-blocks) do
                       (setf (gethash b out)
                             (reduce meet (loop for s in (succs b) collect (gethash s in))))
                       (let ((new
                              (funcall transfer-function b (gethash b out))))
                         (unless (funcall compare new (gethash b in))
                           (setf changep t
                                 (gethash b in) new)))))))
          (:forward
           ;;
           (loop for b in (all-blocks) do
                (setf (gethash b out) initial-value))
           ;;
           (map-dfa-transitions
            dfa (lambda (tr)
                  (loop for tr2 in (dfa-state-transitions (dfa-state dfa (transition-to tr))) do
                       (push tr (gethash tr2 pred-hash)))))
           ;;
           (let ((changep t))
             (loop for i from 0 while changep
                do
                  (setf changep nil)
                  (loop for b in (all-blocks) do
                       (when (preds b)
                         (setf (gethash b in)
                               (reduce meet (loop for s in (preds b) collect (gethash s out))))
                         (let ((new (funcall transfer-function b (gethash b in))))
                           (unless (funcall compare new (gethash b out))
                             (setf changep t
                                   (gethash b out) new)))))))))
        (values in out)))))

(defun set-equal (a b &key (test #'eql))
  (and (every (lambda (x) (member x b :test test)) a)
       (every (lambda (x) (member x a :test test)) b)))


;;;; -- Dead Variable Deletion ----------------------------------------------------------------

(defun delete-dead-variables (dfa)
  (let ((count 0) work-done-p)
    (loop
      (setf (values dfa work-done-p) (delete-dead-variables-1 dfa))
      (unless work-done-p (return))
      (incf count))
    ;; (when (> count 1) (princ "#") (prin1 count))
    dfa))

(defun delete-dead-variables-1 (dfa)
  (let ((kill-count 0)
        (var-memo (make-hash-table :test #'eql))
        (use-memo (make-hash-table :test #'eq))
        (def-memo (make-hash-table :test #'eq)))
    (setf dfa (dfa-copy dfa))
    (push (make-transition :from 2 :sigma nil :to 2 :subst
                           (loop for r in (dfa-output-registers dfa)
                                 collect (cons (gensym) r)))
          (dfa-state-transitions (dfa-state dfa 2)))
    (labels ((var-mask (v)
               (memo (var-memo v)
                     (ash 1 (hash-table-count var-memo))))
             (use (tr)
               (memo (use-memo tr)
                     (let ((z 0))
                       (loop for (nil . s) in (transition-subst tr) do
                             (setq z (logior z (etypecase s
                                                 (atom (var-mask s))
                                                 ((cons (member quote) t) 0)
                                                 ((cons (member + -) (cons atom (cons t null)))
                                                  (var-mask (cadr s)))))))
                       z)))
             (def (tr)
               (memo (def-memo tr)
                     (let ((z 0))
                       (loop for (d . nil) in (transition-subst tr) do
                             (setq z (logior z (var-mask d))))
                       z))))
      (multiple-value-bind (in out)
          (backward-dataflow-analysis
           :roots (dfa-state-transitions (dfa-state dfa 0))
           :successors (lambda (tr)
                         (dfa-state-transitions (dfa-state dfa (transition-to tr))))
           :gen #'use
           :kill #'def
           :initial-value 0
           :meet #'logior
           :transfer-function (lambda (tr out)
                                (logior (use tr) (logandc2 out (def tr))))
           :compare #'=)
        (declare (ignore in))
        (setf (dfa-state-transitions (dfa-state dfa 2))
              nil)
        (map-dfa-transitions dfa
          (lambda (tr)
            (setf (transition-subst tr)
                  (remove-if-not (lambda (ds)
                                   (cond ((logtest (var-mask (car ds)) (gethash tr out 0)))
                                         (t
                                          (incf kill-count)
                                          nil)))
                                 (transition-subst tr)))))
        (values dfa (not (zerop kill-count))) ))))

(defun backward-dataflow-analysis
    (&key
       roots
       successors
       gen
       kill
       (initial-value nil)
       (transfer-function
        #'(lambda (new old)
            (union (funcall gen new)
              (set-difference old (funcall kill new) :test #'equal)
              :test #'equal)))
       (meet #'(lambda (x y) (union x y :test #'equal)))
       (compare #'(lambda (x y) (set-equal x y :test #'equal))))
  (let ((all-blocks
         (let ((yet nil))
           (labels ((aux (b)
                      (unless (member b yet)
                        (push b yet)
                        (mapc #'aux (funcall successors b)))))
             (mapc #'aux roots)
             yet)))
        (in (make-hash-table :test #'eq))
        (out (make-hash-table :test #'eq)))
    (let ((changep t))
      (loop while changep do
            (setf changep nil)
            (loop for b in all-blocks do
                  (setf (gethash b out)
                        (reduce meet (loop for s in (funcall successors b) collect (gethash s in initial-value))
                                :initial-value initial-value))
                  (let ((new
                         (funcall transfer-function b (gethash b out))))
                    (unless (funcall compare new (gethash b in initial-value))
                      (setf changep t
                            (gethash b in initial-value) new))))))
    (values in out)))

#+(or)
(defun delete-dead-variables-1 (dfa)
  "Eliminates dead variables from the DFA /dfa/."
  (let ((name-hash (make-hash-table :test #'eq))
        (next-name -1)
        (deleted 0)
        (def-hash (make-hash-table :test #'eq))
        (use-hash (make-hash-table :test #'eq)))
    (labels ((name-var (var)
               (memo (name-hash var) (incf next-name))))
      (let ((initial
             (loop for reg in (dfa-all-output-registers dfa)
                   sum (ash 1 (name-var reg)))))
        ;;
        (labels ((use (b &aux (res 0))
                   (memo (use-hash b)
                         (assert (transition-p b))
                         (loop for (nil . s) in (transition-subst b) do
                               (when (atom s)
                                 (setf res (logior res (ash 1 (name-var s))))))
                         (when (eq 2 (transition-to b))
                           (loop for register in (dfa-all-output-registers dfa) do
                                 (setf res (logior res (ash 1 (name-var register))))))
                         res))
                 (def (b &aux (res 0))
                   (memo (def-hash b)
                         (and (transition-p b)
                              (loop for ds in (transition-subst b) do
                                    (setf res (logior res (ash 1 (name-var (car ds)))))))
                         res)))
          ;;
          (multiple-value-bind (in out)
              (dataflow-analysis
               dfa
               :direction :backward
               :gen #'use
               :kill #'def
               :initial-value 0
               :transfer-function (lambda (new old)
                                    (logior (use new) (logandc2 old (def new))))
               :meet #'logior
               :compare #'eql)
            (declare (ignore in))
            (map-dfa-transitions
                dfa (lambda (tr)
                      (setf (transition-subst tr)
                            ;; There is a little glitch above. For the transitions to the accepting
                            ;; (or exit) state (2) the output registers are missing in OUT. Thus
                            ;; special case:
                            (let ((out (if (eql 2 (transition-to tr))
                                           initial
                                           (gethash tr out))))
                              (loop for (d . s) in (transition-subst tr)
                                    for use? = (logbitp (name-var d) out)
                                    when use? collect (cons d s)
                                    unless use? do (incf deleted))))))
            (values dfa (not (zerop deleted)))))))))


;;;; -- Delta P Propagation -------------------------------------------------------------------

(defun dfa-delta-p-propagation (dfa)
  (let ((garbage (list :garbage)))
    (multiple-value-bind (in out)
        (dataflow-analysis
         dfa
         :direction :forward
         :transfer-function
         (lambda (b val)
           (etypecase b
             (dfa-state val)
             (transition
              (let ((su (transition-subst b))
                    (eof? (equal (transition-sigma b) (isum-singleton +end-of-file-sentinel+))))
                (setf eof? nil)
                (nconc
                 (mapcar (lambda (ds)
                           (cons (car ds) (if (integerp (cdr ds))
                                              (+ (cdr ds) (if eof? 0 1))
                                              garbage)))
                         (set-difference val su :key #'car))
                 (mapcar (lambda (ds)
                           (cons (car ds)
                                 (if (eq (cdr ds) 'p)
                                     (if eof? 0 1)
                                     garbage)))
                         su))))))
         :meet #'(lambda (xs ys)
                   (nconc (mapcar (lambda (x)
                                    (let ((y (assoc (car x) ys)))
                                      (if (or (null y) (equal x y))
                                          x
                                          (cons (car x) garbage))))
                                  xs)
                          (loop for y in ys
                             unless (assoc (car y) xs)
                             collect y)))
         :initial-value nil)
      (declare (ignore out))
      (let* ((deleted 0))
        (map-dfa-transitions dfa (lambda (tr)
                                   (let ((in (gethash tr in)))
                                     (setf (transition-subst tr)
                                           (mapcar (lambda (ds)
                                                     (let ((q (assoc (cdr ds) in)))
                                                       (cons (car ds)
                                                             (if (and q (integerp (cdr q)))
                                                                 (progn
                                                                   (incf deleted)
                                                                   `(- p ,(cdr q)))
                                                                 (cdr ds)))))
                                                   (transition-subst tr))))))
        (values dfa (not (zerop deleted)))))))


;;;; -- Copy Propagation ----------------------------------------------------------------------

#+(or)
(defun dfa-copy-propagation (dfa)
  (setf dfa (dfa-copy dfa))
  (multiple-value-bind (dfa change) (dfa-copy-propagation-1 (delete-dead-variables dfa))
    (if (> change 0)
        (dfa-copy-propagation dfa) ;why this second invokation is needed, is beyond me.
        dfa)))

(defun dfa-copy-propagation (dfa)
  (setf dfa (dfa-copy dfa))
  (let ((rounds 0) change-count)
    (loop
      (setf dfa (delete-dead-variables dfa))
      (setf (values dfa change-count) (dfa-copy-propagation-1 dfa))
      (when (zerop change-count) (return))
      (incf rounds))
    ;; (when (> rounds 1) (princ "@") (prin1 rounds))
    dfa))

(defun dfa-copy-propagation-1 (dfa)
  "Does copy and constant propagation for the dfa /dfa/."
  (let ((change 0)
        (garbage (list :garbage)))
    (multiple-value-bind (in out)
        (dataflow-analysis
         dfa
         :direction :forward
         :transfer-function
         (lambda (b val &aux preg)
           (etypecase b
             (dfa-state val)
             (transition
              (let* ((su (transition-subst b)))
                (nconc (loop for (d . s) in su collect
                            (if (find s su :key #'car) ;cannot do, source is destroyed right away
                                (cons d garbage)
                                (cons d (cond ((eql s 'p)
                                               ;; ad hoc CSE, to catch multiple assignments to P
                                               (or preg
                                                   (progn (setf preg d) garbage)))
                                              ((and (integerp s) (>= s 0))
                                               s)
                                              ((and (consp s) (eq (car s) 'quote))
                                               s)
                                              (t garbage)))))
                       ;; Gather what is left of 'val'
                       (loop for (d . s) in val
                          ;; when already mentioned in 'gen', skip this
                          unless (find d su :key #'car)
                          ;; when source in 'val' is a destination in the
                          ;; substitution, value turns to unknown aka
                          ;; garbage.
                          collect (cons d (if (find s su :key #'car) garbage s))))))))
         :meet #'(lambda (xs ys)
                   (nconc (loop for x in xs collect
                               (let ((y (assoc (car x) ys)))
                                 (if (or (null y) (equal x y))
                                     x
                                     (cons (car x) garbage))))
                          (loop for y in ys
                             unless (assoc (car y) xs)
                             collect y)))
         :compare #'(lambda (x y)
                      (set-equal x y :test #'equal))
         :initial-value nil)
      (declare (ignore out))
      ;;
      (map-dfa-transitions
       dfa
       (lambda (k)
         (let ((v (gethash k in)))
           (setf (transition-subst k)
                 (mapcan (lambda (ds)
                           (labels ((look (x)
                                      (let ((y (assoc x v)))
                                        (if (and y
                                                 (not (eq (cdr y) garbage))
                                                 (not (eq (cdr y) 'p)))
                                            (look (cdr y))
                                            x))))
                             (let ((it (look (cdr ds))))
                               (prog1
                                   (if (eql (car ds) it)
                                       nil
                                       (list (cons (car ds) it)))
                                 (unless (eq it (cdr ds))
                                   (incf change))))))
                         (transition-subst k))))))
      (values
       dfa
       change))))


;;;; -- Minimization --------------------------------------------------------------------------

;; This follows the usual subset or rather partioning scheme. However we must
;; be careful, as we have different kind of states, namely those dispatching
;; on context, condition, and possible lookahead; Even if those happen to look
;; the same, they may not be conflated.

(defvar *dup-re-count* 0)
(defvar *dup-res* nil)

(defun dfa-minimize (dfa)
  ;; TODO We need a weaker equivalence on PSETQs
  ;;
  ;; And, when a variable is dead at one place and live in the other
  ;; transition we could tolerate redundant stores to get less states. which
  ;; is a trade off.
  ;;
  ;; ### Last minute fix up
  (setf (dfa-state-transitions
         (aref (dfa-states dfa) 2))
        (list (make-transition :from 2 :sigma +isum-everything+ :to 2)))
  ;;
  (let* ((n-q (length (dfa-states dfa)))
         (all-q (loop for i below n-q collect i))
         (condition-dispatch-states
          (dfa-condition-dispatch-states dfa))
         (lookbehind-dispatch-states
          (dfa-lookbehind-dispatch-states dfa)))
    (labels ((new-name (P q)
               (position q P :test #'member))
             (state-class (q)
               "Return the class of the state."
               ;; We use small integers here, as we later have this hack to sort the
               ;; partitions by this class to fix our expected layout.
               ;;
               ;; We would like to leave state (1)' out here, so that other fail states would
               ;; be allocated as 1, too, but our code generator cannot cope with that as it
               ;; would also make state 3 go away. *sigh*
               (cond ((member q '(0 1 2)) q)
                     ((member q condition-dispatch-states)  3)
                     ((member q lookbehind-dispatch-states) 4)
                     (t 99))))
      ;;
      (let* ((P (partition all-q :key #'(lambda (q) (not (null (dfa-state-exit-transition dfa q))))))
             (classes (cons :exit (dfa-char-classes dfa)))
             (n-sigma (length classes))
             (hash (make-hash-table :test #'equal))
             (name (make-array n-q))
             table)
        (labels ((intern-subst (subst)
                   (setq subst (sort-subst subst))
                   (memo (hash subst) subst)))
          ;; Make a look up table: q, a -> (q' . subst)
          (setf table (make-array (list n-q n-sigma)))
          (loop for q below n-q do
                (loop for i from 0 for a in classes do
                      (setf (aref table q i)
                            (multiple-value-bind (q* subst) (dfa-transit dfa q (if (consp a) (isum-witness a) a))
                              (cons (or q* 1) (intern-subst subst)))))))
        ;; Refine the partions, until there is no more refinement to be done.
        (loop
          ;; Figure out the names of the states in P
          (loop for i from 0 for qs in P do
                (loop for q in qs do
                      (setf (aref name q) i)))
          ;; Compute new partion
          (let ((new-P
                 (mapcan (lambda (S)
                           (partition
                            S :test (lambda (q1 q2)
                                      (and (eql (state-class q1)
                                                (state-class q2))
                                           (cond ((eql q1 q2) (error "huh"))
                                                 (t
                                                  (loop for i below n-sigma
                                                        always (let ((x1 (aref table q1 i))
                                                                     (x2 (aref table q2 i)))
                                                                 (and (eq (cdr x1) (cdr x2))
                                                                      (eql (aref name (car x1))
                                                                           (aref name (car x2))))))))))))
                         P)))
            ;; When there is no change, we're done
            (when (= (length new-P) (length P))
              (return))
            (setf P new-P)))
        ;; Sort the partitions by the minimum of its original state number. This is
        ;; kind of like a kludge but ensures that our magic states 0 (start), 1
        ;; (fail), and 2 (accept) stay in place.
        (setq p (mapcar #'(lambda (p) (sort p #'<)) p))
        (setq p (sort p #'< :key #'car))
        ;; Record duplicate REs for some debugging
        (dolist (p p)
          (when (> (length p) 1)
            (incf *dup-re-count* 1)
            (push (loop for i in p collect (dfa-state-re (dfa-state dfa i))) *dup-res*)))
        ;; --- Construct the new automaton ---
        ;; state 0 is the context dispatch
        (let* ((context-states
                (remove-duplicates
                 (loop for tr in (dfa-state-transitions (aref (dfa-states dfa) 0))
                       unless (eql :exit (transition-sigma tr))
                       collect (new-name P (transition-to tr)))))
               (initial-context         ;XXX How would we know?
                (new-name P 3)))
          (setf context-states
                (cons initial-context (remove initial-context context-states)))
          ;;
          (let ((res (dfa-copy dfa)))
            (setf (dfa-states res)
                  (coerce
                   (cons
                    ;; new context dispatch
                    (make-dfa-state
                     :transitions
                     (loop for c in context-states
                           for i from 3 collect
                           (make-transition :from 0 :sigma (isum-singleton i) :to c :subst nil)))
                    (loop for S in (cdr P)
                          for i from 1
                          collect
                          (make-dfa-state
                           :transitions
                           (unless (or (= i 2) (= i 1)) ;Keep fail (1) and accpet (2) state clean.
                             (let ((q* (car S)))
                               (loop for tr in (dfa-state-transitions (aref (dfa-states dfa) q*))
                                     collect
                                     (make-transition
                                      :from i
                                      :sigma (transition-sigma tr)
                                      :to (new-name P (transition-to tr))
                                      :subst (let ((subst (transition-subst tr)))
                                               ;; Fix a possible assignment to CTX, which refers to a state number.
                                               (let ((it (assoc 'ctx subst)))
                                                 (if it
                                                     (progn
                                                       (assert (and (consp (cdr it))
                                                                    (eq 'quote (cadr it))))
                                                       (subst-compose
                                                        subst
                                                        (list (cons 'ctx
                                                                    `',(+ 3 (or (position
                                                                                 (new-name P (caddr it))
                                                                                 context-states)))))))
                                                     subst))))))))))
                   'vector))
            res))))))

(defun sort-subst (alist)
  (sort (copy-list alist) #'var< :key #'car))

(defun partition (sequence &key (test #'eql) (key #'identity))
  (let ((res nil))
    (map nil (lambda (item)
               (let* ((k (funcall key item))
                      (q (assoc k res :test test)))
                 (when (null q)
                   (push (setf q (cons k nil)) res))
                 (push item (cdr q))))
         sequence)
    (mapcar #'cdr res)))

(defun dfa-char-classes (dfa)
  "Returns all the character classes the DFA dispatches on."
  (let ((all-sigmas nil))
    (map-dfa-transitions dfa (lambda (tr)
                               (unless (eq :exit (transition-sigma tr))
                                 (pushnew (isum-intersection (transition-sigma tr)
                                                             +isum-any-character+)
                                          all-sigmas :test #'equal))))
    (cons (isum-singleton +end-of-file-sentinel+)
          (isum-classes all-sigmas))))


;;;; -- Halde ---------------------------------------------------------------------------------

(defun re-prepend-setqs (re)
  (let ((vs (mapcar #'car (re-all-variables re))))
    (let ((x re))
      (loop for v in vs do
           (when (not (member v '(cat last-lineno last-nl b))) ;(and (integerp v) (>= v 0)) ;###
             (setf x (re-and (re-setq (list v nil `',-1)) x))))
      x)))


;;;; -- Code Generation -----------------------------------------------------------------------

(defstruct parsed-lexer
  ;; ### Enter macros here and let SRE-RE do its work
  ;; ### case-sensitive-p!
  conditions
  macros
  rules
  binary-p
  track-file-position-p
  count-lines-p
  count-columns-p
  (syntax :flex)
  interactive-p
  (sloc-mode nil)
  %has-ctx
  (read-sequence-fun nil)
  (unread-sequence-fun nil))

(defstruct lexer-rule
  prefix                                ;### never used
  sre                                   ;the SRE to match
  suffix                                ;trailing context
  condition
  action
  tagmap                                ;A sequence of user var names, the i'th element corresponds to the i'th
                                        ;register.
  orig-pattern
  bolp
  ignorable                             ;Whether to not complain when this rule cannot be matched.
  whole)                                ;The whole rule for diagnostics

(defun parsed-lexer-has-condition (lexer)
  "Whether the lexer expansion has and needs a CONDITION variable."
  (not (null (cdr (parsed-lexer-conditions lexer)))))

(defun parsed-lexer-has-cur-bol (lexer)
  "Whether the lexer expansion has and needs a CUR-BOL variable."
  ;; This is the case, when we either count colums, or have any BOLP rules.
  (or (some #'lexer-rule-bolp (parsed-lexer-rules lexer))
      (parsed-lexer-count-columns-p lexer)
      (member (parsed-lexer-sloc-mode lexer) '(:line-column))))

(defun parsed-lexer-has-cur-line (lexer)
  "Whether the lexer expansion has and needs a CUR-LINE variable."
  (or (parsed-lexer-count-lines-p lexer)
      (member (parsed-lexer-sloc-mode lexer) '(:line))))

(defun parsed-lexer-has-last-nl (lexer)
  "Whether the lexer expansion has and needs a LAST-NL variable."
  (parsed-lexer-has-cur-bol lexer))

(defun parsed-lexer-has-ctx (lexer)
  "Whether the lexer expansion has and needs a CTX register."
  (let ((res (parsed-lexer-%has-ctx lexer)))
    (etypecase res
      ((not (member :unset)) res))))

(defun parsed-lexer-output-registers (lexer)
  "Returns a list of all output registers the DFA compiled from the
lexer must have."
  ;; Note that $0 is implicit by BPTR and FIN-PTR, while $1 ends up at
  ;; register 0 and 1.
  (nconc (copy-list '(last-nl last-lineno cat b))
         (loop for i from 1 to (reduce #'max (mapcar #'(lambda (rule)
                                                     (length (lexer-rule-tagmap rule)))
                                                 (parsed-lexer-rules lexer)))
               collect (tag-start-reg i) ;??
               collect (tag-end-reg i)))) ;??

;;
;; We try to optimize the generated code a bit, we recognize:
;;
;; - Whether the JAM trampolin is really used at all
;; - Whether there is any dispatch on FIN-SEM at all
;;

(defun compile-dfa (dfa lexer)
  (let ((all-registers nil)             ;List of all registers
        (qs (cons 0 (loop for i from 3  ;<-----
                          below (length (dfa-states dfa)) collect i)))
        (action-funs-needed nil)        ;The list of FIN-SEM values for which a dispatch is needed in JAM
        (action-tags-needed nil)        ;The list of FIN-SEM values for which the tag SEM.<n> is needed
        (ctx-states (list 0))
        (condition-states nil)
        (lookbehind-states nil)
        (interesting-states nil)
        (count-lines-p (parsed-lexer-count-lines-p lexer))
        (count-columns-p (parsed-lexer-count-columns-p lexer))
        (track-file-position-p (parsed-lexer-track-file-position-p lexer))
        (track-charactor-position-p (member (parsed-lexer-sloc-mode lexer) '(:character-position)))
        (any-backup-needed-p nil)         ;xxx
        ;; (any-jam-needed-p nil)
        (need-condition-dispatch-p      ;whether any condition dispatch takes place at all.
         ;; (parsed-lexer-has-cur-bol lexer) was used here to figure out whether
         ;; we have :BOL somewhere, but that is gone. (parsed-lexer-has-cur-bol
         ;; lexer) could still be true, because of counting columns. *sigh*
         (or ;; (parsed-lexer-has-cur-bol lexer)
             (parsed-lexer-has-condition lexer)))
        (domain
         (isum-union (isum-singleton +end-of-file-sentinel+)
                     (if (parsed-lexer-binary-p lexer)
                         (isum-range 0 256)
                         +isum-any-character+))))
    ;;
    (setf (parsed-lexer-%has-ctx lexer)
          (> (length ctx-states) 1))
    ;;
    (setf all-registers (mapcar #'(lambda (reg)
                                    (if (integerp reg)
                                        (register-var reg)
                                        reg))
                                (dfa-all-registers dfa)))
    (when count-columns-p
      (push 'start-col all-registers))
    (when count-lines-p
      (push 'start-line all-registers))
    ;;
    ;; Some groups may never match, but we need the output registers. So go
    ;; over all the tagmaps and collect them.
    ;;
    (let ((max-group
           (reduce #'max (mapcar #'(lambda (rule)
                                     (length (lexer-rule-tagmap rule)))
                                 (parsed-lexer-rules lexer)))))
      (loop for i below max-group do
            ;; ### btw we are bust, when tag-start-var /= tag-start-reg
            (pushnew (tag-start-var i) all-registers)
            (pushnew (tag-end-var i) all-registers)))

    ;; Embarrasing.
    (setf dfa (dfa-consolidate-jam-states dfa))

    ;; Collect all the states just after the initial state, which then is
    ;; the context dispatch.
    (loop for q in ctx-states do
         (loop for tr in (dfa-state-transitions (aref (dfa-states dfa) q)) do
              (unless (eq :exit (transition-sigma tr))
                (pushnew (transition-to tr) condition-states))))
    ;; Collect all the states just after the context dispatch, which then is
    ;; the lookbehind dispatch.
    (loop for q in condition-states do
         (loop for tr in (dfa-state-transitions (aref (dfa-states dfa) q)) do
              (unless (eq :exit (transition-sigma tr))
                (pushnew (transition-to tr) lookbehind-states))))
    ;; Collect all the states just after the lookbehind dispatch, which is
    ;; where it gets interesting.
    (loop for q in lookbehind-states do
         (loop for tr in (dfa-state-transitions (aref (dfa-states dfa) q)) do
              (unless (eq :exit (transition-sigma tr))
                (pushnew (transition-to tr) interesting-states))))
    ;; Collect all the states just after where it gets interesting, ... Just kidding.
    (labels
        ((subst-cat (subst)
           ;; Figure out the assignment to CAT from subst, returns NIL, if
           ;; the CAT assignment is not constant. [Which currently does not happen]
           (let ((x (cdr (assoc 'cat subst))))
             (and (consp x) (eq (car x) 'quote) (cadr x))))
           
         (compile-assign (d s &optional (delta-p 0))
           (cond ((eq 'cat d)
                  '(warn "Huh? ~S" (list 'compile-assign d s delta-p))
                  nil)
                 ((eq 'b d)
                  ;; also backup less
                  (list 'fin-ptr (simp-sum `(+ bptr ,(compile-source s delta-p)))))
                 ;;                     
                 ((integerp d)
                  (list (register-var d)
                        (simp-sum (compile-source s delta-p))))
                 ;;
                 (t
                  (list d (simp-sum (compile-source s delta-p))))))
         ;;
         (compile-source (s &optional delta-p)
           (cond ((eq 'p s)
                  `(+ (- rptr bptr) ,delta-p))
                 ((eq 'b s)
                  '(- fin-ptr bptr))
                 ((and (consp s) (eq (car s) 'quote))
                  (if (constantp (cadr s)) (cadr s) s))
                 ((and (consp s) (member (car s) '(+ -)))
                  `(,(car s) ,(compile-source (cadr s) delta-p) ,(caddr s)))
                 ((integerp s)
                  (register-var s))
                 (t s)))
         ;;
         (compile-state (q)
           (check-type q integer)
           (cond ((and (member q condition-states)
                       (not need-condition-dispatch-p))
                  (let ((next (find-if (lambda (tr) (and (consp (transition-sigma tr))
                                                         (isum-member 2 (transition-sigma tr))))
                                       (dfa-state-transitions (aref (dfa-states dfa) q)))))
                    (assert (not (null next)) ()
                            "~S" (dfa-state-transitions (aref (dfa-states dfa) q)))
                    (list (intern-Qn q) 
                          `(go ,(intern-Qn (transition-to next))))))
                 (t (compile-regular-state q))))
         ;;
         ;; compile-exit-psetq (q cat exit-subst delta-p backup-less-p)
         ;;
         (compile-transition (q tr delta-p &key additional-forms &aux cat)
           (multiple-value-bind (subst to)
               (values (transition-subst tr) (transition-to tr))
             (cond ((and (eql 2 to)
                         (setq cat (subst-cat subst)))
                    ;;(pushnew cat action-funs-needed)
                    (pushnew cat action-tags-needed)
                    `(progn
                       ;;,@(compile-psetq (remove 'cat (transition-subst tr) :key 'car) delta-p)
                       ,@additional-forms
                       ,@(compile-exit-psetq q cat subst delta-p t) ;??
                       (go ,(intern-SEMn cat))))
                   (t
                    `(progn
                       ,@(compile-psetq (transition-subst tr) delta-p)
                       ,@additional-forms
                       (go ,(intern-Qn (transition-to tr))))))))
         ;;
         (compile-exit-psetq (q cat exit-subst delta-p backup-less-p)
           (let* ((tagmap (and cat (>= cat 0)
                               (lexer-rule-tagmap (elt (parsed-lexer-rules lexer) cat))))
                  (needed (append '((cat . cat)
                                    (b . b))
                                  (and (parsed-lexer-has-ctx lexer)
                                       '((ctx . ctx)))
                                  ;; ### This exclusion for CAT being -1 needs some pondering still.
                                  (and (not (eql cat -1)) ;Hmm
                                       (parsed-lexer-has-last-nl lexer)
                                       (list '(last-nl . last-nl)))
                                  (and (not (eql cat -1))
                                       count-lines-p
                                       (list '(last-lineno . last-lineno)))
                                  (loop for i from 1 ;???
                                        for nil in tagmap
                                        collect (cons (tag-start-var i)
                                                      (tag-start-reg i))
                                        collect (cons (tag-end-var i)
                                                      (tag-end-reg i)))))
                  (psetq
                   (loop for (d . s) in needed nconc
                         (let ((val (cdr (or (assoc s exit-subst)
                                             (progn
                                               (warn "Assignment for ~S <- ~S was not found.~%exit-subst = ~S"
                                                     d s
                                                     exit-subst)
                                               '(nil . '-1)
                                               (cons d s) ;???
                                               )))))
                           (cond
                             #+(or)
                             ((and (eq 'cat d) backup-less-p)
                              (warn "We nuke CAT at state ~D, subst = ~S" q exit-subst)
                              nil)
                             ((and (eq 'cat d) cat)
                              (unless backup-less-p
                                '(warn "We nuke CAT at state ~D (ii)" q)
                                (push (list cat q) any-backup-needed-p)
                                (pushnew cat action-funs-needed))
                              (pushnew cat action-tags-needed)
                              (list 'fin-sem (compile-source val)))
                             #+NIL
                             ((eq 'cat d)
                              ;;### <--- we _may_ support this
                              '(error "bad ~S / ~S~%needed = ~S~%backup-less-p = ~S"
                                cat exit-subst needed backup-less-p)
                              nil)
                             (t
                              (compile-assign d val delta-p)))))))
             '(warn "~D accepts with psetq = ~S, needed = ~S, exit-subst = ~S" q psetq needed exit-subst)
             (list `(psetq ,@psetq))))
         ;;
         ;; (if (and clauses backup-less-p) -1 0)
         (compile-regular-state (q)
           (let* ((exit            (dfa-state-exit-transition dfa q))
                  (exit-subst      (and exit (transition-subst exit)))
                  (cat             (and exit (subst-cat (transition-subst exit))))
                  #+(or)
                  (backup-less-p   (and exit cat ;we cannot do this, if category is unknown
                                        (not (dfa-state-needs-backup-p dfa q))))
                  ;;
                  (backup-less-p nil) ;### This one is not working quite right.
                  ;;
                  (clauses
                   ;; This is a list of clauses as (<isum> <action>)
                   (let* ((dont-touch-p (or (member q ctx-states)
                                            (member q condition-states)
                                            (member q lookbehind-states)))
                          (special-case-nl-p (and (not dont-touch-p)
                                                  (or count-columns-p count-lines-p)))
                          (nl-tr (and special-case-nl-p
                                      (find-if (lambda (tr)
                                                 (and (listp (transition-sigma tr))
                                                      (isum-member (char-code #\newline)
                                                                   (transition-sigma tr))))
                                               (dfa-state-transitions (aref (dfa-states dfa) q)))))
                          (eof-tr (and nil
                                       (not dont-touch-p)
                                       (find-if (lambda (tr)
                                                  (and (listp (transition-sigma tr))
                                                       (isum-member +end-of-file-sentinel+
                                                                    (transition-sigma tr))))
                                                (dfa-state-transitions (aref (dfa-states dfa) q))))))
                     ;;
                     (nconc
                      (and nl-tr
                           (list `(,(isum-singleton (char-code #\newline))
                                    ,(compile-transition
                                      q nl-tr -1
                                      :additional-forms
                                      `(,@(and (parsed-lexer-has-cur-line lexer)
                                               (list '(incf cur-line)))
                                          ,@(and (parsed-lexer-has-cur-bol lexer)
                                                 (list '(setf cur-bol rptr))))))))
                      (and eof-tr
                           (list `(,(isum-singleton +end-of-file-sentinel+)
                                    ,(compile-transition q eof-tr 0))))
                      (loop for tr in (dfa-state-transitions (aref (dfa-states dfa) q))
                            nconc
                            (and (not (eql :exit (transition-sigma tr)))
                                 (not (eql (transition-to tr) 1))
                                 (let ((sigma (transition-sigma tr)))
                                   (when special-case-nl-p
                                     (setq sigma (isum-difference sigma (isum-singleton (char-code #\newline)))))
                                   (and sigma
                                        (list `(,sigma ,(compile-transition q tr -1))))))))))
                  ;;
                  (exit-psetq
                   (when exit
                     ;; (warn "exit in Q = ~S, cat = ~S." q cat )
                     (compile-exit-psetq q cat exit-subst (if (and clauses backup-less-p) -1 0) backup-less-p)))
                  ;;
                  (jam-goto        `(progn
                                      ,@(and backup-less-p exit-psetq) ;???
                                      (go ,(cond (backup-less-p
                                                  (pushnew cat action-tags-needed)
                                                  (intern-SEMn cat))
                                                 (t
                                                  'jam)))))
                  ;;
                  (dispatch
                   (list
                   (let ((left-over
                          (isum-difference domain
                                           (reduce #'isum-union
                                                   (mapcar #'car clauses) :initial-value +isum-nothing+))))
                     (cond ((isum-empty-p left-over)
                            `(isum-case ch ,@clauses))
                           (t
                            `(isum-case ch
                               ,@(append clauses (list (list t jam-goto)))))))))
                  )
             (declare (ignorable backup-less-p))
             ;;
             (cond
               ;; special case for no effective contextes
               ((and (= 0 q) (= 1 (length condition-states)))
                (list (intern-Qn q)
                      `(go ,(intern-Qn (car condition-states)))))

               ;; same thing for no effective conditions
               ((and nil (null (cdr lookbehind-states)) (member q condition-states))
                (list (intern-Qn q)
                      `(go ,(intern-Qn (car lookbehind-states)))))
               ;;
               (t
                (append
                 (list (intern-Qn q))
                 (and (not backup-less-p) exit-psetq)
                 (and clauses
                      (append
                       (cond ((member q ctx-states)
                              (assert (parsed-lexer-has-ctx lexer))
                              (list
                               `(setq ch ctx)))
                             ((member q condition-states)
                              (and need-condition-dispatch-p
                                   (list
                                    `(setq ch (+ ,(if (parsed-lexer-has-cur-bol lexer)
                                                      '(if (= rptr cur-bol) 1 0)
                                                      '0)
                                                 ,(if (parsed-lexer-has-condition lexer)
                                                      '(* 2 condition)
                                                      '2))))))
                             (t
                              (list `(setq ch (getch)))))
                       ;; -- not needed anymore, as we don't use the EOF sentinel
                       ;; (list (intern-QRn q))
                       ))
                 (and clauses
                      dispatch)
                 (and (not clauses)
                      (cdr jam-goto)))))))
         ;;
         (compile-psetq (action &optional delta-p)
           (and action
                (list `(psetq ,@(loop for (d . s) in action
                                      append (compile-assign d s delta-p)))))))
      ;;
      (let ((inner-dispatch
             (loop for q in qs append (compile-state q))))
        ;; 
        (setf all-registers
              (remove 'p (remove 'ctx (remove 'b (remove 'cat all-registers)))))

        ;;
        ;; (warn "ANY-BACKUP-NEEDED-P = ~S" any-backup-needed-p)
        ;;
        (values
         ;; body
         `(
           ;; Disptach Loop
           (locally
               ;; No user actions happen here, so this could be unsafe.
               ,*template-optimize-settings*

             ;; clear all registers
             (setf ,@(mapcan (lambda (x) (list x -1))
                             (adjoin 'fin-sem all-registers)))
             
             ,(if count-columns-p
                  (progn
                    (assert (parsed-lexer-has-cur-bol lexer))
                    '(SETF START-COL (- BPTR CUR-BOL)))
                  "")
             ,(if count-lines-p
                  (progn
                    (assert (parsed-lexer-has-cur-line lexer))
                    '(setf start-line cur-line))
                  "")
             (tagbody
                ,@inner-dispatch))

           ;; all the semantic actions
           ,@(loop for s in (setf action-tags-needed (remove -1 (sort action-tags-needed #'<))) append
                   (cons (intern-SEMn s)
                         (let ((rule (elt (parsed-lexer-rules lexer) s)))
                           (list
                            `(SETF SAVED-FIN-PTR (MIN FPTR FIN-PTR) ;MIN because of EOF behavior
                                   ,@(and (parsed-lexer-has-cur-bol lexer)
                                          (progn
                                            (assert (parsed-lexer-has-last-nl lexer))
                                            (list 'CUR-BOL '(IF (= LAST-NL -1) CUR-BOL LAST-NL)))) ;### we know better
                                   ,@(and count-lines-p
                                          (progn
                                            (assert (parsed-lexer-has-cur-line lexer))
                                            (list 'CUR-LINE '(IF (= LAST-LINENO -1)
                                                              START-LINE
                                                              LAST-LINENO)))))
                            (cond ((or count-lines-p
                                       count-columns-p
                                       track-file-position-p
                                       track-charactor-position-p)
                                   `(return-from lexer
                                      (multiple-value-bind (sem val)
                                          (block nil
                                            ,(compile-rule-action rule)
                                            (eof-flame ',(intern-SEMn s))
                                            (go start))
                                        (values sem val
                                                ,@(and track-charactor-position-p
                                                       (list '(+ bptr character-position-offset)
                                                             '(+ fin-ptr character-position-offset)))
                                                ,@(if count-lines-p '(start-line) nil)
                                                ,@(if count-columns-p '(start-col) nil)
                                                ,@(if count-lines-p
                                                      (progn
                                                        (assert (parsed-lexer-has-cur-line lexer))
                                                        '(cur-line))
                                                      nil)
                                                ,@(if count-columns-p
                                                      (progn
                                                        (assert (parsed-lexer-has-cur-bol lexer))
                                                        '((- fin-ptr cur-bol))))
                                                ,@(if track-file-position-p
                                                      (list '(aref position-buffer bptr)
                                                            '(aref position-buffer fin-ptr)))
                                                ))))
                                  (t
                                   `(return-from lexer
                                      (block nil
                                        ,(compile-rule-action rule)
                                        (eof-flame ',(intern-SEMn s))
                                        (go start)))))
                            ))))

           ;; --- The fail semantic action
           ;;
           ,(intern-SEMn -1)
           ;; I really don't know where this is comming from. We have default :EOF rules,
           ;; which ought to catch any legitmate EOF; and when we end up here, we really
           ;; failed, didn't we. Disabled for now.
           #+(OR)
           (WHEN (= RPTR BPTR)
             (WHEN (= -1 CH) 
               (RETURN-FROM LEXER :EOF))) ;### What the heck? Where does this come from?

           ,(if (parsed-lexer-has-condition lexer)
                `(lex-error "Garbage seen in context ~S."
                            (elt ',(parsed-lexer-conditions lexer) (1- condition)))
                `(lex-error "Garbage seen."))

           ;; Computed GOTO, will be unsafe again
           JAM
           ,@(and action-funs-needed
                  (list ;; 'jam
                        (if (= (length action-funs-needed) 1)
                            `(go ,(intern-SEMn (car action-funs-needed)))
                            `(locally
                                 ,*template-optimize-settings*
                               (isum-case fin-sem
                                 ;; ignore actions?
                                 ,@(loop for s in action-funs-needed collect
                                         (list s `(go ,(intern-SEMn s))))
                                 (t (error "Huh?! FIM-SEM = ~D" fin-sem))))))))
         ;;
         ;; additional vars:
         (mapcar (lambda (x) (list x -1)) all-registers)
         ;; additional decls:
         (list `(declare (type fixnum ,@all-registers)
                         (ignorable ,@all-registers)))
         ;;
         action-tags-needed
         )))))

(defun intern-Qn (n)     (symcat 'Q n))
(defun intern-SEMn (n)   (if (eql -1 n) 'fail (symcat 'SEM. n)))
(defun intern-QRn (n)    (symcat 'Q n 'R))
(defun intern-$n.s (n)   (symcat '$ n '.S))
(defun intern-$n.e (n)   (symcat '$ n '.E))
(defun intern-Rn (n)     (symcat 'R n))

;;(defun intern-$n (n)     (intern (format nil "$~D" n)))

;;(defun intern-$n.s (n)   (if (integerp n) (* 2 n) (symcat '$ n '.S)))
;;(defun intern-$n.e (n)   (if (integerp n) (1+ (* 2 n)) (symcat '$ n '.E)))

(defun intern-$n (n)
  ;; This is not optimimal. These are user actions, see COMPILE-RULE-ACTION.
  (intern (format nil "$~D" n)))

(defun symcat (&rest rest)
  (let ((package (loop for k in rest do (when (symbolp k) (return (symbol-package k))))))
    (intern (with-standard-io-syntax (format nil "~{~A~}" rest)) package)))

(defun compile-rule-action (rule)
  ;; The extra PROGNs in the macros are to make what is returned not
  ;; SETF-able by accident.
  ;;
  ;; This is a hack. We don't want the user being forced to :USE or :IMPORT the
  ;; $$ or $n symbols. We walk the form, looking for any symbol named that way
  ;; and add symbol macros for each. We rely on TAG-START-VAR and TAG-END-VAR
  ;; for the indices.
  ;;
  (let ((varmap (lexer-rule-tagmap rule)))
    `(with-submatch-macros-2 (buffer)
         ((0 T BPTR FIN-PTR)
          ,@(loop for var in varmap
                  for i from 1          ;???
                  collect (list var 
                                `(>= ,(tag-start-var i) 0)
                                `(+ bptr ,(tag-start-var i))
                                `(+ bptr ,(tag-end-var i)))))
       ,(lexer-rule-action rule))))

(defmacro with-submatch-macros-2 ((string-var) (&rest bindings) &body body)
  ;; Bindings is list of (<var> <test> <start-form> <end-form>)
  (let ((numerical-var-map (form-all-$n-symbol-map body)))
    (setq bindings (mapcan (lambda (b)
                             (cond ((integerp (car b))
                                    (cons b
                                          (mapcar (lambda (x) (cons x (cdr b)))
                                                  (cdr (assoc (car b) numerical-var-map)))))
                                   (t
                                    (list b))))
                           bindings))
    (labels ((buffer-macro (name map)
               ;; map is (<name> <form>)
               `(,name (which)
                       (case which
                         ,@(loop for (name test form) in map collect
                                 `((,name)
                                   '(progn (and ,test ,form))))
                         (otherwise
                          (error "Undefined ~S: ~S" ',name which))))))
      (let ((substring-cache-vars
             ;; A-list of ((<start-form> . <end-form>) . <subseq-cache>)
             (remove-duplicates (mapcar (lambda (b)
                                          (cons (list (caddr b) (cadddr b))
                                                (gensym "BAG.")))
                                        bindings)
                                :key #'car :test #'equal)))
        `(let (,@(mapcar #'cdr substring-cache-vars))
           (declare (ignorable ,@(mapcar #'cdr substring-cache-vars))
                    (type (or null ,*string-type*) ,@(mapcar #'cdr substring-cache-vars)))
           (symbol-macrolet
               (,@(loop for (var test start-reg end-reg) in bindings
                        for cache-var = (cdr (assoc (list start-reg end-reg) substring-cache-vars
                                                    :test 'equal))
                        when (symbolp var)
                        collect
                        `(,var
                          (and ,test
                               (or ,cache-var
                                   (setq ,cache-var
                                         (subseq-string ,string-var ,start-reg ,end-reg)))))))
             (macrolet (,(buffer-macro ':start
                                       (mapcar (lambda (b) (list (car b) (cadr b) (caddr b)))
                                               bindings))
                        ,(buffer-macro ':end
                                       (mapcar (lambda (b) (list (car b) (cadr b) (cadddr b)))
                                               bindings))
                         ,(buffer-macro ':buffer
                                        (mapcar (lambda (b) (list (car b) t string-var))
                                                bindings)))
               ,@body)))))))


;; ----

(defun tag-start-var (i)
  (register-var (tag-start-reg i)))

(defun tag-end-var (i)
  (register-var (tag-end-reg i)))

(defun register-var (index)
  (if (symbolp index)
      index
      ;; A matter of taste
      (intern-Rn index)))

;; (defun tag-start-reg (tag) (intern-$n.s tag))
;; (defun tag-end-reg (tag) (intern-$n.e tag))

(defun tag-start-reg (tag)
  (cond ((integerp tag) (* 2 tag))
        (t (intern-$n.s tag))))

(defun tag-end-reg (tag)
  (cond ((integerp tag) (1+ (* 2 tag)))
        (t (intern-$n.e tag))))

;; ----

(defmacro isum-case (var &body clauses)
  ;; A variation on the theme, actually this is of more general use, since
  ;; Common Lisp implementations lack a jump table based implementation of
  ;; CASE.
  (let* ((last-out nil)
         (res nil)
         (default (find t clauses :key #'car))
         (clauses (remove default clauses))
         (clauses (mapcar (lambda (clause)
                            (cond ((integerp (car clause))
                                   (cons (list (car clause) (1+ (car clause)))
                                         (cdr clause)))
                                  (t clause)))
                          clauses)))
    (assert (every #'evenp (mapcar #'length (mapcar #'car clauses))))
    (loop do
         (when (every #'null (mapcar #'car clauses))
           (return))
         (let ((pivot (reduce #'min (remove nil (mapcar #'caar clauses)))))
           (setf clauses (mapcar (lambda (y)
                                   (if (eql (caar y) pivot) (cons (cdar y) (cdr y)) y))
                                 clauses))
           (let ((out (or (find-if (lambda (y) (oddp (length (car y)))) clauses)
                          default)))
             (unless (equal (cdr out) last-out)
               (push pivot res)
               (push (if (null (cddr out)) (cadr out) `(progn ,@(cdr out)))
                     res)
               (setf last-out (cdr out))))))
    (labels ((cons-if (cond cons alt)
               (cond ((null cons) `(unless ,cond ,alt))
                     ((null alt)  `(when ,cond ,cons))
                     (t           `(if ,cond ,cons ,alt))))
             (cons-progn (x)
               (if (null (cdr x))
                   (car x)
                   `(progn ,@x)))
             (foo (xs default)
               (cond ((null xs) default)
                     ((= 2 (length xs))
                      (cons-if `(< ,var ,(first xs)) default (second xs)))
                     (t
                      (let ((p (* 2 (floor (length xs) 4))))
                        (cons-if `(< ,var ,(elt xs p))
                                 (foo (subseq xs 0 p) default)
                                 (foo (subseq xs (+ 2 p)) (elt xs (1+ p)))))))))
      (foo (reverse res) (cons-progn (cdr default))))))

;;  

(defun simp-sum (form)
  "When /form/ is some combination of applications of + or - simplify the
expressions, so that it does look neater."
  (let ((summands nil)
        (offset 0))
    (labels ((walk (form negp)
               (cond ((and (consp form) (eq (car form) '-))
                      (cond ((null (cddr form))
                             (walk (cadr form) (not negp)))
                            (t
                             (walk (cadr form) negp)
                             (map nil #'(lambda (x) (walk x (not negp))) (cddr form)))))
                     ((and (consp form) (eq (car form) '+))
                      (map nil #'(lambda (x) (walk x negp)) (cdr form)))
                     ((numberp form)
                      (incf offset (* (if negp -1 1) form)))
                     ((member (list (not negp) form) summands :test #'equal)
                      (setf summands (delete (list (not negp) form) summands :test #'equal)))
                     (t
                      (push (list negp form) summands)))))
      (walk form nil)
      (let ((pos (mapcar #'cadr (remove nil summands :key #'car :test-not #'eq)))
            (neg (mapcar #'cadr (remove nil summands :key #'car))))
        (cond ((< offset 0)
               (push (- offset) neg))
              ((> offset 0)
               (push offset pos)))
        (setf pos
              (cond ((null pos) 0)
                    ((null (cdr pos)) (car pos))
                    (t `(the fixnum (+ ,@pos)))))
        (cond ((null neg)
               pos)
              ((and (eql 0 pos) (null (cdr neg)))
               (if (numberp (car neg))
                   (- (car neg))
                   `(the fixnum (- ,(car neg)))))
              (t
               `(the fixnum (- (the fixnum ,pos) ,@neg))))))))


;;;; -- Driver Template -----------------------------------------------------------------------

;; Our buffer is a string and we want to put an underflow sentinel
;; into that buffer to detect buffer underflow. However we stlll want
;; to accept languages, which happen to include this sentinel. Here is
;; what we do:

;; Our underflow sentinel is 0. Reason is, that C programs have
;; serious problems with the NUL character and thus it should occur
;; very seldom in legit text files.

;; The per state dispatch code then looks for this NUL character and
;; calls (UNDERFLOW). Should this be a real underflow, new characters
;; are read into the buffer and scanning continues. (UNDERFLOW) will
;; return the next character.

;; Should this not be a real underflow, the (UNDERFLOW) function
;; returns an underflow substitute, which in our case is -2. The trick
;; is, that each \sigma in each transition maps the real NUL to the
;; substitute -2. And everything is fine again.

;; Should (UNDERFLOW) detect end of file +END-OF-FILE-SENTINEL+, which
;; is -1, is returned.


;; CLEX input buffer:
;;
;;                 fin-ptr // point at which the automaton
;;                       | // was at a final state
;;                       |
;;                       |           fptr = end of buffered data
;;                       |             |
;; buffer:  |-------|XXXX|XXXXX|XXXXXXX|----------------|
;;          0  |    |          |                    buffer-length
;;             |    |          |
;;             |    |        rptr = next char is read here
;;             |    |
;;             |  bptr // where the current token begins
;;             |
;;             bol      // beginning of line, this could well be negative.

(define-condition lexer-condition ()
  ((buffer :initarg :buffer :reader lexer-condition-buffer)
   (bptr :initarg :bptr :reader lexer-condition-bptr)
   (rptr :initarg :rptr :reader lexer-condition-rptr)
   (fptr :initarg :fptr :reader lexer-condition-fptr)
   (input :initarg :input :reader lexer-condition-input))
  (:report report-lexer-condition))

(defun report-lexer-condition (condition stream)
  (multiple-value-bind (buffer bptr rptr fptr input)
      (values (lexer-condition-buffer condition)
              (lexer-condition-bptr condition)
              (lexer-condition-rptr condition)
              (lexer-condition-fptr condition)
              (lexer-condition-input condition))
    ;; figure out the current line
    (handler-case
        (let* ((p1 (position #\Newline buffer
                             :start 0
                             :end (if (and (< bptr fptr) (eql #\newline (char buffer bptr)))
                                      (max 0 (1- bptr))
                                      bptr)
                             :from-end t))
               (p1 (if p1 (1+ p1) 0))
               (p2 (or (position #\Newline buffer :start p1 :end fptr) fptr))
               (line (subseq buffer p1 p2)))
          (format stream "~@[On ~S:~%~]~?~%~%~A~%~v<~>~v,,,v<~>~%"
                  (and (not (vectorp input)) input)
                  (simple-condition-format-control condition)
                  (simple-condition-format-arguments condition)
                  line
                  (- bptr p1)
                  (max 1 (- (min p2 (1- rptr)) bptr)) ;the dashes
                  (or (code-char #x23BA) #\-)
                  ;; ~v,,,'-<~>
                  ;; #.(or (ignore-errors (code-char #x2193)) "v")
                  ;; line
                  ))
      (error (c)
        (format stream "Huh? ~A; ~S" c (list bptr fptr))))))
                
(define-condition lexer-warning (lexer-condition simple-warning)
  ())

(define-condition lexer-error (lexer-condition simple-error)
  ())

;;;; -- Lexer Methods -------------------------------------------------------------------------

;; I call these things method, because they are invoked via FUNCALL aka SEND.

(defun lexer-remaining-input (lexer)
  "Given, that 'lexer' is a function created by LEXER, returns a character
input stream, which will provide all the not yet read input from the
original stream."
  (funcall lexer :remaining-input))


;;;; ------------------------------------------------------------------------------------------

#+(or)
(defmacro mytime (expr)
  (let ((t1 (gensym "T1."))
        (t2 (gensym "T2.")))
    `(let ((,t1 (get-internal-real-time)))
       (multiple-value-prog1
           ,expr
         (let ((,t2 (get-internal-real-time)))
           (note "~S took ~:Dms"
                 ',(car expr) (round (* 1000 (- ,t2 ,t1)) internal-time-units-per-second)))))))

#-(or)
(defmacro mytime (expr)
  expr)

(defvar *saved-state-count* 0)
(defvar *max-state-count* 0)
(defvar *dfa1*)

(defvar *optimize-dfa-p* t)

(defvar *dfa-simped*)                   ;debugging

(defun dfa-simp (dfa)
  (setq dfa (dfa-minimize dfa))         ;???
  (let ((total (length (dfa-states dfa)))
        prev-count)
    (setf *dfa0* (dfa-deep-copy dfa))
    (setq dfa (cleanup-dfa dfa))
    (setf *dfa1* (dfa-deep-copy dfa))
    (when *optimize-dfa-p*
      ;; Somehow this is borken
      ;;(setq dfa (dfa-clean-eof dfa))
      '(dfa-clean-eof dfa)
      (setq *dfa1* dfa)
      #-(or)
      (progn
        ;; Note: In _theory_ all these data flow optimizations would also have
        ;; been done by the compiler. Perhaps we figure out, if they do? But I
        ;; am not holding my breath.
              (progn 'time (setf dfa (dfa-remove-jam-gotos dfa)))
        (loop repeat 1 do
              (setq prev-count (length (dfa-states dfa)))
              (setq dfa (dfa-clean-sigma dfa))
              (setq dfa(dfa-remove-dead-states(dfa-move-exit-substs(dfa-hoist-exits dfa))))
              (setf dfa (mytime (dfa-copy-propagation dfa)))
              (setf dfa (mytime (dfa-delta-p-propagation dfa)))
              (setf dfa (mytime (delete-dead-variables dfa)))
              ;;
              (setq dfa (dfa-clean-sigma dfa))
              (setf dfa (mytime (dfa-minimize dfa)))
              '(setq dfa (dfa-clean-sigma dfa))
              ''(setf dfa (dfa-remove-jam-gotos dfa 2))
              ;;
              (when (= prev-count (length (dfa-states dfa)))
                (return))
              '(print *last-test*)
              '(terpri)
              '(princ "#")
              '(force-output))))
    ;;
    (incf *saved-state-count* (- total (length (dfa-states dfa))))
    (incf *state-count* (length (dfa-states dfa)))
    (setf *max-state-count* (max *max-state-count* (length (dfa-states dfa))))
    (setq *dfa-simped* (dfa-copy dfa))
    dfa))

(defun dfa-move-exit-substs (dfa)
  (setq dfa (dfa-deep-copy dfa))
  ;; This is easy. We map over all transitions and when that transition
  ;; leads to a state having an exit transition, we merge that following
  ;; subst in.
  (map-dfa-transitions dfa
    (lambda (tr)
      (unless (eql :exit (transition-sigma tr))
        (let ((putative-exit
               (dfa-state-exit-transition dfa (transition-to tr))))
          (when putative-exit
            (setf (transition-subst tr)
                  (subst-compose (transition-subst tr)
                                 (mapcar (lambda (ds)
                                           (cons (car ds) (shift-p-in-term (cdr ds) 1)))
                                         (transition-subst putative-exit)))))))))
  '(map-dfa-transitions dfa
    (lambda (tr)
      (when (eql :exit (transition-sigma tr))
        (setf (transition-subst tr) nil))))
  '(map-dfa-states dfa
    (lambda (q)
      (let ((tr (dfa-state-exit-transition dfa q)))
        (when tr
          (let ((new-sigma (isum-difference
                            #+NIL
                            +isum-everything+
                            #-NIL
                            (isum-union
                             (isum-union (isum-singleton +end-of-file-sentinel+)
                                         (isum-singleton +beginning-of-file-sentinel+))
                             +isum-any-character+)
                            (dfa-state-sigma q))))
            (setf (transition-sigma tr) new-sigma)
            (when (isum-empty-p new-sigma)
              (print `(empty ,tr ,(dfa-state-sigma q)))
              (setf (dfa-state-transitions q)
                    (remove tr (dfa-state-transitions q)))))))))
  dfa)

(defun dfa-remove-dead-states (dfa)
  (let ((used nil))
    (labels ((walk (q)
               (or (member q used)
                   (progn
                     (push q used)
                     (dolist (tr (dfa-state-transitions (dfa-state dfa q)))
                       (walk (transition-to tr)))))))
      (dotimes (i 3) (walk i))
      (setq dfa (dfa-copy dfa))
      (setf (dfa-states dfa) (copy-seq (dfa-states dfa)))
      (dotimes (i (length (dfa-states dfa)))
        (unless (member i used)
          (setf (aref (dfa-states dfa) i)
                (let ((q (copy-dfa-state (aref (dfa-states dfa) i))))
                  (setf (dfa-state-re q) nil
                        (dfa-state-transitions q) nil)
                  q))))
      dfa)))

(defun exit-transition-p (tr)
  (eq (transition-sigma tr) :exit))

(defun dfa-hoist-exits (dfa)
  ;; This makes all transitions to states being :EXIT states to go to the
  ;; accepting state 2 directly.
  (setq dfa (dfa-copy dfa))
  (map-dfa-transitions dfa
    (lambda (tr)
      (let ((tr* (dfa-state-transitions (dfa-state dfa (transition-to tr)))))
        (cond ((and (= 1 (length tr*))
                    (exit-transition-p (car tr*))
                    (/= 0 (transition-from tr)))
               (setf (transition-to tr) 2
                     (transition-subst tr)
                     (subst-compose (transition-subst tr)
                                    (shift-subst (transition-subst (car tr*)) 1))))))))
  dfa)

(defun shift-subst (subst delta)
  "Given a transition substitution return one with P shifted by /delta/."
  (mapcar (lambda (ds) (cons (car ds) (shift-p-in-term (cdr ds) delta))) subst))

(defun shift-p-in-term (term delta)
  "Given some term, adjust so that P shifted by /delta/. Or put
   otherwise: When P occurs in /term/ make it (+ P <delta>)."
  (labels ((cons-p-minus-delta (delta)
             (if (zerop delta) 'p `(- p ,delta))))
    (etypecase term
      ((member p) (cons-p-minus-delta (- delta)))
      (atom term)
      ((cons (member quote) t) term)
      ((cons (member -) (cons (member p) (cons integer null)))
       (cons-p-minus-delta (- (caddr term) delta))))))

(defun dfa-clean-sigma (dfa)
  ;; ### We should not produce these in the first place
  (setf dfa (copy-dfa dfa))
  (setf (dfa-states dfa)
        (map 'vector (lambda (q) 
                       (let ((new-trs nil))
                         (loop for tr in (dfa-state-transitions q) do
                              (let ((other (find-if (lambda (other)
                                                      (and (eql (transition-to other)
                                                                (transition-to tr))
                                                           (not (eql (transition-sigma other) :exit))
                                                           (not (eql (transition-sigma tr) :exit))
                                                           (equal (transition-subst other)
                                                                  (transition-subst tr))))
                                                    new-trs)))
                                (if other
                                    (setf (transition-sigma other)
                                          (isum-union (transition-sigma other)
                                                      (transition-sigma tr)))
                                    (push (copy-transition tr) new-trs))))
                         (setf q (copy-dfa-state q))
                         (setf (dfa-state-transitions q) new-trs)
                         q))
             (dfa-states dfa)))
  dfa)

(defun jam-state-p (dfa q)
  (unless (integerp q) (setq q (position q (dfa-states dfa))))
  (let ((yet nil))
    (labels ((walk (q)
               (cond ((member q yet))
                     ((= q 2) (return-from jam-state-p nil))
                     (t
                      (push q yet)
                      (when (dfa-state-exit-transition dfa q)
                        (return-from jam-state-p nil))
                      (when (find 2 (dfa-state-transitions (dfa-state dfa q))
                                  :key #'transition-to)
                        (return-from jam-state-p nil))
                      (map nil #'walk (dfa-state-successors dfa q))))))
      (walk q)
      ;; fall through
      t)))

(defun dfa-remove-jam-gotos (dfa &optional (id 1))
  (loop for q across (dfa-states dfa) do
       (setf (dfa-state-transitions q)
             (remove-if (lambda (tr)
                          (and (not (eq :exit (transition-sigma tr)))
                               (eql id (transition-to tr))))
                        (dfa-state-transitions q))))
  dfa)

(defun dfa-state-needs-backup-p (dfa q)
  (not (every #'(lambda (tr)
                  (let ((q2 (transition-to tr)))
                    (or (eql q2 1)      ;fail state
                        ;(eql q2 0)
                        (eql q2 2)      ;accept state
                        (dfa-state-exit-transition dfa q2))))
              (dfa-state-transitions (aref (dfa-states dfa) q)))))

(defun dfa-states-needing-backup (dfa)
  (let ((starts (append (dfa-net-false-start-states dfa)
                        (dfa-start-states dfa))))
    (loop for q below (length (dfa-states dfa))
       when (and (not (member q starts))
                 (dfa-state-needs-backup-p dfa q))
       collect q)))

(defun dfa-all-final-states (dfa)
  (loop for i below (length (dfa-states dfa))
       when (dfa-state-exit-transition dfa i)
       collect i))

(defun dfa-net-start-states (dfa)
  (remove 2
          (loop for q1 in (dfa-state-successors dfa 0) append
               (loop for q2 in (dfa-state-successors dfa q1) collect q2))))

(defun dfa-net-false-start-states (dfa)
  (cons 0
        (remove 2
                (loop for q1 in (dfa-state-successors dfa 0) collect q1))))

(defun dfa-context-states (dfa)
  "Returns a list of state (numbers), which are start states of a
context. Usually a dispatch on the condition follows."
  (remove 2 (dfa-state-successors dfa 0)))

(defun dfa-start-states (dfa)
  "Returns a list of state (numbers), which are start states of a
context. Usually a dispatch on the condition follows."
  (remove 2 (loop for x in (dfa-state-successors dfa 0)
               append (dfa-state-successors dfa x))))


;;;; -- Lexer Macro ---------------------------------------------------------------------------

;;; MIXING FLEX AND SRE GROUP CAPTURE

;; Groups in flex regular expressions are generated by "(...)" and counted
;; from 1. They are named by integers and could only be referred to by BAG,
;; START, END, BUFFER. FLEX group capture within macros is killed. Internally
;; I prefer the groups just be named by integers starting from 0. This is what
;; LEXER-RULE-TAGMAP is for. The integer names of groups generated by FLEX
;; syntax have nothing to do with our internal names. It is PROCESS-SRE,
;; which establishes this mapping.

(defparameter +eol-suffix+
  '(or #\Newline :eof))

(defmacro lexer ((&whole  w input &key interactive-p) &rest clauses)
  (declare (ignore interactive-p input))
  (let ((*print-pretty* t)
        (*count-lines-p* nil)
        (*count-columns-p* nil)
        (*template-closure-p* t))
    (lexer-aux w clauses)))

(defun lexer-aux (input clauses &key start-form end-form)
  (let ((*pretext-hack* t)
        (*scanning-mode-p* t))
    (compile-parsed-lexer (parse-lexer clauses) :input-form input :start-form start-form :end-form end-form)))

(defvar *lexer-dfa*)

(defun compile-parsed-lexer (lexer &key input-form start-form end-form)
  (prog1
      (setf *expansion*
            (let* ((dfa (dfa-simp
                         ;; I still don't like the special here.
                         (let ((*default-re-syntax* (parsed-lexer-syntax lexer)))
                           (re-dfa lexer
                                   :output-registers (parsed-lexer-output-registers lexer)
                                   :has-condition-dispatch t
                                   :has-lookahead t))))
                   (*track-file-position-p*
                    (parsed-lexer-track-file-position-p lexer))
                   (*count-columns-p*
                    (parsed-lexer-count-columns-p lexer))
                   (*count-lines-p*
                    (parsed-lexer-count-lines-p lexer))
                   (track-charactor-position-p
                    (member (parsed-lexer-sloc-mode lexer) '(:character-position))))
              (setf *dfa* dfa)
              (setf *lexer-dfa* (dfa-copy dfa))
              (setf *lexer* lexer)
              (multiple-value-bind (inner additional-vars additional-decls categories-seen)
                  (mytime (compile-dfa dfa lexer))
                ;; Check for rules that cannot be matched and whine unless they were
                ;; marked ignorable.
                (loop for cat from 0
                      for rule in (parsed-lexer-rules lexer)
                      do (unless (member cat categories-seen)
                           (unless (lexer-rule-ignorable rule)
                             (let ((pat (lexer-rule-orig-pattern rule)))
                               (compiler-descend (lexer-rule-whole rule)
                                 (compiler-warn pat "Rule ~D with pattern ~S cannot be matched."
                                                cat pat))))))
                ;;
                (when track-charactor-position-p
                  (push '(character-position-offset 0) additional-vars)
                  ;; We can't do better, a whole file size may not fit a fixnum
                  (push '(declare (type unsigned-byte character-position-offset)) additional-decls))
                ;;
                (let* ((g.start (gensym "START."))
                       (g.end (gensym "END."))
                       (res
                        (fill-lexer-template
                         inner
                         :lexer lexer
                         :additional-vars additional-vars
                         :additional-decls additional-decls
                         :conditions (parsed-lexer-conditions lexer)
                         :binaryp (parsed-lexer-binary-p lexer)
                         :start-form g.start
                         :end-form g.end
                         :track-charactor-position-p track-charactor-position-p)))
                  ;; ### This must get better: When :START and :END are provided, scan a string
                  ;; ### always and blame, when a stream is passed, as START and END are meaningless
                  ;; ### for streams. Also: We now export START and END, which may collide.
                  `((lambda (input
                             &key (interactive-p ,(parsed-lexer-interactive-p lexer))
                             &aux (,g.start ,start-form) (,g.end ,end-form))
                      ,res)
                    ,@input-form)))))
    (let ((n 0))
      (loop for q below (length (dfa-states *dfa*)) do
            (unless (and (dfa-state-exit-transition *dfa* q)
                         (= 1 (length (dfa-state-transitions (aref (dfa-states *dfa*) q)))))
              (incf n)))
      '(note "We have ~D states (without :exit pseudo states)." n)
      '(note "We have ~D contexts."
        (length (dfa-state-transitions (aref (dfa-states *dfa*) 0)))))))

(defun symbol-equal (x y)               ;### <-- use also in sre-re
  (and (symbolp x) (not (keywordp x))
       (symbolp y) (not (keywordp y))
       (string-equal x y)))

(defun parse-lexer (clauses &key (initial-condition :initial))
  (let ((macros nil)
        (rules nil)
        (conditions nil)
        (syntax :extended)
        (case-sensitive t)
        (track-file-position-p nil)
        (count-lines-p *count-lines-p*)
        (count-columns-p *count-columns-p*)
        (dot-includes-newline nil)
        (interactive-p nil)
        (binaryp nil)
        (sloc-mode :unspecified)
        (read-sequence-fun nil)
        (unread-sequence-fun nil))
    ;;
    (macrolet
        ((destructuring-case ((var) &rest clauses)
           `(cond
              ,@(loop for (funs params . body) in (remove t clauses :key #'car) collect
                      `((or ,@(loop for fun in (if (listp funs) funs (list funs))
                                    collect `(eql (car ,var) ',fun)))
                        (destructuring-bind ,params (cdr ,var)
                          ,@body)))
              ,@(remove t clauses :test-not #'eql :key #'car))))
      ;;
      (labels
          ((expand-new-style (x &aux p)
             (cond ((not (listp x))
                    (compiler-warn x "Bad clause"))
                   (t
                    ;; new style clause?
                    (cond ((and (setf p (position '-> x :test #'symbol-equal))
                                (<= p 1))
                           (let* ((lhs (subseq x 0 p))
                                  (p3 (position '-> x :start (1+ p) :test #'symbol-equal))
                                  (p2 (position '=> x :start p :end (or p3 (length x)) :test #'symbol-equal))
                                  (expr `(and ,@(subseq x (1+ p) (or p2 p3 (length x))))) ;### this destroys some SLI
                                  (action (and p2 (subseq x (1+ p2) (or p3 (length x))))))
                             (cond ((= 2 (length expr))
                                    (setf expr (cadr expr))))
                             (setf action
                                   (if (null lhs)
                                       `(progn ,@action)
                                       `(return (values ,(car lhs) (progn ,@action)))))
                             (cons (list expr action)
                                   (if p3
                                       (expand-new-style (append lhs (subseq x p3)))
                                       nil))))
                          ((and (= 3 (length x))
                                (symbol-equal '= (cadr x)))
                           (destructuring-bind (name equal expansion) x
                             (list (list equal name expansion))))
                          (t
                           ;; old style
                           (list x))))))
           ;;

           ;;
           (add-macro (name expansion)
             (cond ((assoc name macros)
                    (compiler-warn name "Duplicate definition of macro ~S." name))
                   (t
                    (push (cons name expansion) macros))))
           ;;
           (process-clause (condition clause &optional (whole clause))
             (cond ((atom clause)
                    (compiler-warn clause "Bad clause."))
                   ;; Handle options, which is everything starting with a keyword
                   (t
                    (compiler-descend whole
                      (destructuring-case (clause)
                        ;;
                        ((:macro =) (name expansion)
                         (add-macro name expansion))
                        ;;
                        ((:simple-tokens) (&rest tokens)
                         (loop for x in tokens do
                               (push (build-rule `',x
                                                 condition
                                                 `(return ,(intern (string-upcase x) :keyword))
                                                 (list :token x)
                                                 whole)
                                     rules)))
                        ;;
                        ((:in) (condition &rest more)
                         (process-clauses (name-conditions condition) more))
                        ;;
                        ((:eof) (&rest more)
                         (process-clause condition (cons `(and :eof) more) whole))
                        ;;
                        ((:syntax) (syntax-option)
                         (setf syntax syntax-option))
                        ((:sloc) (sloc-mode-option)
                         (etypecase sloc-mode-option
                           ((member nil :line :line-column :file-position :character-position)))
                         (setq sloc-mode sloc-mode-option))
                        ((:dot-includes-newline-p :dot-includes-newline) (boolean)
                         (setq dot-includes-newline boolean))
                        ;;
                        ((:case-sensitive-p :case-sensitive) (flag)
                         (setf case-sensitive flag))
                        ((:case-insensitive-p :case-insensitive) (flag)
                         (setf case-sensitive (not flag)))
                        ;;
                        ((:initial-condition) (condition)
                         (setf initial-condition condition))
                        ;;
                        ((:element-type) (element-type)
                         (cond ((subtypep element-type '(unsigned-byte 8))
                                (setf binaryp t))
                               ((subtypep element-type 'character)
                                (setf binaryp nil))
                               (t
                                (error "Unrecognized ~S option: ~S" :element-type element-type))))
                        ((:interactive-p :interactive) (boolean)
                         (setq interactive-p boolean))
                        ((:track-file-position-p) (boolean)
                         (setq track-file-position-p boolean))
                        ((:count-columns-p) (boolean)
                         (setq count-columns-p boolean))
                        ((:count-lines-p) (boolean)
                         (setq count-lines-p boolean))
                        ((:read-sequence) (fun)
                         (setq read-sequence-fun fun))
                        ((:unread-sequence) (fun)
                         (when *template-closure-p*
                           (error "The ~S option is only supported with ~S."
                                  ':unread-sequence 'lexing))
                         (setq unread-sequence-fun fun))
                        ;;
                        (t
                         (cond ((keywordp (car clause))
                                (compiler-warn clause "Unrecognized lexer option ~S." (car clause)))
                               (t
                                ;; all else is regular rule
                                (push (build-rule (car clause)
                                                  condition
                                                  (if (cddr clause)
                                                      `(progn ,@(cdr clause))
                                                      (cadr clause))
                                                  (car clause)
                                                  whole)
                                      rules)))))))))
           ;;
           (process-clauses (condition clauses)
             (map nil #'(lambda (cl)
                          (map nil #'(lambda (new-cl)
                                       (process-clause condition new-cl cl))
                               (expand-new-style cl)))
                  clauses))
           ;;
           (name-condition (condition)
             (1+ (or (position condition conditions)
                     (prog1
                         (length conditions)
                       (setf conditions (nconc conditions (list condition)))))))
           ;;
           (name-conditions (conditions)
             (cond ((atom conditions) (name-condition conditions))
                   ((mapcar #'name-condition conditions))))
           ;;
           (build-rule (sre cond action orig whole)
             ;; ### Kludge
             (multiple-value-bind (sre ignorable-p)
                 (if (typep sre '(cons (member ignorable :ignorable) (cons t null)))
                     (values (cadr sre) t)
                     (values sre t))
               (multiple-value-bind (sre tagmap) 
                   (process-sre sre macros 
                                :syntax syntax
                                :dot-includes-newline dot-includes-newline
                                :case-sensitive case-sensitive
                                :top-group nil)
                 (pop tagmap)           ;xxx
                 (make-lexer-rule
                  :orig-pattern orig
                  :sre (if (and (not case-sensitive) sre) `(ci ,sre) sre)
                  :condition cond
                  :action action
                  :tagmap tagmap
                  :ignorable ignorable-p
                  :whole whole)))))
        ;;
        ;; ### Hmm. And is that needed at all? Can't we just say that the initial is
        ;; ### called NIL always?
        ;;
        (let ((q (find :initial-condition clauses :key #'car)))
          (when q (setf initial-condition (cadr q))))
        ;;
        (process-clauses (name-condition initial-condition) clauses)
        (make-parsed-lexer
         :sloc-mode (cond ((not (eq sloc-mode :unspecified)) sloc-mode)
                          (count-columns-p :line-column)
                          (count-lines-p :line)
                          (track-file-position-p :file-position)
                          (t nil))
         :syntax syntax
         :binary-p binaryp
         :interactive-p interactive-p
         :track-file-position-p (if (eq sloc-mode :unspecified)
                                    track-file-position-p
                                    (member sloc-mode '(:file-position)))
         :count-lines-p (if (eq sloc-mode :unspecified)
                            count-lines-p
                            (member sloc-mode '(:line)))
         :count-columns-p (if (eq sloc-mode :unspecified)
                              count-columns-p
                              (member sloc-mode '(:line-column)))
         :read-sequence-fun read-sequence-fun
         :unread-sequence-fun unread-sequence-fun
         :conditions conditions
         :rules (append (reverse rules)
                        ;; Add default :EOF rules
                        #-NIL
                        (loop for i from 1 to (length conditions) collect
                              (make-lexer-rule :prefix nil
                                               :suffix nil
                                               :sre :eof
                                               :condition i
                                               :tagmap nil
                                               :action '(return :eof)
                                               :orig-pattern nil
                                               :ignorable t))))))))

(defun borrow (sexpr from)
  "Copies the tree `sexpr', while borrowing from the tree `from'."
  ;; We don't do this for less consing, but to preserve SLOC as much as
  ;; possible.
  ;;
  ;; ### Could be better!
  (let* ((res
          (cond ((equal sexpr from) from)
                ((atom sexpr) sexpr)
                ((atom from) sexpr)
                (t
                 (let ((car (borrow (car sexpr) (car from)))
                       (cdr (borrow (cdr sexpr) (cdr from))))
                   (if (and (eq car (car sexpr))
                            (eq cdr (cdr sexpr)))
                       sexpr
                       (cons car cdr))))))
         #+CCL
         (old-note (ccl::nx-source-note from))
         #+CCL
         (new-note (ccl::nx-source-note res)))
    #+CCL
    (when (and ccl::*nx-source-note-map* old-note (not new-note))
      (setf (gethash res ccl::*nx-source-note-map*) old-note))
    res))

;;;;

#+(OR)
;; This is not used and as written here wrong, see how COMPILE-RULE-ACTION
;; works!
(defun lexer-rule-used-registers (rule)
  "Grovels over the actions of a rule and figures out, which registers are
   really used. It looks for strict lexical appearance; It is a kludge in a bad
   excuse for not having a code walker proper."
  (let ((action (lexer-rule-action rule))
        (tagmap (lexer-rule-tagmap rule))
        (used '(fin-ptr)))
    (subst-if nil (lambda (node)
                    (cond ((eq node '$$)
                           (pushnew 'bptr used)
                           (pushnew 'fin-ptr used))
                          ((atom node)
                           (when (member node tagmap)
                             (pushnew (tag-start-reg (position node tagmap)) used)
                             (pushnew (tag-end-reg (position node tagmap)) used)))
                          ((case (car node)
                             ((start)
                              (when (member (cadr node) tagmap)
                                (pushnew (tag-start-reg (position (cadr node) tagmap)) used)))
                             ((end)
                              (when (member (cadr node) tagmap)
                                (pushnew (tag-end-reg (position (cadr node) tagmap)) used)))
                             ((buffer bag)
                              (when (member (cadr node) tagmap)
                                (pushnew (tag-start-reg (position (cadr node) tagmap)) used)
                                (pushnew (tag-end-reg (position (cadr node) tagmap)) used))))))
                    nil)
              action)
    used))


(defun con-init/lexer (tokens)
  ;; This builds the overall RE that our lexer matchs. First comes the context
  ;; dispatch, the the lookbehind.
  (let* ((count-lines-p (parsed-lexer-count-lines-p tokens))
         ;; ### (count-columns-p (parsed-lexer-count-columns-p tokens))
         (cr-vector
          (loop for tok in (parsed-lexer-rules tokens)
                for i from 0
                do (assert (null (lexer-rule-suffix tok)))
                collect
                (let ((re +epsilon+))
                  (compiler-descend (lexer-rule-whole tok)
                    (setq re (sre-re (lexer-rule-sre tok))))
                  (setq re (re-and (re-or (re-char +beginning-of-file-sentinel+)
                                          (re-set +isum-any-character+))
                                   re))
                  ;; backup position is here, as well as line and column count
                  (setq re (re-and re (re-setq '(b nil p))))
                  (when (parsed-lexer-has-last-nl tokens)
                    (assert (parsed-lexer-has-cur-bol tokens))
                    (setq re (re-and re (re-setq `(last-nl nil cur-bol)))))
                  (when count-lines-p
                    (assert (parsed-lexer-has-cur-line tokens))
                    (setq re (re-and re (re-setq `(last-lineno nil cur-line)))))
                  ;; Wrap the conf
                  (setq re (re-conf re))
                  (when *outrule-empty-match-p*
                    (setq re (re-intersection re (sre-re '(and (or t %bof) (or (+ t) %eof))))))
                  (setq re (re-and re (re-setq `(cat nil ',i))))
                  (list re
                        ;; BOL hack, even conditions are w/o BOL, odd w/ BOL
                        (loop for k in (let ((x (lexer-rule-condition tok)))
                                         (if (listp x) x (list x)))
                              unless (lexer-rule-bolp tok) collect (* 2 k)
                              collect (1+ (* 2 k))))))))
    (re-vector (re-and
                ;; Better preset
                (re-and
                 (if count-lines-p
                     (progn
                       (assert (parsed-lexer-has-cur-line tokens))
                       (sre-re `(setq (last-lineno nil cur-line))))
                     +epsilon+)
                 (if (parsed-lexer-has-last-nl tokens)
                     (sre-re `(setq (last-nl nil '-1)))
                     +epsilon+))

                (re-or* (loop for (token cond) in cr-vector
                              collect (progn 're-conf
                                       (re-prepend-setqs
                                       (re-and (re-set (if (listp cond)
                                                           (set-isum cond)
                                                           (isum-singleton cond)))
                                               token)))))
                )
               ;; ???
               (sre-re '(setq (cat nil '-1) (b nil p))))))


;;;; ------------------------------------------------------------------------------------------

(defun fill-lexer-template (dispatch
                            &key lexer additional-vars additional-decls conditions (binaryp nil) (start-form nil) (end-form nil)
                                 track-charactor-position-p)
  ;; ### I do not like the interactive business. Perhaps it is better
  ;; ### to just unread our lookahead, which is kept in the buffer,
  ;; ### after we are done or leave control.
  (labels ((enc (x)
             (if binaryp x `(code-char ,x)))
           (dec (x)
             (if binaryp x `(char-code ,x))))
    (let* ((element-type
            (if binaryp '(unsigned-byte 8) 'character))
           (main
            `(let (,@additional-vars)
               ,@additional-decls
               (let ((rptr -1)
                     (bptr 0)
                     (fptr saved-fptr)
                     (fin-ptr saved-fin-ptr)
                     (fin-sem -1)
                     (buffer saved-buffer)
                     (buffer-length saved-buffer-length))
                 (declare (type fixnum bptr rptr fptr fin-ptr buffer-length)
                          (type (simple-array ,element-type (*)) buffer)
                          (ignorable fin-sem))
                 (block lexer
                   (labels 
                       ((subseq-string (v s e)
                          ,(if binaryp
                               `(map 'string #'code-char (subseq v s e)) ;### slow
                               `(subseq v s e)))
                        ,(template-underflow-fun
                          lexer
                          :dec #'dec
                          :binaryp binaryp
                          :track-charactor-position-p track-charactor-position-p)
                        ;;
                        (eof-flame (sem)
                          (cond ((> fin-ptr fptr)
                                 (setf fin-ptr (min fptr FIN-PTR))
                                 (lex-error "~@<I am afraid, our universe is finite. ~
                                                     There really is nothing to see beyond end of file. ~
                                                     (Semantic action ~S did not return, after matching EOF).~@:>"
                                            sem))))
                        ;;
                        (lex-warn (format &rest args)
                          (warn 'lexer-warning
                                :buffer buffer :bptr bptr :rptr rptr :fptr fptr :input input
                                :format-control format
                                :format-arguments args))
                        (lex-error (format &rest args)
                          (error 'lexer-error
                                 :buffer buffer :bptr bptr :rptr rptr :fptr fptr :input input
                                 :format-control format
                                 :format-arguments args))
                        ,@(and (cdr conditions)
                               (list `(begin (&optional (cnd :initial)) ;What if the initial condition was specified?
                                             (setf condition
                                                   (ecase cnd
                                                     ,@(loop for i from 1 for x in conditions
                                                             collect (list x i))))))))
                     (declare (notinline underflow lex-warn lex-error
                                         ,@(and (cdr conditions) (list 'begin))
                                         subseq-string))
                     (macrolet ((getch ()
                                  '(locally
                                    ,*template-optimize-settings*
                                    (cond
                                      ((< rptr 0)
                                       (setf rptr (the fixnum (+ rptr 1)))
                                       -2)
                                      ((= rptr fptr)
                                       (underflow))
                                      (t
                                       (prog1 ,(dec '(aref buffer rptr))
                                         (setf rptr (the fixnum (+ rptr 1))))))))
                                (with-input-from-lexer ((var) &body body)
                                  ;; I love the old Common Lisp stream concepts.
                                  ;; ### Does this work with track-file-position-p on?
                                  `(progn
                                     (when (and interactive-p (not virgin-p))
                                       (read-char input)
                                       (setf virgin-p t))
                                     (with-input-from-string (str-stream saved-buffer
                                                                         :start saved-fin-ptr
                                                                         :end saved-fptr
                                                                         :index saved-fin-ptr)
                                       (let ((,var (make-concatenated-stream str-stream input)))
                                         ,@body)))))
                       (let ((ch 0))
                         (declare (type fixnum ch))
                         (tagbody
                           start
                                 (setf bptr     fin-ptr
                                       rptr     (1- fin-ptr))

                                 ,@dispatch)))))))))
      (let (bindings declarations)
        (when (parsed-lexer-has-condition lexer)
          ;;current condition we are in
          (push '(condition 1) bindings)
          (push `(type fixnum condition) declarations))
        (when (parsed-lexer-has-cur-bol lexer)
          (push '(cur-bol saved-fin-ptr) bindings) ;current beginning of line
          (push '(type fixnum cur-bol) declarations))
        (when (parsed-lexer-has-cur-line lexer)
          (push '(cur-line 1) bindings)
          (push '(type unsigned-byte cur-line) declarations))
        (when (parsed-lexer-has-ctx lexer)
          (push '(ctx 3) bindings)
          (push '(type fixnum ctx) declarations))
        ;; ### START and END must be validated!
        `(let* ((stringp (vectorp input))
                (interactive-p
                 (or interactive-p (and (not stringp) (interactive-stream-p input))))
                (virgin-p t #+NIL interactive-p)
                (saved-buffer-length (if interactive-p 128 8192))
                (saved-buffer (if stringp
                                  input
                                  (make-array saved-buffer-length
                                              :element-type ',element-type)))
                (saved-fptr    (if stringp (or ,end-form (length input)) 0))
                ;;
                ,@(when
                   *track-file-position-p*
                   ;; ### STRINGP
                   (list '(position-buffer (make-array (1+ saved-buffer-length)))))
                ;;
                (saved-fin-ptr          ;where we start matching again
                 (if stringp (or ,start-form 0) 0))
                ,@bindings)
           (declare (type fixnum saved-fptr saved-fin-ptr)
                    (ignorable virgin-p)
                    ,@declarations
                    (type fixnum saved-buffer-length))
           ,(if *template-closure-p*
                `(lambda (&optional action arg)
                   (declare (ignorable arg))
                   (cond ((null action)
                          ,main)
                         ;;
                         ((eq action :remaining-input-string)
                          (subseq saved-buffer saved-fin-ptr saved-fptr))
                         ((eq action :remaining-input)
                          (make-concatenated-stream
                           (make-string-input-stream (subseq saved-buffer saved-fin-ptr saved-fptr))
                           input)) ))
                (if (parsed-lexer-unread-sequence-fun lexer)
                    `(unwind-protect
                          ,main
                       (,(parsed-lexer-unread-sequence-fun lexer)
                         saved-buffer input
                         :start saved-fin-ptr
                         :end saved-fptr))
                    main)))))))

;;; Traking FILE-POSITION

;; To do this correctly and portable, we need to read characters at a
;; time and remember the file position for each. This however is
;; painfully slow. Anyhow, we have POSITION-BUFFER in parallel to
;; BUFFER, the only complication is that we want to report the end
;; position as exclusive.

(defun template-underflow-fun (lexer &key dec binaryp track-charactor-position-p)
  (let ((interactive-read
         (if (or binaryp (parsed-lexer-read-sequence-fun lexer))
             `(,(or (parsed-lexer-read-sequence-fun lexer) 'read-sequence) buffer input :start fptr :end (+ fptr 1))
             `(progn
                (unless virgin-p
                  ;; Consume the peeked char. EOF?
                  (read-char input nil))
                (let ((c (peek-char nil input nil nil)))
                  (cond ((null c)
                         ;; (setf virgin-p t)
                         fptr)
                        (t
                         (setf virgin-p nil)
                         (setf (aref buffer fptr) c)
                         (1+ fptr)))))))
        (tracking-interactive-read
         `(progn
            (unless virgin-p
              ;; Consume the peeked char. EOF?
              (read-char input nil))
            (let ((p (file-position input))
                  (c (read-char input nil nil)))
              ;; ### We really can do better!
              (setf (aref position-buffer fptr) p
                    (aref position-buffer (1+ fptr)) (file-position input))
              (cond ((null c)
                     ;; (setf virgin-p t)
                     fptr)
                    (t
                     (unread-char c input)
                     (setf virgin-p nil)
                     (setf (aref buffer fptr) c)
                     (1+ fptr))))))
        (buffered-read
         ;; BTW, where is READ-SEQUENCE-NO-BLOCK or sth. similar
         `(,(or (parsed-lexer-read-sequence-fun lexer) 'read-sequence) buffer input :start fptr :end buffer-length) ))
    `(underflow
      ()
      ,*track-file-position-p*
      (locally
          ,*template-optimize-settings*
        (cond (stringp 
               ;; (setf saved-fptr fptr)
               (incf rptr)              ;really? -- yes!
               (setf saved-fptr fptr)
               ,+end-of-file-sentinel+)
              (t
               (let ((ch 0))
                 (declare (type fixnum ch))
                 "a real underflow"
                 #-CLISP
                 "move stuff in our buffer to make some room for more characters"
                 #-CLISP
                 "but do not move beyond BPTR, which our beginning of the current"
                 #-CLISP
                 "token."
                 ;; Move stuff
                 (let ((keep ,(if *count-columns-p*
                                  (progn
                                    (assert (parsed-lexer-has-cur-bol lexer))
                                    '(min cur-bol bptr))
                                  'bptr)))
                   (declare (type fixnum keep))
                   ;; buffer[0:] = buffer[keep:fptr]
                   (loop for d of-type fixnum from 0
                         for s of-type fixnum from keep below fptr
                         do (setf (aref buffer d) (aref buffer s)))
                   ,(when *track-file-position-p*
                          `(loop for d of-type fixnum from 0
                                 for s of-type fixnum from keep below fptr
                                 do (setf (aref position-buffer d) (aref position-buffer s))))
                   "adjust pointers"
                   ;; hmm
                   (decf rptr keep)
                   (decf fptr keep)
                   (decf fin-ptr keep)
                   ,@(if (parsed-lexer-has-cur-bol lexer)
                         (list '(DECF CUR-BOL KEEP)))
                   (decf bptr keep)
                   ,@(and track-charactor-position-p
                         (list `(incf character-position-offset keep))))
                 ;;
                 (let ((avail (- buffer-length fptr)))
                   (when (< avail 100)
                     "make the buffer larger"
                     (let ((new-length (+ buffer-length (ceiling buffer-length 2))))
                       ,(when 
                         *track-file-position-p*
                         '(setf position-buffer (adjust-array position-buffer (+ new-length 3))))
                       (setf buffer (adjust-array buffer (+ new-length 2))
                             buffer-length new-length
                             saved-buffer buffer
                             saved-buffer-length buffer-length)))
                   (let ((end
                          ,(cond (*track-file-position-p*
                                  tracking-interactive-read)
                                 (t
                                  `(cond (interactive-p
                                          ,interactive-read)
                                         (t
                                          ,buffered-read))))))
                     (cond ((= end fptr)
                            "end of file"
                            (incf rptr) ;really? -- yes!
                            (setf saved-fptr fptr)
                            ,+end-of-file-sentinel+)
                           (t
                            (setf saved-fptr (setf fptr end))
                            (setf ch ,(funcall dec '(aref buffer rptr)))
                            (setf rptr (the fixnum (+ rptr 1)))
                            ch)))))))))) )


;;;; ------------------------------------------------------------------------------------------

(defun test-lexer (fun input)
  (let ((fun (funcall fun input)))
    (loop for i from 0
          for x = (multiple-value-list (funcall fun))
          do
          (format t "~&~{~S~^ ~}" x)
          (when (> i 10) (sleep 1))
          until (eq (car x) :eof) )
    (fresh-line)
    (values)))


;;;; -- Debugging Aids ------------------------------------------------------------------------

;; SHOW-BACKUP:

;; Basically this would identify every state, which is non-accepting
;; and pick characters, which would leave to that state.


;;;; -- CLEX-1 Compatibility ------------------------------------------------------------------

;; I want to use this as a CLEX-1 plug-in replacement.


;;;; -- Scanning ------------------------------------------------------------------------------

(defmacro lexing ((string-or-stream &key (start nil) (end nil)) &body clauses)
  "Inline scanning. The string-or-stream argument is scanned for the
  patterns specified like a LEXER does."
  (let ((*count-lines-p* nil)
        (*count-columns-p* nil)
        (*template-closure-p* nil))
    (mytime
     (lexer-aux (list string-or-stream)
                clauses
                :start-form start :end-form end))))

(defun form-all-$n-symbol-map (form &aux res)
  "Returns an alist mapping integers `n' to all symbols named `$n' in the
s-expression `form'. For ease, we map `$$' as n=0."
  (loop for sym in (all-symbols-in-form form)
        for n = ($n-symbol-p sym) do
        (when n
          (pushnew sym (cdr (or (assoc n res)
                                (car (push (list n) res)))))))
  res)

#+SBCL
(defun all-symbols-in-form (form &aux res (orig *print-pprint-dispatch*))
  "Collect all symbols in the s-expression `form'. Glorious hack needed
because of SBCL. Thanks."
  (handler-bind
      ((error (lambda (cond)
                (let ((*print-pprint-dispatch* orig))
                  (error cond)))))
    (with-standard-io-syntax 
      (let ((default-table (copy-pprint-dispatch nil))
            (*print-pprint-dispatch* (copy-pprint-dispatch nil))
            (*print-pretty* t)
            (*print-circle* t))
        (set-pprint-dispatch 't (lambda (stream object)
                                  (if (symbolp object)
                                      (pushnew object res)
                                      (funcall (pprint-dispatch object default-table) stream object))))
        (pprint form (make-broadcast-stream))
        res))))

#-SBCL
(defun all-symbols-in-form (form &aux res)
  "Sane version for sane Lisps."
  (labels ((walk (x)
             (cond ((symbolp x) (pushnew x res))
                   ((atom x))
                   (t
                    (walk (car x))
                    (walk (cdr x))))))
    (walk form)
    res))

(defun $n-symbol-p (symbol)
  "Does the symbol `symbol` have a name like $<n>, <n> some positive
integer? Returns that `n' or NIL."
  (and (symbolp symbol)
       (not (keywordp symbol))
       (let ((name (string symbol)))
         (cond ((and (> (length name) 1)
                     (char= #\$ (char name 0))
                     (char/= #\0 (char name 1))
                     (every #'digit-char-p (subseq name 1)))
                (parse-integer name :start 1))
               ((string= symbol "$$")
                0)))))


;;;; -- L'esprit de l'escalier ----------------------------------------------------------------

(defun sre-equal (r s)
  "Do the two SREs /r/ and /s/ describe the very same language?"
  (sre-empty-p `(or (- ,r ,s) (- ,s ,r))))

(defun sre-less-p (r s)
  "Is R a subset of S?"
  (sre-empty-p `(- ,r ,s)))

(defun sre-empty-p (r)
  "Does the SRE /r/ denote the empty language?"
  (let ((dfa (re-dfa r :has-lookahead nil :has-condition-dispatch nil)))
    (notany #'(lambda (q) (dfa-state-exit-transition dfa q))
            (dfa-states dfa))))

;;;; ------------------------------------------------------------------------------------------

#-(AND)
(defun sre-rename-groups (sre macros &key (syntax :flex))
  "The task of this function is to take an SRE and rename variable capture,
  so that all capture in the resulting SRE are named by consecutive
  non-negative integers. A second value gives the mapping of these new group
  names to user names."
  ;; -> sre ; tagmap
  ;; ### This is not the right place to do this.
  ;;
  ;; However the tagmap is the user name of each submatch. Submatches are named
  ;; from zero on, no matter what. The n'th element of `tagmap` corresponds to
  ;; the name of the `n`th submatch we have. This name could also be an integer
  ;; naming $n. Note: The submatch numbers of the SRE we return must not match
  ;; that map. E.g:
  ;;
  ;; (SRE-RENAME-GROUPS '(AND (= X "(x)") (= Y "(y)")))
  ;; => (AND (= 0 (= 1 #\x)) (= 2 (= 3 #\y))); (X 1 Y 2)
  ;;
  ;; That is groups of ()'s get renamed.
  ;;
  (let ((tagmap nil)
        (next-group-number 1))          ;next number for FLEX syntax group
    (declare (special integer-ok-p))
    (labels ((cons-and (xs)
               (cond ((= 1 (length xs)) (car xs))
                     (t `(and ,@xs))))
             ;;
             (name-user-tag (tag)
               (cond ((integerp tag)
                      ;; (setf user-name (intern-$n user-name)) ;### for now
                      ;; And: Where does this popup at all? *scratchhead* It's only SCAN which even
                      ;; really looks at this tagmap. This has to improve, so that we could skip
                      ;; INTERN-$N entirely.
                      (prog1 (length tagmap)
                        (let ((user-name (1- (incf next-group-number))))
                          (setf tagmap (nconc tagmap (list user-name))))))
                     ((symbolp tag)
                      (or (and (not (integerp tag)) (position tag tagmap))
                          (prog1
                              (length tagmap)
                            (setf tagmap (nconc tagmap (list tag))))))
                     (t
                      (assert nil))))
             ;;
             (walk (x yet integer-ok-p &aux it)
               (let ((res nil))
                 (setf res
                 (cond ((stringp x)
                        (walk (parse-re x :syntax syntax)
                              yet t))
                       ;; look for macros
                       ((setf it (or (and (symbolp x) (assoc x macros))
                                     ;;(and (consp x) (assoc (car x) macros))
                                     (and (consp x) (eq (car x) :macro)
                                          (assoc (cadr x) macros :test #'string-equal))))
                        (cond ((member (car it) yet)
                               (compiler-warn sre "Recursive SRE macro: ~S." (car it))
                               '(or))
                              (t
                               (walk (cdr it) (cons (car it) yet) integer-ok-p))))
                       ;;
                       ((symbolp x)
                        x)
                       ((atom x)
                        x)
                       ;;
                       ((eq (car x) 'quote)
                        (cond ((stringp (cadr x))
                               (cadr x))
                              (t
                               (compiler-warn sre "QUOTE is supposed be used with a string.")
                               '(or))))
                       ;;
                       ((member (car x) '(VECTOR CNOT SETQ))
                        ;; ### not very orthogonal!
                        ;; ### And we get a bad error message or no indication of what is wrong.
                        (compiler-warn sre "Bad SRE"))
                     
                       ;; assignments
                       ((eq (car x) '=)
                        (destructuring-bind (tag &rest sub) (cdr x)
                          (cond ((or (eql 0 tag) (and (integerp tag) integer-ok-p yet))
                                 ;; skip this, it is the top level group
                                 ;; introduced by PARSE-RE or integer groups
                                 ;; from FLEX regular expressions within
                                 ;; macros.
                                 (cons-and (mapcar #'(lambda (x) (walk x yet integer-ok-p)) sub)))
                                ((and (integerp tag) (not integer-ok-p))
                                 ;; barf
                                 (compiler-warn x "Group capture variables must be symbols.")
                                 `(and ,@(mapcar #'(lambda (x) (walk x yet integer-ok-p)) sub)))
                                ;;
                                ((not (or (integerp tag) (symbolp tag)))
                                 (compiler-warn x "Group capture variables must be symbols.")
                                 `(and ,@(mapcar #'(lambda (x) (walk x yet integer-ok-p)) sub)))
                                ;;
                                (t
                                 (list* '= (name-user-tag tag)
                                        (mapcar #'(lambda (x) (walk x yet integer-ok-p)) sub))))))
                       (t
                        (let ((r (cons (car x) (mapcar #'(lambda (x) (walk x yet integer-ok-p)) (cdr x)))))
                          (if (equal r x)
                              x
                              r)))))
                 (when (null res)
                   (error "~S -> ~S" x res))
                 res)
               ))
      ;; prefix, suffix, :bol, :eol?
      (values (walk sre nil nil) tagmap))))

#-(AND)
(defmacro isum-case (var &body clauses)
  ;; A variation on the theme, actually this is of more general use, since
  ;; Common Lisp implementations lack a jump table based implementation of
  ;; CASE.
  `(cond
     ,@(mapcar (lambda (clause)
                 (destructuring-bind (isum &rest body) clause
                   (if (eql isum 't)
                       `(t ,@body)
                       `((or ,@(loop for (from below) on isum by #'cddr
                                     collect (if (= (1- below) from)
                                                 `(= ,from ,var)
                                                 `(and (<= ,from ,var) (< ,var ,below)))))
                         ,@body))))
               clauses)))

;;;; ------------------------------------------------------------------------------------------

;;; Syntax TODO

(define-condition bad-pattern (parse-error simple-condition)
  ((string   :initarg :string   :reader bad-pattern-string)
   (position :initarg :position :reader bad-pattern-position))
  (:report (lambda (condition stream)
             (format stream "Bad regular expression; ~?~%~A~@[~%~v<~>^~]"
                     (simple-condition-format-control condition)
                     (simple-condition-format-arguments condition)
                     (bad-pattern-string condition)
                     (bad-pattern-position condition)))))

(defun parse-re (string &rest args
                 &key start end syntax ignore-space case-sensitive quote
                      dot-includes-newline)
  (declare (ignore start end syntax ignore-space case-sensitive quote
                   dot-includes-newline))
  "Parses a regular expression given by a string into a symbolic regular expression (sre).

   Syntax Overview

       rs       concatenation
       r|s      disjunction
       r*       Kleene star
       r+       one or more
       r?       zero or more
       .        any character                   [*]
       [...]    character set                   [*]
       ^        beginning of line or input      [*]
       $        end of line or input            [*]
       \<       beginning of word
       \>       end of word
       \b       word boundary
       \B       not a word boundary

       \a       BEL character
       \e       ESC character
       \f       FF character
       \n       LF character
       \r       CR character
       \t       TAB character
       \xh..    hexadecimal
       \x{hh..} hexadecimal

       \d       [[:digit:]]
       \D       [^[:digit:]]
       \s       [[:space:]]
       \S       [^[:space:]]
       \w       [[:alnum:]_]
       \W       [^[:alnum:]_]

       \A       beginning of input
       \z       end of input
       \Z       optional #\\Newline at end of input

       \Q .. \E literal text

       (?x:)    ignore white space
       (?#..)   comment
       (?:..)   non-capturing group
       (?i:)    ignore case

       (?m:...)         make ^ and $ match at beginning and end of line also
       (?-m:...)        make ^ and $ match at beginning and end of input only

       (?s:)            make . match #\newline too
                        make [^..] match #\newline too

       (?=...)   lookahead
       (?<=...)  lookbehind
  "
  (apply #'%parse-re string args))

(defun %parse-re (string &key (start 0) (end nil) (syntax :extended)
                             (ignore-space nil)
                             (case-sensitive t)
                             (quote nil)
                             ((group-counter group-counter) 0)
                             ((topmost-group-p topmost-group-p) t)
                             (dot-includes-newline t))
  (check-type syntax (member :basic :extended :literal))
  (setq end (or end (length string)))
  (check-type start integer)
  (check-type end integer)
  (assert (<= 0 start end (length string)))
  (when (eq syntax :literal)
    (return-from %parse-re `(= 0 ,(if quote `',(subseq string start end) (subseq string start end)))))
  (let ((p start)
        (la nil)
        (val nil)
        ;; (compact t)
        (safe t)
        (flags (nconc (if ignore-space (list #\x))
                      (if case-sensitive nil (list #\i))
                      (if (not dot-includes-newline) (list #\m))
                      (if dot-includes-newline (list #\s)) )))
    (declare (special p flags))         ;sic!
    (labels
        ((blame (format-control &rest args)
           (let ((p (max start (1- p))))
             (error 'bad-pattern
                    :format-control format-control
                    :format-arguments args
                    :string (subseq string start end)
                    :position (- p start))))
         (note (format-control &rest args)
           (let ((p (max start (1- p))))
             (warn "~?~%~A~%~v<~>^"
                   format-control args
                   (subseq string start end) (- (max start (1- p)) start))))
         ;;
         (getch ()
           (when (>= p end)
             (blame "Premature end of regular expression."))
           (char string (1- (incf p))))
         (peekch (&optional (d 0))
           (and (< (+ p d) end) (char string (+ p d))))
         ;;
         (caret ()
           (if (member #\m flags) ':bol ':bof))
         (dollar ()
           (if (member #\m flags) ':eol ':eof))
         (dot ()
           (if (member #\s flags)
               't
               '(- t #\newline)))
         (newline-special-in-char-bag-p ()
           (not (member #\s flags)))
         ;;
         (consume ()
           (let ((c (if (< p end) (getch) nil)))
             (setf (values la val)
                   (cond ((null c) 'fin)
                         ((and (member c '(#\space #\tab #\newline #\return #\page #.(code-char 11)))
                               (member #\x flags))
                          (consume))
                         ((eql #\[ c) (values #\a (parse-bracket-expr)))
                         ((find c "*.^$") (values c c))
                         ((and (not (eql :basic syntax))
                               (find c "?+|"))
                          (values c c))
                         ((eql #\\ c) (consume-escape))
                         ((and (not (eq :basic syntax))
                               (eql #\{ c))
                          (consume-brace))
                         ;; (?# ...)  --comment
                         ((and (not (eq :basic syntax))
                               (eql #\( c)
                               (eql #\? (peekch 0))
                               (eql #\# (peekch 1)))
                          (do () ((eql #\) (getch))))
                          (consume))
                         ;; (?= ...)    epitext
                         ((and (not (eq :basic syntax))
                               (eql #\( c)
                               (eql #\? (peekch 0))
                               (eql #\= (peekch 1)))
                          (getch)
                          (getch)
                          (values :|(?=| nil))
                         ;; (?<= ...)    pretext
                         ((and (not (eq :basic syntax))
                               (eql #\( c)
                               (eql #\? (peekch 0))
                               (eql #\< (peekch 1))
                               (eql #\= (peekch 2)))
                          (getch) (getch) (getch)
                          (values :|(?<=| nil))
                         ;;
                         ((and (not (eq :basic syntax))
                               (eql #\( c)
                               (eql #\? (peekch)))
                          (getch)
                          (let ((new-flags (parse-flags)))
                            (case (getch)
                              (#\:
                               (values :|(?| new-flags))
                              (#\)
                               (setq flags new-flags)
                               (consume))
                              (otherwise
                               (blame "':' or ')' expected"))
                              )))
                         ((and (not (eq :basic syntax))
                               (find c "()"))
                          c)
                         (t
                          (values #\a c))))))
         ;;
         (consume-escape ()
           (let (c)
             (ecase syntax
               (:extended
                (case (setq c (getch))
                  (#\< (values #\a :bow))
                  (#\> (values #\a :eow))
                  (#\b (values #\a `(or :bow :eow)))
                  (#\B (values #\a :nwb))
                  ;;
                  (#\a (values #\a (code-char 7)))
                  (#\e (values #\a (code-char 27)))
                  (#\f (values #\a #\page))
                  (#\n (values #\a #\newline))
                  (#\r (values #\a #\return))
                  (#\t (values #\a #\tab))
                  (#\x (consume-slash-x))
                  ;;
                  (#\d (values #\a (ctype-sre "digit")))
                  (#\D (values #\a (neg-ctype-sre "digit")))
                  (#\s (values #\a (ctype-sre "space")))
                  (#\S (values #\a (neg-ctype-sre "space")))
                  (#\w (values #\a (wordchar-sre)))
                  (#\W (values #\a (neg-wordchar-sre)))
                  (#\Q (consume-slash-q))
                  ;;
                  (#\A (values #\a ':bof))
                  (#\z (values #\a ':eof))
                  (#\Z (values #\a `(:epitext (? #\Newline) %eof)))
                  ;;
                  ((#\^ #\. #\[ #\$ #\( #\) #\| #\* #\+ #\? #\{ #\\
                        ;; hmm, these are UB:
                        #\} #\]
                        )
                   (values #\a c))
                  ;;
                  (otherwise
                   (blame "Unknown escape character")
                   (values #\a c))))
               (:basic
                (case (setq c (getch))
                  (#\( #\()
                  (#\) #\))
                  (#\{ (consume-brace))
                  ((#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
                   (values #\a `(:backref (digit-char-p c))))
                  (otherwise
                   (values #\a c)))))))
         ;;
         (consume-slash-q ()
           (let ((p2 (search "\\E" string :start2 p :end2 end)))
             (unless p2
               (blame "Missing \\E after \\Q"))
             (multiple-value-prog1
                 (values #\a (subseq string p p2))
               (setq p (+ p2 2)))))
         ;;
         (consume-slash-x ()
           (cond ((eql #\{ (peekch))
                  (let ((p2 (position #\} string :start (+ p 1) :end end)))
                    (unless p2 (blame "After \\x{ we miss }"))
                    (unless (every (lambda (c) (digit-char-p c 16))
                                   (subseq string (+ p 1) p2))
                      (blame "Bad: \\x{~A}" (subseq string (+ p 1) p2)))
                    (multiple-value-prog1
                        (values #\a (code-char (if (= p2 (1+ p))
                                                   0
                                                   (parse-integer string :start (+ p 1) :end p2 :radix 16))))
                      (setq p (+ p2 1)))))
                 (t
                  (let ((p2 (or (position-if-not (lambda (c) (digit-char-p c 16))
                                                 string :start p :end end)
                                end)))
                    (multiple-value-prog1
                        (values #\a (code-char
                                     (if (= p p2) 0 (parse-integer string :start p :end p2 :radix 16))))
                      (setq p p2))))))
         ;;
         (ctype-sre (ctype)
           (let ((class (assoc ctype +ascii-ctype+ :test 'equal)))
             (unless class
               (blame "Character class ~S unknown." ctype))
             (cadr class)))
         (neg-ctype-sre (ctype)
           `(- t ,(ctype-sre ctype) ,@(if (newline-special-in-char-bag-p) '(#\newline) nil)))
         (wordchar-sre ()
           `(or ,(ctype-sre "alnum") #\_))
         (neg-wordchar-sre ()
           `(- t ,(wordchar-sre) ,@(if (newline-special-in-char-bag-p) '(#\newline) nil)))
         ;;
         (consume-brace (&aux p2 p3 min max)
           (let ((close-delimiter (if (eq syntax :basic) "\\}" "}")))
             (cond ((not (setq p2 (search close-delimiter string :start2 p :end2 end)))
                    (blame "Unclosed ~S." #\{))
                   #+(or)
                   ((string= "-" string :start2 p :end2 p2)
                    (setq p (+ p2 (length close-delimiter)))
                    (values :|{-}|))
                   ((string= "&" string :start2 p :end2 p2)
                    (setq p (+ p2 (length close-delimiter)))
                    (values #\&))
                   ((and (setf (values p3 min max) (maybe-parse-interval))
                         (= p2 p3))
                    (setq p (+ p2 (length close-delimiter)))
                    (values #\{ (list min max)))
                   ;; No luck
                   (t
                    (blame "Bad brace expression")))))
         ;;
         (maybe-parse-interval ()
           "Parses 'n', 'n,', ',m', or 'n,m'. The '{' is already
            consumed and the '}' is left at the stream."
           ;; -> p; min; max
           (let ((p p))
             (declare (special p))
             (let (min max w)
               (loop for c = (peekch)
                     while (setq w (and c (digit-char-p c)))
                     do (getch) (setf min (+ (* 10 (or min 0)) w)))
               (cond ((eql (peekch) #\,)
                      (getch)
                      (loop for c = (peekch)
                            while (setq w (and c (digit-char-p c)))
                            do (getch) (setf max (+ (* 10 (or max 0)) w)))
                      (when (or min max)
                        (values p (or min 0) (or max '*))))
                     (min
                      (values p min min))))))
         ;;
         (parse-flags (&aux (res flags) (minusp nil))
           (loop
             (let ((c (peekch)))
               (cond ((eql #\- c)
                      (getch) (setq minusp t))
                     ((and c (alpha-char-p c))
                      (case c
                        ((#\i #\x #\m #\s)
                         (getch)
                         (setq res (if minusp (remove c res) (adjoin c res))))
                        (t   (blame "Unknown flag character: ~S" (peekch)))))
                     (t
                      (return (reverse res)))))))
         ;;
         (parse-bracket-expr ()
           (let ((negatedp nil) (xs nil))
             (when (eql #\^ (peekch))
               (getch)
               (setf negatedp t))
             (loop for i from 0
                   for c = (getch) do
                   (cond ((and (eql c #\]) (not (zerop i)))
                          (return))
                         #+(or)
                         ((and (eq syntax :flex) (eql c #\\))
                          (push (get-flex-escape) xs))
                         ((and (char= c #\-) (or (null xs) (eql (peekch) #\])))
                          (push c xs))
                         ((and (char= c #\-) (characterp (car xs)))
                          (let ((c (getch)))
                            #+(or)
                            (when (and (eq syntax :flex) (eql c #\\))
                              (setf c (get-flex-escape)))
                            (unless (>= (char-code c) (char-code (car xs)))
                              (decf p)
                              (blame "Reverse or empty range"))
                            (setf (car xs) `(<= ,(car xs) ,c))))
                         ((char= c #\-)
                          (blame "Misplaced '-'"))
                         ((and (eql #\[ c) (member (peekch) '(#\. #\=)))
                          (blame "~A~A not supported." c (peekch)))
                         ((and (eql #\[ c)
                               (eql #\: (peekch)))
                          (let ((p2 (search ":]" string :start2 (+ 1 p) :end2 end)))
                            (unless p2
                              (blame "~S without closing ~S" "[:" ":]"))
                            (push (ctype-sre (subseq string (+ p 1) p2)) xs)
                            (setf p (+ p2 2))))
                         (t
                          (push c xs))))
             (when (and negatedp (newline-special-in-char-bag-p))
               (push #\newline xs))
             (setq xs (cons-assoc 'or xs))
             (if negatedp `(- t ,xs) xs)))
         ;;
         (expr ()
           (disjunction))
         ;;
         (disjunction ()
           (do ((res (concatenation) (cons-left-assoc 'or res (concatenation))))
               ((not (eql #\| la)) res)
             (consume)))
         ;;
         (concatenation ()
           (let ((terms nil))
             (when (and (eql syntax :basic)
                        (eql la #\^))
               ;; ### How to do for $?
               (consume)
               (push (caret) terms))
             (do ()
                 ((not (member la '(#\a #\. #\^ #\$ #\( :|(?| :|(?=| :|(?<=| #\*))))
               (push (iteration) terms))
             (cons-and (reverse terms))))
         ;;
         (iteration ()
           (let ((res (primary)))
             (loop
               (setq res (prog1 (case la
                                  (#\* `(* ,res))
                                  (#\+ `(+ ,res))
                                  (#\? `(? ,res))
                                  (#\{ `(** ,(car val) ,(cadr val) ,res))
                                  (t (return res)))
                           (consume))))))
         ;;
         (primary ()
           (prog1
               (case la
                 (#\a (if (member #\i flags) `(:ci ,val) val))
                 (#\. (dot))
                 (#\^ (caret))
                 (#\$ (dollar))
                 (#\*
                  (if (eql :basic syntax) la (blame "Nothing to iterate")))
                 (:|(?|
                   (let ((flags val))
                     (declare (special flags))
                     (prog2 (consume) (expr) (rparen))))
                 (:|(?=|
                   (prog2 (consume) `(:epitext ,(expr)) (rparen)))
                 (:|(?<=|
                   (prog2 (consume) `(:pretext (and (* t) ,(expr))) (rparen)))
                 (#\(
                  (let ((flags flags))
                    (declare (special flags))
                    (prog2 (consume) `(= ,(1- (incf group-counter)), (expr))
                      (rparen))))
                 (otherwise
                  (blame "Expected one of: a literal character, '.', '^', '$', or '('")))
             (consume)))
         ;;
         (rparen ()
           (case la
             (#\))
             ((#\+ #\* #\? #\{)      (blame "Nothing to iterate about"))
             (otherwise              (blame "Missing ')'"))))
         ;;
         (cons-left-assoc (op lhs rhs)
           (cond ((and (consp lhs) (eq (car lhs) op))
                  (cond ((null (cdr lhs))
                         rhs)
                        (t `(,op ,@(cdr lhs) ,rhs))))
                 (t (list op lhs rhs))))
         ;;
         (cons-assoc (op terms)
           (cond ((null terms) `(,op))
                 ((null (cdr terms)) (car terms))
                 (t `(,op ,@terms))))
         ;;
         (cons-and (terms)
           (let ((res nil))
             (dolist (x terms)
               (cond ((and (or (stringp x)
                               (and (characterp x) (safe-p x)))
                           (or (stringp (car res))
                               (and (characterp (car res)) (safe-p (car res)))))
                      (setq res (cons (concatenate 'string
                                                   (string (car res))
                                                   (string x))
                                      (cdr res))))
                     (t
                      (push x res))))
             (setq res (reverse res))
             (when quote
               (setq res (mapcar (lambda (x) (if (stringp x) `',x x)) res)))
             (cond ((null res) "")
                   ((null (cdr res)) (car res))
                   (t `(and ,@res)))))
         ;;
         (safe-p (c)
           (or (not safe)
               (and (graphic-char-p c)
                    (not (find c "[]*.^$?+|\\(){}")))))
         ;;
         (toplevel ()
           (prog2
               (consume)
               (values (expr) p)
             (ecase la
               ((fin))
               ((#\))                  (blame "Unmatched ')'"))
               ((#\+ #\* #\? #\{)      (blame "Nothing to iterate about")))))
         ;;
         )
      (values
       (if topmost-group-p
           `(= ,(1- (incf group-counter)) ,(toplevel))
           (toplevel))
       group-counter) )))

;;;; -- TODO ----------------------------------------------------------------------------------

;; - Full list of Enhanced, PCRE, and flex syntax and any other we can find.
;;   We should include all the PCRE syntax in our parser.

;; - Fuzztest this. How? What frameworks exist?

;; Enhanced:

;; \<           :bow
;; \>           :eow
;; \b           :word-boundary
;; \B           (not :wb)
;; \d, \D, \s, \S, \w, \W
;; [[:<:]] and [[:>:]]

(defun install-hash-r ()
  (let ((orig (get-dispatch-macro-character #\# #\R (copy-readtable nil))))
    (set-dispatch-macro-character
     #\# #\r
     (lambda (stream subchar arg)
       (cond ((and (null arg)
                   (not (alphanumericp (peek-char nil stream))))
              (let ((d (read-char stream)))
                (parse-re
                 (with-output-to-string (bag)
                   (loop for c = (read-char stream)
                         until (eql c d)
                         do (write-char c bag)))
                 :syntax :extended)))
             (t
              (funcall orig stream subchar arg)))))))

;; Well, perhaps we make SRE-RE do this for us while renaming vars and
;; returning a mapping. I mean, we want them counted from 0.

;; (?!) - negative lookahead
;; (?=) - positive lookahead
;; (?<=) - positive lookbehind
;; (?<!) - negative lookbehind

;; (?= ... )    positive lookahead      (:epitext ...)

;;;; -- Notes ---------------------------------------------------------------------------------

;;; EOF

;; The never ending story. To us EOF is a character like any else. However we
;; need to be careful as in principle this may lead to buffer indicies beyond
;; the end of buffer, so we need to be careful. This is somewhat ensured by
;; "$" being an epitext and thus the EOF itself cannot be captured.


;;;;

(defun sre-variables (sre)
  (let ((res nil))
    (labels ((walk (x)
               (cond ((atom x))
                     ((eq (car x) '=)
                      (pushnew (cadr x) res)
                      (map nil #'walk (cdr x)))
                     (t
                      (map nil #'walk (cdr x))))))
      (walk sre)
      (reverse res))))


;;;;

(defmacro dynamic-bind (bindings &body body)
  (let ((vars (mapcar (lambda (b) (if (consp b) (car b) b)) bindings))
        (vals (mapcar (lambda (b) (if (consp b) (cadr b) nil)) bindings))
        (tmps (mapcar (lambda (b) (declare (ignore b)) (gensym)) bindings)))
    `(let ,(mapcar #'list tmps vars)
       (unwind-protect
            (progn
              (psetq ,@(mapcan #'list vars vals))
              (locally ,@body))
         (setq ,@(mapcan #'list vars tmps))))))

(defun process-sre (sre macros &rest options &key (top-group t) &allow-other-keys)
  (let ((varmap nil)
        (next-reg 0)
        (next-var 1)
        (nest nil))
    (setq options (copy-list options))
    (remf options :top-group)
    (labels ((walk (sre)
               (setq sre (sre-macroexpand sre macros))
               (typecase sre
                 (string
                  (if nest
                      sre
                      (dynamic-bind ((nest t))
                        (setf (values sre next-var)
                              (apply #'%parse-re sre
                                     'group-counter next-var
                                     'topmost-group-p nil
                                     options))
                        (walk `(:cs ,sre)))))
                 (atom sre)
                 ((cons (member =) (cons t t))
                  (destructuring-bind (var &rest terms) (cdr sre)
                    `(= ,(name-var var) ,@(mapcar #'walk terms))))
                 ((cons (member <=) (cons t (cons t null)))
                  sre)
                 ((cons (member quote) (cons t null))
                  (cadr sre))
                 ((cons (member setq) t)
                  sre)
                 (t
                  (cons (car sre) (mapcar #'walk (cdr sre))))))
             (name-var (v)
               (or (position v varmap)
                   (prog1 (length varmap)
                     (setq varmap (nconc varmap (list v)))))))
      (name-var 0)
      (values (if top-group
                  `(= ,(name-var 0) ,(walk sre))
                  (walk sre))
              varmap
              next-reg))))


;;;; -- SRE Macros ----------------------------------------------------------------------------

#||

(defmacro define-sre-macro (name lambda-list &body body)
  )

(defmacro sre-macrolet (bindings &body body)
  )

(defun sre-macro-function (symbol &optional env)
  )

||#



;;;; ------------------------------------------------------------------------------------------

(defun sre-macro-function (symbol &optional env)
  (let ((q (assoc symbol env)))
    (when q
      #'(lambda (form env)
          (declare (ignore form env))
          (cdr q)))))

(defun sre-macroexpand (form &optional env &aux (orig-form form))
  (let ((deja-vu nil) work-done-p)
    (loop
      (setf (values form work-done-p)
            (sre-macroexpand-1 form env))
      (when (null form)
        (error "? ~S" orig-form))
      (unless work-done-p (return form))
      (when (member form deja-vu :test 'equal)
        (error "I am having a deja vu: ~S" form))
      (push form deja-vu))))

(defun sre-macroexpand-1 (form &optional env)
  '(sleep .1)
  (cond ((symbolp form)
         (let ((fun (sre-macro-function form env)))
           (if fun
               (values (funcall fun (list form) env) t)
               (values form nil))))
        ((atom form) form)
        ((and (consp form) (symbolp (car form)))
         (let ((fun (sre-macro-function (car form) env)))
           (if fun
               (values (funcall fun form env) t)
               (values form nil))))))

(defun dfa-jam-states (dfa)
  "Return a list of all jam states of the DFA /dfa/. Does not include states 1 and 2."
  ;; Now, identify the set of all states that eventually lead to some accepting
  ;; state. We begin with the states that have :EXIT transitions and keep
  ;; walking backwards. We Warshall's algorithm here, it always wins.
  (let* ((n (length (dfa-states dfa)))
         (r (make-array n :initial-element 0)))
    (declare (optimize (speed 3) (safety 0))
             (type fixnum n)
             (type (simple-array t (*)) r))
    (loop for q of-type fixnum below n do 
          (loop for tr in (dfa-state-transitions (dfa-state dfa q)) do
                (setf (svref r (transition-to tr))
                      (logior (svref r (transition-to tr)) (ash 1 q)))))
    (loop for k of-type fixnum below n do
          (loop for i of-type fixnum below n do
                (when (logbitp k (svref r i))
                  (setf (svref r i) (logior (svref r i) (svref r k))))))
    (loop for i from 3 below n unless (logbitp i (svref r 2)) collect i)))

(defun dfa-consolidate-jam-states (dfa)
  (loop for q across (dfa-states dfa) do
        (setf (dfa-state-transitions q)
              (remove-if-not (lambda (tr)
                               (or (eq :exit (transition-sigma tr))
                                   (not (jam-state-p dfa (transition-to tr)))))
                             (dfa-state-transitions q))))
  dfa)

(defun cleanup-dfa (dfa)
  (setq dfa (dfa-deep-copy dfa))
  #-(or)
  (progn
    ;; First of all we remove states that are not reachable.
    (let ((deja nil))
      (labels ((walk (q)
                 (unless (member q deja)
                   (push q deja)
                   (loop for tr in (dfa-state-transitions (dfa-state dfa q)) do
                         (walk (transition-to tr))))))
        (loop for q below 3 do (walk q)))
      (let ((dead
             (loop for q below (length (dfa-states dfa))
                   unless (member q deja)
                   collect q)))
        (when dead
          ;; This really could not happen
          (warn "Dead states: ~S" dead)))))
  ;; Remove all jam transitions
  (let ((jam-states (dfa-jam-states dfa)))
    '(when jam-states
      (warn "Jam states: ~S~%~S" jam-states *last-test*))
    ;; Clear out jam states entirely
    (dolist (q jam-states)
      (setf (dfa-state-transitions (dfa-state dfa q)) nil))
    ;; Remove all jamming transitions
    (dotimes (q (length (dfa-states dfa)))
      (setf (dfa-state-transitions (dfa-state dfa q))
            (remove-if (lambda (tr)
                         (member (transition-to tr) jam-states))
                       (dfa-state-transitions (dfa-state dfa q))))) )
  dfa)


;;;; ------------------------------------------------------------------------------------------

;;; Scanning

;; When doing grep by scanning, we need another starting state. Actually
;; two of them. One for the initial pass through the scanning loop with
;; :BOF asserted and one for the remaing interations with the previous
;; character asserted.

;;;; DFA Code Generation

;; We generate code for matching a single DFA from a string here.

;; Flag to be set when we ever set the WON variable. This happens if there is
;; any :EXIT transition with other clauses present. Set by TRANSLATE-STATE if
;; needed.
(defvar *won-needed*)

(defun translate-expression (expr env)
  "Translate the expression given in some \sigma to a CL expression."
  (etypecase expr
    ((or symbol integer)
     (cdr (or (assoc expr env)
              (error "Ouch, we don't know where ~S is put.~%env = ~S" expr env))))
    ((cons (eql quote) (cons t null))
     (cadr expr))
    ((cons (member + -) (cons t (cons integer null)))
     `(the (integer 0 ,array-total-size-limit)
           (,(car expr)
             ,(translate-expression (cadr expr) env)
             ,(caddr expr))))))

(defun translate-subst (subst env)
  (and subst
       (list
        (translate-psetq (mapcan (lambda (q)
                                   (list (translate-expression (car q) env)
                                         (translate-expression (cdr q) env)))
                                 subst)))))

;;; PSETQ Optimizations

;; Common Lisp compilers are not too clever about removing redundant renaming
;; which raises from PSETQ. Usually copy propagation would kick in, but copy
;; propagation doesn't remove the extra A* with:

;;     (LET ((A ..) A*) (LOOP (SETQ A* (FOO A)) (SETQ A A*)))

;; Hence we need to do that on our own. The task basically is to reorder the
;; moves so that we need as few temponaries as possible.

;; We might contemplate to apply this optimization which appearently has no
;; name even to the whole machine, we'll see.

;; Here is an example:
;;   (PSETQ R7 P.0 R5 P.0 R2 R7 R8 P.0)
;; = (SETQ R5 P.0 R2 R7 R7 P.0 R8 P.0)

#+(or)
;; Something is not working quite right here!
(defun translate-psetq (pairs)
  "Optimize a PSETQ list of pairs. Does not care at all for the return value or order otherwise."
  (labels
      ((form-uses-var (form var)
         (cond ((eq form var) t)
               ((atom form) nil)
               ((some #'(lambda (x) (form-uses-var x var)) form)))))
    (let* ((su (loop for (d s) on pairs by #'cddr collect (cons d s)))
           (tmps nil)
           (free-tmps nil)
           (yet nil))
      ;; This is kind of greedy, find one pair which doesn't do any harm then commit
      ;; it and rinse and repeat. If out of luck, pull a temponary.
      (do () ((null su))
        (let ((ds (loop for ds in su do
                        (unless (some (lambda (other)
                                        (form-uses-var (cdr other) (car ds)))
                                      su)
                          (return ds)))))
          (cond ((null ds)
                 (let ((tmp (or (pop free-tmps)
                                (car (push (gensym "TMP.") tmps)))))
                   (setq ds (pop su))
                   (push (cons tmp (cdr ds)) yet)
                   (push (cons (car ds) tmp) su)))
                (t
                 (when (member (cdr ds) tmps) (push (cdr ds) free-tmps))
                 (push ds yet)
                 (setq su (remove ds su))))))
      (labels ((build (su bound)
                 (cond ((null su) nil)
                       ((and (member (caar su) tmps)
                             (not (member (caar su) bound)))
                        `(LET ((,(caar su) ,(cdar su)))
                           ,(build (cdr su) (cons (caar su) bound))))
                       (t
                        (let ((more (build (cdr su) bound)))
                          (cond ((typep more '(cons (eql SETQ) t))
                                 `(SETQ ,(caar su) ,(cdar su) ,@(cdr more)))
                                ((null more)
                                 `(SETQ ,(caar su) ,(cdar su)))
                                (t
                                 `(PROGN (SETQ ,(caar su) ,(cdar su)) ,more))))))))
        (build (reverse yet) nil)))))

(defun translate-psetq (pairs)
  `(psetq ,@pairs))

(defun translate-goto (to-state subst advance env)
  `(progn ,@(translate-subst subst env)
          ,@(and advance (list (translate-advance env)))
          (go ,(intern-Qn to-state))))

(defun isum-char-test (s c)
  (setq s (isum-intersection s (isum-range 0 char-code-limit)))
  (let ((ns (isum-difference (isum-range 0 char-code-limit) s)))
    (cond ((and (= 2 (length ns))
                (integerp (car ns))
                (integerp (cadr ns))
                (= (car ns) (1- (cadr ns))))
           `(/= ,c ,(car (isum-difference (isum-range 0 char-code-limit)
                                          s))))
          (t
           (let ((xs (loop for (from below) on s by #'cddr collect
                           (cond ((= from (1- below))
                                  `(= ,c ,from))
                                 ((= from 0)
                                  (cond ((= below char-code-limit) 't)
                                        (t `(<= ,c ,(1- below)))))
                                 ((= below char-code-limit)
                                  `(>= ,c ,from))
                                 (t
                                  `(<= ,from ,c ,(1- below)))))))
             (cond ((member 't xs) 't)
                   (t (case (length xs)
                        (0 'nil)
                        (1 (car xs))
                        (t `(or ,@xs))))))))))

(defun translate-advance (env)
  `(setq ,(translate-expression 'p env)
         (the (integer 0 ,array-total-size-limit)
              (+ ,(translate-expression 'p env) 1))))

(defun translate-state (dfa state output-registers env)
  (let ((p (translate-expression 'p env))
        (end (translate-expression 'end env))
        (buffer (translate-expression 'buffer env))
        (won (translate-expression 'won env))
        (c (translate-expression 'curchar env)))
    (let* ((eof-transition (find-if (lambda (tr)
                                      (and (consp (transition-sigma tr))
                                           (isum-member +end-of-file-sentinel+ (transition-sigma tr))))
                                    (dfa-state-transitions state)))
           ;; Now, there isn't anything beyond EOF, so when we have an EOF
           ;; transition pull the :EXIT transition from the next state, if any.
           (eof-exit
            (and eof-transition (dfa-state-exit-transition dfa (transition-to eof-transition))))
           ;;
           (exit-transition
            (find ':exit (dfa-state-transitions state) :key #'transition-sigma))
           ;;
           (char-trs
            (dfa-state-char-transitions state))
           ;;
           (clauses
            (remove nil
                    (let ((yet +isum-nothing+))
                      (loop for tr in char-trs
                            for s = (transition-sigma tr)
                            collect
                            `(,(isum-char-test (isum-union yet s) c)
                               ,(translate-goto (transition-to tr)
                                                (transition-subst tr)
                                                (not (member (transition-to tr) '(1 2)))
                                                env))
                            do (setq yet (isum-union yet s))))
                    :key 'car)))
      ;;
      `(,@(when exit-transition
                (setq *won-needed* t)
                (list
                 `(psetq ,@(loop for o in output-registers
                                 collect (translate-expression o env)
                                 collect (let ((q (assoc o (transition-subst exit-transition))))
                                           (if q
                                               (translate-expression (cdr q) env)
                                               'nil)))
                         ,won t)))
          ,@(and (or clauses eof-transition)
                 `((when (= ,p ,end)
                     ,@(cond ((null eof-transition)
                              (list `(go ,(intern-Qn 1)))
                              )
                             ((null eof-exit)
                              (list (translate-goto (transition-to eof-transition)
                                                    (transition-subst eof-transition)
                                                    nil
                                                    env)))
                             (t
                              ;; (error "what are we doing here?")
                              `(,@(translate-subst (transition-subst eof-transition) env)
                                  ;;
                                  ,(translate-advance env)
                                  ;;
                                  ,@(translate-subst (transition-subst eof-exit) env)
                                  (go ,(intern-Qn 2))))))))
          ,@(and (block foo
                   ;; Don't look!
                   (subst-if nil (lambda (x) (when (eq x c) (return-from foo t))) clauses)
                   nil)
                 (list `(setq ,c (char-code (schar ,buffer ,p)))))
          ,@(and clauses
                 (list
                  (apply #'cg-cond clauses)))))))

(defvar *code*)
(defvar *translate-dfa*)                ;debugging aid

(defun translate-dfa (dfa output-registers &key (use-key-args nil))
  (let ((*gensym-counter* 0))
    (setq *code*
          (let* (*won-needed*
                 ;;
                 (p       #+(or) (gensym "P.") #-(or) 'p)
                 (start   #+(or) (gensym "START.") #-(or) 'start)
                 (end     #+(or) (gensym "END.") #-(or) 'end)
                 (won (gensym "WON."))
                 (buffer (gensym "BUFFER."))
                 (len (gensym "LENGTH."))
                 (c (gensym "C."))
                 (out output-registers)
                 (regs (loop for r in (union out (dfa-all-registers dfa)) collect
                             (etypecase r
                               ((or symbol integer)
                                (cons r (register-var r))))))
                 (env (list* (cons 'p p)
                             (cons 'end end)
                             (cons 'buffer buffer)
                             (cons 'won won)
                             (cons 'curchar c)
                             regs)))
            ;;
            `(lambda ,(if use-key-args
                          `(,buffer &key ((:start ,start) 0) ((:end ,end) nil))
                          `(,buffer &optional (,start 0) (,end nil)))
               #+SBCL (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
               (check-type ,buffer ,*string-type*)
               (locally
                   (declare (type ,*string-type* ,buffer))
                 (let ((,len (length ,buffer)))
                   (declare (type (integer 0 ,array-total-size-limit) ,len))
                   (setq ,end (or ,end ,len))
                   (check-type ,start (integer 0 ,array-total-size-limit))
                   (check-type ,end (integer 0 ,array-total-size-limit))
                   (assert (<= 0 ,start ,end ,len)))
                 (let ((,buffer ,buffer)
                       (,p ,start)
                       (,end ,end)
                       (,c 0))
                   (declare (type ,*string-type* ,buffer)
                            (type (integer 0 ,array-total-size-limit) ,p ,end ,start)
                            (type (integer -2 (,char-code-limit)) ,c)
                            (ignorable ,c)
                            (ignorable ,buffer ,p ,end)
                            (optimize (speed 3) (safety 0)))
                   ,(simp-prog
                     `(prog ((,won nil) ,@(mapcar (lambda (r)
                                                    `(,(cdr r) -1))
                                                  regs))
                              (declare (ignorable ,won ,@(mapcar #'cdr regs))
                                       (type (integer -1 ,array-total-size-limit)
                                             ,@(mapcar #'cdr regs)))
                              ;; When scanning, we need an overall loop incrementing the position we look at.
                              ,@(when *scanning-mode-p*
                                      (let ((bof-transition
                                             (find-if (lambda (tr)
                                                        (and (consp (transition-sigma tr))
                                                             (isum-member +beginning-of-file-sentinel+
                                                                          (transition-sigma tr))))
                                                      (dfa-state-transitions (dfa-state dfa 3))))
                                            #+(or)
                                            (eof-tr
                                             (find-if (lambda (tr)
                                                        (and (consp (transition-sigma tr))
                                                             (isum-member +end-of-file-sentinel+
                                                                          (transition-sigma tr))))
                                                      (dfa-state-transitions (dfa-state dfa 3))))
                                            (non-bof-dispatch
                                             (translate-state dfa (dfa-state dfa 3) out env)))
                                        `(start
                                          ;; The initial dispatch on :BOF
                                          ,@(and bof-transition
                                                 (cg-progn*
                                                  (list (translate-goto (transition-to bof-transition)
                                                                        (shift-subst (transition-subst bof-transition) -1)
                                                                        nil
                                                                        env))))
                                          ;; Any further dispatch
                                          scan-loop
                                          ,@(cond ((null non-bof-dispatch)
                                                   ;; This catches anchored searches in scanning mode. There cannot be
                                                   ;; anything matched when not in BOF.
                                                   `((return (values))))
                                                  (t
                                                   `(
                                                     (when (= ,start ,end)
                                                       (return (values))) ;hmm
                                                     (setq ,p ,start)
                                                     (setq ,start (the (integer 0 ,array-total-size-limit) (+ ,start 1)))
                                                     ;; 3         ;???
                                                     ,@non-bof-dispatch
                                                     (go scan-loop)
                                                     )) ))))
                              ;;
                              ,@(loop for i from 3 below (length (dfa-states dfa)) append
                                      `(,(intern-Qn i)
                                         (progn ,@(translate-state dfa (dfa-state dfa i) out env))
                                         (go ,(intern-Qn 1))))
                  
                              ,(intern-Qn 1)
                              ,@(if *won-needed*
                                    (list `(if ,won (go ,(intern-Qn 2)))))
                              ,(if *scanning-mode-p*
                                   '(go scan-loop)
                                   '(return (values)))
                              ,(intern-Qn 2)
                              (return (values ,@ (let ((g (gensym)))
                                                   (labels ((foo (r)
                                                              (setq r (register-var r))
                                                              (if (member r regs :key #'cdr)
                                                                  `(let ((,g ,r)) (if (eql ,g -1) nil ,g))
                                                                  nil)))
                                                     (mapcar #'foo output-registers))))))))))))))

(defun simp-prog (prog-form)
  (destructuring-bind (bindings &body body) (cdr prog-form)
    (multiple-value-bind (decls body)
        (let ((p (member-if-not #'(lambda (x) (typep x '(or string (cons (member declare) t)))) body)))
          (values (ldiff body p) p))
      (labels ((simp-form (form)
                 (typecase form
                   ((cons (member PROGN) t)
                    (simp-progn (cdr form)))
                   ((cons symbol t)
                    `(,(car form) ,@(mapcar #'simp-form (cdr form))))
                   (t
                    form)))
               (simp-progn (forms)
                 (setq forms (mapcar #'simp-form forms))
                 (setq forms
                       (loop for q on forms
                             for x = (car q)
                             nconc (typecase x
                                     (atom (if (cdr q) nil (list x)))
                                   ((cons (member PROGN) t)
                                    (copy-list (cdr x)))
                                   (t (list x)))))
                 (case (length forms)
                   ((0) nil)
                   ((1) (car forms))
                   (t `(progn ,@forms)))))
        `(prog ,bindings
            ,@decls
            ,@(mapcan (lambda (form)
                        (cond ((atom form) (list form))
                              (t (let ((form (simp-form form)))
                                   (cond ((typep form '(cons (member PROGN) t))
                                          (remove-if #'atom (copy-list (cdr form))))
                                         ((atom form) nil)
                                         (t (list form)))))))
                      body))))))

(defun dfa-state-char-transitions (state)
  "All the transitions pruned to matching characters"
  (mapcan (lambda (tr)
            (and (consp (transition-sigma tr))
                 (let ((remain (isum-intersection (transition-sigma tr) +isum-any-character+)))
                   (and (not (isum-empty-p remain))
                        (let ((tr (copy-transition tr)))
                          (setf (transition-sigma tr) remain)
                          (list tr))))))
          (dfa-state-transitions state)))


;;;; -- Use as RE matcher API -----------------------------------------------------------------

;; We have SCAN, SCANNER, SCAN-TO-STRINGS, WITH-SCAN, and SCAN-CASE.
;; And the same saying 'MATCH' instead of 'SCAN'.

(defun scan (pattern string
             &key (start 0) (end nil)
                  (dot-includes-newline t)
                  (case-sensitive t)
                  (syntax :extended)
                  (anchored nil))
  (funcall (scanner pattern
                    :dot-includes-newline dot-includes-newline
                    :case-sensitive case-sensitive
                    :syntax syntax
                    :anchored anchored)
           string :start start :end end))

(defun scanner (pattern &rest args
                &key (dot-includes-newline t)
                     (case-sensitive t)
                     (syntax :extended)
                     (anchored nil))
  (declare (ignore dot-includes-newline case-sensitive syntax anchored))
  (compile nil (apply #'scanner-lambda pattern args)))

(defun scan-to-strings (pattern string &rest options &key &allow-other-keys)
  (values-list
   (loop for (s e) on (multiple-value-list (apply #'scan pattern string options)) by #'cddr
         collect (and s (subseq string s e)))))

(defmacro with-scan ((pattern input &key (start 0) (end nil)
                              (syntax :extended) (dot-includes-newline t)
                              (case-sensitive t) (anchored nil))
                     &body body)
  (multiple-value-bind (lambda-form varmap)
      (scanner-lambda pattern 
                      :syntax syntax
                      :dot-includes-newline dot-includes-newline
                      :case-sensitive case-sensitive
                      :anchored anchored)
    (let ((start-gensyms (mapcar (lambda (x) (declare (ignore x)) (gensym "S.")) varmap))
          (end-gensyms (mapcar (lambda (x) (declare (ignore x)) (gensym "E.")) varmap))
          (g.string (gensym "STRING.")))
      `(let ((,g.string ,input))
         (multiple-value-bind ,(mapcan #'list start-gensyms end-gensyms)
             (,lambda-form ,g.string :start ,start :end ,end)
           (declare (ignorable ,@(mapcan #'list start-gensyms end-gensyms)))
           (when ,(car start-gensyms)
             (with-submatch-macros-2
                 (,g.string)
                 ,(loop for var in varmap
                        for s in start-gensyms
                        for e in end-gensyms
                        for test in (cons 't (cdr start-gensyms))
                        collect (list var test s e))
               ,@body)))))))

(defmacro scan-case (&whole w
                     (input &key (start 0) (end nil)
                            (syntax :extended) (dot-includes-newline t)
                            (case-sensitive t) (anchored nil))
                     &body clauses)
  (let ((binaryp nil))
    (let ((default-tail (member-if (lambda (c) (member (car c) '(t otherwise))) clauses)))
      (when (cdr default-tail)
        (error "~@<Default clause is not the last in ~S~@:>" w))
      (setq clauses (ldiff clauses default-tail))
      (multiple-value-bind (macros clauses)
          (labels ((f (x) (typep x '(cons (member =) t))))
            (values (remove-if-not #'f clauses)
                    (remove-if #'f clauses)))
        (multiple-value-bind (lambda-form varmap)
            (scanner-lambda (mapcar #'car clauses)
                            :syntax syntax
                            :dot-includes-newline dot-includes-newline
                            :case-sensitive case-sensitive
                            :anchored anchored
                            :%vector-p t
                            :macros (mapcar (lambda (m) (destructuring-bind (n e) (cdr m)
                                                          (cons n e)))
                                            macros))
          (let ((start-gensyms (mapcar (lambda (x) (declare (ignore x)) (gensym "S.")) varmap))
                (end-gensyms (mapcar (lambda (x) (declare (ignore x)) (gensym "E.")) varmap))
                (g.string (gensym "STRING.")))
            `(let ((,g.string ,input))
               (multiple-value-bind (cat ,@(mapcan #'list start-gensyms end-gensyms))
                   (,lambda-form ,g.string :start ,start :end ,end)
                 (declare (ignorable ,@(mapcan #'list start-gensyms end-gensyms)))
                 (labels ((subseq-string (v s e)
                            ,(if binaryp
                                 `(map 'string #'code-char (subseq v s e)) ;### slow
                                 `(subseq v s e))))
                   (declare (inline subseq-string))
                   (with-submatch-macros-2
                       (,g.string)
                       ,(loop for var in varmap
                              for s in start-gensyms
                              for e in end-gensyms
                              for test in (cons 't (cdr start-gensyms))
                              collect (list var test s e))
                     (case cat
                       ,@ (let ((k 0))
                            (mapcar (lambda (c)
                                      `((,(1- (incf k))) ,@(cdr c)))
                                    clauses))
                          ,@ default-tail)))))))))))

;;; "matching" is just an anchored scan.

(defun match (pattern string
              &rest args
              &key (start 0) (end nil)
                   (dot-includes-newline t)
                   (case-sensitive t)
                   (syntax :extended)
                   (anchored t))
  (declare (ignore start end dot-includes-newline case-sensitive syntax))
  (apply #'scan pattern string :anchored anchored args))

(defun matcher (pattern &rest args
                &key (dot-includes-newline t)
                     (case-sensitive t)
                     (syntax :extended))
  (declare (ignore dot-includes-newline case-sensitive syntax))
  (compile nil (apply #'scanner-lambda pattern :anchored t args)))

(defun match-to-strings (pattern string &rest options &key &allow-other-keys)
  (values-list
   (loop for (s e) on (multiple-value-list (apply #'scan pattern string :anchored t options)) by #'cddr
         collect (and s (subseq string s e)))))

(defmacro with-match ((pattern input &key (start 0) (end nil)
                               (syntax :extended) (dot-includes-newline t)
                               (case-sensitive t)
                               (anchored t))
                      &body body)
  `(with-scan (,pattern ,input :start ,start :end ,end
                        :syntax ,syntax
                        :dot-includes-newline ,dot-includes-newline
                        :case-sensitive ,case-sensitive
                        :anchored ,anchored)
     ,@body))

(defmacro match-case ((input &rest options) &rest clauses)
  `(scan-case (,input :anchored t ,@options)
              ,@clauses))


;;; compiler macros

(define-compiler-macro scan (&whole whole
                             pattern string
                             &key (start 0)
                                  (end nil)
                                  (dot-includes-newline t)
                                  (case-sensitive t)
                                  (syntax :extended)
                                  (anchored nil))
  (cond ((and (constantp pattern)
              (constantp dot-includes-newline)
              (constantp case-sensitive)
              (constantp syntax)
              (constantp anchored))
         `(,(scanner-lambda (eval pattern)
                            :dot-includes-newline (eval dot-includes-newline)
                            :case-sensitive (eval case-sensitive)
                            :syntax (eval syntax)
                            :anchored (eval anchored)
                            :%use-key-args nil)
            ,string ,start ,end))
        (t
         whole)))

(define-compiler-macro scanner (&whole whole
                                pattern
                                &key (dot-includes-newline t)
                                     (case-sensitive t)
                                     (syntax :extended)
                                     (anchored nil))
  (cond ((and (constantp pattern)
              (constantp dot-includes-newline)
              (constantp case-sensitive)
              (constantp syntax)
              (constantp anchored))
         `#',(scanner-lambda (eval pattern)
                             :dot-includes-newline (eval dot-includes-newline)
                             :case-sensitive (eval case-sensitive)
                             :syntax (eval syntax)
                             :anchored (eval anchored)))
        (t
         whole)))

(define-compiler-macro scan-to-strings (&whole whole
                                               pattern string
                                               &rest options
                                               &key (start '0) (end 'nil)
                                               &allow-other-keys)
  (setq options (copy-list options))
  (remf options :start)
  (remf options :end)
  (cond ((and (constantp pattern)
              (every #'constantp options))
         (multiple-value-bind (lambda-form varmap)
             (apply #'scanner-lambda
                    (eval pattern)
                    :%use-key-args nil
                    (mapcar #'eval options))
           (let ((gs (mapcan (lambda (x) (declare (ignore x)) (list (gensym) (gensym))) varmap))
                 (g.string (gensym "INPUT.")))
             `(LET ((,g.string ,string))
                (MULTIPLE-VALUE-BIND ,gs
                    (,lambda-form ,g.string ,start ,end)
                  (VALUES
                   ,@(loop for (s e) on gs by #'cddr
                           collect `(AND ,s (SUBSEQ ,g.string ,s ,e)))))))))
        (t
         whole)))

(define-compiler-macro match (pattern string
                              &rest args
                              &key (start 0)
                                   (end nil)
                                   (dot-includes-newline t)
                                   (case-sensitive t)
                                   (syntax :extended)
                                   (anchored 't))
  (declare (ignore start end dot-includes-newline case-sensitive syntax))
  `(scan ,pattern ,string :anchored ,anchored ,@args))

(define-compiler-macro matcher (pattern &rest args)
  `(scanner ,pattern :anchored t ,@args))

(define-compiler-macro match-to-strings (&whole whole
                                                pattern string
                                                &rest options
                                                &key (start '0) (end 'nil)
                                                &allow-other-keys)
  (setq options (copy-list options))
  (remf options :start)
  (remf options :end)
  (cond ((and (constantp pattern)
              (every #'constantp options))
         (multiple-value-bind (lambda-form varmap)
             (apply #'scanner-lambda
                    (eval pattern)
                    :%use-key-args nil
                    :anchored t
                    (mapcar #'eval options))
           (let ((gs (mapcan (lambda (x) (declare (ignore x)) (list (gensym) (gensym))) varmap))
                 (g.string (gensym "INPUT.")))
             `(LET ((,g.string ,string))
                (MULTIPLE-VALUE-BIND ,gs
                    (,lambda-form ,g.string ,start ,end)
                  (DECLARE (IGNORABLE ,@gs))
                  (IF ,(car gs)
                      (VALUES 
                       T
                       ,@(loop for (s e) on (cddr gs) by #'cddr
                               collect `(AND ,s (SUBSEQ ,g.string ,s ,e))))
                      (VALUES NIL)))))))
        (t
         whole)))



(defun scanner-lambda (pattern &key (dot-includes-newline t)
                                    (case-sensitive t)
                                    (syntax :extended)
                                    (anchored nil)
                                    (%vector-p nil)
                                    (macros nil)
                                    (%use-key-args t))
  "Returns a lambda expression for matching /pattern/ and a map of
  group capture names as a sequence."
  (multiple-value-bind (sre varmap)
      (process-sre (if %vector-p
                       `(vector ,@ (let ((k 0))
                                     (mapcar (lambda (p)
                                               `(and (setq (-1 nil '-1))
                                                     ,p
                                                     (setq (-1 nil ',(1- (incf k))))))
                                             pattern)))
                       pattern)
                   macros  
                   :case-sensitive case-sensitive
                   :syntax syntax
                   :dot-includes-newline dot-includes-newline)
    (when anchored
      (setq sre `(and ,(if *scanning-mode-p*
                           '(:pretext %bof)
                           '(:pretext ""))
                      ,sre
                      (:epitext %eof))))
    (setq sre `(and ,@(loop for i from 0 for nil in varmap
                            collect `(setq (,(tag-start-reg i) nil '-1))
                            collect `(setq (,(tag-end-reg i) nil '-1)))
                    ,sre))
    (let* ((output-registers
            (append (and %vector-p '(-1))
                    (loop for i from 0 for nil in varmap
                          collect (tag-start-reg i)
                          collect (tag-end-reg i))))
           (dfa (re-dfa sre
                        :output-registers output-registers
                        :has-condition-dispatch nil
                        :has-lookahead *scanning-mode-p*))
           (dfa (setq *dfa* (dfa-simp dfa))))
      (values (translate-dfa dfa output-registers :use-key-args %use-key-args)
              varmap))))

;;;; ------------------------------------------------------------------------------------------

(defun cg-cond (&rest clauses)
  (setq clauses (remove nil clauses :key #'car))
  (cond ((null clauses) 'nil)
        ((null (cdr clauses))
         (apply #'cg-when (car clauses)))
        (t
         `(cond ,@(mapcar (lambda (clause)
                            (cons (car clause)
                                  (or (cg-progn* (cdr clause)) '(nil))))
                          clauses)))))

(defun cg-when (test &rest body)
  (setq body (apply #'cg-progn body))
  
  (cond ((eq test 't) body)
        (t
         (setq body (if (typep body '(cons (eql progn) t))
                        (cdr body)
                        (list body)))
         `(when ,test ,@body))))

(defun cg-progn (&rest body)
  (cond ((null body) 'nil)
        ((null (cdr body)) (car body))
        (t `(progn ,@body))))

(defun cg-progn* (forms)
  (labels ((aux (form)
             (cond ((atom form) (list form))
                   ((eq (car form) 'progn)
                    (mapcan #'aux (cdr form)))
                   (t (list form)))))
    (mapcan #'aux forms)))

(defun dfa-lookbehind-dispatch-states (dfa)
  (and (dfa-has-lookahead dfa)
       (if (dfa-has-condition-dispatch dfa)
           (remove-if (lambda (i) (<= i 2))
                      (remove-duplicates
                       (mapcan (lambda (p) (copy-list (dfa-state-successors dfa p)))
                               (dfa-condition-dispatch-states dfa))))
           (remove-if (lambda (i) (<= i 2))
                      (dfa-state-successors dfa 0)))))

(defun dfa-condition-dispatch-states (dfa)
  (remove-if (lambda (i) (<= i 2))
             (and (dfa-has-condition-dispatch dfa)
                  (dfa-state-successors dfa 0))))

