;; -*- Mode: Lisp; -*-
;; ---------------------------------------------------------------------------
;;     Title: Lexical Analysis
;;   Created: 2022-12-22
;;    Author: Gilbert Baumann <gilbert@bauhh.de>
;;   License: MIT style (see below)
;; ---------------------------------------------------------------------------
;;;  (c) copyright 2022 by Gilbert Baumann

;;  Permission is hereby granted, free of charge, to any person obtaining
;;  a copy of this software and associated documentation files (the
;;  "Software"), to deal in the Software without restriction, including
;;  without limitation the rights to use, copy, modify, merge, publish,
;;  distribute, sublicense, and/or sell copies of the Software, and to
;;  permit persons to whom the Software is furnished to do so, subject to
;;  the following conditions:
;; 
;;  The above copyright notice and this permission notice shall be
;;  included in all copies or substantial portions of the Software.
;; 
;;  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. 
;;  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;  CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;  TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;  SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(in-package :noffi)

(defvar *c-package*
  :noffi-c
  "Package to use to intern C identifiers.")

(defvar *ht* nil
  "This is a STRING-TABLE hash table serving as a cache for CINTERN in front
   of *C-PACKAGE*, and used by the scanner.

   Keywords (reserved words) are entered here as well.")

(eval-when (:compile-toplevel)
  (declaim (optimize (safety 1) (speed 3))))


;;;; -- Reserved Words ------------------------------------------------------------------------

;; To the parser storgae classes, type qualifiers, type specifiers, function
;; specifiers as just categories, it doesn't see the actual keyword. This
;; makes it easy to add implementation-specific keywords here. OTOH this has
;; flaws because at certain places only specific keyword may appear. e.g. "int
;; a[static]" is fine while "int a[extern]" is not. A later semeantic analysis
;; is supposed to catch that.

;; Also, some ABI might define more basic types like e.g. __int128 or _Float16.

(eval-when (:compile-toplevel :load-toplevel :execute) ;Huh? why?

  ;; Storage Classes
  (defparameter *storage-class-lexemes*
    '((:auto                    "auto")
      (:register                "register")
      (:static                  "static")
      (:extern                  "extern")
      (:typedef                 "typedef")))

  ;; Type Qualifiers
  ;; This should be part of the ABI proper
  (defparameter *type-qualifier-lexemes*
    '((:const                   "const" "__const" "__const__")
      (:volatile                "volatile" "__volatile" "__volatile__")
      (:restrict                "restrict" "__restrict" "__restrict__")

      ;; Apple
      (:_nullable               "_Nullable")
      (:nonnull                 "_Nonnull")
      (:null-unspecified        "_Null_unspecified")
      (:covariant               "__covariant")
      (:contravariant           "__contravariant")

      ;; Objective-C
      ;;
      ;; These cause problems. We need to figure out some way to cope with that.
      ;;
      (:nullable                "nullable")
      (:nonnull                 "nonnull")
      (:oneway                  "oneway")
      ;; (:out                     "out")
      ;; (:in                      "in")
      (:inout                   "inout")
      (:__kindof                "__kindof")
      (:null_unspecified        "null_unspecified") ))

  ;; Function Specifiers
  (defparameter *function-specifier-lexemes*
    '((:inline                  "inline" "__inline" "__inline__")
      (:noreturn                "_Noreturn")                            ;ISO C
      ;; Microsoft:
      (:fastcall                "_fastcall" "__fastcall")
      (:inline                  "_forceinline" "__forceinline")
      (:cdecl                   "_cdecl" "__cdecl")
      (:stdcall                 "_stdcall" "__stdcall")))

  ;; "Basic" Type Specifiers

  ;; We call 'char', 'short', etc "basic type specifiers". Put aside here too to
  ;; make it easy to update things when new or non-standard types pop up. These
  ;; are different from typedefs in that more than one could appear, like "long
  ;; int".

  (defparameter *c-keywords*
    '((:break                   "break")
      (:case                    "case")
      (:continue                "continue")
      (:default                 "default")
      (:do                      "do")
      (:else                    "else")
      (:enum                    "enum")
      (:for                     "for")
      (:goto                    "goto")
      (:if                      "if")
      (:return                  "return")
      (:sizeof                  "sizeof")
      (:struct                  "struct")
      (:switch                  "switch")
      (:union                   "union")
      (:while                   "while")
      (:_alignas                "_Alignas")
      (:_alignof                "_Alignof" "__alignof__" "__alignof")
      (:_pragma                 "_Pragma")

      ;;
      (:_atomic                 "_Atomic")

      ;; This one is funny
      (:__offsetof__            "__offsetof__" "__offsetof"
                                "__builtin_offsetof" "__builtin_offsetof__")
      ;; All of them
      (:asm                     "asm" "__asm" "__asm__")
      (:__pragma                "__pragma")

      ;; GCC or clang
      (:typeof                  "__typeof" "__typeof__")
      (:__attribute__           "__attribute" "__attribute__")
      (:__extension__           "__extension" "__extension__")

      ;; Microsoft
      (:__declspec              "__declspec" "_declspec")

      ;; We
      (:__noffi_type            "__noffi_type") ))
      
  )


;;;;

(declaim (inline cintern-2))
(defun cintern-2 (string start end)
  (let ((q (string-table-get *ht* string start end)))
    (unless q
      (setq q (list :identifier (intern (subseq string start end) *c-package*)))
      (setf (string-table-get *ht* string start end) q))
    (values (car q) (cadr q))))

(defun cintern (string)
  (if (symbolp string)
      string
      (intern string *c-package*)))


;;;; Scanner

(defun c-lexer (input)
  (declare (optimize (speed 3) (safety 0)))
  (clex2:lexer
   (input)
   (:count-lines-p t)
   (:count-columns-p t)
   ;;
   (= float-suffix "[fFlL]")
   (= exp "[eE][-+]?[0-9]+")
   (= hex-exp "[pP][-+]?[0-9]+")
   (= lwsp (or #\space #\tab))
   (= int-suffix "[uUlL]*")
   (= string-prefix (or "[lL]"))
   (= pp-number
      (and "[.]?[0-9]" (* (or "[.$_0-9a-zA-Z]"
                           universal-character-name
                           "[-eEpP+]"))))
   (= universal-character-name
      (or (and #\\ #\u hex-quad)
          (and #\\ #\U hex-quad hex-quad)))
   (= hex-quad "[0-9A-Fa-f]{4}")
   ;;
   (:simple-tokens
    ">>=" "<<=" "+=" "-=" "*=" "/=" "%=" "&=" "^=" "|=" ">>" "<<" "++" "--" "->"
    "&&" "||" "<=" ">=" "==" "!=" ";" "{" "}" "," ":" "=" "(" ")" "[" "]" "." "&"
    "!" "~" "-" "+" "*" "/" "%" "<" ">" "^" "|" "?" "...")

   ;; Identifiers or keyword
   ("[@_$a-zA-Z][_$a-zA-Z0-9]*"
    (return (cintern-2 (:buffer $$) (:start $$) (:end $$))))

   ;; Integers Constants
   (:integer-constant -> (and (or (= dec "[1-9][0-9]*")
                                  (= oct "0[0-7]*")
                                  (and "0[xX]" (= hex "[0-9a-fA-F]+")))
                              (or (= none "")
                                  (= l (or "l" "L"))
                                  (= ll (or "ll" "LL"))
                                  (= u (or "u" "U"))
                                  (= ul (or "u" "U") (or "l" "L"))
                                  (= ull (or "u" "U") (or "ll" "LL"))
                                  (= ui8 (? (or "u" "U")) (or "i" "I") "[1-9][0-9]*")))
                      => `(:integer-constant
                           ,(cond (dec :decimal)
                                  (oct :octal)
                                  (hex :hex))
                           ,(cond (dec (parse-integer dec))
                                  (oct (parse-integer oct :radix 8))
                                  (hex (parse-integer hex :radix 16)))
                           ,(cond (none nil) (l :L) (ll :LL) (u :u) (ul :ul) (ull :ull)
                                  (ui8
                                   ;; ###
                                   nil))))
   ;; Floats
   (:floating-constant -> (and (= it (or (and "[0-9]+" exp)
                                         (and "[0-9]+[.][0-9]*" (? exp))
                                         (and "[0-9]*[.][0-9]+" (? exp))))
                               "[fF]")
                       => `(:floating-constant
                            ,(with-standard-io-syntax
                              (let ((*read-default-float-format* 'single-float))
                                (read-from-string it)))
                            :float))
   ;; ### Same here, not all Lisps have long floats proper.
   (:floating-constant -> (and (= it (or (and "[0-9]+" exp)
                                         (and "[0-9]+[.][0-9]*" (? exp))
                                         (and "[0-9]*[.][0-9]+" (? exp))))
                               (? (= long "[lL]"))) ;???
                       => `(:floating-constant
                            ,(with-standard-io-syntax
                              (let ((*read-default-float-format* 'double-float))
                                (read-from-string it)))
                            ,(if long :long-double :double)))
   ;;
   (:floating-constant -> (and (or "0x" "0X")
                               (or (and (= int "[0-9a-fA-F]+"))
                                   (and (= int "[0-9a-fA-F]+") "[.]" (= frac "[0-9a-fA-F]*"))
                                   (and (= int "[0-9a-fA-F]*") "[.]" (= frac "[0-9a-fA-F]+")))
                               (? "[pP]" (= exp "[-+]?[0-9]+"))
                               (= suffix (* "[fFlL]"))) ;Hmm
                       => `(:floating-constant
                            ,(let* ((n (length frac))
                                    (m (+ (* (expt 16 n) (if int (parse-integer int :radix 16) 0))
                                          (parse-integer frac :radix 16)))
                                    (e (if exp (parse-integer exp :radix 16) 0)))
                              (* (expt 2d0 (- e (* 4 n))) m))
                            :float))    ;###

   ;; We accept anything matching a "preprocessor number" (pp-number) as a
   ;; literal. Also in the parser. It appears that new creative syntax for
   ;; number literals pop up each day. We report the actual text and let
   ;; the compiler deal with that.
   (:pp-number -> pp-number => $$)

   ;; Catch all
   ("[_$a-zA-Z0-9][_$a-zA-Z0-9]*"
    (return (values :bad nil)))

   ;; String literals
   #+(or)
   (:string-literal -> (and (? "L") #\" (= it (* (or (- t (or #\\ #\")) (and #\\ t)))) #\")
                    => (de-escaped-c-string it))
   (:string-literal -> (and (? (= prefix string-prefix))
                            #\" (= it (* (or (- t (or #\\ #\")) (and #\\ t)))) #\")
                    => `(:string-literal ,it ,prefix))
   ;; Char literals.
   ;;
   ;; We want to be as agnostic of the ABI in our parser as possible, so we
   ;; return character literals as strings, let the compiler handle
   ;; multi-letter character constants which may differ from platform to
   ;; platform.
   ;;
   ;; TODO: prefixes.
   ;;
   (:character-constant -> (? (= prefix string-prefix))
                        (and #\' (= it (* (or (- t (or #\\ #\')) (and #\\ t)))) #\')
                        => `(:character-constant ,(de-escaped-c-string it) ,prefix))

   ;; #line directives
   (:line-directive
    -> :bol
    (* lwsp) "#"
    (? (* lwsp) "line")                 ;Somehow cpp does omit the "line"
         (+ lwsp)
         (= no "[0-9]+")
         (+ lwsp)
         #\" (= file (* (or (- t (or #\\ #\")) (and #\\ t)))) #\"
         (* lwsp)
         #\newline
    => (list (parse-integer no) (de-escaped-c-string file)))

   ;; Directives
   (:pragma -> "^[ \\t]*#[ \\t]*pragma[ \\t]+((?:pack).*)" => $1)
   ("^[ \\t]*#.*"
    #+NIL
    (note "Unknown directive: ~S" clex2::$$))

   ;; Comments
   ((and '"//" (* (- t #\newline))))
   ('"/*" (clex2:begin :comment))
   (:in :comment
        ('"*/" (clex2:begin))
        (t))
   ;;
   (-> (+ (or #\space #\return #\tab #\page)))
   (-> #\newline)
   ;; Hmm
   (-> (<= #x80 #xFF))
   #-EXCL
   (-> #\U+FEFF) ))

(defun de-escaped-c-string (string &key (start 0) end)
  (setq end (or end (length string)))
  (let ((p start))
    (with-output-to-string (bag)
      (labels ((getc ()
                 (and (< p end) (char string (1- (incf p)))))
               (ascii-digit-char-p (c &optional (radix 10))
                 (and (<= (char-code c) 127)
                      (digit-char-p c radix)))
               (exactly-n (escape n radix)
                 (unless (and (<= (+ p n) end)
                              (not (position-if-not (lambda (c) (ascii-digit-char-p c radix)) string
                                                    :start p :end (+ p n))))
                   (error "Exactly ~R hex-digits expected ~
                                  after \\~A in C string literal ~S."
                          n (verbatim escape) (subseq string start end)))
                 (write-code (parse-integer string :start p :end (+ p n) :radix radix))
                 (incf p n))
               (write-code (code)
                 ;; ### cater for UTF-16 and UCS-2 Lisps.
                 (write-char (code-char code) bag)))
        (declare (inline getc ascii-digit-char-p exactly-n write-char))
        (loop for c = (getc)
              while c do
              (cond ((char= #\Newline c)
                     (error "~S seen in C string liternal." c))
                    ((char= c #\\)
                     (ecase (setq c (getc))
                       (#\' (write-char c bag))
                       (#\" (write-char c bag))
                       (#\? (write-char c bag))
                       (#\\ (write-char c bag))
                       (#\a (write-char (code-char #x07) bag))
                       (#\b (write-char (code-char #x08) bag))
                       (#\f (write-char (code-char #x0C) bag))
                       (#\n (write-char (code-char #x0A) bag))
                       (#\r (write-char (code-char #x0D) bag))
                       (#\t (write-char (code-char #x09) bag))
                       (#\v (write-char (code-char #x0B) bag))
                       ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7)
                        (let ((p2 (or (position-if-not (lambda (c) (ascii-digit-char-p c 8)) string
                                                       :start p :end (min (+ p 2) end))
                                      (min (+ p 2) end))))
                          (write-code (parse-integer string :radix 8 :start (1- p) :end p2))
                          (setq p p2)))
                       (#\x
                        (let ((p2 (or (position-if-not (lambda (c) (ascii-digit-char-p c 16)) string
                                                       :start p :end end)
                                      end)))
                          (unless (> p2 p)
                            (error "At least one hexdigit expected after \\x in C string literal ~S"
                                   (subseq string start end)))
                          (write-code (parse-integer string :radix 16 :start p :end p2))
                          (setq p p2)))
                       (#\u
                        (exactly-n #\u 4 16))
                       (#\U
                        (exactly-n #\U 8 16))))
                    (t
                     (write-char c bag))))))))

(defun escape-c-string (string)
  (with-output-to-string (bag)
    (clex2:lexing (string)
      ((or #\\ #\? #\") (write-string "\\" bag) (write-string $$ bag))
      (#\newline (write-string "\\n" bag))
      (#\tab (write-string "\\t" bag))
      ((* (- (<= 32 126) #\\ #\? #\")) (write-string $$ bag))
      ((<= 0 #xFFFF) (format bag "\\u~4,'0X" (char-code (char $$ 0))))
      (t (format bag "\\U~8,'0X" (char-code (char $$ 0)))))))


;;;; Transducer

(defun token-main-cat (token)
  (let ((cat (token-cat token)))
    (if (eq 'multi-token cat)
        (token-cat (car (token-val token)))
        cat)))

(defun token-cat-p (token cat)
  (or (eq (token-cat token) cat)
      (and (eq 'multi-token (token-cat token))
           (some (lambda (x) (token-cat-p x cat)) (token-val token)))))

(defun make-c-lexer (input)
  (let ((lexer (c-lexer input)))
    (lambda ()
      (block lexing
        (loop
          (multiple-value-bind (cat val line col end-line end-col)
              (funcall lexer)
            (macrolet ((get-token ()
                         '(multiple-value-bind (cat val line col end-line end-col) (funcall lexer)
                           (make-token-2 cat val
                            (make-line-col-sloc
                             :start-line line :start-col col
                             :end-line end-line :end-col end-col
                             :source input))))
                       (yield-pragma (pragma)
                         `((lambda (pragma)
                             (and pragma
                                  (return (values :pragma pragma
                                                  (make-line-col-sloc
                                                   :start-line line :start-col col
                                                   :end-line end-line :end-col end-col
                                                   :source input)))))
                           ,pragma)))
              (labels ((fetch-parenthesized-tokens ()
                         (let ((toks nil))
                           (push (get-token) toks)
                           (assert (eq :\( (token-cat (car toks))))
                           (let ((nest 1))
                             (do () ((= nest 0))
                               (push (get-token) toks)
                               (case (token-cat (car toks))
                                 (:\( (incf nest))
                                 (:\) (decf nest))
                                 (:eof
                                  ;; ###
                                  (return-from lexing (values :eof))))))
                           (reverse toks))))
                (multiple-value-bind (cat* val*)
                    (if (eq cat 'multi-token)
                        (values (caar val) (cadar val))
                        (values cat val))
                  (cond
                    ;; #pragma
                    ((eq :pragma cat*)
                     (yield-pragma (parse-pragma val*)))
                    ;; __pragma( <token-list> )
                    ((eq :__pragma cat*)
                     (let ((tokens (cdr (butlast (fetch-parenthesized-tokens)))))
                       (yield-pragma (parse-pragma-from-tokens tokens))))
                    ;; _Pragma( <string-literanl> )
                    ((eq :_pragma cat*)
                     (let ((tokens (cdr (butlast (fetch-parenthesized-tokens)))))
                       (assert (and (= 1 (length tokens))
                                    (eq :string-literal (token-cat (car tokens)))))
                       (yield-pragma (parse-pragma (string-literal-decoded-text (token-val (car tokens)))))))
                    ;;
                    ;; GCC
                    ;;
                    ((eq :__extension__ cat*))
                    ((eq :__attribute__ cat*)
                     (let ((toks nil))
                       (setq toks (fetch-parenthesized-tokens))
                       (return (values :__attribute__ toks
                                       (make-line-col-sloc
                                        :start-line line :start-col col
                                        :end-line end-line :end-col end-col
                                        :source input)))))
                    ((eq :asm cat*)
                     (let ((la (get-token)))
                       (when (token-cat-p la :type-qualifier) ;### "volatile"
                         (setq la (get-token)))
                       (assert (token-cat-p la :\() nil
                               "Saw ~S after __asm but expected '('" la)
                       (let ((nest 1))
                         (do () ((= nest 0))
                           (case (token-cat (get-token))
                             (:\( (incf nest))
                             (:\) (decf nest))
                             (:eof (return-from lexing (values :eof))))))))
                    ;;
                    (t
                     (return (values cat val
                                     (make-line-col-sloc
                                      :start-line line :start-col col
                                      :end-line end-line :end-col end-col
                                      :source input))))))))))))))

;;

(defun string-literal-decoded-text (string-literal-token)
  (ensure-type string-literal-token (cons (member :string-literal)))
  (de-escaped-c-string (cadr string-literal-token)))


;;;; ------------------------------------------------------------------------------------------

;; These are obsolete

(deftype storage-class ()
  '(satisfies storage-class-p))

(defun storage-class-p (symbol)
  (member symbol *storage-class-lexemes* :key #'car))

(deftype type-qualifier ()
  '(satisfies type-qualifier-p))

(defun type-qualifier-p (symbol)
  (member symbol *type-qualifier-lexemes* :key #'car))

(deftype function-specifier ()
  '(satisfies function-specifier-p))

(defun function-specifier-p (symbol)
  (member symbol *function-specifier-lexemes* :key #'car))

;; _Atomic           
;; _Complex          
;; _Generic          
;; _Imaginary        
;; _Noreturn         
;; _Static_assert    
;; _Thread_local

;; We then craft (:type-qualifier (:pack n) ...) for _member_ types. This
;; overrides the alignment of some type. We do this with members to make this
;; more robust.

;; The #pragma's travel as tokens all the way to the parser where they drive a
;; second state machine maintaing a '#pragma pack' stack.


;;;;

(defun boot-ht (&key (grammar 'c99-parser) (abi *abi*))
  (let ((ht (make-string-table :size 109)))
    (labels ((enter (k v &optional (fallback #+NIL(list :any-identifier (intern k :noffi-c))))
               (if fallback
                   (setf (string-table-get ht k 0 (length k))
                         (list 'multi-token (list v fallback)))
                   (setf (string-table-get ht k 0 (length k))
                         v))))
      (loop for (k . strings) in *c-keywords*
            do (loop for s in strings
                     do (enter s (list k k))))
      (loop for (k . strings) in (abi-basic-type-specifier-lexemes abi)
            ;; Some compilers have the urge to call them __foo or __foo__ as well,
            ;; register them all.
            do (loop for s in strings
                     do (enter s (list :basic-type-specifier k))
                        (enter (concatenate 'string "__" s) (list :basic-type-specifier k))
                        (enter (concatenate 'string "__" s "__") (list :basic-type-specifier k))))
      (loop for (k . strings) in *type-qualifier-lexemes*
            do (loop for s in strings do (enter s (list :type-qualifier k))))
      (loop for (k . strings) in (abi-type-qualifier-lexemes abi)
            do (loop for s in strings do (enter s (list :type-qualifier k))))
      (loop for (k . strings) in *function-specifier-lexemes*
            do (loop for s in strings
                     do (enter s  (list :function-specifier k))))
      (loop for (k . strings) in *storage-class-lexemes*
            do (loop for s in strings
                     do (enter s (list :storage-class k))))
      ;; objc
      #-(or)
      (loop for s in '("@implementation"
                       "@interface"
                       "@protocol"
                       "@encode"
                       "@synchronized"
                       "@selector"
                       "@end"
                       "@defs"
                       "@class"
                       "@try"
                       "@throw"
                       "@catch"
                       "@finally"
                       "@private"
                       "@package"
                       "@public"
                       "@protected"
                       "@property"
                       "@synthesize"
                       "@dynamic"
                       "@optional"
                       "@required") do
            (enter s (list (intern (string-upcase s) :keyword) (intern (string-upcase s) :keyword))))
      ;;
      ;; This is for the sub-syntax that is there with pragmas, __declspec,
      ;; __attribute__. The grammar has an alist of keywords mentioned in the
      ;; grammar. Register those as multi-tokens being either the keyword or a
      ;; vanilla identifier. We rely on backtracking to sort this out for us.
      ;;
      (loop for (nil . spelling) in (fancy-lalr-table-terminals (get grammar 'lalr-table)) do
            (when (and spelling
                       (or (char= (char spelling 0) #\@)
                           (char= (char spelling 0) #\_)
                           (alphanumericp (char spelling 0)))
                       (not (string-table-get ht spelling 0 (length spelling))))
              (enter spelling 
                     (list (intern (string-upcase spelling) :keyword)
                           (intern (string-upcase spelling) :keyword))
                     (list :identifier
                           (intern spelling :noffi-c))
                     )))
      )
    (setq *ht* ht)))

;; (boot-ht)

