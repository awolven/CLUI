;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: noffi; -*-
;; ---------------------------------------------------------------------------
;;     Title: C parser
;;   Created: 2011-06-09
;;    Author: Gilbert Baumann <gilbert@bauhh.de>
;;   License: MIT style (see below)
;; ---------------------------------------------------------------------------
;;  (c) copyright 2011 by Gilbert Baumann

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

;; (declaim (optimize (debug 2) (speed 1)))


;;;; -- Overview ------------------------------------------------------------------------------

;; Input travels quite some way until it hits the actual parse. The
;; states are:

;; 1. Lexical analysis with the scanner defined by C-LEXER

;; 2. Transducer in MAKE-C-LEXER
;;
;;    - __extension and __asm are just ignored
;;
;;    - __attribute__(..) captures the tokens inside the brackets for
;;      later processing

;; 3. TOK-CANDIDATES
;;
;;    - Identifiers turn to a "multi" token being a typedef-name or a
;;      regular identifier. The order depends on whether the identifier
;;      in question is defined as a type.

;; 4. LAST-MINUTE-TRANSDUCER
;;
;;    - #pragma pack --- A current value for packing and the pack stack are
;;      maintained at the state machine; isn't passed along otherwise.
;;
;;    - __attribute__(...) is parsed by spawning another parser.
;;
;;      Then for e.g. aligned(n) a multi-token being a
;;      declaration-specifier and a type-qualifier is yielded.

;; 5. Backtracking LALR(1) driver in BT-PARSE


;;;; -- Notes ---------------------------------------------------------------------------------

;; what about - (void)addEventListener:(NSString *)type :(id <DOMEventListener>)listener :(BOOL)useCapture __attribute__((availability(macosx,introduced=10_4,deprecated=10_5,message="" ))); ?
;; Is ":(id <DOMEventListener>)listener" an anonymous parameter?

;;;; ------------------------------------------------------------------------------------------

(defvar *parse-env* nil)

(defun left-assoc (op x y) (list op x y))
(defun binop (op x y) (list op x y))
(defun unop (op x) (list op x))
(defun setopf (op x y) (list 'setopf op x y))

(defun implicit-block (x)
  (cond ((and (consp x) (eq (car x) 'begin))
         (cdr x))
        (t (list x))))

(defun wrap (wrappings it)
  (cond ((null wrappings) it)
        (t (list (car wrappings) (wrap (cdr wrappings) it)))))

(defparameter *auto-typedef-p*
  nil)

(defun note (fmt &rest args)
  (format t "~&~<;; ~@;~?~:>" (list fmt args)))

(defun current-packing ()
  (caar (machine-pack-stack *current-machine*)))

(defun parse-pragma (string)
  (parse-pragma-from-tokens (tokenize string)))

(defun parse-pragma-from-tokens (tokens)
  (and (token-cat-p (car tokens) :pack)
       (parse-token-list tokens :grammar 'c99-parser :inject '(/pragma))))

;;; Objective C

;; We are having trouble with "enum foo : bar;" It's ambigious when it appears
;; in a struct, like with this nonesense:

;;    enum foo : 10;

;; The parser cannot tell yet if '10' is a type or an expression.

;; 

(define-grammar c99-parser
    ;; These tokens are for entering some specific subparts of the parser.
    (:tokens @expression @statement @type /gcc-attribute @ms-decl-spec
             /pragma)

  (:precedence (:nonassoc :else) ;This silences the complain about that shift/reduce conflict with "else"
               (:nonassoc ":")
               )

  (start 
   -> (? translation-unit)              => (cons-progn $1)
   -> @expression expression            => $2
   -> @expression block-item-list       => `(begin ,@$2)
   ;; -> @expression type-name             => $2
   ;;
   -> @statement block-item-list        => (if (eql 1 (length $2)) (car $2) `(begin ,@$2))
   -> @type type-name                   => $2
   -> /gcc-attribute gcc-attribute      => $2 
   -> @ms-decl-spec ms-decl-spec        => $2
   -> /pragma pragma                    => $2
   -> /pragma :error                    => nil)

  ;; A.2.1 Expressions
  (primary-expression
   -> :bad                              ;xxx
   -> :identifier
   -> :integer-constant
   -> :character-constant
   -> :floating-constant
   -> string-literal-list
   -> "(" expression ")"                => $2
   -> "(" :error ")"                    => `(error)
   -> "(" "{" (? block-item-list) "}" ")" => `(c-block-expr ,@$3) )
  #+NOFFI-OBJC
  (primary-expression
   -> "[" expression :identifier objc-arguments "]"                => `(objc-invoke ,$2 ,$3 ,@$4))
  #+NOFFI-OBJC
  (objc-arguments
   ->                   => nil
   -> ":" expression    => (list $2)
   -> ":" expression objc-more-arguments => (cons $2 $3))
  #+NOFFI-OBJC
  (objc-more-arguments
   -> :identifier ":" expression (? objc-more-arguments) => (list* $1 $3 $4))

  (string-literal-list -> (+ :string-literal) => (concat-string-literals $1))
  
  (postfix-expression
   -> primary-expression
   -> postfix-expression "[" expression "]"                             => (binop 'aref $1 $3)
   -> postfix-expression "(" (? argument-expression-list) ")"           => `(funcall ,$1 ,@ (reverse $3))
   -> postfix-expression "(" :error ")"                                 => `(funcall ,$1 (error))
   -> "__offsetof__" "(" type-name "," expression ")"                   => `(offsetof ,$3 ,$5)
   -> "__offsetof__" "(" :error ")"                                     => `(error)
   -> postfix-expression "." :identifier                                => (binop '\. $1 $3)
   -> postfix-expression "->" :identifier                               => (binop '-> $1 $3)
   -> postfix-expression "++"                                           => (unop 'post-inc $1)
   -> postfix-expression "--"                                           => (unop 'post-dec $1)
   -> "(" type-name ")" "{" initializer-list "}"                        => `(:OBJECT-LITERAL ,$2 (:LIST ,@(reverse $5)))
   -> "(" type-name ")" "{" initializer-list "," "}"                    => `(:OBJECT-LITERAL ,$2 (:LIST ,@(reverse $5))))
  (argument-expression-list
   -> assignment-expression                                             => (list $1)
   -> argument-expression-list "," assignment-expression                => (cons $3 $1))
  (unary-expression
   -> postfix-expression
   -> "++" unary-expression             => (unop 'pre-inc $2)
   -> "--" unary-expression             => (unop 'pre-dec $2)
   -> "&" cast-expression               => (unop '& $2)
   -> "*" cast-expression               => (unop 'aref $2)
   -> "+" cast-expression               => (unop '+ $2)
   -> "-" cast-expression               => (unop '- $2)
   -> "~" cast-expression               => (unop 'lognot $2)
   -> "!" cast-expression               => (unop 'not $2)
   -> "_Alignof" unary-expression       => (unop 'alignof-expr $2)
   -> "_Alignof" "(" type-name ")"      => (unop 'alignof-type $2)
   -> "sizeof" unary-expression         => (unop 'sizeof-expr $2)
   -> "sizeof" "(" type-name ")"        => (unop 'sizeof-type $3)
   -> "__noffi_type" "(" type-name ")"  => (unop 'noffi-type $3))
  (cast-expression
   -> unary-expression
   -> "(" type-name ")" cast-expression                 => `(the ,$2 ,$4))
  (multiplicative-expression
   -> cast-expression
   -> multiplicative-expression "*" cast-expression     => (left-assoc '* $1 $3)
   -> multiplicative-expression "/" cast-expression     => (left-assoc '/ $1 $3)
   -> multiplicative-expression "%" cast-expression     => (left-assoc '% $1 $3))
  (additive-expression
   -> multiplicative-expression
   -> additive-expression "+" multiplicative-expression     => (left-assoc '+ $1 $3)
   -> additive-expression "-" multiplicative-expression     => (left-assoc '- $1 $3))
  (shift-expression
   -> additive-expression
   -> shift-expression "<<" additive-expression         => (left-assoc '<< $1 $3)
   -> shift-expression ">>" additive-expression         => (left-assoc '>> $1 $3))
  (relational-expression
   -> shift-expression
   -> relational-expression   "<"    shift-expression     => (left-assoc '< $1 $3)
   -> relational-expression   ">"    shift-expression     => (left-assoc '> $1 $3)
   -> relational-expression   "<="   shift-expression     => (left-assoc '<= $1 $3)
   -> relational-expression   ">="   shift-expression     => (left-assoc '>= $1 $3))
  (equality-expression
   -> relational-expression
   -> equality-expression "==" relational-expression      => (binop '= $1 $3)
   -> equality-expression "!=" relational-expression      => (binop '/= $1 $3))
  (AND-expression
   -> equality-expression
   -> AND-expression "&" equality-expression                    => (left-assoc 'logand $1 $3))
  (exclusive-OR-expression
   -> AND-expression
   -> exclusive-OR-expression "^" AND-expression                => (left-assoc 'logxor $1 $3))
  (inclusive-OR-expression
   -> exclusive-OR-expression
   -> inclusive-OR-expression "|" exclusive-OR-expression       => (left-assoc 'logior $1 $3))
  (logical-AND-expression
   -> inclusive-OR-expression
   -> logical-AND-expression "&&" inclusive-OR-expression       => (left-assoc 'and $1 $3))
  (logical-OR-expression
   -> logical-AND-expression
   -> logical-OR-expression "||" logical-AND-expression         => (left-assoc 'or $1 $3))
  (conditional-expression
   -> logical-OR-expression
   -> logical-OR-expression "?" expression ":" conditional-expression
   => `(if ,$1 ,$3 ,$5))
  (assignment-expression
   -> conditional-expression
   -> unary-expression "=" assignment-expression         => (binop 'setf $1 $3)
   -> unary-expression "*=" assignment-expression        => (binop 'mulf $1 $3)
   -> unary-expression "/=" assignment-expression        => (binop 'divf $1 $3)
   -> unary-expression "%=" assignment-expression        => (binop 'modf $1 $3)
   -> unary-expression "+=" assignment-expression        => (binop 'incf $1 $3)
   -> unary-expression "-=" assignment-expression        => (binop 'decf $1 $3)
   -> unary-expression "<<=" assignment-expression       => (binop 'shlf $1 $3)
   -> unary-expression ">>=" assignment-expression       => (binop 'shrf $1 $3)
   -> unary-expression "&=" assignment-expression        => (binop 'logandf $1 $3)
   -> unary-expression "^=" assignment-expression        => (binop 'logxorf $1 $3)
   -> unary-expression "|=" assignment-expression        => (binop 'logiorf $1 $3))
  (expression
   -> assignment-expression
   -> expression "," assignment-expression               => (left-assoc 'progn $1 $3))
  (constant-expression
   -> conditional-expression)
  ;; A.2.2 Declarations
  ;;
  ;; We do this this funny with <declaration-aux> to have NOTE-DECLARE been
  ;; called as soon as possible. Consider "typedef int a; a b;"
  ;;
  (declaration
   -> declaration-aux ";" => $1)
  (declaration-aux
   -> declaration-specifiers (? init-declarator-list)
   => (cons-and-note-declaration $1 $2) )

  ;;
  ;; The grammar as given allows for non-sense like "int struct foo", while we
  ;; really want to see at most one type-specifier. We might want to outrule the
  ;; obviously non-sense at parser for the benefit of less backtracking. gcc and
  ;; clang appears to be eager here and take any identifier after some actual
  ;; type-specifier as an identifier to define.
  ;;
  ;; However: This has down-sides: When the user types "int foo bar;" both gcc
  ;; and clang report a syntax error, while we could report a non-sense type
  ;; "int foo", when we would not prun at the grammar level.
  ;;

  #||
  (declaration-specifiers
  -> declaration-specifiers-2
  => $1)
  (declaration-specifiers-2
  -> one-declaration-specifier                                 => $1
  -> declaration-specifiers-2 one-declaration-specifier        => (append $2 $1))
  (one-declaration-specifier
  -> storage-class-specifier                   => (list $1)
  -> type-specifier                            => (list $1)
  -> type-qualifier                            => (list $1)
  -> function-specifier                        => (list $1)
  -> declaration-specifier                     => $1)
  ||#
  
  (declaration-specifiers
   -> storage-class-specifier (? declaration-specifiers)        => (cons $1 $2)
   -> alignment-specifier (? declaration-specifiers)            => (cons $1 $2)
   -> type-qualifier (? declaration-specifiers)                 => (cons $1 $2)
   -> function-specifier (? declaration-specifiers)             => (cons $1 $2)
   -> declaration-specifier (? declaration-specifiers)          => (append $1 $2)
   -> definitive-type-specifier (? declaration-specifiers-definitive-type)      => (append $1 $2)
   -> :basic-type-specifier (? declaration-specifiers-base-type)                => (cons `(:base-type ,$1) $2))

  (declaration-specifiers-definitive-type
   -> storage-class-specifier (? declaration-specifiers-definitive-type)        => (cons $1 $2)
   -> alignment-specifier (? declaration-specifiers)            => (cons $1 $2)
   -> type-qualifier (? declaration-specifiers-definitive-type)                 => (cons $1 $2)
   -> function-specifier (? declaration-specifiers-definitive-type)             => (cons $1 $2)
   -> declaration-specifier (? declaration-specifiers-definitive-type)          => (append $1 $2) )

  (declaration-specifiers-base-type
   -> storage-class-specifier (? declaration-specifiers-base-type)        => (cons $1 $2)
   -> alignment-specifier (? declaration-specifiers)                      => (cons $1 $2)
   -> type-qualifier (? declaration-specifiers-base-type)                 => (cons $1 $2)
   -> function-specifier (? declaration-specifiers-base-type)             => (cons $1 $2)
   -> declaration-specifier (? declaration-specifiers-base-type)          => (append $1 $2)
   -> :basic-type-specifier (? declaration-specifiers-base-type)          => (cons `(:base-type ,$1) $2) )

  (definitive-type-specifier
      -> struct-or-union-specifier                    => (list $1)
      -> enum-specifier                               => (list $1)
      -> :typedef-name                                => (list (list :type-name $1))
      -> :typedef-name ":" type-name :--tie--         => (list (list :type-name $1))
      -> :typeof "(" expression ")"                   => (list `(:typeof ,$3))
      -> :typeof "(" :error ")"                       => (list `(:typeof (error)))
      ;; ### That's not entirely correct:
      ;; |     If the _Atomic keyword is immediately followed by a left parenthesis, it is interpreted as a type
      ;; |     specifier (with a type name), not as a type qualifier.
      ;; Thus we need an extra token for that.
      ->  :_atomic :--tie3-- "(" type-name ")"        => (list `(:_atomic ,$4))
      )
  #+NOFFI-OBJC
  (definitive-type-specifier
      -> :typedef-name "<" (++ "," type-name) ">"
      => (list `(:objc-angled ,$1 ,$3)))

  (declaration-specifier -> :declaration-specifier      => (list `(:declaration-specifier ,$1))
                         -> "__declspec" ms-decl-spec   => $2)

  (init-declarator-list
   -> init-declarator                                   => (list $1)
   -> init-declarator-list "," init-declarator          => (cons $3 $1))
  (init-declarator
   -> declarator (? decl-spec-list)
   => (let ((x $1)) (dolist (d $2 x) (setq x `(,(car d) ,(cadr d) ,x))))
   -> declarator (? decl-spec-list) "=" initializer
   => `(= ,(let ((x $1)) (dolist (d $2 x) (setq x `(,(car d) ,(cadr d) ,x)))) ,$4))

  (decl-spec-list -> declaration-specifier
                  -> function-specifier => (list $1)
                  -> decl-spec-list declaration-specifier => (append $1 $2)
                  -> decl-spec-list function-specifier    => (append $1 (list $2))
                  )

  (alignment-specifier -> "_Alignas" "(" type-name ")" => `(:declaration-specifier (:alignas-type ,$3))
                       -> "_Alignas" "(" constant-expression ")" => `(:declaration-specifier (:alignas-expr ,$3)))

  (storage-class-specifier -> :storage-class            => `(:storage-class ,$1))
  (type-qualifier -> :type-qualifier                    => `(:type-qualifier ,$1))
  (type-qualifier -> :_atomic                           => `(:type-qualifier ,$1))
  (function-specifier -> :function-specifier            => `(:function-specifier ,$1))

  ;; We reformulated the 'pointer' things, so that we can directly wrap a
  ;; (:POINTER ..) around. Still the wrong order, but better than facing a list
  ;; of qualifiers with "*"s sprinkled. As a consequence `type-qualifier-list'
  ;; is gone.

  ;; Declarator
  (declarator
   -> direct-declarator
   -> "*" pointer-declarator                            => `(:pointer ,$2)
   -> "&" pointer-declarator                            => `(:amp-pointer ,$2)
   -> "^" pointer-declarator                            => `(:block-pointer ,$2))
  (pointer-declarator
   -> direct-declarator
   -> "*" pointer-declarator                            => `(:pointer ,$2)
   -> "&" pointer-declarator                            => `(:amp-pointer ,$2)
   -> "^" pointer-declarator                            => `(:block-pointer ,$2)
   -> :type-qualifier pointer-declarator                => `(:type-qualifier ,$1 ,$2) 
   -> :function-specifier pointer-declarator            => `(:function-specifier ,$1 ,$2)
   -> declaration-specifier pointer-declarator          => `(:declaration-specifier ,$1 ,$2) )
  (pointer-abstract-declarator
   ->
   -> direct-abstract-declarator
   -> "*" pointer-abstract-declarator                   => `(:pointer ,$2)
   -> "&" pointer-abstract-declarator                   => `(:amp-pointer ,$2)
   -> "^" pointer-abstract-declarator                   => `(:block-pointer ,$2)
   -> :type-qualifier pointer-abstract-declarator       => `(:type-qualifier ,$1 ,$2)
   -> :function-specifier pointer-abstract-declarator   => `(:function-specifier ,$1 ,$2) )

  (direct-declarator
   -> :identifier
   -> "(" nested-declarator ")"                         => $2
   -> "(" :error ")"                                    => `(error)
   -> direct-declarator "[" array-dimension "]"         => `(:array ,$1 ,$3)
   -> direct-declarator "(" function-parameters ")"     => `(:function ,$1 ,$3)
   -> direct-declarator "(" :error ")"                  => `(:function ,$1 (error)) )

  (nested-declarator
   -> :function-specifier nested-declarator             => `(:function-specifier ,$1 ,$2)
   -> declaration-specifier nested-declarator           => `(:declaration-specifier ,$1 ,$2)
   -> declarator                                        => $1)
                     
  (nested-abstract-declarator
   -> :function-specifier nested-abstract-declarator    => `(:function-specifier ,$1 ,$2)
   -> declaration-specifier nested-abstract-declarator  => `(:declaration-specifier ,$1 ,$2)
   -> abstract-declarator => $1)
  
  ;; Abstract Declarator
  (abstract-declarator
   -> direct-abstract-declarator
   -> "*" pointer-abstract-declarator                   => `(:pointer ,$2)
   -> "&" pointer-abstract-declarator                   => `(:amp-pointer ,$2)
   -> "^" pointer-abstract-declarator                   => `(:block-pointer ,$2))

  (direct-abstract-declarator
   -> "(" nested-abstract-declarator ")"                                => $2
   ;; -> "(" :error ")"                                                    => '(error)
   -> (? direct-abstract-declarator) "[" array-dimension "]"            => `(:array ,$1 ,$3)
   -> (? direct-abstract-declarator) "(" (? parameter-type-list) ")"    => `(:function ,$1 ,(reverse $3)) 
   ;; -> (? direct-abstract-declarator) "(" :error ")"    => `(:function ,$1 (error)) 
   )

  (parameter-declaration
   -> parameter-specifiers declarator                 => (cons-and-note-declaration $1 (and $2 (list $2)))
   -> parameter-specifiers (? abstract-declarator)    => (cons-and-note-declaration $1 (and $2 (list $2))))

  ;; Same dance again
  (parameter-specifiers
   -> storage-class-specifier (? parameter-specifiers)                  => (cons $1 $2)
   -> type-qualifier (? parameter-specifiers)                           => (cons $1 $2)
   -> definitive-type-specifier (? parameter-specifiers/type-seen)      => (append $1 $2)
   -> basic-type-specifier (? parameter-specifiers/basic-type-seen)     => (cons $1 $2))
  (parameter-specifiers/type-seen
   -> storage-class-specifier (? parameter-specifiers/type-seen)        => (cons $1 $2)
   -> type-qualifier (? parameter-specifiers/type-seen)                 => (cons $1 $2))
  (parameter-specifiers/basic-type-seen
   -> storage-class-specifier (? parameter-specifiers/basic-type-seen)  => (cons $1 $2)
   -> type-qualifier (? parameter-specifiers/basic-type-seen)           => (cons $1 $2)
   -> basic-type-specifier (? parameter-specifiers/basic-type-seen)     => (cons $1 $2))

  #|| 

  (parameter-specifiers
  -> one-parameter-specifier                           => (list $1)
  -> parameter-specifiers one-parameter-specifier      => (cons $2 $1))
  (one-parameter-specifier
  -> storage-class-specifier
   ;; yyy -> type-specifier             ;
  -> type-qualifier)
  ||#
  
  ;;
  (array-dimension
   -> :type-qualifier array-dimension           => `(:type-qualifier ,$1 ,$2)
   -> :storage-class  array-dimension           => `(:storage-class ,$1 ,$2) ;for "static"
   ->                                           => :unspecified
   -> assignment-expression                     => $1
   -> "*"                                       => '* )
  ;;
  (function-parameters
   ->
   -> parameter-type-list                       => (reverse $1)
   -> identifier-list )

  ;; Types

  (struct-or-union-specifier
   ;; ### Accepting empty lists is an extension
   -> struct-or-union struct-decl-specs (? :identifier) "{" (? struct-declaration-list) "}"
   => `(,$1 ,$3 ,$5 ,(mapcar (lambda (x)
                               ;; Hmm
                               (if (and (consp x) (member (car x) '(:declaration-specifier)))
                                   `(:type-qualifier ,(cadr x))
                                   x))
                             $2))
   -> struct-or-union struct-decl-specs :identifier
   =>
   ;; First, why cannot I name the source location. Second: Where to put
   ;; these delcaration?
   (when $2 (warn "Attribute pile ~S ignored" $2))
   ;; (when $2 (blame-for-token *last-good-token* "Pile ignored: ~S" $2))
   `(,$1 ,$3))

  (struct-decl-specs -> (? struct-decl-specs-1) => $1)
  (struct-decl-specs-1 -> declaration-specifier                         => $1
                       -> struct-decl-specs-1 declaration-specifier     => (append $1 $1))

  (struct-or-union
   -> "struct"          => :struct
   -> "union"           => :union)
  (struct-declaration-list
   -> struct-declaration                                => $1
   -> struct-declaration-list struct-declaration        => (append $1 $2))
  (struct-declaration
   -> (? specifier-qualifier-list) struct-declarator-list ";"
   => (cons-member-declaration $1 $2 (current-packing))
   -> (? specifier-qualifier-list) ";"
   => (cons-member-declaration $1 nil (current-packing)))
  #+NOFFI-OBJC
  (struct-declaration
   -> "@private" => nil
   -> "@protected" => nil
   -> "@public"  => nil
   -> "@required" => nil
   -> "@optional" => nil
   -> "@package" => nil)

  ;; Same dance, make sure that there is only at most one definitive type (some
  ;; typedef-name, some enum, struct or union, or a sequence of basic types) in
  ;; this list. This is need to disambiguate beautiful things like "typedef enum
  ;; foo : bar baz;".

  (specifier-qualifier-list
   -> type-qualifier (? specifier-qualifier-list)                           => (cons $1 $2)
   -> alignment-specifier (? declaration-specifiers)                        => (cons $1 $2)   
   -> definitive-type-specifier (? specifier-qualifier-list/type-seen)      => (append $1 $2)
   -> basic-type-specifier (? specifier-qualifier-list/basic-type-seen)     => (cons $1 $2))
  (specifier-qualifier-list/type-seen
   -> alignment-specifier (? declaration-specifiers)                        => (cons $1 $2)   
   -> type-qualifier (? specifier-qualifier-list/type-seen)                 => (cons $1 $2))
  (specifier-qualifier-list/basic-type-seen
   -> alignment-specifier (? declaration-specifiers)                        => (cons $1 $2)   
   -> type-qualifier (? specifier-qualifier-list/basic-type-seen)           => (cons $1 $2)
   -> basic-type-specifier (? specifier-qualifier-list/basic-type-seen)     => (cons $1 $2))

  (basic-type-specifier -> :basic-type-specifier => `(:base-type ,$1))
  

  (struct-declarator-list
   -> struct-declarator                                         => (list $1)
   -> struct-declarator-list "," struct-declarator              => (cons $3 $1) )
  (struct-declarator
   -> declarator (? decl-spec-list)
   => (let ((x $1)) (dolist (d $2 x) (setq x `(,(car d) ,(cadr d) ,x))))
   -> (? declarator) ":" constant-expression                    => `(:bit-field ,$1 ,$3)
   )

  #-NOFFI-OBJC
  (enum-specifier
   -> "enum" (? :identifier) "{" enumerator-list "}"            => `(:enum ,$2 ,(reverse $4))
   -> "enum" (? :identifier) "{" enumerator-list "," "}"        => `(:enum ,$2 ,(reverse $4))
   -> "enum" :identifier                                        => `(:enum ,$2))

  (enumerator-list
   -> enumerator                                                => (list $1)
   -> enumerator-list "," enumerator                            => (cons $3 $1))
  (enumerator
   -> enumeration-constant                                      => $1
   -> enumeration-constant "=" constant-expression              => `(,$1 ,$3) )

  ;;

  (parameter-type-list
   -> parameter-list                            => $1
   -> parameter-list "," "..."                  => (cons '&rest $1)
   -> "..."                                     => (list '&rest) ;### for clang's tg_promote
   )
  (parameter-list
   -> parameter-declaration                     => (list $1)
   -> parameter-list "," parameter-declaration  => (cons $3 $1))

  (identifier-list
   -> :identifier                               => (list $1)
   -> identifier-list "," :identifier           => (cons $3 $1))

  (type-name
   -> specifier-qualifier-list (? abstract-declarator)
   => (fold-type-name $1 $2))

  ;; (typedef-name -> :identifier)
  (initializer
   -> assignment-expression             => $1
   -> "{" initializer-list "}"          => `(:list ,@(reverse $2))
   -> "{" initializer-list "," "}"      => `(:list ,@(reverse $2)))
  (initializer-list
   -> (? designation) initializer                       => (list (if $1 `(:at ,$1 ,$2) $2))
   -> initializer-list "," (? designation) initializer  => (cons (if $3 `(:at ,$3 ,$4) $4) $1))
  (designation
   -> designator-list "="               => (reverse $1))
  (designator-list
   -> designator                        => (list $1)
   -> designator-list designator        => (cons $2 $1))
  (designator
   -> "[" constant-expression "]"       => `(aref ,$2)
   -> "." :identifier                   => `(\. ,$2))
  ;; A.2.3 Statements
  (statement
   -> :identifier ":" statement                                         => `(label ,$1 ,$3)
   -> "case" constant-expression ":" statement                          => `(case ,$2 ,$4)
   -> "default" ":" statement                                           => `(default ,$3)
   -> ";"                                                               => `(nop)
   -> expression ";"                                                    => `(expr ,$1)
   -> "if" "(" expression ")" statement                                 => `(if ,$3 ,$5)
   -> "if" "(" expression ")" statement "else" statement                => `(if ,$3 ,$5 ,$7)
   -> "switch" "(" expression ")" statement                             => `(switch ,$3 ,$5)
   -> "while" "(" expression ")" statement                              => `(while ,$3 ,$5)
   -> "do" statement "while" "(" expression ")" ";"                     => `(do-while ,$2 ,$5)
   -> "for" "(" (? expression) ";" (? expression) ";" (? expression) ")" statement
   => `(for (,$3 ,$5 ,$7) ,$9)
   -> "for" "(" declaration (? expression) ";" (? expression) ")" statement
   => `(for-decl (,$3 ,$4 ,$6) ,$8)
   -> "goto" :identifier ";"                                            => `(goto ,$2)
   -> "continue" ";"                                                    => `(continue)
   -> "break" ";"                                                       => `(break)
   -> "return" (? expression) ";"                                       => `(return ,@(and $2 (list $2)))
   -> :error ";"                                                        => `(error)
   -> compound-statement)
  (compound-statement
   -> "{" tie-in/open-block (? block-item-list) tie-in/close-block "}"
   => `(begin ,@$3)
   -> "{" tie-in/open-block :error tie-in/close-block "}"
   => '(error))
  (tie-in/open-block -> => (start-block))
  (tie-in/close-block -> => (end-block))
  (block-item-list
   -> block-item                        => (list $1)
   -> block-item block-item-list        => (cons $1 $2))
  (block-item
   -> declaration                       => $1
   -> statement                         => $1)

  ;; A.2.4 External definitions
  (translation-unit
   -> external-declaration                      => $1
   -> external-declaration translation-unit     => (append $1 $2))
  (external-declaration
   -> function-definition                       => (list $1)
   -> declaration                               => (list $1)
   -> ";"                                       => nil
   -> :storage-class :string-literal "{" translation-unit "}"        => $4
   -> :storage-class :string-literal external-declaration            => $3
   )

  (function-definition
   -> (? declaration-specifiers) declarator tie-in/open-block (* kr-parameter-declaration) compound-statement tie-in/close-block
   => (cons-defun $1 $2 $4 $5) )

  (kr-parameter-declaration
   -> parameter-specifiers (** "," declarator) ";"
   => (cons-and-note-declaration $1 $2))

  
  ;;
  (enumeration-constant -> :identifier)

  ;;
  (gcc-attribute       -> (or "aligned" "__aligned" "__aligned__")
                       => `(multi-token (:declaration-specifier :align) (:type-specifier :align))
                       -> (or "aligned" "__aligned" "__aligned__") "(" expression ")"
                       => `(multi-token (:declaration-specifier (:align ,$3)) (:type-specifier (:align ,$3)))
                       -> (or "cdecl" "__cdecl" "__cdecl__")
                       => `(:function-specifier :cdecl)
                       -> (or "stdcall" "__stdcall" "__stdcall__")
                       => `(:function-specifier :stdcall)
                       -> (or "noreturn" "__noreturn" "__noreturn__")
                       => `(:function-specifier :noreturn)
                       -> (or "inline" "__inline" "__inline__"
                              "gnu_inline" "__gnu_inline" "__gnu_inline__"
                              "always_inline" "__always_inline" "__always_inline__")
                       => `(:function-specifier :inline)
                       -> (or "sentinel" "__sentinel" "__sentinel__") "(" expression ")"
                       => `(:function-specifier (:sentinel ,$3))
                       )
  ;;
  (ms-decl-spec
   -> "(" (? ms-decl-modifier-seq) ")"          => $2)
  (ms-decl-modifier-seq
   -> ms-decl-modifier                          => $1
   -> ms-decl-modifier-seq ms-decl-modifier     => (append $1 $2))
  (ms-decl-modifier
   -> ms-decl-identifier                        => nil ;; $1
   -> ms-decl-identifier "(" expression ")"     => nil ;; (list $1 $3)
   -> "align" "(" expression ")"                => (list `(:declaration-specifier (:align ,$3)))
   -> "noreturn"                                => (list `(:function-specifier :noreturn))
   -> "noinline"                                => (list `(:function-specifier :noinline))
   ;; -> "restrict"                                => (list `(:function-specifier :ms-restrict))
   -> :type-qualifier                           => nil ;xxx
   )
  (ms-decl-identifier
   -> :identifier
   -> :any-identifier)


  #||
  (ms-decl-spec                 -> "(" (? ms-decl-modifier-seq) ")" => $2
  -> "(" :error ")" => nil)
  (ms-decl-modifier-seq         -> ms-decl-modifier                      => (list $1)
  -> ms-decl-modifier-seq ms-decl-modifier => (append $1 (list $2)))
  (ms-decl-modifier             -> "align" "(" expression ")"
  => `(:type-qualifier (:align ,$3))
  -> :identifier
  => (warn "Cannot make sense of __declspec -- ~S" $1)
  nil
  -> :identifier "(" expression ")"
  => (warn "Cannot make sense of __declspec -- ~S" (list $1 $3))
  nil
  -> :identifier "(" :error ")"
  => (warn "Cannot make sense of __declspec -- ~S" (list $1 $3))
  nil
  )
  ||#

  ;; MS #pragma pack
  (pragma       -> "pack" "(" pragma-pack ")" => `(:pack ,$3))
  (pragma-pack  ->
                => `(:set nil)
                -> :integer-constant
                => `(:set ,(c-eval $1 nil))
                -> "push" (? pragma-pack-comma-ident) (? pragma-pack-comma-integer)
                => `(:push ,$2 ,$3)
                -> "pop" pragma-pack-comma-ident
                => `(:pop ,$2 nil)
                -> "pop" pragma-pack-comma-integer
                => `(:pop ,$2 nil)
                -> "pop"
                => `(:pop nil nil))
  (pragma-pack-comma-ident -> "," :identifier => $2)
  (pragma-pack-comma-integer -> "," :integer-constant => (c-eval $2 nil))


  ;; Objective C
  #+NOFFI-OBJC
  (enum-specifier
   -> "enum" :identifier                         => `(:enum ,$2)
   -> "enum" :identifier enum-def                => `(:enum ,$2 ,$3)
   -> "enum" enum-def                            => `(:enum nil ,$2)
   #||
   -> "enum" derived                             => `(:derived (:enum nil) ,$2)
   -> "enum" derived enum-def                    => `(:derived (:enum nil ,$3) ,$2)
   -> "enum" :identifier derived                 => `(:derived (:enum ,$2) ,$3)
   -> "enum" :identifier derived enum-def        => `(:derived (:enum ,$2 ,$4) ,$3)
   ||#
   -> "enum" derived                             => `(:enum nil)
   -> "enum" derived enum-def                    => `(:enum nil ,$3)
   -> "enum" :identifier derived                 => `(:enum ,$2)
   -> "enum" :identifier derived enum-def        => `(:enum ,$2 ,$4)
   )

  (enum-def -> "{" (++ "," enumerator) (? ",") "}" => $2)
  (derived  -> ":" type-name :--tie--              => $2)

  #-NOFFI-OBJC
  (enum-specifier
   -> "enum" (? :identifier)  "{" enumerator-list "}"            => `(:enum ,$2 ,(reverse $4))
   -> "enum" (? :identifier)  "{" enumerator-list "," "}"        => `(:enum ,$2 ,(reverse $4))
   -> "enum" :identifier                                      => `(:enum ,$2)
   )
  ;; (foo -> (+ :basic-type-specifier) -> :typedef-name)
  ;; (foo -> type-name)

  #+NOFFI-OBJC
  (external-declaration -> "@class" (++ "," objc-class) ";"
                        =>
                        (mapc (lambda (x) (announce-typedef x)) $2)
                        (list `(objc-class ,@$2)))
  ;;

  #+NOFFI-OBJC
  (external-declaration
   ->
   (? declaration-specifiers)
   objc-interface-or-protocol (++ "," objc-class) (? objc-super-class) (? objc-class-struct)
   :--tie--
   => 
   (mapc (lambda (x) (announce-typedef x)) $3)
   (let ((name (car $3)))               ;### No clue
     (declare (ignorable name))
     (append (list `(,$2 :class ,$3 :super ,$4))
             (when $5
               ;; Hmm
               (list `(decl ((:storage-class :typedef))
                            (,name (:struct ,name ,$5))))))))

  (objc-class -> :identifier (? objc-class-item-list)
              => $1)
  (objc-class-item-list
   -> "<" (++ "," type-name) ">" (? objc-class-item-list)
   -> "(" (** "," :identifier) ")" (? objc-class-item-list))

  #+(or)
  (external-declaration ->
                        objc-interface-or-protocol
                        (++ "," objc-class)
                        (? objc-super-class)
                        interface-tail
                        =>
                        (mapc (lambda (x) (announce-typedef x)) $2)
                        (list `(,$1 ,$2 ,$3 ,@$4)))
  #+(or)
  (external-declaration ->
                        :--tie-- (+ :storage-class) 
                        objc-interface-or-protocol
                        (++ "," objc-class)
                        (? objc-super-class)
                        interface-tail
                        =>
                        (mapc (lambda (x) (announce-typedef x)) $4)
                        (list `(,$3 ,$5 ,$3 ,@$6)))
  #+(or)
  (interface-tail ->
                  (? objc-class-struct)
                  (* objc-member-declaration)
                  "@end"
                  => (append $1 $2)
                  -> ";" => nil)
  (objc-super-class -> ":" objc-class)
  #-(or)
  (objc-class-struct -> "{" (? struct-declaration-list) "}" => $2)

  
  (external-declaration
   -> "+" objc-method-signature ";" => (list `(objc-class-method ,$2))
   -> "-" objc-method-signature ";" => (list `(objc-instance-method ,$2))
   -> "@property" (? "(" (** "," objc-property-property) ")" => $2) declaration-aux ";"
   => (list `(objc-property ,$2 ,$3))
   -> "@private" => (list :private)
   -> "@protected" => (list :protected)
   -> "@public"  => (list :public)
   -> "@required" => (list :required)
   -> "@optional" => (list :optional)
   -> "@package" => (list :package)
   -> "@end"     => (list :end)
   )

  (objc-method-signature
   -> (? "(" type-name ")" => $2) :identifier
   => `(:objc-method ,$2 ,$1 ())

   ->   (? "(" type-name ")" => $2)
        (+ (? :identifier) ":" "(" (? "out") type-name ")" :identifier => (list $1 $4 $6))
        (? "," "..." => t)
        (? (+ :function-specifier))
   => `(:objc-method ,(intern (format nil "~{~A:~}" (mapcar #'verbatim (mapcar 'car $2))) *c-package*)
                     ,$1
                     (,@(mapcar (lambda (param)
                                  `(decl nil (,(caddr param) ,(cadr param))))
                                $2)
                        ,@(and $3 '(&rest)))))
  (objc-property-property
   -> :type-qualifier => $1
   -> :identifier     => $1
   -> :any-identifier "=" :any-identifier => `(= ,$1 ,$3)
   -> :identifier "=" :any-identifier => `(= ,$1 ,$3)
   -> :any-identifier "=" :identifier => `(= ,$1 ,$3)
   -> :identifier "=" :identifier => `(= ,$1 ,$3) )

  (objc-interface-or-protocol
   -> "@interface" => 'objc-interface
   -> "@protocol"  => 'objc-protocol)
  )

(defun cons-progn (forms)
  (let ((res nil))
    (labels ((aux (form)
               (cond ((and (consp form) (eq (car form) 'progn))
                      (mapc #'aux (cdr form)))
                     (t
                      (push form res)))))
      (mapc #'aux forms)
      (if (and res (null (cdr res)))
          (car res)
          `(progn ,@(reverse res))))))

(defun concat-string-literals (string-literals)
  (cond ((null (cdr string-literals))
         (car string-literals))
        (t
         ;;
         ;; We cannot yet concatenate the literals because we haven't de-escaped
         ;; the literals yet and e.g. "\7" "7" is not the same as "\77".
         ;;
         (cons :string-literal-list string-literals))))

(defun interpret-ms-decl-spec (expr)
  (cond ((and (consp expr) (string= (car expr) "align"))
         ;; `(:declaration-specifier (:align ,(cadr expr)))
         `(:type-qualifier (:align ,(cadr expr))) )
        ((or (and (consp expr) (string= (car expr) "deprecated"))
             (and (symbolp expr)
                  (string= expr "deprecated")))
         nil)
        ((and (symbolp expr) (string= expr "dllimport"))
         nil)
        ((and (symbolp expr) (string= expr "noreturn"))
         `(:function-specifier :noreturn))
        ((and (symbolp expr) (string-equal expr "restrict"))
         ;; This is not what `restrict' is with ISO C. It applies to a function
         ;; and says something about its return type.
         `(:function-specifier :restrict))
        (t
         (warn "Cannot make sense of MS __declspec: ~S" expr)
         nil)))

;; __declspec(align(8))

(defun cons-and-note-declaration (declaration-specifiers init-declarator-list)
  ;; Caution: Both `declaration-specifiers' and `init-declarator-list' are in reverse.
  (let ((storage-classes nil)
        (qualifiers nil)
        (function-specifiers nil)
        (more-declaration-specifiers nil)
        (base-types nil)
        (types nil))
    (dolist (d declaration-specifiers)
      (ecase (car d)
        ((:storage-class)         (push d storage-classes))
        ((:type-qualifier)        (push d qualifiers))
        ((:function-specifier)    (push d function-specifiers))
        ((:declaration-specifier) (push d more-declaration-specifiers))
        ((:base-type)             (push d base-types))
        ((:_atomic)               (push d base-types))
        ((:type-name :struct :union :enum :typeof
                     :objc-angled :derived)
         (push d types))))
    ;; Special rule:
    ;;
    ;; If we the last thing is a type-name and there are other type specifiers
    ;; make that type-name being a declarator. This nicely catches an idempotent
    ;; typedef, like with "typedef int x; typedef x x;"
    ;;
    (when (and (eq :type-name (caar (last types)))              ;last thing is a typedef
               (null init-declarator-list)                      ;no apparent declarators
               (or (>= (length types) 2)                        ;some other type-name or aggregate type
                   base-types))                                 ; or some base type
      ;; fiddle
      (setq init-declarator-list (cons (cadr (car (last types))) init-declarator-list))
      (setq types (butlast types)))
    ;;
    (when (and (null base-types) (null types))
      (setq base-types '((:base-type :int))))
    ;; Figure out the type
    (let ((type
           (cond ((and base-types (not types) (every (lambda (x) (eq (car x) :base-type)) base-types))
                  (translate-base-type (mapcar #'cadr base-types)))
                 ((and (null base-types)
                       (= 1 (length types))
                       (member (caar types) '(:type-name :struct :union :enum :typeof :objc-angled :derived)))
                  (car types))
                 ((and (= 1 (length base-types))
                       (null types)
                       (member (caar base-types) '(:_atomic)))
                  (push `(:type-qualifier :_atomic) qualifiers)
                  (cadr (car base-types)))
                 (t
                  (error "Cannot make sense of type ~S" (append base-types types))))))
      ;;
      (dolist (q qualifiers)
        (setq type (copy-sloc q `(:type-qualifier ,(cadr q) ,type))))
      ;;
      (labels ((fold (inner declarator)
                 (cond ((symbolp declarator)
                        (values inner declarator))
                       (t (ecase (car declarator)
                            ((:pointer :block-pointer :amp-pointer :array :function :bit-field)
                             (fold (copy-sloc declarator
                                              `(,(car declarator) ,inner ,@(cddr declarator)))
                                   (cadr declarator)))
                            ((:function-specifier)
                             (let ((spec (list (car declarator) (cadr declarator)))
                                   (more (caddr declarator)))
                               (multiple-value-bind (type name)
                                   (fold inner more)
                                 (values (apply-function-specifiers (list spec) type) name))))
                            ((:declaration-specifier)
                             ;; ### clang applies these on the top-level type, while gcc applies it somewhere
                             ;; ### mid-way as it sees fit.
                             ;;
                             ;; This doesn't to the right thing with multiple declarators.
                             (push (copy-sloc declarator (subseq declarator 0 2)) more-declaration-specifiers)
                             (fold inner (caddr declarator)))
                            ((:type-qualifier :storage-class)
                             (fold (copy-sloc declarator
                                              `(,(car declarator) ,(cadr declarator) ,inner))
                                   (caddr declarator))))))))
        (let ((init-declarator-list
               (mapcar (lambda (init-declarator)
                         (multiple-value-bind (declarator init)
                             (if (and (consp init-declarator) (eq (car init-declarator) '=))
                                 (values (cadr init-declarator) (cddr init-declarator))
                                 (values init-declarator nil))
                           (multiple-value-bind (type name)
                               (fold type declarator)
                             (setq type (apply-function-specifiers function-specifiers type))
                             (list* name type init))))
                       (or init-declarator-list
                           ;; we need at least sth to declare
                           (list nil)))))
          (when (member :typedef storage-classes :key #'cadr)
            (mapc #'announce-typedef (mapcar #'car init-declarator-list)))
          `(decl (,@storage-classes ,@more-declaration-specifiers)
                 ,@ (reverse init-declarator-list)))))))

(defun apply-function-specifiers (function-specifiers type)
  (cond ((null function-specifiers) type)
        ((and (consp type) (member (car type) '(:function)))
         (dolist (q function-specifiers)
           (setq type (append type (list q))))
         type)
        ((and (consp type) (member (car type) '(:pointer)))
         `(:pointer ,(apply-function-specifiers function-specifiers (cadr type))
                    ,@(cddr type)))
        (t
         (blame (car function-specifiers)
                "~@<Function specifier ~S applied to non-function ~S.~@:>"
                function-specifiers
                type))))

(defun fold-type-name (specifier-qualifier-list declarator)
  ;; Lazy
  (let ((de (cons-and-note-declaration specifier-qualifier-list (list declarator))))
    (cadr (caddr de))))

(defun cons-member-declaration (specifier-qualifier-list declarators pack)
  (unless (null pack)
    (push `(:declaration-specifier (:pack ,pack)) specifier-qualifier-list))
  ;; Lazy
  (let ((decl (cons-and-note-declaration specifier-qualifier-list declarators)))
    (mapcar (lambda (x)
              `(decl ,(cadr decl) ,x))
            (cddr decl))))

(defun cons-defun (declaration-specifiers declarator kr-parameter-declaration-list compound-statement)
  ;; Hmm. Where to put the K&R parameter declarations?
  ;;
  (let ((decl (cons-and-note-declaration declaration-specifiers (list `(= ,declarator ,compound-statement)))))
    (when kr-parameter-declaration-list
      (assert (and (consp decl) (eq (car decl) 'decl)))
      (print (caddr decl))
      (assert (= 3 (length decl)))
      (let ((type (declaration-type decl)))
        (assert (and (consp type) (eq (car type) ':function)))
        (nconc type (list (list* :parameter-declarations kr-parameter-declaration-list)))))
    decl))


;;;; -- Folding -------------------------------------------------------------------------------

;; :char, :signed-char, :unsigned-char
;; :short, :unsigned-short
;; :int, :unsigned-int
;; :long, :unsigned-long
;; :long-long, :unsigned-long-long
;; :float
;; :double
;; :long-double
;; :void
;; :float-complex
;; :double-complex
;; :long-double-complex

;; (:pointer <type>)
;; (:const <type>)
;; (:volatile <type>)
;; <type> ::= (:function <result-type> <arg-list>)
;; <arg-list> ::= ( {<arg>}* )
;;              | :unspecified
;; <arg> ::= (<name> <type>)
;;         | &REST

;;              (:ENUM <name>)
;;              (:ENUM <name> ( <enum-key>* ))

;; <enum-key> ::= <symbol> | ( <symbol> <value> )

;; (:ARRAY <base>)
;; (:ARRAY <base> <size>)

;; (:STRUCT <name>)
;; (:STRUCT <name> ( <field>* ))

;; (:BIT-FIELD <base> <size>)

;; <field> ::= (<name> <type>)

;; (DECL <name> <type>)

(defun copy-sloc (from to)
  (unless *sloc-table*
    '(warn "No sloc table?"))
  (when *sloc-table*
    (multiple-value-bind (sloc win) (gethash from *sloc-table*)
      (unless win
        (warn "No sloc for ~S" from))
      (when win
        (setf (gethash to *sloc-table*) sloc))))
  to)

(defun fold-declare (declare)
  "Taking one DECLARE form as emited by the parse and folds
inside-out. Returns a list of DECL forms."
  ;; ### Where to put the sclasses?
  (destructuring-bind (sclasses base-type &rest declarators)
      (cdr declare)
    `(:de ,sclasses
          ,@(mapcar (lambda (declarator)
                      (multiple-value-bind (declarator init)
                          (if (typep declarator '(cons (member =) t))
                              (values (cadr declarator) (list (caddr declarator)))
                              (values declarator nil))
                        (multiple-value-bind (ident type)
                            (fold-declare-1 (translate-base-type base-type) declarator)
                          (copy-sloc declare `(,ident ,type ,@init)))))
                   (or declarators '(nil))))))

(defun fold-declare-1 (base-type declarator)
  "-> ident; type"
  (cond ((symbolp declarator)
         (values declarator base-type))
        ((typep declarator '(cons type-qualifier (cons t null)))
         (fold-declare-1 `(,(car declarator) ,base-type) (cadr declarator)))
        ((typep declarator '(cons (member :pointer :block-pointer :inline) (cons t null)))
         (fold-declare-1 `(,(car declarator) ,base-type) (cadr declarator)))
        ((typep declarator '(cons (member :function) (cons t (cons t list))))
         (destructuring-bind (base params &rest fun-specs) (cdr declarator)
           (fold-declare-1 `(,(car declarator) ,base-type ,(fold-parameter-list params)
                              ,@fun-specs)
                           base)))
        ((typep declarator '(cons (member :array) (cons t (cons null null))))
         (fold-declare-1 `(,(car declarator) ,base-type) (cadr declarator)))
        ((typep declarator '(cons (member :array) (cons t (cons t null))))
         (fold-declare-1 `(,(car declarator) ,base-type ,(caddr declarator)) (cadr declarator)))
        ((typep declarator '(cons (member :bit-field) (cons t (cons t null))))
         (fold-declare-1 `(,(car declarator) ,base-type ,(caddr declarator)) (cadr declarator)))
        (t
         (error "Cannot make sense of declarator ~S" declarator))))

(defun translate-base-type (base-type)
  (let ((qualifiers
         (remove-if-not (lambda (x) (typep x 'type-qualifier)) base-type))
        (base-type
         (remove-if (lambda (x) (typep x 'type-qualifier)) base-type)))
    (let ((res
           (cond
             ((abi-maybe-translate-base-type *abi* base-type))
             ((maybe-translate-integer-type base-type))
             ((maybe-translate-float-type base-type))
             ((and (null (cdr base-type))
                   (member (car base-type) '(:int8 :int16 :int32 :int64 :int128
                                             :uint8 :uint16 :uint32 :uint64 :uint128))
                   (car base-type)))
             ((equal '(:void) base-type) :void)
             ((equal '(:bool) base-type) :bool)
             ((equal '(:__builtin_va_list) base-type) (car base-type))
             ((equal '(:__noffi_lispval_t) base-type)   't)
             ((and (= 1 (length base-type))
                   (symbolp (car base-type))
                   (not (keywordp (car base-type))))
              (car base-type))
             ((typep base-type '(cons (cons (member :enum) (cons symbol null)) null))
              (car base-type))
             ((typep base-type '(cons (cons (member :struct :union) (cons symbol null)) null))
              (car base-type))
             ((typep base-type '(cons (cons (member :enum) (cons symbol (cons t null))) null))
              (destructuring-bind (name keys) (cdar base-type)
                `(:enum ,name
                        ,(mapcar (lambda (x)
                                   (etypecase x
                                     (symbol x)
                                     ((cons symbol (cons t null))
                                      x)))
                                 keys))))
             ((typep base-type '(cons (cons (member :struct :union) (cons symbol (cons t null))) null))
              (destructuring-bind (kind name fields) (car base-type)
                (copy-sloc (car base-type)
                           `(,kind ,name ,(fold-member-list fields)))))
             ((typep base-type '(cons (cons (member :typeof) (cons t null)) null))
              base-type)
             (t
              '(setq /base-type base-type)
              (error "Cannot make sense of type ~S~%IS-TYPEDEF says: ~S"
                     base-type
                     (mapcan (lambda (x)
                               (if (identifierp x)
                                   (list x (is-typedef x))))
                             base-type)
                     )))))
      (do () ((null qualifiers)) (setq res (list (pop qualifiers) res)))
      res)))

;; 11   There are three complex types, designated as float _Complex, double
;;      _Complex, and long double _Complex.33) The real floating and complex types
;;      are collectively called the floating types.


(defun maybe-translate-integer-type (type)
  (and (every (lambda (x) (member x '(:signed :unsigned :char :short :int :long :__int64 :__int128)))
              type)
       (let ((key (mapcar (lambda (x)
                            (count x type))
                          '(:signed :unsigned :char :short :int :long :__int64 :__int128))))
         (cond ((member key '((0 0 1 0 0 0 0 0)) :test 'equal) :char)
               ((member key '((1 0 1 0 0 0 0 0)) :test 'equal) :signed-char)
               ((member key '((0 1 1 0 0 0 0 0)) :test 'equal) :unsigned-char)
               ((member key '((0 0 0 1 0 0 0 0)
                              (1 0 0 1 0 0 0 0)
                              (0 0 0 1 1 0 0 0)
                              (1 0 0 1 1 0 0 0) ) :test 'equal) :short)
               ((member key '((0 1 0 1 0 0 0 0)
                              (0 1 0 1 1 0 0 0) ) :test 'equal) :unsigned-short)
               ((member key '((0 0 0 0 1 0 0 0)
                              (1 0 0 0 1 0 0 0)) :test 'equal) :int)
               ((member key '((0 1 0 0 1 0 0 0)) :test 'equal) :unsigned-int)
               ((member key '((0 0 0 0 0 0 0 0)
                              (1 0 0 0 0 0 0 0)) :test 'equal) :int)
               ((member key '((0 1 0 0 0 0 0 0)) :test 'equal) :unsigned-int)
               ((member key '((0 0 0 0 0 1 0 0)
                              (1 0 0 0 0 1 0 0)
                              (0 0 0 0 1 1 0 0)
                              (1 0 0 0 1 1 0 0) ) :test 'equal) :long)
               ((member key '((0 1 0 0 0 1 0 0)
                              (0 1 0 0 1 1 0 0) ) :test 'equal) :unsigned-long)
               ((member key '((0 0 0 0 0 2 0 0)
                              (1 0 0 0 0 2 0 0)
                              (0 0 0 0 1 2 0 0)
                              (1 0 0 0 1 2 0 0) ) :test 'equal) :long-long)
               ((member key '((0 1 0 0 0 2 0 0)
                              (0 1 0 0 1 2 0 0) ) :test 'equal) :unsigned-long-long)
               ((member key '((0 1 0 0 0 0 0 1) ) :test 'equal) :unsigned-int-128)
               ((member key '((0 0 0 0 0 0 1 0)
                              (1 0 0 0 0 0 1 0) ) :test 'equal) :long-long) ;### ABI
               ((member key '((0 1 0 0 0 0 1 0) ) :test 'equal) :unsigned-long-long) ;### ABI
               ((member key '((0 0 0 0 0 0 0 1)
                              (1 0 0 0 0 0 0 1) ) :test 'equal) :int-128)
               ((member key '((0 0 0 0 0 0 0 1)
                              (1 0 0 0 0 0 0 1) ) :test 'equal) :unsigned-int-128)
               (t
                (error "Non-sense: ~S" type))))))

(defun maybe-translate-float-type (type)
  (and (every (lambda (x) (member x '(:float :double :long :complex))) type)
       (let ((key (mapcar (lambda (x) (count x type)) '(:float :double :long :complex))))
         (cond ((member key '((1 0 0 0)) :test 'equal) :float)
               ((member key '((0 1 0 0)) :test 'equal) :double)
               ((member key '((0 1 1 0)) :test 'equal) :long-double)
               ((member key '((1 0 0 1)) :test 'equal) :float-complex)
               ((member key '((0 1 0 1)) :test 'equal) :double-complex)
               ((member key '((0 1 1 1)) :test 'equal) :long-double-complex)
               (t (error "Non-sense: ~S" type))))))

(defun fold-member-list (members)
  (mapcan (lambda (de)
            (etypecase de
              ((cons (member :de) t)
               (when (cadr de)
                 (warn "~@<Storage classes for a struct member? - ~S~@:>" de))
               (mapcar (lambda (x)
                         (when (cddr x)
                           (warn "~@<Initial value for a struct member? - ~S~@:>" x))
                         x)
                       (cddr de)))))
          members))

(defun fold-parameter-list (parameters)
  (cond ((eq :unspecified parameters) :unspecified)
        (t
         (let ((res
                (mapcan (lambda (x)
                          (etypecase x
                            ((member &rest) (list x))
                            ((cons (member :DE) t)
                             (destructuring-bind (de sclasses &rest defs)
                                 x
                               (assert (eq de :de))
                               (assert (= 1 (length defs)))
                               (unless (member sclasses '(() (:register)) :test #'equal)
                                 (blame parameters
                                        "Funny storage classes for a parameter - ~S"
                                        sclasses))
                               (copy-list defs)))))
                        parameters)))
           (if (typep res '(cons (cons t (cons (member :void))) null))
               nil
               res)))))


;;;; MS __declspec

(defun parse-ms-decl-spec-from-tokens (token-list)
  (values (parse-token-list token-list :grammar 'c99-parser :inject '(@ms-decl-spec))))


;;;; GCC attributes

;; GCC's __attribute__ follows no meaningful syntax. Although the manual says
;; that there is a syntax, this isn't adhere to. E.g. "introduced=10.12.2"
;; doesn't parse as an expression.

;; Therefore, we use our machete again. The manual says it's

;;             <attribute> -> "__attribute__" "(" "(" <attribute-list> ")" ")"
;;        <attribute-list> -> (** "," <attribute>)
;;             <attribute> ->
;;                         -> <any-identifier>
;;                         -> <any-identifier> "(" (** "," <attribute-parameter>) ")"
;;   <attribute-parameter> -> <any-identifier>
;;                         -> <any-identifier> (++ "," <expression>)
;;                         -> (** "," <expression>)

;; It's the <attribute-parameter> that doesn't work out here because what we
;; see isn't always some legal <expression>.

;; What we do is that we use an ad-hoc parser to chop the <attribute-list>
;; into <attribute>s and then look whether we know the first token and only
;; then pass that <attribute> to the parser proper.

;; TODO: We rather make error recovery work again and let it handle these
;; cases here.

(defun chop-parenthesized (token-list)
  (cond ((and token-list (token-cat-p (car token-list) :\())
         (let ((nest 0))
           (do ((q token-list (cdr q))) (nil)
             (cond ((null q) (return (values nil token-list nil)))
                   ((and (= nest 1) (token-cat-p (car q) :\)))
                    (return (values (ldiff (cdr token-list) q) (cdr q) t)))
                   ((token-cat-p (car q) :\() (incf nest))
                   ((token-cat-p (car q) :\)) (decf nest))))))
        (t (values nil token-list nil))))

(defun chop-by (sep token-list)
  (let ((p token-list) (nest 0) (bag nil))
    (do ((q token-list (cdr q))) (nil)
      (cond ((null q)
             (push (ldiff p q) bag)
             (return (reverse bag)))
            ((and (eql 0 nest) (token-cat-p (car q) sep))
             (push (ldiff p q) bag)
             (setq p (cdr q)))
            ((token-cat-p (car q) :\() (incf nest))
            ((token-cat-p (car q) :\{) (incf nest))
            ((token-cat-p (car q) :\[) (incf nest))
            ((token-cat-p (car q) :\)) (decf nest))
            ((token-cat-p (car q) :\}) (decf nest))
            ((token-cat-p (car q) :\]) (decf nest))))))            

(defun token-spelling (token)
  (cond ((eq (token-cat token) :identifier)
         (token-val token))
        ((eq (token-cat token) :any-identifier)
         (token-val token))
        ((eq (token-cat token) 'multi-token)
         (some #'token-spelling (token-val token)))))

(defun parse-gcc-attribute-from-tokens (token-list &optional m)
  (multiple-value-bind (it more win) (chop-parenthesized token-list)
    (and win (null more)
         (multiple-value-bind (it more win) (chop-parenthesized it)
           (and win (null more)
                (let ((attributes (chop-by :\, it)))
                  (mapcan (lambda (attribute)
                            (handler-case
                                (list
                                 (parse-token-list attribute
                                                  :grammar 'c99-parser
                                                  :inject '(/gcc-attribute)
                                                  :proto-machine m))
                              (noffi-parsing-error (c)
                                (declare (ignore c))
                                nil)))
                          attributes) ))))))


;;;; ------------------------------------------------------------------------------------------

(defun token-list-declaration-subseqs (token-list)
  (let ((stack nil)                     ;stack of parens
        (cur nil)                       ;current subseq
        (res nil)                       ;all subseqs
        (last-was-paren-expr nil))
    (macrolet ((spill () `(when cur (push (reverse cur) res) (setq cur nil))))
      ;;
      (do ((q token-list (cdr q)))
          ((endp q))
        (let ((token (car q)))
          (push token cur)
          (case (token-main-cat token)
            ((:\( :\[ :\{) (push (token-cat token) stack))
            (:\) (assert (eq :\( (car stack))) (pop stack))
            (:\] (assert (eq :\[ (car stack))) (pop stack))
            (:\} (assert (eq :\{ (car stack)) nil "~S" stack) (pop stack)
                 (when (and (null stack) last-was-paren-expr)
                   (spill)))
            (:\; (when (and (null stack)
                            ;; This is here to catch K&R functions.
                            (or (null (cdr q))
                                (not (eql :\{ (token-cat (cadr q))))))
                   (spill)))
            ((:\+ :\-)
             (when (and (null stack)
                        (or (member :@interface cur :key #'token-main-cat)
                            (member :@protocol cur :key #'token-main-cat)))
               (pop cur)
               (spill)
               (push token cur)))
            #+NIL
            ((:@interface :@protocol)
             ;; 
             (do ((p (cdr q) (cdr p)))
                 ((or (null p)
                      (eql (token-main-cat (car p)) :@end))
                  (unless p (error "No @end?"))
                  (when p (push (car p) cur))
                  (spill)
                  (setq q p))
               (push (car p) cur))
             '(let ((p (member :@end q :key #'token-main-cat)))
               (pop cur) (spill)
               (unless p (error "No @end?"))
               (push (ldiff q (cdr p)) res)
               (setq q p)))
            ((:@end)
             (pop cur) (spill)
             (push token cur) (spill))
            #+(or)
            ((:@end)
             (blame-for-token (car q) "Stray @end?"))
            (:eof (pop cur)))
          (when (null stack)
            (setq last-was-paren-expr (eq :\) (token-cat token)))) ))
      ;;
      (spill)
      (reverse res))))
