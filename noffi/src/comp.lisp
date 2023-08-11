(in-package :noffi)

(defparameter *global-env*
  (make-hash-table :test 'equal))

(defun c-macro-1 (def)
  (cpp def))

(defun c-header-seen-1 (header-name)
  (setf (de.bauhh.cpp::header-file-seen-p header-name) t))


;;;;

(defun decl-1 (storage-classes declarators)
  (dolist (declarator declarators)
    (destructuring-bind (name type &rest init) declarator
      (when name
        (global-enter name `(decl ,storage-classes ,declarator)))
      (labels ((walk (type)
                 (cond ((atom type) type)
                       ((and (member (car type) '(:struct :union))
                             (cadr type)
                             (caddr type))
                        (global-enter `(,(car type) ,(cadr type)) type)
                        type)
                       ((and (member (car type) '(:enum)))
                        (when (cadr type)
                          (global-enter `(:enum ,(cadr type)) type))
                        (when (caddr type)
                          (let ((last-val -1))
                            (dolist (key (caddr type))
                              (let ((key (if (consp key) (car key) key))
                                    (val (if (consp key) (c-eval (cadr key)) (1+ last-val))))
                                (global-enter key `(decl ((:storage-class :enum-key))
                                                         (,key (:enum ,(cadr type)) ,val)))
                                (setq last-val val)))))
                        type)
                       ((and (member (car type) '(:struct :union))
                             (cddr type)
                             (cadr type))
                        ;; ### Expand array sizes and such.
                        (setq type (mapcar #'walk type))
                        (global-enter `(,(car type) ,(cadr type)) type)
                        type)
                       ((and (eql (car type) :array)
                             (caddr type))
                        `(:array ,(walk (cadr type))
                                 (c-eval ,(caddr type) nil)))
                       (t
                        ;; ###
                        (mapcar #'walk type)))))
        (walk type))))
  (remove nil (mapcar #'car declarators)))

(defun global-enter (key def)
  (let ((old (gethash key *global-env*)))
    (when old
      (cond ((identifierp key)
             ;; storage class?
             (unless (types-compatible-p (declaration-type old) (declaration-type def))
               (warn "Incompatible redefinition of ~S~%old = ~S~%new = ~S"
                     key old def)))
            ((and (consp key) (member (car key) '(:enum :struct :union)))
             (unless (types-compatible-p old def)
               (warn "Incompatible redefinition of ~S~%old = ~S~%new = ~S"
                     key old def)))
            (t
             (when (and old (not (equal old def)))
               (warn "Incompatible redefinition of ~S~%old = ~S~%new = ~S"
                     key old def)))))
    (setf (gethash key *global-env*) def))
  key)


(defmacro c-macro (def)
  `(c-macro-1 ',def))

(defmacro c-header-seen (header-name)
  `(c-header-seen-1 ',header-name))

(defmacro decl (storage-classes &rest declarators)
  `(decl-1 ',storage-classes '(,@ declarators)))

(defmacro objc-interface (&rest xs) nil)
(defmacro objc-protocol (&rest xs) nil)
(defmacro objc-class (&rest xs) nil)
(defmacro objc-class-method (&rest xs) nil)
(defmacro objc-instance-method (&rest xs) nil)
(defmacro objc-property (&rest xs) nil)


;;;; Constant Expression Evaluator

;; Actually, what we ought to do is to use the compiler here.

;;; Integer Literals

;; We do this by the spec.

(defun comp-integer-constant (form env)
  ;; We just literally implement the spec here.
  (destructuring-bind (base value suffix) (cdr form)
    (let ((putative-types
           (ecase base
             (:decimal
              (ecase suffix
                ((nil)    '(:int :long :long-long))
                ((:u)     '(:unsigned-int :unsigned-long :unsigned-long-long))
                ((:l)     '(:long :long-long))
                ((:ul)    '(:unsigned-long :unsigned-long-long))
                ((:ll)    '(:long-long))
                ((:ull)   '(:unsigned-long-long))))
             ((:octal :hex)
              (ecase suffix
                ((nil)    '(:int :unsigned-int :long :unsigned-long :long-long :unsigned-long-long))
                ((:u)     '(:unsigned-int :unsigned-long :unsigned-long-long))
                ((:l)     '(:long :unsigned-long :long-long :unsigned-long-long))
                ((:ul)    '(:unsigned-long :unsigned-long-long))
                ((:ll)    '(:long-long :unsigned-long-long))
                ((:ull)   '(:unsigned-long-long)))))))
      (dolist (putative-type putative-types
               (blame form
                      "Integer constant too large to fit any integer type"))
        (when (<= (integer-type-min-value putative-type env)
                  value
                  (integer-type-max-value putative-type env))
          (return (values value putative-type)))))))


(defun c-eval (expr &optional env)
  (multiple-value-bind (form type) (comp-expr expr env)
    (values (as-lisp (eval form)) type)))


;;;; Integer Type Promotion

(defun promoted-integer-type (type env)
  (cond ((not (integer-type-p type)) type)
        ;; 6.3.1.1 §2
        ;;
        ;; — A bit-field of type _Bool, int, signed int, or unsigned int.
        ((<= (integer-type-rank type) 0)
         (cond ((and (<= (integer-type-min-value :int env) (integer-type-min-value type env))
                     (>= (integer-type-max-value :int env) (integer-type-max-value type env)))
                :int)
               ((and (<= (integer-type-min-value :unsigned-int env) (integer-type-min-value type env))
                     (>= (integer-type-max-value :unsigned-int env) (integer-type-max-value type env)))
                :unsigned-int)
               (t
                type)))
        (t type)))

;; Ouch. Enums have a base type as well. *sigh*


;;; Enumerations

;; C99 basically leaves it completely open what integer type should be picked
;; for an enum. The AMD64 ABI sets some rules, which however are not followed
;; by gcc.

;; On AMD64 gcc appears to do the following: Look at all the enumeration
;; constants, and then in pick the smallest integer type from "unsigned, int,
;; unsigned long, long, unsigned long long, long long" which fits the
;; constants.

;; This is what we do as well then.


;;;; Arithmetic Conversion

;; Before doing arithmetic types are promoted, the effect is that anything of
;; lesser rank than 'int' is "upgraded" to 'int'. At this stage we do not
;; expect that values need to be altered in some way.

;; | First, if the corresponding real type of either operand is long double, the other
;; | operand is converted, without change of type domain, to a type whose
;; | corresponding real type is long double.

;; | Otherwise, if the corresponding real type of either operand is double, the other
;; | operand is converted, without change of type domain, to a type whose
;; | corresponding real type is double.

;; | Otherwise, if the corresponding real type of either operand is float, the other
;; | operand is converted, without change of type domain, to a type whose
;; | corresponding real type is float.51)

;; | Otherwise, the integer promotions are performed on both operands. Then the
;; | following rules are applied to the promoted operands:

;; |        If both operands have the same type, then no further conversion is needed.
;; |        Otherwise, if both operands have signed integer types or both have unsigned
;; |        integer types, the operand with the type of lesser integer conversion rank is
;; |        converted to the type of the operand with greater rank.

;; |        Otherwise, if the operand that has unsigned integer type has rank greater or
;; |        equal to the rank of the type of the other operand, then the operand with
;; |        signed integer type is converted to the type of the operand with unsigned
;; |        integer type.

;; |        Otherwise, if the type of the operand with signed integer type can represent
;; |        all of the values of the type of the operand with unsigned integer type, then
;; |        the operand with unsigned integer type is converted to the type of the
;; |        operand with signed integer type.

;; |        Otherwise, both operands are converted to the unsigned integer type
;; |        corresponding to the type of the operand with signed integer type.


(defun usual-arithmetic-conversion (type-a type-b env)
  "Given two types return the common type that arithmetic is done in."
  (cond ((and (float-type-p type-a env)
              (float-type-p type-b env))
         (if (> (float-type-rank type-a env) (float-type-rank type-b env))
             type-a
             type-b))
        ((float-type-p type-a) type-a)
        ((float-type-p type-b) type-b)
        (t
         (setq type-a (promoted-integer-type type-a env)
               type-b (promoted-integer-type type-b env))
         (cond ((and (integer-type-p type-a) (integer-type-p type-b))
                (cond
                  ;; | If both operands have the same type, then no further conversion is needed.
                  ;;
                  ;; We take this as "of same rank". Doesn't matter as promotion already made
                  ;; the types 'canonic' in a way.
                  ((and (eql (integer-type-rank type-a) (integer-type-rank type-b))
                        (eql (if (integer-type-signed-p type-a) t nil)
                             (if (integer-type-signed-p type-b) t nil)))
                   type-a)
                  ;; | Otherwise, if both operands have signed integer types or both have unsigned
                  ;; | integer types, the operand with the type of lesser integer conversion rank is
                  ;; | converted to the type of the operand with greater rank.
                  ((and (eql (if (integer-type-signed-p type-a) t nil)
                             (if (integer-type-signed-p type-b) t nil)))
                   (if (> (integer-type-rank type-a) (integer-type-rank type-b))
                       type-a
                       type-b))
                  (t
                   ;; Mixed signs, one is signed and the other is unsigned.
                   (multiple-value-bind (type-s type-u)
                       (if (integer-type-signed-p type-a)
                           (values type-a type-b)
                           (values type-b type-a))
                     (cond
                       ;; | Otherwise, if the operand that has unsigned integer type has rank greater or
                       ;; | equal to the rank of the type of the other operand, then the operand with
                       ;; | signed integer type is converted to the type of the operand with unsigned
                       ;; | integer type.
                       ((>= (integer-type-rank type-u) (integer-type-rank type-s))
                        type-u)
                       ;; | Otherwise, if the type of the operand with signed integer type can represent
                       ;; | all of the values of the type of the operand with unsigned integer type, then
                       ;; | the operand with unsigned integer type is converted to the type of the
                       ;; | operand with signed integer type.
                       ((and (<= (integer-type-min-value type-s) (integer-type-min-value type-u))
                             (>= (integer-type-max-value type-s) (integer-type-max-value type-u)))
                        type-s)
                       ;; | Otherwise, both operands are converted to the unsigned integer type
                       ;; | corresponding to the type of the operand with signed integer type.
                       (t
                        ;; Hillarious
                        (find-integer-type-with-rank-and-signedess
                         (integer-type-rank type-s env)
                         nil)))))) )))))

(defun find-integer-type-with-rank-and-signedess (rank sigedness &optional env)
  (car (or (find-if (lambda (type-desc)
                      (and (eql (if (fourth type-desc) t nil)
                                (if sigedness t nil))
                           (eql (fifth type-desc) rank)))
                    (abi-integer-types))
           (error "We miss ~A integer type of rank ~D"
                  rank
                  (verbatim (if sigedness "a signed" "an unsigned"))))))


         
;; Binary:

;; * / % + - << >> < > <= >= == != & ^ | && ||

;; = *= /= /= %= += -= <<= >>= &= ^= |=

;; ,

;;;;;;;

;; ?:

;; < > <= >= == != && ||

;; = *= /= /= %= += -= <<= >>= &= ^= |=

;; ,

;; ?:

;; Usual AC:                    + - * / ?:
;; Usual AC int only:           % & | ^

;; First arg: << >>

;; Boolean, put perhaps usual?
;;     && || < <= > >= == !=

;; 


;;;;

(defun comp-expr (form env)
  (cond ((identifierp form)
         (comp-identifier form env))
        ((characterp form)              ;xxx
         (values (char-code form) :int))
        ((eq (car form) ':integer-constant)
         (comp-integer-constant form env))
        ((eq (car form) ':floating-constant)
         (destructuring-bind (value type) (cdr form)
           (values value type)))
        ((and (eq (car form) 'the) (= 3 (length form))) (comp-cast form env))
        ((and (eq (car form) '*) (= 3 (length form))) (comp-arithmetic '* '* form env))
        ((and (eq (car form) '/) (= 3 (length form))) (comp-arithmetic 'truncate '/ form env)) ;<---- ABI
        ((and (eq (car form) '%) (= 3 (length form))) (comp-arithmetic 'rem nil form env)) ;<---- ABI
        ((and (eq (car form) '+) (= 3 (length form))) (comp-arithmetic '+ '+ form env))
        ((and (eq (car form) '-) (= 3 (length form))) (comp-arithmetic '- '- form env))
        ((and (eq (car form) '<<) (= 3 (length form))) (comp-shift +1 form env))
        ((and (eq (car form) '>>) (= 3 (length form))) (comp-shift -1 form env))
        ((and (eq (car form) '<)  (= 3 (length form))) (comp-compare '<  form env))
        ((and (eq (car form) '<=) (= 3 (length form))) (comp-compare '<= form env))
        ((and (eq (car form) '>)  (= 3 (length form))) (comp-compare '>  form env))
        ((and (eq (car form) '>=) (= 3 (length form))) (comp-compare '>= form env))
        ((and (eq (car form) '=)  (= 3 (length form))) (comp-compare '=  form env))
        ((and (eq (car form) '/=) (= 3 (length form))) (comp-compare '/= form env))
        ;;
        ((and (eq (car form) 'logand) (= 3 (length form))) (comp-arithmetic 'logand nil form env))
        ((and (eq (car form) 'logxor) (= 3 (length form))) (comp-arithmetic 'logxor nil form env))
        ((and (eq (car form) 'logior) (= 3 (length form))) (comp-arithmetic 'logior nil form env))
        ;;
        ((and (eq (car form) 'and) (= 3 (length form))) (comp-boolean-expr 'and form env))
        ((and (eq (car form) 'or)  (= 3 (length form))) (comp-boolean-expr 'or form env))
        ;;
        ((and (eq (car form) 'lognot) (= 2 (length form))) (comp-unop 'lognot nil form env))
        ((and (eq (car form) '-) (= 2 (length form))) (comp-unop '- '- form env))
        ((and (eq (car form) '+) (= 2 (length form))) (comp-unop 'progn 'progn form env))
        ;;
        ((and (eq (car form) 'not)  (= 2 (length form)))         (comp-not form env))
        ((and (eq (car form) 'aref) (= 2 (length form)))         (comp-aref/1 form env))
        ;;
        ((and (eq (car form) '--form--) (= 3 (length form)))
         (values (cadr form) (caddr form)))
        ;;
        ((and (eq (car form) 'funcall))
         (comp-funcall form env))
        ;;
        (t
         (error "Don't know how to compile ~S" form))))

(defun comp-identifier (identifier env)
  (let ((decl (find-identifier-declaration identifier env :errorp t)))
    (cond ((enum-key-declaration-p decl)
           (values (declaration-init decl)
                   (declaration-type decl)))
          ((extern-declaration-p decl)
           (let ((type (declaration-type decl)))
             (cond 
               ;; Now, when the thingy is an array we rather return a pointer
               ((array-type-p type)
                (multiple-value-bind (loc loc-type)
                    (comp-identifier-loc identifier env)
                  (assert (pointer-type-p loc-type))
                  (assert (array-type-p (pointer-type-base loc-type)))
                  (let ((rtype (make-pointer-type (array-type-base type env))))
                    (values `(coerce-sap ,loc ',rtype) rtype))))
               ;; Similar for functions
               ((function-type-p type)
                (multiple-value-bind (loc loc-type)
                    (comp-identifier-loc identifier env)
                  (assert (pointer-type-p loc-type))
                  (assert (function-type-p (pointer-type-base loc-type)))
                  (let ((rtype (make-pointer-type type)))
                    (values `(coerce-sap ,loc ',rtype) rtype))))
               (t
                (comp-expr (make-unop-expr
                            'aref
                            (multiple-value-call #'make-compiled-expr
                              (comp-identifier-loc identifier env)))
                           env)))))
          (t
           (blame identifier "No idea what to make of ~S" identifier)))))

(defun comp-identifier-loc (identifier env)
  (let ((decl (find-identifier-declaration identifier env :errorp t)))
    (cond ((enum-key-declaration-p decl)
           (blame identifier "An enum constant is not a valid lvalue"))
          ((extern-declaration-p decl)
           (values `(extern-ref ,(identifier-name identifier)
                                ',(declaration-type decl))
                   (make-pointer-type (declaration-type decl))))
          (t
           (blame identifier "No idea what to make of ~S" identifier)))))

(defun comp-aref/1 (expr env)
  (destructuring-bind (loc-expr) (cdr expr)
    (multiple-value-bind (loc-form loc-type)
        (comp-expr loc-expr env)
      (assert (pointer-type-p loc-type))
      (let ((type (pointer-type-base loc-type)))
        (cond ((integer-type-p type env)
               (values
                (if (integer-type-signed-p type env)
                    (ecase (integer-type-size type env)
                      (  8 `(peek-s8 ,loc-form 0))
                      ( 16 `(peek-s16 ,loc-form 0))
                      ( 32 `(peek-s32 ,loc-form 0))
                      ( 64 `(peek-s64 ,loc-form 0))
                      (128 `(peek-s128 ,loc-form 0)))
                    (ecase (integer-type-size type env)
                      (  8 `(peek-u8 ,loc-form 0))
                      ( 16 `(peek-u16 ,loc-form 0))
                      ( 32 `(peek-u32 ,loc-form 0))
                      ( 64 `(peek-u64 ,loc-form 0))
                      (128 `(peek-u128 ,loc-form 0))))
                type))
              ((pointer-type-p type env)
               (values `(peek-ptr ,loc-form ',type)
                       type))
              ((function-type-p type env)
               (values loc-form loc-type))
              (t
               (blame expr "Don't know how to dereference some ~S" loc-type)))))))

(defun make-unop-expr (op expr)
  `(,op ,expr))

(defun make-compiled-expr (form type)
  `(--form-- ,form ,type))

(defun identifier-name (identifier)
  (symbol-name identifier))

(defun comp-cast (expr env)
  (destructuring-bind (out-type expr) (cdr expr)
    (multiple-value-bind (form type) (comp-expr expr env)
      (values (comp-coerce out-type type form env)
              out-type))))

(defun comp/+ (form env)
  (destructuring-bind (expr-1 expr-2) (cdr form)
    (multiple-value-bind (form-1 type-1) (comp-expr expr-1 env)
      (multiple-value-bind (form-2 type-2) (comp-expr expr-2 env)
        (cond ((and (arithmetic-type-p type-1)
                    (arithmetic-type-p type-2))
               (let ((res-type (usual-arithmetic-conversion type-1 type-2 env)))
                 (values (comp-coerce res-type 't
                                      `(+ ,(comp-coerce res-type type-1 form-1 env)
                                          ,(comp-coerce res-type type-2 form-2 env))
                                      env)
                         res-type)))
              (t
               (blame form "Don't know how to compile ~S" form)))))))

(defun comp-not (expr env)
  (destructuring-bind (expr) (cdr expr)
    (let ((form (comp-boolean expr env)))
      (values `(if ,form 0 1) (the-int-type env)))))

(defun comp-boolean (expr env)
  (multiple-value-bind (form type) (comp-expr expr env)
    (cond ((arithmetic-type-p type env)
           (values `(not (zerop ,form))
                   (the-int-type env)))
          ((pointer-type-p type env)
           (values `(not (null-ptr-p ,form))))
          (t
           (blame expr "Cannot make sense of ~S as a boolean" expr)))))

(defun comp-boolean-expr (op expr env)
  (destructuring-bind (lhs rhs) (cdr expr)
    (values
     `(if (,op ,(comp-boolean lhs env)
               ,(comp-boolean rhs env))
          1 0)
     (the-int-type env))))

(defun comp-arithmetic (int-op flo-op expr env)
  "Compiles a usual arithmetic expression. `int-op' is the Lisp function to
use for integer arguments, `flo-op' is the Lisp function to use for floating
point arguments."
  ;; ### + and - for pointers
  (destructuring-bind (expr-1 expr-2) (cdr expr)
    (multiple-value-bind (form-1 type-1) (comp-expr expr-1 env)
      (multiple-value-bind (form-2 type-2) (comp-expr expr-2 env)
        (cond ((and (arithmetic-type-p type-1)
                    (arithmetic-type-p type-2))
               (let ((res-type (usual-arithmetic-conversion type-1 type-2 env)))
                 (values (comp-coerce res-type 't
                                      `(,(if (integer-type-p res-type env)
                                             (or int-op
                                                 (blame expr "Cannot do with integers"))
                                             (or flo-op
                                                 (blame expr "Cannot do with floats")))
                                         ,(comp-coerce res-type type-1 form-1 env)
                                         ,(comp-coerce res-type type-2 form-2 env))
                                      env)
                         res-type)))
              (t
               (blame expr "Don't know how to compile arithmetic ~S with types ~S and ~S"
                      expr
                      type-1
                      type-2
                      )))))))

(defun comp-unop (int-op flo-op form env)
  (destructuring-bind (expr-1) (cdr form)
    (multiple-value-bind (form-1 type-1) (comp-expr expr-1 env)
      (cond ((arithmetic-type-p type-1)
             (let ((res-type (usual-arithmetic-conversion type-1 type-1 env))) ;Hmm
               (values (comp-coerce res-type 't
                                    `(,(if (integer-type-p res-type env)
                                           (or int-op
                                               (blame form "Cannot do with integers"))
                                           (or flo-op
                                               (blame form "Cannot do with floats")))
                                       ,(comp-coerce res-type type-1 form-1 env))
                                    env)
                       res-type)))
            (t
             (blame form "Don't know how to compile ~S" form))))))

(defun comp-compare (op form env)
  "Compiles a comparison."
  (destructuring-bind (expr-1 expr-2) (cdr form)
    (multiple-value-bind (form-1 type-1) (comp-expr expr-1 env)
      (multiple-value-bind (form-2 type-2) (comp-expr expr-2 env)
        (cond ((and (arithmetic-type-p type-1)
                    (arithmetic-type-p type-2))
               (let ((res-type (usual-arithmetic-conversion type-1 type-2 env)))
                 (values `(if (,op
                               ,(comp-coerce res-type type-1 form-1 env)
                               ,(comp-coerce res-type type-2 form-2 env))
                              1 0)
                         (the-int-type env))))
              (t
               (blame form "Don't know how to compile ~S" form)))))))

(defun comp-shift (dir form env)
  (destructuring-bind (expr-1 expr-2) (cdr form)
    (multiple-value-bind (form-1 type-1) (comp-expr expr-1 env)
      (multiple-value-bind (form-2 type-2) (comp-expr expr-2 env)
        (cond ((and (integer-type-p type-1 env)
                    (integer-type-p type-2 env))
               (let ((res-type (promoted-integer-type type-1 env)))
                 (values (comp-coerce res-type 't
                                      `(ash ,(comp-coerce res-type type-1 form-1 env)
                                            (* ,dir
                                               (mod
                                                ;; coerce? shouldn't be needed
                                                ,form-2
                                                ,(integer-type-size res-type))))
                                      env)
                         res-type)))
              (t
               (blame form "Don't know how to compile ~S" form)))))))

(defun the-int-type (env)
  (find-integer-type-with-rank-and-signedess 0 t env))

(defun comp-coerce (out-type in-type form env
                    &aux it)
  (cond ;; Cannot do as this is also used for claming a result.
        ((equal (bare-expanded-type out-type env)
                (bare-expanded-type in-type env))
         form)
        ((bool-type-p out-type)
         (values `(if ,(comp-boolean `(--form-- ,form ,in-type) env) 1 0)
                 out-type))
        ((integer-type-p out-type)
         (if (integer-type-signed-p out-type)
             `(sldb (byte ,(integer-type-size out-type) 0)
                    ,form)
             `(ldb (byte ,(integer-type-size out-type) 0)
                   ,form)))
        ((setq it (float-type-p out-type))
         `(coerce ,form ',(fourth it)))
        (t
         (blame form "Don't know how to coerce to ~S" out-type))))


;;;; lvalues

;; We borrow our ideas here from SETF. We implement an "lvalue expansion",
;; which like with SETF is a set of temponaries, their values, a load form, a
;; store form. However, this being C, we also have types here.

;; The question however is: How do we cope with structures, which have value
;; semantics? I mean, most of the cases an lvalue would be just a pointer, an
;; offset, and a size. We'll see.

;; However, this pointer and offset approach cannot work with lexicals for
;; that lexicals have no address. The most tricky however is structures passed
;; and returned from functions.

;; With CCL we could do as there are GC-able heap pointers. Which however is
;; nothing that we could count on.


;;;; Runtime

#||
(defun ptr-lessp (ptr-1 ptr-2))
(defun ptr-difference (ptr-1 ptr-2))
(defun ptr-inc (ptr delta))
(defun ptr-int (ptr))
(defun int-ptr (int))

(defun peek-u8 (ptr offset) ...)
(defun peek-u16 (ptr offset) ...)
(defun peek-u32 (ptr offset) ...)
(defun peek-u64 (ptr offset) ...)
(defun peek-u128 (ptr offset) ...)
(defun peek-i8 (ptr offset) ...)
(defun peek-i16 (ptr offset) ...)
(defun peek-i32 (ptr offset) ...)
(defun peek-i64 (ptr offset) ...)
(defun peek-i128 (ptr offset) ...)
(defun peek-float (ptr offset) ...)
(defun peek-double (ptr offset) ...)
||#

;; character const
;; string-literal
;; aref
;; offsetof
;; \. ->
;; post-inc post-dec pre-inc pre-dec
;; :object
;; &
;; unary aref
;; not
;; sizeof-expr
;; sizeof-type
;; + for ptrs
;; - for ptrs
;; comparison for ptrs
;; and, or, ?:
;; ternary ?:
;; setf
;; setf/op
;; progn
;; 

;; I want that those init-values for enums are -- even when evaluated -- expr
;; as well. Which is actually sth we cannot do, as we have a circular
;; dependency here.


;;; Enums

;; We get very different behavior between gcc, clang, and MSVC.

;; First gcc and clang are funny and default to enums being unsigned, while
;; the enum members are signed or unsigned as it pleases them.

;; MSVC appears to be the easiest. Each enum, even if given a member like
;; 0x800000000ULL seems to be of type 'int'.

;; gcc and clang seem to apply the following algorithm. Evaluate all enum
;; constants with usual rules and then take the actual numeric values and find
;; the smallest type that fits in this order: unsigned, int, unsigned long,
;; long.





;;;; -- sizeof, alignof, and offsetof ---------------------------------------------------------

;; In the presnece of VLA all of these must return an expression in general.


(defun sizeof-type-expr (type env)
  (cond ((integer-type-p type env)
         (make-integer-constant-expr (/ (integer-type-size type env) 8) (size_t-type)))
        ((float-type-p type env)
         (make-integer-constant-expr (/ (float-type-size type env) 8) (size_t-type)))
        ((pointer-type-p type env)
         (make-integer-constant-expr (/ (pointer-type-size type env) 8) (size_t-type)))
        ((array-type-p type env)
         (make-binary-expr '* (array-type-count type env) (sizeof-type-expr (array-type-base type env) env)))
        (t
         (blame type "Cannot tell size of ~S" type))))

(defun make-integer-constant-expr (constant type)
  `(--form-- ,constant ,type))

(defun make-binary-expr (op lhs rhs)
  `(,op ,lhs ,rhs))


;;;; -- lvalues -------------------------------------------------------------------------------

;; For lvalues we begin with the simple route and implement is by taking the
;; address of that lvalue. This makes the -> operator the primitive and \. the
;; abbreviation.

;; The other alternative is to go the SETF route. Or we have some specific
;; compiler routine for a chain of AREF and RREF, like we would need with
;; :OBJECT as well.

(defun comp-address-of (expr env)
  "Compiles `expr' as an lvalue an returns a value of pointer type."
  (cond ((identifierp expr)
         (comp-identifier-loc expr env))
        (t
         (blame expr "Don't know how to take the address of")
         )))

;;;; -- External Symbols ----------------------------------------------------------------------

;; For now we are lazy here. We compile the fuction and all the arguments one
;; for one.

(defun comp-funcall (expr env)
  (destructuring-bind (fun &rest args) (cdr expr)
    (multiple-value-bind (fun fun-type)
        (comp-expr fun env)
      (unless (and (pointer-type-p fun-type env)
                   (function-type-p (pointer-type-base fun-type env) env))
        (blame fun "Not a function pointer type"))
      (let ((arg-forms-types (mapcar (lambda (x)
                                       (multiple-value-list (comp-expr x env)))
                                     args)))
        nil))))
        
(defmacro c-form (form &environment env)
  `(as-lisp ,(comp-expr form env)))

(defun clr ()
  (clrhash *global-env*)
  (clrhash *global-env-sloc*)
  (de.bauhh.cpp::cpp-reset)
  (boot-ht))

(defun invoke-with-cleared-database (continuation)
  (let ((*global-env* (make-hash-table :test #'equal))
        (*ht* nil)
        (de.bauhh.cpp::*defs* (de.bauhh.cpp::make-defs-table)))
    (boot-ht)
    (funcall continuation)))
