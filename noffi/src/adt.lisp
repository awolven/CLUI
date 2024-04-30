;; -*- Mode: Lisp; -*-
;; ---------------------------------------------------------------------------
;;     Title: Abstract Data Type for Nocturnal C Types
;;   Created: 2022-12-17
;;    Author: Gilbert Baumann <gilbert@bauhh.de>
;;   License: MIT style (see below)
;; ---------------------------------------------------------------------------
;;  (c) copyright 2022 by Gilbert Baumann

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

(defvar *global-env*
  (make-hash-table :test 'equal))


;;;; Abstract Data Type for C Types

;; Unless you explicitly ask for a type being TYPE-NAME-P, all these functions
;; are transparent to typedefs. And to qualifiers. Otherwise these functions
;; still work on s-expressions until I can decide to spend a few DEFCLASSes.

;; There are more:

;;             __float128††                     16         16      128-bit extended (IEEE-754)
;; Decimal-    _Decimal32                        4         4       32bit BID (IEEE-754R)
;; floating-   _Decimal64                        8         8       64bit BID (IEEE-754R)
;; point       _Decimal128                      16         16      128bit BID (IEEE-754R)
;; Packed      __m64††                           8         8       MMX and 3DNow!
;;             __m128††                         16         16      SSE and SSE-2
;;             __m256††                         32         32      AVX

;; Further we would need:

;; __noffi_size_t
;; __noffi_ssize_t
;; __noffi_ptrdiff_t
;; __noffi_intptr_t
;; __noffi_va_list_t
;; __noffi_intmax_t

;; We need to make enum types mimic as integer types. As for named types,
;; integer-type-... is transparent to those and only enum-type-p specifically
;; report them as enum types.

;; And: Is that really a good idea?

;; TODO: When enum types become integer types, the actual set of enumeration
;; constants become important. The bad news about this is: Here we derivate
;; from what the parser returns.

(defun identifierp (object)
  (and object (symbolp object) (not (keywordp object))))

(deftype identifier ()
  '(satisfies identifierp))

(defun arithmetic-type-p (type &optional env)
  ;; What about _Complex?
  (or (integer-type-p type env)
      (float-type-p type env)))

;;; Integer Types

(defun integer-type-p (type &optional env)
  (let ((type (bare-expanded-type type env)))
    (cond ((enum-type-p type)
           (integer-type-p :int env))
          (t
           (find type (abi-integer-types) :key 'car)))))

(defun integer-type-noffi-spelling (type &optional env)
  (let ((q (integer-type-p type env)))
    (unless q (error "Not an integer type: ~S" type))
    (first q)))

(defun integer-type-size (type &optional env)
  (let ((q (integer-type-p type env)))
    (unless q (error "Not an integer type: ~S" type))
    (second q)))

(defun integer-type-align (type &optional env)
  (let ((q (integer-type-p type env)))
    (unless q (error "Not an integer type: ~S" type))
    (third q)))

(defun integer-type-signed-p (type &optional env)
  (let ((q (integer-type-p type env)))
    (unless q (error "Not an integer type: ~S" type))
    (fourth q)))

(defun integer-type-rank (type &optional env)
  (let ((q (integer-type-p type env)))
    (unless q (error "Not an integer type: ~S" type))
    (fifth q)))

(defun integer-type-spelling (type &optional env)
  (let ((q (integer-type-p type env)))
    (unless q (error "Not an integer type: ~S" type))
    (sixth q)))

(defun integer-type-min-value (type &optional env)
  (if (integer-type-signed-p type env)
      (- (expt 2 (1- (integer-type-size type env))))
      0))

(defun integer-type-max-value (type &optional env)
  (if (integer-type-signed-p type env)
      (1- (expt 2 (1- (integer-type-size type env))))
      (1- (expt 2 (integer-type-size type env)))))

(defun bool-type-p (type &optional env)
  (eq :bool (car (integer-type-p type env))))

;;; Floating Point Types

(defun float-type-p (type &optional env)
  (let ((type (bare-expanded-type type env)))
    (find type (abi-floating-point-types) :key 'car)))

(defun float-type-noffi-spelling (type &optional env)
  (let ((q (float-type-p type env)))
    (unless q (error "Not a floating type: ~S" type))
    (first q)))

(defun float-type-size (type &optional env)
  (let ((q (float-type-p type env)))
    (unless q (error "Not a floating point type: ~S" type))
    (second q)))

(defun float-type-align (type &optional env)
  (let ((q (float-type-p type env)))
    (unless q (error "Not a floating point type: ~S" type))
    (third q)))

(defun float-type-lisp-type (type &optional env)
  (let ((q (float-type-p type env)))
    (unless q (error "Not a floating point type: ~S" type))
    (fourth q)))
  
(defun float-type-rank (type &optional env)
  (let ((q (float-type-p type env)))
    (unless q (error "Not a floating point type: ~S" type))
    (fifth q)))

(defun float-type-spelling (type &optional env)
  (let ((q (float-type-p type env)))
    (unless q (error "Not a floating point type: ~S" type))
    (sixth q)))

#+(or)
;; Not yet, but we'll need that eventually. E.g. the "0" literal will be
;; of this type.
(defun zero-type-p (type &optional env))

;;; Named Types

(defun named-type-p (type &optional env)
  (cond ((and (consp type) (eq (car type) :type-qualifier))
         (named-type-p (caddr type) env))
        ((and (consp type) (eq (car type) ':type-name))
         type)))

(defun make-named-type (name)
  `(:type-name ,name))

(defun named-type-name (type &optional env)
  (let ((q (named-type-p type env)))
    (cond ((not q)
           (error "Not a named type: ~S" type))
          (t
           (cadr q)))))

(defun named-type-expansion (type &optional env)
  (cond ((and (consp type) (eq (car type) :type-qualifier))
         `(,(car type) ,(cadr type) ,(named-type-expansion (caddr type) env)))
        ((and (consp type) (eq (car type) ':type-name))
         (let ((def (find-identifier-declaration (named-type-name type env) env)))
           (unless (typedef-declaration-p def)
             (error "~S doesn't name a type, but ~S" (named-type-name type env) def))
           (declaration-type def)))))

;;; Pointers

(defun pointer-type-p (type &optional env)
  (let ((type (bare-expanded-type type env)))
    (and (and (consp type) (eq (car type) :pointer))
         type)))

(defun make-pointer-type (base)
  `(:pointer ,base))

(defun pointer-type-base (type &optional env)
  (let ((it (pointer-type-p type env)))
    (unless it
      (error "~S is not a pointer type." type))
    (cadr it)))

(defun pointer-type-size (type &optional env)
  (let ((it (pointer-type-p type env)))
    (unless it
      (error "~S is not a pointer type." type))
    (nth-value 0 (abi-pointer-type-size-align type))))

(defun pointer-type-align (type &optional env)
  (let ((it (pointer-type-p type env)))
    (unless it
      (error "~S is not a pointer type." type)))
  (nth-value 1 (abi-pointer-type-size-align type)))

;;; Block Pointers

(defun block-pointer-type-p (type &optional env)
  (let ((type (bare-expanded-type type env)))
    (and (and (consp type) (eq (car type) :block-pointer))
         type)))

;;;

(defun array-type-p (type &optional env)
  (let ((type (bare-expanded-type type env)))
    (and (and (consp type) (eq (car type) :array))
         type)))

(defun array-type-base (type &optional env)
  (let ((it (array-type-p type env)))
    (unless it
      (error "~S is not an array type." type))
    (cadr it)))

(defun array-type-count (type &optional env)
  ;; ### We may change this to demand that the count is present!
  "Returns the number of elements in the array type _type_. May also
   return `:unspecified` when the number of elements was not given."
  (let ((it (array-type-p type env)))
    (unless it
      (error "~S is not an array type." type))
    (caddr it)))

(defun incomplete-array-type-p (type &optional env)
  (and (array-type-p type env)
       (eq :unspecified (array-type-count type))))

;;;

(defun make-function-type (result-type parameter-declarations)
  "Creates a new function datatype.
_result_type_ is the return type of the function. _parameters_ is a
list of declarations optionally ended in &REST for variadic functions.

Note: The idiosyncrasies of C for functions of zero arity versus
unspecified function parameters are _not_ exposed here. Use
_parameters_ = NIL for a function taking no parameter and _parameters_
= :UNSPECIFIED for a function or no specified parameters."
  (let ((parameters
         (cond ((eq parameter-declarations ':unspecified)
                nil)
               ((null parameter-declarations)
                (list (make-declaration :name nil :type ':void)))
               (t
                (do ((q parameter-declarations (cdr q)))
                    ((or (endp q)
                         (and (endp (cdr q)) (eq (car q) '&rest))))
                  (assert (declaration-p (car q)) nil
                          "~@<Does not look like a declaration: ~S~:@>" (car q)))
                parameter-declarations))))
    ;;
    `(:function ,result-type ,parameters)))

(defun function-type-p (type &optional env)
  (let ((type (bare-expanded-type type env)))
    (and (and (consp type) (eq (car type) :function))
         type)))

(defun function-type-result-type (type &optional env)
  (let ((it (function-type-p type env)))
    (unless it
      (error "~S is not a function type." type))
    (cadr it)))

(defun function-type-parameters (type &optional env)
  (let ((it (function-type-p type env)))
    (unless it
      (error "~S is not a function type." type))
    (caddr it)))

(defun function-type-parameters-empty-p (type &optional env)
  (let ((ps (function-type-parameters type env)))
    (and (= 1 (length ps))
         (declaration-p (car ps))
         (void-type-p (declaration-type (car ps)) env))))

(defun function-type-variadic-p (type &optional env)
  (member '&rest (function-type-parameters type env)))

(defun function-type-signature (ftype &optional env)
  "-> result-type; parameter-types; restp
Returns three values: The result type, a list of parameter types, and
a flag indicating whether the function is variadic."
  (let ((rtype (function-type-result-type ftype env)))
    (cond ((function-type-parameters-empty-p ftype env)
           (values rtype nil nil))
          ((null (function-type-parameters ftype env))
           (values rtype nil :unspecified))
          (t
           (loop for q on (function-type-parameters ftype env)
                 when (eq (car q) '&rest) do (return (values rtype parameter-types t))
                 collect (declaration-type (car q)) into parameter-types
                 finally (return (values rtype parameter-types nil)))))))

;;;

(defun record-type-p (type &optional env)
  (or (struct-type-p type env) (union-type-p type env)))

(defun struct-type-p (type &optional env)
  (let ((type (bare-expanded-type type env)))
    (and (and (consp type) (eq (car type) :struct))
         type)))

(defun union-type-p (type &optional env)
  (let ((type (bare-expanded-type type env)))
    (and (and (consp type) (eq (car type) :union))
         type)))

(defun record-type-name (type &optional env)
  (let ((q (record-type-p type env)))
    (unless q (error "Not a struct or union type: ~S" type))
    (cadr q)))

(defun record-type-kind (type &optional env)
  (let ((q (record-type-p type env)))
    (unless q (error "Not a struct or union type: ~S" type))
    (car q)))

(defun record-type-complete-p (type &optional env)
  (let ((q (record-type-p type env)))
    (unless q (error "Not a struct or union type: ~S" type))
    (not (null (cddr q)))))

(defun record-type-qualifiers (type &optional env)
  "Returns a list of type qualifiers that modify this very record type
itself, not necessarily the type as such."
  (let ((q (record-type-p type env)))
    (unless q (error "Not a struct or union type: ~S" type))
    (setq q (or (find-record-type q env) q))
    (mapcan (lambda (x)
              (and (typep x '(cons (member :type-qualifier)))
                   (list (cadr x))))
            (cadddr q))))

(defun record-type-members (type &optional env &key (errorp t) (resolve t))
  (let ((q (record-type-p type env)))
    (unless q (error "Not a struct or union type: ~S" type))
    (let ((def (if resolve (find-record-type q env) (bare-type type))))
      (when (and errorp (null (cddr def)))
        (error "Incomplete struct or union type: ~S" def))
      (let ((q (caddr def)))
        (do () ((or (null q) (not (keywordp (caar q))))) (pop q))
        (values q (not (null (cddr def)))))) ))

(defun find-record-type (type env)
  "Given a 'struct' or 'union' type _type_ find the record definition and
returns it as a complete record type, if there is any."
  (let ((q (record-type-p type env)))
    (unless q (error "Huh?"))
    ;; ### actually this is wrong, as there cannot be both a "union foo" and a "struct foo".
    (let ((it (gethash (list (record-type-kind type) (record-type-name q env))
                       *global-env*)))
      (or it q))))

(defun make-struct-type (name &rest args &key (members nil members-p) pack)
  "See MAKE-RECORD-TYPE"
  (declare (ignore members members-p pack))
  (apply #'make-record-type :struct name args))

(defun make-union-type (name &rest args &key (members nil members-p) pack)
  "See MAKE-RECORD-TYPE"
  (declare (ignore members members-p pack))
  (apply #'make-record-type :union name args))

(defun make-record-type (kind name &key (members nil members-p) pack)
  "Creates a new record data type of kind _kind_ which must be
either :STRUCT or :UNION. _name_ is the struct type tag name and must
either be NIL for an anonymous struct or a C identifier. The structure
members are given my _members_ as a list of declarations."
  (check-type kind (member :struct :union))
  (check-type name (or null identifier))
  (when (and members-p (null members))
    (error "Attempt to define an empty struct"))
  (assert (every (lambda (member)
                   (declaration-p member))
                 members))
  (list* kind name (if members-p (list members) nil)))

;; Hmm, this might be too simplistic.
;;(defun record-member-offset (member))   ;in bits
;; (defun record-member-size (member))     ;in bits

#+NOMORE
(defun record-type-member-layout (record-type member env &key (errorp t))
  (let ((layout (record-type-layout (find-record-type record-type env) env)))
    (labels ((aux (member layout delta)
               (cond ((null layout) nil)
                     (t
                      (destructuring-bind (name type size offset) (car layout) ;ADT
                        (cond ((eq name member)
                               (list name type size (+ offset delta)))
                              ((and (null name)
                                    (record-type-p type)
                                    (aux member (record-type-layout (find-record-type type env) env) (+ delta offset))))
                              (t
                               (aux member (cdr layout) delta))))))))
      (values-list
       (or (cdr (aux member layout 0))
           (and errorp (error "Record type ~S has no member ~S" record-type member)))))))

(defun record-type-member-offset (record-type member env &key (errorp t))
  "Offset of member /member/ within record type /record-type/ in bits.
   This also looks past anonymous record members."
  (nth-value 2 (record-type-member-layout record-type member env :errorp errorp)))

(defun record-type-member-size (record-type member env &key (errorp t))
  "Size of member /member/ within record type /record-type/ in bits.
   This also looks past anonymous record members."
  (nth-value 1 (record-type-member-layout record-type member env :errorp errorp)))

(defun record-type-member-type (record-type member env &key (errorp t))
  "Type of member /member/ within record type /record-type/. Bit
   fields are reported as such. This also looks past anonymous record
   members."
  (nth-value 0 (record-type-member-layout record-type member env :errorp errorp)))

(defun make-record-member (name type)
  (list 'decl nil (list name type)))

(defun record-member-name (member)
  (declaration-name member))

(defun record-member-type (member)
  (declaration-type member))

(defun record-member-pack (member)
  (dolist (s (declaration-specifiers member))
    (when (pack-declaration-specifier-p s)
      (return (pack-declaration-specifier-pack s)))))

(defun pack-declaration-specifier-p (specifier)
  (typep specifier `(cons (member :declaration-specifier)
                          (cons (cons (member :pack) (cons t null))
                                null))))

(defun pack-declaration-specifier-pack (specifier)
  (assert (pack-declaration-specifier-p specifier))
  (cadr (cadr specifier)))

;;; Enumeration Types

(defun enum-type-p (type &optional env)
  (let ((type (bare-expanded-type type env)))
    (and (and (consp type) (eq (car type) :enum))
         type)))

(defun enum-type-name (type &optional env)
  (cadr (or (enum-type-p type env)
            (error "Not an enumeration type: ~S" type))))

(defun enum-type-members (type &optional env &key (errorp t) (resolve t))
  "Returns a list of ENUM-TYPE-MEMBERs for the enumeration type _type_
and as second value a boolean indicating whether the members are
known. If _errorp_ is true (the default) an error is raised, when no
member definitions could be found. When _resolvep_ is true (the
default), the database is consulted for members, if given type does
not already mention them."
  (let ((q (enum-type-p type env)))
    (unless q (error "Not an enumeration type: ~S" type))
    (let ((def (if resolve (find-enum-type q env) (bare-type type))))
      (when (and errorp (null (cddr def)))
        (error "Incomplete enumeration type: ~S" def))
      (values (caddr def) (not (null (cddr def)))))))

(defun enum-member-name (enum-member)
  (etypecase enum-member
    (symbol enum-member)
    ((cons symbol t)
     (car enum-member))))

(defun enum-member-value-form (enum-member)
  (etypecase enum-member
    (symbol nil)
    ((cons symbol t) (cadr enum-member))))

(defun find-enum-type (type env)
  "Returns a complete version of the enumeration type _type_ if one
could be found, when in doubt the incomplete type is returned."
  (let ((type (bare-type (or (enum-type-p type env)
                             (error "Not an enumeration type - ~S" type)))))
    (if (cddr type)
        type
        (or (gethash (list :enum (enum-type-name type env))
                     *global-env*)
            type))))

;;;

(defun bit-field-type-p (type &optional env)
  (let ((type (bare-expanded-type type env)))
    (and (and (consp type) (eq (car type) :bit-field))
         type)))

(defun bit-field-type-width (type &optional env)
  (let ((it (bit-field-type-p type env)))
    (third (or it (error "Not a bit-field type: ~S" type)))))

(defun bit-field-type-base-type (type &optional env)
  (let ((it (bit-field-type-p type env)))
    (second (or it (error "Not a bit-field type: ~S" type)))))

;; (defun bit-field-type-signed-p (type &optional env))



#+(or)
(progn
  (defun align-of-type (type &optional env))
  (defun offset-of (type member &optional env))
  )

(defun size-of-type (type &optional env)
  (let ((bits-per-byte 8))              ;Hmm
    (values
     (ceiling
      (cond ((record-type-p type env)
             (nth-value 1 (record-type-layout type env)))
            ((integer-type-p type env)
             (integer-type-size type))
            ((float-type-p type env)
             (float-type-size type))
            ((pointer-type-p type env)
             (pointer-type-size type env))
            (t
             (error "Oops, cannot tell size of type ~S" type)))
      bits-per-byte))))



;;;;

(defun type-qualifiers (type &optional env)
  "Return all the type qualifiers that apply to _type_. Does follow type names as well."
  (cond ((and (consp type) (eq (car type) :type-qualifier))
         (cons (cadr type)
               (type-qualifiers (caddr type) env)))
        #+(or)
        ((named-type-p type)
         (type-qualifiers (named-type-expansion type) env))
        (t nil)))

(defun qualified-type-p (type &optional env)
  (declare (ignore env))
  (and (consp type) (eq (car type) :type-qualifier)))

(defun qualified-type-qualifier (type &optional env)
  (declare (ignore env))
  (unless (qualified-type-p type)
    (error "~S is not a qualified type." type))
  (cadr type))

(defun qualified-type-base (type &optional env)
  (declare (ignore env))
  (unless (qualified-type-p type)
    (error "~S is not a qualified type." type))
  (caddr type))

(defun make-qualified-type (qualifier base)
  `(:type-qualifier ,qualifier ,base))

(defun expand-type (type &optional env)
  (cond ((atom type) type)
        ((and (consp type) (eq (car type) :type-qualifier))
         `(:type-qualifier ,(cadr type)
                           ,(expand-type (caddr type) env)))
        ((named-type-p type)
         (expand-type (named-type-expansion type env) env))
        (t type)))

(defun bare-type (type)
  (cond ((and (consp type) (eq (car type) :type-qualifier))
         (bare-type (caddr type)))
        (t
         type)))

(defun bare-expanded-type (type &optional env)
  (let ((type (bare-type type)))
    (if (named-type-p type env)
        (bare-expanded-type (named-type-expansion type env) env)
        type)))

(defun find-identifier-declaration (identifier &optional env &key (errorp t))
  (multiple-value-bind (res win) (gethash identifier *global-env*)
    (when (and errorp (not win))
      (blame identifier "~@<Undeclared identifier ~S~@:>" identifier))
    (values res win)))


;;;; Declarations

;; This is only half the truth as one DECL form may carry more than one
;; defined identifier.

(defun make-declaration (&key name type
                              (init nil init-p)
                              specifiers
                              (storage-class nil storage-class-p))
  (when storage-class-p
    (push `(:storage-class ,storage-class) specifiers))
  `(decl ,specifiers (,name ,type ,@(and init-p (list init)))))

(defun declaration-p (object)
  (and (consp object) (eq (car object) 'decl)))

(defun typedef-declaration-p (decl)
  (and (declaration-p decl) (eq :typedef (declaration-storage-class decl))))

;; :ENUM-KEY is a pseudo storage class to enter enum keys with our database.

(defun enum-key-declaration-p (decl)
  (and (declaration-p decl) (eq :enum-key (declaration-storage-class decl))))

(defun declaration-name (decl)
  (assert (declaration-p decl))
  (car (caddr decl)))

(defun declaration-type (decl)
  (assert (declaration-p decl))
  (cadr (caddr decl)))

(defun declaration-init (decl)
  (assert (declaration-p decl))
  (caddr (caddr decl)))

(defun declaration-specifiers (decl)
  (assert (declaration-p decl))
  (cadr decl))

;;;

;; We might want to actually change this as each declaration has exactly
;; one storage class.

(defun declaration-storage-class (decl &optional (default :extern))
  (dolist (decl-spec (declaration-specifiers decl) default)
    (when (storage-class-decl-spec-p decl-spec)
      (return (storage-class-decl-spec-storage-class decl-spec)))))

(defun storage-class-decl-spec-p (decl-spec)
  (and (eq :storage-class (car decl-spec)) decl-spec))

(defun storage-class-decl-spec-storage-class (decl-spec)
  (cadr (or (storage-class-decl-spec-p decl-spec)
            (error "~@<Huh? Not a storage class decl-sepc - ~S~@:>" decl-spec))))

(defun extern-declaration-p (decl)
  (eql :extern (declaration-storage-class decl)))

(defun constant-static-declaration-p (decl &optional env)
  (and (eql :static (declaration-storage-class decl))
       (member :const (type-qualifiers (declaration-type decl) env))))


;;;; Record Layout

;;; Bit-fields

;; First, I cannot find any documentation about gcc's layout for AMD64. It
;; refers to the ABI, but doesn't followit, at least not like I read it.

;; Anyhow, we make the bold assumption that the exactly placement of a
;; bit-fields only depends on what precedes it in the list of members. We also
;; assume that a bit-field never is placed at lower bit-addresses than a
;; previous bit-field.

;; Here is what gcc appears to do: We have a current allocation pointer and a
;; current overall alignment for the struct. The bit-field itself has a
;; certain alignment requirement. This alignment requirement makes it to the
;; overall alignment requirement. Then room is searched for the given
;; bit-field. Say the bit-field base type is `w' bits, with alignment `a' and
;; the bit-field itself has `n' bits, then that chunk of (BYTE n (* k a)) is
;; used with the smallest k. The allocation pointer is bumped and we're done.

(defun record-type-layout (record-type &optional (env nil))
  "Returns a list of (<name> <type> <size> <pos>) size and position
are measured in bits. Second and third return value are the size and alignment in bits."
  (assert (record-type-p record-type))
  (let* ((members        (record-type-members record-type env :errorp t))
         (qualifiers     (record-type-qualifiers record-type))
         (kind           (record-type-kind record-type))
         ;;overall structure alignment in bits
         (max-align      8)
         (allo 0)        ;allocation pointer in bits
         (max-allo 0)    ;overall maximum allocation
                                        ; (may differ from `allo' in case of unions).
         (res nil)                   ;assembled list of member layouts
         )
    (dolist (member members)
      (multiple-value-bind (member-name member-type)
          (values (record-member-name member) (record-member-type member))
        (cond ((bit-field-type-p member-type)
               ;; See above for what we do.
               (let ((width (c-eval (bit-field-type-width member-type) env)) ;eval?
                     (base-type (bit-field-type-base-type member-type env)))
                 (multiple-value-bind (base-size base-align)
                     (type-size-align base-type env)
                   ;;
                   ;; Heh, what about bit-fields with unions? Is that even allowed?
                   ;;
                   ;; Try fitting it directly at `allo' first.
                   ;;
                   ;; I believe this still is not entirely correct. The question rather is:
                   ;; Would given the alignment constraints some memory access using the
                   ;; base-type work? Need more tests and more reading here.
                   (unless (and (not (zerop width))
                                (= (floor allo base-align)
                                   (floor (1- (+ allo width)) base-align)))
                     ;; Around here under pack(1) we see a difference to MSVC
                     ;; with IMAGE_IMPORT_CONTROL_TRANSFER_DYNAMIC_RELOCATION
                     ;; Doesn't fit. Align 'allo' first.
                     (setq allo (* base-align (ceiling allo base-align))))
                   ;; Place it at `allo'
                   (push (list member-name member-type width allo) res)
                   ;; Adjust overall align and size.
                   (when (record-member-pack member)
                     (setq base-align (min base-align (* 8 (record-member-pack member)))))
                   (setf max-align (max max-align base-align))
                   ;; (setq max-allo (max max-allo (+ allo base-size)))
                   (setq max-allo (max max-allo (+ allo width)))
                   (incf allo width))))
              (t
               (multiple-value-bind (member-size member-align)
                   ;; Unspecified array size in records.
                   ;;
                   ;; For structs all of gcc, clang and msvc assume zero.
                   ;; For unions gcc and clnag complain, while msvc assumes one.
                   ;;
                   (if (and (array-type-p member-type) (incomplete-array-type-p member-type))
                       (values (ecase kind
                                 (:struct 0)
                                 (:union (type-size-align (array-type-base member-type) env)))
                               (nth-value 1 (type-size-align (array-type-base member-type) env)))
                       (type-size-align member-type env))
                 ;; Update the overall alignment, if needed
                 (when (record-member-pack member)
                   (setq member-align (min member-align (* 8 (record-member-pack member)))))
                 (setq max-align (max max-align member-align))
                 ;; Bump the allocation pointer according to alignment.
                 (setq allo      (if (eq :union kind)
                                     0
                                     (* member-align (ceiling allo member-align))))
                 ;; Place it
                 (push (list member-name member-type member-size allo) res)
                 (incf allo member-size)
                 (setq max-allo (max max-allo allo)))))))
    ;;
    (setq max-align
          ;; Some __attribute__((aligned(n))) may override this
          (dolist (q qualifiers max-align)
            (when (align-qualifier-p q)
              (return (* 8 (align-qualifier-align q env))))))
    (values
     (reverse res)
     (* max-align (ceiling max-allo max-align))
     max-align)))

(defun align-qualifier-p (qualifier)
  (typep qualifier '(cons (member :align) (cons t null))))

(defun align-qualifier-align (qualifier env)
  (assert (align-qualifier-p qualifier))
  (c-eval (cadr qualifier) env))

(defun type-size (type &optional env)
  "Returns size of type `type' measured in bits."
  (nth-value 0 (type-size-align type env)))

(defun type-align (type &optional env)
  "Returns alignment of type `type' measured in bits."
  (nth-value 1 (type-size-align type env)))

(defun type-size-align (type env &aux it)
  "Returns size and alignment of type `type' measured in bits."
  (declare (ignorable it))
  (cond ((setq it (integer-type-p type env))
         (values (integer-type-size type)
                 (integer-type-align type)))
        ((setq it (float-type-p type env))
         (values (float-type-size type)
                 (float-type-align type)))
        ((or (setq it (pointer-type-p type env))
             (setq it (block-pointer-type-p type env)))
         (values (pointer-type-size type)
                 (pointer-type-align type)))
        ((setq it (record-type-p type))
         (multiple-value-bind (layout size align) (record-type-layout type env)
           (declare (ignore layout))
           (values size align)))
        ((array-type-p type env)
         ;; What about "int[]" ?
         (multiple-value-bind (element-size element-align)
             (type-size-align (array-type-base type env) env)
           (values (and (array-type-count type)
                        (*
                         ;; ###
                         (cval-value (c-coerce (c-eval (array-type-count type) nil)
                                               :unsigned-long-long))
                         element-size))
                   element-align)))
        (t
         (error "Cannot tell size and alignment of ~S" type))))

(defun void-type-p (type &optional env)
  (eql :void (bare-expanded-type type env)))



(defun types-compatible-p (type-1 type-2 &optional env)
  ;; This is very lenient.
  (or (equal type-1 type-2)             ;This shouldn't be needed, but we still choke on this va_list thing!
      (multiple-value-bind (type-1 type-2)
          (values (bare-expanded-type type-1)
                  (bare-expanded-type type-2))
        (cond ((equal type-1 type-2))
              ((integer-type-p type-1 env)
               (and (integer-type-p type-2 env)
                    (eql (integer-type-rank type-1 env)
                         (integer-type-rank type-2 env))
                    (eql (integer-type-signed-p type-1 env)
                         (integer-type-signed-p type-2 env))))
              ((float-type-p type-1 env)
               (eq type-1 type-2))
              ((pointer-type-p type-1 env)
               (and (pointer-type-p type-2 env)
                    (types-compatible-p (pointer-type-base type-1 env) (pointer-type-base type-2 env) env)))
              ;; Note, this is shadowed by the INTEGER-TYPE-P above yet. I am not sure
              ;; what to do with enums here.
              ((enum-type-p type-1 env)
               (and (enum-type-p type-2 env)
                    (or (and (enum-type-name type-1 env)
                             (eql (enum-type-name type-1 env) (enum-type-name type-2 env)))
                        nil)))          ;xxx
              ((function-type-p type-1 env)
               ;; calling convention
               (and (function-type-p type-2 env)
                    (types-compatible-p (function-type-result-type type-1 env)
                                        (function-type-result-type type-2 env))
                    ;; ### K&R
                    (do ((p1 (function-type-parameters type-1 env) (cdr p1))
                         (p2 (function-type-parameters type-2 env) (cdr p2)))
                        ((or (null p1) (null p2))
                         (and (null p1) (null p2)))
                      (unless (cond ((eq (car p1) '&rest)
                                     (eq (car p2) '&rest))
                                    ((declaration-p (car p1))
                                     (and (declaration-p (car p2))
                                          (types-compatible-p (declaration-type (car p1))
                                                              (declaration-type (car p2))
                                                              env)))
                                    (t
                                     (error "Huh? What kind of parameters are ~S and ~S?" (car p1) (car p2))))
                        (return nil)))))
              ((array-type-p type-1 env)
               (and (array-type-p type-2 env)
                    (types-compatible-p (array-type-base type-1 env) (array-type-base type-2 env) env)
                    ;; ###
                    (or (or (incomplete-array-type-p type-1 env)
                            (incomplete-array-type-p type-2 env))
                        ;; ### Doesn't work yet
                        '(equal (c-eval (array-type-count type-1 env) env)
                          (c-eval (array-type-count type-2 env) env)))))
              ((record-type-p type-1 env)
               (and (record-type-p type-2 env)
                    (or (and (struct-type-p type-1 env) (struct-type-p type-2 env))
                        (and (union-type-p type-1 env) (union-type-p type-2 env)))
                    (cond ((record-type-name type-1 env)
                           (and (record-type-name type-1 env)
                                (eq (record-type-name type-1 env)
                                    (record-type-name type-2 env))))
                          ((record-type-name type-2 env) nil)
                          (t
                           (let ((members-1 (record-type-members type-1 env))
                                 (members-2 (record-type-members type-2 env)))
                             (and (= (length members-1) (length members-2))
                                  (every (lambda (member-1 member-2)
                                           (and
                                            (eq (record-member-name member-1)
                                                (record-member-name member-2))
                                            (eql (record-member-pack member-1)
                                                 (record-member-pack member-2))
                                            (types-compatible-p (record-member-type member-1)
                                                                (record-member-type member-2))))
                                         members-1 members-2)))))))
              ((bit-field-type-p type-1 env)
               (and (bit-field-type-p type-2 env)
                    (and (types-compatible-p (bit-field-type-base-type type-1) (bit-field-type-base-type type-2))
                         (equal (bit-field-type-width type-1)
                                (bit-field-type-width type-2)))))
              ;; xxx
              ((eq :__builtin_va_list type-1)
               (eq :__builtin_va_list type-2))
              ((eq :__builtin_va_list type-2) nil)
              ((void-type-p type-1 env)
               (void-type-p type-2 env))
              ;; ### shouldn't happen
              ((and (consp type-1) (eq :function-specifier (car type-1)))
               t)
              ((and (consp type-2) (eq :function-specifier(car type-2)))
               t)
              (t
               (warn "~@<What kind of types are ~S and ~S?~@:>" type-1 type-2)
               nil)))))

                    
(defun record-type-member-layout (record-type member env &key (errorp t))
  (let ((layout (record-type-layout (find-record-type record-type env) env)))
    (labels ((aux (member layout delta)
               (cond ((null layout) nil)
                     (t
                      (destructuring-bind (name type size offset) (car layout) ;ADT
                        (cond ((eq name member)
                               (list name type size (+ offset delta)))
                              ((and (null name)
                                    (record-type-p type)
                                    (aux member (record-type-layout (find-record-type type env) env) (+ delta offset))))
                              (t
                               (aux member (cdr layout) delta))))))))
      (values-list
       (or (cdr (aux member layout 0))
           (and errorp (error "Record type ~S has no member ~S" record-type member)))))))

(defun c-> (object member)
  ;; ### bit fields!
  (setq object (promoted-cval object))
  (labels ((doit (record-type)
             (multiple-value-bind (type size offset)
                 (record-type-member-layout record-type member nil)
               (declare (ignore size))
               (%c-aref (c-coerce (c+ (cons-ptr (ptr-base-sap object) (ptr-offset object) (make-pointer-type :char))
                                      (/ offset 8))
                                  (make-pointer-type type))))))
    (cond ((and (cvalp object)
                (pointer-type-p (cval-type object))
                (record-type-p (pointer-type-base (cval-type object))))
           (doit (pointer-type-base (cval-type object))))
          ((and (cvalp object)
                (record-type-p (cval-type object)))
           (doit (cval-type object)))
          (t
           (error "~S is not a pointer to a record, nor some record reference." object)))))

(defun (setf c->) (nv object member)
  ;; ### bit fields!
  (setq object (promoted-cval object))
  (labels ((doit (record-type)
             (multiple-value-bind (type size offset)
                 (record-type-member-layout record-type member nil)
               (declare (ignore size))
               (setf
                (%c-aref (c-coerce (c+ (cons-ptr (ptr-base-sap object) (ptr-offset object) (make-pointer-type :char))
                                       (/ offset 8))
                                   (make-pointer-type type)))
                nv))))
    (cond ((and (cvalp object)
                (pointer-type-p (cval-type object))
                (record-type-p (pointer-type-base (cval-type object))))
           (doit (pointer-type-base (cval-type object))))
          ((and (cvalp object)
                (record-type-p (cval-type object)))
           (doit (cval-type object)))
          (t
           (error "~S is not a pointer to a record, nor some record reference." object)))))

(defun c->-addr (object member)
  ;; ### code duplication
  (setq object (promoted-cval object))
  (labels ((doit (record-type)
             (multiple-value-bind (type size offset)
                 (record-type-member-layout record-type member nil)
               (declare (ignore size))
               (c-coerce (c+ (cons-ptr (ptr-base-sap object) (ptr-offset object) (make-pointer-type :char))
                             (/ offset 8))
                         (make-pointer-type type)))))
    (cond ((and (cvalp object)
                (pointer-type-p (cval-type object))
                (record-type-p (pointer-type-base (cval-type object))))
           (doit (pointer-type-base (cval-type object))))
          ((and (cvalp object)
                (record-type-p (cval-type object)))
           (doit (cval-type object)))
          (t
           (error "~S is not a pointer to a record, nor some record reference." object)))))
