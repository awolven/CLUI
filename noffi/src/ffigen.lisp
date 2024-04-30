(in-package :noffi)

;;;; -- TODO ----------------------------------------------------------------------------------

;; - For bit-fields e.g. 'int' is unsigned!

;; - Can we carry SLOC around?

;; - Struct layout wrt to bit-fields

;; - Means to verify that we have the same as ffigen.

;; 

;;;; ------------------------------------------------------------------------------------------

;; Here is an idea.

;; Why don't we turn a foreign function definition into a DEFUN.

;; struct foo { int x,y; }; could get:

;; _foo.x macptr
;; _foo.y macptr
;; make/_foo &key x y
;; (with/_foo (p :x ... :y ...) ...)

;; caref p &optional index

;; (_XEvent.type ev)
;; (_x-open-display ":0")

;; Or:

;; (.type ev)

;; (.code (.xkey ev))

;; (_cairo_moveto cr x y)

;; (require :parse-ffi)
;; (ccl::parse-standard-ffi-files "new-xlib")
;; (ccl:%get-cstring



;;;; ------------------------------------------------------------------------------------------

#+(or)
(progn

  (deftype type-qualifier ()
    '(member :const :volatile :restrict :nonnull :nullable))

  #+(or)
  (deftype type-qualifier ()
    `(satisfies type-qualifier-p))

;;;; ------------------------------------------------------------------------------------------

  (defparameter *all* nil)

  (defun ffigen-run (&optional (force nil))
    (when (or (null *all*) force)
      (de.bauhh.cpp::cpp-reset)
      (setq *all* (j-parse "fodder/xlib2.j" :keep-macros-p t))
      (setq *all*
            (mapcan (lambda (x)
                      (if (eq (car x) 'declare)
                          (fold-declare x)
                          (list x)))
                    (cdr *all*))))
    (with-open-file (out "fodder/xlib.ffi"
                         :direction :output
                         :if-exists :supersede)
      (with-standard-io-syntax
        (let ((*package* (find-package :noffi))
              (*print-pretty* t)
              (*print-case* :downcase)
              (*print-pprint-dispatch* (copy-pprint-dispatch)))
          (set-pprint-dispatch 'null
                               #'(lambda (stream object &rest args)
                                   (declare (ignore args))
                                   (write-string "()" stream)
                                   object))
          (maphash (lambda (k v)
                     (cond ((eq (car v) :object)
                            (print `(macro ("" 0)
                                           ,(de.bauhh.cpp::pp-token-text k)
                                           ,(format nil "~{~A~}"
                                                    (mapcar #'de.bauhh.cpp::pp-token-text (cdr v))))
                                   out))))
                   de.bauhh.cpp::*defs*)
          (loop for x in (mapcan #'ffi-translate *all*) do
                (print x out))))
      (terpri out))
    nil)

  (defun ffigen-run (&key (force nil))
    (when (or (null *all*) force)
      (de.bauhh.cpp::cpp-reset)
      (setq *all* (j-parse "fodder/freebsd-gtk3.j" :keep-macros-p t))
      (setq *all*
            (mapcan (lambda (x)
                      (if (eq (car x) 'declare)
                          (fold-declare x)
                          (list x)))
                    (cdr *all*))))
    (with-open-file (out "fodder/freebsd-gtk3.ffi"
                         :direction :output
                         :if-exists :supersede)
      (with-standard-io-syntax
        (let ((*package* (find-package :noffi))
              (*print-pretty* t)
              (*print-case* :downcase)
              (*print-pprint-dispatch* (copy-pprint-dispatch)))
          (set-pprint-dispatch 'null
                               #'(lambda (stream object &rest args)
                                   (declare (ignore args))
                                   (write-string "()" stream)
                                   object))
          (maphash (lambda (k v)
                     (cond ((eq (car v) :object)
                            (print `(macro ("" 0)
                                           ,(de.bauhh.cpp::pp-token-text k)
                                           ,(format nil "~{~A~}"
                                                    (mapcar #'de.bauhh.cpp::pp-token-text (cdr v))))
                                   out))))
                   de.bauhh.cpp::*defs*)
          (loop for x in (mapcan #'ffi-translate *all*) do
                (print x out))))
      (terpri out))
    nil)

                       
;;;; -- FFI File Dumping ----------------------------------------------------------------------

  ;; Those .ffi files don't allow for anonymous or embedded structs. Each
  ;; struct must be refered to by (struct-ref <name>). Same with enums.

  ;; Functions are (FUNCTION <parameter-list> <result>)

  ;; It's a bit confusing: foo()          -> (function ((void ())) (int ()))
  ;;                       foo(void)      -> (function () (int ()))
  ;;                       foo(int,...)   -> (function ((int ()) (void ())) (int ()))
  ;; I happen to believe this is mixed up somehow.

  (defun ffi-translate (form)
    (let ((*ffi-aux* nil))
      (let ((res
             (case (car form)
               (typedef
                `(type ("" 0)
                       ,(symbol-name (cadr form))
                       ,(ffi-translate-type (caddr form))))
               (decl
                (destructuring-bind (name type) (cdr form)
                  (cond ((null name)
                         (ffi-translate-type type)
                         nil)
                        (t
                         `(,(if (function-type-p type nil)
                                'function
                                'var)
                            ("" 0)
                            ,(symbol-name name)
                            ,(ffi-translate-type type)
                            (extern)))))))))
        `(,@(reverse *ffi-aux*)
            ,@(and res (list res))))))

  (defparameter +ffi-type-map+
    '((:bool                (unsigned ()))
      (:char                (char ()))
      (:signed-char         (signed-char ()))
      (:unsigned-char       (unsigned-char ()))
      (:short               (short ()))
      (:unsigned-short      (unsigned-short ()))    
      (:int                 (int ()))
      (:unsigned-int        (unsigned ()))    
      (:long                (long ()))
      (:unsigned-long       (unsigned-long ()))    
      (:long-long           (long-long ()))
      (:unsigned-long-long  (unsigned-long-long ()))
      (:float               (float ()))
      (:double              (double ()))
      (:long-double         (long-double ()))
      (:void                (void ()))))

  (defvar *ffi-aux*)
  (defvar *used-types*)

  (defun ffi-translate-type (type &aux it)
    (cond ((setq it (assoc type +ffi-type-map+))
           (cadr it))
          ((keywordp type)
           (error "Huh? What is the FFI name of ~S?" type))
          ((symbolp type)
           (pushnew type *used-types*)
           `(typedef ,(symbol-name type)))
          ((etypecase (car type)
             (type-qualifier
              (ffi-translate-type (cadr type)))
             ((member :pointer :block-pointer)
              `(pointer ,(ffi-translate-type (cadr type))))
             ((member :array)
              (destructuring-bind (base &optional (size 0)) (cdr type)
                `(array ,(eval-constant size nil) ,(ffi-translate-type base))))
             ((member :enum)
              (destructuring-bind (name &optional (keys nil keys-p)) (cdr type)
                (declare (ignore keys))
                (when keys-p
                  (setq name (ffi-emit-enum type)))
                (assert (not (null name)))
                `(enum-ref ,(symbol-name name))))
             ((member :struct)
              (destructuring-bind (name &optional (fields nil fields-p)) (cdr type)
                (declare (ignore fields))
                (when fields-p
                  (setq name (ffi-emit-struct type)))
                (assert (not (null name)))
                `(struct-ref ,(symbol-name name))))
             ((member :union)
              (destructuring-bind (name &optional (fields nil fields-p)) (cdr type)
                (declare (ignore fields))
                (when fields-p
                  (setq name (ffi-emit-union type)))
                (assert (not (null name)))
                `(union-ref ,(symbol-name name))))
             ((member :function)
              (destructuring-bind (res params) (cdr type)
                `(function
                  ,(cond ((eq params :unspecified)
                          '((void ())))
                         ((equal params '(&rest))
                          '())
                         (t
                          (mapcar (lambda (p)
                                    (cond ((eq p '&rest) '(void ()))
                                          (t
                                           (destructuring-bind (name type) p
                                             (declare (ignore name))
                                             (setq type (strip-type-qualifiers type))
                                             (cond ((typep type '(cons (member :array) t))
                                                    (setq type `(:pointer ,(cadr type)))))
                                             (ffi-translate-type type)))))
                                  params)))
                  ,(ffi-translate-type res)
                  ))) ))))

  (defun strip-type-qualifiers (type)
    (do ()
        ((not (and (consp type) (typep (car type) 'type-qualifier)))
         type)
      (setq type (cadr type))))

  (defun ffi-emit-enum (enum-type)
    (destructuring-bind (name &optional keys) (cdr enum-type)
      (setq name (or name (gensym "")))
      (let* ((next-id 0)
             (key-defs
              (mapcar (lambda (key)
                        (etypecase key
                          (symbol
                           `(,(symbol-name key) ,(1- (incf next-id))))
                          ((cons symbol (cons t null))
                           (setq next-id (eval-constant (cadr key) nil))
                           `(,(symbol-name (car key)) ,(1- (incf next-id))))))
                      keys)))
        (loop for (k v) in key-defs do
              (ffi-emit `(enum-ident ("" 0) ,k ,v)))
        (ffi-emit `(enum ("" 0) ,(symbol-name name) ,key-defs))
        name)))

  (defun ffi-emit-struct (struct-type &optional env)
    (ffi-emit-struct-or-union struct-type env))

  (defun ffi-emit-union (union-type &optional env)
    (ffi-emit-struct-or-union union-type env))

  (defun ffi-emit-struct-or-union (struct-or-union-type &optional env)
    (destructuring-bind (name &optional fields) (cdr struct-or-union-type)
      (setq name (or name (gensym "")))
      (let ((layout (compound-type-layout struct-or-union-type env)))
        (ffi-emit
         `(,(ecase (car struct-or-union-type)
                   (:union 'union)
                   (:struct 'struct))
            ("" 0)
            ,(symbol-name name)
            ,(loop for (field type size pos) in layout collect
                   (if (bit-field-type-p type env)
                       `(,(if field (symbol-name field) "")
                          (bitfield ,(ffi-translate-type (cadr type))
                                    ,pos ,size))
                       (progn
                         (assert (not (and (consp type) (eq (car type) ':bit-field))))
                         `(,(if field (symbol-name field) "")
                            (field ,(ffi-translate-type type)
                                   ,(/ pos 8) ,(/ size 8)))))))))
      name))

  (defun ffi-emit (form)
    (push form *ffi-aux*))

  (defun eval-constant (form env)
    (typecase form
      (real form)
      (character (char-code form))
      ((cons (member <<) (cons t (cons t null)))
       (ash (eval-constant (cadr form) env) (eval-constant (caddr form) env)))
      ((cons (member >>) (cons t (cons t null)))
       (ash (eval-constant (cadr form) env) (- (eval-constant (caddr form) env))))
      ((cons (member +) (cons t (cons t null)))
       (+ (eval-constant (cadr form) env) (eval-constant (caddr form) env)))
      ((cons (member logior) (cons t (cons t null)))
       (logior (eval-constant (cadr form) env) (eval-constant (caddr form) env)))
      ((cons (member logand) (cons t (cons t null)))
       (logand (eval-constant (cadr form) env) (eval-constant (caddr form) env)))
      ((cons (member logxor) (cons t (cons t null)))
       (logxor (eval-constant (cadr form) env) (eval-constant (caddr form) env)))
      ((cons (member -) (cons t (cons t null)))
       (- (eval-constant (cadr form) env) (eval-constant (caddr form) env)))
      ((cons (member -) (cons t null))
       (- (eval-constant (cadr form) env)))
      ((cons (member lognot) (cons t null))
       (lognot (eval-constant (cadr form) env)))
      ((cons (member *) (cons t (cons t null)))
       (* (eval-constant (cadr form) env) (eval-constant (caddr form) env)))
      ((cons (member %) (cons t (cons t null)))
       (rem (eval-constant (cadr form) env) (eval-constant (caddr form) env)))
      ((cons (member =) (cons t (cons t null)))
       (if (= (eval-constant (cadr form) env) (eval-constant (caddr form) env)) 1 0))
      ((cons (member /) (cons t (cons t null)))
       (let ((x (eval-constant (cadr form) env))
             (y (eval-constant (caddr form) env)))
         (cond ((or (floatp x) (floatp y))
                (coerce (/ x y) 'float))
               (t
                (floor x y)))))
      ((cons (member sizeof-type) (cons t null))
       (/ (type-size-align (cadr form) env) 8))
      ((cons (member the) (cons t (cons t null)))
       ;; Hmm
       (eval-constant (caddr form) env))
      ((cons (member if) (cons t (cons t (cons t null))))
       (if (zerop (eval-constant (cadr form) env))
           (eval-constant (cadddr form) env)
           (eval-constant (caddr form) env)))
      (symbol
       (eval-enum-key form env))
      (t
       (error "Don't know how to evaluate ~S" form))))

  (defun bit-field-type-p (type env)
    (typecase type
      (keyword nil)
      ;; Really? You cannot define bit-fields typedefs!
      (symbol (bit-field-type-p (resolve-type type env) env))
      ((cons (member :bit-field) t) t)
      ((cons type-qualifier (cons t null))
       (bit-field-type-p (cadr type) env))
      (t nil)))

  (defun function-type-p (type &optional (env nil))
    (typecase type
      (keyword nil)
      (symbol (function-type-p (resolve-type type env) env))
      ((cons (member :function) t)
       type)
      ((cons type-qualifier (cons t null))
       (function-type-p (cadr type) env))
      (t nil)))

  (defun record-type-p (type &optional (env nil))
    (typecase type
      (keyword nil)
      (symbol (record-type-p (resolve-type type env) env))
      ((cons (member :struct :union) t) type)
      ((cons type-qualifier (cons t null)) (record-type-p (cadr type) env))
      (t nil)))

  (defun resolve-type (type env)
    (cond ((and (symbolp type) (not (keywordp type)))
           (resolve-type (find-typedef type env) env)) ;### recursive typedefs?
          (t
           type)))

  (defun find-typedef (name env &optional (errorp t))
    (dolist (k *all*
             (when errorp
               (error "Undefined typedef ~S" name)))
      (when (and (eq (car k) 'typedef)
                 (eq (cadr k) name))
        (return (caddr k)))))

  (defun find-struct-or-union (needle env &optional (errorp t))
    (labels ((aux (type)
               (cond ((and (consp type)
                           (eq (car type) (car needle))
                           (eq (cadr type) (cadr needle))
                           (cddr type))
                      (return-from find-struct-or-union type))
                     ((atom type))
                     ((typep (car type) 'type-qualifier)
                      (aux (cadr type)))
                     ((member (car type) '(:pointer :array :block-pointer))
                      (aux (cadr type)))
                     ((member (car type) '(:struct :union))
                      (mapc #'(lambda (x) (aux (cadr x))) (caddr type))))))
      (dolist (k *all*
               (when errorp
                 (error "Undefined ~(~A~): ~S" (car needle) needle)))
        (when (eq (car k) 'typedef) (aux (caddr k)))
        (when (eq (car k) 'decl) (aux (caddr k))))))

  (defun struct-type-layout (struct-type env)
    "Returns a list of (<name> <type> <size> <pos>) size and position
are measured in bits."
    (compound-type-layout struct-type env))

  (defun union-type-layout (struct-type env)
    "Returns a list of (<name> <type> <size> <pos>) size and position
are measured in bits."
    (compound-type-layout struct-type env))

  (defun compound-type-layout (struct-or-union-type env)
    "Returns a list of (<name> <type> <size> <pos>) size and position
are measured in bits."
    (unless (cddr struct-or-union-type)
      (setq struct-or-union-type (find-struct-or-union struct-or-union-type env)))
    (assert (member (car struct-or-union-type) '(:struct :union)))
    (destructuring-bind (kind name fields) struct-or-union-type
      (declare (ignore name))
      (let ((max-align 8)
            (max-allo 0)
            (allo 0))
        (values
         (loop for (name type) in fields collect
               (multiple-value-bind (size align)
                   (type-size-align type env)
                 ;; ### bit-fields
                 (setq max-align (max max-align align))
                 (setq allo (if (eq :union kind)
                                0
                                (* align (ceiling allo align))))
                 (prog1
                     (list name type size allo)
                   (incf allo size)
                   (setq max-allo (max max-allo allo)))))
         (* max-align (ceiling max-allo max-align))
         max-align))))

  (defun type-size-align (type env)
    "Returns size and alignment of type `type' measured in bits."
    (etypecase type
      ((member :bool)                                     (values 8 8))
      ((member :char :signed-char :unsigned-char)         (values 8 8))
      ((member :short :unsigned-short)                    (values 16 16))
      ((member :int :unsigned-int)                        (values 32 32))
      ((member :long :unsigned-long)                      (values 64 64))
      ((member :long-long :unsigned-long-long)            (values 64 64))
      ((member :int128 :unsiged-int128)                   (values 128 128))
      ((member :float)                                    (values 32 32))
      ((member :double)                                   (values 64 64))
      ((member :long-double)                              (values 128 128))
      ((member :float128)                                 (values 128 128))
      ;; Hmm: _Decimal{32,64,128}, __m64, __m128, __m256?
      ;;
      ;; For enums the ABI says: "C++ and some implementations of C permit
      ;; enums larger than an int. The underlying type is bumped to an
      ;; unsigned int, long int or unsigned long int, in that order."
      ((cons (member :enum) t)                            (type-size-align :int env))
      ((cons (member :pointer) t)                         (values 64 64))
      ((cons (member :block-pointer) t)                   (values 64 64))
      ((cons type-qualifier)                              (type-size-align (cadr type) env))
      ((cons (member :array) (cons t (cons t null)))
       (multiple-value-bind (element-size element-align)
           (type-size-align (cadr type) env)
         (values (* element-size (eval-constant (caddr type) env))
                 element-align)))
      ((cons (member :struct) t)
       (multiple-value-bind (layout size align) (struct-type-layout type env)
         (declare (ignore layout))
         (values size align)))
      ((cons (member :union) t)
       (multiple-value-bind (layout size align) (union-type-layout type env)
         (declare (ignore layout))
         (values size align)))
      ((cons (member :bit-field) (cons t (cons t null)))
       ;; ###
       (values (eval-constant (caddr type) env) 1))
      (symbol
       (type-size-align (find-typedef type env) env))
      ))

;;; Bit-Fields

  ;; Here is what the ABI says:

  ;; | C struct and union definitions may include bit-fields that define integral
  ;; | values of a specified size.
  ;; |
  ;; | Bit-fields that are neither signed nor unsigned always have non-negative
  ;; | values. Although they may have type char, short, int, or long (which can
  ;; | have negative values), these bit-fields have the same range as a bit-field
  ;; | of the same size with the corresponding unsigned type. Bit-fields obey the
  ;; | same size and alignment rules as other structure and union members.
  ;; |
  ;; | Also:
  ;; | • bit-fields are allocated from right to left
  ;; |
  ;; | • bit-fields must be contained in a storage unit appropriate for its declared
  ;; |   type
  ;;
  ;; Note: This does not talk about the alignment of that storage unit.
  ;;
  ;; |
  ;; | • bit-fields may share a storage unit with other struct / union members
  ;; |   Unnamed bit-fields’ types do not affect the alignment of a structure or
  ;; |   union.

;;;; ------------------------------------------------------------------------------------------

  (defun eval-enum-key (key env &optional (errorp t))
    (labels ((aux (type)
               (typecase type
                 ((cons (member :enum) (cons t (cons t null)))
                  (when (member-if (lambda (x)
                                     (or (eq x key)
                                         (and (consp x) (eq (car x) key))))
                                   (caddr type))
                    (let ((next 0))
                      (dolist (def (caddr type))
                        (multiple-value-bind (name init)
                            (etypecase def
                              (symbol (values def nil))
                              ((cons symbol (cons t null))
                               (values (car def) (cadr def))))
                          (when init
                            (setq next (eval-constant init env)))
                          (when (eq name key)
                            (return-from eval-enum-key next))
                          (incf next))))))
                 ((cons type-qualifier)
                  (aux (cadr type)))
                 ((cons (member :pointer :array :block-pointer))
                  (aux (cadr type)))
                 ((member (car type) '(:struct :union))
                  (mapc #'(lambda (x) (aux (cadr x))) (caddr type))))))
      (dolist (k *all*
               (when errorp
                 (error "Undefined enum key: ~S" key)))
        (when (eq (car k) 'typedef) (aux (caddr k)))
        (when (eq (car k) 'decl) (aux (caddr k))))))

;;;; -- Verification --------------------------------------------------------------------------

  ;; We read an FFI file and convert it to something hopefully canonic. First we
  ;; remove all SLOC. Then we don't dump anonymous structs, unions, or enums but
  ;; rather insert them right away.

  ;; 'gcc -fffigen' is curious as it seems to follow typedefs in function
  ;; arguments. I wonder why. Also arguments of array type are converted to
  ;; pointer types.

  (defun canonize-ffi-file (input-pathname output-pathname)
    (with-standard-io-syntax
      (let ((*package* (find-package :noffi))
            (*print-pretty* t)
            (*print-case* :downcase)
            top)
        (let ((all (with-open-file (input input-pathname)
                     (loop for x = (read input nil input) until (eq x input) collect x))))
          ;; First of all kill sloc.
          (setq all (mapcar (lambda (x) (cons (car x) (cddr x))) all))
          ;; Insert anonymous struct-ref, union-ref, and enum-ref's
          (labels ((aux (form)
                     (cond ((atom form) form)
                           ((and (consp form)
                                 (eq (car form) 'struct-ref)
                                 (digit-char-p (char (cadr form) 0)))
                            (aux (find-def 'struct (cadr form))))
                           ((and (consp form)
                                 (eq (car form) 'union-ref)
                                 (digit-char-p (char (cadr form) 0)))
                            (aux (find-def 'union (cadr form))))
                           ((and (consp form)
                                 (eq (car form) 'enum-ref)
                                 (digit-char-p (char (cadr form) 0)))
                            (aux (find-def 'enum (cadr form))))
                           ;; Make anonymous structs into anonymous.
                           ((and (member (car form) '(union struct enum))
                                 (stringp (cadr form))
                                 (digit-char-p (char (cadr form) 0)))
                            (aux `(,(car form) nil ,@(cddr form))))
                           ;;
                           ((and (not (eq form top)) (eq 'function (car form)))
                            (destructuring-bind (params res) (cdr form)
                              `(function
                                ,@(mapcar (lambda (param)
                                            (cond ((and (typep param '(cons (member pointer)
                                                                       (cons (cons (member typedef) t)
                                                                        t)))
                                                        (let ((it (find-def 'type (cadr (cadr param)))))
                                                          (print it)
                                                          nil))
                                                   (print param)))
                                            param)
                                          params)
                                ,res
                                )))
                           (t
                            (mapcar #'aux form))))
                   (find-def (kind name)
                     (or (find-def-1 kind name all)
                         (error "Unknown ~S ~S~%~S" kind name top)))
                   (find-def-1 (kind name form)
                     (cond ((atom form) nil)
                           ((and (eq (car form) kind)
                                 (equal (cadr form) name))
                            form)
                           (t (some #'(lambda (x) (find-def-1 kind name x)) form)))))
            (setq all (mapcar (lambda (x) (setq top x) (aux x)) all))
            (setq all (remove-if (lambda (x)
                                   (or (eq (car x) 'macro)
                                       (eq (car x) 'undef-macro)
                                       (and (member (car x) '(struct union enum))
                                            (null (cadr x)))))
                                 all))
            (with-open-file (output output-pathname :direction :output :if-exists :supersede)
              (dolist (x all)
                (prin1 x output)
                (terpri output))))))))

  (defun do-compare ()
    (ffigen-run t)
    ;;(canonize-ffi-file "~/ccl/ccl-1.12/darwin-x86-headers64/xlib/C/opt/X11/include/X11/Xlib.ffi" "/tmp/soll.ffi")
    (canonize-ffi-file "~/src2/ffigen4/opt/X11/include/X11/Xlib.ffi" "/tmp/soll.ffi")
    (canonize-ffi-file "fodder/xlib.ffi" "/tmp/ist.ffi")) 

;;;; ------------------------------------------------------------------------------------------

  (defun do-stat ()
    (let ((hash (make-hash-table)))
      (dolist (def *all*)
        (when (and (eq (car def) 'decl)
                   (function-type-p (third def)))
          (let ((nargs (length (function-type-parameters (third def)))))
            (when (member '&rest (function-type-parameters (third def)))
              (print def))
            '(when (> nargs 10) (print def))
            (incf (gethash nargs hash 0)))))
      (sort (hash-table-alist hash) #'< :key 'car)))

  (defun name-stat ()
    (let ((res (make-hash-table :test 'eq))
          (pack (find-package :noffi-c)))
      (labels ((aux (form)
                 (cond ((and (symbolp form) (eq (symbol-package form) pack))
                        (setf (gethash form res) t))
                       ((atom form))
                       (t (mapcar #'aux form)))))
        (aux *all*)
        (setq res (mapcar 'car (hash-table-alist res)))
        (let ((hash (make-hash-table :test 'equal)))
          (dolist (sym res)
            (push sym (gethash (convert-name (symbol-name sym)) hash)))
          (remove-if (lambda (kv)
                       (= 1 (length (cdr kv))))
                     (hash-table-alist hash))))))

  (defstruct (decl (:type list) :named)
    name type)

  (defstruct (typedef (:type list) :named)
    name type)

  (defun name-stat ()
    ;; We want to differiate in name spaces: values, functions, types, and slots.
    (let (types vars funs slots)
      (labels ((walk-type (type)
                 (etypecase type
                   (atom)
                   ((cons type-qualifier (cons t null))
                    (walk-type (cadr type)))
                   ((cons (member :struct :union) t)
                    (and (cadr type) (pushnew (cadr type) types))
                    (dolist (slot (caddr type))
                      (when (car slot) (pushnew (car slot) slots))
                      (walk-type (cadr slot))))
                   ((cons (member :pointer :array :bit-field :block-pointer) t)
                    (walk-type (cadr type)))
                   ((cons (member :enum) t)
                    (and (cadr type) (pushnew (cadr type) types))
                    (dolist (key (caddr type))
                      (push (if (consp key) (car key) key) vars)))
                   ((cons (member :function) t)
                    (walk-type (cadr type))
                    (when (consp (function-type-parameters type))
                      (dolist (p (function-type-parameters type))
                        (when (consp p) (walk-type (cadr p)))))))))
        ;;
        (dolist (def *all*)
          (when (typedef-p def)
            (walk-type (typedef-type def))
            (push (typedef-name def) types))
          (when (decl-p def)
            (walk-type (decl-type def))
            (when (decl-name def)
              (if (function-type-p (decl-type def))
                  (push (decl-name def) funs)
                  (push (decl-name def) vars)))))
        (values (name-conflicts (remove-duplicates types))
                (name-conflicts (remove-duplicates slots))
                (name-conflicts (remove-duplicates vars))
                (name-conflicts (remove-duplicates funs))))))

  (defun name-conflicts (symbols)
    (let ((hash (make-hash-table :test 'equal)))
      (dolist (sym symbols)
        (push sym (gethash (convert-name (symbol-name sym)) hash)))
      (remove-if (lambda (kv)
                   (= 1 (length (cdr kv))))
                 (hash-table-alist hash))))

  (defun convert-name (name &optional ear-muffs)
    (setq name (strip-t-suffix name))
    (cond
      ((every (lambda (c) (or (upper-case-p c) (eql #\_ c))) name)
       (cond ((eql #\_ (char name 0))
              (concatenate 'string "+%" (substitute #\- #\_ (subseq name 1)) "+"))
             (t
              (concatenate 'string "+" (substitute #\- #\_ name) "+"))))
      ((every (lambda (c) (or (lower-case-p c) (eql #\_ c))) name)
       (concatenate 'string
                    ear-muffs
                    (cond ((eql #\_ (char name 0))
                           (concatenate 'string "%" (substitute #\- #\_ (string-upcase (subseq name 1)))))
                          (t
                           (substitute #\- #\_ (string-upcase name))))
                    ear-muffs))
      (t
       (concatenate 'string
                    ear-muffs
                    (with-output-to-string (bag)
                      (loop for c across name
                            for first = t then nil do
                            (when (and (not first) (upper-case-p c))
                              (write-char #\- bag))
                            (cond ((eql #\_ c)
                                   (if first
                                       (write-char #\% bag)
                                       (write-char #\- bag)))
                                  (t
                                   (write-char (char-upcase c) bag)))))
                    ear-muffs))))

;;; Truely anonymous structs

  (defun struct-stat ()
    ;; We want to differiate in name spaces: values, functions, types, and slots.
    (let (types vars funs slots top)
      (labels ((walk-type (type &optional name)
                 (etypecase type
                   (atom)
                   ((cons type-qualifier (cons t null))
                    (walk-type (cadr type) name))
                   ((cons (member :struct :union) t)
                    (and (cadr type) (pushnew (cadr type) types))
                    (let* ((name (and name (symbol-name name)))
                           (name (strip-t-suffix name))
                           ;;
                           (sname (and (cadr type)
                                       (symbol-name (cadr type))))
                           (sname (strip-t-suffix sname))
                           )
                      (when (and (and name sname)
                                 (not (or (string-equal name sname)
                                          (string-equal (format nil "_~A" name)
                                                        sname))))
                        '(print (list (format nil "_~A" name)
                                 sname))
                        (print (list name sname))))
                    ;;
                    (unless (or name (cadr type))
                      '(print (list top type)))
                    (dolist (slot (caddr type))
                      (when (car slot) (pushnew (car slot) slots))
                      (walk-type (cadr slot) )))
                   ((cons (member :pointer) t)
                    (walk-type (cadr type) name))
                   ((cons (member :block-pointer :array :bit-field) t)
                    (walk-type (cadr type)))
                   ((cons (member :enum) t)
                    (and (cadr type) (pushnew (cadr type) types))
                    (dolist (key (caddr type))
                      (push (if (consp key) (car key) key) vars)))
                   ((cons (member :function) t)
                    (walk-type (cadr type))
                    (when (consp (function-type-parameters type))
                      (dolist (p (function-type-parameters type))
                        (when (consp p) (walk-type (cadr p))))))))
             
               )
        ;;
        (dolist (def *all*)
          (when (typedef-p def)
            (setq top def)
            ;; (print `((typedef-name def) = ,(typedef-name def)))
            (walk-type (typedef-type def) (typedef-name def))
            (push (typedef-name def) types))
          (when (decl-p def)
            (setq top def)
            (walk-type (decl-type def))
            (when (decl-name def)
              (if (function-type-p (decl-type def))
                  (push (decl-name def) funs)
                  (push (decl-name def) vars)))))
        (values))))

  (defun struct-stat-2 ()
    ;; We want to differiate in name spaces: values, functions, types, and slots.
    (let (types vars funs slots top
                (name-map (make-hash-table)))
      (labels ((walk-type (type &optional name)
                 (etypecase type
                   (atom)
                   ((cons type-qualifier (cons t null))
                    (walk-type (cadr type) name))
                   ((cons (member :struct :union) t)
                    (and (cadr type) (pushnew (cadr type) types))
                    (when (and (cadr type) name)
                      (pushnew name (gethash (cadr type) name-map)))
                    '(let* ((name (and name (symbol-name name)))
                            (name (strip-t-suffix name))
                            ;;
                            (sname (and (cadr type)
                                        (symbol-name (cadr type))))
                            (sname (strip-t-suffix sname))
                            )
                      (when (and (and name sname)
                                 (not (or (string-equal name sname)
                                          (string-equal (format nil "_~A" name)
                                                        sname))))
                        '(print (list (format nil "_~A" name)
                                 sname))
                        (print (list name sname))))
                    ;;
                    (unless (or name (cadr type))
                      '(print (list top type)))
                    (dolist (slot (caddr type))
                      (when (car slot) (pushnew (car slot) slots))
                      (walk-type (cadr slot) )))
                   ((cons (member :pointer) t)
                    (walk-type (cadr type) name))
                   ((cons (member :block-pointer :array :bit-field) t)
                    (walk-type (cadr type)))
                   ((cons (member :enum) t)
                    (and (cadr type) (pushnew (cadr type) types))
                    (dolist (key (caddr type))
                      (push (if (consp key) (car key) key) vars)))
                   ((cons (member :function) t)
                    (walk-type (cadr type))
                    (when (consp (function-type-parameters type))
                      (dolist (p (function-type-parameters type))
                        (when (consp p) (walk-type (cadr p))))))))
             
               )
        ;;
        (dolist (def *all*)
          (when (typedef-p def)
            (setq top def)
            ;; (print `((typedef-name def) = ,(typedef-name def)))
            (walk-type (typedef-type def) (typedef-name def))
            (push (typedef-name def) types))
          (when (decl-p def)
            (setq top def)
            (walk-type (decl-type def))
            (when (decl-name def)
              (if (function-type-p (decl-type def))
                  (push (decl-name def) funs)
                  (push (decl-name def) vars)))))
        (remove 2 (hash-table-alist name-map) :key 'length))))

  (defun strip-t-suffix (s)
    (and s (if (eql (- (length s) 2) (search "_t" s :from-end t))
               (subseq s 0 (- (length s) 2))
               s)))

;;;; ------------------------------------------------------------------------------------------

  ;; Anonymous struct only hide within other structs. The only exception is the
  ;; one union with #__GValue.

  ;; Another thing we need to consider is anonymous structs within a typedef and
  ;; named structs within typedefs. For the latter, the question araises which
  ;; name we would pick. There are certain conventions though.

  ;; The question rather is: If there is a struct that is named by two different
  ;; typedefs.

;;;; ------------------------------------------------------------------------------------------

  (defun inhale (matter &key cflags)
    (setq matter
          (with-output-to-string (bag)
            (loop for line in (if (listp matter) matter (list matter)) do
                  (write-line line bag))))
    (setq cflags
          (etypecase cflags
            (string (split-by #\space cflags :nuke-empty-p t))
            (list
             (expand-cflags cflags))))
    (de.bauhh.file:with-temponary-file (tmp)
      (with-open-file (out tmp :direction :output :if-exists :supersede
                           :sharing :lock)
        (let ((p (ccl:run-program "cc" (append cflags (list "-E" "-dD" "-"))
                                  :input (make-string-input-stream matter)
                                  :output out
                                  :error *standard-output*)))
          (assert (equal '(:exited 0) (multiple-value-list (ccl:external-process-status p))))))
      ;;
      (de.bauhh.cpp::cpp-reset)
      (setq *all* (j-parse tmp :keep-macros-p t))
      (setq *all*
            (mapcan (lambda (x)
                      (if (eq (car x) 'declare)
                          (fold-declare x)
                          (list x)))
                    (cdr *all*)))))

  (defun expand-cflags (cflags)
    (loop for x in cflags append
          (etypecase x
            (string (list x))
            ((cons (eql :pkg-config) t)
             (let* ((out (make-string-output-stream))
                    (proc (ccl:run-program "pkg-config" (cdr x)
                                           :input nil
                                           :output out
                                           :error *standard-output*)))
               (assert (equal '(:exited 0) (multiple-value-list (ccl:external-process-status proc))))
               (split-by
                #\space (substitute #\space #\newline (get-output-stream-string out))
                :nuke-empty-p t))))))

  (defun foo ()
    (inhale "#include <X11/Xlib.h>"
            :cflags '("-I" "/opt/X11/include"))
    (with-open-file (out "/tmp/a.sexpr" :direction :output :if-exists :supersede)
      (dolist (x *all*)
        (prin1 x out) (terpri out))))

  #+NIL
  (defun foo ()
    (inhale '("#include <gtk/gtk.h>"
              "#include <gtk/gtkx.h>")
            :cflags '((:pkg-config "--cflags" "gtk+-3.0")))
    (with-open-file (out "/tmp/a.sexpr" :direction :output :if-exists :supersede)
      (dolist (x *all*)
        (prin1 x out) (terpri out)))
    )

;;;; ------------------------------------------------------------------------------------------

  (defparameter *record-type-hash*
    (make-hash-table))

  (defparameter *enum-type-hash*
    (make-hash-table))

  (defparameter *aux-definitions*
    nil)

  ;; ok.

  ;; We would need to map our struct and union records to CLOS classes and these
  ;; need to be named. However, it may happen that one struct is named by
  ;; multiple typedefs. So going from a struct name to a typedef name may not be
  ;; unique, while the reverse is.

  ;; Or put otherwise: When a signature says "(blah_t *x)" you would expect a
  ;; "blah_t", even if that structure is defined as a typedef "blup_t" someplace
  ;; else.

  ;; Types not mentioned with signaturs or with pointers in structure slots
  ;; don't bother us.

  ;; By the way we fold declarations, sth like

  ;;     typedef struct { .. } foo_rec, *foo;

  ;; is troublesome.

  ;; Anyhow this should be govern by where the structs come from. We walk
  ;; arguments and resuts and look for structurs encountered there. We name them
  ;; by the top-most name we see. Likewise for structure slots.

  (defun bar ()
    (let (top records)
      (labels ((walk-fundef (def)
                 (let ((res (function-type-result-type (decl-type def)))
                       (params (function-type-parameters (decl-type def))))
                   (walk-type res t nil)
                   (when (consp params)
                     (dolist (p params)
                       (when (consp p) (walk-type (cadr p) t nil))))))
               (walk-type (type at-top name)
                 (etypecase type
                   (keyword nil)
                   ((member noffi-c::|__builtin_va_list|))
                   (symbol (let ((ntype (find-typedef type nil nil)))
                             (cond ((null ntype)
                                    (warn "Unknown type ~S" type))
                                   (t
                                    (walk-type ntype at-top (or name type))))))
                   ((cons type-qualifier t)
                    (walk-type (cadr type) at-top name))
                   ((cons (member :pointer :array :block-pointer) t)
                    (walk-type (cadr type) at-top name))
                   ((cons (member :enum) t)
                    nil)
                   ((cons (member :function) t)
                    ;; ###
                    nil)
                   ((cons (member :struct :union) t)
                    (note-record-type type at-top name)
                    (dolist (slot (caddr type))
                      (walk-type (cadr slot) at-top nil)))))
               (note-record-type (type at-top name)
                 (cond ((not at-top))
                       ((and (null name) (null (cadr type)))
                        '(warn "Unnamed record in ~S" top))
                       (t
                        (note-record-type-2 type (or name (cadr type)))
                        )))
               (note-record-type-2 (type name)
                 (unless (member name records :key 'car)
                   (push (list name (resolve-record-type type nil)) records))))
        (dolist (def *all*)
          (setq top def)
          (when (and (decl-p def)
                     (decl-name def)
                     (function-type-p (decl-type def)))
            (walk-fundef def)))
        records
        )))

  (defun resolve-record-type (record-type env)
    (setq record-type (record-type-p record-type))
    (cond ((cddr record-type) record-type)
          (t
           (let ((ntype (find-struct-or-union record-type env nil)))
             (cond ((null ntype)
                    '(warn "Undefined struct ~S" record-type)
                    record-type)
                   (t
                    ntype))))))

;;;; ------------------------------------------------------------------------------------------

  ;; For arguments we need an "argument giver". A list of temp vars, temp vals, pass-forms, clean-up-form

  (defun expand-argument-giver (cont type value-form name)
    (typecase type
      ((member :char :signed-char)        (funcall cont (list :signed-byte value-form)))
      ((member :unsigned-char)            (funcall cont (list :unsigned-byte value-form)))
      ((member :short)                    (funcall cont (list :signed-halfword value-form)))
      ((member :unsigned-short)            (funcall cont (list :unsigned-halfword value-form)))
      ((member :int)                      (funcall cont (list :signed-fullword value-form)))
      ((member :unsigned-int)              (funcall cont (list :unsigned-fullword value-form)))
      ((member :long)                     (funcall cont (list :signed-doubleword value-form)))
      ((member :unsigned-long)             (funcall cont (list :unsigned-doubleword value-form)))
      ((member :long-long)                (funcall cont (list :signed-doubleword value-form)))
      ((member :unsigned-long-long)        (funcall cont (list :unsigned-doubleword value-form)))
      ((member :float)
       (funcall cont (list :single-float `(coerce ,value-form 'single-float))))
      ((member :double)
       (funcall cont (list :double-float `(coerce ,value-form 'double-float))))
      (keyword
       (error "Don't know how to pass a ~S" type))
      ((cons type-qualifier t)
       (expand-argument-giver cont (cadr type) value-form name))
      ((cons (member :array) t)
       (expand-argument-giver cont `(:pointer ,(cadr type)) value-form name))
      ((cons (member :pointer) t)
       (expand-pointer-argument cont (cadr type) value-form name))
      (symbol
       (expand-argument-giver cont (find-typedef type nil) value-form (or name type)))
      #+NIL
      ((cons (member :struct :union) t)
       ;; (:record size-in-bits)
       )
      ((cons (member :enum) t)
       (note-enum-type type name)
       (expand-argument-giver cont :int value-form name))
      ))
    
  (defun expand-pointer-argument (cont base-type value-form name)
    ;; First of all get at the relevant base-type.
    (setf (values base-type name) (effective-base-type base-type name))
    (etypecase base-type
      ;; When this points to a number or enum, we either take a SAP or a list
      ;; sequence which is then zero terminated.
      ((member :char :signed-char :unsigned-char
               :short :unsigned-short
               :int :unsigned-int
               :long :unsigned-long
               :long-long :unsigned-long-long
               :float :double :long-double
               :bool
               :float-complex :double-complex :long-double-complex)
       (expand-number-pointer-argument cont base-type value-form))
      ((cons (member :enum) t)
       (expand-number-pointer-argument cont :int value-form))
      ((member :void)
       (funcall cont (list :address `(sap-ptr ,value-form))))
      ;; When this points to a record, we note that record with the given name and
      ;; then accept a SAP.
      ((cons (member :struct :union) t)
       (let ((type-name (note-record-type base-type name)))
         (funcall cont
                  (list :address `(get-record-pointer ',type-name ,value-form)))))
      ;;
      ;; When this points to a pointer (or array), we punt for now.
      ((cons (member :pointer :block-pointer) t)
       (expand-pointer-pointer-argument cont base-type value-form))
      ((cons (member :function) t)
       (expand-function-pointer-argument cont base-type value-form)) ))

  (defun effective-base-type (base-type name)
    (loop until (or (keywordp base-type)
                    (typep base-type '(cons (member :pointer :array :struct :union :block-pointer :enum :function) t)))
          do (setq base-type
                   (etypecase base-type
                     ((and symbol (not keyword))
                      (setq name (or name base-type))
                      (find-typedef base-type nil))
                     ((cons type-qualifier t)
                      (cadr base-type)))))
    (values base-type name))

  (defun note-enum-type (base-type name)
    (unless (or name (cadr base-type))
      (error "Anonymous enum: ~S" base-type))
    (let ((lisp-type
           (ffi-intern (or name (cadr base-type)))))
      lisp-type))


  (defun ffi-intern (name &optional ear-muffs)
    (intern (convert-name (string name) ear-muffs) :noffi-ffi))

  (defmethod expand-number-pointer-argument (cont (base-type (eql :char)) value-form)
    (expand-number-pointer-argument-1 cont value-form 1 'ccl:%get-signed-byte 0 'char-sap-p))

  (defmethod expand-number-pointer-argument (cont (base-type (eql :signed-char)) value-form)
    (expand-number-pointer-argument-1 cont value-form 1 'ccl:%get-signed-byte 0 'char-sap-p))

  (defmethod expand-number-pointer-argument (cont (base-type (eql :unsigned-char)) value-form)
    (expand-number-pointer-argument-1 cont value-form 1 'ccl:%get-unsigned-byte 0 'unsigned-char-sap-p))

  (defmethod expand-number-pointer-argument (cont (base-type (eql :short)) value-form)
    (expand-number-pointer-argument-1 cont value-form 2 'ccl:%get-signed-word 0 'short-sap-p))

  (defmethod expand-number-pointer-argument (cont (base-type (eql :unsigned-short)) value-form)
    (expand-number-pointer-argument-1 cont value-form 2 'ccl:%get-unsigned-word 0 'unsigned-short-sap-p))

  (defmethod expand-number-pointer-argument (cont (base-type (eql :int)) value-form)
    (expand-number-pointer-argument-1 cont value-form 4 'ccl:%get-signed-long 0 'int-sap-p))

  (defmethod expand-number-pointer-argument (cont (base-type (eql :unsigned-int)) value-form)
    (expand-number-pointer-argument-1 cont value-form 4 'ccl:%get-unsigned-long 0 'unsigned-int-sap-p))

  (defmethod expand-number-pointer-argument (cont (base-type (eql :long)) value-form)
    (expand-number-pointer-argument-1 cont value-form 8 'ccl:%%get-signed-longlong 0 'long-sap-p))

  (defmethod expand-number-pointer-argument (cont (base-type (eql :unsigned-long)) value-form)
    (expand-number-pointer-argument-1 cont value-form 8 'ccl:%%get-unsigned-longlong 0 'unsigned-long-sap-p))

  (defmethod expand-number-pointer-argument (cont (base-type (eql :long-long)) value-form)
    (expand-number-pointer-argument-1 cont value-form 8 'ccl:%%get-signed-longlong 0 'long-long-sap-p))

  (defmethod expand-number-pointer-argument (cont (base-type (eql :unsigned-long-long)) value-form)
    (expand-number-pointer-argument-1 cont value-form 8 'ccl:%%get-unsigned-longlong 0 'unsigned-long-long-sap-p))

  (defmethod expand-number-pointer-argument (cont (base-type (eql :float)) value-form)
    (expand-number-pointer-argument-1 cont value-form 4 'ccl:%get-single-float 0 'float-sap-p))

  (defmethod expand-number-pointer-argument (cont (base-type (eql :double)) value-form)
    (expand-number-pointer-argument-1 cont value-form 4 'ccl:%get-double-float 0 'double-sap-p))

  (defun expand-number-pointer-argument-1 (cont value-form
                                           element-size accessor zero predicate)
    (let ((g (gensym))
          (a (gensym "A."))
          (len (gensym "LEN."))
          (i (gensym "I."))
          (fun (gensym)))
      `(let ((,g ,value-form))
         (labels ((,fun (,a)
                    ,(funcall cont (list :address a))))
           (declare (dynamic-extent #',fun))
           (cond
             ((null ,g) (,fun ccl:+null-ptr+))
             ,@(and (member element-size '(1))
                    (list `((stringp ,g)
                            (ccl::with-utf-8-cstr (,a ,g)
                              (,fun ,a)))))
             ((vectorp ,g)
              (let ((,len (length ,g)))
                (declare (type fixnum ,len))
                (ccl:%stack-block ((,a (the fixnum (* (the fixnum (1+ ,len)) ,element-size))))
                  (loop for ,i of-type fixnum below ,len do
                        (setf (,accessor ,a (the fixnum (* ,element-size ,i)))
                              (aref ,g ,i)))
                  (setf (,accessor ,a (the fixnum (* ,element-size (the fixnum (1+ ,len)))))
                        ,zero)
                  (,fun ,a))))
             ((,predicate ,g)
              (,fun (sap-ptr ,g)))
             (t
              (error "~S is neither a vector nor a SAP." ,g)))))))

  (defun expand-pointer-pointer-argument (cont base-type value-form)
    ;; ### More!
    (let ((g (gensym))
          (a (gensym))
          (fun (gensym)))
      `(let ((,g ,value-form))
         (labels ((,fun (,a)
                    ,(funcall cont (list :address a))))
           (declare (dynamic-extent #',fun))
           (cond
             ((null ,g) (,fun ccl:+null-ptr+))
             ((pointer-sap-p ,g)
              ;; Type checking!
              (,fun (sap-ptr ,g)))
             (t
              (error "~S is not a SAP." ,g)))))))

  (defun expand-function-pointer-argument (cont base-type value-form)
    ;; ### More!
    (let ((g (gensym))
          (a (gensym))
          (fun (gensym)))
      `(let ((,g ,value-form))
         (labels ((,fun (,a)
                    ,(funcall cont (list :address a))))
           (declare (dynamic-extent #',fun))
           (cond
             ((null ,g) (,fun ccl:+null-ptr+))
             ((function-sap-p ,g)
              ;; Type checking!
              (,fun (sap-ptr ,g)))
             (t
              (error "~S is not a SAP." ,g)))))))

;;;; ------------------------------------------------------------------------------------------

  (defun expand-result-marshaling (type name)
    ;; -> <ccl-type> <function-form>
    (typecase type
      ((member :char :signed-char)        (values :signed-byte 'identity))
      ((member :unsigned-char)            (values :unsigned-byte 'identity))
      ((member :short)                    (values :signed-halfword 'identity))
      ((member :unsigned-short)            (values :unsigned-halfword 'identity))
      ((member :int)                      (values :signed-fullword 'identity))
      ((member :unsigned-int)              (values :unsigned-fullword 'identity))
      ((member :long)                     (values :signed-doubleword 'identity))
      ((member :unsigned-long)             (values :unsigned-doubleword 'identity))
      ((member :long-long)                (values :signed-doubleword 'identity))
      ((member :unsigned-long-long)        (values :unsigned-doubleword 'identity))
      ((member :float)                    (values :single-float 'identity))
      ((member :double)                   (values :double-float 'identity))
      ((cons (member :enum) t)
       (note-enum-type type name)
       (expand-result-marshaling :int name))
      ((member :void)                     (values :void 'identity))
      (keyword
       (error "Don't know how to pass a ~S" type))
      ((cons type-qualifier t)            (expand-result-marshaling (cadr type) name))
      ((cons (member :array) t)           (expand-result-marshaling `(:pointer ,(cadr type)) name))
      ((cons (member :pointer) t)         (expand-pointer-marshaling (cadr type) name))
      (symbol
       (expand-result-marshaling (find-typedef type nil) (or name type)))
      #+NIL
      ((cons (member :struct :union) t)
       ;; (:record size-in-bits)
       )
    
      ))

  (defun expand-pointer-marshaling (base-type name)
    ;; First of all get at the relevant base-type.
    (setf (values base-type name) (effective-base-type base-type name))
    (etypecase base-type
      ;; When this points to a number or enum, we either take a SAP or a list
      ;; sequence which is then zero terminated.
      ((member :char :signed-char)        (values :address `make-char-sap))
      ((member :unsigned-char)            (values :address `make-unsigned-char-sap))
      ((member :short)                    (values :address `make-short-sap))
      ((member :unsigned-short)           (values :address `make-unsigned-short-sap))
      ((member :int)                      (values :address `make-int-sap))
      ((member :unsigned-int)             (values :address `make-unsigned-int-sap))
      ((member :long)                     (values :address `make-long-sap))
      ((member :unsigned-long)            (values :address `make-unsigned-long-sap))
      ((member :long-long)                (values :address `make-long-long-sap))
      ((member :unsigned-long-long)       (values :address `make-unsigned-long-long-sap))
      ((member :float)                    (values :address `make-float-sap))
      ((member :double)                   (values :address `make-double-sap))
      ((member :void)                     (values :address 'make-void-sap))
      ((cons (member :enum) t)            (expand-pointer-marshaling :int name))
      ((cons (member :struct :union) t)
       (let ((type-name (note-record-type base-type name)))
         (values :address (let ((g (gensym)))
                            `(lambda (,g) (make-record-sap ,g ',type-name))))))
      ;;
      ;; When this points to a pointer (or array), we punt for now.
      ((cons (member :pointer :block-pointer) t)
       (values :address 
               (let ((g (gensym)))
                 `(lambda (,g) (make-instance 'pointer-sap :sap ,g :base-type ',base-type)))))
      ;;
      ((cons (member :function) t)
       (values :address 
               (let ((g (gensym)))
                 `(lambda (,g) (make-instance 'function-sap :sap ,g :base-type ',base-type))))) ))

  (defun make-decl-defun (decl)
    (let ((*gensym-counter* 0))
      (let* ((cname (decl-name decl))
             (lname (ffi-intern cname))
             (params (function-type-parameters (decl-type decl)))
             (lparams
              (cond ((eq params :unspecified)
                     (error "We punt on ~S" cname))
                    ((member '&rest params)
                     (warn "We punt on ~S for varargs" cname)
                     (return-from make-decl-defun nil))
                    (t
                     (mapcar (lambda (p)
                               (if (car p) (ffi-intern (car p)) (gensym "ARG.")))
                             params)))))
        (labels ((aux (cparams lparams yet)
                   (cond ((null cparams)
                          (multiple-value-bind (res-type-key marshal-fun)
                              (expand-result-marshaling (function-type-result-type (decl-type decl)) nil)
                            `(,marshal-fun
                              (CCL:%FF-CALL (CCL:%REFERENCE-EXTERNAL-ENTRY-POINT
                                             (LOAD-TIME-VALUE (CCL:EXTERNAL ,(symbol-name cname))))
                                            ,@yet ,res-type-key))))
                         (t
                          (expand-argument-giver
                           (lambda (args)
                             (aux (cdr cparams) (cdr lparams)
                                  (append yet args)))
                           (cadr (car cparams))
                           (car lparams)
                           nil)))))
          `(export (defun ,lname ,lparams
                     ,(aux params lparams nil))
                   :noffi-ffi)))))

  (defun note-record-type (base-type name)
    (setq name (or name (cadr base-type)))
    (unless name
      (error "Anonymous struct: ~S" base-type))
    (setq base-type (resolve-record-type base-type nil))
    (let ((lisp-type
           (ffi-intern name)))
      (unless (gethash lisp-type *record-type-hash*)
        (setf (gethash lisp-type *record-type-hash*) base-type)
        (setq *aux-definitions*
              (nconc *aux-definitions* (make-record-type-def lisp-type base-type name))))
      lisp-type))

  (defun make-record-type-def (lisp-name base-type c-name)
    (multiple-value-bind (layout size-of)
        (and (cddr base-type) (full-struct-type-layout base-type nil))
      (labels ((accessor (slot-name)
                 (intern (format nil "~A.~A"
                                 (symbol-name lisp-name)
                                 (convert-name (symbol-name slot-name)))
                         :noffi-ffi)))
        `(
          (export ',lisp-name :noffi-ffi)
          (defclass ,lisp-name (sap) ())
          ,@(loop for (name type size pos) in layout
                  append
                  (when name
                    (let ((accessor (accessor name)))
                      (list `(export ',accessor :noffi-ffi)
                            `(defmethod ,accessor ((object ,lisp-name))
                               ,(peek-form '(sap-ptr object) (/ pos 8) type nil)
                               )
                            `(defmethod (setf ,accessor) (new-value (object ,lisp-name))
                               ,(poke-form '(sap-ptr object) (/ pos 8) 'new-value type nil)
                               )))))
          ,@(and layout
                 (let ((constructor (intern (format nil "MAKE.~A" (symbol-name lisp-name))
                                            :noffi-ffi))
                       (key-args
                        (mapcar (lambda (slot-name)
                                  (list slot-name
                                        (ffi-intern slot-name)
                                        nil
                                        (gensym)))
                                (remove nil (mapcar #'car layout)))))
                   (list
                    `(export ',constructor :noffi-ffi)
                    `(defun ,constructor (&key ,@(mapcar'cdr key-args))
                       (let ((res (ccl::malloc ,(/ size-of 8))))
                         ,@(loop for (slot-name var nil svar) in key-args collect
                                 `(when ,svar (setf (,(accessor slot-name) res) ,var)))
                         ,(pointer-peek-form 'res base-type c-name))))))))))

  (defun full-struct-type-layout (struct-type env)
    (labels ((foo (struct-type offset prefix)
               (setq struct-type (record-type-p struct-type env))
               (setq struct-type (resolve-record-type struct-type nil))
               (multiple-value-bind (layout size align)
                   (struct-type-layout struct-type env)
                 (when layout
                   (values
                    (loop for (name type size pos) in layout
                          append
                          (cond ((record-type-p type)
                                 (foo type (+ pos offset) (cons name prefix)))
                                (t
                                 (list (list (make-symbol
                                              (format nil "~{~A.~}~A" (reverse prefix) name))
                                             type
                                             size
                                             (+ pos offset))))))
                    size align)))))
      (foo struct-type 0 nil)))

  (defun peek-form (macptr offset type name)
    (etypecase type
      ((eql :char) `(ccl:%get-signed-byte ,macptr ,offset))
      ((eql :signed-char) `(ccl:%get-signed-byte ,macptr ,offset))
      ((eql :unsigned-char) `(ccl:%get-unsigned-byte ,macptr ,offset))
      ((eql :short) `(ccl:%get-signed-word ,macptr ,offset))
      ((eql :unsigned-short) `(ccl:%get-unsigned-word ,macptr ,offset))
      ((eql :int) `(ccl:%get-signed-long ,macptr ,offset))
      ((eql :unsigned-int) `(ccl:%get-unsigned-long ,macptr ,offset))
      ((eql :long) `(ccl:%%get-signed-longlong ,macptr ,offset))
      ((eql :unsigned-long) `(ccl:%%get-unsigned-longlong ,macptr ,offset))
      ((eql :long-long) `(ccl:%%get-signed-longlong ,macptr ,offset))
      ((eql :unsigned-long-long) `(ccl:%%get-unsigned-longlong ,macptr ,offset))
      ((eql :float) `(ccl:%get-single-float ,macptr ,offset))
      ((eql :double) `(ccl:%get-double-float ,macptr ,offset))
      ((cons (member :enum) t)
       (peek-form :int offset type name))
      ((and symbol (not keyword))
       (peek-form macptr offset (find-typedef type nil) (or name type)))
      ((cons (member :pointer) t)
       (pointer-peek-form `(ccl:%get-ptr ,macptr ,offset) (cadr type) name))
      ((cons (member :struct :union) t)
       ;; ###
       nil)
      ((cons (member :array) t)
       ;; ###
       nil)
      ))

  (defun pointer-peek-form (macptr base-type name)
    (multiple-value-bind (type-key marshal-fun)
        (expand-pointer-marshaling base-type name)
      (assert (eq :address type-key))
      `(,marshal-fun ,macptr)))

;;;;;;

  (defun poke-form (macptr offset value type name)
    (etypecase type
      ((eql :char)                `(setf (ccl:%get-signed-byte ,macptr ,offset) ,value))
      ((eql :signed-char)         `(setf (ccl:%get-signed-byte ,macptr ,offset) ,value))
      ((eql :unsigned-char)       `(setf (ccl:%get-unsigned-byte ,macptr ,offset) ,value))
      ((eql :short)               `(setf (ccl:%get-signed-word ,macptr ,offset) ,value))
      ((eql :unsigned-short)      `(setf (ccl:%get-unsigned-word ,macptr ,offset) ,value))
      ((eql :int)                 `(setf (ccl:%get-signed-long ,macptr ,offset) ,value))
      ((eql :unsigned-int)        `(setf (ccl:%get-unsigned-long ,macptr ,offset) ,value))
      ((eql :long)                `(setf (ccl:%%get-signed-longlong ,macptr ,offset) ,value))
      ((eql :unsigned-long)       `(setf (ccl:%%get-unsigned-longlong ,macptr ,offset) ,value))
      ((eql :long-long)           `(setf (ccl:%%get-signed-longlong ,macptr ,offset) ,value))
      ((eql :unsigned-long-long)  `(setf (ccl:%%get-unsigned-longlong ,macptr ,offset) ,value))
      ((eql :float)               `(setf (ccl:%get-single-float ,macptr ,offset) ,value))
      ((eql :double)              `(setf (ccl:%get-double-float ,macptr ,offset) ,value))
      ((cons (member :enum) t)    (poke-form :int offset value type name))
      ((and symbol (not keyword))
       (poke-form macptr offset value (find-typedef type nil) (or name type)))
      ((cons (member :pointer) t)
       (pointer-poke-form macptr offset value (cadr type) name))
      ((cons (member :struct :union) t)
       ;; ###
       nil)
      ((cons (member :array) t)
       ;; ###
       nil)
      ))

  (defun pointer-poke-form (macptr offset value base-type name)
    (expand-pointer-argument (lambda (args)
                               (destructuring-bind (type-key macptr) args
                                 (assert (eq :address type-key))
                                 `(setf (ccl:%get-ptr ,macptr ,offset) ,macptr)))
                             base-type
                             value
                             name))

  (defun quux ()
    (setq *aux-definitions* nil)
    (clrhash *record-type-hash*)
    (clrhash *enum-type-hash*)
    (let ((forms
           (list
            (make-decl-defun (find 'noffi-c::|XOpenDisplay| *all* :key 'cadr))
            (make-decl-defun (find 'noffi-c::|XDefaultScreen| *all* :key 'cadr))
            (make-decl-defun (find 'noffi-c::|XWhitePixel| *all* :key 'cadr))
            (make-decl-defun (find 'noffi-c::|XBlackPixel| *all* :key 'cadr))
            (make-decl-defun (find 'noffi-c::|XMapWindow| *all* :key 'cadr))
            (make-decl-defun (find 'noffi-c::|XCreateSimpleWindow| *all* :key 'cadr))
            (make-decl-defun (find 'noffi-c::|XFlush| *all* :key 'cadr))
            (make-decl-defun (find 'noffi-c::|XRootWindow| *all* :key 'cadr))
            )))
      (with-open-file (out "exp/xlib.lisp" :direction :output :if-exists :supersede)
        (with-standard-io-syntax
          (let ((*print-pretty* t)
                (*print-circle* t))
            (prin1 `(in-package :noffi) out)
            (terpri out)
            (terpri out)
            (let ((*package* (find-package :noffi)))
              (dolist (form 
                        (append *aux-definitions* forms))
                (prin1 form out)
                (terpri out)
                (terpri out))))))))

  (defun quux ()
    ;; (eval '(trace peek-form find-typedef note-record-type))
    (foo)
    (dump-ffi "xlib"))

  (defun quux-macros ()
    (maphash (lambda (k v)
               (cond ((eq (car v) :object)
                      (let ((expr
                             (ignore-errors
                               (eval-constant 
                                (parse-expression (cpp (de.bauhh.cpp::pp-token-text k)))
                                nil))))
                        (when expr
                          (print (list (de.bauhh.cpp::pp-token-text k)
                                       expr)))))))
             de.bauhh.cpp::*defs*)
    )

  (defun dump-ffi (name)
    (setq *aux-definitions* nil)
    (clrhash *record-type-hash*)
    (clrhash *enum-type-hash*)
    (let ((forms))
      (loop for def in *all* do
            (when (and (decl-p def)
                       (decl-name def)
                       (function-type-p (decl-type def)))
              (setq forms
                    (nconc forms
                           (list (make-decl-defun def))))))
      (with-open-file (out (merge-pathnames name "exp/*.lisp") :direction :output :if-exists :supersede)
        (with-standard-io-syntax
          (let ((*print-pretty* t)
                (*print-circle* t))
            (prin1 `(in-package :noffi) out)
            (terpri out)
            (terpri out)
            (maphash (lambda (k v)
                       (cond ((eq (car v) :object)
                              (let ((expr
                                     (ignore-errors
                                       (eval-constant 
                                        (parse-expression (cpp (de.bauhh.cpp::pp-token-text k)))
                                        nil))))
                                (when expr
                                  (prin1 `(export (defconstant ,(ffi-intern (de.bauhh.cpp::pp-token-text k) "+")
                                                    ,expr)
                                                  :noffi-ffi)
                                         out)
                                  (terpri out))))))
                     de.bauhh.cpp::*defs*)
            (terpri out)
            (let ((*package* (find-package :noffi)))
              (dolist (form 
                        (append *aux-definitions* forms))
                (prin1 form out)
                (terpri out)
                (terpri out))))))))

  (defun gen-ffi (bundle-name matter &key cflags)
    (inhale matter :cflags cflags)
    (with-open-file (out "/tmp/a.sexpr" :direction :output :if-exists :supersede)
      (dolist (x *all*)
        (prin1 x out) (terpri out)))
    (dump-ffi bundle-name))

  (defun do-gl ()
    (gen-ffi "gl" '("#include <GL/glx.h>"
                    "#include <GL/gl.h>")
             :cflags '("-I" "/opt/X11/include")))

  (defun do-xlib ()
    (gen-ffi "xlib" '("#include <X11/Xlib.h>"
                      "#include <X11/Xutil.h>"
                      ;; "#include <X11/keysym.h>"
                      ;; "#include <X11/Xft/Xft.h>"
                      "#include <X11/extensions/Xrender.h>"
                      "#include <X11/extensions/Xrandr.h>"
                      "#include <X11/extensions/XKB.h>")
             :cflags '("-I" "/opt/X11/include")))

;;;; ------------------------------------------------------------------------------------------


  (defun test ()
    (with-open-file (o "/tmp/ist" :direction :output :if-exists :supersede)
      (with-open-file (in #p"fodder/freebsd-gtk3.j" :external-format :iso-8859-1)
        (mapc (lambda(x)(print x o)) (time (noffi-lex::tokenize-stream in)))))
    (with-open-file (o "/tmp/soll" :direction :output :if-exists :supersede)
      (with-open-file (in #p"fodder/freebsd-gtk3.j" :external-format :iso-8859-1)
        (let ((scanner (c-lexer in)))
          (mapc (lambda(x)(print x o))
                (time (loop for x = (multiple-value-list (funcall scanner))
                            until (eq :eof (car x))
                            ;; collect (subseq x 0 4)
                            ))))))
    (values))

  
;;;;

  ;; What we may want:

  ;; (:DE ( <sclass>* ) <identifier> <type> [ <init> ]? )

  ;; e.g. (:de (:typedef) #_foo :int)
  ;;      (:de () #_bar (:function :int ((x :int)))
  ;;           (begin (return (* x x))))

  ;; We _may_ contemplate to put all struct definitions at the top-level
  ;; of a scope for sanity.

  ;; e.g. typedef struct foo { ... } foo, fooPtr; =>

  ;; (:de nil nil (:struct foo ...))
  ;; (:de :typedef foo (:struct foo))
  ;; (:de :typedef foo-ptr (:pointer (:struct foo)))

  ;; or even:

  ;; (:de :struct foo ...)
  ;; (:de :typedef foo (:struct foo))
  ;; (:de :typedef foo-ptr (:pointer (:struct foo)))

  ;; But I don't like the latter.

;;; Scope

  ;; Scope is either global or per block. While the argument list of a
  ;; function shares its scope with the function body. While we would fare
  ;; well with pullin structs in aruments to the top of the function body,
  ;; C itself is not. So that's a dangerous thing to do when we want to
  ;; dump our stuff.

  ;; Anyhow, let's go with this :DE. It somewhat sane and somewhat easy to
  ;; work with.



  ;; (:de (:extern) (#_x :int) (#_y :int))

  )
