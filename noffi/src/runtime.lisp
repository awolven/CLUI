(in-package :noffi)

;;;; Pointers

#||

(defun ptr-inc (ptr delta))
(defun ptr-difference (ptr-1 ptr-2))
(defun null-ptr-p (ptr))
(defun null-ptr ())

(defun peek-u8 (ptr &optional offset))
(defun peek-u16 (ptr &optional offset))
(defun peek-u32 (ptr &optional offset))
(defun peek-u64 (ptr &optional offset))
(defun peek-u128 (ptr &optional offset))

(defun peek-i8 (ptr &optional offset))
(defun peek-i16 (ptr &optional offset))
(defun peek-i32 (ptr &optional offset))
(defun peek-i64 (ptr &optional offset))
(defun peek-i128 (ptr &optional offset))

(defun peek-float (ptr &optional offset))
(defun peek-double (ptr &optional offset))
(defun peek-long-double (ptr &optional offset))

(defun peek-ptr (ptr &optional offset base-type))

(defun ptr-base-type (ptr))
(defun coerce-ptr-base-type (ptr new-type))

(defun ptr-int (ptr))
(defun int-ptr (ptr))

(defun c-malloc (size))
(defun c-malloc-gcable (size))
(defun c-free (ptr))
(defun c-memcpy (dest-ptr source-ptr count))

||#


;;;; Host Implementation Foreign Pointers (SAP)

#+CCL
(progn
  (defmacro sap-peek-u8  (sap &optional (offset 0))    `(ccl:%get-unsigned-byte ,sap ,offset))
  (defmacro sap-peek-u16 (sap &optional (offset 0))    `(ccl:%get-unsigned-word ,sap ,offset))
  (defmacro sap-peek-u32 (sap &optional (offset 0))    `(ccl:%get-unsigned-long ,sap ,offset))
  (defmacro sap-peek-u64 (sap &optional (offset 0))    `(ccl:%%get-unsigned-longlong ,sap ,offset))
  (defmacro sap-peek-s8  (sap &optional (offset 0))    `(ccl:%get-signed-byte ,sap ,offset))
  (defmacro sap-peek-s16 (sap &optional (offset 0))    `(ccl:%get-signed-word ,sap ,offset))
  (defmacro sap-peek-s32 (sap &optional (offset 0))    `(ccl:%get-signed-long ,sap ,offset))
  (defmacro sap-peek-s64 (sap &optional (offset 0))    `(ccl:%%get-signed-longlong ,sap ,offset))
  (defmacro sap-peek-float (sap &optional (offset 0))  `(ccl:%get-single-float ,sap ,offset))
  (defmacro sap-peek-double (sap &optional (offset 0)) `(ccl:%get-double-float ,sap ,offset))
  (defmacro sap-peek-sap (sap &optional (offset 0))    `(ccl:%get-ptr ,sap ,offset))
  (defmacro sap-int (sap)                              `(ccl:%ptr-to-int ,sap))
  (defmacro int-sap (int)                              `(ccl:%int-to-ptr ,int))
  (defmacro sap-plus (sap delta)                       `(ccl:%inc-ptr ,sap ,delta))

  (defmacro sap-malloc (size)                          `(ccl::malloc ,size))
  (defmacro sap-free (size)                            `(ccl::free ,size))

  (define-symbol-macro +null-ptr-sap+
    (ccl:%null-ptr))

  (defmacro sap-null-ptr-p (sap)
    `(ccl:%null-ptr-p ,sap))

  (defmacro with-stack-allocated-sap ((&rest sap-size*) &body body)
    `(ccl:%stack-block (,@sap-size*)
       ,@body))
  )
            

#+SBCL
(progn
  (defmacro sap-peek-u8  (sap &optional (offset 0))    `(sb-sys:sap-ref-8 ,sap ,offset))
  (defmacro sap-peek-u16 (sap &optional (offset 0))    `(sb-sys:sap-ref-16 ,sap ,offset))
  (defmacro sap-peek-u32 (sap &optional (offset 0))    `(sb-sys:sap-ref-32 ,sap ,offset))
  (defmacro sap-peek-u64 (sap &optional (offset 0))    `(sb-sys:sap-ref-64 ,sap ,offset))
  (defmacro sap-peek-s8  (sap &optional (offset 0))    `(sb-sys:signed-sap-ref-8 ,sap ,offset))
  (defmacro sap-peek-s16 (sap &optional (offset 0))    `(sb-sys:signed-sap-ref-16 ,sap ,offset))
  (defmacro sap-peek-s32 (sap &optional (offset 0))    `(sb-sys:signed-sap-ref-32 ,sap ,offset))
  (defmacro sap-peek-s64 (sap &optional (offset 0))    `(sb-sys:signed-sap-ref-64 ,sap ,offset))
  (defmacro sap-peek-float (sap &optional (offset 0))  `(sb-sys:sap-ref-single ,sap ,offset))
  (defmacro sap-peek-double (sap &optional (offset 0)) `(sb-sys:sap-ref-double ,sap ,offset))
  (defmacro sap-peek-sap (sap &optional (offset 0))    `(sb-sys:sap-ref-sap ,sap ,offset))
  (defmacro sap-int (sap)                              `(sb-sys:sap-int ,sap))
  (defmacro int-sap (int)                              `(sb-sys:int-sap (ldb (byte #+64-BIT 64 #-64-BIT 32 0) ,int)))
  (defmacro sap-plus (sap delta)                       `(sb-sys:sap+ ,sap (sldb (byte #+64-BIT 64 #-64-BIT 32 0) ,delta)))

  (defun sap-malloc (size)
    ;; this is the native way to allocate foreign memory in SBCL
    (let ((sap (sb-alien:alien-sap (sb-alien:make-alien (sb-alien:unsigned 8) size))))
      (when (zerop (sb-sys:sap-int sap))
        (error "malloc(3) failed us while trying to allocate ~D bytes." size))
      sap))

  (defun sap-free (sap)
    ;; and this is the native way to free foreign memory in SBCL
    (sb-alien:free-alien (sb-alien:sap-alien sap (sb-alien:* (sb-alien:unsigned 8)))))

  (define-symbol-macro +null-ptr-sap+
    (sb-sys:int-sap 0))

  (defmacro sap-null-ptr-p (sap)
    `(eql 0 (sb-sys:sap-int ,sap)))

  (defmacro with-stack-allocated-sap ((&rest sap-size*) &body body &environment env)
    ;; Stick a gensym in front
    (setq sap-size* (mapcar (lambda (b) (cons (gensym) b)) sap-size*))
    ;; First sort out those bindings that have a constant size.
    (let ((constant-size-bindings
           (remove-if-not (lambda (binding) (constantp (caddr binding) env)) sap-size*))
          (variable-size-bindings
           (remove-if (lambda (binding) (constantp (caddr binding) env)) sap-size*)))
      (setq body
            `(let ,(mapcar (lambda (b)
                             (list (cadr b) `(sb-alien:alien-sap ,(car b))))
                           sap-size*)
               ,@body))
      (when constant-size-bindings
        (setq body
              `(sb-alien:with-alien 
                   ,(mapcar (lambda (b)
                              (destructuring-bind (gensym sap size) b
                                (declare (ignore sap))
                                (list gensym
                                      `(array sb-alien:unsigned-char
                                              ,(eval (macroexpand size env))) )))
                            constant-size-bindings)
                 ,body)))
      (dolist (b variable-size-bindings)
        (destructuring-bind (gensym sap size) b
          (declare (ignore sap))
          (setq body `(let ((,gensym (sb-alien:make-alien sb-alien:unsigned-char ,size)))
                        (unwind-protect
                             ,body
                          (sb-alien:free-alien ,gensym))))))
      body)) )

;; Convertion between unsigned 64-bit integers and double floats. We
;; might want to find something that compiles to more efficient code
;; with the given host Lisp.

(defun double-u64 (double)
  (with-stack-allocated-sap ((sap 8))
    (setf (sap-peek-double sap) double)
    (sap-peek-u64 sap)))

(defun u64-double (u64)
  (with-stack-allocated-sap ((sap 8))
    (setf (sap-peek-u64 sap) u64)
    (sap-peek-double sap)))


;;;; -- C Values ------------------------------------------------------------------------------

;; For arrays and functions the `value' is a SAP and these are actual
;; pointers. It's only when these values are actually used for computation
;; that they are turned into pointer types. Structure type also have `value'
;; being the SAP, the only difference is that their type is not automatically
;; coerced.

(defstruct (cval (:constructor cons-cval (value type))
                 (:predicate cvalp))
  value
  type)

(defstruct (ptr (:include cval)
                (:constructor %cons-ptr (value offset type)))
  offset
  #+(OR SBCL EXCL)
  (cookie nil))

#+CCL
(defun cons-ptr (value offset type)
  (and (not (and (ccl:macptrp value)
                 (ccl:%null-ptr-p value)
                 (zerop offset)))
       (%cons-ptr value offset type)))

#+SBCL
(defun cons-ptr (value offset type)
  (cond ((and (sb-sys:system-area-pointer-p value)
              (zerop (sb-sys:sap-int value))
              (zerop offset))
         nil)
        (t
         (check-type value sb-sys:system-area-pointer)
         (%cons-ptr value offset type))))

(defun %cast-ptr (ptr type)
  (let ((res (%cons-ptr (ptr-base-sap ptr) (ptr-offset ptr) type)))
    #+SBCL
    (setf (ptr-cookie res) (ptr-cookie ptr))
    res))

(defun int-ptr (integer &optional (type (make-pointer-type :void)))
  (cons-ptr +null-ptr-sap+ (ldb #+NOFFI-64-BIT-TARGET (byte 64 0)
                                #+NOFFI-32-BIT-TARGET (byte 32 0)
                                integer) type))

(defun ptr-int (ptr)
  (if ptr (+ (sap-int (ptr-base-sap ptr)) (ptr-offset ptr)) 0))

(defun ptr-base-sap (ptr)
  "Returns the pointer part of `ptr'. Note that the offset is _not_ included."
  (cond ((null ptr) +null-ptr-sap+)
        ((cvalp ptr) (cval-value ptr))
        (t (error "~S is not a foreign pointer." ptr))))

(defun ptr-effective-sap (ptr)
  (if ptr
      (let ((offset (ptr-offset ptr)))
        (if (eql 0 offset)
            (ptr-base-sap ptr)
            (sap-plus (ptr-base-sap ptr) offset)))
      +null-ptr-sap+))

(defun ptr-nullptr-p (ptr)
  (or (null ptr) (sap-null-ptr-p (ptr-effective-sap ptr))))

(defun ptr-difference (p1 p2)
  (- (ptr-int p1) (ptr-int p2)))

(defun null-ptr () nil)                 ;Hmm

(macrolet ((aux (table)
             `(progn
                ,@ (loop for (our-name their-accessor) in table
                         collect
                         `(progn
                            (defun ,our-name (ptr &optional (offset 0))
                              (,their-accessor (ptr-base-sap ptr) (+ (ptr-offset ptr) offset)))
                            (defun (setf ,our-name) (value ptr &optional (offset 0))
                              (setf (,their-accessor (ptr-base-sap ptr) (+ (ptr-offset ptr) offset)) value)))))))
  (aux ((peek-u8        sap-peek-u8)
        (peek-u16       sap-peek-u16)
        (peek-u32       sap-peek-u32)
        (peek-u64       sap-peek-u64)
        ;;
        (peek-s8        sap-peek-s8)
        (peek-s16       sap-peek-s16)
        (peek-s32       sap-peek-s32)
        (peek-s64       sap-peek-s64)
        ;;
        (peek-float     sap-peek-float)
        (peek-double    sap-peek-double)) ))

(defun peek-u128 (ptr &optional (offset 0))
  (dpb (peek-u64 ptr (+ offset 8)) (byte 64 64) (peek-u64 ptr offset)))

(defun (setf peek-u128) (new-value ptr &optional (offset 0))
  (setf (peek-u64 ptr (+ offset 0)) (ldb (byte 64 0) new-value)
        (peek-u64 ptr (+ offset 8)) (ldb (byte 64 64) new-value))
  new-value)

(defun peek-s128 (ptr &optional (offset 0))
  (sldb (byte 128 0) (dpb (peek-u64 ptr (+ offset 8)) (byte 64 64) (peek-u64 ptr offset))))

(defun (setf peek-s128) (new-value ptr &optional (offset 0))
  (setf (peek-u64 ptr (+ offset 0)) (ldb (byte 64 0) new-value)
        (peek-u64 ptr (+ offset 8)) (ldb (byte 64 64) new-value))
  new-value)

#+CCL
(defun peek-ptr (ptr type &optional (offset 0))
  (let ((sap (ptr-base-sap ptr)))
    (cond ((ccl::external-entry-point-p sap)
           (cons-ptr
            (ccl:%get-ptr (ccl:%int-to-ptr (ccl:%reference-external-entry-point sap))
                          (+ (ptr-offset ptr) offset))
            0 type))
          (t
           (cons-ptr (sap-peek-sap (ptr-base-sap ptr) (+ (ptr-offset ptr) offset))
                     0 type)))))

(defun (setf peek-ptr) (value ptr type &optional (offset 0))
  (declare (ignore type))
  (setf (sap-peek-sap (ptr-base-sap ptr) (+ (ptr-offset ptr) offset))
        (ptr-effective-sap value))
  value)

#+SBCL
(defun peek-ptr (ptr type &optional (offset 0))
  (cons-ptr (sb-sys:sap-ref-sap (ptr-base-sap ptr) (+ (ptr-offset ptr) offset))
            0 type))

#+EXCL
(defun peek-ptr (ptr type &optional (offset 0))
  (cons-ptr (sap-peek-sap (ptr-base-sap ptr) (+ (ptr-offset ptr) offset))
            0 type))



;;;; External References

;; This in general is troublesome because of dumping. When an image is
;; reloaded those external entry points need to be relinked. CCL does it right
;; and has an external entry point data type. SBCL punts here and makes
;; SB-ALIEN:EXTERN-ALIEN a macro resulting in the address being resolved at
;; runtime.

#+CCL
(defun extern-addr (name-string type)
  "Given a name returns a foreign pointer pointing to a datum of type
`type'. That is this equivalent to &foo in C."
  (cons-ptr
   (ccl:external (string name-string))
   0 (make-pointer-type type)))

#+SBCL
;; What SBCL does here somehow isn't the right thing. It does fail for
;; #_stdout e.g. SB-ALIEN however works for getting at a _value_, but
;; not at the address. Perhaps we need a different approach?
(defun extern-addr (name-string type)
  (cons-ptr
   (sb-sys:foreign-symbol-sap (string name-string))
   0 (make-pointer-type type)))


;;;; ------------------------------------------------------------------------------------------

;; Here also conversion of sequences to C vectors happens.

(defun c-coerce (object result-type)
  ;; Hmm, do want to have this being a cast?
  (cond ((and (integer-type-p result-type) (characterp object))
         (c-coerce (char-code object) result-type))
        ;;
        ((and (integer-type-p result-type) (realp object))
         (cons-cval (if (integer-type-signed-p result-type)
                        (sldb (byte (integer-type-size result-type) 0)
                              (truncate object))
                        (ldb (byte (integer-type-size result-type) 0)
                             (truncate object)))
                    result-type))
        ;;
        ((and (integer-type-p result-type)
              (cvalp object)
              (arithmetic-type-p (cval-type object)))
         (c-coerce (cval-value object) result-type))
        ((and (pointer-type-p result-type)
              (null object))
         (null-ptr))
        ((and (pointer-type-p result-type)
              (cvalp object)
              (array-type-p (cval-type object)))
         (cons-ptr (cval-value object) 0 result-type))
        ((and (pointer-type-p result-type)
              (or (eql object 0)
                  (and (cvalp object)
                       (integer-type-p (cval-type object))
                       (eql 0 (cval-value object)))))
         (null-ptr))
        ((and (float-type-p result-type)
              (realp object))
         (cons-cval (coerce object (float-type-lisp-type result-type)) result-type))
        ((and (float-type-p result-type)
              (cvalp object)
              (arithmetic-type-p (cval-type object)))
         (cons-cval (coerce (cval-value object) (float-type-lisp-type result-type)) result-type))
        ((and (pointer-type-p result-type)
              (cvalp object)
              (pointer-type-p (cval-type object)))
         (%cast-ptr object result-type))
        ;;
        ;; Lisp string -> UTF-8 encoded char*
        ;;
        ((and (pointer-type-p result-type)
              (integer-type-p (pointer-type-base (pointer-type-p result-type)))
              (= 8 (integer-type-size (pointer-type-base (pointer-type-p result-type))))
              (stringp object))
         (make-gcable-c-string object))
        ;; Same for UTF-16
        ((and (pointer-type-p result-type)
              (integer-type-p (pointer-type-base (pointer-type-p result-type)))
              (= 16 (integer-type-size (pointer-type-base (pointer-type-p result-type))))
              (stringp object))
         (make-gcable-c-utf-16-string object))
        
        ;; Same for UCS-4
        #+(or)
        ((and (pointer-type-p result-type)
              (integer-type-p (pointer-type-base (pointer-type-p result-type)))
              (= 32 (integer-type-size (pointer-type-base (pointer-type-p result-type))))
              (stringp object))
         (make-gcable-c-utf-16-string object))
        ;;
        ((and (pointer-type-p result-type)
              (typep object 'sequence))
         (c-sequence-coerce object result-type))
        ;
        ((and (pointer-type-p result-type)
              (integer-type-p (cval-type object)))
         (int-ptr (cval-value object) result-type))
        ((and (pointer-type-p result-type)
              (integerp object))
         (int-ptr object result-type))
        ;;
        ((and (integer-type-p result-type)
              (pointer-type-p (cval-type object)))
         (c-coerce (ptr-int object) result-type))
        ;;
        (t
         (error "Don't know how to make ~S into a ~S." object result-type))))

#+CCL
(defun c-make (type &optional (count 1) (clear t))
  "Allocates a GCable foreign object of type _type_ and returns a CVAL
   pointer to it. If _count_ is given a vector of that length is
   allocated. When _clear_ is true (the default) the memory is zeroed
   before being returned."
  (let* ((size (type-size-align type nil))
         (sap  (ccl::%new-gcable-ptr (* count (/ size 8)) clear))
         (ptr  (cons-ptr sap 0 (make-pointer-type type))))
    ptr))

#+SBCL
(defun make-gcable (ptr)
  (let ((sap (ptr-base-sap ptr)))
    (setf (ptr-cookie ptr) (list nil))
    (sb-ext:finalize (ptr-cookie ptr) #'(lambda ()
                                          ;; (format *trace-output* "~&freeing ~S~%" sap)
                                          (sap-free sap))))
  ptr)

#+SBCL
(defun c-make (type &optional (count 1) (clear t))
  "Allocates a GCable foreign object of type _type_ and returns a CVAL
   pointer to it. If _count_ is given a vector of that length is
   allocated. When _clear_ is true (the default) the memory is zeroed
   before being returned."
  ;; We said SB-ALIEN::%MAKE-ALIEN here, but it also just calls malloc(3).
  ;; So we call either malloc(3) or calloc(3).
  (let* ((size (type-size-align type nil))
         (bytes (* count (/ size 8)))
         (sap  (if clear
                   (SB-ALIEN:ALIEN-FUNCALL
                    (SB-ALIEN:EXTERN-ALIEN "calloc" (FUNCTION SB-SYS:SYSTEM-AREA-POINTER SB-ALIEN:SIZE-T SB-ALIEN:SIZE-T))
                    1 bytes)
                   (SB-ALIEN:ALIEN-FUNCALL
                    (SB-ALIEN:EXTERN-ALIEN "malloc" (FUNCTION SB-SYS:SYSTEM-AREA-POINTER SB-ALIEN:SIZE-T))
                    bytes)))
         (ptr  (cons-ptr sap 0 (make-pointer-type type))))
    (make-gcable ptr)))

(defun c-sequence-coerce (object result-type)
  (cond ((pointer-type-p result-type)
         (let ((base-type (pointer-type-base result-type)))
           (let* ((ptr (c-make (pointer-type-base result-type) (1+ (length object)))))
             (loop for i below (length object)
                   for x = (elt object i)
                   do (setf (%c-aref ptr i) x)
                   finally (cond ((integer-type-p base-type)
                                  (setf (%c-aref ptr i) 0))
                                 ((pointer-type-p base-type)
                                  (setf (%c-aref ptr i) nil))))
             ptr)))
        (t
         (error "huh? coercing ~S to ~S" object result-type))))


;;;; String Conversion

;; For string conversion we prefer to use whatever routines the Lisp
;; implementation provides in the hope that those are reasonable fast.

#+CCL
(progn
  
  (defun make-gcable-c-string (string &key (start 0) (end nil)
                                           (type (load-time-value (make-pointer-type :char))))
    (let ((len (length string)))
      (setq end (or end (length string)))
      (check-type start fixnum)
      (check-type end fixnum)
      (assert (<= 0 start end len))
      (multiple-value-bind (data offset)
          (ccl::array-data-and-offset string)
        (LET* ((end (+ end offset))
               (start (+ start offset))
               (nbytes (ccl::utf-8-octets-in-string data start end)))
          (let ((sap (ccl::%new-gcable-ptr (+ nbytes 1))))
            (ccl::utf-8-memory-encode data sap 0 start end)
            (setf (ccl:%get-unsigned-byte sap nbytes) 0)
            (cons-ptr sap 0 type))))))

  (defun make-gcable-c-utf-16-string (string &key (start 0) (end nil)
                                                  (type (load-time-value (make-pointer-type :unsigned-short))))
    (let ((len (length string)))
      (setq end (or end (length string)))
      (check-type start fixnum)
      (check-type end fixnum)
      (assert (<= 0 start end len))
      (multiple-value-bind (data offset)
          (ccl::array-data-and-offset string)
        (LET* ((end (+ end offset))
               (start (+ start offset))
               (nbytes (ccl::utf-16-octets-in-string data start end)))
          (let ((sap (ccl::%new-gcable-ptr (+ nbytes 2))))
            (ccl::native-utf-16-memory-encode data sap 0 start end)
            (setf (ccl:%get-unsigned-word sap nbytes) 0)
            (cons-ptr sap 0 type))))))

  (defun get-c-string (ptr)
    ;; A pity that CCL::%GET-UTF-8-CSTRING doesn't accept an offset.
    (ccl::%get-utf-8-cstring (ptr-effective-sap ptr)))

  (defun get-native-utf16-string (ptr)
    (ccl::%get-native-utf-16-cstring (ptr-effective-sap ptr))) )

#+SBCL
(progn

  (defun make-gcable-c-string (string &key (start 0) (end nil)
                                           (type (load-time-value (make-pointer-type :char))))
    (make-gcable
     (cons-ptr
      (sb-alien::%make-alien-string string :start start :end end :external-format :UTF-8)
      0 type)))

  (defun make-gcable-c-utf-16-string (string &key (start 0) (end nil)
                                                  (type (load-time-value (make-pointer-type :unsigned-short))))
    (make-gcable
     (cons-ptr
      (sb-alien::%make-alien-string string :start start :end end
                                    :external-format #+LITTLE-ENDIAN :UTF-16LE
                                                     #-LITTLE-ENDIAN :UTF-16BE)
      0 type)))

  #+SBCL
  (defun get-c-string (ptr)
    (sb-alien::c-string-to-string (ptr-effective-sap ptr) :utf-8 'character))

  #+SBCL
  (defun get-native-utf16-string (ptr)
    (sb-alien::c-string-to-string (ptr-effective-sap ptr)
                                  #+LITTLE-ENDIAN :utf-16le
                                  #-LITTLE-ENDIAN :utf-16be
                                  'character)) )


;;;;

(defun c-arith (int-op flo-op x y)
  (values
   (cond ((and (integerp x) (integerp y))
          (funcall int-op x y))
         ((and (realp x) (integerp y))
          (funcall flo-op (coerce x 'float) (coerce y 'float)))
         ((and (integerp x) (realp y))
          (funcall flo-op (coerce x 'float) (coerce y 'float)))
         ;; foreign int x lisp integer
         ((and (cvalp x) (integer-type-p (cval-type x)) (integerp y))
          (funcall int-op (cval-value x) y))
         ((and (integerp x) (cvalp y) (integer-type-p (cval-type y)))
          (funcall int-op x (cval-value y)))
         ;; foreign float x lisp real
         ((and (cvalp x) (float-type-p (cval-type x)) (realp y))
          (funcall flo-op x x y))
         ((and (realp x) (cvalp y) (float-type-p (cval-type y)))
          (funcall flo-op x x y))
         ;; foreign real x foreign real
         ((and (cvalp x) (arithmetic-type-p (cval-type x))
               (cvalp y) (arithmetic-type-p (cval-type y)))
          (let ((rtype (usual-arithmetic-conversion (cval-type x) (cval-type y) nil)))
            ;; Hmm.
            (c-coerce (funcall (if (float-type-p rtype) flo-op int-op)
                               (cval-value x)
                               (cval-value y))
                      rtype)))
         (t
          (error "~@<Cannot do ~S on ~S and ~S.~:@>" int-op x y)))))

(defun c-compare (pred x y)
  (c-coerce
   (if (cond ((and (realp x) (realp y))
              (funcall pred x y))
             ;;
             ((and (cvalp x) (arithmetic-type-p (cval-type x)) (realp y))
              (funcall pred (cval-value x) y))
             ((and (realp x) (cvalp y) (arithmetic-type-p (cval-type y)))
              (funcall pred x (cval-value y)))
             ((and (cvalp x) (cvalp y)
                   (arithmetic-type-p (cval-type x))
                   (arithmetic-type-p (cval-type y)))
              (let ((rtype (usual-arithmetic-conversion (cval-type x) (cval-type y) nil)))
                (funcall pred
                         (cval-value (c-coerce (cval-value x) rtype))
                         (cval-value (c-coerce (cval-value y) rtype)))))
             ((and (cvalp x)
                   (cvalp y)
                   (pointer-type-p (cval-type x))
                   (pointer-type-p (cval-type y)))
              (funcall pred (ptr-int x) (ptr-int y)))
             (t
              (error "Cannot do ~S on ~S and ~S." pred x y)))
       1 0)
   :int))

(defun c-shift (dir x y)
  (let ((count
         (cond ((integerp y) y)
               ((integer-type-p (cval-type y)) (cval-value y))
               (t (error "Bad shift count: ~S" y)))))
    (cond ((integerp x)
           (ash x (* dir y)))
          ((integer-type-p (cval-type x))
           (let ((rtype (promoted-integer-type (cval-type x) nil)))
             (c-coerce (ash (cval-value x) (* dir count))
                       rtype)))
          (t
           (error "Cannot do a shift on ~S and ~S." x y)))))

(defun c+ (x y)
  (setq x (array-pointer-promotion x)
        y (array-pointer-promotion y))
  (cond ((and (cvalp x) (pointer-type-p (cval-type x)))
         (c-ptr+ x y))
        ((and (cvalp y) (pointer-type-p (cval-type y)))
         (c-ptr+ y x))
        (t
         (c-arith '+ '+ x y))))

(defun array-pointer-promotion (x)
  (if (and (cvalp x) (array-type-p (cval-type x)))
      (cons-ptr (ptr-base-sap x) (ptr-offset x)
                (make-pointer-type (array-type-base (cval-type x))))
      x))

(defun c-ptr+ (ptr delta)
  (cond ((integerp delta)
         (cons-ptr (cval-value ptr)
                   (+ (ptr-offset ptr)
                      (if (= delta 0) 0
                          (* (let ((s (type-size (pointer-type-base (cval-type ptr)))))
                               (assert (= 0 (mod s 8)))
                               (floor s 8))
                             delta)))
                   (cval-type ptr)))
        ((and (cvalp delta) (integer-type-p (cval-type delta)))
         (c-ptr+ ptr (cval-value delta)))
        (t
         (error "Cannot do ~S on ~S and ~S" 'c-ptr+ ptr delta))))

(defun c-ptr- (ptr delta)
  (cond ((integerp delta)
         (cons-ptr (cval-value ptr)
                   (- (ptr-offset ptr)
                      (if (= delta 0) 0
                          (* (let ((s (type-size (pointer-type-base (cval-type ptr)))))
                               (assert (= 0 (mod s 8)))
                               (floor s 8))
                             delta)))
                   (cval-type ptr)))
        ((and (cvalp delta) (integer-type-p (cval-type delta)))
         (c-ptr- ptr (cval-value delta)))
        (t
         (error "Cannot do ~S on ~S and ~S" 'c-ptr+ ptr delta))))

(defun %c-sizeof-type (type)
  (let ((s (type-size type)))
    (unless (eql 0 (mod s 8))
      (error "You cannot take the size of ~S" type))
    (cons-cval (floor s 8) (size_t-type))))

(defun c-sizeof-type (type)
  (let ((s (type-size type)))
    (unless (eql 0 (mod s 8))
      (error "You cannot take the size of ~S" type))
    s))

(defun c-alignof-type (type)
  (let ((a (type-align type)))
    (unless (eql 0 (mod a 8))
      (error "You cannot talk about the alignment of ~S" type))
    (cons-cval (floor a 8) (size_t-type))))

(defun c- (x y)
  (cond ((and (cvalp x) (pointer-type-p (cval-value x))
              (or (integerp y) (and (cvalp y) (integer-type-p (cval-type y)))))
         (c-ptr- x y))
        ((and (cvalp y) (pointer-type-p (cval-value y))
              (or (integerp x) (and (cvalp x) (integer-type-p (cval-type x)))))
         (c-ptr- y x))
        #+(or)
        ;; XXX Wrong. C-PTR-DIFFERENCE works on terms of 'char', while this needs to
        ;; work in terms of elements pointed at.
        ((and (cvalp x) (pointer-type-p (cval-value x))
              (cvalp y) (pointer-type-p (cval-value y)))
         (c-ptr-difference x y))
        (t
         (c-arith '- '- x y))))

(defun c* (x y) (c-arith '* '* x y))
(defun c/ (x y) (c-arith 'truncate '/ x y))
(defun c% (x y) (c-arith 'rem nil x y))

(defun c<  (x y) (c-compare '< x y))
(defun c>  (x y) (c-compare '> x y))
(defun c<= (x y) (c-compare '<= x y))
(defun c>= (x y) (c-compare '>= x y))
(defun c== (x y) (c-compare '= x y))
(defun c!= (x y) (c-compare '/= x y))

(defun c<< (x y) (c-shift +1 x y))
(defun c>> (x y) (c-shift -1 x y))

(defun c-logior (x y) (c-arith 'logior nil x y))
(defun c-logxor (x y) (c-arith 'logxor nil x y))
(defun c-logand (x y) (c-arith 'logand nil x y))

(defmacro c-if (test cons alt)
  `(if (c-boolean-value ,test) ,cons ,alt))

(defun c-not (x)
  (c-if x (c-coerce 0 :int) (c-coerce 1 :int)))

(defun c-boolean-value (x)
  (cond ((null x) nil)
        ((realp x) (not (zerop x)))
        ((and (cvalp x) (arithmetic-type-p (cval-type x)))
         (c-boolean-value (cval-value x)))
        ((and (cvalp x) (pointer-type-p (cval-type x)))
         (not (ptr-nullptr-p x)))
        (t
         (error "Don't know how to treat ~S as a boolean value." x))))

(defun c-lognot (x)
  (cond ((integerp x) (lognot x))
        ((and (cvalp x) (integer-type-p (cval-type x)))
         (let ((rtype (promoted-integer-type (cval-type x) nil)))
           (c-coerce (lognot (cval-value (c-coerce x rtype))) rtype)))
        (t
         (error "Don't know how to do ~S on ~S" 'c-lognot x))))

(defun c+/1 (x)
  (cond ((realp x) x)
        ((and (cvalp x) (integer-type-p (cval-type x)))
         (c-coerce x (promoted-integer-type (cval-type x) nil)))
        ((and (cvalp x) (arithmetic-type-p (cval-type x)))
         x)
        (t (error "Don't know how to do unary plus (+) on ~S" x))))

(defun c-/1 (x)
  (cond ((realp x) (- x))
        ((and (cvalp x) (integer-type-p (cval-type x)))
         (let ((rtype (promoted-integer-type (cval-type x) nil)))
           (c-coerce (- (cval-value (c-coerce x rtype)))
                     rtype)))
        ((and (cvalp x) (arithmetic-type-p (cval-type x)))
         (- (cval-value x)))
        (t (error "Don't know how to do unary plus (+) on ~S" x))))

(defun %c-aref (ptr &optional (delta 0 delta-p))
  ;; This is the thing that the "compiler" emits. Returns a CVAL.
  (setq ptr (promoted-cval ptr))
  (when delta-p (setq ptr (c+ delta ptr)))
  (cond ((and (cvalp ptr) (pointer-type-p (cval-type ptr)))
         (c-aref-1 (pointer-type-base (cval-type ptr)) ptr))
        (t
         (error "~S is not a pointer, don't know how to dereference it." ptr))))

(defun c-aref (ptr &optional (delta 0))
  (as-lisp (%c-aref ptr delta)))

(defun c-aref-1 (base ptr)
  (cond ((integer-type-p base)
         (c-coerce (if (integer-type-signed-p base)
                       (ecase (integer-type-size base)
                         (  8 (peek-s8 ptr 0))
                         ( 16 (peek-s16 ptr 0))
                         ( 32 (peek-s32 ptr 0))
                         ( 64 (peek-s64 ptr 0))
                         (128 (peek-s128 ptr 0)))
                       (ecase (integer-type-size base)
                         (  8 (peek-u8 ptr 0))
                         ( 16 (peek-u16 ptr 0))
                         ( 32 (peek-u32 ptr 0))
                         ( 64 (peek-u64 ptr 0))
                         (128 (peek-u128 ptr 0))))
                   base))
        ((eq :float (bare-expanded-type base nil))
         (peek-float ptr 0))
        ((eq :double (bare-expanded-type base nil))
         (peek-double ptr 0))
        ((pointer-type-p base)
         (c-coerce (peek-ptr ptr base) base))
        ((function-type-p base)
         ;; We actually return a function type here as with arrays.
         (cons-ptr (ptr-base-sap ptr) (ptr-offset ptr) base))
        ((array-type-p base)
         ;; The cval-value of an array already is the thing.
         (cons-ptr (ptr-base-sap ptr) (ptr-offset ptr) base))
        ((record-type-p base)
         ;; Ouch!
         (cons-ptr (ptr-base-sap ptr) (ptr-offset ptr) base))
        (t
         (error "Don't know how to dereference some ~S" ptr))))

;; offsetof
;; \.
;; post-inc
;; pre-inc
;; (the type (:object ...))
;; pre-inc
;; pre-dec
;; &    (c-addr c->) (c-addr \.) (c-addr c-aref) (c-addr identifier)
;; not
;; sizeof-expr
;; sizeof-type

;; and or
;; setf: c., c->, c-identifier
;; setf-op * / % + - << >> logand logxor logior
;; progn
;; ?: binary
;; identifier

;;; done

;; * / %
;; + -
;; < > <= >= = /=       (== !=)
;; << >>
;; +/1
;; -/1
;; logand, logxor, logior
;; lognot
;; ->
;; the (as a cast)
;; aref
;; ?: ternary
;; (setf c-aref)
;; funcall

;; To & the only allowed arguments are either (*x), some identifier, or
;; some <expr>.<slot> with <expr> itself being a valid form to &. This
;; however implies prior expansion of x->y to (*x).y and a[i] to *(a + i).

;; 

#+(or)
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

;; Arrays are a bit tricky. Arrays are converted to their pointers when
;; "looked" at. Suppose 'foo' is an array declared by 'int foo[10];'.

;; 'foo' as itself evaluates to type 'int[10]'. It's only e.g a dereference
;; that makes it into a pointer to an element. +/2 does too etc.


;; x.z = (&x)->z = *(&x + <offsetof z>)

;; x.y.z = (&x)->y.z = &((&x)->y)->z

;; &(*(&x + #y))->z

;; (&x + #y)->z

;; &x + #y + #z

;; CCL:%FF-CALL is a regular function

(defmacro c-identifier (identifier &environment env)
  (let ((decl (find-identifier-declaration identifier env :errorp t)))
    `(promoted-cval
      ,(cond ((enum-key-declaration-p decl)
              `(cons-cval ',(declaration-init decl)
                          ',(declaration-type decl)))
             ((extern-declaration-p decl)
              `(%c-aref (extern-addr ',identifier ',(declaration-type decl))))
             ((constant-static-declaration-p decl)
              (assert (not (null (declaration-init decl))))
              ;; Hmm
              `(c-form ,(declaration-init decl)))
             (t
              (blame identifier "No idea what to make of ~S of storage class ~S"
                     identifier
                     (declaration-storage-class decl)))))))

(defun promoted-cval (cval)
  "This does the magic that an array turns into a pointer to its first
element and a function is turned to a pointer."
  (cond ((cvalp cval)
         (let ((type (cval-type cval)))
           (cond ((integer-type-p type)
                  (c-coerce cval (promoted-integer-type type nil)))
                 ((function-type-p type)
                  (cons-ptr (ptr-base-sap cval) (ptr-offset cval) (make-pointer-type type)))
                 ((array-type-p type)
                  (cons-ptr (ptr-base-sap cval) (ptr-offset cval) (make-pointer-type (array-type-base type))))
                 (t
                  cval))))
        (t
         cval)))


;;;; ------------------------------------------------------------------------------------------

(defun (setf %c-aref) (new-value object &optional (offset 0))
  (prog1 new-value
    ;; ### reverse arguments?
    (setq object (promoted-cval object))
    (unless (eql offset 0) (setq object (c+ object offset)))
    (unless (and (cvalp object) (pointer-type-p (cval-type object)))
      (error "Cannot dereference a non-pointer C value, ~S." object))
    (let ((base (pointer-type-base (cval-type object))))
      (cond
        ((integer-type-p base)
         (let ((value (cval-value (c-checked-coerce new-value base))))
           (assert (integerp value))
           (if (integer-type-signed-p base)
               (ecase (integer-type-size base)
                 (  8 (setf (peek-s8 object 0) value))
                 ( 16 (setf (peek-s16 object 0) value))
                 ( 32 (setf (peek-s32 object 0) value))
                 ( 64 (setf (peek-s64 object 0) value))
                 (128 (setf (peek-s128 object 0) value)))
               (ecase (integer-type-size base)
                 (  8 (setf (peek-u8 object 0) value))
                 ( 16 (setf (peek-u16 object 0) value))
                 ( 32 (setf (peek-u32 object 0) value))
                 ( 64 (setf (peek-u64 object 0) value))
                 (128 (setf (peek-u128 object 0) value))))))
        ((pointer-type-p base)
         (let ((value (c-checked-coerce new-value base)))
           (setf (peek-ptr object 0) value)))
        #+(or)
        ((function-type-p base)
         ;; We actually return a function type here as with arrays.
         (cons-ptr (ptr-base-sap ptr) (ptr-offset ptr) base))
        #+(or)
        ((array-type-p base)
         ;; The cval-value of an array already is the thing.
         (cons-ptr (ptr-base-sap ptr) (ptr-offset ptr) base))
        ((eq :float (bare-expanded-type base nil))
         (setf (peek-float object 0) (cval-value (c-checked-coerce new-value ':float))))
        ((eq :double (bare-expanded-type base nil))
         (setf (peek-double object 0) (cval-value (c-checked-coerce new-value ':double))))
        (t
         (error "Don't know how to SETF a reference to some ~S" base)) ))))

(defun (setf c-aref) (new-value object &optional (offset 0))
  (setf (%c-aref object offset) new-value))

(defun c-checked-coerce (object type)
  (c-coerce object type))


;;;; Function Invocation

;; We may want to refactor this a bit as what we actually do is implementing
;; the ABI calling convention in terms of more primitive types like integers,
;; floating point values, and perhaps pointers. That part is ABI specific,
;; while the actuall foreign function call then is Lisp specific. *sigh*


;;;; SBCL

;; ALIEN-FUNCALL really is only known by the compiler. When ALIEN-FUNCALL is
;; invoked with an unknown function argument a stub is generated at runtime
;; and installed with the alien type. [Again w/o any needed locks]

#+CCL
(defmacro static-c-funcall-2 (fun-type identifier &rest arguments)
  #+(or)
  (when (and (pointer-type-p fun-type) (function-type-p (pointer-type-base fun-type)))
    (setq fun (%c-aref fun)))
  (unless (function-type-p fun-type)
    (error "~S is not a C function" identifier))
  ;;
  (let* ()
    (let ((ff-call-args nil)
          (ff-fun `(CCL:%REFERENCE-EXTERNAL-ENTRY-POINT
                    (LOAD-TIME-VALUE (CCL:EXTERNAL ,(identifier-name identifier)))))
          (outs nil))
      (do ((parameter.q (function-type-parameters fun-type) (cdr parameter.q))
           (arg.q arguments (cdr arg.q)))
          ((or (endp arg.q) (endp parameter.q))
           (when arg.q (error "Too many arguments"))
           ;;
           ;; ### We may want to change this eventually and intern or parse empty
           ;; ### parameter lists as such and use :UNSPECIFIED for the "extern foo();"
           ;; ### case.
           ;;
           (when (and parameter.q (not (and (void-type-p (declaration-type (car parameter.q)))
                                            (endp (cdr parameter.q)))))
             (error "Too few arguments")))
        (let ((parameter (car parameter.q)))
          (macrolet (#+NIL
                     (push-ptr-to-ff-call-args (ptr)
                       `((lambda (arg)
                           (push (cond ((null arg) ccl:+null-ptr+)
                                       ((zerop (ptr-offset arg)) (ptr-base-sap arg))
                                       (t (ccl:%inc-ptr (ptr-base-sap arg) (ptr-offset arg))))
                                 ff-call-args))
                         ,ptr)))
            (cond #+NIL
                  ((eq parameter '&rest)
                   ;; For &rest args we might consider to append a NULL sentinel. There are
                   ;; language extensions even mentioning this. But sometimes e.g. -1 is
                   ;; used. We have that as (:FUNCTION-SPECIFIER (:SENTINEL <expr>)) with
                   ;; gcc.
                   ;;
                   ;; As nice as it would be, I don't believe it is wise to add the
                   ;; sentinel automatically because with a different setup being
                   ;; non-standard we might miss the sentinel attribute.
                   ;;
                   (dolist (raw-arg arg.q)
                     (let ((arg (promoted-cval raw-arg))) ;Hmm
                       ;; ### We ought to move that to a routine converting a Lisp value into a
                       ;; ### CVAL. But there is always guesswork involved.
                       (etypecase arg
                         (character
                          (push :unsigned-doubleword ff-call-args)
                          (push (char-code arg) ff-call-args))
                         (integer
                          ;; ### Do we want to range check? Perhaps. But we cannot tell signed from
                          ;; ### unsigned anyway.
                          (push :unsigned-doubleword ff-call-args)
                          (push (ldb (byte 64 0) arg) ff-call-args))
                         (real
                          (push :double-float ff-call-args)
                          (push (coerce arg 'double-float) ff-call-args))
                         (ptr
                          (assert (pointer-type-p (cval-type arg)))
                          (push :address ff-call-args)
                          (push-ptr-to-ff-call-args arg))
                         (null
                          ;; Doesn't matter much; null is null.
                          (push :unsigned-doubleword ff-call-args)
                          (push 0 ff-call-args)))))
                   (setq arg.q nil))
                  (t
                   (let ((arg (car arg.q))
                         (param-type (declaration-type parameter)))
                     ;; Treat parameters of type array type as pointer types. C has no
                     ;; arrays. Not as values.
                     ;;
                     ;; ### Actually there is more to do.
                     ;;
                     (when (array-type-p param-type)
                       (setq param-type (make-pointer-type (array-type-base param-type))))
                     (cond #+NIL
                           ((eq :out arg)
                            ;; ### Experimental
                            (let ((arg (c-make (pointer-type-base param-type))))
                              (push :address ff-call-args)
                              (push (ptr-base-sap arg) ff-call-args)
                              (push arg outs)))
                           ((pointer-type-p param-type)
                            (push :address ff-call-args)
                            (push `(ptr-effective-sap (c-coerce ,arg ',param-type)) ff-call-args))
                           ((arithmetic-type-p param-type)
                            (push (foreign-type-for-funcall param-type) ff-call-args)
                            (push `(cval-value (c-coerce ,arg ',param-type)) ff-call-args))
                           (t
                            (error "~@<No idea what to do with parameter ~S of type ~S~@:>"
                                   parameter param-type)))))))))
      ;;
      (let ((res-type (function-type-result-type fun-type)))
        (cond ((pointer-type-p res-type)
               (push :address ff-call-args)
               `(cons-ptr (ccl:%ff-call ,ff-fun ,@(reverse ff-call-args)) 0 ',res-type))
              ((arithmetic-type-p res-type)
               (push (foreign-type-for-funcall res-type) ff-call-args)
               `(c-coerce (ccl:%ff-call ,ff-fun ,@(reverse ff-call-args)) ',res-type))
              ((void-type-p res-type)
               (push :void ff-call-args)
               `(progn (ccl:%ff-call ,ff-fun ,@(reverse ff-call-args))
                       (values)))
              (t
               (error "No idea what to do with function return type ~S" res-type)))))))

#+SBCL
(defmacro static-c-funcall-2 (fun-type identifier &rest arguments)
  (error "foo!")
  #+(or)
  (when (and (pointer-type-p fun-type) (function-type-p (pointer-type-base fun-type)))
    (setq fun (%c-aref fun)))
  (unless (function-type-p fun-type)
    (error "~S is not a C function" identifier))
  ;;
  (let* ()
    (let ((ff-call-args nil)
          (ff-fun
           `(SB-ALIEN:EXTERN-ALIEN ,(identifier-name identifier) ,(foreign-type-for-funcall fun-type)))
          (outs nil))
      (do ((parameter.q (function-type-parameters fun-type) (cdr parameter.q))
           (arg.q arguments (cdr arg.q)))
          ((or (endp arg.q) (endp parameter.q))
           (when arg.q (error "Too many arguments"))
           ;;
           ;; ### We may want to change this eventually and intern or parse empty
           ;; ### parameter lists as such and use :UNSPECIFIED for the "extern foo();"
           ;; ### case.
           ;;
           (when (and parameter.q (not (and (void-type-p (declaration-type (car parameter.q)))
                                            (endp (cdr parameter.q)))))
             (error "Too few arguments")))
        (let ((parameter (car parameter.q)))
          (cond #+NIL
                  ((eq parameter '&rest)
                   ;; For &rest args we might consider to append a NULL sentinel. There are
                   ;; language extensions even mentioning this. But sometimes e.g. -1 is
                   ;; used. We have that as (:FUNCTION-SPECIFIER (:SENTINEL <expr>)) with
                   ;; gcc.
                   ;;
                   ;; As nice as it would be, I don't believe it is wise to add the
                   ;; sentinel automatically because with a different setup being
                   ;; non-standard we might miss the sentinel attribute.
                   ;;
                   (dolist (raw-arg arg.q)
                     (let ((arg (promoted-cval raw-arg))) ;Hmm
                       ;; ### We ought to move that to a routine converting a Lisp value into a
                       ;; ### CVAL. But there is always guesswork involved.
                       (etypecase arg
                         (character
                          (push :unsigned-doubleword ff-call-args)
                          (push (char-code arg) ff-call-args))
                         (integer
                          ;; ### Do we want to range check? Perhaps. But we cannot tell signed from
                          ;; ### unsigned anyway.
                          (push :unsigned-doubleword ff-call-args)
                          (push (ldb (byte 64 0) arg) ff-call-args))
                         (real
                          (push :double-float ff-call-args)
                          (push (coerce arg 'double-float) ff-call-args))
                         (ptr
                          (assert (pointer-type-p (cval-type arg)))
                          (push :address ff-call-args)
                          (push-ptr-to-ff-call-args arg))
                         (null
                          ;; Doesn't matter much; null is null.
                          (push :unsigned-doubleword ff-call-args)
                          (push 0 ff-call-args)))))
                   (setq arg.q nil))
                  (t
                   (let ((arg (car arg.q))
                         (param-type (declaration-type parameter)))
                     ;; Treat parameters of type array type as pointer types. C has no
                     ;; arrays. Not as values.
                     ;;
                     ;; ### Actually there is more to do.
                     ;;
                     (when (array-type-p param-type)
                       (setq param-type (make-pointer-type (array-type-base param-type))))
                     (cond #+NIL
                           ((eq :out arg)
                            ;; ### Experimental
                            (let ((arg (c-make (pointer-type-base param-type))))
                              (push :address ff-call-args)
                              (push (ptr-effective-sap arg) ff-call-args)
                              (push arg outs)))
                           ((pointer-type-p param-type)
                            (push `(ptr-effective-sap (c-coerce ,arg ',param-type)) ff-call-args))
                           ((integer-type-p param-type)
                            (push `(cval-value (c-coerce ,arg ',param-type)) ff-call-args))
                           ((float-type-p param-type)
                            (push `(cval-value (c-coerce ,arg ',param-type)) ff-call-args))
                           (t
                            (error "~@<No idea what to do with parameter ~S of type ~S~@:>"
                                   parameter param-type))))))))
      (setq ff-call-args (reverse ff-call-args))
      ;;
      (let ((res-type (function-type-result-type fun-type)))
        (let* ((temps (mapcar (lambda (x) (declare (ignore x)) (gensym "ARG.")) ff-call-args))
               (ff-call `(progn #+NIL SB-SYS:WITHOUT-INTERRUPTS (SB-ALIEN:ALIEN-FUNCALL ,ff-fun ,@temps))))
          `(LET ,(mapcar #'list temps ff-call-args)
             ,(cond ((pointer-type-p res-type)
                     `(CONS-PTR ,ff-call 0 ',res-type))
                    ((integer-type-p res-type)
                     `(C-COERCE ,ff-call ',res-type))
                    ((float-type-p res-type)
                     `(C-COERCE ,ff-call ',res-type))
                    ((void-type-p res-type)
                     `(progn ,ff-call (values)))
                    (t
                     (error "No idea what to do with function return type ~S" res-type)))))))))

#+SBCL
(defun foreign-type-for-funcall (type &optional env)
  "Given the Noffi type `type' return an alien type which suffices for
use in an alien funcall."
  (cond ((or (pointer-type-p type env) (array-type-p type env))
         'SB-SYS:SYSTEM-AREA-POINTER)
        ((integer-type-p type env)
         (if (integer-type-signed-p type env)
             `(SB-ALIEN:SIGNED ,(integer-type-size type env))
             `(SB-ALIEN:UNSIGNED ,(integer-type-size type env))))
        ((float-type-p type env)
         (ecase (bare-expanded-type type env)
           (:float 'SINGLE-FLOAT)
           (:double 'DOUBLE-FLOAT)))
        ((void-type-p type env)
         'sb-alien:void)
        ((function-type-p type env)
         `(FUNCTION ,(foreign-type-for-funcall (function-type-result-type type env))
                    ,@(cond ((function-type-parameters-empty-p type env)
                             nil)
                            (t (mapcar (lambda (x) (foreign-type-for-funcall x env))
                                       (mapcar #'declaration-type
                                               (function-type-parameters type env)))))))
        (t
         (error "What would be an alien type for the noffi type ~S?" type))))

#+CCL
(defun foreign-type-for-funcall (type &optional env)
  (cond ((integer-type-p type env)
         (if (integer-type-signed-p type env)
             (ecase (integer-type-size type env)
               (8 :signed-byte)
               (16 :signed-halfword)
               (32 :signed-fullword)
               (64 :signed-doubleword))
             (ecase (integer-type-size type env)
               (8 :unsigned-byte)
               (16 :unsigned-halfword)
               (32 :unsigned-fullword)
               (64 :unsigned-doubleword))))
        ((float-type-p type env)
         (ecase (bare-expanded-type type env)
           (:float :single-float)
           (:double :double-float)))
        ((pointer-type-p type env)
         :address)
        ((void-type-p type env)
         :void)
        (t
         (error "What's the foreign type to use for CCL:FF-CALL given noffi type ~S?" type))))


;;;;

(defun c-cast (result-type object)
  ;; *sigh*
  (c-coerce object result-type))


;;;;

;; This is still _way_ too much cut and paste here. 

(defun c-> (object member)
  ;; ### bit fields!
  (setq object (promoted-cval object))
  (labels ((doit (record-type)
             (let* ((layout (record-type-layout record-type nil))
                    (member-layout (assoc member layout)))
               (unless member-layout
                 (error "A member named ~S was not found with ~S" member record-type))
               (multiple-value-bind (type offset)
                   (values (member-layout-type member-layout)
                           (member-layout-bit-offset member-layout))
                 (%c-aref (c-coerce (c+ (cons-ptr (ptr-base-sap object) (ptr-offset object) (make-pointer-type :char))
                                       (/ offset 8))
                                   (make-pointer-type type)))))))
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
             (let* ((layout (record-type-layout record-type nil))
                    (member-layout (assoc member layout)))
               (unless member-layout
                 (error "A member named ~S was not found with ~S" member record-type))
               (multiple-value-bind (type offset)
                   (values (member-layout-type member-layout)
                           (member-layout-bit-offset member-layout))
                 (setf
                  (%c-aref (c-coerce (c+ (cons-ptr (ptr-base-sap object) (ptr-offset object) (make-pointer-type :char))
                                        (/ offset 8))
                                    (make-pointer-type type)))
                  nv)))))
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
             (let* ((layout (record-type-layout record-type nil))
                    (member-layout (assoc member layout)))
               (unless member-layout
                 (error "A member named ~S was not found with ~S" member record-type))
               (multiple-value-bind (type offset)
                   (values (member-layout-type member-layout)
                           (member-layout-bit-offset member-layout))
                 (c-coerce (c+ (cons-ptr (ptr-base-sap object) (ptr-offset object) (make-pointer-type :char))
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

(defmacro c. (object member)
  ;; Hmm
  `(c-> (c-addr-of ,object) ,member))

(defun member-layout-name (member-layout) (first member-layout))
(defun member-layout-type (member-layout) (second member-layout))
(defun member-layout-bit-size (member-layout) (third member-layout))
(defun member-layout-bit-offset (member-layout) (fourth member-layout))
    

;;;;

(defmacro c-addr-of (place &environment env)
  (setf place (macroexpand place env))
  (cond ((and (consp place) (member (car place) '(c-aref %c-aref)))
         `(c-aref-addr ,@(cdr place)))
        ((and (consp place) (eq (car place) 'c->))
         `(c->-addr ,@(cdr place)))
        ((and (consp place) (eq (car place) 'c.))
         `(c.-addr ,@(cdr place)))
        ((and (consp place) (eq (car place) 'c-identifier))
         `(c-identifier-addr ,@(cdr place)))
        (t
         (error "Bad place to take address of: ~S" place))))

(defun c-aref-addr (x &optional (y 0))
  (setq x (promoted-cval x)
        y (promoted-cval y))
  (cond ((or (and (cvalp x) (pointer-type-p (cval-type x))
                  (or (integerp y)
                      (and (cvalp y) (integer-type-p (cval-type y)))))
             (and (cvalp y) (pointer-type-p (cval-type y))
                  (or (integerp x)
                      (and (cvalp x) (integer-type-p (cval-type x))))))
         (c+ x y))
        (t
         (error "Bad arguments to ~S: ~S ~S" 'c-aref x y))))

(defmacro c-identifier-addr (identifier &environment env)
  (let ((decl (find-identifier-declaration identifier env :errorp t)))
    (cond ((extern-declaration-p decl)
           `(extern-addr ',identifier ',(declaration-type decl)))
          (t
           (blame identifier "No idea what to make of ~S" identifier)))))



(defparameter *cheap-binop-table*
  '((* c*) (/ c/) (% c%) (+ c+) (- c-)
    (<< c<<) (>> c>>)
    (< c<) (> c>) (<= c<=) (>= c>=) (= c==) (/= c!=)
    (logior c-logior) (logxor c-logxor) (logand c-logand)
    (aref %c-aref)
    (progn progn)))

(defparameter *cheap-unop-table*
  '((+ c+/1) (- c-/1) (aref %c-aref)
    (lognot c-lognot)
    (& c-addr-of)
    (not c-not)))

(defun comp-expr (expr env &aux it)
  (cond ((identifierp expr)
         `(c-identifier ,expr))
        ((characterp expr)              ;xxx
         `(cons-cval ,(char-code expr) :int))
        ((eq (car expr) ':integer-constant)
         (multiple-value-bind (value type)
             (comp-integer-constant expr env)
           `(cons-cval ,value ',type)))
        ((eq (car expr) ':floating-constant)
         (destructuring-bind (value type) (cdr expr)
           `(cons-cval ,value ',type)))
        ((and (eq (car expr) 'the) (= 3 (length expr)))
         `(c-cast ',(cadr expr) ,(comp-expr (caddr expr) env)))
        
        ((and (= 3 (length expr))
              (setq it (assoc (car expr) *cheap-binop-table*)))
         `(,(cadr it) ,(comp-expr (cadr expr) env) ,(comp-expr (caddr expr) env)))

        ((and (= 2 (length expr))
              (setq it (assoc (car expr) *cheap-unop-table*)))
         `(,(cadr it) ,(comp-expr (cadr expr) env)))

        ((and (= 2 (length expr)) (eq (car expr) 'lisp))
         (cadr expr))

        ((and (= 3 (length expr)) (eq (car expr) '->))
         `(c-> ,(comp-expr (cadr expr) env) ',(caddr expr)))
        
        ((and (= 3 (length expr)) (eq (car expr) '\.))
         `(c. ,(comp-expr (cadr expr) env) ',(caddr expr)))
        
        ;; ((and (eq (car expr) 'and) (= 3 (length expr))) (comp-boolean-expr 'and expr env))
        ;; ((and (eq (car expr) 'or)  (= 3 (length expr))) (comp-boolean-expr 'or expr env))
        ((and (eq (car expr) 'funcall))
         (destructuring-bind (fun &rest args) (cdr expr)
           (cond ((identifierp fun)
                  (let ((type (declaration-type (find-identifier-declaration fun env))))
		    (declare (ignorable type))
                    `(static-c-funcall ,fun ,@(comp-list args env))))
                 (t
                  `(c-funcall ,@(mapcar (lambda (x) (comp-expr x env)) (cdr expr)))))))
        ;;
        ((and (eq (car expr) 'if) (= 4 (length expr)))
         ;; Hmm
         `(if (zerop (as-lisp (c== (c-coerce 0 :int) ,(comp-expr (cadr expr) env))))
              ,(comp-expr (caddr expr) env)
              ,(comp-expr (cadddr expr) env)))
        ;;
        ((and (= 2 (length expr)) (eq (car expr) 'sizeof-type))
         (destructuring-bind (type) (cdr expr)
           (let ((s (type-size type)))
             (unless (eql 0 (mod s 8))
               (error "You cannot take the size of ~S" type))
             `(cons-cval (floor ,s 8) ',(size_t-type)))))
        ;;
        ((and (= 3 (length expr)) (eq (car expr) ':character-constant))
         `(cons-cval ,(evaluate-character-constant (cadr expr)) ':int))
        ((and (= 3 (length expr)) (eq (car expr) ':string-literal))
         ;; This still is far from optimal.
         (destructuring-bind (str prefix) (cdr expr)
           (cond ((null prefix)
                  `(c-coerce ,(de-escaped-c-string str) (make-pointer-type :char)))
                 ((equal "L" prefix)
                  `(c-coerce ,(de-escaped-c-string str)
                            (make-pointer-type
                             (make-named-type (intern "wchar_t" *c-package*)))))
                 (t
                  (error "Unknown string prefix: ~S" prefix)))))
        ((and (= 2 (length expr)) (eq (car expr) 'sizeof-expr))
         (comp-sizeof-expr (cadr expr) env))
        ;;
        ((and (= 3 (length expr)) (eq (car expr) 'setf))
         (destructuring-bind (place new-value) (cdr expr)
           `(SETF ,(comp-expr place env) ,(comp-expr new-value env))))
        ((and (= 2 (length expr)) (eq (car expr) 'alignof-type))
         (destructuring-bind (type) (cdr expr)
           `(c-coerce ,(align-of-type type env)
                      ',(abi-size_t-type *abi*))))
        ((and (= 3 (length expr)) (eq (car expr) 'offsetof))
         (comp-offsetof expr env))
        ((and (= 3 (length expr)) (eq (car expr) 'and))
         (comp-and expr env))
        ((and (= 3 (length expr)) (eq (car expr) 'or))
         (comp-or expr env))
        (t
         (error "Don't know how to compile ~S" expr))))

(defun comp-list (exprs env)
  (mapcar (lambda (expr) (comp-expr expr env)) exprs))

(defun comp-sizeof-expr (expr env)
  `(c-coerce ,(sizeof-expr expr env) `,(abi-size_t-type *abi*)))

(defun comp-offsetof (expr env)
  ;; ### offsetof(a, b.c)
  (destructuring-bind (type member) (cdr expr)
    (multiple-value-bind (member-type size offset)
        (record-type-member-layout type member env)
      (declare (ignore size))
      (assert (not (bit-field-type-p member-type env)))
      (/ offset 8))))

(defun comp-and (expr env)
  (destructuring-bind (a b) (cdr expr)
    (let ((g (gensym)))
      `(let ((,g ,(comp-expr a env)))
         ,(comp-expr `(if (lisp ,g) ,b (lisp ,g)) env)))))

(defun comp-or (expr env)
  (destructuring-bind (a b) (cdr expr)
    (let ((g (gensym)))
      `(let ((,g ,(comp-expr a env)))
         ,(comp-expr `(if (lisp ,g) (lisp ,g) ,b) env)))))

;; For CLET I don't know yet what exactly I want.

;; e.g. (clet ((x #<struct point> :x 10 :y 20))
;;         ...)

;; Would be just beautiful.

;; Also (#_foo 30 (:x 10 :y 20)) ...

;; (#_XCreateWindow dpy root ... (:object #_background black #_border 1))

;; (c:obj #<struct point> :x 10 :y 20)

;; For objects, perhaps we use something like an auto-release pool or some
;; such.

(defmacro clet& (bindings &body body)
  (labels ((aux (bindings lets)
             (cond ((endp bindings)
                    `(LET ,lets ,@body))
                   (t
                    (destructuring-bind ((var type &optional (init nil init-p)) &rest more) bindings
                      (let ((g.sap (gensym "SAP."))
                            (g (gensym "PTR.")))
                        `(WITH-STACK-ALLOCATED-SAP ((,g.sap ,(/ (type-size-align type nil) 8)))
                           (LET ((,g (CONS-PTR ,g.sap 0 ',(make-pointer-type type))))
                             ,@(and init-p
                                    (list `(SETF (C-AREF ,g) ,init)))
                             ,(aux more (cons (list var g) lets))))))))))
    (aux bindings nil)))

(defmacro clet (bindings &body body)
  (let ((gs (mapcar (lambda (x) (declare (ignore x)) (gensym)) bindings)))
    `(CLET& ,(mapcar (lambda (b g) `(,g ,@(cdr b))) bindings gs)
       (SYMBOL-MACROLET ,(mapcar (lambda (b g) (list (car b) `(C-AREF ,g))) bindings gs)
         ,@body))))

(defun as-lisp (cval)
  (cond ((and (cvalp cval)
              (arithmetic-type-p (cval-type cval)))
         (cval-value cval))
        (t cval)))

(define-setf-expander c-ref (place &rest members &environment env)
  (setq place `(c-> ,place ',(car members)))
  (dolist (m (cdr members) place)
    (setq place `(c. ,place ',m)))
  (get-setf-expansion place env))

(defmacro c-ref (place &rest members)
  (setq place `(c-> ,place ',(car members)))
  (dolist (m (cdr members) place)
    (setq place `(c. ,place ',m)))
  `(as-lisp ,place))

#+CCL
(defmacro defcfun ((name res-type) lambda-list &body body)
  ;; ### Box as a :FUNCTION!
  (let ((pascal-name
         (intern (format nil "(PASCAL-FUNCTION ~A)" (verbatim name))
                 (symbol-package name))))
    `(progn
       (ccl:defcallback ,pascal-name
           (,@(mapcan (lambda (parameter)
                        (destructuring-bind (var type)
                            parameter
                          (list (foreign-type-for-funcall type) var)))
                      lambda-list)
              ,(foreign-type-for-funcall res-type))
         (unbox-to-native-object
          (let ,(mapcar (lambda (parameter)
                          (destructuring-bind (var type)
                              parameter
                            `(,var (box-native-object ,var ',type))))
                        lambda-list)

            ,@body)
          ',res-type))
       (define-symbol-macro ,name
           (cons-ptr ,pascal-name 0
                     (make-pointer-type :void)))))) ;xxx

#+SBCL
(defmacro defcfun ((name res-type) lambda-list &body body)
  ;; ### Box as a :FUNCTION!
  (let ((pascal-name
         (intern (format nil "(PASCAL-FUNCTION ~A)" (verbatim name))
                 (symbol-package name)))
        (temps (mapcar (lambda (x) (declare (ignore x)) (gensym "ARG.")) lambda-list))
        (ftype (make-function-type res-type
                                   (mapcar #'(lambda (type)
                                               ;; ###
                                               `(decl nil (nil ,type)))
                                           (mapcar #'cadr lambda-list)))))
    `(progn
       (defparameter ,pascal-name
         (sb-alien::alien-callback
          ,(foreign-type-for-funcall ftype)
          #'(lambda ,temps
              (unbox-to-native-object
               (let ,(mapcar (lambda (parameter temp)
                               (destructuring-bind (var type) parameter
                                 `(,var (box-native-object ,temp ',type))))
                             lambda-list temps)
                 ,@body)
               ',res-type))))
       (define-symbol-macro ,name
           (cons-ptr (sb-alien:alien-sap ,pascal-name)
                     0
                     (make-pointer-type ',ftype))))))

#+CCL
(defun box-native-object (object type)
  (cond ((and (pointer-type-p type)
              (ccl:macptrp object))
         (cons-ptr object 0 type))
        ((or (integer-type-p type) (float-type-p type))
         object)
        (t
         (c-coerce object type))))

#+SBCL
(defun box-native-object (object type)
  (cond ((and (pointer-type-p type)
              (sb-sys:system-area-pointer-p object))
         (cons-ptr object 0 type))
        ((or (integer-type-p type) (float-type-p type))
         object)
        (t
         (c-coerce object type))))

(defun unbox-to-native-object (cval type)
  (cond ((void-type-p type nil)
         nil)
        ((integer-type-p type)
         cval)
        (t
         (error "How to unbox ~S which is a ~S" cval type))))


;;;;

#+CCL
(defun c-funcall (fun &rest args)
  (when (and (pointer-type-p (cval-type fun)) (function-type-p (pointer-type-base (cval-type fun))))
    (setq fun (%c-aref fun)))
  (unless (function-type-p (cval-type fun))
    (error "~S is not a C function" fun))
  (multiple-value-bind (rtype ptypes restp)
      (function-type-signature (cval-type fun))
    (let ((ff-call-args nil))
      ;;
      (labels ((add-arg (arg ptype)
                 (when (array-type-p ptype)
                   (setq ptype (make-pointer-type (array-type-base ptype))))
                 (setq arg (promoted-cval arg))
                 (cond ((arithmetic-type-p ptype)
                        (push (foreign-type-for-funcall ptype) ff-call-args)
                        (push (cval-value (c-coerce arg ptype)) ff-call-args))
                       ((pointer-type-p ptype)
                        (push (foreign-type-for-funcall ptype) ff-call-args)
                        (push (ptr-effective-sap (c-coerce arg ptype)) ff-call-args))
                       (t
                        (error "Don't know how to pass a parameter of type ~S" ptype)))))
        (do ((a args (cdr a))
             (p ptypes (cdr p)))
            ((or (null a) (null p))
             (when (and (null a) p)
               (error "Too few arguments"))
             (when (and (null p) a (not restp))
               (error "Too many arguments"))
             ;; Rest args
             (dolist (arg a)
               (cond ((cvalp arg)       (add-arg arg (cval-type arg)))
                     ((integerp arg)    (add-arg arg ':int))
                     ((floatp arg)      (add-arg arg ':double))
                     (t
                      (error "Cannot handle ~S as an argument to a variadic function." arg)))))
          ;; Required arg
          (add-arg (car a) (car p))))
      ;;
      (let ((ff-fun (let ((sap (ptr-effective-sap fun)))
                      (if (ccl::external-entry-point-p sap)
                          (ccl:%reference-external-entry-point sap)
                          (ccl:%ptr-to-int sap)))))
        (cond ((pointer-type-p rtype)
               (push :address ff-call-args)
               (let ((ff-res (apply #'ccl:%ff-call ff-fun (reverse ff-call-args))))
                 (cons-ptr ff-res 0 rtype)))
              ((arithmetic-type-p rtype)
               (push (foreign-type-for-funcall rtype) ff-call-args)
               (c-coerce (apply #'ccl:%ff-call ff-fun (reverse ff-call-args))
                         rtype))
              ((void-type-p rtype)
               (push :void ff-call-args)
               (apply #'ccl:%ff-call ff-fun (reverse ff-call-args))
               (values))
              (t
               (error "No idea what to do with function return type ~S" rtype)))))))

#+SBCL
(defun c-funcall (fun &rest args)
  (error "foo!")
  (when (and (pointer-type-p (cval-type fun)) (function-type-p (pointer-type-base (cval-type fun))))
    (setq fun (%c-aref fun)))
  (unless (function-type-p (cval-type fun))
    (error "~S is not a C function" fun))
  (multiple-value-bind (rtype ptypes restp)
      (function-type-signature (cval-type fun))
    (let ((alien-parameter-types nil)
          (alien-arguments nil))
      (labels ((add-arg (arg ptype)
                 (when (array-type-p ptype)
                   (setq ptype (make-pointer-type (array-type-base ptype))))
                 (setq arg (promoted-cval arg))
                 (cond ((arithmetic-type-p ptype)
                        (push (foreign-type-for-funcall ptype) alien-parameter-types)
                        (push (cval-value (c-coerce arg ptype)) alien-arguments))
                       ((pointer-type-p ptype)
                        (push (foreign-type-for-funcall ptype) alien-parameter-types)
                        (push (ptr-effective-sap (c-coerce arg ptype)) alien-arguments))
                       (t
                        (error "Don't know how to pass a parameter of type ~S" ptype)))))
        (do ((a args (cdr a))
             (p ptypes (cdr p)))
            ((or (null a) (null p))
             (when (and (null a) p)
               (error "Too few arguments"))
             (when (and (null p) a (not restp))
               (error "Too many arguments"))
             ;; Rest args
             (dolist (arg a)
               (cond ((cvalp arg)       (add-arg arg (cval-type arg)))
                     ((integerp arg)    (add-arg arg ':int))
                     ;;
                     ;; Passing floating point parameters with AMD64 on Windows with SBCL is broken.
                     ;;
                     #-SBCL ((floatp arg)      (add-arg arg ':double))
                     (t
                      (error "Cannot handle ~S as an argument to a variadic function." arg)))))
          ;; Required arg
          (add-arg (car a) (car p))))
      ;;
      (setq alien-parameter-types (reverse alien-parameter-types)
            alien-arguments (reverse alien-arguments))
      (let ((ff-res (apply #'sb-alien:alien-funcall 
                           (sb-alien::%sap-alien (ptr-effective-sap fun)
                                                 (sb-alien::parse-alien-type
                                                  `(FUNCTION ,(foreign-type-for-funcall rtype) ,@alien-parameter-types)
                                                  nil))
                           alien-arguments)))
        (cond ((pointer-type-p rtype)
               (cons-ptr ff-res 0 rtype))
              ((arithmetic-type-p rtype)
               (c-coerce ff-res rtype))
              ((void-type-p rtype)
               (values))
              (t
               (error "No idea what to do with function return type ~S" rtype)))))))


(defun promoted-cval-for-funcall (cval)
  (cond ((cvalp cval)    (promoted-cval cval))
        ((integerp cval) (c-coerce cval ':int))
        ((realp cval)    (c-coerce cval ':double))
        ((stringp cval)  (c-coerce cval (make-pointer-type :char)))
        (t (error "Oops"))))

(defmacro static-c-funcall (identifier &rest args &environment env)
  (let ((type (declaration-type (find-identifier-declaration identifier env))))
    (cond ((nth-value 2 (function-type-signature type))
           `(c-funcall (c-identifier ,identifier) ,@args))
          (t
           (translate-funcall type identifier args)))))

(defun translate-funcall (function-type identifier args)
  ;; Arguments of the the form (:out) are treated as "out" arguments. Storage is
  ;; allocated on the stack with CLET&, a pointer is passed, and the resut of
  ;; C-AREF then is returned as secondary or further return value.
  ;;
  ;; E.g. (#_remquo 4.5 1 (:out)) -> 0.5D0; 4
  ;;
  ;; Or (#_gettimeofday (:out) nil)
  ;;
  (multiple-value-bind (result-type parameters restp)
      (function-type-signature function-type)
    (declare (ignore result-type))
    (assert (not restp))
    (when (< (length args) (length parameters))
      (error "Too few arguments to ~S" identifier))
    (when (> (length args) (length parameters))
      (error "Too many arguments to ~S" identifier))
    (let* ((wrappers nil)
           (values-forms nil)
           (new-args
            (loop for ptype in parameters
                  for arg in args
                  collect (let ((arg arg) (ptype ptype))
                            (cond ((and (consp arg) (eq (car arg) ':out))
                                   (destructuring-bind (&optional (out-type (pointer-type-base ptype)))
                                       (cdr arg)
                                     (let ((g (gensym "OUT.")))
                                       (cond ((record-type-p out-type)
                                              ;; A kludge as our C-AREF doesn't copy on aggegrate types yet.
                                              (push (lambda (body)
                                                      `(let ((,g (c-make ',out-type)))
                                                         ,body))
                                                    wrappers))
                                             (t
                                              (push (lambda (body)
                                                      `(clet& ((,g ,out-type))
                                                         ,body))
                                                    wrappers)))
                                       (push `(c-aref ,g) values-forms)
                                       g)))
                                  (t
                                   arg))))))
      (let ((body `(static-c-funcall-2
                    ,function-type
                    ,identifier
                    ,@new-args)))
        (setq body
              `(values ,body
                       ,@values-forms))
        (dolist (w wrappers) (setq body (funcall w body)))
        body))))



(defun ctypep (object ctype &optional env)
  ;; ### What about (ctypep 0 '#_<void*>)
  ;; ### What about (ctypep (c-make '#_<char>) '#_<void*>)? Perhaps NIL, read ISO C.
  (typecase object
    (integer
     (and (integer-type-p ctype env)
          (<= (integer-type-min-value ctype env) object (integer-type-max-value ctype env))))
    (float
     (and (float-type-p ctype env)
          (typep object (float-type-lisp-type ctype env))))
    (cval
     (types-compatible-p (cval-type object) ctype env))
    (null
     ;; Null pointer
     (pointer-type-p ctype env))
    (t
     nil)))

'(AREF
 (:STRING-LITERAL
  "WMDM/DeviceFirmwareVersion"
  "L")
 (:INTEGER-CONSTANT :OCTAL 0 NIL))

(defun expr-type-of (expr &optional env)
  (select expr
    ((:string-literal) (text prefix) (declare (ignore text))
     (cond ((equal prefix "L") (make-pointer-type :short)) ;###
           ((null prefix) (make-pointer-type :char))
           (t (error "Unknown prefix"))))
    ((:integer-constant) (&rest ignore) (declare (ignore ignore))
     (nth-value 1 (comp-integer-constant expr env)))
    ((aref) (a b)
     (let ((a-type (expr-type-of a env))
           (b-type (expr-type-of b env)))
       (cond ((pointer-type-p a-type env) (pointer-type-base a-type env))
             ((pointer-type-p b-type env) (pointer-type-base b-type env))
             (t
              (error "Can't tell type of ~S" expr)))))))

(defun sizeof-expr (expr env)
  (cond ((typep expr '(cons (member :string-literal)))
         (destructuring-bind (text prefix) (cdr expr)
           (cond ((null prefix)
                  (1+ (length (de-escaped-c-string text))))
                 ((equal prefix "L")
                  (* 2 (1+ (length (de-escaped-c-string text)))))
                 (t
                  (error "Unknown string prefix ~S" prefix)))))
        ((typep expr '(cons (member :string-literal-list)))
         (1+ (loop for q in (cdr expr)
                   do (check-type q (cons (member :string-literal)))
                   sum (1- (sizeof-expr q env)))))
        (t
         (size-of-type (expr-type-of expr) env))))



;;;; -- Allegro CL ----------------------------------------------------------------------------

#+EXCL
(defun terminate-ptr (ptr)
  (format *trace-output* "freeing 0x~x~%" (car ptr))
  (ff:free-fobject (car ptr)))

#+EXCL
(defun c-make (type &optional (count 1) (clear t))
  "Allocates a GCable foreign object of type _type_ and returns a CVAL
   pointer to it. If _count_ is given a vector of that length is
   allocated. When _clear_ is true (the default) the memory is zeroed
   before being returned."
  (let* ((el-size (type-size-align type nil))
         (size (* count (/ el-size 8)))
         (sap  (ff:allocate-fobject :char :c size))
         (ptr  (cons-ptr sap 0 (make-pointer-type type)))
         (cookie (list sap)))
    (when clear
      (dotimes (i size)
        (setf (sap-peek-u8 sap i) 0)))
    (setf (ptr-cookie ptr) cookie)
    (excl:schedule-finalization cookie #'terminate-ptr)
    ptr))

#+EXCL
(defun make-gcable-c-string (string)
  ;; xxx
  (let* ((n (length string))
         (ptr (c-make ':char (1+ n) t)))
    (dotimes (i n)
      (setf (peek-u8 ptr i) (ldb (byte 8 0) (char-code (char string i)))))
    ptr))

#+EXCL
(defun make-gcable-c-utf-16-string (string)
  ;; xxx
  (let* ((n (length string))
         (ptr (c-make ':unsigned-short (1+ n) t)))
    (dotimes (i n)
      (setf (peek-u16 ptr (* 2 i)) (ldb (byte 16 0) (char-code (char string i)))))
    ptr))

#+EXCL
(defun get-c-string (ptr)
  (with-output-to-string (bag)
    (do* ((i 0 (+ i 1))
          (c (peek-u8 ptr i) (peek-u8 ptr i)))
         ((eql 0 c))
      (princ (code-char c) bag))))

#+EXCL
(defun cons-ptr (value offset type)
  (if (and (sap-null-ptr-p value) (eql offset 0))
      nil
      (%cons-ptr value offset type)))

#+EXCL
(defmacro defcfun ((name res-type) lambda-list &body body)
  ;; ### Box as a :FUNCTION!
  (let ()
    `(progn
       (ff:defun-foreign-callable ,name
           (,@(mapcar (lambda (parameter)
                        (destructuring-bind (var type)
                            parameter
                          (list var (foreign-type-for-funcall type))))
                      lambda-list))
         (declare (:returning ,(foreign-type-for-funcall res-type)))
         (unbox-to-native-object
          (let ,(mapcar (lambda (parameter)
                          (destructuring-bind (var type)
                              parameter
                            `(,var (box-native-object ,var ',type))))
                        lambda-list)

            ,@body)
          ',res-type))
       (defparameter ,name
         (int-ptr
          (ff:register-foreign-callable #',name)
          ',(make-pointer-type
             (make-function-type
              res-type
              (loop for (var type) in lambda-list
                    collect (make-declaration :name var :type type)))))))))

#+EXCL
(defun box-native-object (object type)
  (cond ((pointer-type-p type)
         (int-ptr object type))
        ((or (integer-type-p type) (float-type-p type))
         object)
        (t
         (c-coerce object type))))
