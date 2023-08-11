;; -*- Mode: Lisp; -*-
;; ---------------------------------------------------------------------------
;;     Title: AMD64 SYSV Calling Convention
;;   Created: 2023-05-18
;;    Author: Gilbert Baumann <gilbert@lazurite.local>
;;   License: MIT style (see below)
;; ---------------------------------------------------------------------------
;;;  (c) copyright 2023 by Gilbert Baumann

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

#+NOFFI-AMD64-SYSV-ABI (progn


;;;; -- AMD64 SYSV Calling Convention ---------------------------------------------------------

;;; In a Nutshell

;; tl;dr It's complicated.

;; Integer arguments are passed in RDI, RSI, RDX, RCX, R8, R9, while
;; floating point arguments are pass in XMM0..XMM7 from left to right.
;; Further arguments are pushed onto the stack.

;; Integer return values are placed RAX and RDX. Floating point return
;; values are in XMM0 and XMM1.

;; Aggregate types are passed and returned via registers when they don't
;; execeed 16-bytes in total size, all their and all members are aligned
;; and when there are enough registers left to pass the complete
;; aggregate. Whether to use integer or floating point registers is
;; decided by looking at the members of the aggregate. An eightbyte word
;; is passed in a floating point register, if all members occupying that
;; word are floating point values. Integer registers are used otherwise.

;; An aggregate that cannot be passed in registers, is spilled onto the
;; stack. For returning an aggregate that does not fit registers is
;; returned by copying to a buffer provided by the caller as an implicit
;; first pointer (integer) argument.

;; In addition for variadic functions, or functions without a prototype,
;; AL is set to the number of floating point registers (XMM0..XMM7) used.

;; CCL can do this fine. The CCL:%FF-CALL primitive has two pseudo
;; argument types. The first is the type being an integer, indicating
;; that this many words from the MACPTR following are pushed onto the
;; stack. For returning more than a single value, there is the
;; :REGISTERS pseudo type. The values of RAX, RDX, XMM0, XMM1 are then
;; place in a buffer provided by a MACPTR.

;; For SBCL the situation is not that nice, we need a trampoline.


;;; Common Template

#+NOFFI-AMD64-SYSV-ABI
(defun record-type-word-classes (record-type &optional env)
  "Computes the classes of all the words (eightbytes) in the record
type. Returns a vector of the classes. May also just return :MEMORY."
  (let* ((size (size-of-type record-type env)) ;###
         (classes (make-array (ceiling size 8) :initial-element nil)))
    ;; Recursively traverse the record layout and update the classes as
    ;; needed.
    (labels ((aux (type offset)
               (dolist (member-layout (record-type-layout type env))
                 ;; First check for natural alignment
                 (let ((member-type (member-layout-type member-layout))
                       (member-offset (+ offset (member-layout-bit-offset member-layout))))
                   (multiple-value-bind (member-size member-align) (type-size-align member-type env)
                     ;; Check alignment. The ABI says that when we face unaligned fields,
                     ;; we're class MEMORY. Further: When we encounter MEMORY, all is MEMORY.
                     (unless (zerop (mod member-offset member-align))
                       (return-from record-type-word-classes :memory))
                     ;;
                     (labels ((have (class)
                                ;; ###
                                (have-at (floor member-offset 64) class))
                              (have-at (word-index class)
                                (let ((old-class (elt classes word-index)))
                                  (setf (elt classes word-index) (merge-classes old-class class))))
                              (merge-classes (old new)
                                (cond ((eql old new) new)
                                      ((null old) new)
                                      ((null new) old)
                                      ((or (eql old :memory) (eql new :memory)) :memory)
                                      ((or (eql old :integer) (eql new :integer)) :integer)
                                      ((or (member old '(:X87 :X87UP :COMPLEX_X87))
                                           (member new '(:X87 :X87UP :COMPLEX_X87)))
                                       :memory)
                                      (t :sse))))
                       ;;
                       (cond ((integer-type-p member-type) (have :integer))
                             ((float-type-p member-type) (have :sse))
                             ((pointer-type-p member-type) (have :integer))
                             ((record-type-p member-type)
                              (aux member-type member-offset))
                             ((array-type-p member-type)
                              (nyi))
                             ((bit-field-type-p member-type)
                              (nyi))
                             (t
                              (error "Huh? We found a member typed ~S." member-type)))))))))
      ;;
      (aux record-type 0)
      (cond ((or
              (find :memory classes)
              (> (length classes) 2))
             :memory)
            (t classes)))))

#+NOFFI-AMD64-SYSV-ABI
(defmacro $add-arg (&key (arg-p t))
  ` (cond ((integer-type-p param-type)
           (add-integer-arg param-type ,@(and arg-p (list '(cval-value (c-coerce arg param-type)))))
           ,(and arg-p '(incf n-integer)))
          ((void-type-p param-type))
          ((float-type-p param-type)
           (add-sse-arg param-type ,@(and arg-p (list '(cval-value (c-coerce arg param-type)))))
           ,(and arg-p '(incf n-sse)))
          ((pointer-type-p param-type)
           (add-pointer-arg param-type ,@(and arg-p (list '(ptr-effective-sap (c-coerce arg param-type)))))
           ,(and arg-p '(incf n-integer)))
          ((array-type-p param-type)
           (setq param-type (make-pointer-type (array-type-base param-type)))
           (add-pointer-arg param-type ,@(and arg-p (list '(ptr-effective-sap (c-coerce arg param-type)))))
           ,(and arg-p '(incf n-integer)))
          #+(or)
          ((function-type-p param-type)
           (add-arg (make-pointer-type param-type) arg))
          ((and (consp param-type) (eq (car param-type) :registers)))
          ((record-type-p param-type)
           ,@(and arg-p
                  (list '(setq arg (%cast-ptr arg (make-pointer-type param-type)))))
           (cond ((> (size-of-type param-type) 16)
                  ;; Cannot do. Needs to be fixed once we support SSEUP and X87.
                  (add-memory-arg (size-of-type param-type) ,@(and arg-p (list 'arg))))
                 (t
                  (let ((classes (record-type-word-classes param-type)))
                    (cond ((or (eq classes :memory)
                               (find :memory classes)
                               (> (length classes) 2)
                               (> (+ (count :integer classes) n-integer) 6)
                               (> (+ (count :see classes) n-sse) 8))
                           (add-memory-arg (size-of-type param-type) ,@(and arg-p (list 'arg))))
                          (t
                           (loop for offset from 0 by 8
                                 for class across classes
                                 do (ecase class
                                      (:integer
                                       (add-integer-arg :unsigned-long-long ,@(and arg-p (list '(peek-u64 arg offset))))
                                       ,(and arg-p '(incf n-integer)))
                                      (:sse
                                       (add-sse-arg :double ,@(and arg-p (list '(peek-double arg offset))))
                                       ,(and arg-p '(incf n-sse)))))))))))
          (t
           (error "Don't know how to pass an argument of type ~S." param-type))))

#+NOFFI-AMD64-SYSV-ABI
(defmacro $c-funcall-guts ()
  ` (multiple-value-bind (res-type param-types restp)
        (function-type-signature fun-type)
      ($c-funcall-guts-2)) )

#+NOFFI-AMD64-SYSV-ABI
(defmacro $c-funcall-guts-2 ()
  ` (labels ((c-funcall-1 (res-type param-types restp args)
               (let ((n-integer 0)
                     (n-sse 0))
                 ;; Care for the result first, as it might already allocate an integer register.
                 (labels ((add-integer-arg (type)
                            (push (foreign-type-for-funcall type) ff-res))
                          (add-pointer-arg (type)
                            (push (foreign-type-for-funcall type) ff-res))
                          (add-sse-arg (type)
                            (push (foreign-type-for-funcall type) ff-res))
                          (add-memory-arg (nbytes)
                            (declare (ignore nbytes))
                            (error "How did that happen?")))
                   (let ((param-type res-type))
                     ($add-arg :arg-p nil)
                     (setq ff-res (reverse ff-res))))
                 ;; Now for the arguments
                 (labels ((add-arg (param-type arg)
                            ($add-arg)))
                   ;;
                   (do ((param.q param-types (cdr param.q))
                        (arg.q args (cdr arg.q)))
                       ((or (endp arg.q) (endp param.q))
                        (when param.q (error "Too few arguments."))
                        (when (and arg.q (not restp)) (error "Too many arguments."))
                        (loop for arg in arg.q do
                              (let ((arg (promoted-cval-for-funcall arg)))
                                (add-arg (cval-type arg) arg))))
                     (cond #+CCL
                           ((eq :registers (car param.q))
                            (push (car param.q) ff-args)
                            (push (car arg.q) ff-args))
                           (t
                            (add-arg (car param.q) (car arg.q)))))
                   ;;
                   (multiple-value-bind (r1 r2)
                       (do-ff-call res-type)
                     (values r1 r2)
                     )))))
      ;;
      (cond
        #+CCL
        ((and (consp res-type) (eq (car res-type) :registers))
         ($with-stack-allocated-sap ((buf 32))
           (let ((ioff 0) (foff 0))
             ($progn
              (c-funcall-1 :void (cons :registers param-types) restp (cons buf args))
              ($values (ecase (cadr res-type)
                         (:integer (incf ioff) ($sap-peek-u64 buf 0))
                         (:sse     (incf foff) ($sap-peek-u64 buf 16)))
                       (ecase (caddr res-type)
                         (:integer ($sap-peek-u64 buf (* 8 ioff)))
                         (:sse     ($sap-peek-u64 buf (+ 16 (* 8 foff))))))))))
        #+SBCL
        ((and (consp res-type) (eq (car res-type) :registers))
         (c-funcall-1 res-type param-types restp args))
        ((record-type-p res-type) (error "Foo!"))
        ;;
        ((pointer-type-p res-type)
         ($cons-ptr (c-funcall-1 res-type param-types restp args) 0 res-type))
        ((integer-type-p res-type)
         (cval-value (c-coerce (c-funcall-1 res-type param-types restp args) res-type)))
        ((float-type-p res-type)
         (cval-value (c-coerce (c-funcall-1 res-type param-types restp args) res-type)))
        ((void-type-p res-type)
         (c-funcall-1 res-type param-types restp args))
        (t
         (error "Don't know how to handle result type ~S" res-type)))) )

(defun function-and-signature-for-funcall (fun)
  (when (and (pointer-type-p (cval-type fun)) (function-type-p (pointer-type-base (cval-type fun))))
    (setq fun (c-aref fun)))
  (unless (function-type-p (cval-type fun))
    (error "~S is not a C function" fun))
  (multiple-value-bind (r ps r?)
      (function-type-signature (cval-type fun))
    (values (cval-function-sap fun) r ps r?)))

#+CCL
(defun cval-function-sap (fun)
  (ccl:%reference-external-entry-point (ptr-base-sap fun)))

#+SBCL
(defun cval-function-sap (fun)
  (ptr-effective-sap fun))

(defmacro at-runtime (&body body)
  `(macrolet (($progn (&body body) `(progn ,@body))
              ($values (&body vals) `(values ,@vals))
              ($let (bindings &body body) `(let ,bindings ,@body))
              ($multiple-value-bind (vars &body body) `(multiple-value-bind ,vars ,@body))
              ($with-stack-allocated-sap (bindings &body body) `(with-stack-allocated-sap ,bindings ,@body))
              ;;
              ($double-u64 (x) `(double-u64 ,x))
              ($c-make (&rest xs) `(c-make ,@xs))
              ($c-aref (&rest xs) `(c-aref ,@xs))
              ($sap-peek-u64 (&rest xs) `(sap-peek-u64 ,@xs))
              ($poke-u64 (nv sap off) `(setf (peek-u64 ,sap ,off) ,nv))
              ($poke-double (nv sap off) `(setf (peek-double ,sap ,off) ,nv))
              ($peek-u64 (&rest xs) `(peek-u64 ,@xs))
              ($cons-ptr (p o type) `(cons-ptr ,p ,o ,type)))
     ,@body))

(defmacro at-me-time (&body body)
  `(macrolet (($progn (&body body) ``(progn ,@(list ,@body)))
              ($values (&body vals) ``(values ,@(list ,@vals)))
              ($let (bindings &body body)
                (let ((bindings (mapcar (lambda (b) (cons (gensym) (if (atom b) (list b) b))) bindings)))
                  `(LET ,(mapcar (lambda (b) (list (cadr b) '(gensym))) bindings)
                     `(let ,(list ,@(mapcar (lambda (b) `(list ,(cadr b) ,(caddr b))) bindings))
                        ,@(list ,@body)))))
              ($multiple-value-bind (vars vals &body body)
                (let ((gensym-vars (mapcar (lambda (v) (list (gensym) v)) vars)))
                  `(LET ,(mapcar (lambda (b) (list (cadr b) '(gensym))) gensym-vars)
                     `(multiple-value-bind ,(list ,@(mapcar (lambda (b) (cadr b)) gensym-vars)) ,,vals
                        ,@(list ,@body)))))
              ($with-stack-allocated-sap (bindings &body body)
                (let ((bindings (mapcar (lambda (b) (cons (gensym) (if (atom b) (list b) b))) bindings)))
                  `(LET ,(mapcar (lambda (b) (list (cadr b) '(gensym))) bindings)
                     `(with-stack-allocated-sap ,(list ,@(mapcar (lambda (b) `(list ,(cadr b) ,(caddr b))) bindings))
                        ,@(list ,@body))))))
     (labels (($c-make (type &rest xs) `(c-make ',type ,@xs))
              ($c-aref (&rest xs) `(c-aref ,@xs))
              ($sap-peek-u64 (&rest xs) `(sap-peek-u64 ,@xs))
              ($poke-u64 (nv sap off) `(setf (peek-u64 ,sap ,off) ,nv))
              ($poke-double (nv sap off) `(setf (peek-double ,sap ,off) ,nv))
              ($peek-u64 (&rest xs) `(peek-u64 ,@xs))
              (cval-value (x) `(cval-value ,x))
              (c-coerce (x y) `(c-coerce ,x ',y))
              (ptr-effective-sap (x) `(ptr-effective-sap ,x))
              ($double-u64 (x) `(double-u64 ,x))
              ($cons-ptr (p o type) `(cons-ptr ,p ',o ',type))
              (promoted-cval-for-funcall (x) `(promoted-cval-for-funcall ,x))
              ;;
              (peek-u8 (ptr &optional (offset 0)) `(peek-u8 ,ptr ',offset))
              (peek-u16 (ptr &optional (offset 0)) `(peek-u16 ,ptr ',offset))
              (peek-u32 (ptr &optional (offset 0)) `(peek-u32 ,ptr ',offset))
              (peek-u64 (ptr &optional (offset 0)) `(peek-u64 ,ptr ',offset))
              (peek-double (ptr &optional (offset 0)) `(peek-double ,ptr ',offset))
              ;;
              (promoted-cval (x) `(promoted-cval ,x))
              (%cast-ptr (x type) `(%cast-ptr ,x ',type))
              ($ldb (x y) `(ldb ',x ,y)))
       ,@body)))

(defmacro $amd64-struct-ret-wrap (cont fun-sap res-type param-types restp args)
  `((lambda (fun-sap res-type param-types restp args)
      (cond ((record-type-p res-type)
             (let ((classes (record-type-word-classes res-type)))
               (cond ((eq :memory classes)
                      ($let ((res ($c-make res-type 1 nil)))
                        (,cont fun-sap :void
                               (cons (make-pointer-type res-type) param-types)
                               restp
                               (cons res args))
                        ($c-aref res)))
                     ((equalp '#(:integer) classes)
                      ($let ((res ($c-make res-type 1 nil)))
                        ($poke-u64 (,cont fun-sap :unsigned-long-long param-types restp args) res 0)
                        res))
                     ((equalp '#(:sse) classes)
                      ($let ((res ($c-make res-type 1 nil)))
                        ($poke-double (,cont fun-sap :double param-types restp args) res 0)
                        res))
                     ((= 2 (length classes))
                      ;; Now.
                      ($multiple-value-bind (r1 r2)
                        (,cont fun-sap `(:registers ,(elt classes 0) ,(elt classes 1)) param-types restp args)
                        ($let ((res ($c-make res-type 1 nil)))
                          ($poke-u64 r1 res 0)
                          ($poke-u64 r2 res 8)
                          res)))
                     (t
                      (error "Foo!")))))         
            (t
             (,cont fun-sap res-type param-types restp args))))
    ,fun-sap ,res-type ,param-types ,restp ,args))


;;;; -- CCL -----------------------------------------------------------------------------------

#+(AND CCL NOFFI-AMD64-SYSV-ABI)
(defun c-funcall (fun &rest args)
  (at-runtime
    (multiple-value-bind (fun-sap res-type param-types restp)
        (function-and-signature-for-funcall fun)
      (let ((ff-args nil) (ff-res nil))
        (labels ((add-pointer-arg (type arg)
                   (declare (ignore type))
                   (push :address ff-args)
                   (push arg ff-args))
                 (add-integer-arg (type arg)
                   (push (foreign-type-for-funcall type) ff-args)
                   (push arg ff-args))
                 (add-sse-arg (type arg)
                   (push (foreign-type-for-funcall type) ff-args)
                   (push arg ff-args))
                 (add-memory-arg (nbytes arg)
                   (push (ceiling nbytes 8) ff-args)
                   (push (ptr-effective-sap arg) ff-args))
                 ;;
                 (do-ff-call (res-type)
                   (declare (ignore res-type))
                   (ecase (length ff-res)
                     (0 (push :void ff-args))
                     (1 (push (car ff-res) ff-args)))
                   (apply #'ccl:%ff-call fun-sap (reverse ff-args)) ))
          (declare (inline add-integer-arg add-sse-arg add-memory-arg))
          ;;
          ($amd64-struct-ret-wrap
           (lambda (fun-sap res-type param-types restp args)
             ($c-funcall-guts-2))
           fun-sap res-type param-types restp args))))))

#+(AND CCL NOFFI-AMD64-SYSV-ABI)
(defmacro static-c-funcall-2 (fun-type identifier &rest args)
  (at-me-time
    (multiple-value-bind (res-type param-types restp)
        (function-type-signature fun-type)
      (let ((fun-sap `(CCL:%REFERENCE-EXTERNAL-ENTRY-POINT
                       (LOAD-TIME-VALUE (CCL:EXTERNAL ,(identifier-name identifier))))))
        ;;
        (let ((ff-args nil) (ff-res nil))
          (labels ((add-pointer-arg (type arg)
                     (declare (ignore type))
                     (push :address ff-args)
                     (push arg ff-args))
                   (add-integer-arg (type arg)
                     (push (foreign-type-for-funcall type) ff-args)
                     (push arg ff-args))
                   (add-sse-arg (type arg)
                     (push (foreign-type-for-funcall type) ff-args)
                     (push arg ff-args))
                   (add-memory-arg (nbytes arg)
                     (push (ceiling nbytes 8) ff-args)
                     (push (ptr-effective-sap arg) ff-args))
                   ;;
                   (do-ff-call (res-type)
                     (declare (ignore res-type))
                     (ecase (length ff-res)
                       (0 (push :void ff-args))
                       (1 (push (car ff-res) ff-args)))
                     `(ccl:%ff-call ,fun-sap ,@(reverse ff-args)) ))
            (declare (inline add-integer-arg add-sse-arg add-memory-arg))
            ;;
            ($amd64-struct-ret-wrap
             (lambda (fun-sap res-type param-types restp args)
               (declare (ignore fun-sap))
               ($c-funcall-guts-2))
             fun-sap res-type param-types restp args)))))))


;;;; -- SBCL ----------------------------------------------------------------------------------

;;; Trampoline

;; On AMD64 SYSV SBCL completely misses support for passing records by
;; value. And SBCL really cannot craft a foreign call in runtime.
;; SB-ALIEN:ALIEN-FUNCALL is only known by the comopiler and when
;; invoked at runtime it crafts a lambda with SB-ALIEN:ALIEN-FUNCALL,
;; compiles it and then invokes it. Therefore for those cases we go
;; through a trampoline of ours.

;; This is a trampoline to invoke foreign functions with the AMD64 ABI
;; with uncooperative Lisp implementation, most notably SBCL.

;; In C the prototype would look like

;; struct ff_call {
;;   /* Arguments of class INTEGER to be passed in RDI, RSI, RDX, RCX, R8, R9 */
;;   /* Upon return RAX and RDX are found here */
;;   uint64_t ireg[6];
;;   /* Arguments of class SSE to be passed in xmm0..xmm7 */
;;   /* Upon return xmm0 and xmm7 are deposited here */
;;   uint64_t freg[8];
;;   /* rax to pass. %al is the number of SSE arguments */
;;   uint64_t rax;
;;   /* number of 64-bit words to push onto the stack. Must be an even number. */
;;   uint64_t nmem;
;;   /* pointer to words to push onto the stack */
;;   uint64_t *mem;
;; };
;;
;; extern void trampoline (void *fun, struct ff_call *io);
;;

#+(AND SBCL NOFFI-AMD64-SYSV-ABI)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant %ffcall.ireg 0)
  (defconstant %ffcall.freg (* 8 6))
  (defconstant %ffcall.rax  (* 8 (+ 6 8)))
  (defconstant %ffcall.nmem (* 8 (+ 6 8 1)))
  (defconstant %ffcall.mem  (* 8 (+ 6 8 2)))
  (defconstant %ffcall-sz   (* 8 (+ 6 8 3))))

#+(AND SBCL NOFFI-AMD64-SYSV-ABI)
(defparameter *amd64-sysv-ff-call-trampoline*
  (coerce
   '(
     #|   0:|#  #x55                                 ; pushq    %rbp
     #|   1:|#  #x48 #x89 #xe5                       ; movq     %rsp, %rbp
     #|   4:|#  #x53                                 ; pushq    %rbx
     #|   5:|#  #x50                                 ; pushq    %rax
     #|   6:|#  #x48 #x89 #xf3                       ; movq     %rsi, %rbx
     #|   9:|#  #x4c #x8b #x53 #x78                  ; movq     120(%rbx), %r10
     #|   d:|#  #x49 #xc1 #xe2 #x03                  ; shlq     $3, %r10
     #|  11:|#  #x4c #x29 #xd4                       ; subq     %r10, %rsp
     #|  14:|#  #x48 #x8b #xb6 #x80 #x00 #x00 #x00   ; movq     128(%rsi), %rsi
     #|  1b:|#  #x48 #x31 #xc9                       ; xorq     %rcx, %rcx
     #|  1e:|#  #x49 #x39 #xca                       ; cmpq     %rcx, %r10
     #|  21:|#  #x74 #x0e                            ; je       14 <_ff_invoke+0x31>
     #|  23:|#  #x48 #x8b #x04 #x0e                  ; movq     (%rsi,%rcx), %rax
     #|  27:|#  #x48 #x89 #x04 #x0c                  ; movq     %rax, (%rsp,%rcx)
     #|  2b:|#  #x48 #x83 #xc1 #x08                  ; addq     $8, %rcx
     #|  2f:|#  #xeb #xed                            ; jmp      -19 <_ff_invoke+0x1e>
     #|  31:|#  #x49 #x89 #xfa                       ; movq     %rdi, %r10
     #|  34:|#  #x48 #x8b #x3b                       ; movq     (%rbx), %rdi
     #|  37:|#  #x48 #x8b #x73 #x08                  ; movq     8(%rbx), %rsi
     #|  3b:|#  #x48 #x8b #x53 #x10                  ; movq     16(%rbx), %rdx
     #|  3f:|#  #x48 #x8b #x4b #x18                  ; movq     24(%rbx), %rcx
     #|  43:|#  #x4c #x8b #x43 #x20                  ; movq     32(%rbx), %r8
     #|  47:|#  #x4c #x8b #x4b #x28                  ; movq     40(%rbx), %r9
     #|  4b:|#  #xf2 #x0f #x10 #x43 #x30             ; movsd    48(%rbx), %xmm0
     #|  50:|#  #xf2 #x0f #x10 #x4b #x38             ; movsd    56(%rbx), %xmm1
     #|  55:|#  #xf2 #x0f #x10 #x53 #x40             ; movsd    64(%rbx), %xmm2
     #|  5a:|#  #xf2 #x0f #x10 #x5b #x48             ; movsd    72(%rbx), %xmm3
     #|  5f:|#  #xf2 #x0f #x10 #x63 #x50             ; movsd    80(%rbx), %xmm4
     #|  64:|#  #xf2 #x0f #x10 #x6b #x58             ; movsd    88(%rbx), %xmm5
     #|  69:|#  #xf2 #x0f #x10 #x73 #x60             ; movsd    96(%rbx), %xmm6
     #|  6e:|#  #xf2 #x0f #x10 #x7b #x68             ; movsd    104(%rbx), %xmm7
     #|  73:|#  #x90 #x48 #x8b #x43 #x70             ; movq     112(%rbx), %rax
     #|  78:|#  #x41 #xff #xd2                       ; callq    *%r10
     #|  7b:|#  #x48 #x89 #x03                       ; movq     %rax, (%rbx)
     #|  7e:|#  #x48 #x89 #x53 #x08                  ; movq     %rdx, 8(%rbx)
     #|  82:|#  #xf2 #x0f #x11 #x43 #x30             ; movsd    %xmm0, 48(%rbx)
     #|  87:|#  #xf2 #x0f #x11 #x4b #x38             ; movsd    %xmm1, 56(%rbx)
     #|  8c:|#  #x48 #x8d #x65 #xf8                  ; leaq     -8(%rbp), %rsp
     #|  90:|#  #x5b                                 ; popq     %rbx
     #|  91:|#  #x5d                                 ; popq     %rbp
     #|  92:|#  #xc3                                 ; retq     
     ) '(simple-array (unsigned-byte 8) (*)) ))

#+(AND SBCL NOFFI-AMD64-SYSV-ABI)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant %ff-call-initial-mem-count 16))

#+(AND SBCL NOFFI-AMD64-SYSV-ABI)
(defun c-funcall (fun &rest args)
  (at-runtime
    (multiple-value-bind (fun-sap res-type param-types restp)
        (function-and-signature-for-funcall fun)
      (with-stack-allocated-sap ((io (+ %ffcall-sz (* 8 %ff-call-initial-mem-count))))
        (let ((mem (sb-sys:sap+ io %ffcall-sz)))
          (let (ff-res
                (n-int 0)
                (n-sse 0)
                (n-mem 0)
                (n-mem-phys %ff-call-initial-mem-count) ;actually allocated memory space
                (dyn-mem-alien nil)) ;ALIEN to dynamically allocated mem space, if any
            (declare (type fixnum n-int n-sse n-mem n-mem-phys))
            (labels ((add-pointer-arg (type arg)
                       (declare (ignore type))
                       (cond ((>= n-int 6)
                              (ensure-mem-room)
                              (setf (sap-peek-sap mem (+ (* 8 n-mem))) arg)
                              (incf n-mem))
                             (t
                              (setf (sap-peek-sap io (+ (* 8 n-int) %ffcall.ireg)) arg)
                              (incf n-int))))
                     (add-integer-arg (type arg)
                       (let ((arg (etypecase arg
                                    (integer (ldb (byte 64 0) arg))
                                    (real    (ldb (byte 64 0) (truncate arg)))
                                    (cval    (cval-value (c-coerce arg type))))))
                         (cond ((>= n-int 6)
                                (ensure-mem-room)
                                (setf (sap-peek-u64 mem (+ (* 8 n-mem)))
                                      (ldb (byte 64 0) arg))
                                (incf n-mem))
                               (t
                                (setf (sap-peek-u64 io (+ (* 8 n-int) %ffcall.ireg))
                                      (ldb (byte 64 0) arg))
                                (incf n-int)))))
                     ;;
                     (add-sse-arg (type arg)
                       (declare (ignore type))
                       (cond ((>= n-sse 6)
                              (ensure-mem-room)
                              (setf (sap-peek-double mem (+ (* 8 n-mem))) arg)
                              (incf n-mem))
                             (t
                              (setf (sap-peek-double io (+ (* 8 n-sse) %ffcall.freg)) arg)
                              (incf n-sse))))
                     ;;
                     (add-memory-arg (nbytes arg)
                       (ensure-mem-room)
                       (let ((src (ptr-effective-sap arg)))
                         (do ((i 0 (+ i 8)))
                             ((>= i nbytes))
                           (setf (sap-peek-u64 mem (+ (* 8 n-mem)))
                                 (sap-peek-u64 src i))
                           (incf n-mem))))
                     ;;
                     (ensure-mem-room ()
                       (when (= n-mem n-mem-phys)
                         (make-more-mem-room)))
                     ;;
                     (make-more-mem-room ()
                       (let* ((new-size (+ 4 n-mem-phys (floor n-mem-phys 2)))
                              (new (sb-alien:make-alien (* sb-alien:unsigned-char)
                                                        (* 8 new-size)))
                              (new-sap (sb-alien:alien-sap new)))
                         (dotimes (i n-mem)
                           (setf (sap-peek-u64 new-sap i) (sap-peek-u64 mem i)))
                         (when dyn-mem-alien (sb-alien:free-alien dyn-mem-alien))
                         (setf dyn-mem-alien new
                               n-mem-phys new-size
                               mem new-sap)))
                     ;;
                     (do-ff-call (res-type)
                       (setf (sap-peek-sap io %ffcall.mem) mem)
                       (setf (sap-peek-u64 io %ffcall.nmem) (* 2 (ceiling n-mem 2)))
                       (setf (sap-peek-u64 io %ffcall.rax) n-sse)
                       ;;
                       (multiple-value-prog1
                           (cond ((and (consp res-type) (eq (car res-type) :registers))
                                  (let ((trampo *amd64-sysv-ff-call-trampoline*))
                                    (sb-sys:with-pinned-objects (trampo)
                                      (sb-alien:alien-funcall
                                       (sb-alien:sap-alien (sb-sys:vector-sap trampo)
                                                           (function (values)
                                                                     sb-sys:system-area-pointer sb-sys:system-area-pointer))
                                       (ptr-effective-sap fun)
                                       io)))
                                  (let ((ioff 0) (foff 0))
                                    (values
                                     (ecase (cadr res-type)
                                       (:integer (incf ioff) (sap-peek-u64 io %ffcall.ireg))
                                       (:sse  (incf foff) (sap-peek-u64 io %ffcall.freg)))
                                     (ecase (caddr res-type)
                                       (:integer (sap-peek-u64 io (+ (* 8 ioff) %ffcall.ireg)))
                                       (:sse  (sap-peek-u64 io (+ (* 8 foff) %ffcall.freg)))))))
                                 ((float-type-p res-type)
                                  (let ((trampo *amd64-sysv-ff-call-trampoline*))
                                    (sb-sys:with-pinned-objects (trampo)
                                      (sb-alien:alien-funcall
                                       (sb-alien:sap-alien (sb-sys:vector-sap trampo)
                                                           (function double-float
                                                                     sb-sys:system-area-pointer sb-sys:system-area-pointer))
                                       (ptr-effective-sap fun)
                                       io))))
                                 ((pointer-type-p res-type)
                                  (let ((trampo *amd64-sysv-ff-call-trampoline*))
                                    (sb-sys:with-pinned-objects (trampo)
                                      (sb-alien:alien-funcall
                                       (sb-alien:sap-alien (sb-sys:vector-sap trampo)
                                                           (function sb-sys:system-area-pointer
                                                                     sb-sys:system-area-pointer sb-sys:system-area-pointer))
                                       (ptr-effective-sap fun)
                                       io))))
                                 (t
                                  (let ((trampo *amd64-sysv-ff-call-trampoline*))
                                    (sb-sys:with-pinned-objects (trampo)
                                      (sb-alien:alien-funcall
                                       (sb-alien:sap-alien (sb-sys:vector-sap trampo)
                                                           (function (sb-alien:unsigned 64)
                                                                     sb-sys:system-area-pointer sb-sys:system-area-pointer))
                                       (ptr-effective-sap fun)
                                       io)))))
                         (when dyn-mem-alien (sb-alien:free-alien dyn-mem-alien)))))
              (declare (inline add-integer-arg add-sse-arg add-memory-arg ensure-mem-room))
              ($amd64-struct-ret-wrap
               (lambda (fun-sap res-type param-types restp args)
                 ($c-funcall-guts-2))
               fun-sap res-type param-types restp args))))))))

#+(AND SBCL NOFFI-AMD64-SYSV-ABI)
(defmacro static-c-funcall-2 (fun-type identifier &rest args)
  (at-me-time
    (multiple-value-bind (res-type param-types restp)
        (function-type-signature fun-type)
      ;; First check, whether we can do that at all.
      (cond ((or restp
                 (and (record-type-p res-type)
                      (let ((classes (record-type-word-classes res-type)))
                        (not (member classes '(:memory #(:integer)) :test #'equalp)))))
             `(c-funcall (c-identifier ,identifier) ,@args))
            (t
             (let ((fun-sap 'huh?))
               ;;
               (let ((alien-types nil)
                     (alien-args nil)
                     (alien-mem-types nil)
                     (alien-mem-args nil)
                     (n-int 0)
                     (n-sse 0)
                     ff-res)
                 (labels ((add-integer-arg (type arg)
                            (cond ((>= n-int 6)
                                   (push (foreign-type-for-funcall type) alien-mem-types)
                                   (push arg alien-mem-args))
                                  (t
                                   (push (foreign-type-for-funcall type) alien-types)
                                   (push arg alien-args)
                                   (incf n-int))))
                          (add-pointer-arg (type arg)
                            (add-integer-arg type arg))
                          (add-sse-arg (type arg)
                            (cond ((>= n-sse 8)
                                   (push (foreign-type-for-funcall type) alien-mem-types)
                                   (push arg alien-mem-args))
                                  (t
                                   (push (foreign-type-for-funcall type) alien-types)
                                   (push arg alien-args)
                                   (incf n-sse))))
                          (add-memory-arg (nbytes arg)
                            (loop for i below (ceiling nbytes 8)
                                  do (progn
                                       (push (foreign-type-for-funcall :unsigned-long-long) alien-mem-types)
                                       (push (peek-u64 arg (* 8 i)) alien-mem-args))))
                          ;;
                          (do-ff-call (res-type)
                            ;; Pad the integere registers in case we have MEMORY arguments
                            (do () ((or (not alien-mem-args) (>= n-int 6)))
                              (push (foreign-type-for-funcall ':unsigned-long-long) alien-types)
                              (push 0 alien-args)
                              (incf n-int))
                            ;;
                            (cond ((and (consp res-type) (eq (car res-type) ':registers))
                                   (assert (eq (cadr res-type) (caddr res-type))) ;SBCL is confused here
                                   ($multiple-value-bind (r1 r2)
                                     `(SB-ALIEN:ALIEN-FUNCALL
                                       (SB-ALIEN:EXTERN-ALIEN ,(identifier-name identifier)
                                                              (FUNCTION
                                                               (VALUES
                                                                ,@(mapcar (lambda (x)
                                                                            (ecase x
                                                                              (:integer '(sb-alien:unsigned 64))
                                                                              (:sse     'sb-alien:double)))
                                                                          (cdr res-type)))
                                                               ,@(reverse alien-types)
                                                               ,@(reverse alien-mem-types)))
                                       ,@(reverse alien-args)
                                       ,@(reverse alien-mem-args)) 
                                     ($values
                                      (ecase (cadr res-type)
                                        (:integer r1)
                                        (:sse ($double-u64 r1)))
                                      (ecase (caddr res-type)
                                        (:integer r2)
                                        (:sse ($double-u64 r2))))))
                                  (t
                                   `(SB-ALIEN:ALIEN-FUNCALL
                                     (SB-ALIEN:EXTERN-ALIEN ,(identifier-name identifier)
                                                            (FUNCTION ,(foreign-type-for-funcall res-type)
                                                                      ,@(reverse alien-types)
                                                                      ,@(reverse alien-mem-types)))
                                     ,@(reverse alien-args)
                                     ,@(reverse alien-mem-args))))) )
                   (declare (inline add-integer-arg add-sse-arg add-memory-arg))
                   ($amd64-struct-ret-wrap
                    (lambda (fun-sap res-type param-types restp args)
                      (declare (ignore fun-sap))
                      ($c-funcall-guts-2))
                    fun-sap res-type param-types restp args)))))))))

) ; #+NOFFI-AMD64-SYSV-ABI progn
