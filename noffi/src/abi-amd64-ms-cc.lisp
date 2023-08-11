;; -*- Mode: Lisp; -*-
;; ---------------------------------------------------------------------------
;;     Title: AMD64 Microsoft Calling Convention
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

#+NOFFI-AMD64-MS-ABI (progn

;;;; -- Overview ------------------------------------------------------------------------------

;;; Summary

;; Integer arguments are passed in rcx, rdx, r8, r9 and floating point
;; arguments are passed in xmm0..xmm3. Unlike with the SYSV ABI each
;; argument consumes both registers. That is when an integer was passed
;; in rcx, a floating point argument would be passed in xmm1. Variadic
;; functions and functions without a prototype have their arguments
;; passed in _both_ the floating point and integer register.

;; Records not of size 8, 16, 32, or 64 bits are passed by reference.

;; Integer types are returned with RAX, floating point values in XMM0.
;; Unused bits in the return registers are left unspecified.

;; Records of size 8, 16, 32, or 64 bits are returned with RAX.

;; Other record types are returned by passing a pointer to the result as
;; first argument.


;;;; -- Common --------------------------------------------------------------------------------

(defmacro $amd64-ms-funcall-template ()
  ` (let ((to-free nil))
      (when (and (pointer-type-p (cval-type fun)) (function-type-p (pointer-type-base (cval-type fun))))
        (setq fun (c-aref fun)))
      (unless (function-type-p (cval-type fun))
        (error "~S is not a C function" fun))
      (multiple-value-bind (rtype ptypes restp)
          (function-type-signature (cval-type fun))
        ($amd64-ms-funcall-template-2))) )

;; cval-value
;; c-coerce
;; ptr-effective-sap
;; peek-u8, ...
;; promoted-cval
;; cons-ptr

;; add-int-arg type value
;; add-double-arg type value
;; add-ptr-arg type value
;; do-call type
;; make-res type
;; deposit-res accessor value
;; get-res

(defmacro $amd64-ms-funcall-template-2 ()
  ` (labels ((process-arg (arg parameter-type)
               (when (array-type-p parameter-type)
                 (setq parameter-type (make-pointer-type (array-type-base parameter-type))))
               (setq arg (promoted-cval arg))
               (cond ((integer-type-p parameter-type)
                      (add-int-arg parameter-type (cval-value (c-coerce arg :unsigned-long-long))))
                     ((float-type-p parameter-type)
                      (add-double-arg parameter-type (cval-value (c-coerce arg :double))))
                     ((pointer-type-p parameter-type)
                      (add-ptr-arg parameter-type
                               (ptr-effective-sap (c-coerce arg parameter-type))))
                     ((record-type-p parameter-type)
                      (let ((size (type-size-align parameter-type nil)))
                        (case size
                          ( 8 (add-int-arg :unsigned-char       (peek-u8 arg)))
                          (16 (add-int-arg :unsigned-short      (peek-u16 arg)))
                          (32 (add-int-arg :unsigned-int        (peek-u32 arg)))
                          (64 (add-int-arg :unsigned-long-long  (peek-u64 arg)))
                          (t
                           ;; For now ... We rather craft a copy. Which sucks.
                           (add-ptr-arg '(:pointer :void) (ptr-effective-sap arg)) ))))
                     (t
                      (error "Don't know how to pass a parameter of type ~S" parameter-type)))) )
      ;;
      (when (record-type-p rtype)
        (make-res rtype)
        (unless (member (type-size-align rtype nil) '(8 16 32 64))
          (add-ptr-arg (make-pointer-type rtype) (ptr-effective-sap (get-res)))))
      ;;
      (do ((a args (cdr a))
           (p ptypes (cdr p)))
          ((or (null a) (null p))
           (when (and (null a) p)
             (error "Too few arguments"))
           (when (and (null p) a (not restp))
             (error "Too many arguments"))
           ;; Rest args
           (dolist (arg a)
             (cond ((cvalp arg)       (process-arg arg (cval-type arg)))
                   ((integerp arg)    (process-arg arg ':int))
                   ((floatp arg)      (process-arg arg ':double))
                   (t
                    (error "Cannot handle ~S as an argument to a variadic function." arg)))))
        ;; Required arg
        (process-arg (car a) (car p)))
      ;;
      (prog1
          (cond ((pointer-type-p rtype)
                 (cons-ptr (do-call rtype) 0 rtype))
                ((arithmetic-type-p rtype)
                 (c-coerce (do-call rtype) rtype))
                ((void-type-p rtype)
                 (do-call rtype))
                ((record-type-p rtype)
                 ($progn
                  (let ((size (type-size-align rtype nil)))
                    (case size
                      ( 8 (deposit-res peek-u8  ($ldb (byte  8 0) (do-call :unsigned-long-long))))
                      (16 (deposit-res peek-u16 ($ldb (byte 16 0) (do-call :unsigned-long-long))))
                      (32 (deposit-res peek-u32 ($ldb (byte 32 0) (do-call :unsigned-long-long))))
                      (64 (deposit-res peek-u64 ($ldb (byte 64 0) (do-call :unsigned-long-long))))
                      (t  (do-call :void))))
                  (get-res)))
                (t
                 (error "No idea what to do with function return type ~S" rtype)))
        '(dolist (sap to-free)
          (sap-free sap)))))


;;;; -- CCL -----------------------------------------------------------------------------------

#+(AND CCL NOFFI-AMD64-MS-ABI)
(defun c-funcall (fun &rest args)
  (let (#+(or) to-free ff-call-args res)
    (labels ((add-int-arg (type val)
               (declare (ignore type))
               (push :unsigned-doubleword ff-call-args)
               (push val ff-call-args))
             (add-double-arg (type val)
               (declare (ignore type))
               (push :double-float ff-call-args)
               (push val ff-call-args))
             (add-ptr-arg (type val)
               (declare (ignore type))
               (push :address ff-call-args)
               (push val ff-call-args))
             (do-call (rtype)
               (let ((ff-fun (let ((sap (ptr-base-sap fun)))
                               (if (ccl::external-entry-point-p sap)
                                   (ccl:%reference-external-entry-point sap)
                                   (ccl:%ptr-to-int sap)))))
                 (push (foreign-type-for-funcall rtype) ff-call-args)
                 (apply #'ccl:%ff-call ff-fun (reverse ff-call-args))))
             (make-res (type)
               (setq res (c-make type 1 nil)))
             (get-res ()
               res))
      (macrolet ((deposit-res (accessor value)
                   `(setf (,accessor res 0) ,value))
                 ($progn (&body body) `(progn ,@body))
                 ($ldb (&rest x) `(ldb ,@x)))
        ($amd64-ms-funcall-template)))))

#+(AND CCL NOFFI-AMD64-MS-ABI)
(defmacro static-c-funcall-2 (fun-type identifier &rest args)
  (multiple-value-bind (rtype ptypes restp)
      (function-type-signature fun-type)
    (let (#+(or) to-free ff-call-args
            (fun :bogus)
            (res (gensym "RES."))
            (res-init nil))
      (labels ((cval-value (x) `(cval-value ,x))
               (c-coerce (x y) `(c-coerce ,x ',y))
               (ptr-effective-sap (x) `(ptr-effective-sap ,x))
               (peek-u8 (&rest xs) `(peek-u8 ,@xs))
               (peek-u16 (&rest xs) `(peek-u16 ,@xs))
               (peek-u32 (&rest xs) `(peek-u32 ,@xs))
               (peek-u64 (&rest xs) `(peek-u64 ,@xs))
               (promoted-cval (x) `(promoted-cval ,x))
               (cons-ptr (x y z) `(cons-ptr ,x ,y ',z))
               ($ldb (x y) `(ldb ',x ,y)))
        (labels ((add-int-arg (type val)
                   (declare (ignore type))
                   (push :unsigned-doubleword ff-call-args)
                   (push val ff-call-args))
                 (add-double-arg (type val)
                   (declare (ignore type))
                   (push :double-float ff-call-args)
                   (push val ff-call-args))
                 (add-ptr-arg (type val)
                   (declare (ignore type))
                   (push :address ff-call-args)
                   (push val ff-call-args))
                 (do-call (rtype)
                   `(ccl:%ff-call (ccl:%reference-external-entry-point
                                   (load-time-value
                                    (ccl:external ',(identifier-name identifier))))
                                  ,@(reverse ff-call-args)
                                  ,(foreign-type-for-funcall rtype)))
                 (make-res (type)
                   (setq res-init `(c-make ',type 1 nil)))
                 (get-res ()
                   res)
                 (deposit-res-1 (accessor value)
                   `(setf (,accessor ,res 0) ,value))
                 ($progn (&rest body)
                   `(progn ,@body)))
          (macrolet ((deposit-res (accessor value)
                       `(deposit-res-1 ',accessor ,value)))
            (let ((body ($amd64-ms-funcall-template-2)))
              (when res-init
                (setq body `(let ((,res ,res-init)) ,body)))
              body)))))))


;;;; -- SBCL ----------------------------------------------------------------------------------

#+(AND SBCL NOFFI-AMD64-MS-ABI)
(defparameter +amd64-ms-trampoline+
  (coerce 
   '(
     #|   0:|#  #x55                       ; push   %rbp 
     #|   1:|#  #x48 #x89 #xe5             ; mov    %rsp,%rbp 
     #|   4:|#  #x48 #x83 #xfa #x00        ; cmp    $0x0,%rdx 
     #|   8:|#  #x74 #x0e                  ; je     18 <.text+0x18> 
     #|   a:|#  #x48 #xff #xca             ; dec    %rdx 
     #|   d:|#  #x49 #x8b #x04 #xd0        ; mov    (%r8,%rdx,8),%rax 
     #|  11:|#  #x50                       ; push   %rax 
     #|  12:|#  #x48 #x83 #xfa #x00        ; cmp    $0x0,%rdx 
     #|  16:|#  #x75 #xf2                  ; jne    a <.text+0xa> 
     #|  18:|#  #x48 #x89 #xc8             ; mov    %rcx,%rax 
     #|  1b:|#  #x48 #x8b #x0c #x24        ; mov    (%rsp),%rcx 
     #|  1f:|#  #x48 #x8b #x54 #x24 #x08   ; mov    0x8(%rsp),%rdx 
     #|  24:|#  #x4c #x8b #x44 #x24 #x10   ; mov    0x10(%rsp),%r8 
     #|  29:|#  #x4c #x8b #x4c #x24 #x18   ; mov    0x18(%rsp),%r9 
     #|  2e:|#  #x66 #x48 #x0f #x6e #xc1   ; movq   %rcx,%xmm0 
     #|  33:|#  #x66 #x48 #x0f #x6e #xca   ; movq   %rdx,%xmm1 
     #|  38:|#  #x66 #x49 #x0f #x6e #xd0   ; movq   %r8,%xmm2 
     #|  3d:|#  #x66 #x49 #x0f #x6e #xd9   ; movq   %r9,%xmm3 
     #|  42:|#  #xff #xd0                  ; call   *%rax 
     #|  44:|#  #x48 #x89 #xec             ; mov    %rbp,%rsp 
     #|  47:|#  #x5d                       ; pop    %rbp 
     #|  48:|#  #xc3                       ; ret      
     )
   '(simple-array (unsigned-byte 8) (*))))

#+(AND SBCL X86-64 WIN32)
(defun amd-64-ms-ff-call (fun count io-sap)
  (let ((trampo +amd64-ms-trampoline+))
    (sb-sys:with-pinned-objects (trampo)
      (sb-alien:alien-funcall
       (sb-alien:sap-alien (sb-sys:vector-sap trampo)
                           (function (values (sb-alien:unsigned 64))
                                     sb-sys:system-area-pointer
                                     (sb-alien:unsigned 64)
                                     sb-sys:system-area-pointer))
       fun count io-sap))))

#+(AND SBCL NOFFI-AMD64-MS-ABI)
(defun c-funcall (fun &rest args)
  (let ((nargs (length args)) (io-off 0)
        the-res)
    (with-stack-allocated-sap ((io (* 16 (ceiling (1+ nargs) 2))))
      (labels ((add-int-arg (type val)
                 (declare (ignore type))
                 (setf (sap-peek-u64 io io-off) (ldb (byte 64 0) val))
                 (incf io-off 8))
               (add-double-arg (type val)
                 (declare (ignore type))
                 (setf (sap-peek-double io io-off) val)
                 (incf io-off 8))
               (add-ptr-arg (type val)
                 (declare (ignore type))
                 (setf (sap-peek-sap io io-off) val)
                 (incf io-off 8))
               (do-call (rtype)
                 (declare (ignore rtype))
                 (amd-64-ms-ff-call (ptr-effective-sap fun) (* 2 (ceiling (1+ nargs))) io))
               (make-res (type)
                 (setq the-res (c-make type 1 nil)))
               (get-res ()
                 the-res))
        (macrolet ((deposit-res (accessor value)
                     `(setf (,accessor the-res 0) ,value))
                   ($progn (&body body) `(progn ,@body))
                   ($ldb (&rest x) `(ldb ,@x)))
          ($amd64-ms-funcall-template))))) )

#+(AND SBCL NOFFI-AMD64-MS-ABI)
(defmacro static-c-funcall-2 (fun-type identifier &rest args)
  (let ((alien-args nil) (alien-parameter-types nil) to-free
        (res (gensym "RES."))
        res-init)
    (labels ((cval-value (x) `(cval-value ,x))
             (c-coerce (x y) `(c-coerce ,x ',y))
             (ptr-effective-sap (x) `(ptr-effective-sap ,x))
             (peek-u8 (&rest xs) `(peek-u8 ,@xs))
             (peek-u16 (&rest xs) `(peek-u16 ,@xs))
             (peek-u32 (&rest xs) `(peek-u32 ,@xs))
             (peek-u64 (&rest xs) `(peek-u64 ,@xs))
             (promoted-cval (x) `(promoted-cval ,x))
             (cons-ptr (x y z) `(cons-ptr ,x ,y ',z))
             ($ldb (x y) `(ldb ',x ,y)))
      (labels ((add-int-arg (type arg)
                 (declare (ignore type))
                 (push '(sb-alien:unsigned 64) alien-parameter-types)
                 (push arg alien-args))
               (add-double-arg (type arg)
                 (declare (ignore type))
                 (push 'double-float alien-parameter-types)
                 (push arg alien-args))
               (add-ptr-arg (type arg)
                 (declare (ignore type))
                 (push 'sb-sys:system-area-pointer alien-parameter-types)
                 (push arg alien-args))
               (do-call (rtype)
                 (let ((alien-type
                        `(function ,(foreign-type-for-funcall rtype)
                                   ,@(reverse alien-parameter-types))))
                   `(sb-alien:alien-funcall 
                     (sb-alien:extern-alien ,(identifier-name identifier) ,alien-type)
                     ,@(reverse alien-args))))
               (make-res (type)
                 (setq res-init `(c-make ',type 1 nil)))
               (get-res ()
                 res)
               (deposit-res-1 (accessor value)
                 `(setf (,accessor ,res 0) ,value))
               ($progn (&rest body)
                 `(progn ,@body)))
        (macrolet ((deposit-res (accessor value)
                     `(deposit-res-1 ',accessor ,value)))
          (multiple-value-bind (rtype ptypes restp)
              (function-type-signature fun-type)
            (let ((body ($amd64-ms-funcall-template-2)))
              (when res-init
                (setq body `(let ((,res ,res-init)) ,body)))
              body)))))))

) ;#+NOFFI-AMD64-SYSV-ABI progn
