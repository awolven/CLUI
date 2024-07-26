;; -*- Mode: Lisp; -*-
;; ---------------------------------------------------------------------------
;;     Title: noffi backend for Allegro Common Lisp
;;   Created: 2024-06-17
;;    Author: Gilbert Baumann <gilbert@bauhh.de>
;;   License: MIT style (see below)
;; ---------------------------------------------------------------------------
;;  (c) copyright 2024 by Gilbert Baumann

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


;;;; Notes

;; It appears like we cannot really make FOBJECT's that point to some
;; non-aligned address. Also it appears there is no way to make such a
;; pointer.


;; Our SAPs are just integers.

(defmacro sap-peek-u8  (sap &optional (offset 0))    `(ff:fslot-value-typed ':unsigned-char :c (+ ,sap ,offset)))
(defmacro sap-peek-u16 (sap &optional (offset 0))    `(ff:fslot-value-typed ':unsigned-short :c (+ ,sap ,offset)))
(defmacro sap-peek-u32 (sap &optional (offset 0))    `(ff:fslot-value-typed ':unsigned-int :c (+ ,sap ,offset)))
(defmacro sap-peek-u64 (sap &optional (offset 0))    `(ff:fslot-value-typed ':unsigned-long-long :c (+ ,sap ,offset)))
(defmacro sap-peek-s8  (sap &optional (offset 0))    `(ff:fslot-value-typed ':char :c (+ ,sap ,offset)))
(defmacro sap-peek-s16 (sap &optional (offset 0))    `(ff:fslot-value-typed ':short :c (+ ,sap ,offset)))
(defmacro sap-peek-s32 (sap &optional (offset 0))    `(ff:fslot-value-typed ':int :c (+ ,sap ,offset)))
(defmacro sap-peek-s64 (sap &optional (offset 0))    `(ff:fslot-value-typed ':long-long :c (+ ,sap ,offset)))
(defmacro sap-peek-float (sap &optional (offset 0))  `(ff:fslot-value-typed ':float :c (+ ,sap ,offset)))
(defmacro sap-peek-double (sap &optional (offset 0)) `(ff:fslot-value-typed ':double :c (+ ,sap ,offset)))
(defmacro sap-peek-sap (sap &optional (offset 0))    `(ff:fslot-value-typed ':nat :c (+ ,sap ,offset)))
(defmacro sap-int (sap)                              sap)
(defmacro int-sap (int)                              int)
(defmacro sap-plus (sap delta)                       `(+ ,sap ,delta))
(define-symbol-macro +null-ptr-sap+                  0)
(defmacro sap-null-ptr-p (sap)                       `(eql 0 ,sap))

(defmacro with-stack-allocated-sap ((&rest sap-size*) &body body &environment env)
  (let ((gs (loop for nil in sap-size* collect (gensym))))
    `(ff:with-static-fobjects
         ,(mapcar (lambda (binding g)
                    (destructuring-bind (sap-var size-form) binding
                      (declare (ignore sap-var))
                      (if (constantp size-form)
                          `(,g '(:ARRAY :CHAR ,size-form) :ALLOCATION :FOREIGN-STATIC-GC)
                          `(,g :CHAR :SIZE ,size-form :ALLOCATION :FOREIGN-STATIC-GC))))
                  sap-size* gs)
       (let ,(mapcar (lambda (binding g)
                       (destructuring-bind (sap-var size-form) binding
                         (declare (ignore size-form))
                         `(,sap-var (ff:fslot-address ,g))))
                     sap-size* gs)
         ,@body))))

(defun sap-malloc (size)
  (ff:allocate-fobject :char :c size))

(defun sap-free (sap)
  (ff:free-fobject sap))


;; The PTR datatype needs a cookie

#||
(ff:def-foreign-call (%ff-call-nat "noffi_amd64_ms_trampoline_ull")
    ((fun :nat) (n-stack-words :nat) (stack :nat))
  :returning :nat)

(ff:def-foreign-call (%ff-call-double "noffi_amd64_ms_trampoline_double")
    ((fun :nat) (n-stack-words :nat) (stack :nat))
  :returning :double)

(ff:def-foreign-call (%ff-call-float "noffi_amd64_ms_trampoline_double")
    ((fun :nat) (n-stack-words :nat) (stack :nat))
  :returning :float)

(load "noffi:support;amd64-mswin-trampoline.dll")
||#

;;

(defmacro ff-call (name &rest args)
  (expand-ff-call name args))

(defun make-def-foreign-call-form (lisp-name c-name args &rest options)
  ;; def-foreign-call-form
  (multiple-value-bind (ret-type args)
      (values (car (last args)) (butlast args))
    (let* ((args
            (loop for (type value) on args by #'cddr
                  collect (list (gensym) type value))))
      `(ff:def-foreign-call (,lisp-name ,c-name)
           ,(if (null args)
                '(:void)
                (loop for (parameter type) in args
                      collect `(,parameter ,type)))
         :returning ,ret-type
         ,@options))))

(defun expand-ff-call (name args)
  (multiple-value-bind (ret-type args)
      (values (car (last args)) (butlast args))
    (let* ((args
            (loop for (type value) on args by #'cddr
                  collect (list (gensym) type value)))
           (fun-sym (gensym))
           (def-foreign-call
            `(ff:def-foreign-call (,fun-sym ,name)
                 ,(if (null args)
                      '(:void)
                      (loop for (parameter type) in args
                            collect `(,parameter ,type)))
               :returning ,ret-type
               :arg-checking t
               :strings-convert t
               :call-direct nil))
           (expanded
            (handler-case
                (macroexpand-1 def-foreign-call)
              (error (c)
                (error "~@<We were blamed trying to expand ~_~S. ~_They say:~:>~%~A"
                       def-foreign-call (princ-to-string c)))))
           (lambda-form
            (block steal
              (subst-if nil (lambda (x)
                              (when (typep x `(cons (member excl:named-annotated-function)
                                                    (cons (member ,fun-sym)
                                                          (cons (cons (member LAMBDA))
                                                                t)
                                                          null)))
                                (return-from steal (third x))))
                        expanded)
              (set'/e expanded)
              (error "We lost. We cannot identify the actual function definition for~%~S~%which gave us:~%~S~%"
                     def-foreign-call expanded))))
      ;;
      `(,lambda-form
        ,@(mapcar #'third args)))))

(defun foreign-type-for-funcall (type &optional env)
  (cond ((integer-type-p type env)
         (if (integer-type-signed-p type env)
             (ecase (integer-type-size type env)
               (8 :char)
               (16 :short)
               (32 :int)
               (64 :long-long))
             (ecase (integer-type-size type env)
               (8 :unsigned-char)
               (16 :unsigned-short)
               (32 :unsigned-int)
               (64 :unsigned-long-long))))
        ((float-type-p type env)
         (ecase (bare-expanded-type type env)
           (:float :single)
           (:double :double)))
        ((pointer-type-p type env)
         :unsigned-nat)
        ((void-type-p type env)
         :void)
        (t
         (error "What's the foreign type to use for CCL:FF-CALL given noffi type ~S?" type))))

;;;

(defun load-foreign-library (which)
  (etypecase which
    (pathname
     (load which))
    (string
     ;; We just try this 
     (handler-case
         (load (concatenate 'string which ".dll"))
       (error ()
         (load which))))))

(defun use-library (x &key search-path)
  (load-foreign-library x))

(defun extern-addr (name type)
  (let ((p (ff:get-entry-point (string name))))
    (unless p
      (error "Entry point ~S cannot be found." name))
    (cons-ptr p 0 type)))


;;;; -- wip -----------------------------------------------------------------------------------

(defun trampoline-ep ()
  (let ((ep (excl::make-entry-vec-boa))
        (buf (sap-malloc (length +amd64-ms-trampoline+))))
    (loop for i from 0
          for x across +amd64-ms-trampoline+
          do (setf (sap-peek-u8 buf i) x))
    (setf (aref ep 1) buf)
    (setf (aref ep 3) 2)
    (setf (aref ep 5) 24)
    ep))

(defun %ff-call-nat (fun n-stack-words stack)
  (SYSTEM::FF_FUNCALL (trampoline-ep)
                      '(:NAT (INTEGER -9223372036854775808 9223372036854775807))
                      FUN
                      '(:NAT (INTEGER -9223372036854775808 9223372036854775807))
                      N-STACK-WORDS
                      '(:NAT (INTEGER -9223372036854775808 9223372036854775807))
                      STACK
                      '(:NAT (INTEGER -9223372036854775808 9223372036854775807))))

(defun %ff-call-double (fun n-stack-words stack)
  (SYSTEM::FF_FUNCALL (trampoline-ep)
                      '(:NAT (INTEGER -9223372036854775808 9223372036854775807))
                      FUN
                      '(:NAT (INTEGER -9223372036854775808 9223372036854775807))
                      N-STACK-WORDS
                      '(:NAT (INTEGER -9223372036854775808 9223372036854775807))
                      STACK
                      '(:DOUBLE (DOUBLE-FLOAT * *))))

(defun %ff-call-float (fun n-stack-words stack)
  (SYSTEM::FF_FUNCALL (trampoline-ep)
                      '(:NAT (INTEGER -9223372036854775808 9223372036854775807))
                      FUN
                      '(:NAT (INTEGER -9223372036854775808 9223372036854775807))
                      N-STACK-WORDS
                      '(:NAT (INTEGER -9223372036854775808 9223372036854775807))
                      STACK
                      '(:float (single-FLOAT * *))))
