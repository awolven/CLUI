;; -*- Mode: Lisp; -*-
;; ---------------------------------------------------------------------------
;;     Title: Compiler Warnings
;;   Created: 2012
;;    Author: Gilbert Baumann <gilbert@bauhh.de>
;;   License: MIT style (see below)
;; ---------------------------------------------------------------------------
;;;  (c) copyright 2012-2021 by Gilbert Baumann

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

(defpackage :de.bauhh.compiler-warn
  (:use :common-lisp)
  (:export #:compiler-warn
           #:compiler-error
           #:compiler-descend))

(in-package :de.bauhh.compiler-warn)

;;;; -- Compiler Warnings -----------------------------------------------------

(defvar *compiler-context-chain* nil)

(defun compiler-warn (form format &rest args)
  "Issue a warning about the form /form/. When possible implementation
specific means are used to point the finger at the form, so that eventually
SLIME or whatever IDE could highlight the context. When not available or not
within the compiler, a normal warning is issued.

Obviously this does not work if /form/ is an atom.

To narrow down the context you could setup a next best context with
COMPILER-DESCEND."
  ;; Any adaption to other Common Lisp implementations is welcome.
  #+CCL
  (let ((context (some #'ccl::nx-source-note (cons form *compiler-context-chain*))))
    (if context
        (let ((ccl::*nx-current-note* context))
          (ccl::nx1-whine :program-error (apply #'format nil format args)))
        (warn "~? --- ~S" format args form)))
  #-CCL
  (warn "~? --- ~S" format args form))

(defun compiler-error (form format &rest args)
  #+CCL
  (let ((context (some #'ccl::nx-source-note (cons form *compiler-context-chain*))))
    (if context
        (let ((ccl::*nx-current-note* context))
          (apply #'ccl::nx-error format args))
        (error "~? --- ~S" format args form)))
  #-CCL
  (error "~? --- ~S" format args form))

(defmacro compiler-descend (form &body body)
  (let ((g.form (gensym "FORM.")))
    `(let* ((,g.form ,form)
            (*compiler-context-chain*
             (if (consp ,g.form)
                 (cons ,g.form *compiler-context-chain*)
                 *compiler-context-chain*)))
       ,@body)))
