;; -*- Mode: Lisp; -*-
;; ---------------------------------------------------------------------------
;;     Title: Lisp implementation dependent functions and patches
;;   Created: 2024-03-27
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


;;;; -- Overview ------------------------------------------------------------------------------

;; +UTF-8+
;; +ISO-8859-1+
;; +ISO-8859-1/CRNL+
;; +UTF-8/CRNL+

;; MAKE-HASH-TABLE &key ... weak ...
;; RUN-PROGRAM
;; EXIT-STATUS
;; POSIX-GETENV

;; NATIVE-NAMESTRING pathname
;; NATIVE-PATHNAME
;; NATIVE-DIRECTORY-PATHNAME
;; MAKE-LOCK &optional name
;; WITH-LOCK-HELD (place) &body body


;;;; External Formats
#+CCL
(progn
  (defconstant +utf-8+ :utf-8)
  (defconstant +iso-8859-1+ :iso-8859-1)
  (define-symbol-macro +iso-8859-1/crnl+
      (ccl:make-external-format :character-encoding :iso-8859-1 :line-termination :crlf))
  (define-symbol-macro +utf-8/crnl+
      (ccl:make-external-format :character-encoding :utf-8 :line-termination :crlf)))

#+SBCL
(progn
  (defconstant +utf-8+ :utf-8)
  (defconstant +iso-8859-1+ :iso-8859-1)
  (define-symbol-macro +iso-8859-1/crnl+
    ;; No luck
      :iso-8859-1)
  (define-symbol-macro +utf-8/crnl+
      :utf-8))


;;;; Weak Hash Tables

#+(OR CCL SBCL ECL CLISP ABCL)
(defun make-hash-table (&rest args &key weak &allow-other-keys)
  (remf args :weak)
  #+SBCL
  (progn
    ;; Now with SBCL again this is problematic. It says "This keyword
    ;; argument is experimental, and may change incompatibly or be removed
    ;; in the future." What to do? Have a hash of hashtables mapping to
    ;; locks and patch GETHASH?
    (setq args (list* :synchronized t args)))
  (ecase weak
    ((nil) (apply #'cl:make-hash-table args))
    ((:value)
     #+(OR CCL CLISP)
     (apply #'cl:make-hash-table :weak weak args)
     #+(OR ECL SBCL ABCL)
     (apply #'cl:make-hash-table :weakness weak args))
    ((:key)
     #+(OR CCL CLISP)
     (apply #'cl:make-hash-table :weak weak args)
     #+(OR ECL SBCL ABCL)
     (apply #'cl:make-hash-table :weakness weak args))))

#+(OR CCL SBCL ECL CLISP ABCL)
(define-compiler-macro make-hash-table (&whole whole &rest args &key weak &allow-other-keys)
  (declare (ignorable whole))
  (setq args (copy-list args))
  (remf args :weak)
  #+SBCL (setq args (list* ':synchronized 't args))
  #+(OR CCL CLISP)
  `(cl:make-hash-table ,@(when weak `(:weak ,weak)) ,@args)
  #+(OR ECL SBCL ABCL)
  `(cl:make-hash-table ,@(when weak `(:weakness ,weak)) ,@args))


;;;; Running External Programs

;; This is a thin layer over the common RUN-PROGRAM API that is so
;; common.

#+(OR SBCL CCL)
(defun run-program (program arguments
                    &rest rest
                    &key wait
                         input
                         (output *standard-output*)
                         if-output-exists
                         (error *error-output*)
                         (external-format #+NOFFI-WINDOWS +utf-8/crnl+ #-NOFFI-WINDOWS +utf-8+))
  (declare (ignorable wait input output error external-format if-output-exists))
  (destructuring-bind (program &rest arguments)
      (mapcar (lambda (x)
                (etypecase x
                  (pathname (native-namestring x))
                  (string x)))
              (cons program arguments))
    #+CCL
    (let ((proc
           (apply #'ccl:run-program program arguments
                  :external-format external-format
                  :output output :error error
                  rest)))
      (values
       proc
       (ccl:external-process-output-stream proc)
       (ccl:external-process-error-stream proc)
       (ccl:external-process-input-stream proc)))
    #+SBCL
    (let ((proc
           (apply #'sb-ext:run-program program arguments
                  :search t
                  :external-format external-format
                  :output output :error error
                  rest)))
      (values
       proc
       (sb-ext:process-output proc)
       (sb-ext:process-error proc)
       (sb-ext:process-input proc)))))

(defun exit-status (proc)
  #+CCL
  (nth-value 1 (ccl:external-process-status proc))
  #+SBCL
  (sb-ext:process-exit-code proc))


;;;; Native Pathnames

;; What we really want is two routines: (1) one that returns a pathname
;; for consumption of system calls and the like and (2) one for
;; consumption by shell commands. The latter may involve escaping and using e.g. "\" inst

#+CCL
(progn
  ;; CCL does what we need.
  
  (defun native-namestring (pathname)
    (ccl:native-translated-namestring pathname))

  (defun native-pathname (string)
    (ccl:native-to-pathname string))

  (defun native-directory-pathname (string)
    (ccl::native-to-directory-pathname string)))

#+SBCL
(progn

  ;; Again. Bugs. Or some strange kind of humor or both.

  ;; First of all *DEFAULT-PATHNAME-DEFAULTS* is considered for the host,
  ;; which of course fails when that is a logical pathname.
  ;; *DEFAULT-PATHNAME-DEFAULTS* should not be considered at all. We got
  ;; this pathname from some foreign function or from some external
  ;; program and those have no such concept and don't know about our
  ;; *DEFAULT-PATHNAME-DEFAULTS*.

  ;; Then we may end up with directories named "". There are no
  ;; directories with an empty name.

  (defun native-namestring (pathname)
    (sb-ext:native-namestring (translate-logical-pathname (pathname pathname))))

  (defun native-pathname (string)
    (native-pathname-1 string nil))

  (defun native-directory-pathname (string)
    (native-pathname-1 string t))

  (defun native-pathname-1 (string directoryp)
    (let ((pn (sb-ext:parse-native-namestring
               string nil
               (make-pathname :host sb-impl::*physical-host*
                              :device :unspecific :directory nil :name nil :type nil :version nil)
               :as-directory directoryp)))
      (make-pathname :directory (remove "" (pathname-directory pn) :test #'equal)
                     :defaults pn))))


;;;; Multi-processing
  
#+SBCL
(progn
  (defun make-lock (&optional name)
    (sb-thread:make-mutex :name (princ-to-string (or name "Anonymous recursive lock"))))

  (defmacro with-lock-held ((place) &body body)
    `(sb-thread:with-recursive-lock (,place) ,@body)))

#+CCL
(progn
  (defun make-lock (&optional name)
    (ccl:make-lock (or name "Anonymous recursive lock")))

  (defmacro with-lock-held ((place) &body body)
    `(ccl:with-lock-grabbed (,place) ,@body)))


;;;; POSIX

#+SBCL
(defun posix-getenv (name)
  (sb-ext:posix-getenv name))

#+CCL
(defun posix-getenv (name)
  (ccl:getenv name))
