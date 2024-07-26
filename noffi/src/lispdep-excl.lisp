;; -*- Mode: Lisp; -*-
;; ---------------------------------------------------------------------------
;;     Title: Lisp dependent definitions for Allegro Common Lisp
;;   Created: 2024-06-19
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

#-EXCL #.(error "Do I look like Allegro?")

(defconstant +utf-8+ :utf-8)
(defconstant +iso-8859-1+ :iso-8859-1)
;; We need to figure these out.
(define-symbol-macro +iso-8859-1/crnl+ :iso-8859-1)
(define-symbol-macro +utf-8/crnl+ :utf-8)

(defun make-hash-table (&rest args &key weak &allow-other-keys)
  (setq args (copy-list args))
  (remf args :weak)
  (ecase weak
    (:value
     (apply #'cl:make-hash-table :values :weak args))
    (:key
     (apply #'cl:make-hash-table :weak-keys t args))
    ((nil)
     (apply #'cl:make-hash-table args))))

(defstruct external-process
  command
  state
  exit-code
  pid
  stdin stdout stderr
  ;; Helper threads
  stdin-helper
  stdout-helper
  stderr-helper)

(defun run-program (program arguments
                    &rest args
                    &key (wait t)
                         (output *standard-output*)
                         (error *error-output*)
                         (input nil)
                         external-format
                         if-output-exists)
  (declare (ignore external-format))
  (print `(run-program ,program ,arguments ,@args))(terpri)
  (labels ((copy-stream (in out)
             (loop for c = (let ((c (handler-case
                                        (read-char-no-hang in nil :eof)
                                      (error () :error))))
                             (if (null c)
                                 (progn
                                   (force-output out)
                                   (setf c (handler-case
                                               (read-char in nil :eof)
                                             (error () :error))))
                                 c))
                   while (characterp c)
                   do (write-char c out))))
    (multiple-value-bind (output-arg need-stdout-helper return-stdout)
        (etypecase output
          (null (values nil nil nil))
          ((member :stream) (values :stream nil t))
          (file-stream (values output nil nil))
          (stream (values :stream t nil)))
      (etypecase input
        (null)
        ((member :stream)
         (when wait (error "You cannot have ~S ~S and ~S ~S."
                           ':input input ':wait wait)))
        (stream))
      (etypecase error
        (null)
        ((member :stream)
         (when wait (error "You cannot have ~S ~S and ~S ~S."
                           ':error error ':wait wait)))
        (stream))
      (let ((command
             (labels ((escape-command-line-arg (arg)
                        (if (every (lambda (c)
                                     (not (member c '(#\space #\tab #\"))))
                                   arg)
                            arg
                            (with-output-to-string (bag)
                              (princ #\" bag)
                              (loop for c across arg
                                    do (case c
                                         (#\" (princ "\\\"" bag))
                                         (#\\ (princ "\\\\" bag))
                                         (t (princ c bag))))
                              (princ #\" bag)))))
               (format nil "~{~A~^ ~}"
                       (mapcar #'copy-seq (mapcar #'escape-command-line-arg (cons program arguments)))))))
        (multiple-value-bind (stdin stdout stderr pid)
            (excl:run-shell-command
             command
             :show-window :hide
             :separate-streams t
             :wait nil
             :output output-arg
             :error-output (and error :stream)
             :input (and input :stream))
          (let ((proc
                 (make-external-process
                  :command command
                  :state :running
                  :pid pid :stdin stdin :stdout stdout :stderr stderr
                  :stdout-helper
                  (and need-stdout-helper
                       (mp:process-run-function
                        (format nil "pid ~D stdout helper" pid)
                        (lambda () (copy-stream stdout output))))
                  :stderr-helper
                  (and (streamp error)
                       (mp:process-run-function
                        (format nil "pid ~D stdout helper" pid)
                        (lambda () (copy-stream stderr error))))
                  :stdin-helper
                  (and (streamp input)
                       (mp:process-run-function
                        (format nil "pid ~D stdin helper" pid)
                        (lambda ()
                          (progn
                            (copy-stream input stdin)
                            (ignore-errors (close stdin)))))))))
            (when wait
              (exit-status proc))
            (values proc
                    (and return-stdout stdout)
                    (and (eq error ':stream) stderr)
                    (and (eq input ':stream) stdin))))))))

(defun exit-status (proc)
  (when (eql (external-process-state proc) ':running)
    (when (external-process-stdout-helper proc)
      (mp:process-join (external-process-stdout-helper proc))
      (setf (external-process-stdout-helper proc) nil))
    (when (external-process-stderr-helper proc)
      (mp:process-join (external-process-stderr-helper proc))
      (setf (external-process-stderr-helper proc) nil))
    (when (external-process-stdin-helper proc)
      (mp:process-join (external-process-stdin-helper proc))
      (setf (external-process-stdin-helper proc) nil))
    (when (external-process-stdin proc)
      (ignore-errors (close (external-process-stdin proc)))
      (setf (external-process-stdin proc) nil))
    (when (external-process-stdout proc)
      (ignore-errors (close (external-process-stdout proc)))
      (setf (external-process-stdout proc) nil))
    (when (external-process-stderr proc)
      (ignore-errors (close (external-process-stderr proc)))
      (setf (external-process-stderr proc) nil))
    (setf (external-process-state proc) :exited
          (external-process-exit-code proc)
          (sys:reap-os-subprocess :pid (external-process-pid proc) :wait t)))
  (external-process-exit-code proc))

(defun native-namestring (pathname)
  ;; no idea
  (namestring (translate-logical-pathname (pathname pathname))))

#+(or)
(defun native-pathname (string)
  )

#+(or)
(defun native-directory-pathname (string)
  )

(defun make-lock (&optional name)
  (mp:make-process-lock :name name))

(defmacro with-lock-held ((place) &body body)
  `(mp:with-process-lock (,place)
     ,@body))

(defmacro without-interrupts (&body body)
  `(mp:without-interrupts ,@body))

(defun posix-getenv (name)
  (sys:getenv name))

(defun eval-in-env (form env)
  (excl::%eval-compile-time form env))
