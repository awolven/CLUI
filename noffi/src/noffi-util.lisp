;; -*- Mode: Lisp; -*-
;; ---------------------------------------------------------------------------
;;     Title: Some utilities for easier use of noffi
;;   Created: 2023-05-22
;;    Author: Gilbert Baumann <gilbert@bauhh.de>
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

(defun use-include (name)
  (setq name (cond ((pathnamep name) (concatenate 'string "\""  (namestring name) "\""))
                   ((char= (char name 0) #\<) name)
                   (t (concatenate 'string "\"" name "\""))))
  (eval (c-toplevel-process-string (format nil "#include ~A~%" (verbatim name))))
  name)

(defparameter +c-shared-object-pathname-type+
  (or
   #+NOFFI-DARWIN "dylib"
   #+NOFFI-LINUX "so"
   #+NOFFI-WINDOWS "dll"
   "so"))


;;;; Using the host C compiler

(defun c-compile-file-pathname (pathname)
  (make-pathname :type +c-shared-object-pathname-type+
                 :defaults pathname))

(defun c-compile-file (pathname &key (output-file (c-compile-file-pathname pathname)))
  (let ((cmd
         #+NOFFI-DARWIN (list "cc" "-shared" "-o" (native-namestring output-file) (native-namestring pathname))
         ;; Hmm
         #+NOFFI-WINDOWS (list "cc" "-shared" "-o" (native-namestring output-file) (native-namestring pathname)) 
         ))
    (when *compile-verbose*
      (format t "~&; Compiling C file ~S ..." pathname)
      (force-output))
    (let ((exit (exit-status
                 (run-program (car cmd) (cdr cmd) :error *error-output*))))
      (unless (zerop exit)
        (error "Shell command ~S failed." cmd))
      (when *compile-verbose*
        (format t " done~%")
        (force-output))
      output-file)))

#+(OR CCL SBCL)
(defun use-library (name)
  "Uses the library named by `name' which is the name of the library
  without platform specific suffixes or prefix just like you would
  pass it as \"-lfoo\" to the C compiler."
  (when (pathnamep name)
    (setq name (native-namestring name)))
  (let ((prefixes (list #+(AND (OR CCL UNIX) UNIX) "" "lib"))
        (suffixes (list #+(AND (OR CCL SBCL) DARWIN) "" ".dylib"
                        #+(AND (OR CCL SBCL) UNIX) ".so"
                        #+(OR (AND CCL WINDOWS) (AND SBCL WIN32)) ".dll")))
    (let ((names (cons name
                       (loop for suffix in suffixes append
                             (loop for prefix in prefixes collect
                                   (concatenate 'string prefix name suffix))))))
      (or (some #'(lambda (candidate)
                    (block nil
                      (handler-case
                          (progn
                            #+CCL  (ccl:open-shared-library candidate)
                            #+SBCL (sb-alien:load-shared-object candidate)
                            (when *load-verbose*
                              (format t "~&; Loaded ~S.~%" candidate)
                              (force-output)))
                          (error (c)
                            (declare (ignore c))
                            (return nil)))
                      ;; Fall through
                      candidate))
                names)
          (error "Cannot load shared library ~S, tried: ~S." name names)))))


;;;; pkg-config support

(defvar *pkg-config-program* "pkg-config")

(defun pkg-config (&rest args)
  (let* ((output (make-string-output-stream))
         (error-output (make-string-output-stream)))
    (unless (eql 0 (exit-status
                    (run-program *pkg-config-program*
                                 args
                                 :output output
                                 :error error-output)))
      (error "Failed to run 'pkg-config ~{~A~^ ~}'~%~A"
             (mapcar #'verbatim args)
             (verbatim (get-output-stream-string error-output))))
    (get-output-stream-string output)))

(defun pkg-cflags (&rest args)
  (split-by-if (lambda (c)
                 (or (eql c #\space) (char<= #\tab c #\return)))
               (apply #'pkg-config "--cflags" args)
               :nuke-empty-p t))

(defun pkg-libs (&rest args)
  (split-by-if (lambda (c)
                 (or (eql c #\space) (char<= #\tab c #\return)))
               (apply #'pkg-config "--libs" args)
               :nuke-empty-p t))

#+(AND (OR) DARWIN)
(defun pkg-link (pkg-name)
  (unless (listp pkg-name)
    (setq pkg-name (list pkg-name)))
  ;; We might consider to run otool -L on that
  (let ((filename (format nil
                          #+DARWIN "/tmp/~A.dylib"
                          #+LINUX "/tmp/~A.so"
                          (verbatim
                           (with-standard-io-syntax
                             (write-to-string (+ (expt 36 12) (random (expt 36 12)))
                                              :base 36))))))
    (let ((flags
           (apply #'append (mapcar #'pkg-libs pkg-name))))
      #+DARWIN (apply #'cc "-dynamiclib" "-o" filename flags)
      #+LINUX (apply #'cc "-shared" "-o" filename flags)
      )
    (ccl:open-shared-library filename)))

#+(OR)
(defun cc (&rest args)
  (let* ((output (make-string-output-stream))
         (error-output (make-string-output-stream))
         (proc (ccl:run-program "cc"
                                args
                                :output output
                                :error error-output)))
    (unless (equal (multiple-value-list (ccl:external-process-status proc))
                   '(:exited 0))
      (error "Failed to run 'cc ~{~A~^ ~}'~%~A"
             (mapcar #'verbatim args)
             (verbatim (get-output-stream-string error-output))))
    (get-output-stream-string output)))

(defun pkg-link (pkg-names)
  (unless (listp pkg-names)
    (setq pkg-names (list pkg-names)))
  (let ((flags (apply #'pkg-libs pkg-names))
        (search-path nil)
        (to-load nil))
    (loop for flag in flags do
          (cond ((eql 0 (search "-l" flag))
                 (push (subseq flag 2) to-load))
                ((eql 0 (search "-L" flag))
                 (push (format nil "~A/" (verbatim (string-right-trim "/" (subseq flag 2)))) search-path))
                ((equal "-pthread" flag))
                (t
                 (format t "~&;; *** unknown linker flag: ~S~%" flag))))
    (setq search-path (reverse search-path))
    (setq to-load (reverse to-load))
    (loop for lib in to-load do
          (block attempt
            (loop for p in (cons "" search-path)
                  do (let ((lib (format nil "~A~A" (verbatim p) (verbatim lib))))
                       (handler-case
                           (progn (use-library lib)
                                  (return-from attempt t))
                         (error (c) (declare (ignore c))))))
            ;;
            (error "Cannot load ~S, looked at ~S, no luck."
                   lib search-path)))))

(defmacro pkg-use (pkg-names &rest c-stuff-or-options)
  (unless (listp pkg-names)
    (setq pkg-names (list pkg-names)))
  (let ((c-stuff nil) (config-program nil))
    (loop for q in c-stuff-or-options do
          (etypecase q
            (string (setf c-stuff (nconc c-stuff (list q))))
            ((cons (member :CONFIG-PROGRAM) (cons string null))
             (setq config-program (cadr q)))))
    (let ((*pkg-config-program* (or config-program *pkg-config-program*)))
      (let ((de.bauhh.cpp::*cc-args* (apply #'pkg-cflags pkg-names)))
        `(progn
           ,(parse-top-level
             (cpp (format nil "~{~A~%~}" (mapcar #'verbatim c-stuff))))
           (let ((*pkg-config-program* ',*pkg-config-program*))
             (pkg-link ',pkg-names))))))) ;Hmm
