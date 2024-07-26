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

(defmacro use-include (name)
  (setq name (cond ((pathnamep name) (concatenate 'string "\""  (namestring name) "\""))
                   ((char= (char name 0) #\<) name)
                   (t (concatenate 'string "\"" name "\""))))
  `(progn
     ,(c-toplevel-process-string (format nil "#include ~A~%" (verbatim name)))
     ',name))

(defparameter +c-shared-object-pathname-type+
  (or
   #+NOFFI-DARWIN "dylib"
   #+NOFFI-LINUX "so"
   #+NOFFI-WINDOWS "dll"
   "so"))

(defparameter +c-object-pathname-type+
  (or
   #+NOFFI-DARWIN "o"
   #+NOFFI-LINUX "o"
   #+NOFFI-WINDOWS "obj"
   "o"))

(defparameter +c-executable-pathname-type+
  #+NOFFI-WINDOWS "exe" #-NOFFI-WINDOWS "bin")


;;;; Using the host C compiler

(defvar *cc* "cc"
  "C compiler to use with C-COMPILE-FILE. Either a string or a list of
the command name followed by command line arguments.")

(defun c-compile-file-pathname (pathname &key (output-type :shared-object))
  (make-pathname :type (ecase output-type
                         (:preprocessed "i")
                         (:preprocessed-and-verify "i")
                         (:preprocessed-with-definitions "j")
                         (:shared-object +c-shared-object-pathname-type+)
                         (:object +c-object-pathname-type+)
                         ((:executable :run) +c-executable-pathname-type+))
                 :defaults pathname))

;; CL.EXE
;; /Fe:
;; /Fo:
;; /std:clatest
;; /c
;; /LD

(defun c-compile-file (pathname &key ((:cc *cc*) *cc*)
                                     (output-type :shared-object)
                                     (output-file (c-compile-file-pathname pathname :output-type output-type))
                                     (verbose *compile-verbose*))
  (check-type output-type (member :object :shared-object :executable :run :preprocessed :preprocessed-with-definitions :preprocessed-and-verify))
  (let ((native-output (native-namestring output-file))
        (cc (if (stringp *cc*) (list *cc*) *cc*))
        (stdout-is-output nil)
        (temp-files nil)
        (t0 (get-internal-real-time)))
    (let ((cmd
           (if (string-equal "cl" (pathname-name (car cc)))
               (append cc
                       (list "/nologo"
                             ;; "/std:c11"
                             )
                       (ecase output-type
                         (:preprocessed-and-verify
                          (let ((obj-file (native-namestring
                                           (c-compile-file-pathname pathname :output-type :object))))
                            (push obj-file temp-files)
                            (list "/c" "/P" "/Fo:" obj-file "/Fi:" native-output)))
                         (:preprocessed
                          (list "/E" "/P" "/Fi:" native-output))
                         (:preprocessed-with-definitions
                          (list
                           "/Zc:preprocessor" "/E" "/P" "/PD"
                           "/Fi:" native-output))
                         (:object
                          (list "/c" "/Fo:" native-output))
                         ((:executable :run)
                          (let ((obj-file (native-namestring
                                           (c-compile-file-pathname pathname :output-type :object))))
                            (push obj-file temp-files)
                            (list "/Fe:" native-output
                                  "/Fo:" (native-namestring obj-file))))
                         (:shared-object
                          (list "/LD" "/Fe:" native-output)))
                       (list (native-namestring pathname)))
               (append cc
                       (ecase output-type
                         (:preprocessed
                          (setq stdout-is-output t)
                          (list "-E"))
                         (:preprocessed-with-definitions
                          (setq stdout-is-output t)
                          (list "-dD" "-E"))
                         (:object (list "-c"))
                         ((:executable :run) nil)
                         (:shared-object (list "-shared")))
                       (list "-o" native-output)
                       (list (native-namestring pathname)) ))))
      (assert (every #'stringp cmd))
      (ensure-directories-exist output-file :verbose verbose)
      (when verbose
        ;;(format t "~&; Compiling C file ~S~%" pathname)
        (format t "~&# ~{~A~^ ~}~%" (mapcar #'verbatim cmd))
        (force-output))
      (ignore-errors (delete-file output-file))
      (dolist (k temp-files) (ignore-errors (delete-file k)) (ensure-directories-exist k))
      (let ((won nil))
        (unwind-protect
             (let ((exit (exit-status
                          (run-program (car cmd) (cdr cmd)
                                       :output (if stdout-is-output
                                                   output-file
                                                   *standard-output*)
                                       :if-output-exists :supersede
                                       :error *error-output*
                                       :external-format +utf-8/crnl+))))
               (unless (zerop exit)
                 (error "Shell command ~S failed. Exit code: ~D" cmd exit))
               (setq won t))
          (unless won
            (ignore-errors (delete-file output-file))
            (dolist (k temp-files) (ignore-errors (delete-file k)))))
        (when verbose
          (format t "~&# done, took ~:Dms.~%" (round (- (get-internal-real-time) t0)
                                                     (/ internal-time-units-per-second 1000)))
          (force-output))
        output-file))))

#+(OR CCL SBCL)
(defun use-library (name &key search-path)
  "Uses the library named by `name'.

   The name can be the library name without platform-specific prefixes or
   suffixes, just like you would pass it as e.g. `-lfoo` to the C
   linker/compiler.

   _search-path_ is a list of directories to search for the library. System
   default directories (like e.g. `/usr/lib` are always searched)."
  (when (pathnamep name)
    (setq name (native-namestring name))) ;Really?
  ;; This is trial and error.
  (let ((candidates nil))
    (labels ((candidate (x)
               (setf candidates (nconc candidates (list x))))
             (try-path (path)
               (when (pathnamep path)
                 (setq path (native-namestring path)))
               ;; Ensure exactly one slash
               (try-path-1
                (concatenate 'string (string-right-trim "/" (concatenate 'string path "/")) "/")))
             (try-path-1 (path)
               (dolist (suffix (list #+(AND (OR CCL SBCL) DARWIN) ".dylib"
                                     #+(AND (OR CCL SBCL) UNIX (NOT DARWIN)) ".so"
                                     #+(OR (AND CCL WINDOWS) (AND SBCL WIN32)) ".dll"
                                     ""))
                 (dolist (prefix (list #+(AND (OR CCL UNIX) UNIX) "lib"
                                       ""))
                   (candidate (concatenate 'string path prefix name suffix))))))
      (mapc #'try-path search-path)
      (try-path-1 "")
      ;;
      (unless (block try
                (dolist (candidate candidates)
                  (handler-case
                      (progn
                        #+CCL  (ccl:open-shared-library candidate)
                        #+SBCL (sb-alien:load-shared-object candidate)
                        (when *load-verbose*
                          (format t "~&; Loaded ~S.~%" candidate)
                          (force-output))
                        (return-from try 't))
                    (error (c)
                      (declare (ignore c))
                      nil))))
        (error "~@<Cannot load library ~S. We tried looking at: ~@<~{~S~^ ~}~:>~@:>"
               name candidates)))))


;;;; -- pkg-config Support --------------------------------------------------------------------

(defvar *pkg-config-program*
  "pkg-config"
  "Default program to use to find pkg configurations.")

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

(defun pkg-link (&rest pkg-names)
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
          (use-library lib :search-path search-path))))

(defmacro pkg-use (pkg-names &body c-stuff-or-options)
  "Arranges to use a given pkg by means of `pkg-config`.

_pkg-names_ is either a single pkg name or a list of pkg names. Then follows
either C material for `#include` directives or options. Options supported are:

`(:additional-config-args `{ _string_ }*`)` ::

    Additional command line arguments to pass to the `pkg-config` program.

`(:config-program `_string_`)` ::

    The program to use for `pkg-config`. The default is
    `*pkg-config-program*` which in turn defaults to `\"pkg-config\"`.

`pkg-use` then processes remaining strings as C (at compile time) and arranges for
the needed libraries to be linked into the running image at runtime.

#### Examples

    (pkg-use \"x11\"
        \"#include <X11/Xlib.h>\")

    (pkg-use (\"acme\" \"acme-extra\")
        (:additional-config-args \"--atleast-version=42\")
        (:config-program \"/opt/acme/acme-config\")
        \"#define ACME_SMALL\"
        \"#include <acme/acme.h>\")
"
  (unless (listp pkg-names)
    (setq pkg-names (list pkg-names)))
  (let ((c-stuff nil)
        (config-program nil)
        (config-args nil))
    (loop for q in c-stuff-or-options do
          (etypecase q
            (string (setf c-stuff (nconc c-stuff (list q))))
            ((cons (member :ADDITIONAL-CONFIG-ARGS))
             ;; I'm not happy with this, those args should rather be evaluated.
             (mapc (lambda (x)
                     (unless (stringp x)
                       (error "~S must be a list of strings, got ~S"
                              ':additional-config-args q)))
                   (cdr q))
             (setq config-args
                   (append config-args (cdr q))))
            ((cons (member :CONFIG-PROGRAM) (cons string null))
             (setq config-program (cadr q)))))
    (let ((*pkg-config-program* (or config-program *pkg-config-program*)))
      (let ((pkg-args (append config-args pkg-names)))
        (let ((de.bauhh.cpp::*cc-args* (apply #'pkg-cflags pkg-args)))
          `(progn
             ,(parse-top-level
               (cpp (format nil "~{~A~%~}" (mapcar #'verbatim c-stuff))))
             (let ((*pkg-config-program* ',*pkg-config-program*))
               (pkg-link ,@(mapcar (lambda (x) `',x) pkg-args)))))))))

