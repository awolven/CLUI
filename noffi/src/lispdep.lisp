(in-package :noffi)


;;;; Weak Hash Tables

#+ECL #.(shadow "MAKE-HASH-TABLE")

(eval-when (:compile-toplevel :execute :load-toplevel)
  (shadow "MAKE-HASH-TABLE" :noffi))

#-(OR CCL SBCL ECL CLISP ABCL)
#.(error "How to make weak hash tables?")

(defun make-hash-table (&rest args &key weak &allow-other-keys)
  (remf args :weak)
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

(define-compiler-macro make-hash-table (&whole whole &rest args &key weak &allow-other-keys)
  (declare (ignorable whole))
  (remf args :weak)
  #+(OR CCL CLISP)
  `(cl:make-hash-table ,@(when weak `(:weak ,weak)) ,@args)
  #+(OR ECL SBCL ABCL)
  `(cl:make-hash-table ,@(when weak `(:weakness ,weak)) ,@args))


;;;; Running External Programs

;; This is a thin layer over the common RUN-PROGRAM API that is so
;; common.

(defun run-program (program arguments
                    &rest rest
                    &key wait
                         input
                         output
                         error
                         (external-format :utf-8))
  (declare (ignorable wait input output error external-format))
  #+CCL
  (let ((proc
         (apply #'ccl:run-program program arguments
                :external-format external-format
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
                rest)))
    (values
     proc
     (sb-ext:process-output proc)
     (sb-ext:process-error proc)
     (sb-ext:process-input proc))))

(defun exit-status (proc)
  #+CCL
  (nth-value 1 (ccl:external-process-status proc))
  #+SBCL
  (sb-ext:process-exit-code proc))

#+CCL  (defun native-namestring (pathname) (ccl:native-translated-namestring pathname))
#+SBCL (defun native-namestring (pathname) (sb-ext:native-namestring (translate-logical-pathname (pathname pathname))))
