(in-package :cl-user)

(labels ((sanify (s)
           (string-downcase
            (substitute-if-not #\- (lambda (c) (and (standard-char-p c)
                                                    (or (alphanumericp c)
                                                        (find c "-"))))
                               s))))
  (setf (logical-pathname-translations "noffi")
        `((,(format nil "**;*.~A.*" (pathname-type (compile-file-pathname "")))
            ,(merge-pathnames
              (make-pathname
               :directory `(:relative "fasl"
                                      ,(sanify (lisp-implementation-type))
                                      ,(sanify (lisp-implementation-version))
                                      ,(sanify (software-type))
                                      ,(sanify (machine-type))
                                      :wild-inferiors)
               :name :wild
               :type :wild
               :version :wild)
              *load-truename*))
          ("**;*.*.*" ,(merge-pathnames (make-pathname
                                         :directory '(:relative :wild-inferiors)
                                         :name :wild
                                         :type :wild
                                         :version :wild)
                                        *load-truename*)))))

(defun load-noffi ()
  (let ((*load-verbose* t))
    (let ((compiling-p nil))
      (labels
          ((compile-and-load (pathname)
             (load (or (when (or compiling-p
                                 (not (probe-file (compile-file-pathname pathname)))
                                 (> (file-write-date pathname)
                                    (file-write-date (compile-file-pathname pathname))))
                         (setq compiling-p t)
                         (ensure-directories-exist (compile-file-pathname pathname) :verbose t)
                         (prog1
                             (compile-file pathname)
                           (force-output *error-output*)))
                       (compile-file-pathname pathname)))))
        (mapc #'compile-and-load
              '(#+CCL
                "noffi:src;patch-ccl.lisp"
                ;;
                "noffi:other;clex;src;clex.lisp"
                "noffi:other;lalr;lalr.lisp"
                ;;
                "noffi:src;file.lisp"
                "noffi:src;compiler-warn.lisp"
                "noffi:src;package.lisp"
                "noffi:src;features.lisp"
         #+SBCL "noffi:src;patch-sbcl.lisp"
                "noffi:src;forward.lisp"
                #+(OR CCL SBCL ECL ABCL CLISP)
                "noffi:src;lispdep.lisp"
         #+EXCL "noffi:src;lispdep-excl.lisp"
                "noffi:src;util.lisp"
                "noffi:src;string-table.lisp"
                ;;
                "noffi:src;abi.lisp"
                ;;
                "noffi:src;define-grammar.lisp"
                "noffi:src;lexer.lisp"
                "noffi:src;cpp.lisp"
                "noffi:src;parsing.lisp"
                "noffi:src;grammar.lisp"
                "noffi:src;adt.lisp"
                "noffi:src;comp.lisp"
         #+EXCL "noffi:src;backend-excl.lisp"
                "noffi:src;runtime.lisp"
                "noffi:src;syntax.lisp"
                ;;
                "noffi:src;abi-amd64-sysv.lisp"
                "noffi:src;abi-amd64-mingw64.lisp"
                "noffi:src;abi-amd64-ms.lisp"
                ;;
                "noffi:src;abi-amd64-sysv-cc.lisp"
                "noffi:src;abi-amd64-ms-cc.lisp"
                ;;
                "noffi:src;config.lisp"
                "noffi:src;noffi-util.lisp" ))))) )

(load-noffi)
