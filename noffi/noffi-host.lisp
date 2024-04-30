(in-package :cl-user)

(setf (logical-pathname-translations "noffi")
      (list (list "**;*.*.*"
                  (macrolet ((aux ()
                               (merge-pathnames
                                (make-pathname
                                 :directory '(:relative :wild-inferiors)
                                 :name :wild
                                 :type :wild
                                 :version :wild)
                                *compile-file-truename*)))
                    (aux)))))
