(in-package :noffi)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (shadow '("OPEN" "WITH-OPEN-FILE" "PRINT")))

(defun open (pathname &rest args)
  (when (eq :new-version (getf args :if-exists))
    (setq args (copy-list args))
    (setf (getf args :if-exists) :supersede))
  (apply #'cl:open pathname args))

(defmacro with-open-file ((stream-var pathname &rest options) &body body)
  (let ((abortp (gensym "ABORTP."))
        (stream (gensym "STREAM.")))
    `(let ((,abortp t)
           (,stream (open ,pathname ,@options)))
       (unwind-protect
            (multiple-value-prog1
                (let ((,stream-var ,stream))
                  ,@body)
              (setq ,abortp nil))
         (when ,stream
           (close ,stream :abort ,abortp))))))

;; Another very nice idea would be to FORCE-OUTPUT to *STANDARD-OUTPUT*
;; when you enter the debugger, that is using the very same underlying
;; stream. And before printing the debugger prompt. I mean you say ...
;; (print 'hey) (/ 1 0) ... and what you see is "While blah blah blah
;; DIVISION-BY-ZERO blahHEY blah blah". The "HEY" is buried. The output
;; does not match the sequence of things happening.

(defun print (x &optional s)
  (multiple-value-prog1 (cl:print x s) (or s (force-output s))))

