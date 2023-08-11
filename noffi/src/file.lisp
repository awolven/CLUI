(defpackage :de.bauhh.file
  (:use :common-lisp)
  (:export
   ;;
   #:with-temponary-file
   #:copy-file
   #:unix-link
   #:read-file-to-string
   ;;
   ))

(in-package :de.bauhh.file)

;;;; -- Temponary Files -----------------------------------------------------------------------

(defmacro with-temponary-file ((filename) &body body)
  `(invoke-with-temponary-file (lambda (,filename)
                                 ,@body)))

(defun invoke-with-temponary-file (continuation)
  (let (fn stream)
    (loop
       (setf fn (with-standard-io-syntax
                  (let ((*print-base* 36))
                    (pathname (concatenate 'string "/tmp/" (princ-to-string (random (expt 36 20))))))))
       (when (setf stream (open fn :if-exists nil :if-does-not-exist :create :direction :output))
         (close stream)
         (return)))
    (unwind-protect (funcall continuation fn)
      (ignore-errors (delete-file fn)))))

;;;; -- File Tools ----------------------------------------------------------------------------

(defun copy-file (source destination)
  (with-open-file (input source :element-type '(unsigned-byte 8))
    (ensure-directories-exist destination :verbose t)
    (with-open-file (output destination :element-type '(unsigned-byte 8) :direction :output :if-exists :supersede)
      (let ((buf (make-array 8192 :element-type '(unsigned-byte 8))))
        (loop for n = (read-sequence buf input) while (> n 0) do
             (write-sequence buf output :end n))))))

#+(AND CCL (NOT WINDOWS))
(defun unix-link (oldpath newpath)
  (let ((errno
         (ccl::with-filename-cstrs ((oldpath oldpath)
                                    (newpath newpath))
           (ccl::int-errno-call (#_link oldpath newpath)))))
    (unless (zerop errno)
      (error "~A" (verbatim (ccl::%strerror errno))))))

;;;; ------------------------------------------------------------------------------------------

(defun read-file-to-string (pathname &key (external-format :utf-8))
  (with-open-file (input pathname :external-format external-format)
    (let ((buffer (make-string 512)))
      (with-output-to-string (bag)
        (loop for n = (read-sequence buffer input)
              until (zerop n) do
              (write-string buffer bag :end n))))))

