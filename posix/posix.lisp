(in-package :clui)

(defun poll-posix (fds count &optional (timeout nil))  ;; timeout is in seconds
  (unless timeout (setq timeout most-positive-double-float))
  (setq timeout (coerce timeout 'double-float))
  (loop
    do (if timeout
	   (let ((base (get-internal-real-time))
		 (result))

	     #+(or linux freebsd openbsd cygwin netbsd)
	     (let* ((seconds (floor timeout))
		    (nanoseconds (round (* (cl:the double-float (- timeout seconds)) 1d9))))
	       (clet ((ts #_<struct timespec>))
		 (let ((&ts (c-addr-of ts)))
		   (setf (#_.tv_sec &ts) seconds
			 (#_.tv_nsec &ts) nanoseconds)
		   #-netbsd
		   (setq result (#_ppoll fds count &ts nil))
		   #+netbsd
		   (setq result (#_pollts fds count &ts nil)))))

	     #-(or linux freebsd openbsd cygwin netbsd)
	     (let ((milliseconds (round (* timeout 1d3))))
	       (setq result (#_poll fds count milliseconds)))

	     (let ((error #_errno))
	       (cond ((> result 0) (return t))
		     ((and (= result -1) (/= error #_EINTR) (/= error #_EAGAIN)) (return nil))
		     ((>= 0.0d0 (setq timeout (- timeout (/ (- (get-internal-real-time) base) internal-time-units-per-second))))
		      (return nil)))))
	   
	   (let ((result (#_poll fds count -1)))
	     (cond ((> result 0) (return t))
		   ((and (= result -1) (/= #_errno #_EINTR) (/= #_errno #_EAGAIN)) (return nil)))))))
