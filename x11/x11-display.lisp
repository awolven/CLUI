(in-package :clui)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (noffi::noffi-syntax t))

(defvar *xim->display* (make-hash-table :test #'eq))
(defvar *xdisplay->display* (make-hash-table :test #'eq))

(defun display-for-xim (xim)
  (gethash (ptr-value xim) *xim->display*))

(defun display-for-xdisplay (xdisplay)
  (gethash (ptr-value xdisplay) *xdisplay->display*))

(defun wait-for-x11-event (display timeout)
  (setq timeout (coerce timeout 'double-float))
  (clet ((fd #_<struct pollfd>))
    (let ((&fd (c-addr-of fd))
	  (xdisplay (display-xdisplay display)))
      (setf (#_.fd &fd) (#_ConnectionNumber xdisplay)
	    (#_.events &fd) #_POLLIN)

      (loop while (zerop (#_XPending xdisplay))
	    unless (poll-posix &fd 1 timeout)
	      do (return nil)
	    finally (return t)))))

(defun wait-for-any-event (display timeout)
  (setq timeout (coerce timeout 'double-float))
  (let ((count 2))
    (clet ((fds #_<struct pollfd[3]>))
      (let ((&fds (c-addr-of fds))
	    (xdisplay (display-xdisplay display)))
	(setf (#_.fd (c-aref &fds 0)) (#_ConnectionNumber xdisplay)
	      (#_.event (c-aref &fds 0)) #_POLLIN)
	(setf (#_.fd (c-aref &fds 1)) (aref (empty-event-pipes display) 0)
	      (#_.event (c-aref &fds 1)) #_POLLIN)

	#+linux
	(when (typep display 'x11:local-server-mixin)
	  #+NIL
	  (when (joysticks-initialized? *linux*)
	    (setf (#_.fd (c-aref &fds count)) (linux-joysticks-inotify *linux*)
		  (#_.event (c-aref &fds count)) #_POLLIN)
	    (incf count)))

	(loop while (zerop (#_XPending xdisplay))
	      unless (poll-posix &fds count timeout)
		do (return nil)
	      do (loop for i from 1 below count
		       when (logtest (#_.revents (c-aref &fds i)) #_POLLIN)
			 do (return-from wait-for-any-event t))
	      finally (return t))))))

(defun write-empty-event (display)
  (loop
    do (clet ((byte #_<char> 0))
	 (let* ((&byte (c-addr-of byte))
		(result (#_write (c-aref (empty-event-pipes display) 1) &byte 1)))
	   (when (or (= result 1) (and (= result -1) (/= #_errno #_EINTR)))
	     (return (values)))))))

(defun drain-empty-events (display)
  (loop
    do (clet ((dummy #_<char[64]>))
	 (let ((result (#_read (c-aref (empty-event-pipes display) 0) dummy 64)))
	   (when (and (= result -1) (/= #_errno #_EINTR))
	     (return (values)))))))

(defun get-x11-display-content-scale (display)
  (let ((dpi 96.0f0)
	(rms (#_XResourceManagerString (display-xdisplay display))))

    (when rms
      (let ((db (#_XrmGetStringDatabase rms)))

	(when db

	  (clet ((value #_<XrmValue>)
		 (type #_<char*>))
	    (let ((&type (c-addr-of type))
		  (&value (c-addr-of value)))

	      (when (#_XrmGetResource db "Xft.dpi" "Xft.Dpi" &type &value)

		(when (and type (string= type "String"))
		  (setq dpi (#_.addr &value))))))

	  (#_XrmDestroyDatabase db))))

    (/ dpi 96.0f0)))

;; (defcfun (input-method-destroy-callback #_<void>)
;; 	 ((im #_<XIM>) (client-data #_<XPointer) (call-data #_<XPointer>))
;;   (declare (ignore client-data call-data))
;;   (let ((display (display-for-xim im)))
;;     (when display
;;       (remhash (ptr-value xim) *xim->display*)
;;       (setf (display-input-method display) nil))
;;     (values)))

(defcfun (input-method-instantiate-callback #_<void>)
	 ((xdisplay #_<Display*>) (client-data #_<XPointer>) (call-data #_<XPointer>))
  (declare (ignore client-data call-data))
  (let ((display (display-for-xdisplay xdisplay)))
    (when display
      (input-method-instantiate display xdisplay))
    (values)))

(defun input-method-instantiate (display xdisplay)
  (block nil

    (when (display-input-method display)
      (return (values)))
      
    (setf (display-input-method display) (#_XOpenIM xdisplay 0 nil nil))

    (when (display-input-method display)

      (unless (has-usable-input-method-style? display)
	(#_XCloseIM (display-input-method display))
	(setf (display-input-method display) nil)))

    (when (display-input-method display)
      (setf (gethash (cval-value (display-input-method display)) *xim->display*) display)
      (clet ((callback #_<XIMCallback>))
	(let ((&callback (c-addr-of callback)))
	  (setf ;;(#_.callback &callback) input-method-destroy-callback
		(#_.client_data &callback) nil)
	  (#_XSetIMValues (display-input-method display) #_XNDestroyCallback &callback nil)

	  (do ((window (display-window-list-head display) (window-next window)))
	      (window)
	      (create-x11-input-context window)))))
    (values)))

(defun create-x11-cursor (display image xhot yhot)
  (block nil
    (unless (xcursor-handle display)
      (return nil))

    (let ((native (#_XcursorImageCreate (image-width image) (image-height image))))
      (unless native
	(return nil))

      (setf (#_.xhot native) (round xhot)
	    (#_.yhot native) (round yhot))

      (let ((source (image-pixels image))
	    (target (#_pixels native)))

	(loop for i from 0 below (* (image-width image) (image-height image))
	      for source-index from 0 by 4
	      do (let ((alpha (aref source (+ source-index 3))))
		   
		   (setf (c-aref target i) (logior (ash alpha 24)
						   (floor (* (aref source (+ source-index 0)) alpha) 255)
						   (floor (* (aref source (+ source-index 1)) alpha) 255)
						   (floor (* (aref source (+ source-index 2)) alpha) 255)))))

	(prog1 (#_XcursorImageLoadCursor (display-xdisplay display) native)
	  (#_XcursorImageDestroy native))))))
