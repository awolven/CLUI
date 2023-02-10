(in-package :clui)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (noffi::noffi-syntax t))

(defun wait-for-visibility-notify (window)
  (let* ((display (window-display window))
	 (xdisplay (display-xdisplay display))
	 (handle (h window))
	 (timeout 0.1d0))
    (noffi::clet& ((&dummy #_<XEvent>))

      (loop while (zerop (#_XCheckTypedWindowEvent xdisplay handle
						   #_VisibilityNotify
						   &dummy))
	    unless (wait-for-x11-event display timeout)
	      do (return nil)
	    finally (return t)))))

#{ struct {
#_CARD32 state;
#_Window icon;
} window_state_state;
}


