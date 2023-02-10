(in-package :clui)

(defun create-helper-x11-window (display)
  (clet ((wa #_<XSetWindowAttributes>))
    (let ((&wa (c-addr-of wa)))
      (setf (#_.event_mask &wa) #_PropertyChangeMask)

      (setf (helper-window-handle display)
	    (#_XCreateWindow (display-xdisplay display)
			     (default-root-window-handle display)
			     0 0 1 1 0 0
			     #_InputOnly
			     (#_DefaultVisual (display-xdisplay display)
					      (default-screen-id display))
			     #_CWEventMask &wa)))))

(defun create-hidden-x11-cursor (display)
  (let* ((size (* 16 16 4))
	 (pixels (make-array size :element-type '(unsigned-byte 8) :initial-element 0))
	 (image (make-image 16 16 pixels)))
    
    (setf (hidden-cursor-handle display) (create-x11-cursor display image 0 0))))

(defun create-empty-event-pipes (display)
  (let ((pipes (#_malloc (* 2 (cval-value (c-sizeof-type '#_<int>))))))
    (setq pipes (c-cast '#_<int[2]> pipes))
    (#_pipe pipes)

    
      (loop for i from 0 below 2
	    with sf
	    with df
	    do (setq sf (#_fcntl (c-aref pipes i) #_F_GETFL 0))
	       (setq df (#_fcntl (c-aref pipes i) #_F_GETFD 0))

	       (when (or (= -1 sf)
			 (= -1 df)
			 (= -1 (#_fcntl (c-aref pipes i) #_F_SETFL (logior sf #_O_NONBLOCK)))
			 (= -1 (#_fcntl (c-aref pipes i) #_F_SETFD (logior sf #_FD_CLOEXEC))))
		 (error "Failed to set flags for empty event pipe: ~S" (#_strerror #_errno))))

    (setf (empty-event-pipes display) pipes)))
	  

(defun init-and-connect-x11 (display)
  (let ((x11-lib
	  #+(or linux openbsd netbsd) "libX11.so"
	  #+cygwin "libX11-6.so"
	  #+darwin "libX11.dylib"
	  #-(or linux openbsd netbsd cygwin darwin) "libX11.so.6")
	(vidmode-lib #+(or openbsd netbsd) "libXxf86vm.so"
		     #-(or openbsd netbsd) "libXxf86vm.so.1")
	(xi-lib #+cygwin "libXi-6.so"
		#+(or openbsd netbsd) "libXi.so"
		#-(or cygwin openbsd netbsd) "libXi.so.6")
	(randr-lib #+cygwin "libXrandr-2.so"
		   #+(or openbsd netbsd) "libXrandr.so"
		   #-(or cygwin openbsd netbsd) "libXrandr.so.2")
	(xcursor-lib #+cygwin "libXcursor-1.so"
		     #+(or openbsd netbsd) "libXcursor.so"
		     #-(or cygwin openbsd netbsd) "libXcursor.so.1")
	(xinerama-lib #+cygwin "libXinerama-1.so"
		      #+(or openbsd netbsd) "libXinerama.so"
		      #-(or cygwin openbsd netbsd) "libXinerama.so.1")
	(x11-xcb-lib #+cygwin "libX11-xcb-1.so"
		      #+(or openbsd netbsd) "libX11-xcb.so"
		      #-(or cygwin openbsd netbsd) "libX11-xcb.so.1")
	(xrender-lib #+cygwin "libXrender-1.so"
		      #+(or openbsd netbsd) "libXrender.so"
		      #-(or cygwin openbsd netbsd) "libXrender.so.1")
	(xext-lib #+cygwin "libXext-6.so"
		  #+(or openbsd netbsd) "libXext.so"
		  #-(or cygwin openbsd netbsd) "libXext.so.6"))
	
    #+CCL (ccl:open-shared-library x11-lib)

    (#_XInitThreads)
    (#_XrmInitialize)

    (setf (display-xdisplay display) (#_XOpenDisplay (display-name display)))

    (setf (default-screen-id display) (#_DefaultScreen (display-xdisplay display)))

    (setf (default-root-window-handle display) (#_RootWindow (display-xdisplay display)
							     (default-screen-id display)))

    (setf (unique-context display) (#_XUniqueContext))

    (setf (content-scale display) (get-x11-display-content-scale display))

    (create-empty-event-pipes display)

    #+CCL (ccl:open-shared-library vidmode-lib)
    
    #+ccl (ccl:open-shared-library xi-lib)

    #+CCL (ccl:open-shared-library randr-lib)
    
    #+CCL (ccl:open-shared-library randr-lib)

    #+CCL (ccl:open-shared-library xcursor-lib)
    
    #+CCL (ccl:open-shared-library xinerama-lib)
    
    #+CCL (ccl:open-shared-library x11-xcb-lib)

    #+CCL (ccl:open-shared-library xrender-lib)
    
    #+CCL (ccl:open-shared-library xext-lib)

    (create-helper-x11-window display)
    (create-hidden-x11-cursor display)

    (unless (zerop (#_XSupportsLocale))
      (#_XSetLocaleModifiers "")

      (#_XRegisterIMInstantiateCallback (display-xdisplay display)
					nil nil nil
					input-method-instantiate-callback
					nil))

    t))
  

	      
