(in-package :clui)

(defun create-helper-x11-window (display)
  (clet ((wa #_<XSetWindowAttributes>))
    (let ((&wa (c-addr-of wa)))
      (setf (#_.event_mask &wa) #_PropertyChangeMask)

      (setf (helper-window-handle display)
	    (#_XCreateWindow (h display)
			     (default-root-window-handle display)
			     0 0 1 1 0 0
			     #_InputOnly
			     (#_DefaultVisual (h display)
					      (default-screen-id display))
			     #_CWEventMask &wa)))))

(defun create-hidden-x11-cursor (display)
  (let* ((size (* 16 16 4))
	 (pixels (make-array size :element-type '(unsigned-byte 8) :initial-element 0))
	 (image (make-image 16 16 pixels)))
    
    (setf (hidden-cursor display) (create-x11-cursor display image 0 0))))

(defun create-empty-event-pipes (display)
  (let ((pipes (#_malloc (* 2 (cval-value (c-sizeof-type '#_<int>))))))
    ;;(setq pipes (c-cast '#_<int*> pipes))
    (#_pipe pipes)
    (setq pipes (cons-ptr (ptr-value pipes) 0 '#_<int[2]>))
    
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

(defun %detect-EWMH (display)
  (let ((xdisplay (h display))
	(root (default-root-window-handle display)))

    (with-slots (NET_SUPPORTING_WM_CHECK
		 NET_SUPPORTED
		 NET_WM_STATE
		 NET_WM_STATE_ABOVE
		 NET_WM_STATE_FULLSCREEN
		 NET_WM_STATE_MAXIMIZED_VERT
		 NET_WM_STATE_MAXIMIZED_HORZ
		 NET_WM_STATE_DEMANDS_ATTENTION
		 NET_WM_FULLSCREEN_MONITORS
		 NET_WM_WINDOW_TYPE
		 NET_WM_WINDOW_TYPE_NORMAL
		 NET_WORKAREA
		 NET_CURRENT_DESKTOP
		 NET_ACTIVE_WINDOW
		 NET_FRAME_EXTENTS
		 NET_REQUEST_FRAME_EXTENTS)
	
	(display-window-manager display)

      (clet ((window-from-root #_<Window*> #_NULL)
	     (window-from-child #_<Window*> #_NULL))
	(let ((&window-from-root (c-addr-of window-from-root))
	      (&window-from-child (c-addr-of window-from-child)))
	
	  (when (zerop (%get-x11-window-property xdisplay root NET_SUPPORTING_WM_CHECK
						 #_XA_WINDOW &window-from-root))
	    (return-from %detect-EWMH nil))
	  
	  (%grab-x11-error-handler display)

	  (unwind-protect
	       (when (zerop (%get-x11-window-property xdisplay (c-aref window-from-root 0)
						      NET_SUPPORTING_WM_CHECK
						      #_XA_WINDOW &window-from-child))
		 (#_XFree window-from-root)
		 (return-from %detect-EWMH nil))

	    (%release-x11-error-handler display))

	  (unwind-protect (unless (= (cval-value (c-aref window-from-root 0))
				     (cval-value (c-aref window-from-child 0)))
			    (return-from %detect-EWMH nil))
	    
	    (#_XFree window-from-root)
	    (#_XFree window-from-child))))

      (clet ((supported-atoms #_<Atom*> #_NULL))
	(let* ((&supported-atoms (c-addr-of supported-atoms))
	       (atom-count (%get-x11-window-property xdisplay root NET_SUPPORTED
						     #_XA_ATOM &supported-atoms)))
	  (flet ((get-if-supported (atom-name)
		   (let ((atom (#_XInternAtom xdisplay atom-name #_False)))
		     (loop for i from 0 below atom-count
			   when (= atom (cval-value (c-aref supported-atoms i)))
			     do (return atom)
			   finally (return nil)))))

	    (setf NET_WM_STATE (get-if-supported "_NET_WM_STATE")
		  NET_WM_STATE_ABOVE (get-if-supported "_NET_WM_STATE_ABOVE")
		  NET_WM_STATE_FULLSCREEN (get-if-supported "_NET_WM_STATE_FULLSCREEN")
		  NET_WM_STATE_MAXIMIZED_VERT (get-if-supported "_NET_WM_STATE_MAXIMIZED_VERT")
		  NET_WM_STATE_MAXIMIZED_HORZ (get-if-supported "_NET_WM_STATE_MAXIMIZED_HORZ")
		  NET_WM_STATE_DEMANDS_ATTENTION (get-if-supported "_NET_WM_STATE_DEMANDS_ATTENTION")
		  NET_WM_FULLSCREEN_MONITORS (get-if-supported "_NET_WM_STATE_FULLSCREEN_MONITORS")
		  NET_WM_WINDOW_TYPE (get-if-supported "_NET_WM_WINDOW_TYPE")
		  NET_WM_WINDOW_TYPE_NORMAL (get-if-supported "_NET_WM_WINDOW_TYPE_NORMAL")
		  NET_WORKAREA (get-if-supported "_NET_WORKAREA")
		  NET_CURRENT_DESKTOP (get-if-supported "_NET_CURRENT_DESKTOP")
		  NET_ACTIVE_WINDOW (get-if-supported "_NET_ACTIVE_WINDOW")
		  NET_FRAME_EXTENTS (get-if-supported "_NET_FRAME_EXTENTS")
		  NET_REQUEST_FRAME_EXTENTS (get-if-supported "_NET_REQUEST_FRAME_EXTENTS"))

	    (unless (ptr-nullptr-p supported-atoms)
	      (#_XFree supported-atoms))

	    t))))))
		
			
				       
	      
	    

	    
      
	  

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
		  #-(or cygwin openbsd netbsd) "libXext.so.6")
	(x11-state (display-x11-state display)))
	
    #+CCL (ccl:open-shared-library x11-lib)

    (#_XInitThreads)
    (#_XrmInitialize)

    (let* ((xdisplay (#_XOpenDisplay (display-name display)))
	   (screen-id (#_DefaultScreen xdisplay))
	   (root (#_RootWindow xdisplay screen-id)))
      
      (setf (h display) xdisplay)

      (setf (display-for-xdisplay xdisplay) display)

      (setf (default-screen display)
	    (make-instance 'screen
			   :h root
			   :display display
			   :screen-id screen-id))

      (setf (unique-context display) (#_XUniqueContext))
      
      (setf (display-content-scale display) (get-x11-display-content-scale display))
      
      (create-empty-event-pipes display)
      
      #+CCL (ccl:open-shared-library vidmode-lib)
      
      #+ccl (ccl:open-shared-library xi-lib)
      
      #+CCL (ccl:open-shared-library randr-lib)

      (clet ((event-base #_<int>)
	     (error-base #_<int>))
	(let* ((&event-base (c-addr-of event-base))
	       (&error-base (c-addr-of error-base)))

	  (unless (zerop (#_XRRQueryExtension xdisplay &event-base &error-base))
	    (setf (randr-event-base x11-state) event-base
		  (randr-error-base x11-state) error-base)
	    (clet ((major #_<int>)
		   (minor #_<int>))
	      (let* ((&major (c-addr-of major))
		     (&minor (c-addr-of minor)))
		(#_XRRQueryVersion xdisplay &major &minor)
		(setf (randr-major x11-state) (cval-value major)
		      (randr-minor x11-state) (cval-value minor))

		(when (or (> (cval-value major) 1)
			  (>= (cval-value minor) 3))
		  (setf (randr-available? x11-state) t)))))))

      (when (randr-available? x11-state)
	(let ((sr (#_XRRGetScreenResourcesCurrent xdisplay root)))

	  (when (or (zerop (#_.ncrtc sr)) (zerop (#_XRRGetCrtcGammaSize xdisplay (c-aref (#_.crtcs sr) 0))))
	    (setf (randr-gamma-broken? x11-state) t))

	  (when (zerop (#_.ncrtc sr))
	    (setf (randr-monitor-broken? x11-state) t))

	  (#_XRRFreeScreenResources sr)))

      (when (and (randr-available? x11-state) (not (randr-monitor-broken? x11-state)))
	(#_XRRSelectInput xdisplay root #_RROutputChangeNotifyMask))
		
		      
      #+CCL (ccl:open-shared-library xcursor-lib)
		      
      #+CCL (ccl:open-shared-library xinerama-lib)

      (clet ((major #_<int>)
	     (minor #_<int>))
	(let* ((&major (c-addr-of major))
	       (&minor (c-addr-of minor)))
	  (unless (zerop (#_XineramaQueryExtension xdisplay &major &minor))
	    (setf (xinerama-major x11-state) (cval-value major)
		  (xinerama-minor x11-state) (cval-value minor))
	    (unless (zerop (#_XineramaIsActive xdisplay))
	      (setf (xinerama-available? x11-state) t)))))
    
      #+CCL (ccl:open-shared-library x11-xcb-lib)
      
      #+CCL (ccl:open-shared-library xrender-lib)
      
      #+CCL (ccl:open-shared-library xext-lib)

      (clet ((major-opcode #_<int>)
	     (event-base #_<int>)
	     (error-base #_<int>)
	     (major #_<int> 1)
	     (minor #_<int> 0))
	(unless (zerop (#_XkbQueryExtension xdisplay
					    (c-addr-of major-opcode)
					    (c-addr-of event-base)
					    (c-addr-of error-base)
					    (c-addr-of major)
					    (c-addr-of minor)))
	  (setf (xkb-available? x11-state) t
		(xkb-major-opcode x11-state) (cval-value major-opcode)
		(xkb-event-base x11-state) (cval-value event-base)
		(xkb-error-base x11-state) (cval-value error-base)
		(xkb-major x11-state) (cval-value major)
		(xkb-minor x11-state) (cval-value minor))

	  (clet ((supported #_<Bool>))

	    (unless (zerop (#_XkbSetDetectableAutoRepeat xdisplay #_True (c-addr-of supported)))
	      (unless (zerop (cval-value supported))
		(setf (xkb-detectable? x11-state) t))))

	  (clet ((state #_<XkbStateRec>))

	    (unless (zerop (#_XkbGetState xdisplay #_XkbUseCoreKbd (c-addr-of state)))
	      (setf (xkb-group x11-state) (c-cast '#_<unsigned int> (#_.group state))))

	    (#_XkbSelectEventDetails xdisplay #_XkbUseCoreKbd #_XkbStateNotify
				     #_XkbGroupStateMask #_XkbGroupStateMask))))

	    

      (create-x11-key-tables display)

      (with-slots (NULL
		   UTF8_STRING
		   ATOM_PAIR
		   CLUI_SELECTION)
	  display
	
      	(setf NULL (#_XInternAtom xdisplay "NULL" #_False)
	      UTF8_STRING (#_XInternAtom xdisplay "UTF8_STRING" #_False)
	      CLUI_SELECTION (#_XInternAtom xdisplay "CLUI_SELECTION" #_False)))

      (with-slots (TARGETS
		   MULTIPLE
		   PRIMARY
		   INCR
		   CLIPBOARD
		   CLIPBOARD_MANAGER
		   ATOM_PAIR
		   SAVE_TARGETS)

	  (display-clipboard-manager display)

	(setf TARGETS (#_XInternAtom xdisplay "TARGETS" #_False)
	      MULTIPLE (#_XInternAtom xdisplay "MULTIPLE" #_False)
	      PRIMARY (#_XInternAtom xdisplay "PRIMARY" #_False)
	      INCR (#_XInternAtom xdisplay "INCR" #_False)
	      CLIPBOARD (#_XInternAtom xdisplay "CLIPBOARD" #_False)
	      CLIPBOARD_MANAGER (#_XInternAtom xdisplay "CLIPBOARD_MANAGER" #_False)
	      ATOM_PAIR (#_XInternAtom xdisplay "ATOM_PAIR" #_False)
	      SAVE_TARGETS (#_XInternAtom xdisplay "SAVE_TARGETS" #_False)))

      (with-slots (XdndAware
		   XdndEnter
		   XdndPosition
		   XdndStatus
		   XdndActionCopy
		   XdndDrop
		   XdndFinished
		   XdndSelection
		   XdndTypeList
		   text/uri_list)
	  
	  (display-drag-and-drop display)

	(setf XdndAware (#_XInternAtom xdisplay "XdndAware" #_False)
	      XdndEnter (#_XInternAtom xdisplay "XdndEnter" #_False)
	      XdndPosition (#_XInternAtom xdisplay "XdndPosition" #_False)
	      XdndStatus (#_XInternAtom xdisplay "XdndStatus" #_False)
	      XdndActionCopy (#_XInternAtom xdisplay "XdndActionCopy" #_False)
	      XdndDrop (#_XInternAtom xdisplay "XdndDrop" #_False)
	      XdndFinished (#_XInternAtom xdisplay "XdndFinished" #_False)
	      XdndSelection (#_XInternAtom xdisplay "XdndSelection" #_False)
	      XdndTypeList (#_XInternAtom xdisplay "text/uri-list" #_False)))

      (with-slots (WM_PROTOCOLS
		   WM_STATE
		   WM_DELETE_WINDOW
		   NET_SUPPORTED
		   NET_SUPPORTING_WM_CHECK
		   NET_WM_ICON
		   NET_WM_PING
		   NET_WM_PID
		   NET_WM_NAME
		   NET_WM_ICON_NAME
		   NET_WM_BYPASS_COMPOSITOR
		   NET_WM_WINDOW_OPACITY
		   MOTIF_WM_HINTS
		   NET_WM_CM_Sx)
	  
	  (display-window-manager display)

	(setf WM_PROTOCOLS (#_XInternAtom xdisplay "WM_PROTOCOLS" #_False)
	      WM_STATE (#_XInternAtom xdisplay "WM_STATE" #_False)
	      WM_DELETE_WINDOW (#_XInternAtom xdisplay "WM_DELETE_WINDOW" #_False)
	      NET_SUPPORTED (#_XInternAtom xdisplay "_NET_SUPPORTED" #_False)
	      NET_SUPPORTING_WM_CHECK (#_XInternAtom xdisplay "_NET_SUPPORTING_WM_CHECK" #_False)
	      NET_WM_ICON (#_XInternAtom xdisplay "_NET_WM_ICON" #_False)
	      NET_WM_PING (#_XInternAtom xdisplay "_NET_WM_PING" #_False)
	      NET_WM_PID (#_XInternAtom xdisplay "_NET_WM_PID" #_False)
	      NET_WM_NAME (#_XInternAtom xdisplay "_NET_MN_NAME" #_False)
	      NET_WM_ICON_NAME (#_XInternAtom xdisplay "_NET_WM_ICON_NAME" #_False)
	      NET_WM_BYPASS_COMPOSITOR (#_XInternAtom xdisplay "_NET_WM_BYPASS_COMPOSITOR" #_False)
	      NET_WM_WINDOW_OPACITY (#_XInternAtom xdisplay "_NET_WM_WINDOW_OPACITY" #_False)
	      MOTIF_WM_HINTS (#_XInternAtom xdisplay "_MOTIF_WM_HINTS" #_False)
	      NET_WM_CM_Sx (#_XInternAtom xdisplay (format nil "_NET_WM_CM_S~A" screen-id) #_False)))
		
      (%detect-EWMH display)

      (create-helper-x11-window display)
      (create-hidden-x11-cursor display)

      (unless (zerop (#_XSupportsLocale))
	(#_XSetLocaleModifiers "")

	(#_XRegisterIMInstantiateCallback xdisplay
					  nil nil nil
					  input-method-instantiate-callback
					  nil))
					

      (setf (x11-initialized? x11-state) t))))
  

	      
