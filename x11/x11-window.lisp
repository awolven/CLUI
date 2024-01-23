(in-package :clui)

(defconstant MWM_HINTS_DECORATIONS 2)
(defconstant MWM_DECOR_ALL 0)

(defmethod initialize-window-devices ((window x11:window-mixin) &rest args &key &allow-other-keys)
  (declare (ignore args))
  (values))

(defun update-normal-hints (window width height)
  (let ((display (window-display window)))
    (let ((hints (#_XAllocSizeHints)))
      (clet& ((&supplied #_<long>))
	(#_XGetWMNormalHints (h display)
			     (h window)
			     hints
			     &supplied)

	(setf (#_.flags hints)
	      (logand (#_.flags hints)
		      (lognot (logior #_PMinSize #_PMaxSize #_PAspect))))

	(unless (window-fullscreen-monitor window)
	  (if (last-resizable? window)

	      (progn
		(unless (or (eq (window-min-width window) :dont-care)
			    (eq (window-min-height window) :dont-care))
		  (setf (#_.flags hints) (logior (#_.flags hints) #_PMinSize))
		  (setf (#_.min_width hints) (round (window-min-width window))
			(#_.min_height hints) (round (window-min-height window))))
		
		(unless (or (eq (window-max-width window) :dont-care)
			    (eq (window-max-height window) :dont-care))
		  (setf (#_.flags hints) (logior (#_.flags hints) #_PMaxSize))
		  (setf (#_.max_width hints) (round (window-max-width window))
			(#_.max_height hints) (round (window-max-height window))))

		(unless (or (eq (window-aspect-numer window) :dont-care)
			    (eq (window-aspect-denom window) :dont-care))
		  (setf (#_.flags hints) (logior (#_.flags hints) #_PAspect))
		  (let ((&min-aspect (c->-addr hints '#_min_aspect))
			(&max-aspect (c->-addr hints '#_max_aspect)))
		    (setf (#_.x &min-aspect)
			  (setf (#_.x &max-aspect)
				(round (window-aspect-numer window))))
		    (setf (#_.y &min-aspect)
			  (setf (#_.y &max-aspect)
				(round (window-aspect-denom window)))))))

	      (setf (#_.flags hints) (logior (#_.flags hints) #_PMinSize #_PMaxSize)
		    (#_.min_width hints) (setf (#_.max_width hints) (round width))
		    (#_.min_height hints) (setf (#_.max_height hints) (round height)))))

	(#_XSetWMNormalHints (h display) (h window) hints)
	(#_XFree hints)
	(values)))))

(defun wait-for-visibility-notify (window)
  (let* ((display (window-display window))
	 (xdisplay (h display))
	 (handle (h window))
	 (timeout 0.1d0))
    (clet& ((&dummy #_<XEvent>))

      (loop while (zerop (#_XCheckTypedWindowEvent xdisplay handle
						   #_VisibilityNotify
						   &dummy))
	    unless (wait-for-x11-event display timeout)
	      do (return nil)
	    finally (return t)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
#_{ struct window_state {
#_CARD32 state;
#_Window icon;
};
}
)

(defun get-x11-window-iconified (window)
  (let ((result #_WithdrawnState)
	(display (window-display window)))

    (with-slots (WM_STATE) (display-window-manager display)
      
      (clet& ((&state #_<struct window_state>))
	
	(when (>= (%get-x11-window-property (h display)
					    (h window)
					    WM_STATE WM_STATE &state)
		  2)
	  (setq result (#_.state &state)))

	result))))



(defun %get-x11-window-property (xdisplay window-handle property type value)
  (clet& ((&actual-type #_<Atom>)
	  (&actual-format #_<int>)
	  (&bytes-after #_<unsigned long>))
    (clet ((item-count #_<unsigned long>))
      (let ((&item-count (c-addr-of item-count)))

	(unless (= #_Success
		   (#_XGetWindowProperty xdisplay
					 window-handle
					 property
					 0
					 #_LONG_MAX
					 #_False
					 type
					 &actual-type
					 &actual-format
					 &item-count
					 &bytes-after
					 value))
	  (warn "XGetWindowProperty failed."))

	(cval-value item-count)))))

(defun window-from-window-handle (window-handle-integer)
  (gethash window-handle-integer *window-handle->window-table*))

(defcfun (input-context-destroy-callback #_<void>)
	 ((ic #_<XIC>) (client-data #_<XPointer>) (call-data #_<XPointer>))
  (declare (ignore ic call-data))
  (let ((window (window-from-window-handle (sap-int (ptr-value client-data)))))
    (when window
      (setf (window-input-context window) nil))
    (values)))




(defun create-x11-input-context (window)
  (let* ((window-handle (h window))
	 (display (window-display window))
	 (xdisplay (h display)))
    (clet& ((&callback #_<XIMCallback>))
      (setf (#_.callback &callback) input-context-destroy-callback)
      (setf (#_.client_data &callback) (c-coerce (noffi::int-ptr window-handle) '#_<XPointer>))

      (setf (window-input-context window)
	    (noffi::c-funcall #_XCreateIC (display-input-method display)
			      #_XNInputStyle
			      (logior #_XIMPreeditNothing #_XIMStatusNothing)
			      #_XNClientWindow
			      window-handle
			      #_XNFocusWindow
			      window-handle
			      #_XNDestroyCallback
			      &callback
			      0))

      (let ((ic (window-input-context window)))
	(when ic
	  (clet& ((&attribs #_<XWindowAttributes>))
	    (#_XGetWindowAttributes xdisplay window-handle &attribs)
	    
	    (clet ((filter #_<unsigned long>))
	      (when (null (noffi::c-funcall #_XGetICValues ic #_XNFilterEvents (c-addr-of filter) 0))
		(#_XSelectInput xdisplay window-handle
				(logior (#_.your_event_mask &attribs) (cval-value filter))))))))
      (values))))

(defun set-x11-window-cursor (window cursor)
  (declare (ignorable cursor))
  ;;(when (win32-cursor-in-content-area? window)
    (update-x11-cursor-image window));;)  

(defun update-x11-cursor-image (window)
  (if (or (eq (window-cursor-mode window) :normal)
	  (eq (window-cursor-mode window) :captured))
      (if (window-cursor window)
	  (#_XDefineCursor (h (window-display window))
			   (h window)
			   (h (window-cursor window)))
	  (#_XUndefineCursor (h (window-display window))
			     (h window)))
      (#_XDefineCursor (h (window-display window))
			   (h window)
			   (h (hidden-cursor window))))
  (values))


(defun create-native-x11-window (window
				 &rest initargs
				 &key (visible? t)
				 &allow-other-keys)
  (apply #'%create-native-x11-window window initargs)
  (when visible? (show-x11-window window))
  window)

(defun %create-native-x11-window (window
				  &rest initargs
				  &key
				    display
				    parent
				    root
				    (visual (#_DefaultVisual (h display) (screen-id root)))
				    (title "CLUI")
				    (xpos nil)
				    (ypos nil)
				    (width 640)
				    (height 480)
				    (depth (#_DefaultDepth (h display) (screen-id root)))
				    (decorated? t)
				    (resizable? t)
				    (floating? nil)
				    (maximized? nil)
				    instance-name
				    class-name
				    (scale-to-monitor? t)
				  &allow-other-keys)

  (declare (ignore initargs))

  (let ((xdisplay (h display))
	(root (h root)))

    (when scale-to-monitor?
      (setq width (* width (display-content-scale display))
	    height (* height (display-content-scale display))))

    (setf (window-colormap window) (#_XCreateColormap xdisplay
						      root
						      visual
						      #_AllocNone))

    (setf (last-transparent? window) (%x11-visual-transparent? xdisplay visual))

    (clet ((wa #_<XSetWindowAttributes>))
      (let ((&wa (c-addr-of wa)))
	(#_memset &wa 0 (c-sizeof-type '#_<XSetWindowAttributes>))
	(setf (#_.colormap &wa) (window-colormap window))
	(setf (#_.event_mask &wa) (logior #_StructureNotifyMask #_KeyPressMask #_KeyReleaseMask
					  #_PointerMotionMask #_ButtonPressMask #_ButtonReleaseMask
					  #_ExposureMask #_FocusChangeMask #_VisibilityChangeMask
					  #_EnterWindowMask #_LeaveWindowMask #_PropertyChangeMask))

	(setf (window-parent window) parent)
	
	(%grab-x11-error-handler display)

	(let ((w (or (and width (round width)) 640))
	      (h (or (and height (round height)) 480)))
	  (unwind-protect
	       (setf (h window) (#_XCreateWindow xdisplay
						 (or (and parent (h parent)) (h root))
						 (or (and xpos (round xpos)) 0)
						 (or (and ypos (round ypos)) 0)
						 w h
						 0 ;; border width
						 depth
						 #_InputOutput
						 visual
						 (logior #_CWBorderPixel #_CWColormap #_CWEventMask)
						 &wa))

	    (%release-x11-error-handler display))

	  (when (null (h window))
	    (error "X11: Failed to create window."))

	  (initialize-window-devices window :width w :height h))

	(setf (gethash (h window) *window-handle->window-table*) window)

	(let ((xwindow (c-coerce (noffi::int-ptr (h window)) '#_<XPointer>)))
	  (#_XSaveContext xdisplay (h window)
			  (unique-context display)
			  xwindow))

	(unless decorated?
	  (set-x11-window-decorated window nil))

	(with-slots (NET_WM_STATE
		     NET_WM_STATE_ABOVE
		     NET_WM_STATE_MAXIMIZED_VERT
		     NET_WM_STATE_MAXIMIZED_HORZ
		     WM_DELETE_WINDOW
		     NET_WM_PING
		     NET_WM_PID
		     NET_WM_WINDOW_TYPE
		     NET_WM_WINDOW_TYPE_NORMAL)
	    
	    (display-window-manager display)

	  (when (and NET_WM_STATE (not (window-fullscreen-monitor window)))

	    (clet ((states #_<Atom[3]>))
	      (let ((count -1))

		(when floating?
		  (when NET_WM_STATE_ABOVE
		    (setf (c-aref states (incf count)) NET_WM_STATE_ABOVE)))

		(when maximized?
		  (when (and NET_WM_STATE_MAXIMIZED_VERT
			     NET_WM_STATE_MAXIMIZED_HORZ)
		    (setf (c-aref states (incf count)) NET_WM_STATE_MAXIMIZED_VERT
			  (c-aref states (incf count)) NET_WM_STATE_MAXIMIZED_HORZ)
		    (setf (last-maximized? window) t)))

		(unless (minusp count)
		  (#_XChangeProperty xdisplay (h window)
				     NET_WM_STATE #_XA_ATOM 32
				     #_PropModeReplace states (incf count))))))

	  (clet ((protocols #_<Atom[2]>))
	    (setf (c-aref protocols 0) WM_DELETE_WINDOW
		  (c-aref protocols 1) NET_WM_PING)
	    
	    (#_XSetWMProtocols xdisplay (h window) protocols 2))

	  (clet ((pid #_<long> #+CCL (ccl::getpid) #+SBCL (sb-unix:unix-getpid)))
	    (#_XChangeProperty xdisplay (h window)
			       NET_WM_PID #_XA_CARDINAL 32
			       #_PropModeReplace
			       (c-addr-of pid) 1))

	  (when (and NET_WM_WINDOW_TYPE NET_WM_WINDOW_TYPE_NORMAL)
	    (clet ((type #_<Atom> NET_WM_WINDOW_TYPE_NORMAL))
	      (#_XChangeProperty xdisplay (h window)
				 NET_WM_WINDOW_TYPE #_XA_ATOM 32
				 #_PropModeReplace (c-addr-of type) 1)))

	  (let ((hints (#_XAllocWMHints)))

	    (unless hints
	      (error "X11: Failed to allocate WM hints."))

	    (setf (#_.flags hints) #_StateHint
		  (#_.initial_state hints) #_NormalState)

	    (#_XSetWMHints xdisplay (h window) hints)
	    (#_XFree hints))

	  (let ((hints (#_XAllocSizeHints)))

	    (unless hints
	      (error "X11: Failed to allocate size hints."))

	    (unless resizable?

	      (setf (#_.flags hints) (logior (#_.flags hints) #_PMinSize #_PMaxSize))
	      (setf (#_.min_width hints)
		    (setf (#_.max_width hints) (round width)))
	      (setf (#_.min_height hints)
		    (setf (#_.max_height hints) (round height))))

	    (when (and xpos ypos)
	      (setf (#_.flags hints) (logior (#_.flags hints) #_PPosition))
	      (setf (#_.x hints) 0)
	      (setf (#_.y hints) 0))

	    (setf (#_.flags hints) (logior (#_.flags hints) #_PWinGravity)
		  (#_.win_gravity hints) #_StaticGravity)

	    (#_XFree hints))

	  (let ((hint (#_XAllocClassHint)))

	    (if (and instance-name (not (string= instance-name ""))
		     class-name (not (string= class-name "")))

		(setf (#_.res_name hint) instance-name
		      (#_.res_class hint) class-name)

		(let ((resource-name #+CCL (ccl::getenv "RESOURCE_NAME") #+SBCL (sb-posix:getenv "RESOURCE_NAME")))
		  (cond ((and resource-name (not (string= resource-name "")))
			 (setf (#_.res_name hint) resource-name))
			((and title (not (string= title "")))
			 (setf (#_.res_name hint) title))
			(t (setf (#_.res_name hint) "clui-application")))

		  (cond ((and title (not (string= title "")))
			 (setf (#_.res_class hint) title))
			(t (setf (#_.res_class hint) "CLUI-application")))))

	    (#_XSetClassHint xdisplay (h window) hint)
	    (#_XFree hint))

	  (with-slots (XdndAware
		       version)
	      (display-drag-and-drop display)
	    (clet ((version #_<Atom> version))
	      (#_XChangeProperty xdisplay (h window)
				 XdndAware #_XA_ATOM 32
				 #_PropModeReplace (c-addr-of version) 1)))

	  (when (display-input-method display)
	    (create-x11-input-context window))

	  (set-x11-window-title window title)

	  (multiple-value-bind (xpos ypos) (get-x11-window-pos window)
	    (setf (last-pos-x window) xpos
		  (last-pos-y window) ypos))

	  (multiple-value-bind (width height) (get-x11-window-size window)
	    (setf (last-width window) width
		  (last-height window) height))

	  (multiple-value-bind (x y) (get-x11-window-cursor-pos window)
	    (setf (cursor-warp-pos-x window) x
		  (cursor-warp-pos-y window) y))
	  
	  t)))))

(defun get-x11-window-content-scale (window)
  (let ((scale (display-content-scale (window-display window))))
    (values scale scale)))

(defun get-x11-window-cursor-pos (window)
  (clet ((root #_<Window>)
	 (child #_<Window>)
	 (root-x #_<int>)
	 (root-y #_<int>)
	 (child-x #_<int>)
	 (child-y #_<int>)
	 (mask #_<unsigned int>))

    (#_XQueryPointer (h (window-display window)) (h window)
		     (c-addr-of root) (c-addr-of child)
		     (c-addr-of root-x) (c-addr-of root-y)
		     (c-addr-of child-x) (c-addr-of child-y)
		     (c-addr-of mask))

    (values (cval-value child-x) (cval-value child-y))))

(defun set-x11-window-cursor-pos (window x y)
  (setf (cursor-warp-pos-x window) x
	(cursor-warp-pos-y window) y)

  (let ((xdisplay (h (window-display window))))
    (#_XWarpPointer xdisplay #_None (h window) 0 0 0 0 (round x) (round y))
    (#_XFlush xdisplay)
    (values)))

(defun show-x11-window (window)

  (when (get-x11-window-visible window)
    (return-from show-x11-window (values)))

  (#_XMapWindow (h (window-display window)) (h window))
  (wait-for-visibility-notify window)
  (values))

(defun set-x11-window-title (window title)
  (let* ((display (window-display window))
	 (window-manager (display-window-manager display)))
    
    (when (and title (stringp title) (not (string= title "")))
      (#_Xutf8SetWMProperties (h display)
			      (h window)
			      title title
			      nil 0
			      nil nil nil)
    
      (with-slots (NET_WM_NAME
		   NET_WM_ICON_NAME)
	
	  window-manager

	(with-slots (UTF8_STRING)
	    display

	  (#_XChangeProperty (h display)
			     (h window)
			     NET_WM_NAME UTF8_STRING 8
			     #_PropModeReplace title (#_strlen title))

	  (#_XChangeProperty (h display)
			     (h window)
			     NET_WM_ICON_NAME UTF8_STRING 8
			     #_PropModeReplace title (#_strlen title))))
    
      (#_XFlush (h display)))
    (values)))

#+NIL
(defun set-x11-window-icon (window count images)
  (if (plusp count)
      
      (let ((long-count 0))
	
	(loop for i from 0 below count
	      do (incf long-count (+ 2 (image-width (elt images i)) (image-height (elt images i)))))

	(let ((icon (#_malloc (* long-count (c-sizeof-type '#_<unsigned long>))))
	      (target icon))

	  (loop for i from 0 below count
		do (setf (noffi::c-aref target i) (image-width (elt images i))
			 (noffi::c-aref target (1+ i)) (image-height (elt images i))))))))

(defun get-x11-window-visible (window)
  (clet& ((&wa #_<XWindowAttributes>))
    (#_XGetWindowAttributes (h (window-display window)) (h window) &wa)
    (= (#_.map_state &wa) #_IsViewable)))

(defun get-x11-window-pos (window)
  (clet ((dummy #_<Window>)
	 (x #_<int>)
	 (y #_<int>))

    (#_XTranslateCoordinates (h (window-display window)) (h window) (h (window-parent window))
			     0 0 (c-addr-of x) (c-addr-of y) (c-addr-of dummy))

    (values (cval-value x) (cval-value y))))

(defun set-x11-window-pos (window x y)

  (let ((display (window-display window)))
    (unless (get-x11-window-visible window)
      (clet ((supplied #_<long>))
	(let ((hints (#_XAllocSizeHints)))

	  (unless (zerop (#_XGetWMNormalHints (h display) (h window) hints (c-addr-of supplied)))

	    (setf (#_.flags hints) (logior (#_.flags hints) #_PPosition)
		  (#_.x hints) (setf (#_.y hints) 0))

	    (#_XSetWMNormalHints (h display) (h window) hints))

	  (#_XFree hints))))

    (#_XMoveWindow (h display) (h window) (round x) (round y))
    (#_XFlush (h display))
    (values)))

(defun get-x11-window-size (window)
  (clet& ((&attribs #_<XWindowAttributes>))
    (#_XGetWindowAttributes (h (window-display window)) (h window) &attribs)

    (values (#_.width &attribs)
	    (#_.height &attribs))))

(defun get-x11-window-framebuffer-size (window)
  (get-x11-window-size window))

(defun set-x11-window-size (window width height)
  (if (window-fullscreen-monitor window)
      
      (when (eq (monitor-window (window-fullscreen-monitor window)) window)
	(acquire-x11-monitor window (window-fullscreen-monitor window)))

      (progn

	(unless (last-resizable? window)
	  (update-normal-hints window width height))

	(#_XResizeWindow (h (window-display window)) (h window) (round width) (round height))))

  (#_XFlush (h (window-display window)))
  (values))

(eval-when (:compile-toplevel :load-toplevel :execute)
  #_{
  struct decorated_hints {
  unsigned long flags;
  unsigned long functions;
  unsigned long decorations;
  long input_mode;
  unsigned long status;
  } ;
  }
  )

(defun set-x11-window-decorated (window enabled?)
  (clet& ((&hints #_<struct decorated_hints>))
    (setf (#_.flags &hints) MWM_HINTS_DECORATIONS
	  (#_.decorations &hints) (if enabled? MWM_DECOR_ALL 0))

    (with-slots (MOTIF_WM_HINTS)
	(display-window-manager (window-display window))

      (#_XChangeProperty (h (window-display window)) (h window)
			 MOTIF_WM_HINTS
			 MOTIF_WM_HINTS 32
			 #_PropModeReplace
			 &hints
			 (/ (c-sizeof-type '#_<struct decorated_hints>) (c-sizeof-type #_<long>)))
      (values))))
		       

(defun %x11-visual-transparent? (xdisplay visual)
  (let ((pf (#_XRenderFindVisualFormat xdisplay visual)))
    (and pf (not (zerop (#_.alphaMask (c->-addr pf '#_direct)))))))

(defun set-x11-raw-mouse-motion (window value)
  (let ((display (window-display window)))
    (unless (and (xi-available? (display-x11-state display))
		 (eq (disabled-cursor-window display) window))
      (return-from set-x11-raw-mouse-motion nil))

    (if value
	(progn (enable-x11-raw-mouse-motion window)
	       t)
	(progn (disable-x11-raw-mouse-motion window)
	       nil))))

(defun enable-x11-raw-mouse-motion (window)
      #+NIL

  (clet ((em #_<XIEventMask>))
    (let ((size (#_XIMaskLen #_XI_RawMotion))
	  (mask (#_malloc size)))
    (unwind-protect
	 (let ((&em (c-addr-of em)))
	   (#_memset &mask 0 size)
	   (setf (#_.deviceid &em) #_XIAllMasterDevices)
	   (setf (#_.mask_len &em) size)
	   (setf (#_.mask &em) mask)
	   (#_XISetMask mask #_XI_RawMotion)
	   
	   (#_XISelectEvents (h (window-display window)) (default-root-window-handle (window-display window))
			     &em 1)
	   (values))
      (#_free mask)))))

(defun get-x11-window-screen-and-root (window)
  (let ((display (window-display window)))
    (clet& ((&attribs #_<XWindowAttributes>))
      (#_XGetWindowAttributes (h display) (h window) &attribs)
      (values (#_.screen &attribs)
	      (#_.root &attribs)))))
  
(defun get-x11-window-monitor (window)
  ;; we're going by the upper left hand coordinate of the window
  ;; being within the monitor's coordinates
  (let* ((display (window-display window))
	 (x11-state (display-x11-state display)))
    (unless (display-monitors display)
      (poll-x11-monitors display))
    (clet& ((&attribs #_<XWindowAttributes>))
      (#_XGetWindowAttributes (h display) (h window) &attribs)
      (labels ((no-xinerama-method ()
		 (let ((sr (#_XRRGetScreenResourcesCurrent (h display) (#_.root &attribs))))
		   (unwind-protect 
			(loop for i from 0 below (#_.noutput sr)
			      do (let* ((output (c-aref (#_.outputs sr) i))
					(oi (#_XRRGetOutputInfo (h display) sr output)))
				   (unwind-protect
					(when (and (= (#_.connection oi) #_RR_Connected)
						   (not (noffi::ptr-nullptr-p (#_.crtcs oi))))
					  (let* ((crtc (#_.crtc oi))
						 (ci (#_XRRGetCrtcInfo (h display) sr crtc)))
					    (unwind-protect
						 (unless (= 0 (#_.noutput ci))
						   (let ((x (#_.x ci))
							 (y (#_.y ci)))
						     (when (and (<= x (#_.x &attribs) (+ x (#_.width ci)))
								(<= y (#_.y &attribs) (+ y (#_.height ci))))
						       (mapcar #'(lambda (monitor)
								   (and (equalp (monitor-x11-crtc monitor) crtc)
									(equalp (monitor-x11-output monitor) output))
								   (return-from get-x11-window-monitor monitor))
							       (display-monitors display)))))
					      (#_XRRFreeCrtcInfo ci))))
				     (#_XRRFreeOutputInfo oi))))
		     (#_XRRFreeScreenResources sr))))

	       (xinerama-method ()
		 (clet ((screen-count #_<int> 0))
		   (let ((xinerama-screens (#_XineramaQueryScreens (h display) (c-addr-of screen-count)))
			 (xinerama-screen-index))
		     (progn
		       (unwind-protect
			    (loop for i from 0 below (cval-value screen-count)
				  do (let ((xorg (#_.x_org (c-aref xinerama-screens i)))
					   (yorg (#_.y_org (c-aref xinerama-screens i))))
				       (when (and (<= xorg
						      (#_.x &attribs)
						      (+ xorg (#_.width (c-aref xinerama-screens i))))
						  (<= yorg
						      (#_.y &attribs)
						      (+ yorg (#_.height (c-aref xinerama-screens i)))))
					 ;; upper left coordinate of window is in xinerama-screen
					 (setq xinerama-screen-index i)
					 (return))))
			 (#_XFree xinerama-screens))
		       (if xinerama-screen-index
			   (or
			    (mapcar #'(lambda (monitor)
					(when (and (monitor-x11-index monitor)
						   (= (monitor-x11-index monitor) xinerama-screen-index))
					  (return-from get-x11-window-monitor monitor)))
				    (display-monitors display))
			    (no-xinerama-method))
			   (no-xinerama-method)))))))
	
	(if (and x11-state (xinerama-available? x11-state))
	    (xinerama-method)
	    (no-xinerama-method))))))
	      



(defun destroy-x11-window (window)
  (let ((display (window-display window)))
    
    (when (disabled-cursor-window display)
      (enable-x11-cursor window))

    (when (window-fullscreen-monitor window)
      (release-monitor window (window-fullscreen-monitor window)))

    (when (window-input-context window)
      (#_XDestroyIC (window-input-context window))
      (setf (window-input-context window) nil))

    (when (h window)
      (#_XDeleteContext (h display) (h window) (unique-context display))
      (#_XUnmapWindow (h display) (h window))
      (#_XDestroyWindow (h display) (h window))
      (remhash (h window) *window-handle->window-table*)
      (setf (h window) nil))

    (when (window-colormap window)
      (#_XFreeColormap (h display) (window-colormap window))
      (setf (window-colormap window) nil))

    (if (eq window (display-window-list-head display))
	  
	(setf (display-window-list-head display) (window-next window))
	  
	(do ((candidate (display-window-list-head display) (window-next candidate)))
	    ((null candidate))

	  (when (eq (window-next candidate) window)
	    (setf (window-next candidate) (window-next window))
	    (return))))

    (#_XFlush (h display))))

      
	
      
      

    
