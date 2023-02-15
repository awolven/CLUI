(in-package :clui)
(noffi-syntax)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (noffi-syntax t))

;;#_{extern XIC XCreateIC (XIM,
;;			 char *, unsigned long,
;;			 char *, unsigned long,
;;			 char *, void *,
;;			 char *, long,
;;			 void *);
;;}


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

	(unless (window-monitor window)
	  (if (currently-resizable? window)

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

(defun get-x11-window-state (window)
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
      (setf (#_.client_data &callback) (cons-ptr (ccl::%int-to-ptr window-handle)
						 0 '#_<XPointer>))
      (when (display-input-method display)
	(setf (window-input-context window)
	      (#_XCreateIC (display-input-method display)
			   #_XNClientWindow
			   window-handle
			   #_XNFocusWindow
			   window-handle
			   #_XNDestroyCallback
			   &callback
			   #_XNInputStyle
			   (logior #_XIMPreeditNothing #_XIMStatusNothing)
			   nil)))

      (let ((ic (window-input-context window)))
	(when ic
	  (clet& ((&attribs #_<XWindowAttributes>))
	    (#_XGetWindowAttributes xdisplay window-handle &attribs)
	    
	    (clet ((filter #_<unsigned long>))
	      (let (#+NIL(one (ccl::make-cstring #_XNFilterEvents)))
		(unless (#_XGetICValues ic #_XNFilterEvents (c-addr-of filter) nil)
		  (#_XSelectInput xdisplay window-handle
				  (logior (#_.your_event_mask &attribs) (cval-value filter)))))))))
      (values))))

(defun %create-native-x11-window (window
				  &rest initargs
				  &key (display (window-display window))
				    (visual (#_DefaultVisual (h display) (default-screen-id display)))
				    (title "CLUI")
				    (xpos nil)
				    (ypos nil)
				    (width 640)
				    (height 480)
				    (depth (#_DefaultDepth (h display) (default-screen-id display)))
				    (decorated? t)
				    (resizable? t)
				    (floating? nil)
				    (maximized? nil)
				    instance-name
				    class-name
				    (parent (default-screen display))
				    (scale-to-monitor? t))

  (declare (ignore initargs))

  (let ((xdisplay (h display))
	(root (default-root-window-handle display)))	 

  (when scale-to-monitor?
    (setq width (* width (display-content-scale display))
	  height (* height (display-content-scale display))))

    (setf (window-colormap window) (#_XCreateColormap xdisplay
						      root
						      visual
						      #_AllocNone))

    (setf (currently-transparent? window) (%x11-visual-transparent? xdisplay visual))

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

	(unwind-protect
	     (setf (h window) (#_XCreateWindow xdisplay
					       (or (and parent (h parent)) root)
					       (or (and xpos (round xpos)) 0)
					       (or (and ypos (round ypos)) 0)
					       (or (and width (round width)) 640)
					       (or (and height (round height)) 480)
					       0 ;; border width
					       depth
					       #_InputOutput
					       visual
					       (logior #_CWBorderPixel #_CWColormap #_CWEventMask)
					       &wa))

	  (%release-x11-error-handler display))

	(when (null (h window))
	  (error "X11: Failed to create window."))

	(#_XSaveContext xdisplay (h window)
			(unique-context display)
			xdisplay)

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

	  (when (and NET_WM_STATE (not (window-monitor window)))

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
		    (setf (currently-maximized? window) t)))

		(unless (minusp count)
		  (#_XChangeProperty xdisplay (h window)
				     NET_WM_STATE #_XA_ATOM 32
				     #_PropModeReplace states (incf count))))))

	  (clet ((protocols #_<Atom[2]>))
	    (setf (c-aref protocols 0) WM_DELETE_WINDOW
		  (c-aref protocols 1) NET_WM_PING)
	    
	    (#_XSetWMProtocols xdisplay (h window) protocols 2))

	  (clet ((pid #_<long> (ccl::getpid)))
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

		(let ((resource-name (ccl::getenv "RESOURCE_NAME")))
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

	  (create-x11-input-context window)


	  (set-x11-window-title window title)


	  (multiple-value-bind (xpos ypos) (get-x11-window-pos window)
	    (setf (x window) xpos
		  (y window) ypos))

	  (multiple-value-bind (width height) (get-x11-window-size window)
	    (setf (width window) width
		  (height window) height))
	  t)))))

(defun show-x11-window (window)

  (when (get-x11-window-visible window)
    (return-from show-x11-window (values)))

  (#_XMapWindow (h (window-display window)) (h window))
  (wait-for-visibility-notify window)
  (values))

(defun set-x11-window-title (window title)
  (let* ((display (window-display window))
	 (window-manager (display-window-manager display))
	 (clipboard (display-clipboard display)))
    
    (when (and title (stringp title) (not (string= title "")))
      #+NIL
      (#_Xutf8SetWMProperties (h (window-display window))
			      (h window)
			      title title
			      nil 0
			      nil nil nil)
    
      (with-slots (NET_WM_NAME
		   NET_WM_ICON_NAME)
	
	  window-manager

	(with-slots (UTF8_STRING)
	    clipboard

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
	(let ((hints (#_AllocSizeHints)))

	  (unless (zerop (#_XGetWMNormalHints (h display) (h window) (c-addr-of supplied)))

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

(defun set-x11-window-size (window width height)
  (if (window-monitor window)
      
      (when (eq (monitor-window (window-monitor window)) window)
	(acquire-x11-monitor window (window-monitor window)))

      (progn

	(unless (currently-resizable? window)
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
    (setf (#_.flags &hints) #_MWM_HINTS_DECORATIONS
	  (#_.decorations &hints) (if enabled? #_MWM_DECOR_ALL 0))

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
    (and pf (#_.alphaMask (c->-addr pf '#_direct)))))
