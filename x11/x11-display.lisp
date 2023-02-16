(in-package :clui)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (noffi-syntax t))

(defvar *xim->display* (make-hash-table :test #'eq))
(defvar *xdisplay->display* (make-hash-table :test #'eq))

(defun display-for-xim (xim)
  (gethash (ccl::%ptr-to-int (ptr-value xim)) *xim->display*))

(defun display-for-xdisplay (xdisplay)
  (gethash (ccl::%ptr-to-int (ptr-value xdisplay)) *xdisplay->display*))

(defun (setf display-for-xdisplay) (display xdisplay)
  (setf (gethash (ccl::%ptr-to-int (ptr-value xdisplay)) *xdisplay->display*) display))

(defcfun (x11-error-handler #_<int>)
	 ((xdisplay #_<Display*>)
	  (event #_<XErrorEvent*>))

  (let ((display (display-for-xdisplay xdisplay)))
    (when display
      (setf (display-error-code display) (#_.error_code event)))
    0))

(defun %grab-x11-error-handler (display)
  (assert (null (display-error-handler display)))
  (setf (display-error-code display) #_Success)
  (setf (display-error-handler display) (#_XSetErrorHandler x11-error-handler))
  (values))

(defun %release-x11-error-handler (display)
  (#_XSync (h display) #_False)
  (#_XSetErrorHandler (display-error-handler display))
  (setf (display-error-handler display) nil)
  (values))

(defun wait-for-x11-event (display timeout)
  (setq timeout (coerce timeout 'double-float))
  (clet ((fd #_<struct pollfd>))
    (let ((&fd (c-addr-of fd))
	  (xdisplay (h display)))
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
	    (xdisplay (h display)))
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
	(rms (#_XResourceManagerString (h display))))

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

(defcfun (input-method-destroy-callback #_<void>)
 	 ((im #_<XIM>) (client-data #_<XPointer>) (call-data #_<XPointer>))
  (declare (ignore client-data call-data))
  (let ((display (display-for-xim im)))
    (break "22")
    (when display
      (break "33")
      (remhash (cval-value im) *xim->display*)
      (setf (display-input-method display) nil))
    (values)))

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

      (when (not (has-usable-input-method-style? display))
	(#_XCloseIM (display-input-method display))
	(setf (display-input-method display) nil)))

    (when (display-input-method display)
      (setf (gethash (cval-value (display-input-method display)) *xim->display*) display)
      (clet ((callback #_<XIMCallback>))
	(let ((&callback (c-addr-of callback)))
	  (setf (#_.callback &callback) input-method-destroy-callback
		(#_.client_data &callback) nil)
	  (let (#+NIL(one (ccl::make-cstring )))
	    (#_XSetIMValues (display-input-method display)
			    #_XNDestroyCallback &callback 0))

	  (do ((window (display-window-list-head display) (window-next window)))
	      ((null window))
	    (create-x11-input-context window)))))
    (values)))

(defun x11-selection-event? (display event pointer)
  (declare (ignore pointer))
  (block nil
    (unless (equalp (#_.window (c->-addr event #_.xany)) (helper-window-handle display))
      (return nil))

    (let ((type (#_.type event)))
      (return (or (= type #_SelectionRequest)
		  (= type #_SelectionNotify)
		  (= type #_SelectionClear))))))

(defun x11-frame-extents-event? (display event pointer)
  (and (= (#_.type event) #_PropertyNotify)
       (= (#_.state (c->-addr event '#_xproperty)) #_PropertyNewValue)
       (= (#_.window (c->-addr event '#_xproperty)) pointer)
       (= (#_.atom (c->-addr event '#_xproperty))
	  (slot-value (display-window-manager display) 'NET_FRAME_EXTENTS))))

(defun x11-sel-prop-new-value-notify? (display event pointer)
  (declare (ignore display))
  (let* ((notification (c-cast '#_<XEvent*> pointer))
	 (&xselection (c->-addr notification '#_xselection)))
    (and (= (#_.type event) #_PropertyNotify)
	 (= (#_.state (c->-addr event '#_xproperty)) #_PropertyNewValue)
	 (= (#_.window (c->-addr event '#_xproperty)) (#_.requestor &xselection)
	    (= (#_.atom (c->-addr event '#_xproperty)) (#_.property &xselection))))))

(defun send-event-to-wm (window type a b c d e)
  (clet& ((&event #_<XEvent>))
    (setf (#_.type &event) #_ClientMessage)
    (let ((&xclient (c->-addr &event '#_xclient)))
      (setf (#_.window &xclient) (h window)
	    (#_.format &xclient) 32
	    (#_.message_type &xclient) type)
      (let* ((l (#_.l (c->-addr &xclient '#_data))))
	(setf (c-aref l 0) a
	      (c-aref l 1) b
	      (c-aref l 2) c
	      (c-aref l 3) d
	      (c-aref l 4) e)
	(let ((display (window-display window)))
	  (#_XSendEvent (h display) (default-root-window-handle display)
			#_False
			(logior #_SubstructureNotifyMask #_SubstructureRedirectMask)
			&event)
	  (values))))))

(defun has-usable-input-method-style? (display)
  (block nil
    (let ((found? nil))
      (clet ((styles #_<XIMStyles*> #_NULL))
	(let ((&styles (c-addr-of styles)))

	  (let (#+NIL(one (ccl::make-cstring #_XNQueryInputStyle)))
	    (when (#_XGetIMValues (display-input-method display)
				  #_XNQueryInputStyle &styles 0)
	      (return nil)))

	  (loop for i from 0 below (#_.count_styles styles)
		when (= (cval-value (c-aref (#_.supported_styles styles) i))
			(logior #_XIMPreeditNothing
			 #_XIMStatusNothing))
		  do (setq found? t)
		     (return))
	  
	  (#_XFree styles)
	  (return found?))))))
	  


(defun create-x11-cursor (display image xhot yhot)
  (let ((native (#_XcursorImageCreate (image-width image) (image-height image))))
    (unless native
      (return-from create-x11-cursor nil))

    (setf (#_.xhot native) (round xhot)
	  (#_.yhot native) (round yhot))

    (let ((source (image-pixels image))
	  (target (#_.pixels native)))

      (loop for i from 0 below (* (image-width image) (image-height image))
	    for source-index from 0 by 4
	    do (let ((alpha (aref source (+ source-index 3))))
		   
		 (setf (c-aref target i) (logior (ash alpha 24)
						 (floor (* (aref source (+ source-index 0)) alpha) 255)
						 (floor (* (aref source (+ source-index 1)) alpha) 255)
						 (floor (* (aref source (+ source-index 2)) alpha) 255)))))
	
      (prog1 (make-instance 'clui:cursor
			    :h (#_XcursorImageLoadCursor (h display) native))
	(#_XcursorImageDestroy native)))))

(defun translate-state (state)
  (let ((mods 0))

    (when (logtest state #_ShiftMask)
      (setq mods (logior mods +shift-modifier+)))
    (when (logtest state #_ControlMask)
      (setq mods (logior mods +right-ctrl-modifier+ +left-ctrl-modifier+)))
    (when (logtest state #_Mod1Mask)
      (setq mods (logior mods +right-alt-modifier+ +left-alt-modifier+)))
    (when (logtest state #_Mod4Mask)
      (setq mods (logior mods +super-modifier+)))

    mods))
  

(defun process-event (display event)

  (block nil
    (let ((x11-state (display-x11-state display))
	  (keycode)
	  (filtered nil)
	  (event-type (cval-value (#_.type event))))
      
      (when (or (= event-type #_KeyPress) (= event-type #_KeyRelease))
	(setq keycode (#_.keycode (c->-addr event '#_xkey))))

      (setq filtered (#_XFilterEvent event #_None))
      
      (when (randr-available? x11-state)
	(when (= event-type (+ (randr-event-base x11-state) #_RRNotify))
	  (#_XRRUpdateConfiguration event)
	  (poll-x11-monitors display)
	  (return)))
      
      (when (xkb-available? x11-state)
	(when (= event-type (+ (xkb-event-base x11-state) #_XkbEventCode))
	  (setq event (c-cast '#_<XkbEvent*> event))
	  (when (and (= (#_.xkb_type (c->-addr event '#_any)) #_XkbStateNotify)
		     (logtest (#_.changed (c->-addr event '#_state)) #_XkbGroupStateMask))
	    (setf (xkb-group x11-state) (#_.group (c->-addr event '#_state))))
	  (return)))

      (when (= event-type #_GenericEvent)

	(when (xi-available? x11-state)

	  (let ((window (disabled-cursor-window display)))

	    (when (and window (last-raw-mouse-motion? window)
		       (= (#_.extension (c->-addr event '#_xcookie)) (xi-major-opcode x11-state))
		       (#_XGetEventData (h display) (#_.xcookie event))
		       (= (#_.evtype (c->-addr event '#_xcookie)) #_XI_RawMotion)) ;; whew.

	      (let ((re (#_.data (c->-addr event '#_xcookie))))

		(unless (zerop (#_.mask_len (c->-addr re '#_valuators)))

		  (let ((values (#_.raw_values re))
			(xpos (virtual-cursor-xpos window))
			(ypos (virtual-cursor-ypos window)))

		    (unless (zerop (#_XIMaskIsSet (#_.mask (c->-addr re '#_valuators)) 0))
		      (setq xpos (+ xpos (c-aref values 0)))
		      (incf values))

		    (unless (zerop (#_XIMaskIsSet (#_.mask (c->-addr re '#_valuators)) 1))
		      (setq ypos (+ ypos (c-aref values 0)))

		      (handle-event window (make-instance 'pointer-motion-event
							  :x xpos
							  :y ypos)))))))))

	(return))

      (when (= event-type #_SelectionRequest)

	(handle-selection-request display event)
	(return))

      (clet ((xw #_<Window>))
	(when (zerop (#_XFindContext (h display)
				     (#_.window (c->-addr event '#_any))
				     (unique-context display)
				     (c-addr-of xw)))
	  (return))
	
	(let ((window (display-for-xdisplay (cval-value xw))))

	  (case event-type

	    (#.#_ReparentNotify (setf (window-parent window) (#_.parent (c->-addr event '#_xreparent)))
	     (return))

	    (#.#_KeyPress
	     (let ((key (translate-key keycode))
		   (mods (translate-state (#_.state (c->-addr event '#_xkey))))
		   (plain (not (logtest mods (logior +ctrl-modifier+ +alt-modifier+)))))

	       (if (window-input-context window)

		   (progn
		     (let ((diff (- (#_time (c->-addr event '#_xkey)) (aref (key-press-times window) keycode))))
		   
		       (when (or (= diff (#_time (c->-addr event '#_xkey))) (and (> diff 0) (< diff #.(ash 1 31))))
			 
			 (unless (zerop keycode)
			   (handle-event window (make-instance 'key-press-event
							       :key-name key 
							       :mods mods)))))

		     (unless filtered
		     
		       (let ((count)
			     (chars))
		     
			 (clet ((status #_<Status>)
				(buffer #_<char[100]>))
		       
			   (setq chars buffer)
		       
			   (setq count (#_Xutf8LookupString (window-input-context window)
							    (#_.key event)
							    buffer 99
							    nil &status))

			   (when (= (cval-value status) #_XBufferOverflow)
			     (setq chars (#_malloc (1+ count)))
			     (setq count  (#_Xutf8LookupString (window-input-context window)
							       (#_.key event)
							       chars count
							       nil &status)))
			   (when (or (= (cval-value status) #_XLookupChars)
				     (= (cval-value status) #_XLookupBoth))

			     (let ((c chars))
			       (setf (c-aref chars count) 0)
			       (loop while (< (- c chars) count)
				     do (handle-event window 'character-event
						      :character (decode-utf-8 c)
						      :mods mods
						      :plain plain))))

			   (unless (eq (cval-value chars) (cval-value buffer))
			     (#_free chars))))))

		   (clet ((keysym #_<KeySym>))
		     (#_XLookupString (#_.xkey event) nil 0 (c-addr-of keysym) nil)
		     
		     (handle-event window (make-instance 'key-press-event
							 :key-name key
							 :mods mods))

		     (let ((codepoint (key-sym-to-unicode keysym)))
		       (unless (eq codepoint :invalid)
			 (handle-event window (make-instance 'character-event
							     :character codepoint
							     :mods mods
							     :plain plain))))))

	       (return)))

	    (#.#_KeyRelease
	     (let ((key (translate-key keycode))
		   (mods (translate-state (#_state (c->-addr event '#_xkey)))))

	       (unless (xkb-detectable? display)

		 (when (#_XEventsQueued xdisplay #_QueuedAfterReading)

		   (clet& ((&next #_<XEvent>))
			  
		     (#_XEventPeek xdisplay &next)

		     (when (and (= (#_.type &next) #_KeyPress)
				(= (#_window (c->-addr &next '#_xkey))
				   (#_window (c->-addr event '#_xkey)))
				(= (#_.keycode (c->-addr &next '#_xkey)) keycode))

		       (when (< (#_time (c->-addr &next '#_xkey)) 20)

			 ;; server generated repeat event
			 (return)))))))

	     (handle-event window (make-instance 'key-release-event
						 :key key
						 :keycode keycode
						 :mods mods))
	     (return))

	    (#.#_ButtonPress

	     (let ((mods (translate-state (#_state (c->-addr event '#_xbutton))))
		   (button (#_button (c->-addr event '#_xbutton))))

	       (cond ((= button #_Button1)
		      (handle-event window (make-instance 'pointer-button-press-event
							  :button +pointer-left-button+
							  :mods mods)))

		     ((= button #_Button2)
		      (handle-event window (make-instance 'pointer-button-press-event
							  :button +pointer-middle-button+
							  :mods mods)))

		     ((= button #_Button3)
		      (handle-event window (make-instance 'pointer-button-press-event
							  :button +pointer-right-button+
							  :mods mods)))

		     ((= button #_Button4)
		      (handle-event window (make-instance 'pointer-wheel-event
							  :delta-y 1.0
							  :delta-x 0.0
							  :mods mods)))

		     ((= button #_Button5)
		      (handle-event window (make-instance 'pointer-wheel-event
							  :delta-y -1.0
							  :delta-x 0.0
							  :mods mods)))

		     ((= button #_Button6)
		      (handle-event window (make-instance 'pointer-wheel-event
							  :delta-x 1.0
							  :delta-y 0.0
							  :mods mods)))

		     ((= button #_Button7)
		      (handle-event window (make-instance 'pointer-wheel-event
							  :delta-x -1.0
							  :delta-y 0.0
							  :mods mods)))

		     (t (handle-event window (make-instance 'pointer-button-press-event
							    :button (- button #_Button1 #_Button4)
							    :mods mods))))

	       (return)))

	    (#.#_ButtonRelease

	     (let ((mods (translate-state (#_state (c->-addr event '#_xbutton))))
		   (button (#_button (c->-addr event '#_xbutton))))

	       (cond ((= button #_Button1)
		      (handle-event window (make-instance 'pointer-button-release-event
							  :button +pointer-left-button+
							  :mods mods)))

		     ((= button #_Button2)
		      (handle-event window (make-instance 'pointer-button-release-event
							  :button +pointer-middle-button+
							  :mods mods)))

		     ((= button #_Button3)
		      (handle-event window (make-instance 'pointer-button-release-event
							  :button +pointer-right-button+
							  :mods mods)))

		     (t (handle-event window (make-instance 'pointer-button-release-event
							    :button (- button #_Button1 #_Button4)
							    :mods mods))))

	       (return)))

	    (#.#_EnterNotify
	     (let ((x (#_.x (c->-addr event '#_xcrossing)))
		   (y (#_.y (c->-addr event '#_xcrossing))))

	       (when (eq (window-cursor-mode window) :hidden)
		 (update-cursor-image window))

	       (handle-event window (make-instance 'pointer-enter-event
						   :x x
						   :y y))
	       (handle-event window (make-instance 'pointer-motion-event
						   :x x
						   :y y))

	       (setf (last-cursor-pos-x window) x
		     (last-cursor-pos-y window) y)
	       
	       (return)))

	    (#.#_LeaveNotify
	     (let ((x (#_.x (c->-addr event '#_xcrossing)))
		   (y (#_.y (c->-addr event '#_xcrossing))))
	       
	       (handle-event window (make-instance 'pointer-exit-event
						   :x x
						   :y y))
	       (return)))


	    (#.#_MotionNotify
	     (let ((x (#_.x (c->-addr event '#_xmotion)))
		   (y (#_.y (c->-addr event '#_xmotion))))

	       (when (or (/= x (cursor-pos-warp-x window))
			 (/= y (cursor-pos-warp-y window)))
		 ;; the cursor was moved by something other than CLUI
		 (if (eq (window-cursor-mode window) :disabled)
		     
		     (let ((dx (- x (last-cursor-pos-x window)))
			   (dy (- y (last-cursor-pos-y window))))
		       
		       (unless (eq (disabled-cursor-window display) window)
			 (return))
		       
		       (when (last-raw-mouse-motion? window)
			 (return))

		       (handle-event window 'pointer-motion-event
				     :x (+ (virtual-cursor-pos-x window) dx)
				     :y (+ (virtual-cursor-pos-y window) dy)))

		     (handle-event window 'pointer-motion-event
				   :x x :y y)))

	       (setf (last-cursor-pos-x window) x)
	       (setf (last-cursor-pos-y window) y)
	       (return)))

	    (#.#_ConfigureNotify

	     (let ((&xconfigure (c->-addr event '#_xconfigure)))
	       
	       (when (or (=/ (#_.width &xconfigure) (last-width window))
			 (=/ (#_.height &xconfigure) (last-height window)))

		 (handle-event window (make-instance 'window-resize-event
						     :width (#_.width &xconfigure)
						     :height (#_.height &xconfigure)))

		 (setf (window-width window) (#_.width &xconfigure)
		       (window-height window) (#_.height &xconfigure)))

	       (clet ((xpos #_<int> (#_.x &xconfigure))
		      (ypos #_<int> (#_.y &xconfigure)))

		 (when (and (zerop (#_.send_event (c->-addr event '#_xany)))
			    (not (= (h (window-parent window)) (default-root-window-handle display))))

		   (%grab-x11-error-handler display)

		   (clet& ((&dummy #_<Window>))

		     (#_XTranslateCoordinates xdisplay (h (window-parent window))
					      xpos ypos
					      (c-addr-of xpos) (c-addr-of ypos)
					      &dummy))

		   (%release-x11-error-handler display)

		   (when (= (display-error-code display) #_BadWindow)
		     (return))

		   (when (or (/= (last-xpos window) (cval-value xpos))
			     (/= (last-ypos window) (cval-value ypos)))
		     
		     (handle-event window (make-instance 'window-position-event
							 :x (cval-value xpos)
							 :y (cval-value ypos)))

		     (setf (window-last-xpos window) (cval-value xpos)
			   (window-last-ypos window) (cval-value ypos)))

		   (return)))))

	    (#.#_ClientMessage

	     (when filtered
	       (return))

	     (when (= (#_.message_type (c->-addr event '#_xclient)) #_None)
	       (return))

	     (with-slots (WM_PROTOCOLS
			  WM_DELETE_WINDOW
			  NET_WM_PING)
		 (display-window-manager display)
	     
	       (cond ((= (#_.message_type (c->-addr event '#_xclient)) WM_PROTOCOLS)
		      (let ((protocol (c-aref (c->-addr (c->-addr (c->-addr event '#_xclient) '#_data) '#_l) 0)))

			(cond ((= protocol #_None) (return))
			      
			      ((= protocol WM_DELETE_WINDOW)
			       (handle-event window (make-instance 'window-close-event)))
			      
			      ((= protocol NET_WM_PING)

			       (let ((reply event))
				 (setf (#_.window (c->-addr reply '#_xclient)) (h (window-root window)))

				 (#_XSendEvent xdisplay (h (window-root window))
					       #_False
					       (logior #_SubstructureNotifyMask #_SubstructureRedirectMask)
					       reply))))))

		     ((= (#_.message_type (c->-addr event '#_xclient)) XdndEnter))

		     ((= (#_.message_type (c->-addr event '#_xclient)) XdndDrop))

		     ((= (#_.message_type (c->-addr event '#_xclient)) XdndPosition)))))

	    (#.#_SelectionNotify

	     (if (= (#_.property (c->-addr event '#_xselection)) XdndSelection)
		 'foo
		 'bar)
	     (return))

	    (#.#_FocusIn
	     (when (or (= (#_.mode (c->-addr event '#_xfocus)) #_NotifyGrab)
		       (= (#_.mode (c->-addr event '#_xfocus)) #_NotifyUngrab))
	       ;; Ignore focus events from popup indicator windows, window menu, key chords and window dragging
	       (return))

	     (cond ((eq (window-cursor-mode window) :disabled) (disable-x11-cursor window))
		   ((eq (window-cursor-mode window) :captured) (capture-x11-cursor window)))

	     (when (window-input-context window)
	       (#_XSetICFocus (window-input-context window)))

	     (handle-event window (make-instance 'window-focus-event))
	     (return))

	    (#.#_FocusOut
	     (when (or (= (#_.mode (c->-addr event '#_xfocus)) #_NotifyGrab)
		       (= (#_.mode (c->-addr event '#_xfocus)) #_NotifyUngrab))
	       ;; Ignore focus events from popup indicator windows, window menu, key chords and window dragging
	       (return))

	     (cond ((eq (window-cursor-mode window) :disabled) (enable-x11-cursor window))
		   ((eq (window-cursor-mode window) :captured) (release-x11-cursor window)))

	     (when (window-input-context window)
	       (#_XUnsetICFocus (window-input-context window)))

	     (when (and (window-monitor window) (window-auto-iconify? window))
	       (iconify-x11-window window))

	     (handle-event window (make-instance 'window-defocus-event))
	     (return))	     

	    (#.#_Expose
	     (handle-event window (make-instance 'window-repaint-event)))

	    (#.#_PropertyNotify
	     
	     (when (= (#_.state (c->-addr event '#_xproperty)) #_PropertyNewValue)
	       (return))

	     (cond ((= (#_.atom (c->-addr event '#_xproperty)) WM_STATE)
		    (let ((state (get-x11-window-state window)))
		      (unless (or (eq state :iconic) (eq state :normal))
			(return)))
		    
		    (let ((iconified? (eq state :iconic)))
		      
		      (if (window-monitor window)
			  
			  (if iconified?
			      (release-x11-monitor window (window-monitor window))
			      (acquire-x11-monitor window)))

		      (setf (last-iconified window) iconified?)

		      (if iconified?
			  (handle-event window (make-instance 'window-iconify-event))
			  (handle-event window (make-instance 'window-restore-event)))))
		   ((= (#_.atom (c->-addr event '#_xproperty)) NET_WM_STATE)

		    (let ((maximized? (get-x11-window-maximized window)))

		      (when (not (eq (last-maximized? window) maximized?))

			(set-x11-window-maximized window maximized?)

			(if maximized?
			    (handle-event window (make-instance 'window-maximize-event))
			    (handle-event window (make-instance 'window-restore-event)))))))
	     (return))	       

	    (#.#_DestroyNotify
	     (return))))))))				   
		       
			 
	     


		     
		     
	     

	    
	      
	  
      

    
