(in-package :clui)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (noffi-syntax t))

(defvar *xim->display* (make-hash-table :test #'equalp))
(defvar *xdisplay->display* (make-hash-table :test #'eq))

(defun display-for-xim (xim)
  (gethash (noffi::ptr-int xim) *xim->display*))

(defun display-for-xdisplay (xdisplay)
  (gethash (noffi::ptr-int xdisplay) *xdisplay->display*))

(defun (setf display-for-xdisplay) (display xdisplay)
  (setf (gethash (noffi::ptr-int xdisplay) *xdisplay->display*) display))

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

(defun wait-for-x11-event (display &optional (timeout nil))
  (unless timeout (setq timeout most-positive-double-float))
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

(defun wait-for-any-event (display &optional (timeout nil))
  (unless timeout (setq timeout most-positive-double-float))
  (setq timeout (coerce timeout 'double-float))
  (let ((count 2))
    (clet ((fds #_<struct pollfd[3]>))
      (let ((xdisplay (h display)))
	(setf (#_.fd (noffi::c+ fds 0)) (#_ConnectionNumber xdisplay)
	      (#_.events (noffi::c+ fds 0)) #_POLLIN)
	(setf (#_.fd (noffi::c+ fds 1)) (c-aref (empty-event-pipes display) 0)
	      (#_.events (noffi::c+ fds 1)) #_POLLIN)

	#+linux
	(when (typep display 'x11:local-server-mixin)
	  #+NIL
	  (when (joysticks-initialized? *linux*)
	    (setf (#_.fd (noffi::c+ &fds count)) (linux-joysticks-inotify *linux*)
		  (#_.event (c-aref &fds count)) #_POLLIN)
	    (incf count)))

	(loop while (zerop (#_XPending xdisplay))
	      unless (poll-posix fds count timeout)
		do (return nil)
	      do (loop for i from 1 below count
		       when (logtest (#_.revents (noffi::c+ fds i)) #_POLLIN)
			 do (return-from wait-for-any-event t))
	      finally (return t))))))

(defun write-empty-event (display)
  (loop
    do (clet ((byte #_<char> 0))
	 (let* ((&byte (c-addr-of byte))
		(result (#_write (c-aref (empty-event-pipes display) 1) &byte 1)))
	   (when (or (noffi::c== result 1) (and (noffi::c== result -1) (noffi::c!= #_errno #_EINTR)))
	     (return (values)))))))

(defun drain-empty-events (display)
  (loop
    do (clet ((dummy #_<char[64]>))
	 (let ((result (#_read (c-aref (empty-event-pipes display) 0) dummy 64)))
	   (when (and (noffi::c== result -1) (noffi::c!= #_errno #_EINTR))
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

		(when (and type (zerop (#_strcmp type "String")))
		  (setq dpi (#+ccl ccl::%ptr-to-int #+sbcl sb-sys::sap-int (#_.addr &value)))))))

	  (#_XrmDestroyDatabase db))))

    (/ dpi 96.0f0)))

(defcfun (input-method-destroy-callback #_<void>)
 	 ((im #_<XIM>) (client-data #_<XPointer>) (call-data #_<XPointer>))
  (declare (ignore client-data call-data))
  (let ((display (display-for-xim im)))
    (break "22")
    (when display
      (break "33")
      (remhash (noffi::ptr-int im) *xim->display*)
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
      (setf (gethash (noffi::ptr-int (display-input-method display)) *xim->display*) display)
      (clet& ((&callback #_<XIMCallback>))
	(setf (#_.callback &callback) input-method-destroy-callback
	      (#_.client_data &callback) nil)
	(noffi::c-funcall #_XSetIMValues (display-input-method display)
			#_XNDestroyCallback &callback 0)

	(do ((window (display-window-list-head display) (window-next window)))
	    ((null window))
	  (create-x11-input-context window))))
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
	 (= (#_.window (c->-addr event '#_xproperty)) (#_.requestor &xselection))
	 (= (#_.atom (c->-addr event '#_xproperty)) (#_.property &xselection)))))

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

	  (when (noffi::c-funcall #_XGetIMValues (display-input-method display)
				#_XNQueryInputStyle &styles 0)
	    (return nil))

	  (loop for i from 0 below (#_.count_styles styles)
		when (= (cval-value (c-aref (#_.supported_styles styles) i))
			(logior #_XIMPreeditNothing
				#_XIMStatusNothing))
		  do (setq found? t)
		     (return))
	  
	  (#_XFree styles)
	  (return found?))))))
	  
(defun release-x11-cursor (display)
  (#_XUngrabPointer (h display) #_CurrentTime))

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

(defun translate-x11-state (state)
  (let ((mods 0)
	(lock-mods 0))

    (when (logtest state #_ShiftMask)
      (setq mods (logior mods +shift-modifier+)))
    (when (logtest state #_ControlMask)
      (setq mods (logior mods +ctrl-modifier+)))
    (when (logtest state #_Mod1Mask)
      (setq mods (logior mods +meta-modifier+)))
    (when (logtest state #_Mod4Mask)
      (setq mods (logior mods +super-modifier+)))
    
    (when (logtest state #_LockMask)
      (setq lock-mods (logior lock-mods +caps-lock-modifier+)))
    (when (logtest state #_Mod2Mask)
      (setq lock-mods (logior lock-mods +num-lock-modifier+)))

    (values mods lock-mods)))

(defun %write-target-to-property (display request)
  (block write-target
    (with-slots (UTF8_STRING
		 NULL)
	display
      (with-slots (PRIMARY
		   TARGETS
		   MULTIPLE
		   ATOM_PAIR
		   SAVE_TARGETS)
	  (display-clipboard-manager display)
	(let ((selection-string)
	      (format-count 2))
	  (clet ((formats #_<Atom[2]>))
	    (setf (c-aref formats 0) UTF8_STRING
		  (c-aref formats 1) #_XA_STRING)

	    (if (= (#_.selection request) PRIMARY)
		(setq selection-string (primary-selection-string display))
		(setq selection-string (clipboard-string display)))

	    (when (= (#_.property request) #_None)
	      (return-from write-target #_None))

	    (when (= (#_.target request) TARGETS)
	      (clet ((targets #_<Atom[4]>))
		(setf (c-aref targets 0) TARGETS
		      (c-aref targets 1) MULTIPLE
		      (c-aref targets 2) UTF8_STRING
		      (c-aref targets 3) #_XA_STRING)

		(#_XChangeProperty (h display)
				   (#_.requestor request)
				   (#_.property request)
				   #_XA_ATOM
				   32
				   #_PropModeReplace
				   targets
				   4)

		(return-from write-target (#_.property request))))

	    (when (= (#_.target request) MULTIPLE)
	      (clet ((targets #_<Atom*>))
		(let ((count (%get-x11-window-property (h display)
						       (#_.requestor request)
						       (#_.property request)
						       ATOM_PAIR
						       (c-addr-of targets))))

		  (loop for i from 0 below count by 2
			with found? = nil
			do (loop for j from 0 below format-count
				 when (= (cval-value (c-aref targets i)) (cval-value (c-aref formats j)))
				   do (setq found? t)
				      (return))
			   
			   (if found?
			       (#_XChangeProperty (h display)
						  (#_.requestor request)
						  (c-aref targets (1+ i))
						  (c-aref targets i)
						  8
						  #_PropModeReplace
						  selection-string
						  (#_strlen selection-string))
			       (setf (c-aref targets (1+ i)) #_None)))

		  (#_XChangeProperty (h display)
				     (#_.requestor request)
				     (#_.property request)
				     ATOM_PAIR
				     32
				     #_PropModeReplace
				     targets
				     count)

		  (return-from write-target (#_.property request)))))

	    (when (= (#_.target request) SAVE_TARGETS)

	      (#_XChangeProperty (h display)
				 (#_.requestor request)
				 (#_.property request)
				 NULL
				 32
				 #_PropModeReplace
				 nil
				 0)

	      (return-from write-target (#_.property request)))

	    ;; conversion to a data target was requested
	    (loop for i from 0 below format-count
		  when (= (cval-value (#_.target request)) (cval-value (c-aref formats i)))
		    do (#_XChangeProperty  (h display)
					   (#_.requestor request)
					   (#_.property request)
					   (#_.target request)
					   8
					   #_PropModeReplace
					   selection-string
					   (#_strlen selection-string))
		       (return-from write-target (#_.property request)))

	    #_None))))))
  
(defun %handle-selection-request (display event)
  (let ((request (#_.xselectionrequest event)))

    (clet& ((&reply #_<XEvent>))
      (setf (#_.type &reply) #_SelectionNotify)

      (let ((&xselection (c->-addr &reply '#_xselection)))

	(setf (#_.property &xselection) (%write-target-to-property display request)
	      (#_.display &xselection) (#_.display request)
	      (#_.requestor &xselection) (#_.requestor request)
	      (#_.selection &xselection) (#_.selection request)
	      (#_.target &xselection) (#_.target request)
	      (#_.time &xselection) (#_.time request))

	(#_XSendEvent (h display) (#_.requestor request) #_False 0 &reply)))))

(defparameter *utf8-offsets*
  (make-array 6 :initial-contents (list #x00000000 #x00003080 #x000e2080
					#x03c82080 #xfa082080 #x82082080)))

(defun decode-utf8-char (s)
  (setq s (ptr-effective-sap s))
  (let ((codepoint 0)
	(count 0))

    (loop do (setq codepoint (+ (ash codepoint 6) (noffi::peek-u8 s 0)))
	  #+CCL(ccl::%incf-ptr s 1)
	  #+SBCL(setf s (sb-alien::sap+ s 1))
	     (incf count)
	  while (= (logand (noffi::peek-u8 s 0) #xc0) #x80))

    (assert (<= count 6))

    (code-char (- codepoint (aref *utf8-offsets* (1- count))))))
	 
(defun process-event (display event)
  (block nil

    (let ((x11-state (display-x11-state display))
	  (timestamp (get-internal-real-time))
	  (x11-keycode)
	  (filtered nil)
	  (event-type (#_.type event)))
      
      (when (or (= event-type #_KeyPress) (= event-type #_KeyRelease))
	(setq x11-keycode (#_.keycode (c->-addr event '#_xkey))))

      (setq filtered (#_XFilterEvent event #_None))
      
      (when (randr-available? x11-state)
	(when (= event-type (+ (cval-value (randr-event-base x11-state)) #_RRNotify))
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

	    (when (and window (raw-mouse-motion? window)
		       (= (#_.extension (c->-addr event '#_xcookie)) (xi-major-opcode x11-state))
		       (#_XGetEventData (h display) (#_.xcookie event))
		       (= (#_.evtype (c->-addr event '#_xcookie)) #_XI_RawMotion)) ;; whew.

	      (let ((re (#_.data (c->-addr event '#_xcookie))))

		(unless (zerop (#_.mask_len (c->-addr re '#_valuators)))

		  (let ((values (#_.raw_values re))
			(xpos (virtual-cursor-pos-x window))
			(ypos (virtual-cursor-pos-y window)))

		    (unless (zerop (#_XIMaskIsSet (#_.mask (c->-addr re '#_valuators)) 0))
		      (setq xpos (+ xpos (c-aref values 0)))
		      (incf values))

		    (unless (zerop (#_XIMaskIsSet (#_.mask (c->-addr re '#_valuators)) 1))
		      (setq ypos (+ ypos (c-aref values 0)))

		      (handle-event window (make-instance 'pointer-motion-event
							  :window window
							  :x xpos
							  :y ypos)))))))))
	(return))

      (when (= event-type #_SelectionRequest)

	(%handle-selection-request display event)
	(return))

      (clet ((xw #_<Window>))
	(unless (zerop (#_XFindContext (h display)
				       (#_.window (c->-addr event '#_xany))
				       (unique-context display)
				       (c-addr-of xw)))
	  (return))

	(let ((window (window-from-window-handle (cval-value xw))))

	  (case event-type
	    
	    (#.#_ReparentNotify
	     (let ((new-parent (window-from-window-handle (#_.parent (c->-addr event '#_xreparent)))))
	       (when new-parent
		 (setf (window-parent window) new-parent)))
	     (return))

	    (#.#_KeyPress
	     (let* ((key (translate-key display x11-keycode))
		    (&xkey (c->-addr event '#_xkey)))
	       (multiple-value-bind (mods lock-mods) (translate-x11-state (#_.state &xkey))
		 (let ((plain? (not (logtest mods (logior +ctrl-modifier+ +meta-modifier+)))))
		   (multiple-value-bind (x y) (window-cursor-position window)

		     (if (window-input-context window)

			 (progn
			   (let ((diff (- (#_time &xkey) (aref (key-press-times window) x11-keycode))))
		   
			     (when (or (= diff (#_time &xkey)) (and (> diff 0) (< diff #.(ash 1 31))))
			 
			       (unless (zerop x11-keycode)
				 (input-key window key :press x y mods lock-mods timestamp))
			 
			       (setf (aref (key-press-times window) x11-keycode) (#_.time &xkey))))

			   (when (zerop filtered)
		     
			     (let ((count)
				   (chars))
		     
			       (clet ((status #_<Status>)
				      (buffer #_<char[100]>))
				 (#_memset buffer 0 100)
		       
				 (setq chars buffer)
			   
				 (setq count (#_Xutf8LookupString (window-input-context window)
								  &xkey
								  buffer 99
								  nil (c-addr-of status)))

				 (when (= (cval-value status) #_XBufferOverflow)
				   (setq chars (#_malloc (1+ count)))
				   (setq count  (#_Xutf8LookupString (window-input-context window)
								     &xkey
								     chars count
								     nil (c-addr-of status))))
			   
				 (when (or (= (cval-value status) #_XLookupChars)
					   (= (cval-value status) #_XLookupBoth))

				   (loop for char across (get-c-string chars)
					 do (input-char window (char-code char) mods lock-mods plain?)))

				 (unless (equalp chars buffer)
				   (#_free chars))))))
		   
			 (clet ((keysym #_<KeySym>))
			   (#_XLookupString (#_.xkey event) nil 0 (c-addr-of keysym) nil)

			   (input-key window key :press x y mods lock-mods timestamp)

			   (let ((char (xkb-keysym-to-unicode keysym)))
			     (when char
			       (input-char window char mods lock-mods plain?)))))

		     (return))))))

	    (#.#_KeyRelease
	     (let ((key (translate-key display x11-keycode)))
	       (multiple-value-bind (mods lock-mods) (translate-x11-state (#_.state (c->-addr event '#_xkey)))
		 (multiple-value-bind (x y) (window-cursor-position window)

		   (unless (xkb-detectable? x11-state)

		     (when (#_XEventsQueued (h display) #_QueuedAfterReading)

		       (clet& ((&next #_<XEvent>))
			 (let ((&xkey (c->-addr &next '#_xkey)))
			  
			   (#_XPeekEvent (h display) &next)

			   (when (and (= (#_.type &next) #_KeyPress)
				      (= (#_.window &xkey)
					 (#_.window (c->-addr event '#_xkey)))
				      (= (#_.keycode &xkey) x11-keycode))

			     (when (< (#_time &xkey) 20)

			       ;; server generated repeat event
			       (return)))))))

		   (input-key window key :release x y mods lock-mods timestamp)
	       
		   (return)))))

	    (#.#_ButtonPress

	     (let ((button (#_.button (c->-addr event '#_xbutton))))
	       (multiple-value-bind (mods lock-mods) (translate-x11-state (#_.state (c->-addr event '#_xbutton)))
		 (multiple-value-bind (x y) (window-cursor-position window)
		   
		   (cond ((= button #_Button1)
			  (input-mouse-click window +pointer-left-button+ :press x y mods lock-mods timestamp))

			 ((= button #_Button2)
			  (input-mouse-click window +pointer-middle-button+ :press x y mods lock-mods timestamp))

			 ((= button #_Button3)
			
			  (input-mouse-click window +pointer-right-button+ :press x y mods lock-mods timestamp))

			 ((= button #_Button4)
			  (handle-event window (make-instance 'pointer-wheel-event
							      :window window
							      :input-code +pointer-wheel+
							      :yoffset 1.0
							      :x x
							      :y y
							      :modifier-state mods
							      :lock-modifier-state lock-mods
							      :timestamp timestamp)))

			 ((= button #_Button5)
			  (handle-event window (make-instance 'pointer-wheel-event
							      :window window
							      :input-code +pointer-wheel+
							      :yoffset -1.0
							      :x x
							      :y y
							      :modifier-state mods
							      :lock-modifier-state lock-mods
							      :timestamp timestamp)))

			 #+NOTYET
			 ((= button #_Button6)
			  (handle-event window (make-instance 'pointer-wheel-event
							      :window window
							      :x x
							      :y y
							      :delta-x 1.0
							      :delta-y 0.0
							      :modifier-state mods)))
			 #+NOTYET
			 ((= button #_Button7)
			  (handle-event window (make-instance 'pointer-wheel-event
							      :window window
							      :x x
							      :y y
							      :delta-x -1.0
							      :delta-y 0.0
							      :modifier-state mods)))

			 (t (input-mouse-click window (- button #_Button1 #_Button4) :press x y mods lock-mods timestamp)))))

	       (return)))

	    (#.#_ButtonRelease

	     (let ((button (#_.button (c->-addr event '#_xbutton))))
	       (multiple-value-bind (mods lock-mods) (translate-x11-state (#_.state (c->-addr event '#_xbutton)))
		 (multiple-value-bind (x y) (window-cursor-position window)

		   (cond ((= button #_Button1)
			  (input-mouse-click window +pointer-left-button+ :release x y mods lock-mods timestamp))

			 ((= button #_Button2)
			  (input-mouse-click window +pointer-middle-button+ :release x y mods lock-mods timestamp))

			 ((= button #_Button3)
			  (input-mouse-click window +pointer-right-button+ :release x y mods lock-mods timestamp))

			 (t (handle-event window (make-instance 'pointer-button-release-event
								:window window
								:button (- button #_Button1 #_Button4)
								:modifier-state mods))))))

	       (return)))

	    (#.#_EnterNotify
	     (let ((x (#_.x (c->-addr event '#_xcrossing)))
		   (y (#_.y (c->-addr event '#_xcrossing))))

	       (when (eq (window-cursor-mode window) :hidden)
		 (update-cursor-image window))

	       (handle-event window (make-instance 'pointer-enter-event
						   :window window
						   :x x
						   :y y))
	       (handle-event window (make-instance 'pointer-motion-event
						   :window window
						   :x x
						   :y y))

	       (setf (last-cursor-pos-x window) x
		     (last-cursor-pos-y window) y)
	       
	       (return)))

	    (#.#_LeaveNotify
	     (let ((x (#_.x (c->-addr event '#_xcrossing)))
		   (y (#_.y (c->-addr event '#_xcrossing))))

	       (handle-event window (make-instance 'pointer-exit-event
						   :window window
						   :x x
						   :y y))
	       (return)))


	    (#.#_MotionNotify
	     (let ((x (#_.x (c->-addr event '#_xmotion)))
		   (y (#_.y (c->-addr event '#_xmotion))))

	       (when (or (/= x (cursor-warp-pos-x window))
			 (/= y (cursor-warp-pos-y window)))
		 ;; the cursor was moved by something other than CLUI
		 (if (eq (window-cursor-mode window) :disabled)
		     
		     (let ((dx (- x (last-cursor-pos-x window)))
			   (dy (- y (last-cursor-pos-y window))))
		       
		       (unless (eq (disabled-cursor-window display) window)
			 (return))
		       
		       (when (raw-mouse-motion? window)
			 (return))

		       (handle-event window (make-instance 'pointer-motion-event
							   :window window
							   :x (+ (virtual-cursor-pos-x window) dx)
							   :y (+ (virtual-cursor-pos-y window) dy))))

		     (handle-event window (make-instance 'pointer-motion-event
							 :window window
							 :x x :y y))))

	       (setf (last-cursor-pos-x window) x)
	       (setf (last-cursor-pos-y window) y)
	       (return)))

	    (#.#_ConfigureNotify

	     (let ((&xconfigure (c->-addr event '#_xconfigure)))
	       
	       (when (or (/= (#_.width &xconfigure) (last-width window))
			 (/= (#_.height &xconfigure) (last-height window)))

		 (handle-event window (make-instance 'window-resize-event
						     :window window
						     :new-width (#_.width &xconfigure)
						     :new-height (#_.height &xconfigure)))

		 (setf (last-width window) (#_.width &xconfigure)
		       (last-height window) (#_.height &xconfigure)))

	       (clet ((xpos #_<int> (#_.x &xconfigure))
		      (ypos #_<int> (#_.y &xconfigure)))

		 (when (and (zerop (#_.send_event (c->-addr event '#_xany)))
			    (not (eq (window-parent window) (window-root window))))

		   (%grab-x11-error-handler display)

		   (clet& ((&dummy #_<Window>))

		     (noffi::c-funcall #_XTranslateCoordinates (h display) (h (window-parent window))
				       xpos ypos
				       (c-addr-of xpos) (c-addr-of ypos)
				       &dummy))

		   (%release-x11-error-handler display)

		   (when (= (display-error-code display) #_BadWindow)
		     (return))

		   (when (or (/= (last-pos-x window) (cval-value xpos))
			     (/= (last-pos-y window) (cval-value ypos)))
		     
		     (handle-event window (make-instance 'window-move-event
							 :window window
							 :new-x (cval-value xpos)
							 :new-y (cval-value ypos)))

		     (setf (last-pos-x window) (cval-value xpos)
			   (last-pos-y window) (cval-value ypos)))

		   (return)))))

	    (#.#_ClientMessage

	     (unless (zerop filtered)
	       (return))

	     (let ((&xclient (c->-addr event '#_xclient)))
	       
	       (when (= (#_.message_type &xclient) #_None)
		 (return))

	       (with-slots (WM_PROTOCOLS
			    WM_DELETE_WINDOW
			    NET_WM_PING)
		 
		   (display-window-manager display)

		 (with-slots (XdndEnter
			      XdndDrop
			      XdndPosition)

		     (display-drag-and-drop display)
	     
		   (cond ((= (#_.message_type &xclient) WM_PROTOCOLS)
			  (let ((protocol (cval-value (c-aref (#_.l (c->-addr &xclient '#_data)) 0))))

			    (cond ((= protocol #_None) (return))
			      
				  ((= protocol WM_DELETE_WINDOW)
				   (handle-event window (make-instance 'window-close-event
								       :window window)))
			      
				  ((= protocol NET_WM_PING)

				   (let ((reply event))
				     (setf (#_.window (c->-addr reply '#_xclient)) (h (window-parent window)))

				     (#_XSendEvent (h display) (h (window-parent window))
						   #_False
						   (logior #_SubstructureNotifyMask #_SubstructureRedirectMask)
						   reply))))))

			 ((= (#_.message_type &xclient) XdndEnter))

			 ((= (#_.message_type &xclient) XdndDrop))

			 ((= (#_.message_type &xclient) XdndPosition)))))))

	    (#.#_SelectionNotify

	     (with-slots (XdndSelection)
		 (display-drag-and-drop display)
	       (if (= (#_.property (c->-addr event '#_xselection)) XdndSelection)
		   'foo
		   'bar)
	       (return)))

	    (#.#_FocusIn
	     (when (or (= (#_.mode (c->-addr event '#_xfocus)) #_NotifyGrab)
		       (= (#_.mode (c->-addr event '#_xfocus)) #_NotifyUngrab))
	       ;; Ignore focus events from popup indicator windows, window menu, key chords and window dragging
	       (return))

	     (cond ((eq (window-cursor-mode window) :disabled) (disable-cursor window))
		   ((eq (window-cursor-mode window) :captured) (capture-cursor window)))

	     (when (window-input-context window)
	       (#_XSetICFocus (window-input-context window)))

	     (handle-event window (make-instance 'window-focus-event
						 :window window))
	     (return))

	    (#.#_FocusOut
	     (when (or (= (#_.mode (c->-addr event '#_xfocus)) #_NotifyGrab)
		       (= (#_.mode (c->-addr event '#_xfocus)) #_NotifyUngrab))
	       ;; Ignore focus events from popup indicator windows, window menu, key chords and window dragging
	       (return))

	     (cond ((eq (window-cursor-mode window) :disabled) (enable-cursor window))
		   ((eq (window-cursor-mode window) :captured) (release-cursor window)))

	     (when (window-input-context window)
	       (#_XUnsetICFocus (window-input-context window)))

	     (when (and (window-monitor window) (auto-iconify? window))
	       (setf (window-iconified? window) t))

	     (handle-event window (make-instance 'window-defocus-event
						 :window window))
	     (return))	     

	    (#.#_Expose
	     (handle-event window (make-instance 'window-repaint-event
						 :window window)))

	    (#.#_PropertyNotify
	     
	     (when (= (#_.state (c->-addr event '#_xproperty)) #_PropertyNewValue)
	       (return))

	     (with-slots (WM_STATE NET_WM_STATE)
		 (display-window-manager display)

	       (cond ((= (#_.atom (c->-addr event '#_xproperty)) WM_STATE)
		      (let ((state (get-x11-window-iconified window)))
			
			(unless (or (eq state :iconic) (eq state :normal))
			  (return))
		    
			(let ((iconified? state))
		      
			  (if (window-monitor window)
			  
			      (if iconified?
				  (release-monitor window (window-monitor window))
				  (acquire-monitor window (window-monitor window))))

			  (setf (last-iconified? window) iconified?)

			  (if iconified?
			      (handle-event window (make-instance 'window-iconify-event
								  :window window
								  :new-width 0
								  :new-height 0))
			      (handle-event window (make-instance 'window-restore-event))))))
		   
		     ((= (#_.atom (c->-addr event '#_xproperty)) NET_WM_STATE)

		      (let ((maximized? (window-maximized? window)))

			(when (not (eq (last-maximized? window) maximized?))

			  (setf (window-maximized? window) maximized?)

			  (if maximized?
			      (handle-event window (make-instance 'window-maximize-event
								  :window window))
			      (handle-event window (make-instance 'window-restore-event
								  :window window))))))))
	     (return))

	    (#.#_DestroyNotify
	     (return))))))))

(defun poll-x11-events (display)

  (let ((xdisplay (h display)))

    (drain-empty-events display)

    (#_XPending xdisplay)

    (loop until (zerop (#_QLength xdisplay))
	  do (clet ((event #_<XEvent>))

	       (#_XNextEvent xdisplay (c-addr-of event))
	       (process-event display (c-addr-of event))))

    (let ((window (disabled-cursor-window display)))

      (when window

	(multiple-value-bind (width height)
	    (get-x11-window-size window)

	  (when (or (/= (last-cursor-pos-x window) (/ width 2))
		    (/= (last-cursor-pos-y window) (/ height 2)))

	    (set-x11-cursor-pos window (/ width 2) (/ height 2))))))

    (#_XFlush xdisplay)
    (values)))



(defun wait-x11-events (display &optional (timeout nil))
  (wait-for-any-event display timeout)
  (poll-x11-events display))

(defun run (&optional (display (default-display)))
  (loop 
	(wait-x11-events display)))
