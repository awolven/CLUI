(in-package :clui)

(defun x11-copy-string-to-clipboard (display string)
  (let ((clipboard-manager (display-clipboard-manager display)))
    (setf (display-clipboard-string display) (copy-seq string))
    (with-slots (CLIPBOARD) clipboard-manager
      (#_XSetSelectionOwner (h display)
			    CLIPBOARD
			    (h (helper-window display))
			    #_CurrentTime)

      (unless (= (#_XGetSelectionOwner (h display) CLIPBOARD) (h (helper-window display)))
	(error "x11: failed to become owner of clipboard selection")))
    (values)))	

(defun x11-copy-string-from-clipboard (display)
  (let ((clipboard-manager (display-clipboard-manager display)))
    (with-slots (CLIPBOARD) clipboard-manager
      (%get-selection-string display CLIPBOARD))))

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

(defun %write-target-to-property (display request)
  ;; set the specified property to the selection converted to the requested target
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
		(setq selection-string (copy-seq (primary-selection-string display)))
		(setq selection-string (copy-seq (display-clipboard-string display))))

	    (when (= (#_.property request) #_None)
	      ;; the requestor is a legacy client
	      (return-from write-target #_None))

	    (when (= (#_.target request) TARGETS)
	      ;; a list pf supported targets was requested
	      (clet ((targetz #_<Atom[4]>))
		(setf (c-aref targetz 2) UTF8_STRING)
		(setf (c-aref targetz 3) #_XA_STRING)
		(setf (c-aref targetz 0) (cons-cval TARGETS '#_<Atom>))
		(setf (c-aref targetz 1) (cons-cval MULTIPLE '#_<Atom>))

		(#_XChangeProperty (h display)
				   (#_.requestor request)
				   (#_.property request)
				   #_XA_ATOM
				   32
				   #_PropModeReplace
				   (c-cast '#_<unsigned char*> targetz)
				   4)
		(return-from write-target (#_.property request))))

	    (when (= (#_.target request) MULTIPLE)
	      ;; multiple conversions were requested
	      (clet ((targetz #_<Atom*>))
		(let ((count (%get-x11-window-property (h display)
						       (#_.requestor request)
						       (#_.property request)
						       ATOM_PAIR
						       (c-cast '#_<unsigned char**> (c-addr-of targetz)))))

		  (loop for i from 0 below count by 2
			with found? = nil
			do (loop for j from 0 below format-count
				 when (= (cval-value (c-aref targetz i)) (cval-value (c-aref formats j)))
				   do (setq found? t)
				      (return))
			   
			   (if found?
			       (#_XChangeProperty (h display)
						  (#_.requestor request)
						  (c-aref targetz (1+ i))
						  (c-aref targetz i)
						  8
						  #_PropModeReplace
						  selection-string
						  (#_strlen selection-string))
			       (setf (c-aref targetz (1+ i)) #_None)))

		  (#_XChangeProperty (h display)
				     (#_.requestor request)
				     (#_.property request)
				     ATOM_PAIR
				     32
				     #_PropModeReplace
				     (c-cast '#_<unsigned char*> targetz)
				     count)

		  (return-from write-target (#_.property request)))))

	    (when (= (#_.target request) SAVE_TARGETS)
	      ;; the request is a check whether we support SAVE_TARGETS
	      ;; it should be handled as a no-op side effect target

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
		  when (= (#_.target request) (cval-value (c-aref formats i)))
		    ;; the requested target is one we support
		    do (#_XChangeProperty  (h display)
					   (#_.requestor request)
					   (#_.property request)
					   (#_.target request)
					   8
					   #_PropModeReplace
					   selection-string
					   (#_strlen selection-string))
		       (return-from write-target (#_.property request)))

	    ;; the requested target is not supported
	    #_None))))))

(defun convert-latin1-to-utf8 (string)
  string)

(defcfun (isSelPropNewValueNotify #_<char>) ((xdisplay #_<Display*>) (event #_<XEvent*>) (pointer #_<XPointer>))
  ;; returns whether it is a property event for the specified selection transfer
  (declare (ignore xdisplay))
  (let* ((notification (c-cast '#_<XEvent*> pointer))
	 (&xselection (c->-addr notification '#_xselection))
	 (&xproperty (c->-addr event '#_xproperty)))
    (if (and (= (#_.type event) #_PropertyNotify)
	     (= (#_.state &xproperty) #_PropertyNewValue)
	     (= (#_.window &xproperty) (#_.requestor &xselection))
	     (= (#_.atom &xproperty) (#_.property &xselection)))
	1 0)))

(defun x11-get-clipboard-owner (display)
  (let ((clipboard-manager (display-clipboard-manager display)))
    (with-slots (CLIPBOARD) clipboard-manager
      (#_XGetSelectionOwner (h display) CLIPBOARD))))

(defun %get-selection-string (display selection)
  (with-slots (PRIMARY INCR) (display-clipboard-manager display)

    (when (= (#_XGetSelectionOwner (h display) selection) (h (helper-window display)))
      (return-from %get-selection-string
	(copy-seq
	 (if (= selection PRIMARY)
	     (primary-selection-string display)
	     (display-clipboard-string display)))))

    (with-slots (UTF8_STRING CLUI_SELECTION) display
      
      (let ((target-count 2)
	    (selection-string ""))
	
	(clet ((targets #_<Atom[2]>))
	  
	  (setf (c-aref targets 0) UTF8_STRING
		(c-aref targets 1) #_XA_STRING)

	  (loop for i from 0 below target-count
		do (clet& ((&notification #_<XEvent>)
			   (&dummy #_<XEvent>))

		     (clet ((actual-type #_<Atom>)
			    (actual-format #_<int>)
			    (item-count #_<unsigned long>)
			    (bytes-after #_<unsigned long>)
			    (data #_<char*>))

		       (#_XConvertSelection (h display)
					    selection
					    (c-aref targets i)
					    CLUI_SELECTION
					    (h (helper-window display))
					    #_CurrentTime)
		   
		       (loop while (not (#_XCheckTypedWindowEvent (h display)
								  (h (helper-window display))
								  #_SelectionNotify
								  &notification))
			     do (wait-for-x11-event display))

		       (let ((&xselection (c->-addr &notification '#_xselection)))

			 (unless (= (#_.property &xselection) #_None)
			   (unless (= (#_.requestor &xselection) 0)
			     
			     (#_XCheckIfEvent (h display)
					      &dummy
					      isSelPropNewValueNotify
					      &notification)

			     (#_XGetWindowProperty (h display)
						   (#_.requestor &xselection)
						   (#_.property &xselection)
						   0
						   #_LONG_MAX
						   #_True
						   #_AnyPropertyType
						   (c-addr-of actual-type)
						   (c-addr-of actual-format)
						   (c-addr-of item-count)
						   (c-addr-of bytes-after)
						   (c-addr-of data))

			     (when (= (cval-value actual-type) INCR)
			       (let ((string ""))

				 (loop
				   do
				      (loop while (not (#_XCheckIfEvent (h display)
									&dummy
									isSelPropNewValueNotify
									&notification))
					    do (wait-for-x11-event display))

				      (when data
					(let ((data-ptr (cons-ptr (cval-value data) 0 '#_<char*>)))
					  (#_XFree data-ptr)))

				      (unless (= (#_.requestor &xselection) 0)
					(#_XGetWindowProperty (h display)
							      (#_.requestor &xselection)
							      (#_.property &xselection)
							      0
							      #_LONG_MAX
							      #_True
							      #_AnyPropertyType
							      (c-addr-of actual-type)
							      (c-addr-of actual-format)
							      (c-addr-of item-count)
							      (c-addr-of bytes-after)
							      (c-addr-of data))

					(unless (zerop (cval-value item-count))
					  (when data
					    (let ((data-ptr (cons-ptr (cval-value data) 0 '#_<char*>)))
					      (setq string (concatenate 'string string (get-c-string data-ptr))))))

					(when (zerop (cval-value item-count))
					  (unless (string= "" string)
					    (if (= (c-aref targets i) #_XA_STRING)
						(setq selection-string (convert-latin1-to-utf8 string))
						(setq selection-string string)))
					  (return))))))

			     (when data
			       (let ((data-ptr (cons-ptr (cval-value data) 0 '#_<char*>)))

				 (when (= (cval-value actual-type) (cval-value (c-aref targets i)))

				   (if (= (cval-value (c-aref targets i)) #_XA_STRING)
				       (setq selection-string (convert-latin1-to-utf8 (get-c-string data-ptr)))
				       (setq selection-string (get-c-string data-ptr))))
			     
				 (#_XFree data-ptr)))

			     (unless (string= selection-string "")
			       (return))))))))
	  
	  selection-string)))))
