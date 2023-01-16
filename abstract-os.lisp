(in-package :abstract-os)

(defvar *app* nil)

(defmethod default-application-class-for-window ((window essential-os-window-mixin))
  'abstract-os-application)

(defmethod default-window-class-for-application ((app application-mixin))
  'os-window)

(defun input-framebuffer-size (window width height)
  (declare (ignorable window width height))
  (values))

(defun input-window-damage (window)
  (declare (ignore window))
  (values))

(defun input-cursor-pos (window pos-x pos-y)
  (declare (ignorable window pos-x pos-y))
  (values))

(defun input-mouse-clicked (window button action mods)
  (declare (ignorable window button action mods))
  (values))

(defun input-cursor-enter (window entered?)
  (declare (ignorable window entered?))
  (values))

(defun show-cursor (window)
  (declare (ignorable window))
  (values))

(defun hide-cursor (window)
  (declare (ignorable window))
  (values))

(defmethod input-window-damaged (window)
  (declare (ignorable window))
  (values))

(defun input-window-close-request (window)
  (declare (ignorable window))
  (values))

(defun initialize-os-window (window &rest args
			     &key (width 640) (height 480) (title "Abstract OS")
			       (monitor nil)
			       (share nil)
			       (resizable? t)
			       (decorated? t)
			       (auto-iconify? t)
			       (floating? t)
			       (focus-on-show? t)
			       (mouse-passthrough? nil)
			       &allow-other-keys)
  (declare (ignore args))
  (declare (ignorable share))
  (assert title)

  (unless *app*
    (make-instance (default-application-class-for-window window)))

  ;;require-init-or-return

  (when (or (<= width 0) (<= height 0))
    (error "Invalid window size: ~AX~A" width height))

  (setf (window-next window) (application-window-list-head *app*))
  (setf (application-window-list-head *app*) window)
  (let (#+NIL(video-mode (application-default-video-mode window)))
    #+NIL(setf (video-mode-width video-mode) width
	  (video-mode-height video-mode) height
	  (video-mode-refresh-rate video-mode) (hints-refresh-rate (hints *app*)))

    (when decorated?
      (print 'decorated))
    
    (setf (window-monitor window) monitor
	  (resizable? window) resizable?
	  (decorated? window) decorated?
	  (auto-iconify? window) auto-iconify?
	  (floating? window) floating?
	  (focus-on-show? window) focus-on-show?
	  (mouse-passthrough? window) mouse-passthrough?
	  (window-cursor-mode window) :normal

	  (window-min-width window) :dont-care
	  (window-min-height window) :dont-care
	  (window-max-width window) :dont-care
	  (window-numer window) :dont-care
	  (window-denom window) :dont-care))
  t)

#+notyet
(defun input-window-focus (window focused?)
  (if (null window)
      (warn "window was nil in input-window-iconify")
      (locally
	  (declare (type essential-os-window-mixin window))
	(unless focused?
	  (let ((keys (window-keys window))
		(mouse-buttons (window-mouse-buttons window)))
	    (loop for key from 0 below +key-last+
		  when (= (aref keys key) +press+)
		    do (let ((scancode (get-key-scancode *app* key)))
			 (input-key window key scancode +release+ 0)))
	    (loop for button from 0 below +button-last+
		  when (= (aref mouse-buttons button) +press+)
		    do (input-mouse-click window button +release+ 0))))))
  (values))
  

(defun input-window-pos (window x y)
  (if (null window)
      (warn "window was nil in input-window-pos")
      (locally
	  (declare (type essential-os-window-mixin window))
	(when (slot-value window 'pos-callback)
	  (funcall (slot-value window 'pos-callback)
		   window x y))))
  (values))

(defun input-window-size (window width height)
  (if (null window)
      (warn "window was nil in input-window-size")
      (locally
	  (declare (type essential-os-window-mixin window))
	(when (slot-value window 'size-callback)
	  (funcall (slot-value window 'size-callback)
		   window width height))))
  (values))

(defun input-window-iconify (window iconified?)
  (if (null window)
      (warn "window was nil in input-window-iconify")
      (locally
	  (declare (type essential-os-window-mixin window))
	(when (slot-value window 'iconify-callback)
	  (funcall (slot-value window 'iconify-callback)
		   window iconified?))))
  (values))

(defun input-window-maximize (window maximized?)
  (if (null window)
      (warn "window was nil in input-window-maximize")
      (locally
	  (declare (type essential-os-window-mixin window))
	(when (slot-value window 'maximize-callback)
	  (funcall (slot-value window 'maximize-callback)
		   window maximized?))))
  (values))

(defun input-window-monitor (window monitor)
  (if (null window)
      (warn "window was nil in input-window-monitor")
      (locally
	  (declare (type essential-os-window-mixin window))
	(setf (window-monitor window) monitor))))

(defun input-monitor-window (monitor window)
  (declare (ignorable monitor window))
  (values))

(defun input-monitor (monitor &key (action nil) (placement nil))
  (declare (type monitor-mixin monitor))
  (declare (type application-mixin *app*))
  (let ((monitors (application-monitors *app*)))

    (ecase action
      (:connected
       (incf (slot-value *app* 'monitor-count))

       (ecase placement
	 (:insert-first (vector-push-extend nil monitors)
	  (when (> (length monitors) 1)
	    (loop for i from (- (length monitors) 2) downto 0
		  do (setf (aref monitors (1+ i)) (aref monitors i))
		  finally (setf (aref (slot-value *app* 'monitors) 0) monitor))))
	 
	 (:insert-last (vector-push-extend monitor monitors))))

      (:disconnected
       (do ((window (application-window-list-head *app*)))
	   ((window-next window))
	 
	 (if (eq (window-monitor window) monitor)
	       (multiple-value-bind (width height) (get-os-window-size window)
		 (set-window-monitor *app* window nil 0 0 width height :blah))))))))

(defmethod set-window-monitor ((app application-mixin) window monitor xpos ypos width height refresh-rate)
  (declare (ignorable monitor xpos ypos))
  (declare (type essential-os-window-mixin window))
  (when (or (minusp width) (minusp height))
    (warn "invalid window size: ~AX~A" width height)
    (return-from set-window-monitor))

  (when (and (not (eq refresh-rate :dont-care))
	     (minusp refresh-rate))
    (warn "invalid refresh rate: ~A" refresh-rate)
    (return-from set-window-monitor))

  (let ((video-mode (window-video-mode window)))
    (setf (video-mode-width video-mode) width)
    (setf (video-mode-height video-mode) height)
    (setf (video-mode-refresh-rate video-mode) refresh-rate))

  (call-next-method))


;;
