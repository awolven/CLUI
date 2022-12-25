(in-package :abstract-os)

(defvar *app*)

(defmethod initialize-instance :after ((window essential-os-window-mixin)
				       &rest initargs)
  (apply #'initialize-os-window window initargs)
  (values))

(defun initialize-os-window (window &rest args
			     &key width height (title "silica")
			       (monitor nil)
			       resizable? decorated? auto-iconify? floating?
			       focus-on-show? mouse-passthrough?
			     &allow-other-keys)
  (assert title)

  ;;require-init-or-return

  (when (or (<= width 0) (<= height 0))
    (error "Invalid window size: ~AX~A" width height))

  (setf (window-next window) (application-window-list-head *app*))
  (setf (application-window-list-head *app*) window)
  (let ((video-mode (application-default-video-mode window)))
    (setf (video-mode-width video-mode) width
	  (video-mode-height video-mode) height
	  (video-mode-refresh-rate video-mode) (hints-refresh-rate (hints *app*)))

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

  (apply #'create-win32-window *app* window args))

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
       (do ((window (application-window-list-head *app*) (window-next window)))
	   ()
	   (if (eq (window-monitor window) monitor)
	       (multiple-value-bind (width height) (get-window-size window)
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

(defmethod create-window (&rest args &key &allow-other-keys)
  (apply #'create-os-window args))

(defmethod destroy-window ((window essential-os-window-mixin))
  (destroy-os-window window))			   

(defmethod window-title ((window essential-os-window-mixin))
  (os-window-title window))

(defmethod (setf window-title) (string (window essential-os-window-mixin))
  (setf (os-window-title window) string))

(defmethod (setf window-icon) (image (window essential-os-window-mixin))
  (setf (os-window-icon window) image))

(defmethod window-pos ((window essential-os-window-mixin))
  (os-window-pos window))

(defmethod (setf window-pos) (pos (window essential-os-window-mixin))
  (setf (os-window-pos window) pos))

(defmethod window-size ((window essential-os-window-mixin))
  (os-window-size window))

(defmethod (setf window-size) (size (window essential-os-window-mixin))
  (setf (os-window-size window) size))

(defmethod (setf window-size-limits) (rect (window essential-os-window-mixin))
  (setf (os-window-size window) rect))

(defmethod (setf window-aspect-ratio) (ratio (window essential-os-window-mixin))
  (setf (os-window-aspect-ratio window) ratio))

(defmethod window-frame-size ((window essential-os-window-mixin))
  (os-window-frame-size window))

(defmethod window-content-scale ((window essential-os-window-mixin))
  (os-window-content-scale window))

(defmethod iconify-window ((window essential-os-window-mixin))
  (iconify-os-window window))

(defmethod restore-window ((window essential-os-window-mixin))
  (restore-os-window window))

(defmethod maximize-window ((window essential-os-window-mixin))
  (maximize-os-window window))

(defmethod show-window ((window essential-os-window-mixin))
  (show-os-window window))

(defmethod hide-window ((window essential-os-window-mixin))
  (hide-os-window window))

(defmethod request-window-attention ((window essential-os-window-mixin))
  (request-os-window-attention window))

(defmethod focus-window ((window essential-os-window-mixin))
  (focus-os-window window))

(defmethod (setf window-monitor) (monitor (window essential-os-window-mixin))
  (setf (os-window-monitor window) monitor))

(defmethod window-focused? ((window essential-os-window-mixin))
 (os-window-focused? window))

(defmethod window-iconified? ((window essential-os-window-mixin))
  (os-window-iconified? window))

(defmethod window-visible? ((window essential-os-window-mixin))
  (os-window-visible? window))

(defmethod window-maximized? ((window essential-os-window-mixin))
  (os-window-maximized? window))

(defmethod window-hovered? ((window essential-os-window-mixin))
  (os-window-hovered? window))

(defmethod window-opacity ((window essential-os-window-mixin))
  (os-window-opacity window))

(defmethod (setf window-resizable?) (value (window essential-os-window-mixin))
  (setf (os-window-resizable? window) value))

(defmethod (setf window-decorated?) (value (window essential-os-window-mixin))
  (setf (os-window-decorated? window) value))

(defmethod (setf window-floating?) (value (window essential-os-window-mixin))
  (setf (os-window-floating? window) value))

(defmethod (setf window-opacity) (value (window essential-os-window-mixin))
  (setf (os-window-opacity window) value))

(defmethod (setf window-mouse-passthrough) (value (window essential-os-window-mixin))
  (setf (os-window-mouse-passthrough window) value))

(defmethod window-poll-events ((window essential-os-window-mixin))
  (os-window-poll-events window))

(defmethod window-wait-events ((window essential-os-window-mixin))
  (os-window-wait-events window))

(defmethod window-wait-events-timeout ((window essential-os-window-mixin) timeout)
  (os-window-wait-events-timeout window timeout))

(defmethod window-post-empty-event ((window essential-os-window-mixin))
  (os-window-post-empty-event window))

(defun monitor-pos (monitor)
  (declare (ignorable monitor)))

(defun monitor-content-scale (monitor)
  (declare (ignorable monitor)))

(defun monitor-work-area (monitor)
  (declare (ignorable monitor)))

(defun video-modes (monitor)
  (declare (ignorable monitor)))

(defun video-mode (monitor)
  (declare (ignorable monitor)))

(defun gamma-ramp (monitor)
  (declare (ignorable monitor)))

(defun (setf gamma-ramp) (gamma-ramp monitor)
  (declare (ignorable monitor gamma-ramp)))

