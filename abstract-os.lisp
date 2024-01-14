(in-package :clui)

(defun translate-key (display scancode)
  (when (or (< scancode 0) (> scancode 255))
    (return-from translate-key nil))
  (aref (display-keycodes display) scancode))



(defun initialize-os-window (window &rest args
			     &key (width 640) (height 480) (title "clui")
			     (display (default-display))
			       (share nil)
			       (maximized? nil)
			       (resizable? t)
			       (decorated? t)
			       (auto-iconify? t)
			       (floating? nil)
			       (focus-on-show? t)
			       (mouse-passthrough? nil)
			       &allow-other-keys)
  (declare (ignore args))
  (declare (ignorable share))
  (assert title)

  (when (or (<= width 0) (<= height 0))
    (error "Invalid window size: ~AX~A" width height))

  (setf (window-next window) (display-window-list-head display))
  (setf (display-window-list-head display) window)
  (let (#+NIL(video-mode (application-default-video-mode window)))
    #+NIL(setf (video-mode-width video-mode) width
	  (video-mode-height video-mode) height
	  (video-mode-refresh-rate video-mode) 0.0f0)



    (setf (last-maximized? window) maximized?
	  (last-resizable? window) resizable?
	  (last-decorated? window) decorated?
	  (last-floating? window) floating?
	  
	  (auto-iconify? window) auto-iconify?
	  
	  (focus-on-show? window) focus-on-show?
	  (mouse-passthrough? window) mouse-passthrough?)
    
    (setf (window-cursor-mode window) :normal

	  (window-min-width window) :dont-care
	  (window-min-height window) :dont-care
	  (window-max-width window) :dont-care
	  (window-aspect-numer window) :dont-care
	  (window-aspect-denom window) :dont-care))
  t)

#+notyet
(defun input-window-focus (window focused?)
  (if (null window)
      (warn "window was nil in input-window-iconify")
      (locally
	  (declare (type os-window-mixin window))
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


#+win32
(defmethod poll-monitors ((display win32:display-mixin))
  (poll-win32-monitors display))

#+cocoa
(defmethod poll-monitors ((display cocoa:display-mixin))
  (poll-cocoa-monitors display))

#+x11
(defmethod poll-monitors ((display x11:server-mixin))
  (poll-x11-monitors display))

#+wayland
(defmethod poll-monitors ((display wayland:display-mixin))
  (poll-wayland-monitors display))  

#+win32
(defmethod acquire-monitor ((window win32:window-mixin) (monitor win32:monitor-mixin))
  (acquire-win32-monitor window monitor))

#+cocoa
(defmethod acquire-monitor ((window cocoa:window-mixin) (monitor cocoa:monitor-mixin))
  (acquire-cocoa-monitor window monitor))

#+x11
(defmethod acquire-monitor ((window x11:window-mixin) (monitor x11:monitor-mixin))
  (acquire-x11-monitor window monitor))

#+wayland
(defmethod acquire-monitor ((window wayland:window-mixin) (monitor wayland:monitor-mixin))
  (acquire-wayland-monitor window monitor))

#+win32
(defmethod release-monitor ((window win32:window-mixin) (monitor win32:monitor-mixin))
  (release-win32-monitor window monitor))

#+cocoa
(defmethod release-monitor ((window cocoa:window-mixin) (monitor cocoa:monitor-mixin))
  (release-cocoa-monitor window monitor))

#+x11
(defmethod release-monitor ((window x11:window-mixin) (monitor x11:monitor-mixin))
  (release-x11-monitor window monitor))

#+wayland
(defmethod release-monitor ((window wayland:window-mixin) (monitor wayland:monitor-mixin))
  (release-wayland-monitor window monitor))

(defun capture-cursor (window)
  #+windows(capture-win32-cursor window))

(defun update-cursor-image (window)
  #+windows(update-win32-cursor-image window))

#+win32
(defmethod release-cursor ((display win32:display-mixin))
  (release-win32-cursor display))

#+cocoa
(defmethod release-cursor ((display cocoa:display-mixin))
  (release-cocoa-cursor display))

#+x11
(defmethod release-cursor ((display x11:server-mixin))
  (release-x11-cursor display))

#+x11
(defmethod enable-raw-mouse-motion ((window x11:window-mixin))
  (enable-x11-raw-mouse-motion window))

(defun get-window-cursor-pos (window)
  (if (eq (window-cursor-mode window) :disabled)
      (values (virtual-cursor-pos-x window)
	      (virtual-cursor-pos-y window))
      (%get-window-cursor-pos window)))

#+cocoa
(defmethod %get-window-cursor-pos ((window cocoa:window-mixin))
  (get-cocoa-window-cursor-pos window))

#+win32
(defmethod %get-window-cursor-pos ((window win32:window-mixin))
  (get-win32-window-cursor-pos window))

#+x11
(defmethod %get-window-cursor-pos ((window x11:window-mixin))
  (get-x11-window-cursor-pos window))


(defun enable-cursor (window)
  (when (raw-mouse-motion? window)
    (setf (raw-mouse-motion? window) t))

  (let ((display (window-display window)))
    
    (setf (disabled-cursor-window display) nil)
    
    (release-cursor display)
    
    (set-window-cursor-position window
				(restore-cursor-pos-x display)
				(restore-cursor-pos-y display))
    (update-cursor-image window)
    (values)))

(defun disable-cursor (window)
  (when (last-raw-mouse-motion? window)
    (enable-raw-mouse-motion window))

  (let ((display (window-display window)))
  
    (setf (disabled-cursor-window display) window)
  
    (multiple-value-bind (x y) (window-cursor-position window)
    
      (setf (restore-cursor-pos-x display) x
	    (restore-cursor-pos-y display) y)
    
      (update-cursor-image window)
      (center-cursor-in-content-area window)
      (capture-cursor window)
    
      (values))))

(defun maybe-acquire-monitor (window)
  (let ((monitor (window-fullscreen-monitor window)))
    (when monitor
      (acquire-monitor window monitor))))

(defun maybe-release-monitor (window)
  (let ((monitor (window-fullscreen-monitor window)))
    (when monitor
      (release-monitor window monitor))))


 
