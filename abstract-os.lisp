(in-package :clui)

(defun initialize-os-window (window &rest args
			     &key (width 640) (height 480) (title "clui")
			     (display (default-display))
			       (share nil)
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



    (setf ;;(currently-maximized? window) maximized?
     (currently-resizable? window) resizable?
	  (currently-decorated? window) decorated?
	  (auto-iconify? window) auto-iconify?
	  (currently-floating? window) floating?
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
(defmethod poll-monitors ((display win32:desktop-mixin))
  (poll-win32-monitors display))

#+cocoa
(defmethod poll-monitors ((display cocoa:desktop-mixin))
  (poll-cocoa-monitors display))

#+x11
(defmethod poll-monitors ((display x11:server-mixin))
  #+notyet(poll-x11-monitors display))

#+wayland
(defmethod poll-monitors ((display wayland:desktop-mixin))
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

(defun release-cursor (app)
  #+windows(release-win32-cursor app))

(defun enable-cursor (window)
  (when (window-raw-mouse-motion? window)
    (enable-raw-mouse-motion window))
  (setf (disabled-cursor-window (window-display window)) nil)
  (release-cursor (window-display window))
  (set-os-window-cursor-pos window
			    (restore-cursor-pos-x (window-display window))
			    (restore-cursor-pos-y (window-display window)))
  (update-cursor-image window)
  (values))

(defun disable-cursor (window)
  (setf (disabled-cursor-window (window-display window)) window)
  (multiple-value-bind (x y) (get-os-window-cursor-pos window)
    (setf (restore-cursor-pos-x (window-display window)) x
	  (restore-cursor-pos-y (window-display window)) y)
    (update-cursor-image window)
    (center-cursor-in-content-area window)
    (capture-cursor window)
    (when (window-raw-mouse-motion? window)
      (enable-raw-mouse-motion window))
    (values)))

(defun maybe-acquire-monitor (window)
  (let ((monitor (window-monitor window)))
    (when monitor
      (acquire-monitor window monitor))))

(defun maybe-release-monitor (window)
  (let ((monitor (window-monitor window)))
    (when monitor
      (release-monitor window monitor))))


 
