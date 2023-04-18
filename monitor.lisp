(in-package :clui)

(defun %refresh-video-modes (monitor)
  (let ((modes nil))
    
  (when (monitor-modes monitor)
    (return-from %refresh-video-modes t))

  (setq modes (get-monitor-video-modes monitor))

  (unless modes
    (return-from %refresh-video-modes nil))

  (sort modes #'(lambda (a b)
		  (zerop (compare-video-modes a b))))

  (setf (monitor-modes monitor) modes)

  t))

;; Event API:

(defun input-monitor (display monitor &key (action nil) (placement nil))
  (declare (type monitor-mixin monitor))
  (declare (type display-mixin display))
  (ecase action
    (:connected

     (ecase placement
       (:insert-first (push monitor (display-monitors display)))
	 
       (:insert-last (setf (display-monitors display)
			   (nconc (display-monitors display) (list monitor))))))

    (:disconnected
     (do ((window (display-window-list-head display) (window-next window)))
	 ((not window))
	 
       (when (eq (window-monitor window) monitor)
	 (multiple-value-bind (width height) (window-size window)
	   (set-window-monitor window nil :xpos 0 :ypos 0 :width width :height height :refresh-rate :blah)
	   (multiple-value-bind (xoff yoff) (window-frame-size window)
	     (set-window-position window xoff yoff)))))

     (setf (display-monitors display)
	   (remove monitor (display-monitors display)))))
  (values))

;; internal API:

(defun choose-video-mode (monitor desired-video-mode)
  (let ((size-diff most-positive-fixnum)
	(least-size-diff most-positive-fixnum)
	(rate-diff most-positive-single-float)
	(least-rate-diff most-positive-single-float)
	(color-diff 0)
	(least-color-diff most-positive-fixnum)
	(closest nil))

    (unless (%refresh-video-modes monitor)
      (return-from choose-video-mode nil))

    (loop for current in (monitor-modes monitor)
       unless (eq (video-mode-red-bits desired-video-mode) :dont-care)
       do (setq color-diff (+ color-diff (abs (- (video-mode-red-bits current) (video-mode-red-bits desired-video-mode)))))
	 
       unless (eq (video-mode-green-bits desired-video-mode) :dont-care)
       do (setq color-diff (+ color-diff (abs (- (video-mode-green-bits current) (video-mode-green-bits desired-video-mode)))))
	 
       unless (eq (video-mode-blue-bits desired-video-mode) :dont-care)
       do (setq color-diff (+ color-diff (abs (- (video-mode-blue-bits current) (video-mode-blue-bits desired-video-mode)))))

       do (setq size-diff (abs (+ (expt (- (video-mode-width current) (video-mode-width desired-video-mode)) 2)
				  (expt (- (video-mode-height current) (video-mode-height desired-video-mode)) 2))))

	 (if (eq (video-mode-refresh-rate desired-video-mode) :dont-care)
	     (setq rate-diff (abs (- (video-mode-refresh-rate current) (video-mode-refresh-rate desired-video-mode))))
	     (setq rate-diff (- most-positive-single-float (video-mode-refresh-rate current))))
	 
       when (or (< color-diff least-color-diff)
		(and (= color-diff least-color-diff)
		     (= size-diff least-size-diff))
		(and (= color-diff least-color-diff)
		     (= size-diff least-size-diff)
		     (= rate-diff least-rate-diff)))
	 
       do (setq closest current)
	 (setq least-size-diff size-diff)
	 (setq least-rate-diff rate-diff)
	 (setq least-color-diff color-diff)

       finally (return (or closest current)))))

(defun compare-video-modes (fm sm)
  (let ((fbpp (+ (video-mode-red-bits fm)
		 (video-mode-green-bits fm)
		 (video-mode-blue-bits fm)))
	(sbpp (+ (video-mode-red-bits sm)
		 (video-mode-green-bits sm)
		 (video-mode-blue-bits sm)))
	(farea (* (video-mode-width fm)
		  (video-mode-height fm)))
	(sarea (* (video-mode-width sm)
		  (video-mode-height sm))))

    (unless (= fbpp sbpp)
      (return-from compare-video-modes (- fbpp sbpp)))

    (unless (= farea sarea)
      (return-from compare-video-modes (- farea sarea)))

    (unless (= (video-mode-width fm) (video-mode-width sm))
      (return-from compare-video-modes (- (video-mode-width fm) (video-mode-width sm))))

    (- (video-mode-refresh-rate fm) (video-mode-refresh-rate sm))))


(defun split-bpp (bpp)

  (when (= bpp 32)
    (setq bpp 24))

  (let* ((bpp/3 (truncate (/ bpp 3)))
	 (red bpp/3)
	 (green bpp/3)
	 (blue bpp/3)
	 (delta (- bpp (* red 3))))

    (when (>= delta 1)
      (setq green (1+ green)))

    (when (= delta 2)
      (setq red (1+ red)))

    (values red green blue)))

;; public API

(defun get-primary-monitor (display)
  (car (display-monitors display)))

#+win32
(defmethod get-monitor-pos ((monitor win32:monitor-mixin))
  (get-win32-monitor-pos monitor))

#+cocoa
(defmethod get-monitor-pos ((monitor cocoa:monitor-mixin))
  (get-cocoa-monitor-pos monitor))

#+x11
(defmethod get-monitor-pos ((monitor x11:monitor-mixin))
  (get-x11-monitor-pos monitor))

#+wayland
(defmethod get-monitor-pos ((monitor wayland:monitor-mixin))
  (get-wayland-monitor-pos monitor))

#+win32
(defmethod get-monitor-workarea ((monitor win32:monitor-mixin))
  (get-win32-monitor-workarea monitor))

#+cocoa
(defmethod get-monitor-workarea ((monitor cocoa:monitor-mixin))
  (get-cocoa-monitor-workarea monitor))

#+x11
(defmethod get-monitor-workarea ((monitor x11:monitor-mixin))
  (get-x11-monitor-workarea monitor))

#+wayland
(defmethod get-monitor-workarea ((monitor wayland:monitor-mixin))
  (get-wayland-monitor-workarea monitor))

(defun get-monitor-physical-size (monitor)
  (values (monitor-width-mm monitor)
	  (monitor-height-mm monitor)))
#+win32
(defmethod get-monitor-content-scale ((monitor win32:monitor-mixin))
  (get-win32-monitor-content-scale monitor))

#+cocoa
(defmethod get-monitor-content-scale ((monitor cocoa:monitor-mixin))
  (get-cocoa-monitor-content-scale monitor))

#+x11
(defmethod get-monitor-content-scale ((monitor x11:monitor-mixin))
  (get-x11-monitor-content-scale monitor))

#+wayland
(defmethod get-monitor-content-scale ((monitor wayland:monitor-mixin))
  (get-wayland-monitor-content-scale monitor))

#+win32
(defmethod get-monitor-video-modes ((monitor win32:monitor-mixin))
  (get-win32-monitor-video-modes monitor))

#+cocoa
(defmethod get-monitor-video-modes ((monitor cocoa:monitor-mixin))
  (get-cocoa-monitor-video-modes monitor))

#+x11
(defmethod get-monitor-video-modes ((monitor x11:monitor-mixin))
  (get-x11-monitor-video-modes monitor))

#+wayland
(defmethod get-monitor-video-modes ((monitor wayland:monitor-mixin))
  (get-wayland-monitor-video-modes monitor))

(defun set-gamma (monitor gamma)
  (setq gamma (coerce gamma 'single-float))
  
  (when (<= gamma 0.0f0)
    (error "Invalid gamma value ~A" gamma))

  (let ((original (get-gamma-ramp monitor)))
    (unless original
      (return-from set-gamma (values)))

    (let ((original-size (length (gamma-ramp-red original)))
	  (values ()))

      (loop for i from 0 below original-size
	    with value
	    do (setq value (/ i (1- original-size)))
	       (setq value (+ (* (expt value (/ gamma)) 65536.0f0) 0.5f0))
	       (setq value (floor (min value 65535.0f0)))
	       (push value values)
	    finally (setq values (nreverse values)))

      (set-gamma-ramp
       monitor
       (make-gamma-ramp
	:red (make-array original-size
			 :element-type '(unsigned-byte 16)
			 :initial-contents values)
	:green (make-array original-size
			   :element-type '(unsigned-byte 16)
			  :initial-contents values)
	:blue (make-array original-size
			  :element-type '(unsigned-byte 16)
			  :initial-contents values)))

      (values))))

#+win32
(defmethod get-gamma-ramp ((monitor win32:monitor-mixin))
  (get-win32-monitor-gamma-ramp monitor))

#+cocoa
(defmethod get-gamma-ramp ((monitor cocoa:monitor-mixin))
  (get-cocoa-monitor-gamma-ramp monitor))

#+x11
(defmethod get-gamma-ramp ((monitor x11:monitor-mixin))
  (get-x11-monitor-gamma-ramp monitor))

#+wayland
(defmethod get-gamma-ramp ((monitor wayland:monitor-mixin))
  (get-wayland-monitor-gamma-ramp monitor))

(defmethod set-gamma-ramp :around ((monitor monitor-mixin) ramp)
  (assert ramp)
  (assert (gamma-ramp-red ramp))
  (assert (gamma-ramp-green ramp))
  (assert (gamma-ramp-blue ramp))
  (when (= 0 (fill-pointer (gamma-ramp-red ramp)))
    (error "zero gamma ramp size ~S" ramp))

  (unless (monitor-original-ramp monitor)
    (unless (get-gamma-ramp monitor)))

  (call-next-method))

#+win32
(defmethod set-gamma-ramp ((monitor win32:monitor-mixin) ramp)
  (set-win32-monitor-gamma-ramp monitor ramp))

#+cocoa
(defmethod set-gamma-ramp ((monitor cocoa:monitor-mixin) ramp)
  (set-cocoa-monitor-gamma-ramp monitor ramp))

#+x11
(defmethod set-gamma-ramp ((monitor x11:monitor-mixin) ramp)
  (set-x11-monitor-gamma-ramp monitor ramp))

#+wayland
(defmethod set-gamma-ramp ((monitor wayland:monitor-mixin) ramp)
  (set-wayland-monitor-gamma-ramp monitor ramp))
