(in-package :abstract-os)

(defun %refresh-video-modes (monitor)
  (let ((modes nil))
    
  (when (monitor-modes monitor)
    (return-from %refresh-video-modes t))

  (setq modes
	#+darwin(get-cocoa-video-modes monitor)
	#+windows(get-win32-video-modes monitor)
	#+linux(get-linux-video-modes monitor))

  (unless modes
    (return-from %refresh-video-modes nil))

  (sort modes #'(lambda (a b)
		  (zerop (compare-video-modes a b))))

  (setf (monitor-modes monitor) modes)

  t))

;; Event API:

(defun input-monitor (app monitor &key (action nil) (placement nil))
  (declare (type monitor-mixin monitor))
  (declare (type application-mixin app))
  (ecase action
    (:connected

     (ecase placement
       (:insert-first (push monitor (application-monitors app)))
	 
       (:insert-last (setf (application-monitors app)
			   (nconc (application-monitors app) (list monitor))))))

    (:disconnected
     (do ((window (application-window-list-head app)))
	 ((window-next window))
	 
       (when (eq (window-monitor window) monitor)
	 (multiple-value-bind (width height) (get-os-window-size window)
	   (set-window-monitor window nil :xpos 0 :ypos 0 :width width :height height :refresh-rate :blah)
	   (multiple-value-bind (xoff yoff) (get-os-window-frame-size window)
	     (set-os-window-pos window xoff yoff)))))

     (setf (application-monitors app)
	   (remove monitor (application-monitors app)))))
  (values))

(defun input-monitor-window (monitor window)
  (declare (ignorable monitor window))
  (assert monitor)
  (setf (monitor-window monitor) window)
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

       finally (return closest))))

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

(defun get-monitors (app)
  (application-monitors app))

(defun get-primary-monitor (app)
  (car (application-monitors app)))

(defun get-monitor-pos (monitor)
  #+darwin(get-cocoa-monitor-pos monitor)
  #+windows(get-win32-monitor-pos monitor)
  #+linux(get-linux-monitor-pos monitor))

(defun get-monitor-workarea (monitor)
  #+darwin(get-cocoa-monitor-workarea monitor)
  #+windows(get-win32-monitor-workarea monitor)
  #+linux(get-linux-monitor-workarea monitor))

(defun get-monitor-physical-size (monitor)
  (values (monitor-width-mm monitor)
	  (monitor-height-mm monitor)))

(defun get-monitor-content-scale (monitor)
  #+darwin(get-cocoa-monitor-content-scale monitor)
  #+windows(get-win32-monitor-content-scale monitor)
  #+linux(get-linux-monitor-content-scale monitor))

(defun get-video-modes (monitor)
  
  (unless (%refresh-video-modes monitor)
    (return-from get-video-modes nil))
  
  (monitor-modes monitor))

(defun set-gamma (monitor gamma)
  (assert monitor)

  (setq gamma (coerce gamma 'single-float))

  (when (<= gamma 0.0f0)
    (error "Invalid gamma value ~A" gamma))

  (let ((original (get-gamma-ramp monitor)))
    (unless original
      (return-from set-gamma (values)))

    (let ((original-size (fill-pointer (gamma-ramp-red original)))
	  (ramp (make-gamma-ramp))
	  (values ()))

      (loop for i from 0 below original-size
	 with value
	 do (setq value (/ i (1- original-size)))
	   (setq value (+ (* (expt value (/ gamma)) 65536.0f0) 0.5f0))
	   (setq value (min value 65535.0f0))
	   (push value values)
	 finally (setq values (nreverse values)))

      (setf (gamma-ramp-red ramp) (make-array original-size :adjustable t :fill-pointer original-size
					      :initial-contents values))
      (setf (gamma-ramp-green ramp) (make-array original-size :adjustable t :fill-pointer original-size
						:initial-contents values))
      (setf (gamma-ramp-blue ramp) (make-array original-size :adjustable t :fill-pointer original-size
					       :initial-contents values))
      (set-gamma-ramp monitor ramp)
      (values))))

(defun get-gamma-ramp (monitor)
  #+darwin(get-cocoa-gamma-ramp monitor)
  #+windows(get-win32-gamma-ramp monitor)
  #+linux(get-linux-gama-ramp monitor))

(defun set-gamma-ramp (monitor ramp)
  (assert monitor)
  (assert ramp)
  (assert (gamma-ramp-red ramp))
  (assert (gamma-ramp-green ramp))
  (assert (gamma-ramp-blue ramp))
  (when (= 0 (fill-pointer (gamma-ramp-red ramp)))
    (error "zero gamma ramp size ~S" ramp))

  (unless (monitor-original-ramp monitor)
    (unless (get-gamma-ramp monitor)))
  
  #+darwin(set-cocoa-gamma-ramp monitor ramp)
  #+windows(set-win32-gamma-ramp monitor ramp)
  #+linux(set-linux-gamma-ramp monitor ramp))
