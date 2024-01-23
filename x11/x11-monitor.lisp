(in-package :clui)

(defun x11-display-mode-is-good? (mi)
  (zerop (logand (#_.modeFlags mi) #_RR_Interlace)))

(defun calculate-x11-refresh-rate (mi)
  (block nil
    (unless (or (zerop (#_.hTotal mi)) (zerop (#_.vTotal mi)))
      (return (coerce (/ (#_.dotClock mi) (* (#_.hTotal mi) (#_.vTotal mi))) 'double-float)))
    (return 0.0d0)))

(defun get-x11-mode-info (sr id)
  (loop for i from 0 below (#_.nmode sr)
	when (= (#_.id (c-aref (#_.modes sr) i)) id)
	  do (return (noffi::c+ (#_.modes sr) i))))

(defun vidmode-from-x11-mode-info (display mi ci)
  (let ((width)
	(height)
	(refresh-rate))

    (if (or (= (#_.rotation ci) #_RR_Rotate_90)
	    (= (#_.rotation ci) #_RR_Rotate_270))

	(setq width (#_.height mi)
	      height (#_.width mi))

	(setq width (#_.width mi)
	      height (#_.height mi)))

    (setq refresh-rate (calculate-x11-refresh-rate mi))

    (multiple-value-bind (red green blue)
	(split-bpp (#_DefaultDepth (h display) (default-screen-id display)))

      (make-video-mode :width width :height height :red-bits red :green-bits green :blue-bits blue
		       :refresh-rate refresh-rate))))

(defun poll-x11-monitors (display)
  (let ((xdisplay (h display))
	(screen-id (default-screen-id display))
	(root (default-root-window-handle display))
	(x11-state (display-x11-state display)))
    
    (if (and (randr-available? x11-state)
	     (not (randr-monitor-broken? x11-state)))
	
	(let* ((sr (#_XRRGetScreenResourcesCurrent xdisplay root))
	       (primary (#_XRRGetOutputPrimary xdisplay root))
	       (disconnected (copy-list (display-monitors display)))
	       (screens nil))
	  
	  (clet ((screen-count #_<int> 0))
	    (when (xinerama-available? x11-state)
	      (setq screens (#_XineramaQueryScreens xdisplay (c-addr-of screen-count))))

	    (loop for i from 0 below (#_.noutput sr)
		  with output
		  with oi
		  do (setq output (c-aref (#_.outputs sr) i))
		     (setq oi (#_XRRGetOutputInfo xdisplay sr output))
		  when (and (= (#_.connection oi) #_RR_Connected)
			    (not (noffi::ptr-nullptr-p (#_.crtcs oi))))
		    do (let ((found (find output disconnected :key #'monitor-x11-output :test #'equalp)))
			 (if found
			     (setq disconnected (remove found disconnected))
			     (let* ((crtc (c-aref (#_.crtcs oi) 0))
				    (ci (#_XRRGetCrtcInfo xdisplay sr crtc))
				    (width-mm)
				    (height-mm)
				    (placement :insert-last))

			       (if (or (= (#_.rotation ci) #_RR_Rotate_90) (= (#_.rotation ci) #_RR_Rotate_270))
				   (setq width-mm (#_.mm_height oi)
					 height-mm (#_.mm_width oi))
				   (setq width-mm (#_.mm_width oi)
					 height-mm (#_.mm_height oi)))
			     
			       (when (or (<= width-mm 0) (<= height-mm 0))
				 (setq width-mm (/ (* (#_.width ci) 25.4) 96.0)
				       height-mm (/ (* (#_.height ci) 25.4) 96.0)))

			       (let ((monitor (make-instance 'monitor
							     :display display
							     :name (noffi::get-c-string (#_.name oi))
							     :width-mm width-mm
							     :height-mm height-mm
							     :x11-output output
							     :x11-crtc crtc)))
		     
				 (loop for j from 0 below (cval-value screen-count) ;; ugly.
				       when (and (= (#_.x_org (c-aref screens j)) (#_.x ci))
						 (= (#_.y_org (c-aref screens j)) (#_.y ci))
						 (= (#_.width (c-aref screens j)) (#_.width ci))
						 (= (#_.height (c-aref screens j)) (#_.height ci)))
					 do (setf (monitor-x11-index monitor) j)
					    (return))

				 (when (equalp primary (monitor-x11-output monitor))
				   (setq placement :insert-first))
		     
				 (input-monitor display monitor :action :connected :placement placement))

			       (#_XRRFreeCrtcInfo ci))))
		  do (#_XRRFreeOutputInfo oi))

	    (#_XRRFreeScreenResources sr)
	    (when screens (#_XFree screens))

	    (loop for discon in disconnected
		  do (input-monitor display discon :action :disconnected))))

	(let ((width-mm (#_DisplayWidthMM xdisplay screen-id))
	      (height-mm (#_DisplayHeightMM xdisplay screen-id)))

	  (setf (display-monitors display) nil)
	  (input-monitor display
			 (make-instance 'monitor
					:display display
					:name "X11 monitor"
					:width-mm width-mm
					:height-mm height-mm)
			 :action :connected
			 :placement :insert-first)))
    (values)))

(defun set-x11-monitor-video-mode (monitor desired)
  (let* ((display (monitor-display monitor))
	 (x11-state (display-x11-state display)))
    
    (when (and (randr-available? x11-state) (not (randr-monitor-broken? x11-state)))
      (let ((best (choose-video-mode monitor desired))
	    (current (get-x11-monitor-video-mode monitor))
	    (native nil))
	   
	(when (zerop (compare-video-modes current best))
	  (return-from set-x11-monitor-video-mode (values)))
	   
	(let* ((xdisplay (h display))
	       (root (default-root-window-handle display))
	       (sr (#_XRRGetScreenResourcesCurrent xdisplay root))
	       (ci (#_XRRGetCrtcInfo xdisplay sr (monitor-x11-crtc monitor)))
	       (oi (#_XRRGetOutputInfo xdisplay sr (monitor-x11-output monitor))))
	     
	  (loop for i from 0 below (#_.nmode oi)
		with mi
		with mode
		do (setq mi (get-x11-mode-info sr (cval-value (c-aref (#_.modes oi) i))))
		when (x11-display-mode-is-good? mi)
		  do (setq mode (vidmode-from-x11-mode-info display mi ci))
		     (when (zerop (compare-video-modes best mode))
		       (setq native (#_.id mi))
		       (return)))

	  (when native
	    (unless (monitor-old-video-mode monitor)
	      (setf (monitor-old-video-mode monitor) (#_.mode ci)))

	    (#_XRRSetCrtcConfig xdisplay
				sr (monitor-x11-crtc monitor)
				#_CurrentTime
				(#_.x ci) (#_.y ci)
				native
				(#_.rotation ci)
				(#_.outputs ci)
				(#_.noutput ci)))

	  (#_XRRFreeOutputInfo oi)
	  (#_XRRFreeCrtcInfo ci)
	  (#_XRRFreeScreenResources sr)

	  (values))))))

(defun restore-x11-monitor-video-mode (monitor)
  (let* ((display (monitor-display monitor))
	 (x11-state (display-x11-state display)))
    
    (when (and (randr-available? x11-state) (not (randr-monitor-broken? x11-state)))
      (unless (monitor-old-video-mode monitor)
	(return-from restore-x11-monitor-video-mode (values)))

      (let* ((xdisplay (h display))
	     (root (default-root-window-handle display))
	     (sr (#_XRRGetScreenResourcesCurrent xdisplay root))
	     (ci (#_XRRGetCrtcInfo xdisplay sr (monitor-x11-crtc monitor))))

	(#_XRRSetCrtcConfig xdisplay
			    sr (monitor-x11-crtc monitor)
			    #_CurrentTime
			    (#_.x ci) (#_.y ci)
			    (monitor-old-video-mode monitor)
			    (#_.rotation ci)
			    (#_.outputs ci)
			    (#_.noutput ci))

	(#_XRRFreeCrtcInfo ci)
	(#_XRRFreeScreenResources sr)
	(setf (monitor-old-video-mode monitor) nil)

	(values)))))

(defun get-x11-monitor-pos (monitor)
  (let* ((display (monitor-display monitor))
	 (x11-state (display-x11-state display)))
    (when (and (randr-available? x11-state) (not (randr-monitor-broken? x11-state)))
      (let* ((xdisplay (h display))
	     (root (default-root-window-handle display))
	     (sr (#_XRRGetScreenResourcesCurrent xdisplay root))
	     (ci (#_XRRGetCrtcInfo xdisplay sr (monitor-x11-crtc monitor))))

	(prog1
	    (when ci
	      (prog1 (values (#_.x ci) (#_.y ci))
		(#_XRRFreeCrtcInfo ci)))
	  (#_XRRFreeScreenResources sr))))))

(defun get-x11-monitor-content-scale (monitor)
  (let ((scale (display-content-scale (monitor-display monitor))))
    (values scale scale)))

(defun get-x11-monitor-workarea (monitor)
  (let* ((display (monitor-display monitor))
	 (xdisplay (h display))
	 (root (default-root-window-handle display))
	 (x11-state (display-x11-state display))
	 (x 0)
	 (y 0)
	 (width)
	 (height))
    
    (if (and (randr-available? x11-state) (not (randr-monitor-broken? x11-state)))
	
	(let* ((sr (#_XRRGetScreenResourcesCurrent xdisplay root))
	       (ci (#_XRRGetCrtcInfo xdisplay sr (monitor-x11-crtc monitor)))
	       (mi (get-x11-mode-info sr (#_.mode ci))))

	  (setq x (#_.x ci)
		y (#_.y ci))

	  (if (or (= (#_.rotation ci) #_RR_Rotate_90)
		  (= (#_.rotation ci) #_RR_Rotate_270))
		
	      (setq width (#_.height mi)
		    height (#_.width mi))
		  
	      (setq width (#_.width mi)
		    height (#_.height mi)))
	  
	  (#_XRRFreeCrtcInfo ci)
	  (#_XRRFreeScreenResources sr))

	(let ((screen-id (default-screen-id display)))
	  (setq width (#_DisplayWidth xdisplay screen-id))
	  (setq height (#_DisplayHeight xdisplay screen-id))))

    (clet ((extents #_<Atom*> #_NULL)
	   (desktop #_<Atom*> #_NULL))
      (let ((&extents (c-addr-of extents))
	    (&desktop (c-addr-of desktop)))

	(with-slots (NET_WORKAREA
		     NET_CURRENT_DESKTOP)
	    
	    (display-window-manager display)
      
	  (let ((extent-count (%get-x11-window-property xdisplay root NET_WORKAREA #_XA_CARDINAL &extents)))
	    (when (> (%get-x11-window-property xdisplay root NET_CURRENT_DESKTOP #_XA_CARDINAL &desktop) 0)

	      (when (and (>= extent-count 4) (< (cval-value (c-aref desktop 0)) (/ extent-count 4)))

		(let* ((base-idx (* (cval-value (c-aref desktop 0)) 4))
		       (global-x (cval-value (c-aref extents (+ base-idx 0))))
		       (global-y (cval-value (c-aref extents (+ base-idx 1))))
		       (global-width (cval-value (c-aref extents (+ base-idx 2))))
		       (global-height (cval-value (c-aref extents (+ base-idx 3)))))

		  (when (< x global-x)
		    (setq width (- width (- global-x x)))
		    (setq x global-x))

		  (when (< y global-y)
		    (setq height (- height (- global-y y)))
		    (setq y global-y))

		  (when (> (+ x width) (+ global-x global-width))
		    (setq width (+ (- global-x x) width)))

		  (when (> (+ y height) (+ global-y global-height))
		    (setq height (+ (- global-y y) height))))))

	    (unless (ptr-nullptr-p extents)
	      (#_XFree extents))

	    (unless (ptr-nullptr-p desktop)
	      (#_XFree desktop))))))

    (values x y width height)))  

(defun get-x11-monitor-video-modes (monitor)
  (let* ((display (monitor-display monitor))
	 (x11-state (display-x11-state display))
	 (xdisplay (h display)))
    
    (if (and (randr-available? x11-state) (not (randr-monitor-broken? x11-state)))
	
	(let* ((root (default-root-window-handle display))
	       (sr (#_XRRGetScreenResourcesCurrent xdisplay root))
	       (ci (#_XRRGetCrtcInfo xdisplay sr (monitor-x11-crtc monitor)))
	       (oi (#_XRRGetOutputInfo xdisplay sr (monitor-x11-output monitor)))
	       (results ()))

	  (loop for i from 0 below (#_.nmode oi)
		with mi
		do (setq mi (get-x11-mode-info sr (cval-value (c-aref (#_.modes oi) i))))
		when (x11-display-mode-is-good? mi)
		  do (let ((mode (vidmode-from-x11-mode-info display mi ci)))

		       (unless (find mode results :test (lambda (a b)
							  (zerop (compare-video-modes a b))))
			 (push mode results))))

	  (#_XRRFreeOutputInfo oi)
	  (#_XRRFreeCrtcInfo ci)
	  (#_XRRFreeScreenResources sr)

	  (nreverse results))

	(list (get-x11-monitor-video-mode monitor)))))

(defun get-x11-monitor-video-mode (monitor)
  (let* ((display (monitor-display monitor))
	 (x11-state (display-x11-state display))
	 (xdisplay (h display)))
    
    (if (and (randr-available? x11-state) (not (randr-monitor-broken? x11-state)))
	
	(let* ((root (default-root-window-handle display))
	       (sr (#_XRRGetScreenResourcesCurrent xdisplay root))
	       (ci (#_XRRGetCrtcInfo xdisplay sr (monitor-x11-crtc monitor)))
	       (mode))

	  (when ci

	    (let ((mi (get-x11-mode-info sr (#_.mode ci))))

	      (when mi
		(setq mode (vidmode-from-x11-mode-info display mi ci)))

	      (#_XRRFreeCrtcInfo ci)))

	  (#_XRRFreeScreenResources sr)
	  mode)

	(let* ((screen-id (default-screen-id display))
	       (width (#_DisplayWidth xdisplay screen-id))
	       (height (#_DisplayHeight xdisplay screen-id))
	       (refresh-rate 0.0d0))

	  (multiple-value-bind (red green blue) (split-bpp (#_DefaultDepth xdisplay screen-id))
	    (make-video-mode :width width
			     :height height
			     :refresh-rate refresh-rate
			     :red-bits red
			     :green-bits green
			     :blue-bits blue))))))

#+NOTYET
(defun get-x11-monitor-gamma-ramp (monitor)
  (let* ((display (monitor-display monitor))
	 (x11-state (display-x11-state display))
	 (xdisplay (h display)))
    
    (if (and (randr-available? x11-state) (not (randr-monitor-broken? x11-state)))
	
	(let* ((size (#_XRRGetCrtcGammaSize xdisplay (monitor-x11-crtc monitor)))
	       (gamma (#_XRRGetCrtcGamma  xdisplay (monitor-x11-crtc monitor)))
	       (red (make-array size :element-type '(unsigned-byte 16)))
	       (green (make-array size :element-type '(unsigned-byte 16)))
	       (blue (make-array size :element-type '(unsigned-byte 16))))

	  ;;(ccl::%copy-ptr-to-ivector (ptr-value (#_.red gamma)) red (* size 2))
	  ;;(ccl::%copy-ptr-to-ivector (ptr-value (#_.green gamma)) green (* size 2))
	  ;;(ccl::%copy-ptr-to-ivector (ptr-value (#_.blue gamma)) blue (* size 2))
	  ))))
	  

	  
  
