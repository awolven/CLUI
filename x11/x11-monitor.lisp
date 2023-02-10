(in-package :clui)

(defun mode-is-good (mi)
  (zerop (logand (#_.modeFlags mi) #_RR_Interlace)))

(defun calculate-refresh-rate (mi)
  (block nil
    (unless (or (zerop (#_hTotal mi)) (zerop (#_vTotal mi)))
      (return (* (/ (#_dotClock mi) (#_hTotal mi)) (#_vTotal mi))))

    (return 0.0d0)))

(defun get-mode-info (sr id)
  (loop for i from 0 below (#_nmode sr)
	when (= (c-aref (#_.modes sr) i) id)
	  do (return (noffi::c+ (#_.modes sr) i))))

(defun vidmode-from-mode-info (display mi ci)
  (let ((width)
	(height)
	(refresh-rate))

    (if (or (= (#_.rotation ci) #_RR_Rotate_90)
	    (= (#_.rotation ci) #_RR_Rotate_270))

	(setq width (#_.height mi)
	      height (#_.width mi))

	(setq width (#_.width mi)
	      height (#_.height mi)))

    (setq refresh-rate (calculate-refresh-rate mi))

    (multiple-value-bind (red green blue)
	(split-bpp (#_DefaultDepth (display-xdisplay display) (default-screen-id display)))

      (make-video-mode :width width :height height :red-bits red :green-bits green :blue-bits blue
		       :refresh-rate refresh-rate))))

(defun poll-x11-monitors (display)
  (let ((xdisplay (display-xdisplay display))
	(screen-id (default-screen-id display))
	(root (default-root-window-handle display)))
    
    (if (and (display-randr-available? display)
	     (not (display-randr-monitor-broken? display)))
	
	(let* ((sr (#_XRRGetScreenResourcesCurrent xdisplay root))
	       (primary (#_XRRGetOutputPrimary xdisplay root))
	       (screens nil))

	  (clet ((screen-count #_<int> 0))
	    (when (xinerama-available? display)
	      (setq screens (#_XineramaQueryScreens xdisplay (c-addr-of screen-count))))

	    (let ((disconnected (copy-list (display-monitors display))))
	      (loop for i from 0 below (#_.noutput sr)
		    with oi
		    with ci
		    with skip?
		    with found
		    with width-mm
		    with height-mm
		    with placement
		    do (setq skip? nil)
		       (setq found nil)
		       (setq oi (#_XRRGetOutputInfo xdisplay sr (c-aref (#_.outputs sr) i)))
		    when (and (or (/= (#_.connection oi) #_RR_Connected) (= (#_.crtc oi) #_None))
			      (setq skip? t))
		      do (#_XRRFreeOutputInfo oi)
		    unless skip?
		      when (setq found (find (c-aref (#_.outputs sr) i) disconnected :key #'monitor-x11-output :test #'=))
			do (setq disconnected (remove found disconnected))
			   (#_XRRFreeOutputInfo oi)
		    unless (or found skip?)
		      do (setq ci (#_XRRGetCrtcInfo xdisplay sr (#_.crtc oi)))
			 (if (or (= (#_.rotation ci) #_RR_Rotate_90) (= (#_.rotation ci) #_RR_Rotate_270))
			     (setq width-mm (#_.height oi)
				   height-mm (#_.width oi))
			     (setq width-mm (#_.width oi)
				   height-mm (#_.height oi)))
			 (when (or (<= width-mm 0) (<= height-mm 0))
			   (setq width-mm (/ (* (#_.width ci) 25.4) 96.0)
				 height-mm (/ (* (#_.height ci) 25.4) 96.0)))
			 (let ((monitor (make-instance 'monitor
						       :name (#_.name oi)
						       :width-mm width-mm
						       :height-mm height-mm
						       :x11-output (c-aref (#_.outputs sr) i)
						       :x11-crtc (#_.crtc oi))))
		     
			   (loop for j from 0 below screen-count ;; ugly.
				 when (and (= (#_.x_org (c-aref screens j)) (#_.y ci))
					   (= (#_.y_org (c-aref screens j)) (#_.x ci))
					   (= (#_.width (c-aref screens j)) (#_.width ci))
					   (= (#_.height (c-aref screens j)) (#_.height ci)))
				   do (setf (monitor-x11-index monitor) j)
				      (return))

			   (if (= primary (monitor-x11-output monitor))
			       (setq placement :insert-first)
			       (setq placement :insert-last))
		     
			   (input-monitor display monitor :action :connected :placement placement))
		   
			 (#_XRRFreeCrtcInfo ci)
		    do
		       (#_XRRFreeOutputInfo oi))

	      (loop for discon in disconnected
		    do (input-monitor display discon :action :disconnected)))

	    (when screens (#_XFree screens)))

	  (#_XRRFreeScreenResources sr))

	(let ((width-mm (#_DisplayWidthMM xdisplay screen-id))
	      (height-mm (#_DisplayHeightMM xdisplay screen-id)))

	  (input-monitor display
			 (make-instance 'monitor
					:name "X11 monitor"
					:width-mm width-mm
					:height-mm height-mm)
			 :action :connected
			 :placement :insert-first)))
    (values)))
  
		   
					       
		 
      
      

