(in-package :clui)

(eval-when (:compile-toplevel :load-toplevel :execute)
  #_{typedef struct monitor_cons {LPCWSTR adapterName; HMONITOR handle;} monitor_cons;}
  )


(defun get-win32-monitor-gamma-ramp (monitor)
  (declare (ignore monitor))
  (values))

(defun set-win32-monitor-gamma-ramp (monitor ramp)
  (declare (ignore monitor ramp))
  (values))

(defcfun (monitor-callback #_<BOOL>)
	 ((handle #_<HMONITOR>)
	  (dc #_<HDC>)
	  (rect #_<RECT*>)
	  (data #_<LPARAM>))
  (declare (ignore dc rect))
  (maybe-get-monitor-handle handle data))

(defun maybe-get-monitor-handle (handle data)
  (clet ((mi #_<MONITORINFOEX>))
    (let* ((&mi (c-addr-of mi))
	   (szDevice (#_.szDevice &mi))
	   (size #_(sizeof(MONITORINFOEX)))
	   (&mc (noffi::cons-ptr (int-sap data) 0 '#_<monitor_cons>)))
      ;;(#_memset &mi size 0)
      (setf (#_.cbSize (c-cast '#_<MONITORINFO*> &mi)) size)
      (setf (#_.handle &mc) (noffi::%cons-ptr (int-sap 0) 0 '#_<HMONITOR>))

      (unless (zerop (#_GetMonitorInfo handle &mi))

	(when (string= (lpcwstr->string szDevice) (lpcwstr->string (#_.adapterName &mc)))
	  (setf (#_.handle &mc) handle)))

      #_TRUE)))

(defun ptr-inc (ptr offset type)
  (cons-ptr (int-sap (+ (sap-int (ptr-effective-sap ptr)) offset)) 0 type))

(defun create-monitor (adapter* display* display)
  (let ((adapter-name (lpcwstr->string (#_.DeviceName adapter*)))
	(display-name (lpcwstr->string (#_.DeviceName display*)))
	(adapter-string (lpcwstr->string (#_.DeviceString adapter*)))
	(display-string (lpcwstr->string (#_.DeviceString display*)))
	(width-mm)
	(height-mm)
	(name)
	(monitor))

    (if display*
	(setq name display-string)
	(setq name adapter-string))

    (clet ((dm #_<DEVMODE>))
      (let ((&dm (c-addr-of dm))
	    (dm-size #_(sizeof(DEVMODE))))
	;;(#_memset &dm dm-size 0)
	(setf (#_.dmSize &dm) dm-size)

	(#_EnumDisplaySettings adapter-name #_ENUM_CURRENT_SETTINGS &dm)

	(let ((hdc (#_CreateDC "DISPLAY" adapter-name nil nil)))

	    (if (windows-8.1-or-greater?)
		(progn
		  (setq width-mm (#_GetDeviceCaps hdc #_HORZSIZE))
		  (setq height-mm (#_GetDeviceCaps hdc #_VERTSIZE)))

		(progn
		  (setq width-mm (/ (* (#_.dmPelsWidth &dm) 25.4) (#_GetDeviceCaps hdc #_LOGPIXELSX)))
		  (setq height-mm (/ (* (#_.dmPelsHeight &dm) 25.4) (#_GetDeviceCaps hdc #_LOGPIXELSY)))))

	    (#_DeleteDC hdc))

	(setq monitor (make-instance 'monitor
				     :display display
				     :name name
				     :width-mm width-mm
				     :height-mm height-mm
				     :modes-pruned? (logtest (#_.StateFlags adapter*) #_DISPLAY_DEVICE_MODESPRUNED)
				     :adapter-name adapter-name
				     :display-name (when display*
						     display-name)))

	(let ((dmPosition (ptr-inc &dm (+ (* 2 32) (* 2 4) 4) '#_<POINTL*>)))
	  
	  (setf (h monitor) (find-monitor-handle adapter-name
						 (#_.x dmPosition)
						 (#_.y dmPosition)
						 (+ (#_.x dmPosition) (#_.dmPelsWidth &dm))
						 (+ (#_.y dmPosition) (#_.dmPelsHeight &dm))))
	  monitor)))))

(defun find-monitor-handle (adapter-name &optional (left nil) (top nil) (right nil) (bottom nil))
  (clet ((mc #_<monitor_cons>))
    (let* ((&mc (c-addr-of mc))
	   (lparam (cons-cval (sap-int (ptr-effective-sap &mc)) '#_<LPARAM>)))
      (setf (#_.adapterName &mc) adapter-name)
      (setf (#_.handle &mc) (noffi::%cons-ptr (int-sap 0) 0 '#_<HMONITOR>))
      (if bottom
	  (clet ((rect #_<RECT>))
	    (let ((&rect (c-addr-of rect)))
	      (setf (#_.left &rect) left
		    (#_.top &rect) top
		    (#_.right &rect) right
		    (#_.bottom &rect) bottom)
	      (#_EnumDisplayMonitors nil &rect monitor-callback lparam)))
	  (#_EnumDisplayMonitors nil nil monitor-callback lparam))
      (#_.handle &mc))))

(defun memset (p v n) (dotimes (i n) (setf (noffi::peek-u8 p i) v)))

(defun poll-win32-monitors (dpy)

  (clet ((adapter #_<DISPLAY_DEVICE>)
	 (display #_<DISPLAY_DEVICE>))
    (let* ((&adapter (c-addr-of adapter))
	   (&display (c-addr-of display))
	   (size #_(sizeof(DISPLAY_DEVICE)))
	   (disconnected (copy-list (display-monitors dpy))))


      (loop for adapter-index from 0
	    do (let ((placement :insert-last))
		 (memset &adapter size 0)
		 (setf (#_.cb &adapter) size)
		 
		 (when (zerop (#_EnumDisplayDevices nil adapter-index &adapter 0))
		   (return))

		 (let ((adapter-name (lpcwstr->string (#_.DeviceName &adapter))))
		 
		   (when (logtest (#_.StateFlags &adapter) #_DISPLAY_DEVICE_ACTIVE)
		   
		     (when (logtest (#_.StateFlags &adapter) #_DISPLAY_DEVICE_PRIMARY_DEVICE)
		       (setq placement :insert-first))
		     
		     (loop for display-index from 0
			   with monitor
			   with found? = nil
			   do (memset &display size 0)
			      (setf (#_.cb &display) #_(sizeof(DISPLAY_DEVICE)))
			      
			      (when (zerop (#_EnumDisplayDevices
					    adapter-name
					    display-index &display 0))
				(return))

			      (let ((display-name (lpcwstr->string (#_.DeviceName &display))))

				(unless (logtest (#_.StateFlags &display) #_DISPLAY_DEVICE_ACTIVE)
			      
				  (let ((m (find display-name disconnected :key #'display-name :test #'string=)))
				    (when m
				      (setq disconnected (remove m disconnected))
				      (setf (h m) (find-monitor-handle display-name))
				      (setq found? t)
				      (return))))

				(unless found?
				  (setq monitor (create-monitor &adapter &display dpy))
				  (unless monitor
				    (return))
				  
				  (input-monitor dpy monitor :action :connected :placement placement))
				
				(setq placement :insert-last)))))))
      
      (loop for discon in disconnected
	    do (input-monitor dpy discon :action :disconnected)))))

(defun restore-win32-monitor-video-mode (monitor)
  (when (mode-changed? monitor)
    (#_ChangeDisplaySettingsEx (adapter-name monitor) nil nil #_CDS_FULLSCREEN nil)
    (setf (mode-changed? monitor) nil))
  (values))

(defun set-win32-monitor-video-mode (monitor desired)
  (block nil
    (let ((best (choose-video-mode monitor desired))
	  (current (get-win32-monitor-video-mode monitor))
	  (result))
      
      (when (zerop (compare-video-modes current best))
	(return (values)))

      (clet ((dm #_<DEVMODE>))
	(let ((&dm (c-addr-of dm))
	      (dm-size #_(sizeof(DEVMODE))))
	  ;;(#_memset &dm dm-size 0)
	  (setf (#_.dmSize &dm) dm-size)

	  (setf (#_.dmFields &dm) (logior #_DM_PELSWIDTH #_DM_PELSHEIGHT #_DM_BITSPERPEL)
		(#_.dmPelsWidth &dm) (video-mode-width best)
		(#_.dmPelsHeight &dm) (video-mode-height best)
		(#_.dmBitsPerPel &dm) (+ (video-mode-red-bits best)
					 (video-mode-green-bits best)
					 (video-mode-blue-bits best))
		(#_.dmDisplayFrequency &dm) (video-mode-refresh-rate best))

	  (when (or (< (#_.dmBitsPerPel &dm) 15)
		    (>= (#_.dmBitsPerPel &dm) 24))
	    (setf (#_.dmBitsPerPel &dm) 32))

	  (setq result (#_ChangeDisplaySettingsEx (adapter-name monitor) &dm nil #_CDS_FULLSCREEN nil))

	  (if (= result #_DISP_CHANGE_SUCCESSFUL)
	      (return
		(progn
		  (setf (mode-changed? monitor) t)
		  (values)))

	      (error "Failed to set video mode, reason: ~A"
		     (cond
		       ((= result #_DISP_CHANGE_BADDUALVIEW) "The system uses DualView")
		       ((= result #_DISP_CHANGE_BADFLAGS) "Invalid flags")
		       ((= result #_DISP_CHANGE_BADMODE) "Graphics mode not supported")
		       ((= result #_DISP_CHANGE_BADPARAM) "Invalid parameter")
		       ((= result #_DISP_CHANGE_FAILED) "Graphics mode failed")
		       ((= result #_DISP_CHANGE_NOTUPDATED) "Failed to write to registry")
		       ((= result #_DISP_CHANGE_RESTART) "Computer restart required")))))))))

(defun get-win32-monitor-pos (monitor)
  (clet ((dm #_<DEVMODEW>))
    (let* ((&dm (c-addr-of dm))
	   (dm-size #_(sizeof(DEVMODE)))
	   (dmPosition (ptr-inc &dm (+ (* 2 32) (* 2 4) 4) '#_<POINTL*>)))
      (setf (#_.dmSize &dm) dm-size)

      (#_EnumDisplaySettingsEx (adapter-name monitor) #_ENUM_CURRENT_SETTINGS &dm #_EDS_ROTATEDMODE)

      (values (#_.x dmPosition)
	      (#_.y dmPosition)))))
	      
  
(defun get-win32-monitor-content-scale (monitor)
  (get-hmonitor-content-scale (h monitor)))

(defun get-win32-monitor-workarea (monitor)
  (clet ((mi #_<MONITORINFO>))
    (let ((&mi (c-addr-of mi)))
      (setf (#_.cbSize &mi) (c-sizeof-type '#_<MONITORINFO>))

      (#_GetMonitorInfo (h monitor) &mi)

      (values (#_.left #_(&(@mi).rcWork))
	      (#_.top #_(&(@mi).rcWork))
	      (- (#_.right #_(&(@mi).rcWork)) (#_.left #_(&(@mi).rcWork)))
	      (- (#_.bottom #_(&(@mi).rcWork)) (#_.top #_(&(@mi).rcWork)))))))

(defun get-win32-monitor-video-mode (monitor)
  (clet ((dm #_<DEVMODE>))
    (let ((&dm (c-addr-of dm))
	  (dm-size #_(sizeof(DEVMODE))))
      ;;(#_memset &dm dm-size 0)
      (setf (#_.dmSize &dm) dm-size)

      (#_EnumDisplaySettings (adapter-name monitor) #_ENUM_CURRENT_SETTINGS &dm)

      (multiple-value-bind (red green blue) (split-bpp (#_.dmBitsPerPel &dm))
	(make-video-mode
	 :width (#_.dmPelsWidth &dm)
	 :height (#_.dmPelsHeight &dm)
	 :refresh-rate (#_.dmDisplayFrequency &dm)
	 :red-bits red
	 :green-bits green
	 :blue-bits blue)))))

(defun get-win32-monitor-video-modes (monitor)
  ;;(when (modes-pruned? monitor)
  (clet ((dm #_<DEVMODE>))
    (let ((&dm (c-addr-of dm))
	  (dm-size #_(sizeof(DEVMODE))))
      (loop for mode-index from 0
	    with results = ()
	    do 
	       ;;(#_memset &dm dm-size 0)
	       (setf (#_.dmSize &dm) dm-size)
		 
	    when (zerop (#_EnumDisplaySettings (adapter-name monitor) mode-index &dm))
	      do (return (nreverse results))

	    when (>= (#_.dmBitsPerPel &dm) 15)
	      do (multiple-value-bind (red green blue) (split-bpp (#_.dmBitsPerPel &dm))
		   (let ((mode (make-video-mode
				:width (#_.dmPelsWidth &dm)
				:height (#_.dmPelsHeight &dm)
				:refresh-rate (#_.dmDisplayFrequency &dm)
				:red-bits red
				:green-bits green
				:blue-bits blue)))
		       
		     (unless (find mode results :test (lambda (a b) (zerop (compare-video-modes a b))))
		       (when (= (#_ChangeDisplaySettingsEx (adapter-name monitor) &dm nil #_CDS_TEST nil)
				#_DISP_CHANGE_SUCCESSFUL)
			 (push mode results))))))))) ;;)
			  
			  
    
