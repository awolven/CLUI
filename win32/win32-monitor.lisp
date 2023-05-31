(in-package :clui)
(noffi::noffi-syntax t)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (noffi::noffi-syntax t))

(eval-when (:compile-toplevel :load-toplevel :execute)
  #_{typedef struct monitor_cons {LPCWSTR adapterName; HMONITOR handle;} monitor_cons;}
  )


(defcfun (monitor-callback #_<BOOL>)
	 ((handle #_<HMONITOR>)
	  (dc #_<HDC>)
	  (rect #_<RECT*>)
	  (data #_<LPARAM>))
  (declare (ignore dc rect))
  (maybe-get-monitor-handle handle data))

(defun maybe-get-monitor-handle (handle data)
  (clet ((mi #_<MONITORINFOEXW>))
    (let* ((&mi (c-addr-of mi))
	   (p-szDevice (c->-addr &mi '#_szDevice))
	   (size (c-sizeof-type '#_<MONITORINFOEXW>))
	   (&mc (noffi::cons-ptr (int-sap data) 0 '#_<monitor_cons>)))
      (#_memset &mi size 0)
      (setf (#_.cbSize (c-cast '#_<MONITORINFO*> &mi)) size)
      (setf (#_.handle &mc) (noffi::%cons-ptr (int-sap 0) 0 '#_<HMONITOR>))

      (unless (zerop (#_GetMonitorInfoW handle &mi))

	(when (zerop (#_wcscmp p-szDevice (#_.adapterName &mc)))
	  (setf (#_.handle &mc) handle)))

      #_TRUE)))

(defun create-monitor (&adapter &display desktop)
  (let ((p-adapter-name (c->-addr &adapter '#_DeviceName))
	(p-display-name (c->-addr &display '#_DeviceName))
	(p-adapter-string (c->-addr &adapter '#_DeviceString))
	(p-display-string (c->-addr &display '#_DeviceString))
	(width-mm)
	(height-mm)
	(name)
	(monitor))

    (if &display
	(setq name (lpcwstr->string p-display-string))
	(setq name (lpcwstr->string p-adapter-string)))

    (clet ((dm #_<DEVMODEW>))
      (let ((&dm (c-addr-of dm))
	    (dm-size (c-sizeof-type '#_<DEVMODEW>)))
	(#_memset &dm dm-size 0)
	(setf (#_.dmSize &dm) dm-size)

	(#_EnumDisplaySettingsW p-adapter-name #_ENUM_CURRENT_SETTINGS &dm)

	(with-lpcwstr (dstring "DISPLAY")
	  (let ((hdc (#_CreateDCW dstring p-adapter-name nil nil)))

	    (if (windows-8.1-or-greater?)
		(progn
		  (setq width-mm (#_GetDeviceCaps hdc #_HORZSIZE))
		  (setq height-mm (#_GetDeviceCaps hdc #_VERTSIZE)))

		(progn
		  (setq width-mm (/ (* (#_.dmPelsWidth &dm) 25.4) (#_GetDeviceCaps hdc #_LOGPIXELSX)))
		  (setq height-mm (/ (* (#_.dmPelsHeight &dm) 25.4) (#_GetDeviceCaps hdc #_LOGPIXELSY)))))

	    (#_DeleteDC hdc)))

	(setq monitor (make-instance 'monitor
				     :display desktop
				     :name name
				     :width-mm width-mm
				     :height-mm height-mm
				     :modes-pruned? (logtest (#_.StateFlags &adapter) #_DISPLAY_DEVICE_MODESPRUNED)
				     :adapter-name (lpcwstr->string p-adapter-name)
				     :display-name (when &display
						     (lpcwstr->string p-display-name))))

	(let ((dmPosition (ptr-inc &dm (+ (* 2 32) (* 2 4) 4) '#_<POINTL*>)))
	  
	  (setf (h monitor) (find-monitor-handle (lpcwstr->string p-adapter-name)
						 (#_.x dmPosition)
						 (#_.y dmPosition)
						 (+ (#_.x dmPosition) (#_.dmPelsWidth &dm))
						 (+ (#_.y dmPosition) (#_.dmPelsHeight &dm))))
	  monitor)))))

(defun find-monitor-handle (adapter-name &optional (left nil) (top nil) (right nil) (bottom nil))
  (clet ((mc #_<monitor_cons>))
    (let* ((&mc (c-addr-of mc))
	   (lparam (cons-cval (sap-int (ptr-effective-sap &mc)) '#_<LPARAM>)))
      (with-lpcwstr (padaptername adapter-name)
	(setf (#_.adapterName &mc) padaptername)
	(setf (#_.handle &mc) (noffi::%cons-ptr (int-sap 0) 0 '#_<HMONITOR>))
	(if bottom
	    (clet ((rect #_<RECT>))
	      (let ((&rect (c-addr-of rect)))
		(setf (#_.left &rect) left
		      (#_.top &rect) top
		      (#_.right &rect) right
		      (#_.bottom &rect) bottom)
		(#_EnumDisplayMonitors nil &rect (noffi::callback 'monitor-callback) lparam)))
	    (#_EnumDisplayMonitors nil nil (noffi::callback 'monitor-callback) lparam))
	(#_.handle &mc)))))

(defun poll-win32-monitors (desktop)
  (clet ((adapter #_<DISPLAY_DEVICEW>)
	 (display #_<DISPLAY_DEVICEW>))
    (let* ((&adapter (c-addr-of adapter))
	   (p-adapter-name (c->-addr &adapter '#_DeviceName))
	   (&display (c-addr-of display))
	   (p-display-name (c->-addr &display '#_DeviceName))
	   (size (c-sizeof-type '#_<DISPLAY_DEVICEW>))
	   (disconnected (copy-list (display-monitors desktop))))

      (loop for adapter-index from 0
	    do (let ((placement :insert-last))
		 (#_memset &adapter size 0)
		 (setf (#_.cb &adapter) size)

		 (when (zerop (#_EnumDisplayDevicesW nil adapter-index &adapter 0))
		   (return))

		 (when (logtest (#_.StateFlags &adapter) #_DISPLAY_DEVICE_ACTIVE)

		   (when (logtest (#_.StateFlags &adapter) #_DISPLAY_DEVICE_PRIMARY_DEVICE)
		     (setq placement :insert-first))

		   (loop for display-index from 0
			 with monitor
			 with found? = nil
			 with display-name
			 do (#_memset &display size 0)
			    (setf (#_.cb &display) size)
			    
			    (when (zerop (#_EnumDisplayDevicesW p-adapter-name display-index &display 0))
			      (return))

			    (unless (logtest (#_.StateFlags &display) #_DISPLAY_DEVICE_ACTIVE)
			      
			      (setq display-name (lpcwstr->string p-display-name))
			      
			      (let ((m (find display-name disconnected :key #'display-name :test #'string=)))
				(when m
				  (setq disconnected (remove m disconnected))
				  (setf (h m) (find-monitor-handle (lpcwstr->string p-adapter-name)))
				  (setq found? t)
				  (return))))

			    (unless found?
			      (setq monitor (create-monitor &adapter &display desktop))
			      (unless monitor
				(return))

			      (input-monitor desktop monitor :action :connected :placement placement))

			    (setq placement :insert-last)))))

      (loop for discon in disconnected
	    do (input-monitor desktop discon :action :disconnected)))))

(defun restore-win32-monitor-video-mode (monitor)
  (when (mode-changed? monitor)
    (with-lpcwstr (p-adapter-name (adapter-name monitor))
      (#_ChangeDisplaySettingsExW p-adapter-name nil nil #_CDS_FULLSCREEN nil)
      (setf (mode-changed? monitor) nil)))
  (values))

(defun set-win32-monitor-video-mode (monitor desired)
  (block nil
    (let ((best (choose-video-mode monitor desired))
	  (current (get-win32-monitor-video-mode monitor))
	  (result))
      
      (when (zerop (compare-video-modes current best))
	(return (values)))

      (clet ((dm #_<DEVMODEW>))
	(let ((&dm (c-addr-of dm))
	      (dm-size (c-sizeof-type '#_<DEVMODEW>)))
	  (#_memset &dm dm-size 0)
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

	  (with-lpcwstr (p-adapter-name (adapter-name monitor))
	    (setq result (#_ChangeDisplaySettingsExW p-adapter-name &dm nil #_CDS_FULLSCREEN nil))

	    (if (= result #_DISP_CHANGE_SUCCESSFUL)
		(return
		  (progn
		    (setf (mode-changed? monitor) t)
		    (values)))

		(error "Failed to set video mode, reason: ~A"
		       (case result
			 (#.#_DISP_CHANGE_BADDUALVIEW "The system uses DualView")
			 (#.#_DISP_CHANGE_BADFLAGS "Invalid flags")
			 (#.#_DISP_CHANGE_BADMODE "Graphics mode not supported")
			 (#.#_DISP_CHANGE_BADPARAM "Invalid parameter")
			 (#.#_DISP_CHANGE_FAILED "Graphics mode failed")
			 (#.#_DISP_CHANGE_NOTUPDATED "Failed to write to registry")
			 (#.#_DISP_CHANGE_RESTART "Computer restart required"))))))))))

(defun get-win32-monitor-pos (monitor)
  (clet ((dm #_<DEVMODEW>))
    (let* ((&dm (c-addr-of dm))
	   (dm-size (c-sizeof-type '#_<DEVMODEW>))
	   (dmPosition (ptr-inc &dm (+ (* 2 32) (* 2 4) 4) '#_<POINTL*>)))
      (setf (#_.dmSize &dm) dm-size)

      (with-lpcwstr (p-adapter-name (adapter-name monitor))
	(#_EnumDisplaySettingsExW p-adapter-name #_ENUM_CURRENT_SETTINGS &dm #_EDS_ROTATEDMODE)

	(values (#_.x dmPosition)
		(#_.y dmPosition))))))
	      
  
(defun get-win32-monitor-content-scale (monitor)
  (get-hmonitor-content-scale (h monitor)))

(defun get-win32-monitor-workarea (monitor)
  (clet ((mi #_<MONITORINFO>))
    (let ((&mi (c-addr-of mi)))
      (setf (#_.cbSize &mi) (c-sizeof-type '#_<MONITORINFO>))

      (#_GetMonitorInfoW (h monitor) &mi)

      (values (#_.left #_(&(@mi).rcWork))
	      (#_.top #_(&(@mi).rcWork))
	      (- (#_.right #_(&(@mi).rcWork)) (#_.left #_(&(@mi).rcWork)))
	      (- (#_.bottom #_(&(@mi).rcWork)) (#_.top #_(&(@mi).rcWork)))))))

(defun get-win32-monitor-video-mode (monitor)
  (clet ((dm #_<DEVMODEW>))
    (let ((&dm (c-addr-of dm))
	  (dm-size (c-sizeof-type '#_<DEVMODEW>)))
      (#_memset &dm dm-size 0)
      (setf (#_.dmSize &dm) dm-size)

      (with-lpcwstr (p-adapter-name (adapter-name monitor))
	(#_EnumDisplaySettingsW p-adapter-name #_ENUM_CURRENT_SETTINGS &dm)

	(multiple-value-bind (red green blue) (split-bpp (#_.dmBitsPerPel &dm))
	  (make-video-mode
	   :width (#_.dmPelsWidth &dm)
	   :height (#_.dmPelsHeight &dm)
	   :refresh-rate (#_.dmDisplayFrequency &dm)
	   :red-bits red
	   :green-bits green
	   :blue-bits blue))))))

(defun get-win32-monitor-video-modes (monitor)
  ;;(when (modes-pruned? monitor)
    (clet ((dm #_<DEVMODEW>))
      (let ((&dm (c-addr-of dm))
	    (dm-size (c-sizeof-type '#_<DEVMODEW>)))
	(with-lpcwstr (p-adapter-name (adapter-name monitor))
	  (loop for mode-index from 0
		with results = ()
		do 
		   (#_memset &dm dm-size 0)
		   (setf (#_.dmSize &dm) dm-size)
		 
		when (zerop (#_EnumDisplaySettings p-adapter-name mode-index &dm))
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
			   (when (= (#_ChangeDisplaySettingsExW p-adapter-name &dm nil #_CDS_TEST nil)
				    #_DISP_CHANGE_SUCCESSFUL)
			     (push mode results))))))))));;)
			  
			  
    
