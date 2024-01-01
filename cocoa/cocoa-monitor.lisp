(in-package :clui)
(named-readtables:in-readtable :objc-readtable)

(defun get-cocoa-display-name (display-id &optional (screen nil))
  (when screen
    (when (ns:|respondsToSelector:| screen @(localizedName))
      (let ((name (ns:|valueForKey:| screen (objc-runtime::make-nsstring "localizedName"))))
	(unless (cffi:null-pointer-p name)
	  (return-from get-cocoa-display-name (objc-runtime::extract-nsstring name))))))

  (let ((info)
	(vendor-id-ref)
	(product-id-ref)
	(service))
    
    (cffi:with-foreign-object (it :unsigned-int)

     (unless (= 0 (IOServiceGetMatchingServices MACH_PORT_NULL (IOServiceMatching "IODisplayConnect") it))
       (return-from get-cocoa-display-name "Display"))

     (loop while (not (= 0 (setq service (IOIteratorNext (cffi:mem-aref it :unsigned-int)))))
	do (setq info (IODisplayCreateInfoDictionary MACH_PORT_NULL kIODisplayOnlyPreferredName))
		
	  (setq vendor-id-ref (CFDictionaryGetValue info (CFSTR kDisplayVendorID)))
	  (setq product-id-ref (CFDictionaryGetValue info (CFSTR kDisplayProductID)))
	  
	  (if (or (cffi:null-pointer-p vendor-id-ref)
		  (cffi:null-pointer-p product-id-ref))
		       
	      (CFRelease info)
	      
	      (cffi:with-foreign-objects ((p-vendor-id :unsigned-int)
					  (p-product-id :unsigned-int))
	        (CFNumberGetValue vendor-id-ref kCFNumberIntType p-vendor-id)
		(CFNumberGetValue product-id-ref kCFNumberIntType p-product-id)
	      
		(when (and (= (CGDisplayVendorNumber display-id) (cffi:mem-aref p-vendor-id :unsigned-int))
			   (= (CGDisplayModelNumber display-id) (cffi:mem-aref p-product-id :unsigned-int)))
		  (return))

		(CFRelease info))))

     (IOObjectRelease (cffi:mem-aref it :unsigned-int)))

    (when (= 0 service)
      (return-from get-cocoa-display-name "Display"))

    (let ((names (CFDictionaryGetValue info (CFSTR kDisplayProductName))))
      (cffi:with-foreign-object (name-ref :pointer)
        (when (or (cffi:null-pointer-p names) (CFDictionaryGetValueIfPresent names (CFSTR "en_US") name-ref))
	  (CFRelease info)
	  (return-from get-cocoa-display-name "Display"))

	(let ((size (CFStringGetMaximumSizeForEncoding (CFStringGetLength (cffi:mem-aref name-ref :pointer))
						       kCFStringEncodingUTF8)))
	  (cffi:with-foreign-object (name :char (1+ size))
           (CFStringGetCString name-ref name size kCFStringEncodingUTF8)
					    
	   (CFRelease info)
	   (cffi:foreign-string-to-lisp name)))))))

(defun cocoa-display-mode-is-good? (mode)
  (let ((flags (CGDisplayModeGetIOFlags mode)))
    (when (or (not (logtest flags kDisplayModeValidFlag))
	      (not (logtest flags kDisplayModeSafeFlag)))
      (return-from cocoa-display-mode-is-good? nil))
    (when (or (logtest flags kDisplayModeInterlacedFlag)
	      (logtest flags kDisplayModeStretchedFlag))
      (return-from cocoa-display-mode-is-good? nil))
    t))

(defun vidmode-from-cocoa-display-mode (mode &optional (fallback-refresh-rate 0.0d0))
  (let ((width (CGDisplayModeGetPixelWidth mode))
	(height (CGDisplayModeGetPixelHeight mode))
	(refresh-rate (CGDisplayModeGetRefreshRate mode)))
    
    (when (= 0 (round refresh-rate))
      (setq refresh-rate fallback-refresh-rate))
    
    (let ((red-bits 8)
	  (green-bits 8)
	  (blue-bits 8))
      
      (make-video-mode :width width :height height :red-bits red-bits
		       :green-bits green-bits :blue-bits blue-bits
		       :refresh-rate refresh-rate))))


(defun cocoa-begin-fade-reservation ()
  (cffi:with-foreign-object (p-token :unsigned-int)
    (setf (cffi:mem-aref p-token :unsigned-int) kCGDisplayFadeReservationInvalidToken)
    (when (= kCGErrorSuccess (CGAcquireDisplayFadeReservation 5.0f0 p-token))
      (CGDisplayFade (cffi:mem-aref p-token :unsigned-int)
		     0.3
		     kCGDisplayBlendNormal
		     kCGDisplayBlendSolidColor
		     0.0f0 0.0f0 0.0f0
		     t))
    (cffi:mem-aref p-token :unsigned-int)))

(defun cocoa-end-fade-reservation (token)
  (unless (= token kCGDisplayFadeReservationInvalidToken)
    (unwind-protect
	 (CGDisplayFade token
			0.5
			kCGDisplayBlendSolidColor
			kCGDisplayBlendNormal
			0.0f0 0.0f0 0.0f0
			nil)
      (CGReleaseDisplayFadeReservation token))))

(defun get-cocoa-display-fallback-refresh-rate (display-id)
  (let ((refresh-rate 60.0f0)
	(service 0))
      
    (cffi:with-foreign-object (it :unsigned-int)
      (unwind-protect
	   (progn
	     (unless (= 0 (IOServiceGetMatchingServices MACH_PORT_NULL (IOServiceMatching "IOFramebuffer") it))
	       (return-from get-cocoa-display-fallback-refresh-rate refresh-rate))

	     (loop while (not (= 0 (setq service (IOIteratorNext (cffi:mem-aref it :unsigned-int)))))
		do (let ((index-ref (IORegistryEntryCreateCFProperty service (CFSTR "IOFramebufferOpenGLIndex")
								     kCFAllocatorDefault kNilOptions)))

		     (unless (= 0 index-ref)
		       (cffi:with-foreign-object (p-index :unsigned-int)

			 (CFNumberGetValue index-ref kCFNumberIntType p-index)
			 (CFRelease (int-sap index-ref))

			 (when (= (CGOpenGLDisplayMaskToDisplayID (ash 1 (cffi:mem-aref p-index :unsigned-int))) display-id)
			   (let ((clock-ref)
				 (count-ref))

			     (setq clock-ref (IORegistryEntryCreateCFProperty service
									      (CFSTR "IOFBCurrentPixelClock")
									      kCFAllocatorDefault kNilOptions))
			     (setq count-ref (IORegistryEntryCreateCFProperty service
									      (CFSTR "IOFBCurrentPixelCount")
									      kCFAllocatorDefault kNilOptions))

			     (cffi:with-foreign-objects ((p-clock :unsigned-int)
							 (p-count :unsigned-int))
			  
			       (unless (= 0 clock-ref)
				 (CFNumberGetValue clock-ref kCFNumberIntType p-clock)
				 (CFRelease (int-sap clock-ref)))

			       (unless (= 0 count-ref)
				 (CFNumberGetValue clock-ref kCFNumberIntType p-count)
				 (CFRelease (int-sap count-ref)))

			       (let ((clock (cffi:mem-aref p-clock :unsigned-int))
				     (count (cffi:mem-aref p-count :unsigned-int)))
				 (when (and (plusp clock) (plusp count))
				   (setq refresh-rate (float (/ clock count))))
				 (return-from get-cocoa-display-fallback-refresh-rate refresh-rate))))))))
		finally (return-from get-cocoa-display-fallback-refresh-rate refresh-rate)))
	(IOObjectRelease (cffi:mem-aref it :unsigned-int))))))

(defun poll-cocoa-monitors (display)
  (cffi:with-foreign-object (p-display-count :unsigned-int)
   (CGGetOnlineDisplayList 0 (cffi:null-pointer) p-display-count)
    (let* ((display-count (cffi:mem-aref p-display-count :unsigned-int)))
      (cffi:with-foreign-object (p-displays :unsigned-int display-count)
        (CGGetOnlineDisplayList display-count p-displays p-display-count)

	(mapcar #'(lambda (monitor)
		    (setf (monitor-screen monitor) nil))
		(display-monitors display))
	
	(let ((disconnected (copy-list (display-monitors display))))
	  
	  (loop for i from 0 below display-count
	     with display-id
	     with unit-number
	     with screen
	     with found?
	     do (setq display-id (cffi:mem-aref p-displays :unsigned-int i))
	     unless (CGDisplayIsAsleep display-id)
	     do (setq unit-number (CGDisplayUnitNumber display-id))
	       (setq screen nil)

	       (block nil
		 (objc-runtime.data-extractors::map-nsarray 
		  (lambda (scr)
		    (let* ((description (ns::|deviceDescription| scr)) ;; an NSDictionary
			   (screen-number-object
			    (ns::|objectForKey:| description (objc-runtime::make-nsstring "NSScreenNumber")))
			   (screen-number (ns::|unsignedIntValue| screen-number-object)))
		      (when (= (CGDisplayUnitNumber screen-number) unit-number)
			(setq screen scr)
			(return))))
		  (ns::|screens| #@NSScreen)))
	       
	       (let ((m (find unit-number disconnected :key #'monitor-unit-number)))
		 (when m
		   (setf (monitor-screen m) screen)
		   (setq disconnected (remove m disconnected))
		   (setq found? t)
		   (return)))
	       
	     unless found?
	     do (let ((size (CGDisplayScreenSize display-id))
		      (name (get-cocoa-display-name display-id screen))
		      (monitor)
		      (mode))
		  (when name
		    (setq monitor (make-instance 'monitor
						 :display display
						 :name name
						 :width-mm (getf size 'ns::width)
						 :height-mm (getf size 'ns::height)
						 :display-id display-id
						 :unit-number unit-number
						 :screen screen))
		    
		    (setq mode (CGDisplayCopyDisplayMode display-id))
		    
		    (when (zerop (CGDisplayModeGetRefreshRate mode))
		      (setf (monitor-fallback-refresh-rate monitor) (get-cocoa-display-fallback-refresh-rate display-id)))
		    
		    (CGDisplayModeRelease mode)
		    
		    (input-monitor display monitor :action :connected :placement :insert-last))))

	  (loop for discon in disconnected
	     do (input-monitor display discon :action :disconnected)))))))

(defun set-cocoa-monitor-video-mode (monitor desired-video-mode)
  (let ((current (get-monitor-video-mode monitor))
	(best (choose-video-mode monitor desired-video-mode)))
    
    (unless (compare-video-modes current best)
      (return-from set-cocoa-monitor-video-mode (values)))

    (let* ((modes (CGDisplayCopyAllDisplayModes (monitor-display-id monitor) (cffi:null-pointer))))
      (unwind-protect
	   (let ((count (CFArrayGetCount modes))
		 (native (cffi:null-pointer)))

	     (loop for i from 0 below count
		with dm
		with mode
		do (setq dm (CFArrayGetValueAtIndex modes i))
		when (cocoa-display-mode-is-good? dm)
		do (setq mode (vidmode-from-cocoa-display-mode dm (monitor-fallback-refresh-rate monitor)))
		when (compare-video-modes best mode)
		do (setq native dm)
		  (return))
      
	     (unless (cffi:null-pointer-p native)
	
	       (unless (monitor-previous-video-mode monitor)
		 (setf (monitor-previous-video-mode monitor)
		       (CGDisplayCopyDisplayMode (monitor-display-id monitor))))
	
	       (let ((token (cocoa-begin-fade-reservation)))
		 (CGDisplaySetDisplayMode (monitor-display-id monitor) native (cffi:null-pointer))
		 (cocoa-end-fade-reservation token)
		 (values))))
	
	(CFRelease modes)))))

(defun get-cocoa-monitor-video-modes (monitor)
  (with-autorelease-pool (pool)
    (let* ((modes (CGDisplayCopyAllDisplayModes (monitor-display-id monitor) (cffi:null-pointer)))
	   (found (CFArrayGetCount modes)))

      (unwind-protect
	   (loop for i from 0 below found
	      with result = ()
	      with dm
	      with mode
	      do (setq dm (CFArrayGetValueAtIndex modes i))
	      when (cocoa-display-mode-is-good? dm)
	      do (setq mode (vidmode-from-cocoa-display-mode dm (monitor-fallback-refresh-rate monitor)))

		(pushnew mode result :test #'(lambda (new existing)
					       (zerop (compare-video-modes new existing))))
	   
	      finally (return (nreverse result)))

	(CFRelease modes)))))

(defun get-cocoa-monitor-video-mode (monitor)
  (let ((native (CGDisplayCopyDisplayMode (monitor-display-id monitor))))
    (unwind-protect
	 (vidmode-from-cocoa-display-mode native (monitor-fallback-refresh-rate monitor))
      (CGDisplayModeRelease native))))

(defun restore-cocoa-monitor-video-mode (monitor)
  (let ((previous (monitor-previous-video-mode monitor)))
    (when previous
      (let ((token (cocoa-begin-fade-reservation)))
	(CGDisplaySetDisplayMode (monitor-display-id monitor) previous (cffi:null-pointer))
	(cocoa-end-fade-reservation token)
	(CGDisplayModeRelease previous)
	(setf (monitor-previous-video-mode monitor) nil)))
    (values)))

(defun get-cocoa-monitor-gamma-ramp (monitor)
  (with-autorelease-pool (pool)
    (let ((size (CGDisplayGammaTableCapacity (monitor-display-id monitor)))
	  (ramp (make-gamma-ramp)))
      (flet ((adjust-gamma-arrays ()
	       (adjust-array (gamma-ramp-red ramp) size)
	       (setf (fill-pointer (gamma-ramp-red ramp)) size)
	       (adjust-array (gamma-ramp-green ramp) size)
	       (setf (fill-pointer (gamma-ramp-green ramp)) size)
	       (adjust-array (gamma-ramp-blue ramp) size)
	       (setf (fill-pointer (gamma-ramp-blue ramp)) size)))
	       
      (cffi:with-foreign-objects ((p-size :unsigned-int)
				  (p-values :float (* size 3)))
	(setf (cffi:mem-aref p-size :unsigned-int) size)
	(CGGetDisplayTransferByTable (monitor-display-id monitor)
				     size
				     p-values
				     (cffi:inc-pointer p-values size)
				     (cffi:inc-pointer p-values (* 2 size))
				     p-size)
	
	(adjust-gamma-arrays)

	(loop for i from 0 below size
	   do (setf (aref (gamma-ramp-red ramp) i) (truncate (* 65536 (cffi:mem-aref p-values :float i)))
		    (aref (gamma-ramp-green ramp) i) (truncate
						      (* 65535 (cffi:mem-aref (cffi:inc-pointer p-values size) :float i)))
		    (aref (gamma-ramp-blue ramp) i) (truncate
						     (* 65535 (cffi:mem-aref (cffi:inc-pointer p-values (* size)) :float i)))))
	t)))))



(defun set-cocoa-monitor-gamma-ramp (monitor ramp)
  (with-autorelease-pool (pool)
    (let ((size (fill-pointer (gamma-ramp-red ramp))))
      (cffi:with-foreign-object (p-values :float (* size 3))
       (loop for i from 0 below size
	  do (setf (cffi:mem-aref p-values :float i) (/ (aref (gamma-ramp-red ramp) i) 65535.0f0)
		   (cffi:mem-aref (cffi:inc-pointer p-values size) :float i) (/ (aref (gamma-ramp-green ramp) i) 65535.0f0)
		   (cffi:mem-aref (cffi:inc-pointer p-values (* size 2)) :float i) (/ (aref (gamma-ramp-blue ramp) i) 65535.0f0)))
       (CGSetDisplayTransferByTable (monitor-display-id monitor)
				    size
				    p-values
				    (cffi:inc-pointer p-values size)
				    (cffi:inc-pointer p-values (* size 2)))
       (values)))))
	 


(defun get-cocoa-monitor-pos (monitor)
  (let ((bounds (CGDisplayBounds (monitor-display-id monitor))))
    (values (getf bounds 'ns::x)
	    (getf bounds 'ns::y))))

(defun get-cocoa-monitor-content-scale (monitor)
  (unless (monitor-screen monitor)
    (error "monitor does not have a screen to query."))
  (with-autorelease-pool (pool)
    (let ((screen (monitor-screen monitor)))
      (let* ((points (ns:|frame| screen))
	     (pixels (ns:|convertRectToBacking:| screen points)))
	(values (/ (getf pixels 'ns::width) (getf points 'ns::width))
		(/ (getf pixels 'ns::height) (getf points 'ns::height)))))))

(defun get-cocoa-monitor-workarea (monitor)
  (let ((screen (monitor-screen monitor)))
    (unless screen
      (error "monitor does not have a screen to query."))
    (with-autorelease-pool (pool)
      (let ((frame-rect (ns:|visibleFrame| screen)))
	(values (getf frame-rect 'ns::x)
		(cocoa-transform-y (1- (+ (getf frame-rect 'ns::y) (getf frame-rect 'ns::height))))
		(getf frame-rect 'ns::width)
		(getf frame-rect 'ns::height))))))

    
(defun get-cocoa-monitor (monitor)
  (monitor-display-id monitor))
