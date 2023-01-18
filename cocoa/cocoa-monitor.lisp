(in-package :abstract-os)
(named-readtables:in-readtable :objc-readtable)

(defun get-cocoa-monitor-pos (monitor)
  (let ((bounds (CGDisplayBounds (monitor-display-id monitor))))
    (values (getf bounds 'ns::x)
	    (getf bounds 'ns::y))))

(defun get-cocoa-video-mode (monitor)
  (let ((native (CGDisplayCopyDisplayMode (monitor-display-id monitor))))
    (unwind-protect
	 (vidmode-from-cg-display-mode native
					  (monitor-fallback-refresh-rate monitor))
      (CGDisplayModeRelease native))))

(defun vidmode-from-cg-display-mode (mode &optional (fallback-refresh-rate 0.0d0))
  (let ((width (CGDisplayModeGetWidth mode))
	(height (CGDisplayModeGetHeight mode))
	(refresh-rate (round (CGDisplayModeGetRefreshRate mode))))
    (when (= 0 refresh-rate)
      (setq refresh-rate (round fallback-refresh-rate)))
    (let ((red-bits 8)
	  (green-bits 8)
	  (blue-bits 8))
      (make-video-mode :width width :height height :red-bits red-bits
		       :green-bits green-bits :blue-bits blue-bits))))

#+NOTYET
(defun get-fallback-refresh-rate (display-id)
  (let ((refresh-rate 60.0f0)
	(service 0))
      
    (cffi:with-foreign-object (it '(:struct io_iterator_t))
      (unwind-protect
	   (progn
	     (unless (= 0 (IOGetServiceMatchingServices MACH_PORT_NULL
							(IOServiceMatching "IOFramebuffer")
							it))
	       (return-from get-fallback-refresh-rate refresh-rate))

	     (loop while (not (= 0 (setq service (IOIteratorNext it))))
		do (let ((index-ref (IORegistryEntryCreateCFProperty service (CFSTR "IOFramebufferOpenGLIndex")
								     kCFAllocatorDefault kNilOptions)))

		     (unless (cffi:null-pointer-p index-ref)
		       (with-foreign-objects (p-index :unsigned-int)
			 (CFNumberGetValue index-ref kCFNumberIntType p-index)
			 (CFRelease index-ref)

			 (when (= (CGOpenGLDisplayMaskToDisplayID (ash 1 index)) display-id)
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
			  
			       (unless (null-pointer-p clock-ref)
				 (CFNumberGetValue clock-ref kCFNumberIntType clock p-clock)
				 (CFRelease clock-ref))

			       (unless (null-pointer-p count-ref)
				 (CFNumberGetValue clock-ref kCFNumberIntType clock p-count)
				 (CFRelease count-ref))

			       (let ((clock (cffi:mem-aref p-clock :unsigned-int))
				     (count (cffi:mem-aref p-count :unsigned-int)))
				 (when (and (plusp clock) (plusp count))
				   (setq refresh-rate (float (/ clock count))))
				 (return-from get-fallback-refresh-rate refresh-rate)))))))))))
      (IOObjectRelease it))))

(defun poll-monitors-cocoa (app)
  (cffi:with-foreign-object (p-display-count :unsigned-int)
   (CGGetOnlineDisplayList 0 (cffi:null-pointer) p-display-count)
    (let* ((display-count (cffi:mem-aref p-display-count :unsigned-int)))
      (cffi:with-foreign-object (p-displays :unsigned-int display-count)
        (CGGetOnlineDisplayList display-count p-displays p-display-count)

	(mapcar #'(lambda (monitor)
		    (setf (monitor-screen monitor) nil))
		(application-monitors app))
	
	(let ((disconnected (copy-list (application-monitors app))))
	  
	  (loop for i from 0 below display-count
	     with display
	     with unit-number
	     with screen
	     with found?
	     do (setq display (cffi:mem-aref p-displays :unsigned-int i))
	     unless (CGDisplayIsAsleep display)
	     do (setq unit-number (CGDisplayUnitNumber display))
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
	     do (let ((size (CGDisplayScreenSize display))
		      (name (get-monitor-name display screen))
		      (monitor)
		      (mode))
		  (when name
		    (setq monitor (make-instance 'monitor
						 :name name
						 :width (getf size 'ns::width)
						 :height (getf size 'ns::height)
						 :display-id display
						 :unit-number unit-number
						 :screen screen))
		    
		    (setq mode (CGDisplayCopyDisplayMode display))
		    
		    (when (zerop (CGDisplayModeGetRefreshRate mode))
		      (setf (monitor-fallback-refresh-rate monitor) (get-fallback-refresh-rate display)))
		    
		    (CGDisplayModeRelease mode)
		    
		    (input-monitor monitor :action :connected :placement :insert-last))))

	  (loop for discon in disconnected
	     do (input-monitor discon :action :disconnected)))))))
