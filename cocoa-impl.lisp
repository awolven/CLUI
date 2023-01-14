(in-package :abstract-os)
(named-readtables:in-readtable :objc-readtable)

(defvar *yes* 1)
(defvar *no* 0)

(defvar NSDefaultRunLoopMode (objc-runtime::make-nsstring "NSDefaultRunLoopMode"))

(cffi:defcallback exception-handler :void ((exception :pointer))
  (objc-runtime::with-selectors (reason)
    (error "~&objc exception: ~a~%" (objc-runtime::extract-nsstring [exception reason]))))

(defvar *trace-callbacks* t)

(defmacro deftraceable-callback (name return-type (&rest args) &body body)
  `(cffi:defcallback ,name ,return-type (,@args)
     (declare (ignorable ,@(mapcar #'car args)))
     (when *trace-callbacks*
       (format t "~%~A" ',name)
       (finish-output))
     (locally 
	 ,@body)))

(defun sap-int (sap)
  #+sbcl(sb-sys:sap-int sap)
  #+ccl(ccl::%ptr-to-int sap))

(defmacro new-msg-send (selector ((&rest arg-types) return-type))
  (let ((arg-syms (mapcar (lambda (_) _ (gensym))
                          arg-types)))
    `(lambda ,(cons 'target arg-syms)
       (cffi:foreign-funcall "objc_msgSend"
                             :pointer target
                             :pointer ,selector
                             ,@(mapcan #'list arg-types arg-syms)
                             ,return-type))))


(defmacro new-msg-send-stret (selector ((&rest arg-types) return-type))
  (let ((arg-syms (mapcar (lambda (_) _ (gensym))
                          arg-types)))
    `(lambda ,(cons 'target arg-syms)
       (cffi:foreign-funcall "objc_msgSend_stret"
			     :pointer target
			     :pointer ,selector
			     ,@(mapcan #'list arg-types arg-syms)
			     ,return-type))))

(cffi:defcfun (set-uncaught-exception-handler "objc_setUncaughtExceptionHandler")
    :void
  (cb :pointer))

(set-uncaught-exception-handler (cffi:callback exception-handler))



(cffi:defcfun (class_getSuperclass "class_getSuperclass") :pointer (cls :pointer))



(cffi:defcfun (ivar_getName "ivar_getName") :string (ivar :pointer))

(cffi:defcfun (object_setInstanceVariable "object_setInstanceVariable") :pointer (object :pointer) (name :string) (value :pointer))

;;(cffi:defcfun (protocol_getProperty "object_getProperty"))

(cffi:defcfun (object_getClass "object_getClass") :pointer (object :pointer))

(cffi:defcfun (method_getName "method_getName") :pointer (method :pointer))






(cffi:defcfun (objc_msgSendSuper "objc_msgSendSuper") :pointer (obj :pointer) (selector :pointer) &rest)

(cffi:defcstruct objc_super
  (reciever :pointer)
  (super_class :pointer))

(defun super-msg-send (thing selector &rest args)
  (cffi:with-foreign-object (objc-super '(:struct objc_super))
    (setf (cffi:foreign-slot-value objc-super '(:struct objc_super) 'reciever) (ns-object-ptr thing)
	  (cffi:foreign-slot-value objc-super '(:struct objc_super) 'super_class) (class_getSuperclass (object_getClass (ns-object-ptr thing))))
    (eval `(objc_msgSendSuper ,objc-super ,selector ,@args))))

(defmacro new-msg-send-super (selector ((&rest arg-types) return-type))
  (let ((arg-syms (mapcar (lambda (_) _ (gensym))
                          arg-types)))
    `(lambda ,(cons 'target arg-syms)
       (cffi:with-foreign-object (objc-super '(:struct objc_super))
	 (setf (cffi:foreign-slot-value objc-super '(:struct objc_super) 'reciever) (ns-object-ptr target)
	       (cffi:foreign-slot-value objc-super '(:struct objc_super) 'super_class) (class_getSuperclass (object_getClass (ns-object-ptr target))))
	 (cffi:foreign-funcall "objc_msgSendSuper"
                             :pointer objc-super
                             :pointer ,selector
                             ,@(mapcan #'list arg-types arg-syms)
                             ,return-type)))))



(cffi:defcfun ("CFBundleGetMainBundle" CFBundleGetMainBundle) :pointer)
(cffi:defcfun ("CFBundleCopyResourcesDirectoryURL" CFBundleCopyResourcesDirectoryURL) :pointer (bundle :pointer))
(cffi:defcfun ("CFURLCopyLastPathComponent" CFURLCopyLastPathComponent) :pointer (url :pointer))
(cffi:defcfun ("CFStringCompare" CFStringCompare) :int64 (theString1 :pointer) (theString2 :pointer) (compare-options :int))
(cffi:defcfun ("CFURLGetFileSystemRepresentation" CFURLGetFileSystemRepresentation) :boolean
  (url :pointer) (resolve-against-base :boolean) (buffer :pointer) (max-buffer-length :int64))
(cffi:defcfun ("CFRelease" CFRelease) :void (pointer :pointer))
(cffi:defcfun ("CFStringCreateWithCharacters" CFStringCreateWithCharacters) :pointer (alloc :pointer) (chars :string) (num-chars :int64))

(cffi:defcfun ("_NSGetProgname" _NSGetProgname) :string)

(defconstant kCFCompareLessThan -1)
(defconstant kCFCompareEqualTo 0)
(defconstant kCFCompareGreaterThan 1)
(defconstant MAXPATHLEN 1024)

(defconstant NSEventModifierFlagControl (ash 1 18))
(defconstant NSEventModifierFlagOption (ash 1 19))
(defconstant NSEventModifierFlagCommand (ash 1 20))
(defconstant NSEventTypeApplicationDefined 15)


(defun change-to-resources-directory ()
  (let ((bundle (CFBundleGetMainBundle)))
    (when (cffi:null-pointer-p bundle)
      (return-from change-to-resources-directory nil))
    (let* ((resources-url (CFBundleCopyResourcesDirectoryURL bundle))
	   (last (CFURLCopyLastPathComponent resources-url)))

      (cffi:with-foreign-string (p-string "RESOURCES")
	(let ((cf-string (CFStringCreateWithCharacters (cffi:null-pointer) p-string 9)))
	  (unless (= kCFCompareEqualTo (CFStringCompare cf-string last 0))
	    (CFRelease last)
	    (CFRelease resources-url)
	    (return-from change-to-resources-directory t))))
      
      (CFRelease last)

      (cffi:with-foreign-object (p-resources-path :char MAXPATHLEN)
	(unless (CFURLGetFileSystemRepresentation resources-url t p-resources-path MAXPATHLEN)
	  (CFRelease resources-url)
	  (return-from change-to-resources-directory nil))
	
	(CFRelease resources-url)

	(sb-posix:chdir (cffi:foreign-string-to-lisp p-resources-path))
	t))))

(defun create-menu-bar (app)
  (let ((app-name (application-name app)))

    (unless app-name
      (let ((progname "" #+NIL(_NSGetProgname)))
	(if (not (string= progname ""))
	    (setq app-name progname)
	    (setq app-name "Abstract OS Application"))))

    (let ((bar [[#@NSMenu @(alloc)] @(init)]))
      [(ns-object-ptr app) @(setMainMenu:) :pointer bar]

      (let ((app-menu-item [bar @(addItemWithTitle:action:keyEquivalent:) :pointer [#@NSString @(string)]
			   :pointer (cffi:null-pointer) :pointer [#@NSString @(string)]])
	    (app-menu [[#@NSMenu @(alloc)] @(init)]))

	[app-menu-item @(setSubmenu:) :pointer app-menu]
	[app-menu @(addItemWithTitle:action:keyEquivalent:)
	 :pointer (objc-runtime::make-nsstring (concatenate 'string "About " app-name))
	 :pointer @(orderFrontStandardAboutPanel:) :pointer (objc-runtime::make-nsstring "")]
	 [app-menu @(addItem:) :pointer [#@NSMenuItem @(separatorItem)]]

	 (let ((services-menu [[#@NSMenu @(alloc)] @(init)]))
	  [(ns-object-ptr app) @(setServicesMenu:) :pointer services-menu]
	  [app-menu @(addItemWithTitle:action:keyEquivalent:) :pointer (objc-runtime::make-nsstring "Services") :pointer (cffi:null-pointer)
	  :pointer (objc-runtime::make-nsstring "")]

	  [services-menu @(release)]

	  [app-menu @(addItem:) :pointer [#@NSMenuItem @(separatorItem)]]
	  [app-menu @(addItemWithTitle:action:keyEquivalent:)
	  :pointer (objc-runtime::make-nsstring (concatenate 'string "Hide " app-name))
	  :pointer @(hide:) :pointer (objc-runtime::make-nsstring "h")]

	  [[app-menu @(addItemWithTitle:action:keyEquivalent:)
	   :pointer (objc-runtime::make-nsstring "Hide Others")
	   :pointer @(hideOtherApplications:) :pointer (objc-runtime::make-nsstring "h")]
	   @(setKeyEquivalentModifierMask:) :long-long (logior NSEventModifierFlagOption NSEventModifierFlagCommand)]

	   [app-menu @(addItemWithTitle:action:keyEquivalent:)
	  :pointer (objc-runtime::make-nsstring "Show All")
	  :pointer @(unhideAllApplications:) :pointer (objc-runtime::make-nsstring "")]
	  [app-menu @(addItem:) :pointer [#@NSMenuItem @(separatorItem)]]
	  [app-menu @(addItemWithTitle:action:keyEquivalent:)
	  :pointer (objc-runtime::make-nsstring (concatenate 'string "Quit " app-name))
	  :pointer @(terminate:) :pointer (objc-runtime::make-nsstring "q")]

	  (let ((window-menu-item [bar @(addItemWithTitle:action:keyEquivalent:) :pointer (objc-runtime::make-nsstring "")
				  :pointer (cffi:null-pointer) :pointer (objc-runtime::make-nsstring "")])
		(window-menu [[#@NSMenu @(alloc)] @(initWithTitle:) :pointer (objc-runtime::make-nsstring "Window")]))
	    [(ns-object-ptr app) @(setWindowsMenu:) :pointer window-menu]
	    [window-menu-item @(setSubmenu:) :pointer window-menu]
	    [window-menu @(addItemWithTitle:action:keyEquivalent:)
	    :pointer (objc-runtime::make-nsstring "Minimize")
	    :pointer @(performMiniaturize:) :pointer (objc-runtime::make-nsstring "m")]
	    [window-menu @(addItemWithTitle:action:keyEquivalent:)
	    :pointer (objc-runtime::make-nsstring "Zoom")
	    :pointer @(performZoom:) :pointer (objc-runtime::make-nsstring "")]
	    [app-menu @(addItem:) :pointer [#@NSMenuItem @(separatorItem)]]
	    [window-menu @(addItemWithTitle:action:keyEquivalent:)
	    :pointer (objc-runtime::make-nsstring "Bring All To Front")
	    :pointer @(arrangeInFront:) :pointer (objc-runtime::make-nsstring "")]

	    [app-menu @(addItem:) :pointer [#@NSMenuItem @(separatorItem)]]
	    [[window-menu @(addItemWithTitle:action:keyEquivalent:)
	     :pointer (objc-runtime::make-nsstring "Enter Full Screen")
	     :pointer @(toggleFullScreen:) :pointer (objc-runtime::make-nsstring "f")]
	     @(setKeyEquivalentModifierMask:) :int (logior NSEventModifierFlagControl NSEventModifierFlagCommand)]

	     [(ns-object-ptr app) @(performSelector:withObject:) :pointer @(setAppleMenu:) :pointer app-menu]
	     [bar @(release)]
	     (values)))))))

(cffi:defcfun (TISCopyCurrentKeyboardLayoutInputSource "TISCopyCurrentKeyboardLayoutInputSource") :pointer)
(cffi:defcfun (TISGetInputSourceProperty "TISGetInputSourceProperty") :pointer (source :pointer) (input-source-id :pointer))
(cffi:defcfun (CFBundleGetBundleWithIdentifier "CFBundleGetBundleWithIdentifier") :pointer (identifier :pointer))
(cffi:defcfun (CFBundleGetDataPointerForName "CFBundleGetDataPointerForName") :pointer (bundle :pointer) (name :pointer))

(defun modifier-flags (event)
  (let ((selector (new-msg-send @(modifierFlags) (() :int64))))
    (funcall selector (ns-object-ptr event))))

(deftraceable-callback closure-like-thingy-named-block-callback :pointer ((event :pointer))
  (closure-like-thingy-named-block event)
  event)

(defun closure-like-thingy-named-block (event)
  (unless (zerop (logand (modifier-flags event) NSEventModifierFlagCommand))
    [[(ns-object-ptr *app*) @(keyWindow)] @(sendEvent:) :pointer event])
  event)
  

(defmacro make-dictionary (&rest objc-values)
  (alexandria:with-gensyms (selector)
    `(let ((,selector (new-msg-send @(dictionaryWithObjectsAndKeys:)
                          ((,@(mapcar (lambda (_) _ :pointer) objc-values) :pointer)
                           :pointer))))
       (funcall ,selector #@NSDictionary ,@objc-values (cffi:null-pointer)))))

(defconstant NSEventTypeKeyUp 11)
(defconstant NSEventMaskKeyUp (ash 1 NSEventTypeKeyUp))

(cffi:defcfun (CGEventSourceCreate "CGEventSourceCreate") :pointer (state-id :uint32))
(cffi:defcfun (CGEventSourceSetLocalEventsSuppressionInterval "CGEventSourceSetLocalEventsSuppressionInterval") :void
  (source :pointer) (seconds :double))

(defconstant kCGEventSourceStatePrivate -1)
(defconstant kCGEventSourceStateCombinedSessionState 0)
(defconstant kCGEventSourceStateHIDSystemState 1)

(defconstant NSApplicationActivationPolicyRegular 0)

(defun init-cocoa (app)
  (flet ((pre-init-app ()
	   (setf (helper-class app) (make-helper-class))
	   (setf (application-delegate-class app) (make-application-delegate-class))
	   (setf (window-class app) (make-window-class))
	   (setf (window-delegate-class app) (make-window-delegate-class))
	   (setf (content-view-class app) (make-content-view-class))

	   (setf (delegate->clos-window-table app) (make-hash-table :test #'eq))
	   (setf (content-view->clos-content-view-table app) (make-hash-table :test #'eq))
  
	   (setf (application-helper app) [[(helper-class app) @(alloc)] @(init)])

	   (when (cffi:null-pointer-p (application-helper app))
	     (error "Cocoa: failed to create application helper."))
  
	   (setf (application-delegate app) [[(application-delegate-class app) @(alloc)] @(init)])

	   (when (cffi:null-pointer-p (application-delegate app))
	     (error "Cocoa: failed to create application delegate."))))

    (pre-init-app)

    (send #@NSThread @(detachNewThreadSelector:toTarget:withObject:) :void :pointer @(doNothing:)
     :pointer (ns-object-ptr (application-helper app)) :pointer (cffi:null-pointer))
     
     (send #@NSApplication @(sharedApplication) :pointer)

     (setf (ns-object-ptr app) objc-runtime::ns-app)
  
     (send app @(setDelegate:) :void :pointer (application-delegate app))

     (setf (key-up-monitor app)
	   (send #@NSEvent @(addLocalMonitorForEventsMatchingMask:handler:) :pointer
	    :long-long NSEventMaskKeyUp :int 0 :pointer (cffi:callback closure-like-thingy-named-block-callback)))

     (change-to-resources-directory)

     (let ((defaults (make-dictionary (cffi::null-pointer)
				      (objc-runtime::make-nsstring "ApplePressAndHoldEnabled")))
	   (NSTextInputContextKeyboardSelectionDidChangeNotification
	    (objc-runtime::make-nsstring
	     "NSTextInputContextKeyboardSelectionDidChangeNotification")))
    
       (send
	(send #@NSUserDefaults @(standardUserDefaults) :pointer) @(registerDefaults:) :void :pointer defaults)

       (send
	(send #@NSNotificationCenter @(defaultCenter) :pointer) @(addObserver:selector:name:object:)
	:void
        :pointer (ns-object-ptr (application-helper app))
	:pointer @(selectedKeyboardInputSourceChanged:)
	:pointer NSTextInputContextKeyboardSelectionDidChangeNotification
	:pointer (cffi:null-pointer))

	;;(create-key-tables app)

	(setf (application-event-source app) (CGEventSourceCreate 0))

	(when (cffi:null-pointer-p (application-event-source app))
	  (return-from init-cocoa nil))

	(CGEventSourceSetLocalEventsSuppressionInterval (application-event-source app) 0.0d0)

	#+NOTYET
	(unless (initialize-tis app)
	  (return-from init-cocoa nil))

	;;(poll-monitors-cocoa app)

	(when (cffi:null-pointer-p
	       (send
		(send #@NSRunningApplication @(currentApplication) :pointer) @(isFinishedLaunching) :pointer))
	  (send app @(run) :void))

	(send app @(setActivationPolicy:) :void :long-long NSApplicationActivationPolicyRegular)

	t)))

#+NOTYET
(defun poll-monitors-cocoa (app)
  (cffi:with-foreign-object (p-display-count :uint32)
    (CGGetOnlineDisplayLists 0 (cffi:null-pointer) p-display-count)
    (let ((display-count (cffi:mem-aref p-display-count :uint32)))
      (cffi:with-foreign-object (p-displays :unsigned-int display-count)
	(CGGetOnlineDisplayList display-count p-displays p-display-count)

	(loop for i from 0 below (application-monitor-count app)
	   do (setf (monitor-screen (elt (application-monitors app) i) nil)))

	(let ((disconnected-count (application-monitor-count app))
backtra	      (disconnected))

	  (loop for i from 0 below display-count
	     unless (CGDisplayIsAsleep (cffi:mem-aref p-displays :unsigned-int i))
	     do (let ((unit-number (CGDisplayUnitNumber (cffi:mem-aref p-displays :unsigned-int i)))
		      (screen))

		  (block nil
		    (objc-runtime.data-extractors::map-nsarray 
		     (lambda (scr)
		       (let ((screen-number (getf (objc-runtime.data-extractors::get-plist [scr @(deviceDescription)]) :ns-screen-number)))
			 (when (eq (CGDisplayUnitNumber [screen-number @(unsignedIntValue)]) unit-number)
			   (setq screen scr)
			   (return))))
		     [#@NSScreen @(screens)]))

		  

		  )))))))

		  
		  
      
	

  

   
  
			     
  

(defun update-unicode-data (app kPropertyUnicodeKeyLayoutData)
  (when (tis-input-source app)
    (CFRelease (tis-input-source app))
    (setf (tis-input-source app) nil)
    (setf (application-unicode-data app) nil))

  (let ((source (TISCopyCurrentKeyboardLayoutInputSource)))
  
    (when (cffi:null-pointer-p source)
      (error "Cocoa: Failed to retrieve keyboard layout input source."))
    
    (setf (tis-input-source app) source)

    (let ((data (TISGetInputSourceProperty source kPropertyUnicodeKeyLayoutData)))
      
      (when (cffi:null-pointer-p data)
	(error "Cocoa: Failed to retrieve keyboard layout unicode data."))
      
      (setf (application-unicode-data app) data)
      
      t)))

(defun CFSTR (string)
  (CFStringCreateWithCharacters (cffi:null-pointer) string (length string)))

(defun initialize-TIS (app)
  (let ((tis-bundle (CFBundleGetBundleWithIdentifier
		     (CFSTR "com.apple.HIToolbox"))))

    (when (cffi:null-pointer-p tis-bundle)
      (error "Cocoa: Failed to load HIToolbox.framework"))

    (setf (tis-bundle app) tis-bundle)

    (setf (slot-value app 'kPropertyUnicodeKeyLayoutData)
	  (CFBundleGetDataPointerForName
	   tis-bundle (CFSTR "kPropertyUnicodeKeyLayoutData")))

    (setf (slot-value app 'TISCopyCurrentKeyboardLayoutInputSource)
	  (CFBundleGetDataPointerForName
	   tis-bundle (CFSTR "TISCopyCurrentKeyboardLayoutInputSource")))

    (setf (slot-value app 'TISGetInputSourceProperty)
	  (CFBundleGetDataPointerForName
	   tis-bundle (CFSTR "TISGetInputSourceProperty")))

    (setf (slot-value app 'LMGetKbdType)
	  (CFBundleGetDataPointerForName
	   tis-bundle (CFSTR "LMGetKbdType")))

    (when (or (cffi:null-pointer-p (slot-value app 'kPropertyUnicodeKeyLayoutData))
	      (cffi:null-pointer-p (slot-value app 'TISCopyCurrentKeyboardLayoutInputSource))
	      (cffi:null-pointer-p (slot-value app 'TISGetInputSourceProperty))
	      (cffi:null-pointer-p (slot-value app 'LMGetKbdType)))
      (error "Cocoa: Failed to load TIS API symbols."))

    (update-unicode-data app (cffi:mem-aref (slot-value app 'kPropertyUnicodeKeyLayoutData) :pointer))))
	      
	  
      
  

	
      

    

(defconstant NSWindowStyleMaskMiniaturizable (ash 1 2))
(defconstant NSWindowStyleMaskBorderless 0)
(defconstant NSWindowStyleMaskTitled (ash 1 0))
(defconstant NSWindowStyleMaskClosable (ash 1 1))
(defconstant NSWindowStyleMaskResizable (ash 1 3))
(defconstant NSWindowStyleMaskFullScreen (ash 1 14))
(defconstant NSBackingStoreBuffered 2)
(defconstant NSMainMenuWindowLevel 24)
(defconstant NSWindowCollectionBehaviorFullScreenPrimary (ash 1 7))
(defconstant NSWindowCollectionBehaviorManaged (ash 1 2))
(defconstant NSWindowCollectionBehaviorFullScreenNone (ash 1 9))
(defconstant NSFloatingWindowLevel 3)
(defconstant NSTrackingMouseEnteredAndExited #x01)
(defconstant NSTrackingActiveInKeyWindow #x20)
(defconstant NSTrackingEnabledDuringMouseDrag #x400)
(defconstant NSTrackingCursorUpdate #x04)
(defconstant NSTrackingInVisibleRect #x200)
(defconstant NSTrackingAssumeInside #x100)
(defconstant NSWindowOcclusionStateVisible (ash 1 1))
(defconstant NSNormalWindowLevel 0)
(defconstant NSFloatingWindowLevel 3)
(defconstant NSMainMenuWindowLevel 24)
(defconstant NSModelPanelWindowLevel 8)
(defconstant NSPopUpMenuWindowLevel 101)
(defconstant NSScreenSaverWindowLevel 1000)
(defconstant NSStatusWindowLevel 25)
(defconstant NSTornOffMenuWindowLevel 3)
(defconstant NSSubmenuWindowLevel NSTornOffMenuWindowLevel)

(defconstant NSEventMaskAny #xffffffffffffffff)


(cffi:defcstruct NSPoint
  (x :double)
  (y :double))

(defun ns-point-x (ns-point)
  (cffi:mem-aref ns-point :double 0))

(defun ns-point-y (ns-point)
  (cffi:mem-aref ns-point :double 1))

(cffi:defcstruct NSSize
  (width :double)
  (height :double))

(cffi:defcstruct NSRect
  (x :double)
  (y :double)
  (width :double)
  (height :double))

(defun super-init-with-frame (self frame)
  (let ((selector (new-msg-send-super @(initWithFrame:)
				      (((:struct NSRect)) :pointer))))
    (funcall selector self frame)))


(cffi:defcfun (CGMainDisplayID "CGMainDisplayID") :unsigned-int)
(cffi:defcfun (CGDisplayBounds "CGDisplayBounds") (:struct NSRect) (display :unsigned-int))
(cffi:defcfun (CGDisplayCopyDisplayMode "CGDisplayCopyDisplayMode") :pointer (display :unsigned-int))
(cffi:defcfun (CGDisplayModeGetWidth "CGDisplayModeGetWidth") :int64 (mode :pointer))
(cffi:defcfun (CGDisplayModeGetHeight "CGDisplayModeGetHeight") :int64 (mode :pointer))
(cffi:defcfun (CGDisplayModeGetRefreshRate "CGDisplayModeGetRefreshRate") :double (mode :pointer))
(cffi:defcfun (CGDisplayModeRelease "CGDisplayModeRelease") :void (mode :pointer))

(defmacro with-nspoint ((var &key (x 0.0d0) (y 0.0d0)) &body body)
  (let ((x-sym (gensym))
	(y-sym (gensym)))
    `(let ((,x-sym ,x)
	   (,y-sym ,y))
       (cffi:with-foreign-object (,var '(:struct NSPoint))
	 (setf (cffi:mem-aref ,var :double 0) ,x-sym
	       (cffi:mem-aref ,var :double 1) ,y-sym)
	 ,@body))))

(defmacro with-nssize ((var &key (width 0.0d0) (height 0.0d0)) &body body)
  (let ((width-sym (gensym))
	(height-sym (gensym)))
    `(let ((,width-sym ,width)
	   (,height-sym ,height))
       (cffi:with-foreign-object (,var '(:struct NSSize))
	 (setf (cffi:mem-aref ,var :double 0) ,width-sym
	       (cffi:mem-aref ,var :double 1) ,height-sym)
	 ,@body))))

(defmacro with-nsrect ((var &key (x 0.0d0) (y 0.0d0) (width 0.0d0) (height 0.0d0)) &body body)
  (let ((x-sym (gensym))
	(y-sym (gensym))
	(width-sym (gensym))
	(height-sym (gensym)))
    `(let ((,x-sym ,x)
	   (,y-sym ,y)
	   (,width-sym ,width)
	   (,height-sym ,height))
       (cffi:with-foreign-object (,var '(:struct NSRect))
	 (setf (cffi:mem-aref ,var :double 0) ,x-sym
	       (cffi:mem-aref ,var :double 1) ,y-sym
	       (cffi:mem-aref ,var :double 2) ,width-sym
	       (cffi:mem-aref ,var :double 3) ,height-sym)
	 ,@body))))

(defun make-other-event-with-type (type point modifier-flags timestamp window-number context subtype
				   &optional (data1 0) (data2 0))
  (let ((selector (new-msg-send @(otherEventWithType:location:modifierFlags:timestamp:windowNumber:context:subtype:data1:data2:)
				((:int (:struct NSPoint) :int :int :int :pointer :int :int :int) :pointer))))
    (funcall selector #@NSEvent type point modifier-flags timestamp
	     window-number (if context
			       (ns-object-ptr context)
			       (cffi:null-pointer))
	     subtype data1 data2)))

(defun post-empty-event (app)
  (with-nspoint (point)
    (let ((event (make-other-event-with-type NSEventTypeApplicationDefined (list 'y 0.0d0 'x 0.0d0)
					     0 0 0 nil 0 0 0)))
      [(ns-object-ptr app) @(postEvent:atStart:) :pointer event :int 1])))
		 

(deftraceable-callback content-view-dealloc-callback :void ((self :pointer) (_cmd :pointer))
;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       (content-view->clos-content-view-table *app*))))
    (when content-view
      (content-view-dealloc content-view))
    (values)))

(defun content-view-dealloc (content-view)
  (declare (ignorable content-view))
  ;; todo
  )
  

(deftraceable-callback content-view-is-opaque-callback :char ((self :pointer) (_cmd :pointer))
;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       (content-view->clos-content-view-table *app*))))
    (when content-view
      (content-view-is-opaque content-view))))

(defun content-view-is-opaque (content-view) 
  (let ((window (content-view-owner content-view)))
    (when window
      (if (is-opaque? window)
	  *yes*
	  *no*))))

(deftraceable-callback content-view-can-become-key-view-callback :char ((self :pointer) (_cmd :pointer))
;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       (content-view->clos-content-view-table *app*))))
    (when content-view
      (content-view-can-become-key-view content-view))))

(defun content-view-can-become-key-view (content-view)
  (declare (ignore content-view))
  *yes*)

(deftraceable-callback content-view-accepts-first-responder-callback :char ((self :pointer) (_cmd :pointer))
;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       (content-view->clos-content-view-table *app*))))
    (when content-view
      (content-view-accepts-first-responder content-view))))

(defun content-view-accepts-first-responder (content-view)
  (declare (ignore content-view))
  *yes*)

(deftraceable-callback content-view-wants-update-layer-callback :char ((self :pointer) (_cmd :pointer))
;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       (content-view->clos-content-view-table *app*))))
    (when content-view
      (content-view-wants-update-layer content-view))
    #+IGNORE
    *no*))

(defun content-view-wants-update-layer (content-view)
  (declare (ignore content-view))
  *yes*)

(deftraceable-callback content-view-update-layer-callback :void ((self :pointer) (_cmd :pointer))
;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       (content-view->clos-content-view-table *app*))))
    (when content-view
      (content-view-update-layer content-view))
    (values)))

(defun content-view-update-layer (content-view)
  (content-view-draw-rect content-view nil)
  (input-window-damage (content-view-owner content-view))
  (values))

(deftraceable-callback content-view-cursor-update-callback :void ((self :pointer) (_cmd :pointer) (event :pointer))
;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       (content-view->clos-content-view-table *app*))))
    (when content-view
      (content-view-cursor-update content-view event))
    (values)))

(defun content-view-cursor-update (content-view event)
  (declare (ignore event))
  (update-cursor-image (content-view-owner content-view))
  (values))

(defun update-cursor-image (window)
  (declare (ignore window))
  (values))

(deftraceable-callback content-view-accepts-first-mouse-callback :char ((self :pointer) (_cmd :pointer))
;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       (content-view->clos-content-view-table *app*))))
    (when content-view
      (content-view-accepts-first-mouse content-view))))

(defun content-view-accepts-first-mouse (content-view)
  (declare (ignore content-view))
  *yes*)

(deftraceable-callback content-view-mouse-down-callback :void ((self :pointer) (_cmd :pointer) (event :pointer))
;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       (content-view->clos-content-view-table *app*))))
    (when content-view
      (content-view-mouse-down content-view event))
    (values)))

(defun content-view-mouse-down (content-view event)
  (input-mouse-clicked (content-view-owner content-view)
		     :mouse-button-left :press
		     (translate-flags [event @(modifierFlags)]))
  (values))

(defun translate-flags (event-modifier-flags)
  event-modifier-flags)

(deftraceable-callback content-view-mouse-dragged-callback :void ((self :pointer) (_cmd :pointer) (event :pointer))
;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       (content-view->clos-content-view-table *app*))))
    (when content-view
      (content-view-mouse-dragged content-view event))
    (values)))

(defun content-view-mouse-dragged (content-view event)
  [(ns-object-ptr content-view) @(mouseMoved) :pointer event]
  (values))

(deftraceable-callback content-view-mouse-up-callback :void ((self :pointer) (_cmd :pointer) (event :pointer))
;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       (content-view->clos-content-view-table *app*))))
    (when content-view
      (content-view-mouse-up content-view event))
    (values)))

(defun content-view-mouse-up (content-view event)
  (input-mouse-clicked (content-view-owner content-view)
		     :mouse-button-left :release
		     (translate-flags [event @(modifierFlags)]))
  (values))

(deftraceable-callback content-view-mouse-moved-callback :void ((self :pointer) (_cmd :pointer) (event :pointer))
;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       (content-view->clos-content-view-table *app*))))
    (when content-view
      (content-view-mouse-moved content-view event))
    (values)))



(defun content-view-mouse-moved (content-view event)
  (let ((window (content-view-owner content-view)))

    (if (eq (window-cursor-mode window) :disabled)
	(let ((dx (- (cffi:mem-aref [event @(deltaX)] :int) (cursor-warp-delta-x window)))
	      (dy (- (cffi:mem-aref [event @(deltaY)] :int) (cursor-warp-delta-y window))))

	  (input-cursor-pos window
			    (+ (virtual-cursor-pos-x window) dx)
			    (+ (virtual-cursor-pos-y window) dy)))
	(let ((content-rect [(ns-object-ptr content-view) @(frame)])
	      (pos [event @(locationInWindow)]))

	  (input-cursor-pos window
			    (ns-point-x pos)
			    (- (cffi:mem-aref [[content-rect @(size)] @(height)] :int) (ns-point-y pos)))))

    (setf (cursor-warp-delta-x window) 0
	  (cursor-warp-delta-y window) 0)
    (values)))

(deftraceable-callback content-view-right-mouse-down-callback :void ((self :pointer) (_cmd :pointer) (event :pointer))
;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       (content-view->clos-content-view-table *app*))))
    (when content-view
      (content-view-right-mouse-down content-view event))
    (values)))

(defun content-view-right-mouse-down (content-view event)
  (input-mouse-clicked (content-view-owner content-view)
		       :mouse-button-right
		       :press
		       (translate-flags [event @(modifierFlags)])))

(deftraceable-callback content-view-right-mouse-dragged-callback :void ((self :pointer) (_cmd :pointer) (event :pointer))
;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       (content-view->clos-content-view-table *app*))))
    (when content-view
      (content-view-right-mouse-dragged content-view event))
    (values)))

(defun content-view-right-mouse-dragged (content-view event)
  [(ns-object-ptr content-view) @(mouseMoved) :pointer event]
  (values))
  
(deftraceable-callback content-view-right-mouse-up-callback :void ((self :pointer) (_cmd :pointer) (event :pointer))
;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       (content-view->clos-content-view-table *app*))))
    (when content-view
      (content-view-right-mouse-up content-view event))
    (values)))

(defun content-view-right-mouse-up (content-view event)
  (input-mouse-clicked (content-view-owner content-view)
		       :mouse-button-right
		       :release
		       (translate-flags [event @(modifierFlags)])))

(deftraceable-callback content-view-other-mouse-down-callback :void ((self :pointer) (_cmd :pointer) (event :pointer))
;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       (content-view->clos-content-view-table *app*))))
    (when content-view
      (content-view-other-mouse-down content-view event))
    (values)))

(defun content-view-other-mouse-down (content-view event)
  (input-mouse-clicked (content-view-owner content-view)
		       (aref #(:mouse-button-right :mouse-button-left :mouse-button-middle)
			     (cffi:mem-aref [event @(buttonNumber)] :int))
		       :press
		       (translate-flags [event @(modifierFlags)])))

(deftraceable-callback content-view-other-mouse-dragged-callback :void ((self :pointer) (_cmd :pointer) (event :pointer))
;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       (content-view->clos-content-view-table *app*))))
    (when content-view
      (content-view-other-mouse-dragged content-view event))
    (values)))

(defun content-view-other-mouse-dragged (content-view event)
  [(ns-object-ptr content-view) @(mouseMoved) :pointer event]
  (values))
  
(deftraceable-callback content-view-other-mouse-up-callback :void ((self :pointer) (_cmd :pointer) (event :pointer))
;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       (content-view->clos-content-view-table *app*))))
    (when content-view
      (content-view-other-mouse-up content-view event))
    (values)))

(defun content-view-other-mouse-up (content-view event)
  (input-mouse-clicked (content-view-owner content-view)
		       (aref #(:mouse-button-right :mouse-button-left :mouse-button-middle)
			     (cffi:mem-aref [event @(buttonNumber)] :int))
		       :release
		       (translate-flags [event @(modifierFlags)])))

(deftraceable-callback content-view-other-mouse-exited-callback :void ((self :pointer) (_cmd :pointer) (event :pointer))
;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       (content-view->clos-content-view-table *app*))))
    (when content-view
      (content-view-other-mouse-exited content-view event))
    (values)))

(defun content-view-other-mouse-exited (content-view event)
  (declare (ignore event))
  (let ((window (content-view-owner content-view)))
    (when (eq (window-cursor-mode window) :hidden)
      (show-cursor window))
    (input-cursor-enter window nil)))

(deftraceable-callback content-view-other-mouse-entered-callback :void ((self :pointer) (_cmd :pointer) (event :pointer))
;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       (content-view->clos-content-view-table *app*))))
    (when content-view
      (content-view-other-mouse-entered content-view event))
    (values)))

(defun content-view-other-mouse-entered (content-view event)
  (declare (ignore event))
  (let ((window (content-view-owner content-view)))
    (when (eq (window-cursor-mode window) :hidden)
      (hide-cursor window))
    (input-cursor-enter window t)))

(deftraceable-callback content-view-view-did-change-backing-properties-callback :void ((self :pointer) (_cmd :pointer))
;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       (content-view->clos-content-view-table *app*))))
    (when content-view
      (content-view-view-did-change-backing-properties content-view))
    (values)))

(defun content-view-view-did-change-backing-properties (content-view)
  (declare (ignorable content-view))
  #+NOTYET
  (let* ((window (content-view-owner content-view))
	 (content-rect [(ns-object-ptr content-view) @(frame)])
	 (fb-rect [(ns-object-ptr content-view) @(convertRectToBacking:) :pointer content-rect])
	 (x-scale (/ (cffi:mem-aref [[fb-rect @(size)] @(width)] :int) (cffi:mem-aref [[content-rect @(size)] @(width)] :int)))
	 (y-scale (/ (cffi:mem-aref [[fb-rect @(size)] @(height)] :int) (cffi:mem-aref [[content-rect @(size)] @(height)] :int))))

    (when (or (/= x-scale (window-x-scale window)) (/= y-scale (window-y-scale window)))
      (when (and (window-retina? window) (window-layer window))
	[(ns-object-ptr (window-layer)) @(setContentsScale:) :integer [(ns-object-ptr window) @(backingScaleFactor)]])

      (setf (window-x-scale window) x-scale
	    (window-y-scale window) y-scale)
      (input-window-content-scale window x-scale y-scale))

    (when (or (/= (cffi:mem-aref [[fb-rect @(size)] @(width)] :int) (window-fb-width window))
	      (/= (cffi:mem-aref [[fb-rect @(size)] @(height)] :int) (window-fb-height window)))
      (input-framebuffer-size window
			      (cffi:mem-aref [[fb-rect @(size)] @(width)] :int)
			      (cffi:mem-aref [[fb-rect @(size)] @(height)] :int))))
    
  (values))

(deftraceable-callback content-view-draw-rect-callback :void ((self :pointer) (_cmd :pointer) (rect :pointer))
;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       (content-view->clos-content-view-table *app*))))
    (when content-view
      (content-view-draw-rect content-view rect))
    (values)))

(defun content-view-draw-rect (content-view rect)
  (declare (ignorable rect))
  (format t "~%running draw-rect")
  (finish-output)
  [[#@NSColor @(whiteColor)] @(set)]
  (bezier-path-stroke-rect (list 'height 1.0d0 'width 1.0d0 'x 0.0d0 'y 0.0d0))
  ;;(NSRectFill (get-bounds content-view))
  [(window-graphics-context (content-view-owner content-view)) @(flushGraphics)]
  (input-window-damaged (content-view-owner content-view)))

(deftraceable-callback content-view-update-tracking-areas-callback :void ((self :pointer) (_cmd :pointer))
;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       (content-view->clos-content-view-table *app*))))
    (when content-view
      (content-view-update-tracking-areas content-view))
    (values)))

(defun bezier-path-stroke-rect (rect)
  (let ((selector (new-msg-send @(strokeRect:)
				(((:struct NSRect)) :void))))
    (funcall selector #@NSBezierPath rect)))

(defun content-view-update-tracking-areas (content-view)
  (declare (ignorable content-view))
  #+NIL
  (let ((tracking-area (content-view-tracking-area content-view))
	(content-view-ptr (ns-object-ptr content-view)))
    
    (when tracking-area
      [content-view-ptr @(removeTrackingArea:) :pointer tracking-area]
      [tracking-area @(release)])

    (let ((options (logior NSTrackingMouseEnteredAndExited
			   NSTrackingActiveInKeyWindow
			   NSTrackingEnabledDuringMouseDrag
			   NSTrackingCursorUpdate
			   NSTrackingInVisibleRect
			   NSTrackingAssumeInside)))

      (setf (content-view-tracking-area content-view)
	    (init-with-rect [#@NSTrackingArea @(alloc)] (get-bounds content-view) options content-view
			    (cffi:null-pointer)))
      
      [content-view-ptr @(addTrackingArea:) :pointer (content-view-tracking-area content-view)]
      (super-msg-send content-view @(updateTrackingAreas))))
  (values))

(defun wait-cocoa-events (app)
  (declare (ignorable app)))

(defun poll-cocoa-events (app)
  (loop
     do
       (let ((event [(ns-object-ptr app) @(nextEventMatchingMask:untilDate:inMode:dequeue:)
		    :uint64 NSEventMaskAny
		    :pointer [#@NSDate @(distantPast)]
		    :pointer NSDefaultRunLoopMode
		    :char *yes*]))
	 
	 (when (cffi:null-pointer-p event)
	   (return (values)))

	 [(ns-object-ptr app) @(sendEvent:) :pointer event])))
    
		    


(defun display (thing)
  (let ((selector (new-msg-send @(display) (() :void))))
    (funcall selector (ns-object-ptr thing))))

(defun views-need-display? (thing)
  (let ((selector (new-msg-send @(viewsNeedDisplay) (() :bool))))
    (funcall selector (ns-object-ptr thing))))

(defun g-state (thing)
  (let ((selector (new-msg-send @(gState) (() :int))))
    (funcall selector (ns-object-ptr thing))))

(defun order-front (thing &optional (sender nil))
  (let ((selector (new-msg-send @(orderFront:) ((:pointer) :void))))
    (funcall selector (ns-object-ptr thing) (ns-object-ptr sender))))

(defun order-front-regardless (thing)
  (let ((selector (new-msg-send @(orderFrontRegardless) (() :void))))
    (funcall selector (ns-object-ptr thing))))

(defun make-key-and-order-front (thing &optional (sender nil))
  (let ((selector (new-msg-send @(makeKeyAndOrderFront:) ((:pointer) :void))))
    (funcall selector (ns-object-ptr thing) (ns-object-ptr sender))))

(defun order-out (thing &optional (sender nil))
  (let ((selector (new-msg-send @(orderOut:) ((:pointer) :void))))
    (funcall selector (ns-object-ptr thing) (ns-object-ptr sender))))

(defun ordered-index (thing)
  (let ((selector (new-msg-send @(orderedIndex) (() :int))))
    (funcall selector (ns-object-ptr thing))))

(defun set-ordered-index (thing index)
  (let ((selector (new-msg-send @(setOrderedIndex:) ((:int) :void))))
    (funcall selector (ns-object-ptr thing) index)))

#+SBCL
(defun init-with-content-rect (thing content-rect style-mask backing defer)
  (let ((selector (new-msg-send @(initWithContentRect:styleMask:backing:defer:)
				(((:struct NSRect) :uint64 :uint64 :char) :pointer))))
    (funcall selector thing
	     (list 'height (print (cffi:mem-aref content-rect :double 3))
		   'width (print (cffi:mem-aref content-rect :double 2))
		   'y (print (cffi:mem-aref content-rect :double 1))
		   'x (print (cffi:mem-aref content-rect :double 0)))
	     style-mask backing (if defer 1 0))))

(defmethod cffi:translate-to-foreign (value (type (eql :bool)))
  (if value 1 0))

(defmethod cffi:translate-from-foreign (value (type (eql :bool)))
  (if (= value 0)
      nil
      t))

#+sbcl
(defmethod ns-object-ptr ((thing sb-sys:system-area-pointer))
  thing)

#+ccl
(defmethod ns-object-ptr ((thing ccl::macptr))
  thing)

(defmethod ns-object-ptr ((thing null))
  (cffi:null-pointer))

(defun init-with-rect (thing bounds options owner user-info)
  ;; bounds is plist.
  (let ((selector (new-msg-send @(initWithRect:options:owner:userInfo:)
				(((:struct NSRect) :uint64 :pointer :pointer) :pointer))))
    (funcall selector (ns-object-ptr thing) bounds options (ns-object-ptr owner) user-info)))

(defun convert-rect-to-backing (content-view rect)
  ;; rect is plist. retval is plist (under cffi)
  (let ((selector (new-msg-send-stret @(convertRectToBacking:) (((:struct NSRect)) (:struct NSRect)))))
    (funcall selector (ns-object-ptr content-view) rect)))

(defun get-frame (content-view)
  (let ((selector (new-msg-send-stret @(frame) (() (:struct NSRect)))))
    (funcall selector (ns-object-ptr content-view))))

(defun get-bounds (content-view)
  (let ((selector (new-msg-send-stret @(bounds) (() (:struct NSRect)))))
    (funcall selector (ns-object-ptr content-view))))

(defun cascade-top-left-from-point (thing point)
  ;; point is plist.
  (let ((selector (new-msg-send @(cascadeTopLeftFromPoint:) (((:struct NSPoint)) (:struct NSPoint)))))
    (funcall selector (ns-object-ptr thing) point)))

(defun is-visible? (thing)
  (let ((selector (new-msg-send @(isVisible) (() :bool))))
    (funcall selector (ns-object-ptr thing))))

(defun level (thing)
  (let ((selector (new-msg-send @(level) (() :uint64))))
    (funcall selector (ns-object-ptr thing))))

(defun can-be-visible-on-all-spaces? (thing)
  (let ((selector (new-msg-send @(canBeVisibleOnAllSpaces) (() :bool))))
    (funcall selector (ns-object-ptr thing))))

(defun set-can-be-visible-on-all-spaces (thing value)
  (let ((selector (new-msg-send @(setCanBeVisibleOnAllSpaces:) ((:bool) :void))))
    (funcall selector (ns-object-ptr thing) (and value t))))

(defun is-opaque? (thing)
  (let ((selector (new-msg-send @(isOpaque) (() :bool))))
    (funcall selector (ns-object-ptr thing))))

(defun is-resizable? (thing)
  (let ((selector (new-msg-send @(isResizable) (() :bool))))
    (funcall selector (ns-object-ptr thing))))

(defun is-restorable? (thing)
  (let ((selector (new-msg-send @(isRestorable) (() :bool))))
    (funcall selector (ns-object-ptr thing))))

(defun is-sheet? (thing)
  (let ((selector (new-msg-send @(isSheet) (() :bool))))
    (funcall selector (ns-object-ptr thing))))

(defun is-movable? (thing)
  (let ((selector (new-msg-send @(isMovable) (() :bool))))
    (funcall selector (ns-object-ptr thing))))

(defun is-tabbed? (thing)
  (let ((selector (new-msg-send @(isTabbed) (() :bool))))
    (funcall selector (ns-object-ptr thing))))

(defun is-on-active-space? (thing)
  (let ((selector (new-msg-send @(isOnActiveSpace) (() :bool))))
    (funcall selector (ns-object-ptr thing))))

(defun is-miniaturizable? (thing)
  (let ((selector (new-msg-send @(isMiniaturizable) (() :bool))))
    (funcall selector (ns-object-ptr thing))))

(defun is-miniaturized? (thing)
  (let ((selector (new-msg-send @(isMiniaturized) (() :bool))))
    (funcall selector (ns-object-ptr thing))))

(defun miniaturize (thing &optional (sender nil))
  (let ((selector (new-msg-send @(miniaturize:) ((:pointer) :void))))
    (funcall selector (ns-object-ptr thing) (ns-object-ptr (or sender (cffi:null-pointer))))))

(defun is-key-window? (thing)
  (let ((selector (new-msg-send @(isKeyWindow) (() :bool))))
    (funcall selector (ns-object-ptr thing))))

(defun make-key-window (thing)
  (let ((selector (new-msg-send @(makeKeyWindow) (() :void))))
    (funcall selector (ns-object-ptr thing))))

(defun is-main-window? (thing)
  (let ((selector (new-msg-send @(isMainWindow)	(() :bool))))
    (funcall selector (ns-object-ptr thing))))

(defun make-main-window (thing)
  (let ((selector (new-msg-send @(makeMainWindow) (() :void))))
    (funcall selector (ns-object-ptr thing))))

(defun is-excluded-from-windows-menu? (thing)
  (let ((selector (new-msg-send @(isExcludedFromWindowsMenu)
				(() :bool))))
    (funcall selector (ns-object-ptr thing))))

(defun is-in-key-window? (thing)
  (let ((selector (new-msg-send @(isInKeyWindow) (() :bool))))
    (funcall selector (ns-object-ptr thing))))

(defun is-zoomable? (thing)
  (let ((selector (new-msg-send @(isZoomable) (() :bool))))
    (funcall selector (ns-object-ptr thing))))

(defun is-autodisplay? (thing)
  (let ((selector (new-msg-send @(isAutodisplay) (() :bool))))
    (funcall selector (ns-object-ptr thing))))

(defun is-floating-panel? (thing)
  (let ((selector (new-msg-send @(isFloatingPanel) (() :bool))))
    (funcall selector (ns-object-ptr thing))))

(defun is-one-shot? (thing)
  (let ((selector (new-msg-send @(isOneShot) (() :bool))))
    (funcall selector (ns-object-ptr thing))))

(defun is-zoomed? (thing)
  (let ((selector (new-msg-send @(isZoomed) (() :bool))))
    (funcall selector (ns-object-ptr thing))))

(defun perform-zoom (thing &optional (sender nil))
  (let ((selector (new-msg-send @(performZoom:) ((:pointer) :void))))
    (funcall selector (ns-object-ptr thing) (ns-object-ptr (or sender (cffi:null-pointer))))))

(defun zoom (thing &optional (sender nil))
  (let ((selector (new-msg-send @(zoom:) ((:pointer) :void))))
    (funcall selector (ns-object-ptr thing) (ns-object-ptr (or sender (cffi:null-pointer))))))

(defun style-mask (thing)
  (let ((selector (new-msg-send @(styleMask) (() :uint64))))
    (funcall selector (ns-object-ptr thing))))

(defun borderless? (window)
  (logtest NSWindowStyleMaskBorderLess (style-mask window)))

(defun full-screen? (window)
  (logtest NSWindowStyleMaskFullScreen (style-mask window)))

(defun toggle-full-screen (window &optional (sender nil))
  (let ((selector (new-msg-send @(toggleFullScreen:) ((:pointer) :void))))
    (funcall selector (ns-object-ptr window) (ns-object-ptr (or sender (cffi:null-pointer))))))

(defun titled-style? (window)
  (logtest NSWindowStyleMaskTitled (style-mask window)))

(defun closable-style? (window)
  (logtest NSWindowStyleMaskClosable (style-mask window)))

(defun miniaturizable-style? (window)
  (logtest NSWindowStyleMaskMiniaturizable (style-mask window)))

(defun resizable-style? (window)
  (logtest NSWindowStyleMaskResizable (style-mask window)))

(defun is-modal-panel? (thing)
  (let ((selector (new-msg-send @(isModalPanel)	(() :bool))))
    (funcall selector (ns-object-ptr thing))))

(defun is-in-full-screen-mode? (view)
  (let ((selector (new-msg-send @(isInFullScreenMode) (() :bool))))
    (funcall selector (ns-object-ptr view))))

(defun has-close-box? (thing)
  (let ((selector (new-msg-send @(hasCloseBox) (() :bool))))
    (funcall selector (ns-object-ptr thing))))

(defun has-title-bar? (thing)
  (let ((selector (new-msg-send @(hasTitleBar) (() :bool))))
    (funcall selector (ns-object-ptr thing))))

(defun can-enter-full-screen-mode? (thing)
  (let ((selector (new-msg-send @(canEnterFullScreenMode)
				(() :bool))))
    (funcall selector (ns-object-ptr thing))))

(defun enter-full-screen-mode (thing screen &optional (options nil))
  (let ((selector (new-msg-send @(enterFullScreenMode:)
				((:pointer :int) :bool))))
    (funcall selector (ns-object-ptr thing) (ns-object-ptr screen) (if options options 0))))

(defun exit-full-screen-mode (thing &optional (options nil))
  (let ((selector (new-msg-send @(exitFullScreenMode:)
				((:int) :bool))))
    (funcall selector (ns-object-ptr thing) (if options options 0))))

(defun get-screen (thing)
  (let ((selector (new-msg-send @(screen) (() :pointer))))
    (make-instance 'ns-screen :ptr (funcall selector (ns-object-ptr thing)))))

(defun can-become-key-window? (thing)
  (let ((selector (new-msg-send @(canBecomeKeyWindow) (() :bool))))
    (funcall selector (ns-object-ptr thing))))

(defun can-become-main-window? (thing)
  (let ((selector (new-msg-send @(canBecomeMainWindow) (() :bool))))
    (funcall selector (ns-object-ptr thing))))

(defun can-represent-display-gamut? (thing)
  (let ((selector (new-msg-send @(canRepresentDisplayGamut)
				(() :bool))))
    (funcall selector (ns-object-ptr thing))))

(defun can-store-color? (thing)
  (let ((selector (new-msg-send @(canStoreColor) (() :bool))))
    (funcall selector (ns-object-ptr thing))))

(defun can-hide? (thing)
  (let ((selector (new-msg-send @(canHide) (() :bool))))
    (funcall selector (ns-object-ptr thing))))

(defun get-delegate (thing)
  (let* ((selector (new-msg-send @(delegate) (() :pointer)))
	 (pdelegate (funcall selector (ns-object-ptr thing))))
    pdelegate))

(defun get-content-view (thing)
  (let* ((selector (new-msg-send @(contentView) (() :pointer))))
    (gethash (sap-int (funcall selector (ns-object-ptr thing)))
	     (content-view->clos-content-view-table *app*))))

(defun get-window-controller (thing)
  (let* ((selector (new-msg-send @(windowController) (() :pointer))))
    (funcall selector (ns-object-ptr thing))))


(defun get-cocoa-monitor-pos (monitor)
  (let ((bounds (CGDisplayBounds (monitor-display-id monitor))))
    (values (getf bounds 'x)
	    (getf bounds 'y))))


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

(defun get-cocoa-video-mode (monitor)
  (let ((native (CGDisplayCopyDisplayMode (monitor-display-id monitor))))
    (unwind-protect
	 (vidmode-from-cg-display-mode native
					  (monitor-fallback-refresh-rate monitor))
      (CGDisplayModeRelease native))))

(defun cocoa-transform-y (y)
  (declare (type real y))
  (coerce (1- (- (getf (CGDisplayBounds (CGMainDisplayID)) 'height) y)) 'double-float))

(defun (setf cocoa-window-title) (string window)
  [(ns-object-ptr window) @(setTitle:)
  :pointer (objc-runtime::make-nsstring string)]
  string)

(defun get-cocoa-window-frame-size (window)
  (declare (ignorable window))
  )

(defun get-cocoa-window-content-scale (window)
  (declare (ignorable window))
  (values 2.0d0 2.0d0)
  )

(defun get-cocoa-window-cursor-pos (window)
  (declare (ignorable window))
  (values 100 100)
  )

(defun get-cocoa-window-framebuffer-size (window)
  (let* ((content-rect (get-frame (window-content-view window))))
    (print content-rect)
    (let ((fb-rect (convert-rect-to-backing (window-content-view window) content-rect)))
      (print fb-rect)
      (values (getf fb-rect 'width) (getf fb-rect 'height)))))

(defun get-cocoa-window-pos (window)
  (declare (ignorable window))
  )

(defun get-cocoa-window-size (window)
  (let ((frame (get-frame (window-content-view window))))
    (values (getf frame 'width) (getf frame 'height))))

(defun iconify-cocoa-window (window)
  (declare (ignorable window))
  )

(defun maximize-cocoa-window (window)
  (declare (ignorable window))
  )

(defun restore-cocoa-window (window)
  (declare (ignorable window))
  )

(defun set-cocoa-window-aspect-ratio (window numer denom)
  (declare (ignorable window numer denom))
  )

(defun set-cocoa-window-pos (window x y)
  (declare (ignorable window x y))
  )

(defun set-cocoa-window-size-limits (window width height)
  (declare (ignorable window width height))
  )

(defun terminate-cocoa-application (app)
  (declare (ignorable app))
  )

(defun backing-scale-factor (window)
  (let ((selector (new-msg-send @(backingScaleFactor)
				(() :double))))
    (funcall selector (ns-object-ptr window))))



#+vulkan
(defun get-cocoa-required-instance-extensions ()
  (list "VK_KHR_surface"
	(if (symbol-value (intern (symbol-name '*use-metal-surface*) :vk))
	    "VK_EXT_metal_surface"
	    "VK_MVK_macos_surface")))


(defun cocoa-window-title (window)
  (let ((ptr [(ns-object-ptr window) @(title)]))
    (unless (cffi:null-pointer-p ptr)
      (objc-runtime::extract-nsstring ptr))))

(defun set-delegate (window delegate)
  (let ((selector (new-msg-send @(setDelegate:)
				((:pointer) :void))))
    (funcall selector (ns-object-ptr window) (ns-object-ptr delegate))))

(defun set-content-view (window view)
  (let ((selector (new-msg-send @(setContentView:)
				((:pointer) :void))))
    (funcall selector (ns-object-ptr window) (ns-object-ptr view))))

(defun make-first-responder (window view)
  (let ((selector (new-msg-send @(makeFirstResponder:)
				((:pointer) :void))))
    (funcall selector (ns-object-ptr window) (ns-object-ptr view))))

(defun ff-call (name return-type &rest args)
  (eval
   (multiple-value-bind (types ctypes fargs rettype)
       (cffi::parse-args-and-types (append args (list return-type)))
     (let ((syms (cffi::make-gensym-list (length types))))
       (cffi::foreign-funcall-form/fsbv-with-libffi name
						    fargs
						    syms
						    types
						    rettype
						    ctypes
						    nil)))))
					       
					       
					       
#+OLD
(defun ff-call (name return-type &rest args)
  (apply #'sb-alien::alien-funcall
	 (sb-alien-internals:%alien-value
	  (sb-alien::foreign-symbol-sap name nil)
	  0
	  (sb-alien::parse-alien-type (list* 'sb-alien::function
					     (cffi-sys::convert-foreign-type return-type)
					     (mapcar #'cffi-sys::convert-foreign-type
						     (serapeum::plist-keys args)))
					nil))
	 (serapeum:plist-values args)))

(defun send (object message return-type &rest args)
  (if (listp return-type)
      (apply #'ff-call "objc_msgSend_stret"
	     return-type
	     :pointer (ns-object-ptr object)
	     :pointer message
	     args)
      (apply #'ff-call "objc_msgSend"
	     return-type
	     :pointer (ns-object-ptr object)
	     :pointer message
	     args)))

(defun array-with-objects (&rest sequence)
  (let ((args (append
	       (loop for i from 0 below (length sequence)
		  append (list :pointer (elt sequence i)))
	       (list :pointer (cffi:null-pointer)))))
  (apply #'send #@NSArray @(arrayWithObjects:) :pointer args)))

(defun register-for-dragged-types (view &rest new-types)
  (send view @(registerForDraggedTypes:) :void :pointer (apply #'array-with-objects new-types)))

(defun alloc-init (objc-class)
  (init (alloc objc-class)))

(defun alloc (objc-class)
  (send objc-class @(alloc) :pointer))

(defun init (objc-object)
  (send objc-object @(init) :pointer))

(defun create-native-window (window &rest args
			     &key (xpos nil) (ypos nil)
			       (width 640) (height 480)
			       (title "Abstract OS")
			       (maximized? nil) 
			       (resizable? t)
			       (floating? nil)
			       (transparent? nil)
			       (frame-name "Abstract OS")
			       (retina? t)
			       &allow-other-keys)
  (declare (ignorable args))

  #+sbcl
  (sb-int:set-floating-point-modes :traps '())
  
  (with-nsrect (content-rect)
    (if (window-monitor window)
	  
	(multiple-value-bind (xpos ypos) (get-cocoa-monitor-pos (window-monitor window))
	  (let* ((mode (get-cocoa-video-mode (window-monitor window)))
		 (width (video-mode-width mode))
		 (height (video-mode-height mode)))
	      
	    (setf (cffi:foreign-slot-value content-rect '(:struct NSRect) 'x) (coerce xpos 'double-float)
		  (cffi:foreign-slot-value content-rect '(:struct NSRect) 'y) (coerce ypos 'double-float)
		  (cffi:foreign-slot-value content-rect '(:struct NSRect) 'width) (coerce width 'double-float)
		  (cffi:foreign-slot-value content-rect '(:struct NSRect) 'height) (coerce height 'double-float))))

	(if (or (null xpos) (null ypos))
	    (setf (cffi:foreign-slot-value content-rect '(:struct NSRect) 'x) 0.0d0
		  (cffi:foreign-slot-value content-rect '(:struct NSRect) 'y) 0.0d0
		  (cffi:foreign-slot-value content-rect '(:struct NSRect) 'width)
		  (or (and width (coerce width 'double-float)) 640.0d0)
		  (cffi:foreign-slot-value content-rect '(:struct NSRect) 'height)
		  (or (and height (coerce height 'double-float)) 480.0d0))

	    (setf (cffi:foreign-slot-value content-rect '(:struct NSRect) 'x)
		  (coerce xpos 'double-float)
		  
		  (cffi:foreign-slot-value content-rect '(:struct NSRect) 'y)
		  (cocoa-transform-y (1- (+ ypos (or height 480.0d0))))
		  
		  (cffi:foreign-slot-value content-rect '(:struct NSRect) 'width)
		  (or (and width (coerce width 'double-float)) 640.0d0)
		  
		  (cffi:foreign-slot-value content-rect '(:struct NSRect) 'height)
		  (or (and height (coerce height 'double-float)) 480.0d0))))

    (let ((style-mask NSWindowStyleMaskMiniaturizable))

      (if (or (window-monitor window) (not (decorated? window)))
	  
	  (setq style-mask (logior style-mask NSWindowStyleMaskBorderless))
	  
	  (progn
	    (setq style-mask (logior style-mask NSWindowStyleMaskTitled NSWindowStyleMaskClosable))
	    
	    (when (resizable? window)
	      (setq style-mask (logior style-mask NSWindowStyleMaskResizable)))))

      (setf (ns-object-ptr window)
	    (init-with-content-rect (alloc (window-class *app*))
				    content-rect
				    style-mask NSBackingStoreBuffered nil))
	
      (when (cffi:null-pointer-p (ns-object-ptr window))
	(error "Cocoa: Failed to create window."))

      (setf (cocoa-window-title window)
	    (or (and title (objc-runtime::make-nsstring title)) (objc-runtime::make-nsstring "Abstract OS")))
      
      (setf (window-delegate window)
	    (make-instance 'window-delegate
			   :ptr (alloc-init (window-delegate-class *app*))
			   :owner window))

      (set-delegate window (window-delegate window))
	  
      (setf (window-content-view window)
	    (make-instance 'content-view
			   :ptr (alloc-init (content-view-class *app*))
			   :owner window
			   :marked-text (alloc-init #@NSMutableAttributedString)))

      ;;      (super-init-with-frame (window-content-view window) (get-frame window))
      (send (window-content-view window) @(updateTrackingAreas) :void)

      (set-content-view window (window-content-view window))
      (make-first-responder window (window-content-view window))

      (register-for-dragged-types (window-content-view window)
				  (objc-runtime::make-nsstring "NSPasteboardTypedURL"))
	
      (if (window-monitor window)

	  (send window @(setLevel:) :void :int (1+ NSMainMenuWindowLevel))
	    
	  (progn
	      
	    (when (or (null xpos) (null ypos))
	      (setf (cascade-point *app*)
		    (cascade-top-left-from-point window (cascade-point *app*))))
	      
	    (let ((behavior (if resizable?
				(logior NSWindowCollectionBehaviorFullScreenPrimary
					NSWindowCollectionBehaviorManaged)
				NSWindowCollectionBehaviorFullScreenNone)))
		  
	      (send window @(setCollectionBehavior:) :void :int behavior))
	      
	    (when floating?
	      (send window @(setLevel:) :void :int NSFloatingWindowLevel))
	      
	    (when maximized?
	      (send window @(zoom:) :void :char *yes*))))
	  
      (when (and frame-name (not (string= frame-name "")))
	(send window @(setFrameAutosaveName:) :void :pointer (objc-runtime::make-nsstring frame-name)))
	  
      (setf (window-retina? window) retina?)

      (when transparent?
	(send window @(setOpaque:) :void :char *yes*)
	(send window @(setHasShadow:) :void :char *yes*)
	(send window @(setBackgroundColor:) :void (send #@NSColor @(clearColor) :pointer)))

      (send window @(setAcceptsMouseMovedEvents:) :void :char *yes*)
      (send window @(setRestorable:) :void :char *no*)

      (multiple-value-bind (width height)
	  (get-cocoa-window-size window)
	(setf (width window) width
	      (height window) height))

      (multiple-value-bind (fb-width fb-height)
	  (get-cocoa-window-framebuffer-size window)
	(setf (window-fb-width window) fb-width
	      (window-fb-height window) fb-height))

      #+NOTYET
      (setf (window-graphics-context window)
	    (send #@NSGraphicsContext @(graphicsContextWithWindow:) :pointer :pointer (ns-object-ptr window)))

      t)))

(defun set-background-color (window color)
  (let ((selector (new-msg-send @(setBackgroundColor:)
				((:pointer) :void))))
    (funcall selector (ns-object-ptr window) (ns-object-ptr color))))
	    

(defun NSRectFill (rect)
  (cffi:foreign-funcall "NSRectFill" (:struct NSRect) rect :void))



    

(defun create-cocoa-window (window &rest initargs
			    &key (visible? t)
			      (focused? t)
			      (auto-iconify? t)
			      (focus-on-show? t)
			      (center-cursor? t)
			      (mouse-passthrough? nil)
			      &allow-other-keys)
  (declare (ignorable auto-iconify? focus-on-show?))
  (apply #'create-native-window window initargs)

  (when mouse-passthrough?
    #+NOTYET
    (set-cocoa-window-mouse-passthrough window t))

  (if (window-monitor window)
      (progn
	(show-cocoa-window window)
	(focus-cocoa-window window)
	(acquire-monitor window)
	(when center-cursor?
	  #+NOTYET
	  (center-cursor-in-content-area window)))
      (when visible?
	(show-cocoa-window window)
	(when focused?
	  (focus-cocoa-window window))))
  t)

(defun show-cocoa-window (window)
  (order-front window))

(defun hide-cocoa-window (window)
  (order-out window))

(defun focus-cocoa-window (window)
  [(ns-object-ptr *app*) @(activateIgnoringOtherApps:) :char *yes*]
  (make-key-and-order-front window))
	      
(defun set-cocoa-video-mode (monitor video-mode)
  (declare (ignorable monitor video-mode))
  (values))

(defun acquire-monitor (window)
  (set-cocoa-video-mode (window-monitor window) (window-video-mode window))
  (let ((bounds (CGDisplayBounds (monitor-display-id (window-monitor window)))))
    (with-nsrect (frame :x (getf bounds 'x)
			:y (cocoa-transform-y (1- (+ (getf bounds 'y) (getf bounds 'height))))
			:width (getf bounds 'width)
			:height (getf bounds 'height))
      ;;[(ns-object-ptr window) @(setFrame:) (:struct NSRect) frame :pointer @(display:) :boolean t]
      (input-monitor-window (window-monitor window) window))))


;;

(deftraceable-callback application-delegate-application-should-terminate-callback :pointer
    ((self :pointer) (_cmd :pointer) (notification :pointer))
;;  (declare (ignorable self _cmd))
  (let ((application *app*))
    (when application
      (application-should-terminate application notification))))

(defun application-should-terminate (application notification)
  (declare (ignorable application notification))
  application)

(deftraceable-callback application-delegate-application-did-change-screen-parameters-callback :void
    ((self :pointer) (_cmd :pointer) (notification :pointer))
;;  (declare (ignorable self _cmd))
  (format t "~%application-did-change-screen-parameters")
  (finish-output)
  (let ((application *app*))
    (when application
      (application-did-change-screen-parameters application notification))
    (values)))

(defun application-did-change-screen-parameters (application notification)
  (declare (ignorable application notification))
  (values))

(deftraceable-callback application-delegate-application-will-finish-launching-callback :void
    ((self :pointer) (_cmd :pointer) (notification :pointer))
;;  (declare (ignorable self _cmd))
  (format t "~%application-will-finish-launching")
  (finish-output)
  (let ((application *app*))
    (when application
      (application-will-finish-launching application notification))
    (values)))

(defun application-will-finish-launching (application notification)
  (declare (ignorable application notification))
  (create-menu-bar application)
  (values))

(deftraceable-callback application-delegate-application-did-finish-launching-callback :void
    ((self :pointer) (_cmd :pointer) (notification :pointer))
;;  (declare (ignorable self _cmd))
  (let ((application *app*))
    (when application
      (application-did-finish-launching application notification))
    (values)))

(defun application-did-finish-launching (application notification)
  (declare (ignorable application notification))
  ;;[(ns-object-ptr application) @(setAutomaticCustomizeTouchBarMenuItemEnabled:) :int 0]
  (post-empty-event application)
  [(ns-object-ptr application) @(stop:) :pointer (cffi:null-pointer)]
  (values))

(deftraceable-callback application-delegate-application-did-hide-callback :void
    ((self :pointer) (_cmd :pointer) (notification :pointer))
;;  (declare (ignorable self _cmd))
  (let ((application *app*))
    (when application
      (application-did-hide application notification))
    (values)))

(defun application-did-hide (application notification)
  (declare (ignorable application notification))
  (values))

(deftraceable-callback observe-value-for-key-path-callback :void
    ((self :pointer) (_cmd :pointer) (key-path :pointer)
     (of-object :pointer) (change :pointer) (context :pointer))
  (values))

(defun make-application-delegate-class ()
  (let ((application-delegate-class (objc-runtime::objc-allocate-class-pair
				     #@NSObject "AbstractOSApplicationDelegate" 0)))
    (objc-runtime::class-add-method application-delegate-class @(applicationShouldTerminate:)
				    (cffi:callback application-delegate-application-should-terminate-callback)
				    "@@:@")
    (objc-runtime::class-add-method application-delegate-class @(applicationDidChangeScreenParameters:)
				    (cffi:callback application-delegate-application-did-change-screen-parameters-callback)
				    "v@:@")
    (objc-runtime::class-add-method application-delegate-class @(applicationWillFinishLaunching:)
				    (cffi:callback application-delegate-application-will-finish-launching-callback)
				    "v@:@")
    (objc-runtime::class-add-method application-delegate-class @(applicationDidFinishLaunching:)
				    (cffi:callback application-delegate-application-did-finish-launching-callback)
				    "v@:@")
    (objc-runtime::class-add-method application-delegate-class @(applicationDidHide:)
				    (cffi:callback application-delegate-application-did-hide-callback)
				    "v@:@")
    (objc-runtime::class-add-method application-delegate-class @(observeValueForKeyPath:ofObject:change:context:)
				    (cffi:callback observe-value-for-key-path-callback)
				    "v@:@@@@")
    application-delegate-class))

(deftraceable-callback window-delegate-window-should-close-callback :void ((self :pointer) (sender :pointer))
;;  (declare (ignore sender))
  (let ((window (gethash (sap-int self) (delegate->clos-window-table *app*))))
    (when window
      (ns-window-should-close window))
    (values)))

(defun ns-window-should-close (window)
  (input-window-close-request window))

(deftraceable-callback window-delegate-window-did-resize-callback :void ((self :pointer) (notification :pointer))
;;  (declare (ignore notification))
  (let ((window (gethash (sap-int self) (delegate->clos-window-table *app*))))
    (when window
      (window-did-resize window))
    (values)))

(defun window-did-resize (window)
  (declare (ignorable window))
  #+NIL
  (when (eq window (disabled-cursor-window *app*))
    (center-cursor-in-content-area window))
  (let ((maximized? (is-zoomed? window)))
    (unless (eq (maximized? window) maximized?)
      (setf (maximized? window) maximized?)
      (input-window-maximize window maximized?)))
  (let* ((content-rect (get-frame (window-content-view window)))
	 (fb-rect (convert-rect-to-backing (window-content-view window) content-rect))
	 (fb-width (getf fb-rect 'width))
	 (fb-height (getf fb-rect 'height)))
    (when (or (/= fb-width (window-fb-width window))
	      (/= fb-height (window-fb-height window)))
      (setf (window-fb-width window) fb-width)
      (setf (window-fb-height window) fb-height)
      (input-framebuffer-size window fb-width fb-height))))

(deftraceable-callback window-delegate-window-did-move-callback :void ((self :pointer) (notification :pointer))
;;  (declare (ignore notification))
  (let ((window (gethash (sap-int self) (delegate->clos-window-table *app*))))
    (when window
      (window-did-move window))
    (values)))

(defun window-did-move (window)
  (declare (ignorable window))
  (values))

(deftraceable-callback window-delegate-window-did-miniaturize-callback :void ((self :pointer) (notification :pointer))
;;  (declare (ignore notification))
  (let ((window (gethash (sap-int self) (delegate->clos-window-table *app*))))
    (when window
      (window-did-miniaturize window))
    (values)))

(defun window-did-miniaturize (window)
  (declare (ignorable window))
  #+NOTYET(release-monitor window)
  (values))

(deftraceable-callback window-delegate-window-did-deminiaturize-callback :void ((self :pointer) (notification :pointer))
;;  (declare (ignore notification))
  (let ((window (gethash (sap-int self) (delegate->clos-window-table *app*))))
    (when window
      (window-did-deminiaturize window))
    (values)))

(defun window-did-deminiaturize (window)
  (declare (ignorable window))
  (acquire-monitor window)
  (values))

(deftraceable-callback window-delegate-window-did-become-key-callback :void ((self :pointer) (notification :pointer))
;;  (declare (ignore notification))
  (let ((window (gethash (sap-int self) (delegate->clos-window-table *app*))))
    (when window
      (window-did-become-key window))
    (values)))

(defun window-did-become-key (window)
  (declare (ignorable window))
  ;;(set-needs-display-in-rect (window-content-view window) (get-bounds (window-content-view window)))
  (values))

(deftraceable-callback window-delegate-window-did-resign-key-callback :void ((self :pointer) (notification :pointer))
;;  (declare (ignore notification))
  (let ((window (gethash (sap-int self) (delegate->clos-window-table *app*))))
    (when window
      (window-did-resign-key window))
    (values)))

(defun window-did-resign-key (window)
  (declare (ignorable window))
  (values))

(deftraceable-callback window-delegate-window-did-change-occlusion-state-callback :void ((self :pointer) (notification :pointer))
;;  (declare (ignore notification))
  (let ((window (gethash (sap-int self) (delegate->clos-window-table *app*))))
    (when window
      (window-did-change-occlusion-state window))
    (values)))

(defun window-did-change-occlusion-state (window)
  (declare (ignorable window))
  (values))


    
(defun make-window-delegate-class ()
  (let ((window-delegate-class (objc-runtime::objc-allocate-class-pair
				#@NSObject "AbstractOSWindowDelegate" 0)))
    (objc-runtime::class-add-method window-delegate-class @(windowShouldClose:)
				    (cffi:callback window-delegate-window-should-close-callback)
				    "v@:@")
    (objc-runtime::class-add-method window-delegate-class @(windowDidResize:)
				    (cffi:callback window-delegate-window-did-resize-callback)
				    "v@:@")
    (objc-runtime::class-add-method window-delegate-class @(windowDidMove:)
				    (cffi:callback window-delegate-window-did-move-callback)
				    "v@:@")
    (objc-runtime::class-add-method window-delegate-class @(windowDidMiniaturize:)
				    (cffi:callback window-delegate-window-did-miniaturize-callback)
				    "v@:@")
    (objc-runtime::class-add-method window-delegate-class @(windowDidDeminiaturize:)
				    (cffi:callback window-delegate-window-did-deminiaturize-callback)
				    "v@:@")
    (objc-runtime::class-add-method window-delegate-class @(windowDidBecomeKey:)
				    (cffi:callback window-delegate-window-did-become-key-callback)
				    "v@:@")
    (objc-runtime::class-add-method window-delegate-class @(windowDidResignKey:)
				    (cffi:callback window-delegate-window-did-resign-key-callback)
				    "v@:@")
    (objc-runtime::class-add-method window-delegate-class @(windowDidChangeOcclusionState:)
				    (cffi:callback window-delegate-window-did-change-occlusion-state-callback)
				    "v@:@")
    window-delegate-class))

(defun make-content-view-class ()
  (let ((content-view-class
	 (objc-runtime::objc-allocate-class-pair
	  #@NSView "AbstractOSContentView" 0)))

    (objc-runtime::class-add-method content-view-class @(dealloc)
				    (cffi:callback content-view-dealloc-callback)
				    "v@:")
    (objc-runtime::class-add-method content-view-class @(isOpaque)
				    (cffi:callback content-view-is-opaque-callback)
				    "c@:")
    (objc-runtime::class-add-method content-view-class @(canBecomeKeyView)
				    (cffi:callback content-view-can-become-key-view-callback)
				    "c@:")
    (objc-runtime::class-add-method content-view-class @(acceptsFirstResponder)
				    (cffi:callback content-view-accepts-first-responder-callback)
				    "c@:")
    (objc-runtime::class-add-method content-view-class @(wantsUpdateLayer)
				    (cffi:callback content-view-wants-update-layer-callback)
				    "c@:")
    (objc-runtime::class-add-method content-view-class @(updateLayer)
				    (cffi:callback content-view-update-layer-callback)
				    "v@:")
    (objc-runtime::class-add-method content-view-class @(cursorUpdate)
				    (cffi:callback content-view-cursor-update-callback)
				    "v@:@")
    (objc-runtime::class-add-method content-view-class @(acceptsFirstMouse)
				    (cffi:callback content-view-accepts-first-mouse-callback)
				    "c@:")
    (objc-runtime::class-add-method content-view-class @(mouseDown)
				    (cffi:callback content-view-mouse-down-callback)
				    "v@:@")
    (objc-runtime::class-add-method content-view-class @(mouseDragged)
				    (cffi:callback content-view-mouse-dragged-callback)
				    "v@:@")
    (objc-runtime::class-add-method content-view-class @(mouseUp)
				    (cffi:callback content-view-mouse-up-callback)
				    "v@:@")
    (objc-runtime::class-add-method content-view-class @(mouseMoved)
				    (cffi:callback content-view-mouse-moved-callback)
				    "v@:@")
    (objc-runtime::class-add-method content-view-class @(rightMouseDown)
				    (cffi:callback content-view-right-mouse-down-callback)
				    "v@:@")
    (objc-runtime::class-add-method content-view-class @(rightMouseDragged)
				    (cffi:callback content-view-right-mouse-dragged-callback)
				    "v@:@")
    (objc-runtime::class-add-method content-view-class @(rightMouseUp)
				    (cffi:callback content-view-right-mouse-up-callback)
				    "v@:@")
    (objc-runtime::class-add-method content-view-class @(otherMouseDown)
				    (cffi:callback content-view-other-mouse-down-callback)
				    "v@:@")
    (objc-runtime::class-add-method content-view-class @(otherMouseDragged)
				    (cffi:callback content-view-other-mouse-dragged-callback)
				    "v@:@")
    (objc-runtime::class-add-method content-view-class @(otherMouseUp)
				    (cffi:callback content-view-other-mouse-up-callback)
				    "v@:@")
    (objc-runtime::class-add-method content-view-class @(otherMouseExited)
				    (cffi:callback content-view-other-mouse-exited-callback)
				    "v@:@")
    (objc-runtime::class-add-method content-view-class @(otherMouseEntered)
				    (cffi:callback content-view-other-mouse-entered-callback)
				    "v@:@")
    (objc-runtime::class-add-method content-view-class @(didChangeBackingProperties)
				    (cffi:callback content-view-view-did-change-backing-properties-callback)
				    "v@:")
    (objc-runtime::class-add-method content-view-class @(drawRect)
				    (cffi:callback content-view-draw-rect-callback)
				    "v@:@")
    (objc-runtime::class-add-method content-view-class @(updateTrackingAreas)
				    (cffi:callback content-view-update-tracking-areas-callback)
				    "v@:")
    content-view-class))

(deftraceable-callback window-can-become-key-window-callback :char ((self :pointer) (_cmd :pointer))
;;  (declare (ignore self _cmd))
  *yes*)

(deftraceable-callback window-can-become-main-window-callback :char ((self :pointer) (_cmd :pointer))
;;  (declare (ignore self _cmd))
  *yes*)

(defun make-window-class ()
  (let ((window-class
	 (objc-runtime::objc-allocate-class-pair
	  #@NSWindow "AbstractOSWindow" 0)))
    (objc-runtime::class-add-method window-class @(canBecomeKeyWindow)
				    (cffi:callback window-can-become-key-window-callback)
				    "c@:")
    (objc-runtime::class-add-method window-class @(canBecomeMainWindow)
				    (cffi:callback window-can-become-main-window-callback)
				    "c@:")
    (objc-runtime::class-add-method window-class @(observeValueForKeyPath:ofObject:change:context:)
				    (cffi:callback observe-value-for-key-path-callback)
				    "v@:@@@@")
    window-class))

(deftraceable-callback application-helper-do-nothing-callback :void ((self :pointer) (_cmd :pointer) (object :pointer))
;;  (declare (ignore self _cmd object))
  (values))

(deftraceable-callback application-helper-selected-keyboard-input-source-changed-callback :void
    ((self :pointer) (_cmd :pointer) (object :pointer))
;;  (declare (ignorable self _cmd))
  (application-helper-selected-keyboard-input-source-changed *app* object)
  (values))

(defun application-helper-selected-keyboard-input-source-changed (application object)
  (declare (ignorable application))
  (declare (ignore object))
  #+NOTYET(update-unicode-data application))
  

(defun make-helper-class ()
  (let ((helper-class (objc-runtime::objc-allocate-class-pair #@NSObject "AbstractOSAppHelper" 0)))
    (objc-runtime::class-add-method helper-class @(doNothing:)
				    (cffi:callback application-helper-do-nothing-callback)
				    "v@:@")
    (objc-runtime::class-add-method helper-class @(selectedKeyboardInputSourceChanged:)
				    (cffi:callback application-helper-selected-keyboard-input-source-changed-callback)
				    "v@:@")
    (objc-runtime::class-add-method helper-class @(observeValueForKeyPath:ofObject:change:context:)
				    (cffi:callback observe-value-for-key-path-callback)
				    "v@:@@@@")
    helper-class))

(defun is-hidden? (content-view)
  (let ((selector (new-msg-send @(isHidden)
				(() :bool))))
    (funcall selector (ns-object-ptr content-view))))

#+NIL
(trace ns-window-should-close window-did-resize window-did-move window-did-miniaturize
       window-did-deminiaturize window-did-become-key window-did-resign-key window-did-change-occlusion-state
       content-view-dealloc
       content-view-is-opaque  content-view-accepts-first-responder
       content-view-wants-update-layer content-view-update-layer content-view-cursor-update
       content-view-accepts-first-mouse content-view-mouse-down content-view-mouse-dragged
       content-view-mouse-up content-view-mouse-moved content-view-right-mouse-down
       content-view-right-mouse-dragged content-view-right-mouse-up content-view-other-mouse-down
       content-view-other-mouse-dragged content-view-other-mouse-up content-view-other-mouse-exited
       content-view-other-mouse-entered  content-view-draw-rect
       content-view-update-tracking-areas)

#+NIL
(defsystem clim-cocoa-mac-ui
  (:default-pathname "ccl:mac-ui;")
  (:serial
   :objc-support
   "cf-utils"
    "libdispatch"
    "ccl-application"
    "event-process"
    "cg"))

(defvar w)

(defun main-loop-body (window app)
  (set-needs-display-in-rect (window-content-view window) (get-bounds (window-content-view window)))
  (poll-cocoa-events app)
  
  ;;(content-view-draw-rect (window-content-view window) (get-bounds (window-content-view window)))
  [(window-graphics-context w) @(flushGraphics)]
  )

(defun set-needs-display-in-rect (self rect)
  (let ((selector (new-msg-send @(setNeedsDisplayInRect:)
				(((:struct NSRect)) :void))))
    (funcall selector (ns-object-ptr self) rect)))

(defun test-main-loop (app)
  (let ((window w #+NIL(setq w (make-os-window))))
    (set-needs-display-in-rect (window-content-view window) (get-bounds (window-content-view window)))
    (tagbody
     start
       (main-loop-body window app)
       (go start))))

(defun test ()
  #+sbcl
  (sb-int:set-floating-point-modes :traps '())
  [(ns-object-ptr *app*) @(run)])

(defun visible-rect (content-view)
  (let ((selector (new-msg-send @(visibleRect)
				(() (:struct NSRect)))))
    (funcall selector (ns-object-ptr content-view))))

      
  
