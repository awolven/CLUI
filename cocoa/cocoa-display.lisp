(in-package :clui)
(named-readtables:in-readtable :objc-readtable)

(defun terminate-cocoa-application (display)
  ;; todo: close windows here.
  (ns:|stop:| display nil)
  (values))

(deftraceable-callback application-helper-do-nothing-callback :void ((self :pointer) (_cmd :pointer) (object :pointer))
;;  (declare (ignore self _cmd object))
  (values))

(deftraceable-callback application-helper-selected-keyboard-input-source-changed-callback :void
    ((self :pointer) (_cmd :pointer) (object :pointer))
  ;;  (declare (ignorable self _cmd))
  (let ((display (find-if (lambda (object)
			    (typep object 'cocoa:desktop-mixin))
			  *displays*)))
    (application-helper-selected-keyboard-input-source-changed display)
    (values)))

(defun application-helper-selected-keyboard-input-source-changed ()
  (update-unicode-data display (slot-value display 'kPropertyUnicodeKeyLayoutData)))

(defun make-helper-class ()
  (let ((helper-class (objc-runtime::objc-allocate-class-pair #@NSObject "CluiAppHelper" 0)))
    (objc-runtime::class-add-method helper-class @(doNothing:)
				    (cffi:callback application-helper-do-nothing-callback)
				    "v@:@")
    (objc-runtime::class-add-method helper-class @(selectedKeyboardInputSourceChanged:)
				    (cffi:callback application-helper-selected-keyboard-input-source-changed-callback)
				    "v@:@")
    (objc_registerClassPair helper-class)
    helper-class))

(deftraceable-callback window-controller-make-touch-bar-callback :pointer ((self :pointer) (_cmd :pointer))
  (let ((tb (alloc-init #@NSTouchBar)))
    (ns:|setTouchBar:| self tb)
    tb))

(deftraceable-callback window-controller-touch-bar-callback :pointer ((self :pointer) (_cmd :pointer))
  (alloc-init #@NSTouchBar))

(deftraceable-callback window-controller-touch-bar-make-item-for-identifier-callback :pointer ((self :pointer) (_cmd :pointer) (touch-bar :pointer) (identifier :pointer))
		      (cffi:null-pointer))

(deftraceable-callback application-delegate-make-touch-bar-callback :pointer ((self :pointer) (_cmd :pointer))
  (alloc-init #@NSTouchBar))

(deftraceable-callback application-delegate-touch-bar-make-item-for-identifier-callback :pointer ((self :pointer) (_cmd :pointer) (touch-bar :pointer) (identifier :pointer))
		      (cffi:null-pointer))

(deftraceable-callback application-delegate-touch-bar-callback :pointer ((self :pointer) (_cmd :pointer))
  (alloc-init #@NSTouchBar))

(defun make-window-controller-class ()
  (let ((window-controller-class (objc-runtime::objc-allocate-class-pair #@NSWindowController "CluiWindowController" 0)))

    (objc-runtime::class-add-method window-controller-class @(makeTouchBar)
				    (cffi:callback window-controller-make-touch-bar-callback)
				    "v@:")
    
    (objc-runtime::class-add-method window-controller-class @(touchBar)
				    (cffi:callback window-controller-touch-bar-callback)
				    "v@:")

    (objc-runtime::class-add-method window-controller-class @(touchBar:makeItemForIdentifier:)
				    (cffi:callback window-controller-touch-bar-make-item-for-identifier-callback)
				    "v@:@@")
    (objc_registerClassPair window-controller-class)
    
    window-controller-class))



(deftraceable-callback application-delegate-application-should-terminate-callback :pointer
    ((self :pointer) (_cmd :pointer) (notification :pointer))
  ;;  (declare (ignorable self _cmd))
  (application-should-terminate notification))

(defun application-should-terminate (notification)
  (declare (ignorable notification))
  (values))

(deftraceable-callback application-delegate-application-did-change-screen-parameters-callback :void
    ((self :pointer) (_cmd :pointer) (notification :pointer))
;;  (declare (ignorable self _cmd))
  (application-did-change-screen-parameters notification)
  (values))

(defun application-did-change-screen-parameters (notification)
  (declare (ignorable notification))
  (values))

(deftraceable-callback application-delegate-application-will-finish-launching-callback :void
    ((self :pointer) (_cmd :pointer) (notification :pointer))
;;  (declare (ignorable self _cmd))
  (format t "~%application-will-finish-launching")
  (finish-output)
  (application-will-finish-launching notification)
  (values))

(defun application-will-finish-launching (notification)
  (declare (ignorable notification))
  (create-menu-bar)
  (values))

(deftraceable-callback application-delegate-application-did-finish-launching-callback :void
    ((self :pointer) (_cmd :pointer) (notification :pointer))
  ;;  (declare (ignorable self _cmd))
  (application-did-finish-launching self notification)
  (values))

(defmethod application-did-finish-launching (self notification)
  (declare (ignorable notification))
  (ns::|setActivationPolicy:| objc-runtime::ns-app NSApplicationActivationPolicyRegular)
  (values))



(deftraceable-callback application-delegate-application-did-hide-callback :void
    ((self :pointer) (_cmd :pointer) (notification :pointer))
  ;;  (declare (ignorable self _cmd))
  (application-did-hide notification)
  (values))

(defun application-did-hide (notification)
  (declare (ignorable notification))
  (values))



(defun make-application-delegate-class ()
  (let ((application-delegate-class (objc-runtime::objc-allocate-class-pair
				     #@NSObject "CluiApplicationDelegate" 0)))
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

    (objc_registerClassPair application-delegate-class)
    application-delegate-class))

(defconstant MAXPATHLEN 1024)

(cffi:defcfun (TISCopyCurrentKeyboardLayoutInputSource "TISCopyCurrentKeyboardLayoutInputSource") :pointer)
(cffi:defcfun (TISGetInputSourceProperty "TISGetInputSourceProperty") :pointer (source :pointer) (input-source-id :pointer))

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

	#+sbcl
	(sb-posix:chdir (cffi:foreign-string-to-lisp p-resources-path))

	#+ccl
	(setf (ccl:current-directory) (cffi:foreign-string-to-lisp p-resources-path))
	t))))

(defun create-menu-bar ()
  (let ((app-name "CLUI"))

    (unless app-name
      (let ((progname "" #+NIL(_NSGetProgname)))
	(if (not (string= progname ""))
	    (setq app-name progname)
	    (setq app-name "Clui Application"))))

    (let ((bar (alloc-init #@NSMenu)))
      (ns:|setMainMenu:| objc-runtime::ns-app bar)

      
      (let ((app-menu-item (ns:|addItemWithTitle:action:keyEquivalent:| bar (ns:|string| #@NSString)
			       (cffi:null-pointer) (ns:|string| #@NSString)))
	    (app-menu (alloc-init #@NSMenu)))
	
	(ns:|setSubmenu:| app-menu-item app-menu)
	(ns:|addItemWithTitle:action:keyEquivalent:| app-menu
	    (objc-runtime::make-nsstring (concatenate 'string "About " app-name))
	    @(orderFrontStandardAboutPanel:) (objc-runtime::make-nsstring ""))

	(ns:|addItem:| app-menu (ns:|separatorItem| #@NSMenuItem))

	(let ((services-menu (alloc-init #@NSMenu)))
	  (ns:|setServicesMenu:| objc-runtime::ns-app services-menu)
	  (ns:|addItemWithTitle:action:keyEquivalent:| app-menu
	      (objc-runtime::make-nsstring "Services") (cffi:null-pointer) (objc-runtime::make-nsstring ""))

	  (ns:|release| services-menu)
	  (ns:|addItem:| app-menu (ns:|separatorItem| #@NSMenuItem))

	  (ns:|addItemWithTitle:action:keyEquivalent:| app-menu
	      (objc-runtime::make-nsstring (concatenate 'string "Hide " app-name))
	      @(hide:) (objc-runtime::make-nsstring "h"))

	  (ns:|setKeyEquivalentModifierMask:| 
	      (ns:|addItemWithTitle:action:keyEquivalent:| app-menu
		  (objc-runtime::make-nsstring "Hide Others")
		  @(hideOtherApplications:) (objc-runtime::make-nsstring "h"))
	      (logior NSEventModifierFlagOption NSEventModifierFlagCommand))

	  (ns:|addItemWithTitle:action:keyEquivalent:| app-menu
	      (objc-runtime::make-nsstring "Show All")
	      @(unhideAllApplications:) (objc-runtime::make-nsstring ""))
	  
	  (ns:|addItem:| app-menu (ns:|separatorItem| #@NSMenuItem))

	  (ns:|addItemWithTitle:action:keyEquivalent:| app-menu
	      (objc-runtime::make-nsstring (concatenate 'string "Quit " app-name))
	      @(terminate:) (objc-runtime::make-nsstring "q"))

	  (ns:|addItemWithTitle:action:keyEquivalent:| bar
	      (objc-runtime::make-nsstring "") (cffi:null-pointer) (objc-runtime::make-nsstring ""))

	  (let ((window-menu-item (ns:|addItemWithTitle:action:keyEquivalent:| bar
				      (objc-runtime::make-nsstring "") (cffi:null-pointer)
				      (objc-runtime::make-nsstring "")))
		(window-menu (ns:|initWithTitle:| (alloc #@NSMenu) (objc-runtime::make-nsstring "Window"))))

	    (ns:|setWindowsMenu:| objc-runtime::ns-app window-menu)

	    (ns:|setSubmenu:| window-menu-item window-menu)

	    (ns:|addItemWithTitle:action:keyEquivalent:| window-menu
		(objc-runtime::make-nsstring "Minimize")
		@(performMiniaturize:)
		(objc-runtime::make-nsstring "m"))

	    (ns:|addItemWithTitle:action:keyEquivalent:| window-menu
		(objc-runtime::make-nsstring "Zoom")
		@(performZoom:) (objc-runtime::make-nsstring ""))

	    (ns:|addItem:| window-menu (ns:|separatorItem| #@NSMenuItem))

	    (ns:|addItemWithTitle:action:keyEquivalent:| window-menu
		(objc-runtime::make-nsstring "Bring All To Front")
		@(arrangeInFront:)
		(objc-runtime::make-nsstring ""))

	    (ns:|addItem:| window-menu (ns:|separatorItem| #@NSMenuItem))

	    (ns:|setKeyEquivalentModifierMask:|
		(ns:|addItemWithTitle:action:keyEquivalent:| window-menu
		    (objc-runtime::make-nsstring "Enter Full Screen")
		    @(toggleFullScreen:) (objc-runtime::make-nsstring "f"))
		(logior NSEventModifierFlagControl NSEventModifierFlagCommand))

	    (ns:|performSelector:withObject:| objc-runtime::ns-app
		@(setAppleMenu:) window-menu)

	    (ns:|release| bar)
	    
	    (values)))))))

(deftraceable-callback closure-like-thingy-named-block-callback :pointer ((event :pointer))
  (closure-like-thingy-named-block event)
  event)

(defun closure-like-thingy-named-block (event)
  (unless (logtest (ns::|modifierFlags| event) NSEventModifierFlagCommand)
    (ns::|sendEvent:| (ns::|keyWindow| objc-runtime::ns-app) event))
  event)
  
(defun init-cocoa (display)
  (with-autorelease-pool (pool)
    (flet ((pre-init-app ()
	     (setf (application-helper display) (alloc-init (objc-helper-class display)))
	   
	     (when (cffi:null-pointer-p (application-helper display))
	       (error "Cocoa: failed to create application helper."))
  
	     (setf (application-delegate display) (alloc-init (objc-application-delegate-class display)))

	     (when (cffi:null-pointer-p (application-delegate display))
	       (error "Cocoa: failed to create application delegate."))))

      (pre-init-app)

      (ns::|detachNewThreadSelector:toTarget:withObject:| #@NSThread @(doNothing:) (application-helper display) nil)

      (ns::|sharedApplication| #@NSApplication)

      (ns::|setDelegate:| display (application-delegate display))

      #+NIL
      (setf (key-up-monitor display)
	    (ns::|addLocalMonitorForEventsMatchingMask:handler:| #@NSEvent NSEventMaskKeyUp (cffi:callback closure-like-thingy-named-block-callback)))

      (change-to-resources-directory)

      (let ((defaults (make-dictionary (cffi:null-pointer) (objc-runtime::make-nsstring "ApplePressAndHoldEnabled")))
	    (NSTextInputContextKeyboardSelectionDidChangeNotification
	     (objc-runtime::make-nsstring
	      "NSTextInputContextKeyboardSelectionDidChangeNotification")))
    
	(ns::|registerDefaults:| (ns::|standardUserDefaults| #@NSUserDefaults) defaults)

	(ns::|addObserver:selector:name:object:|
	     (ns::|defaultCenter| #@NSNotificationCenter)
	     (application-helper display)
	     @(selectedKeyboardInputSourceChanged:)
	     NSTextInputContextKeyboardSelectionDidChangeNotification
	     nil)

	(create-cocoa-key-tables display)

	(setf (desktop-event-source display) (CGEventSourceCreate 0))

	(when (cffi:null-pointer-p (desktop-event-source display))
	  (return-from init-cocoa nil))

	(CGEventSourceSetLocalEventsSuppressionInterval (desktop-event-source display) 0.0d0)

	(unless (initialize-tis display)
	  (return-from init-cocoa nil))

	(setf (default-screen display) (make-instance 'screen :display display))

	(let ((helper-window
	       (setf (helper-window display)
		     (create-cocoa-helper-window display))))

	  (setf (window-content-view helper-window)
		(make-instance 'content-view
			       :ptr (alloc (objc-content-view-class display))
			       :owner helper-window
			       :marked-text (alloc-init #@NSMutableAttributedString)))

	  (ns:|setContentView:| helper-window (window-content-view helper-window))

	  (initialize-helper-window display helper-window))

	t))))

(defun update-unicode-data (display kPropertyUnicodeKeyLayoutData)
  (when (tis-input-source display)
    (CFRelease (tis-input-source display))
    (setf (tis-input-source display) nil)
    (setf (desktop-unicode-data display) nil))

  (let ((source (TISCopyCurrentKeyboardLayoutInputSource)))
  
    (when (cffi:null-pointer-p source)
      (error "Cocoa: Failed to retrieve keyboard layout input source."))
    
    (setf (tis-input-source display) source)

    (let ((data (TISGetInputSourceProperty source kPropertyUnicodeKeyLayoutData)))
      
      (when (cffi:null-pointer-p data)
	(error "Cocoa: Failed to retrieve keyboard layout unicode data."))
      
      (setf (desktop-unicode-data display) data)
      
      t)))



(defun initialize-TIS (display)
  (let ((tis-bundle (CFBundleGetBundleWithIdentifier
		     (CFSTR "com.apple.HIToolbox"))))

    (when (cffi:null-pointer-p tis-bundle)
      (error "Cocoa: Failed to load HIToolbox.framework"))

    (setf (tis-bundle display) tis-bundle)

    (setf (slot-value display 'kPropertyUnicodeKeyLayoutData)
	  (CFBundleGetDataPointerForName
	   tis-bundle (CFSTR "kTISPropertyUnicodeKeyLayoutData")))

    (setf (slot-value display 'TISCopyCurrentKeyboardLayoutInputSource)
	  (CFBundleGetDataPointerForName
	   tis-bundle (CFSTR "TISCopyCurrentKeyboardLayoutInputSource")))

    (setf (slot-value display 'TISGetInputSourceProperty)
	  (CFBundleGetDataPointerForName
	   tis-bundle (CFSTR "TISGetInputSourceProperty")))

    (setf (slot-value display 'LMGetKbdType)
	  (CFBundleGetDataPointerForName
	   tis-bundle (CFSTR "LMGetKbdType")))

    (when (or (cffi:null-pointer-p (slot-value display 'kPropertyUnicodeKeyLayoutData))
	      (cffi:null-pointer-p (slot-value display 'TISCopyCurrentKeyboardLayoutInputSource))
	      (cffi:null-pointer-p (slot-value display 'TISGetInputSourceProperty))
	      (cffi:null-pointer-p (slot-value display 'LMGetKbdType)))
      (error "Cocoa: Failed to load TIS API symbols."))

    (update-unicode-data display (cffi:mem-aref (slot-value display 'kPropertyUnicodeKeyLayoutData) :pointer))))

(defun poll-cocoa-events (display)
  (with-autorelease-pool (pool)
    (loop
       do
	 (let ((event (ns::|nextEventMatchingMask:untilDate:inMode:dequeue:|
			   display NSEventMaskAny
			   (ns::|distantPast| #@NSDate)
			   NSDefaultRunLoopMode t)))
	 
	   (when (cffi:null-pointer-p event)
	     (return (values)))

	   (ns::|sendEvent:| display event)))))

(defun cocoa-finish-init (display)
  (unless (ns::|isFinishedLaunching| (ns::|currentApplication| #@NSRunningApplication))
    (ns::|run| display)))

(defun wait-cocoa-events (display)
  (with-autorelease-pool (pool)
    (let ((event (ns::|nextEventMatchingMask:untilDate:inMode:dequeue:|
		      display NSEventMaskAny
		      (ns::|distantFuture| #@NSDate)
		      NSDefaultRunLoopMode t)))
	 
      (ns::|sendEvent:| display event))

    (poll-cocoa-events display)))

(defun main-loop-body (display)
  (wait-application-events display))

(defun test-main (display)
  (with-autorelease-pool (pool)
    (with-slots (exit?) display
      (tagbody
       test
	 (when exit?
	   (return-from test-main))
       start
	 (main-loop-body display)
	 (go test)))))

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
