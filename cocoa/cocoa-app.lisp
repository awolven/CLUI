(in-package :abstract-os)
(named-readtables:in-readtable :objc-readtable)

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
  #+NOTYET
  (update-unicode-data application))
  

(defun make-helper-class ()
  (let ((helper-class (objc-runtime::objc-allocate-class-pair #@NSObject "AbstractOSAppHelper" 0)))
    (objc-runtime::class-add-method helper-class @(doNothing:)
				    (cffi:callback application-helper-do-nothing-callback)
				    "v@:@")
    (objc-runtime::class-add-method helper-class @(selectedKeyboardInputSourceChanged:)
				    (cffi:callback application-helper-selected-keyboard-input-source-changed-callback)
				    "v@:@")
    #+NIL
    (objc-runtime::class-add-method helper-class @(observeValueForKeyPath:ofObject:change:context:)
				    (cffi:callback observe-value-for-key-path-callback)
				    "v@:@@@@")
    helper-class))

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
  #+NIL(create-menu-bar application)
  (values))

(deftraceable-callback application-delegate-application-did-finish-launching-callback :void
    ((self :pointer) (_cmd :pointer) (notification :pointer))
;;  (declare (ignorable self _cmd))
  (let ((application *app*))
    (when application
      (application-did-finish-launching application notification))
    (values)))

(defmethod application-did-finish-launching (application notification)
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
    #+NIL
    (objc-runtime::class-add-method application-delegate-class @(observeValueForKeyPath:ofObject:change:context:)
				    (cffi:callback observe-value-for-key-path-callback)
				    "v@:@@@@")
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

(defun create-menu-bar (app)
  (let ((app-name (application-name app)))

    (unless app-name
      (let ((progname "" #+NIL(_NSGetProgname)))
	(if (not (string= progname ""))
	    (setq app-name progname)
	    (setq app-name "Abstract OS Application"))))

    (let ((bar [[#@NSMenu @(alloc)] @(init)]))
      [(objc-object-id app) @(setMainMenu:) :pointer bar]

      (let ((app-menu-item [bar @(addItemWithTitle:action:keyEquivalent:) :pointer [#@NSString @(string)]
			   :pointer (cffi:null-pointer) :pointer [#@NSString @(string)]])
	    (app-menu [[#@NSMenu @(alloc)] @(init)]))

	[app-menu-item @(setSubmenu:) :pointer app-menu]
	[app-menu @(addItemWithTitle:action:keyEquivalent:)
	 :pointer (objc-runtime::make-nsstring (concatenate 'string "About " app-name))
	 :pointer @(orderFrontStandardAboutPanel:) :pointer (objc-runtime::make-nsstring "")]
	 [app-menu @(addItem:) :pointer [#@NSMenuItem @(separatorItem)]]

	 (let ((services-menu [[#@NSMenu @(alloc)] @(init)]))
	  [(objc-object-id app) @(setServicesMenu:) :pointer services-menu]
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
	    [(objc-object-id app) @(setWindowsMenu:) :pointer window-menu]
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

	     [(objc-object-id app) @(performSelector:withObject:) :pointer @(setAppleMenu:) :pointer app-menu]
	     [bar @(release)]
	     (values)))))))

(deftraceable-callback closure-like-thingy-named-block-callback :pointer ((event :pointer))
  (closure-like-thingy-named-block event)
  event)

(defun closure-like-thingy-named-block (event)
  (unless (zerop (logand (ns::|modifierFlags| event) NSEventModifierFlagCommand))
    (ns::|sendEvent:| (ns::|keyWindow| *app*) event))
  event)
  
(defun init-cocoa (app)
  (flet ((pre-init-app ()
	   (setf (application-window-list-head app) nil)
	   (setf (objc-helper-class app) (make-helper-class))
	   (setf (objc-application-delegate-class app) (make-application-delegate-class))
	   (setf (objc-window-class app) (make-window-class))
	   (setf (objc-window-delegate-class app) (make-window-delegate-class))
	   (setf (objc-content-view-class app) (make-content-view-class))

	   (setf (delegate->clos-window-table app) (make-hash-table :test #'eq))
	   (setf (content-view->clos-content-view-table app) (make-hash-table :test #'eq))
  
	   (setf (application-helper app) (alloc-init (objc-helper-class app)))

	   (when (cffi:null-pointer-p (application-helper app))
	     (error "Cocoa: failed to create application helper."))
  
	   (setf (application-delegate app) (alloc-init (objc-application-delegate-class app)))

	   (when (cffi:null-pointer-p (application-delegate app))
	     (error "Cocoa: failed to create application delegate."))))

    (pre-init-app)

    (ns::|detachNewThreadSelector:toTarget:withObject:| #@NSThread @(doNothing:) (application-helper app) nil)

    (ns::|sharedApplication| #@NSApplication)

    (ns::|setDelegate:| app (application-delegate app))

    (setf (key-up-monitor app)
	  (ns::|addLocalMonitorForEventsMatchingMask:handler:| #@NSEvent NSEventMaskKeyUp (cffi:callback closure-like-thingy-named-block-callback)))

    (change-to-resources-directory)

    (let ((defaults (make-dictionary (cffi:null-pointer) (objc-runtime::make-nsstring "ApplePressAndHoldEnabled")))
	  (NSTextInputContextKeyboardSelectionDidChangeNotification
	   (objc-runtime::make-nsstring
	    "NSTextInputContextKeyboardSelectionDidChangeNotification")))
    
      (ns::|registerDefaults:| (ns::|standardUserDefaults| #@NSUserDefaults) defaults)

      (ns::|addObserver:selector:name:object:|
	   (ns::|defaultCenter| #@NSNotificationCenter)
	   (application-helper app)
	   @(selectedKeyboardInputSourceChanged:)
	   NSTextInputContextKeyboardSelectionDidChangeNotification
	   nil)

      ;;(create-key-tables app)

      (setf (application-event-source app) (CGEventSourceCreate 0))

      (when (cffi:null-pointer-p (application-event-source app))
	(return-from init-cocoa nil))

      (CGEventSourceSetLocalEventsSuppressionInterval (application-event-source app) 0.0d0)

      #+NOTYET
      (unless (initialize-tis app)
	(return-from init-cocoa nil))

      ;;(poll-monitors-cocoa app)

      ;; this line of code is what is causing ns::|run| to crash on TouchBar Observer:
      ;;(ns::|setActivationPolicy:| app NSApplicationActivationPolicyRegular)

      t)))

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

(defun poll-cocoa-events (app)
  (with-autorelease-pool (pool)
    (loop
       do
	 (let ((event (ns::|nextEventMatchingMask:untilDate:inMode:dequeue:|
			   app NSEventMaskAny
			   (ns::|distantPast| #@NSDate)
			   NSDefaultRunLoopMode t)))
	 
	   (when (cffi:null-pointer-p event)
	     (return (values)))

	   (ns::|sendEvent:| app event)))))

(defun cocoa-finish-init (app)
  (unless (ns::|isFinishedLaunching| (ns::|currentApplication| #@NSRunningApplication))
    (ns::|run| app)))

(defun wait-cocoa-events (app)
  (with-autorelease-pool (pool)
    (let ((event (ns::|nextEventMatchingMask:untilDate:inMode:dequeue:|
		      app NSEventMaskAny
		      (ns::|distantFuture| #@NSDate)
		      NSDefaultRunLoopMode t)))
	 
      (ns::|sendEvent:| app event))

    (poll-cocoa-events app)))

(defun main-loop-body (app)
  (wait-application-events app))

(defun test-main (app)
  (with-autorelease-pool (pool)
    (with-slots (exit?) app
      (tagbody
       test
	 (when exit?
	   (return-from test-main))
       start
	 (main-loop-body app)
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
