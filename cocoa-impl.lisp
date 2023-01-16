(in-package :abstract-os)
(named-readtables:in-readtable :objc-readtable)

(defmacro with-autorelease-pool ((var) &body body)
  `(let ((,var (ns::|new| #@NSAutoReleasePool)))
     (unwind-protect (progn ,@body)
       (ns::|release| ,var))))

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






(cffi:defcfun (set-uncaught-exception-handler "objc_setUncaughtExceptionHandler")
    :void
  (cb :pointer))

(set-uncaught-exception-handler (cffi:callback exception-handler))



(cffi:defcfun (class_getSuperclass "class_getSuperclass") :pointer (cls :pointer))





(cffi:defcfun (object_setInstanceVariable "object_setInstanceVariable") :pointer (object :pointer) (name :string) (value :pointer))

;;(cffi:defcfun (protocol_getProperty "object_getProperty"))

(cffi:defcfun (object_getClass "object_getClass") :pointer (object :pointer))








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

(deftraceable-callback closure-like-thingy-named-block-callback :pointer ((event :pointer))
  (closure-like-thingy-named-block event)
  event)

(defun closure-like-thingy-named-block (event)
  (unless (zerop (logand (ns::|modifierFlags| event) NSEventModifierFlagCommand))
    (ns::|sendEvent:| (ns::|keyWindow| *app*) event))
  event)
  

(defmacro make-dictionary (&rest objc-values)
  (alexandria:with-gensyms (selector)
    `(let ((,selector (make-message-lambda @(dictionaryWithObjectsAndKeys:)
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

(defun NS::|addLocalMonitorForEventsMatchingMask:handler:| (thing arg0 handler)
  (send (ns-object-ptr thing) @(addLocalMonitorForEventsMatchingMask:handler:) ':POINTER 
	':UNSIGNED-LONG-LONG arg0
	':unsigned-long-long 0
	':POINTER (ns-object-ptr handler)))

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

(defun super-init-with-frame (self frame)
  (let ((selector (new-msg-send-super @(initWithFrame:)
				      (((:struct ns::|CGRect|)) :pointer))))
    (funcall selector self frame)))


(cffi:defcfun (CGMainDisplayID "CGMainDisplayID") :unsigned-int)
(cffi:defcfun (CGDisplayBounds "CGDisplayBounds") (:struct ns::|CGRect|) (display :unsigned-int))
(cffi:defcfun (CGDisplayCopyDisplayMode "CGDisplayCopyDisplayMode") :pointer (display :unsigned-int))
(cffi:defcfun (CGDisplayModeGetWidth "CGDisplayModeGetWidth") :int64 (mode :pointer))
(cffi:defcfun (CGDisplayModeGetHeight "CGDisplayModeGetHeight") :int64 (mode :pointer))
(cffi:defcfun (CGDisplayModeGetRefreshRate "CGDisplayModeGetRefreshRate") :double (mode :pointer))
(cffi:defcfun (CGDisplayModeRelease "CGDisplayModeRelease") :void (mode :pointer))


(defun make-other-event-with-type (type point modifier-flags timestamp window-number context subtype
				   &optional (data1 0) (data2 0))
  (ns::|otherEventWithType:location:modifierFlags:timestamp:windowNumber:context:subtype:data1:data2:|
       #@NSEvent type point modifier-flags timestamp
       window-number (if context
			 (ns-object-ptr context)
			 nil)
       subtype data1 data2))

(defun post-empty-event (app)
  (let ((event (make-other-event-with-type
		  NSEventTypeApplicationDefined (list 'ns::y 0.0d0 'ns::x 0.0d0) 0 0.0d0 0 nil 0 0 0)))
      (ns::|postEvent:atStart:| app event t)))
		 

(deftraceable-callback content-view-dealloc-callback :void ((self :pointer) (_cmd :pointer))
;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       (content-view->clos-content-view-table *app*))))
    (when content-view
      (ns::|dealloc| content-view))
    (values)))

(deftraceable-callback content-view-is-opaque-callback :char ((self :pointer) (_cmd :pointer))
;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       (content-view->clos-content-view-table *app*))))
    (if content-view
	(content-view-is-opaque content-view)
	*yes*)))

(defun content-view-is-opaque (view)
  (let ((window (content-view-owner view)))
    (when window
      (cocoa-window-is-opaque window))))

(defmethod cocoa-window-is-opaque ((window essential-os-window-mixin))
  ;; write other methods for this generic function on other window classes
  (if (ns::|isOpaque| window)
      *yes*
      *no*))

(deftraceable-callback content-view-can-become-key-view-callback :char ((self :pointer) (_cmd :pointer))
;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       (content-view->clos-content-view-table *app*))))
    (when content-view
      (content-view-can-become-key-view content-view))))

(defun content-view-can-become-key-view (content-view)
  (cocoa-window-can-become-key (content-view-owner content-view)))

(defmethod cocoa-window-can-become-key ((window essential-os-window-mixin))
  *yes*)

(deftraceable-callback content-view-accepts-first-responder-callback :char ((self :pointer) (_cmd :pointer))
;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       (content-view->clos-content-view-table *app*))))
    (when content-view
      (content-view-accepts-first-responder content-view))))

(defun content-view-accepts-first-responder (view)
  (cocoa-window-accepts-first-responder (content-view-owner view)))

(defmethod cocoa-window-accepts-first-responder ((window essential-os-window-mixin))
  *yes*)

(deftraceable-callback content-view-wants-update-layer-callback :char ((self :pointer) (_cmd :pointer))
;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       (content-view->clos-content-view-table *app*))))
    (if content-view
      (content-view-wants-update-layer content-view)
      (progn (break)
      *no*))))

(defun content-view-wants-update-layer (view)
  ;; rather than making a separate content-view class for each window class and dispatching on content-view type
  ;; just dispatch on window type
  (cocoa-window-wants-update-layer (content-view-owner view)))

(defmethod cocoa-window-wants-update-layer ((window essential-os-window-mixin))
  ;; for example, a method for this generic function for a metal window would answer differently
  *no*)

(defmethod cocoa-window-wants-update-layer ((window constant-refresh-os-window-mixin))
  *yes*)

(deftraceable-callback content-view-update-layer-callback :void ((self :pointer) (_cmd :pointer))
  ;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       (content-view->clos-content-view-table *app*))))
    (when content-view
      (content-view-update-layer content-view))))

(defun content-view-update-layer (content-view)
  (cocoa-window-update-view-layer (content-view-owner content-view))
  (values))

(defmethod cocoa-window-update-view-layer ((window essential-os-window-mixin))
  (input-window-damage window))

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

(deftraceable-callback content-view-accepts-first-mouse-callback :bool ((self :pointer) (_cmd :pointer))
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
;;  (ns::|setNeedsDisplay:| content-view t)
  (input-mouse-clicked (content-view-owner content-view)
		     :mouse-button-left :press
		     (translate-flags (ns::|modifierFlags| event)))
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
  (ns::|mouseMoved:| content-view event)
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
		     (translate-flags (ns::|modifierFlags| event)))
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
	(let ((dx (- (ns::|deltaX| event) (cursor-warp-delta-x window)))
	      (dy (- (ns::|deltaY| event) (cursor-warp-delta-y window))))

	  (input-cursor-pos window
			    (+ (virtual-cursor-pos-x window) dx)
			    (+ (virtual-cursor-pos-y window) dy)))
	(let ((content-rect (ns::|frame| content-view))
	      (pos (ns::|locationInWindow| event)))

	  (input-cursor-pos window
			    (getf pos 'ns::x)
			    (- (getf content-rect 'ns::height) (getf pos 'ns::y)))))

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
		       (translate-flags (ns::|modifierFlags| event))))

(deftraceable-callback content-view-right-mouse-dragged-callback :void ((self :pointer) (_cmd :pointer) (event :pointer))
;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       (content-view->clos-content-view-table *app*))))
    (when content-view
      (content-view-right-mouse-dragged content-view event))
    (values)))

(defun content-view-right-mouse-dragged (content-view event)
  (ns::|mouseMoved:| content-view event)
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
		       (translate-flags (ns::|modifierFlags| event))))

(deftraceable-callback content-view-other-mouse-down-callback :void ((self :pointer) (_cmd :pointer) (event :pointer))
;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       (content-view->clos-content-view-table *app*))))
    (when content-view
      (content-view-other-mouse-down content-view event))
    (values)))

(defun content-view-other-mouse-down (content-view event)
  (ns::|toggleFullScreen:| (content-view-owner content-view) nil)
  (input-mouse-clicked (content-view-owner content-view)
		       (aref #(:mouse-button-right :mouse-button-left :mouse-button-middle)
			     (ns::|buttonNumber| event))
		       :press
		       (translate-flags (ns::|modifierFlags| event))))

(deftraceable-callback content-view-other-mouse-dragged-callback :void ((self :pointer) (_cmd :pointer) (event :pointer))
;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       (content-view->clos-content-view-table *app*))))
    (when content-view
      (content-view-other-mouse-dragged content-view event))
    (values)))

(defun content-view-other-mouse-dragged (content-view event)
  (ns::|mouseMoved:| content-view event)
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
			     (ns::|buttonNumber| event))
		       :release
		       (translate-flags (ns::|modifierFlags| event))))

(deftraceable-callback content-view-other-mouse-exited-callback :void ((self :pointer) (_cmd :pointer) (event :pointer))
;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       (content-view->clos-content-view-table *app*))))
    (when content-view
      (content-view-other-mouse-exited content-view event))
    (values)))

(defun content-view-other-mouse-exited (view event)
  (let ((window (content-view-owner view)))
    (on-cocoa-other-mouse-exited window event)))

(defmethod on-cocoa-other-mouse-exited ((window essential-os-window-mixin) event)
  (when (eq (window-cursor-mode window) :hidden)
    (show-cursor window))
  (input-cursor-enter window nil))
    

(deftraceable-callback content-view-other-mouse-entered-callback :void ((self :pointer) (_cmd :pointer) (event :pointer))
;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       (content-view->clos-content-view-table *app*))))
    (when content-view
      (content-view-other-mouse-entered content-view event))
    (values)))

(defun content-view-other-mouse-entered (view event)

  (let ((window (content-view-owner view)))
    (on-cocoa-other-mouse-entered window event)))

(defmethod on-cocoa-other-mouse-entered ((window essential-os-window-mixin) event)
  (declare (ignore event))
  (when (eq (window-cursor-mode window) :hidden)
    (hide-cursor window))
  (input-cursor-enter window t))

(deftraceable-callback content-view-view-did-change-backing-properties-callback :void ((self :pointer) (_cmd :pointer))
;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       (content-view->clos-content-view-table *app*))))
    (when content-view
      (content-view-view-did-change-backing-properties content-view))
    (values)))

(defun content-view-view-did-change-backing-properties (view)
  (cocoa-window-did-change-backing-properties (content-view-owner view)))

(defmethod cocoa-window-did-change-backing-properties ((window essential-os-window-mixin))
  (values))

(defmethod cocoa-window-did-change-backing-properties ((window constant-refresh-os-window-mixin))
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
      (content-view-draw-rect (content-view-owner content-view) content-view rect))
    (values)))

(defmethod content-view-draw-rect ((window essential-os-window-mixin) (view content-view) rect)
  (declare (ignorable rect))
  (format t "~%running draw-rect")
  (finish-output)
  (ns::|set| (ns::|whiteColor| #@NSColor))
  (ns::|strokeRect:| #@NSBezierPath (make-ns-rect 0 0 1 1))
  (NSRectFill (ns::|bounds| view))
  (ns::|flushGraphics| (window-graphics-context (content-view-owner view)))
  (input-window-damaged (content-view-owner view)))

(defmethod content-view-draw-rect ((window constant-refresh-os-window-mixin) (view content-view) rect)
  (declare (ignorable view))
  (input-window-damaged (content-view-owner view))
  (values))

(deftraceable-callback content-view-update-tracking-areas-callback :void ((self :pointer) (_cmd :pointer))
;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       (content-view->clos-content-view-table *app*))))
    (when content-view
      (content-view-update-tracking-areas content-view))
    (values)))

(defun content-view-update-tracking-areas (view)
  (cocoa-window-update-tracking-areas (content-view-owner view)))

(defmethod cocoa-window-update-tracking-areas ((window essential-os-window-mixin))
  #+NIL
  (let ((content-view (window-content-view window)))
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
	(super-msg-send content-view @(updateTrackingAreas)))))
  (values))


  

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

(defun wait-cocoa-events (app)
  (with-autorelease-pool (pool)
    (let ((event (ns::|nextEventMatchingMask:untilDate:inMode:dequeue:|
		      app NSEventMaskAny
		      (ns::|distantFuture| #@NSDate)
		      NSDefaultRunLoopMode t)))
	 
      (ns::|sendEvent:| app event))

    (poll-cocoa-events app)))
    
		    



#+sbcl
(defmethod ns-object-ptr ((thing sb-sys:system-area-pointer))
  thing)

#+ccl
(defmethod ns-object-ptr ((thing ccl::macptr))
  thing)

(defmethod ns-object-ptr ((thing null))
  (cffi:null-pointer))

(defun titled-style? (window)
  (logtest NSWindowStyleMaskTitled (ns::|styleMask| window)))

(defun closable-style? (window)
  (logtest NSWindowStyleMaskClosable (ns::|styleMask| window)))

(defun miniaturizable-style? (window)
  (logtest NSWindowStyleMaskMiniaturizable (ns::|styleMask| window)))

(defun resizable-style? (window)
  (logtest NSWindowStyleMaskResizable (ns::|styleMask| window)))

(defun get-cocoa-monitor-pos (monitor)
  (let ((bounds (CGDisplayBounds (monitor-display-id monitor))))
    (values (getf bounds 'ns::x)
	    (getf bounds 'ns::y))))


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
  (coerce (1- (- (getf (CGDisplayBounds (CGMainDisplayID)) 'ns::height) y)) 'double-float))

(defun (setf cocoa-window-title) (string window)
  (ns::|setTitle:| window (objc-runtime::make-nsstring string))
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
  (with-autorelease-pool (pool)
    (let* ((content-rect (ns::|frame| (window-content-view window))))
      (let ((fb-rect (ns::|convertRectToBacking:| (window-content-view window) content-rect)))
	(values (getf fb-rect 'ns::width) (getf fb-rect 'ns::height))))))

(defun get-cocoa-window-pos (window)
  (declare (ignorable window))
  )

(defun get-cocoa-window-size (window)
  (with-autorelease-pool (pool)
    (let ((frame (ns::|frame| (window-content-view window))))
      (values (getf frame 'ns::width) (getf frame 'ns::height)))))

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


(defun ff-call (name return-type &rest args)
  (multiple-value-bind (types ctypes fargs rettype)
      (cffi::parse-args-and-types (append args (list return-type)))
    (let ((syms (cffi::make-gensym-list (length types))))
      (eval 
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



(defun alloc-init (objc-class)
  (ns::|init| (alloc objc-class)))

(defun alloc (objc-class)
  (ns::|alloc| objc-class))

(defun init (objc-object)
  (ns::|init| objc-object))

(defun make-ns-rect (x y width height)
  (list 'ns::height (coerce height 'double-float) 'ns::width (coerce width 'double-float)
	'ns::y (coerce y 'double-float) 'ns::x (coerce x 'double-float)))

(defun make-ns-point (x y)
  (list 'ns::y (coerce y 'double-float) 'ns::x (coerce x 'double-float)))

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
  
  (let ((content-rect))
    (if (window-monitor window)
	
	(multiple-value-bind (xpos ypos) (get-cocoa-monitor-pos (window-monitor window))
	  (let* ((mode (get-cocoa-video-mode (window-monitor window)))
		 (width (video-mode-width mode))
		 (height (video-mode-height mode)))

	    (setq content-rect (make-ns-rect xpos ypos width height))))

	(if (or (null xpos) (null ypos))
	    (setq content-rect (make-ns-rect 0 0 (or width 640) (or height 480)))

	    (setq content-rect (make-ns-rect xpos
					     (cocoa-transform-y (1- (+ ypos (or height 480.0d0))))
					     (or width 640) (or height 480)))))

    (let ((style-mask NSWindowStyleMaskMiniaturizable))

      (if (or (window-monitor window) (not (decorated? window)))

	  (progn
	    (setq style-mask (logior style-mask NSWindowStyleMaskBorderless)))
	  
	  (progn
	    (setq style-mask (logior style-mask NSWindowStyleMaskTitled NSWindowStyleMaskClosable))
 	    
	    (when (resizable? window)
	      (setq style-mask (logior style-mask NSWindowStyleMaskResizable)))))

      (setf (ns-object-ptr window)
	    (NS::|initWithContentRect:styleMask:backing:defer:|
		 (alloc (objc-window-class *app*))
		 content-rect
		 #+NIL(logior NSWindowStyleMaskTitled
			     NSWindowStyleMaskClosable
			     NSWindowStyleMaskMiniaturizable
			     NSWindowStyleMaskResizable)
		 style-mask
		 NSBackingStoreBuffered nil))
	
      (when (cffi:null-pointer-p (ns-object-ptr window))
	(error "Cocoa: Failed to create window."))

      (setf (cocoa-window-title window) (or title "Abstract OS"))
      
      (setf (window-delegate window)
	    (make-instance 'window-delegate
			   :ptr (alloc-init (objc-window-delegate-class *app*))
			   :owner window))

      (ns::|setDelegate:| window (window-delegate window))
	  
      (setf (window-content-view window)
	    (make-instance 'content-view
			   :ptr (alloc (objc-content-view-class *app*))
			   :owner window
			   :marked-text (alloc-init #@NSMutableAttributedString)))



      (super-init-with-frame (window-content-view window) (ns::|frame| window))
      (ns::|updateTrackingAreas| (window-content-view window))

      (ns::|setContentView:| window (window-content-view window))
      (ns::|makeFirstResponder:| window (window-content-view window))

      ;; these can be called once we have a content view
      (multiple-value-bind (width height)
	  (get-cocoa-window-size window)
	(setf (width window) width
	      (height window) height))

      (multiple-value-bind (fb-width fb-height)
	  (get-cocoa-window-framebuffer-size window)
	(setf (window-fb-width window) fb-width
	      (window-fb-height window) fb-height))

      (ns::|registerForDraggedTypes:| (window-content-view window) (array-with-objects (objc-runtime::make-nsstring "NSPasteboardTypedURL")))
	
      (if (window-monitor window)

	  (ns::|setLevel:| window (1+ NSMainMenuWindowLevel))
	    
	  (progn
	      
	    (when (or (null xpos) (null ypos))
	      #+NIL
	      (setf (cascade-point *app*)
		    (ns::|cascadeTopLeftFromPoint:| window (cascade-point *app*))))

	    (let ((behavior (if resizable?
				(logior NSWindowCollectionBehaviorFullScreenPrimary
					NSWindowCollectionBehaviorManaged)
				NSWindowCollectionBehaviorFullScreenNone)))
		  
	      (ns::|setCollectionBehavior:| window behavior))

	    (when floating?
	      (ns::|setLevel:| window NSFloatingWindowLevel))
	      
	    (when maximized?
	      (ns::|zoom:| window t))))

      (when (and frame-name (not (string= frame-name "")))
	(ns::|setFrameAutosaveName:| window (objc-runtime::make-nsstring frame-name)))
	  
      (setf (window-retina? window) retina?)

      (when transparent?
	(ns::|setOpaque:| window t)
	(ns::|setHasShadow:| window t)
	(ns::|setBackgroundColor:| window (ns::|clearColor| #@NScolor)))

      (ns::|setAcceptsMouseMovedEvents:| window t)

      (ns::|setRestorable:| window nil)

      

      (setf (window-graphics-context window)
	    (ns::|graphicsContextWithWindow:| #@NSGraphicsContext window))

      t)))

	    

(defun NSRectFill (rect)
  (cffi:foreign-funcall "NSRectFill" (:struct ns::|CGRect|) rect :void))



    

(defun create-cocoa-window (window &rest initargs
			    &key (visible? t)
			      (focused? t)
			      (auto-iconify? t)
			      (focus-on-show? t)
			      (center-cursor? t)
			      (mouse-passthrough? nil)
			      &allow-other-keys)
  (declare (ignorable auto-iconify? focus-on-show?))
  
  (with-autorelease-pool (pool)
  
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
    t))

(defun show-cocoa-window (window)
  (ns::|setIsVisible:| window t)
  
  #+NIL
  (with-autorelease-pool (pool)
    (ns::|orderFront:| window (cffi:null-pointer))))

(defun hide-cocoa-window (window)
  (with-autorelease-pool (pool)
    (ns::|orderOut:| window (cffi:null-pointer))))

(defun focus-cocoa-window (window)
  (with-autorelease-pool (pool)
    ;;(ns::|activateIgnoringOtherApps:| *app* t)
    (ns::|makeKeyAndOrderFront:| window (cffi:null-pointer))))
	      
(defun set-cocoa-video-mode (monitor video-mode)
  (declare (ignorable monitor video-mode))
  (values))

(defun acquire-monitor (window)
  (set-cocoa-video-mode (window-monitor window) (window-video-mode window))
  (let ((bounds (CGDisplayBounds (monitor-display-id (window-monitor window)))))
    (declare (ignorable bounds))
    #+NIL
    (with-nsrect (frame :x (getf bounds 'ns::x)
			:y (cocoa-transform-y (1- (+ (getf bounds 'ns::y) (getf bounds 'height))))
			:width (getf bounds 'ns::width)
			:height (getf bounds 'ns::height))
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
    #+NIL
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
  (let ((maximized? (ns::|isZoomed| window)))
    (unless (eq (maximized? window) maximized?)
      (setf (maximized? window) maximized?)
      (input-window-maximize window maximized?)))
  (let* ((content-rect (ns::|frame| (window-content-view window))))
    (let ((fb-rect (ns::|convertRectToBacking:| (window-content-view window) content-rect)))
      (let ((fb-width (getf fb-rect 'ns::width))
	    (fb-height (getf fb-rect 'ns::height)))
	(when (or (/= fb-width (window-fb-width window))
		  (/= fb-height (window-fb-height window)))
	  (setf (window-fb-width window) fb-width)
	  (setf (window-fb-height window) fb-height)
	  (input-framebuffer-size window fb-width fb-height)))
      (when (or (/= (getf content-rect 'ns::width) (width window))
		(/= (getf content-rect 'ns::height) (height window)))
	(setf (width window) (getf content-rect 'ns::width)
	      (height window) (getf content-rect 'ns::height))
	(input-window-size window (getf content-rect 'ns::width) (getf content-rect 'ns::height))))))

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
  #+NOTYET(acquire-monitor window)
  (values))

(deftraceable-callback window-delegate-window-did-become-key-callback :void ((self :pointer) (notification :pointer))
;;  (declare (ignore notification))
  (let ((window (gethash (sap-int self) (delegate->clos-window-table *app*))))
    (when window
      (window-did-become-key window))
    (values)))

(defun window-did-become-key (window)
  (declare (ignorable window))
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
    (objc-runtime::class-add-method content-view-class @(cursorUpdate:)
				    (cffi:callback content-view-cursor-update-callback)
				    "v@:@")
    (objc-runtime::class-add-method content-view-class @(acceptsFirstMouse)
				    (cffi:callback content-view-accepts-first-mouse-callback)
				    "c@:")
    (objc-runtime::class-add-method content-view-class @(mouseDown:)
				    (cffi:callback content-view-mouse-down-callback)
				    "v@:@")
    (objc-runtime::class-add-method content-view-class @(mouseDragged:)
				    (cffi:callback content-view-mouse-dragged-callback)
				    "v@:@")
    (objc-runtime::class-add-method content-view-class @(mouseUp:)
				    (cffi:callback content-view-mouse-up-callback)
				    "v@:@")
    (objc-runtime::class-add-method content-view-class @(mouseMoved:)
				    (cffi:callback content-view-mouse-moved-callback)
				    "v@:@")
    (objc-runtime::class-add-method content-view-class @(rightMouseDown:)
				    (cffi:callback content-view-right-mouse-down-callback)
				    "v@:@")
    (objc-runtime::class-add-method content-view-class @(rightMouseDragged:)
				    (cffi:callback content-view-right-mouse-dragged-callback)
				    "v@:@")
    (objc-runtime::class-add-method content-view-class @(rightMouseUp:)
				    (cffi:callback content-view-right-mouse-up-callback)
				    "v@:@")
    (objc-runtime::class-add-method content-view-class @(otherMouseDown:)
				    (cffi:callback content-view-other-mouse-down-callback)
				    "v@:@")
    (objc-runtime::class-add-method content-view-class @(otherMouseDragged:)
				    (cffi:callback content-view-other-mouse-dragged-callback)
				    "v@:@")
    (objc-runtime::class-add-method content-view-class @(otherMouseUp:)
				    (cffi:callback content-view-other-mouse-up-callback)
				    "v@:@")
    (objc-runtime::class-add-method content-view-class @(otherMouseExited:)
				    (cffi:callback content-view-other-mouse-exited-callback)
				    "v@:@")
    (objc-runtime::class-add-method content-view-class @(otherMouseEntered:)
				    (cffi:callback content-view-other-mouse-entered-callback)
				    "v@:@")
    (objc-runtime::class-add-method content-view-class @(didChangeBackingProperties)
				    (cffi:callback content-view-view-did-change-backing-properties-callback)
				    "v@:")
    (objc-runtime::class-add-method content-view-class @(drawRect:)
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
    #+NIL
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

(defun test ()
  #+sbcl
  (sb-int:set-floating-point-modes :traps '())
  (ns::|run| *app*))

(defun cocoa-finish-init (app)
  (unless (ns::|isFinishedLaunching| (ns::|currentApplication| #@NSRunningApplication))
    (ns::|run| app)))

(defun small-test ()
  (sb-int:set-floating-point-modes :traps '())
  (print objc-runtime::ns-app)
  (with-autorelease-pool (pool)
    (let ((window (ns::|initWithContentRect:styleMask:backing:defer:|
		       [#@NSWindow @(alloc)]
		       (make-ns-rect 0 0 300 300)
		       (logior NSWindowStyleMaskTitled
			       NSWindowStyleMaskClosable
			       NSWindowStyleMaskMiniaturizable
			       NSWindowStyleMaskResizable)
		       NSBackingStoreBuffered nil)))
      (print objc-runtime::ns-app)
      (ns::|setTitle:| window (objc-runtime::make-nsstring "Look! A Window"))
      (print objc-runtime::ns-app)
      (ns::|setIsVisible:| window t)
      (print objc-runtime::ns-app)
      (ns::|makeKeyAndOrderFront:| window nil)
      (print objc-runtime::ns-app)
      (ns::|run| objc-runtime::ns-app)
      window)))

#|
   NSWindow *win = [[NSWindow alloc]
                    initWithContentRect:NSMakeRect(0, 0, 300, 300)
                              styleMask:( NSWindowStyleMaskTitled |
                                          NSWindowStyleMaskClosable |
                                          NSWindowStyleMaskMiniaturizable |
                                          NSWindowStyleMaskResizable)
                                backing:NSBackingStoreBuffered
                                  defer:NO];
  [win setTitle:@"Look! A Window"];
  [win setIsVisible:YES];
  [win makeKeyAndOrderFront: NULL];
  [NSApp run];
  return 0;
|#


     
