(in-package :clui)
(named-readtables:in-readtable :objc-readtable)

(cffi:defctype ns::|NSUInteger| #+64-bit :unsigned-long-long #+32-bit :unsigned-int)
(cffi:defcstruct ns::|NSEdgeInsets|) ;;fixme

(cffi:defcstruct ns::|_NSRange|
		 (location ns::|NSUInteger|)
		 (length ns::|NSUInteger|))

(cffi:defcstruct ns::|_NSModalSession|) ;;fixme
(cffi:defcstruct NS::|_NSZone|)

(cffi:defcstruct ns::|OpaqueWindowPtr|) ;;fixme
(cffi:defcstruct NS::|__IOHIDEvent|) ;; fixme
(cffi:defcstruct NS::|pthread_override_s|)
(cffi:defcstruct NS::|ProcessSerialNumber|)
(cffi:defcstruct NS::__LSASN)
(cffi:defcstruct NS::|Object|)
(cffi:defcstruct NS::|objc_method_description|)
(cffi:defcstruct NS::|ValueInterpolator|)

(defvar *yes* 1)
(defvar *no* 0)

(defvar NSDefaultRunLoopMode (objc-runtime::make-nsstring "NSDefaultRunLoopMode"))
(cffi:defcfun ("_NSGetProgname" _NSGetProgname) :string)

(defconstant NSEventTypeKeyUp 11)
(defconstant NSEventMaskKeyUp (ash 1 NSEventTypeKeyUp))
(defconstant NSApplicationActivationPolicyRegular 0)

(defconstant NSEventModifierFlagControl (ash 1 18))
(defconstant NSEventModifierFlagOption (ash 1 19))
(defconstant NSEventModifierFlagCommand (ash 1 20))
(defconstant NSEventTypeApplicationDefined 15)

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

(defconstant NSViewLayerContentsRedrawOnSetNeedsDisplay 1)

(defconstant NSInformationalRequest 10)

(defmacro make-dictionary (&rest objc-values)
  (alexandria:with-gensyms (selector)
    `(let ((,selector (make-message-lambda @(dictionaryWithObjectsAndKeys:)
                          ((,@(mapcar (lambda (_) _ :pointer) objc-values) :pointer)
                           :pointer))))
       (funcall ,selector #@NSDictionary ,@objc-values (cffi:null-pointer)))))

(defun array-with-objects (&rest sequence)
  (let ((args (append
	       (loop for i from 0 below (length sequence)
		  append (list :pointer (elt sequence i)))
	       (list :pointer (cffi:null-pointer)))))
  (apply #'send #@NSArray @(arrayWithObjects:) :pointer args)))

(defun NS:|addLocalMonitorForEventsMatchingMask:handler:| (thing arg0 handler)
  (send (objc-object-id thing) @(addLocalMonitorForEventsMatchingMask:handler:) ':POINTER 
	':UNSIGNED-LONG-LONG arg0
	':unsigned-long-long 0
	':POINTER (objc-object-id handler)))



(defun super-init-with-frame (self frame)
  (let ((selector (make-super-message-lambda
		   @(initWithFrame:)
		   (((:struct ns::|CGRect|)) :pointer))))
    (funcall selector self frame)))

(defun super-responds-to-selector (self sel)
  (let ((selector (make-super-message-lambda
		   @(respondsToSelector:)
		   (((:pointer)) :char))))
    (funcall selector self sel)))

(defun super-resolve-class-method (self sel)
  (let ((selector (make-super-message-lambda
		   @(resolveClassMethod:)
		   (((:pointer)) :char))))
    (funcall selector self sel)))

(defun super-update-tracking-areas (self)
  (let ((selector (make-super-message-lambda
		   @(updateTrackingAreas) (nil :void))))
    (funcall selector self)))

(defun super-init-with-window (self window)
  (let ((selector (make-super-message-lambda @(initWithWindow:) ((:pointer) :pointer))))
    (funcall selector (objc-object-id self) (objc-object-id window))))
	    
				      

(defun NSRectFill (rect)
  (cffi:foreign-funcall "NSRectFill" (:struct ns::|CGRect|) rect :void))

(defun make-nsrect (x y width height)
  (list 'ns::height (coerce height 'double-float) 'ns::width (coerce width 'double-float)
	'ns::y (coerce y 'double-float) 'ns::x (coerce x 'double-float)))

(defun make-nspoint (x y)
  (list 'ns::y (coerce y 'double-float) 'ns::x (coerce x 'double-float)))

(defun make-nssize (width height)
  (list 'ns::width (coerce width 'double-float) 'ns::height (coerce height 'double-float)))

(cffi:defcfun (NSMouseInRect "NSMouseInRect") :char
  (point (:struct ns::|CGPoint|))
  (rect (:struct ns::|CGRect|))
  (flipped :char))

(defun ns-mouse-in-rect (point rect &optional (flipped? nil))
  (if (= 1 (NSMouseInRect point rect (if flipped? 1 0)))
      t
      nil))

(defun ns-get-x (ns-struct)
  (getf ns-struct 'ns::x))

(defun ns-get-y (ns-struct)
  (getf ns-struct 'ns::y))

(defun ns-get-width (ns-struct)
  (getf ns-struct 'ns::width))

(defun ns-get-height (ns-struct)
  (getf ns-struct 'ns::height))
  
