(in-package :abstract-os)
(named-readtables:in-readtable :objc-readtable)

(cffi:defcallback exception-handler :void ((exception :pointer))
  (objc-runtime::with-selectors (reason)
    (error "~&objc exception: ~a~%" [exception reason])))

(defun sap-int (sap)
  #+sbcl(sb-sys:sap-int sap)
  #+ccl(ccl::%ptr-to-int sap))

(cffi:defcfun (set-uncaught-exception-handler "objc_setUncaughtExceptionHandler")
    :void
  (cb :pointer))

(set-uncaught-exception-handler (cffi:callback exception-handler))

(cffi:defcfun (class_copyIvarList "class_copyIvarList") :pointer (cls :pointer) (out-count :pointer))

(cffi:defcfun (class_copyMethodList "class_copyMethodList") :pointer (cls :pointer) (out-count :pointer))

(cffi:defcfun (ivar_getName "ivar_getName") :string (ivar :pointer))

(cffi:defcfun (method_getName "method_getName") :pointer (method :pointer))

(defun class-copy-ivar-list (class)
  (cffi:with-foreign-object (count :unsigned-int)
    (let ((list (class_copyIvarList class count)))
      (loop for i from 0 below (cffi:mem-aref count :unsigned-int)
	 collect (ivar_getName (cffi:mem-aref list :pointer i))))))

(defun class-copy-method-list (class)
  (cffi:with-foreign-object (count :unsigned-int)
    (let ((list (class_copyMethodList class count)))
      (loop for i from 0 below (cffi:mem-aref count :unsigned-int)
	 collect (let ((sel (method_getName (cffi:mem-aref list :pointer i))))
	      (objc-runtime::sel-get-name sel))))))


(cffi:defcfun (objc_msgSendSuper "objc_msgSendSuper") :pointer (obj :pointer) (selector :pointer))

(cffi:defcstruct objc_super
  (reciever :pointer)
  (super_class :pointer))

(defun super-update-tracking-areas (content-view)
  (cffi:with-foreign-object (objc-super '(:struct objc_super))
    (setf (cffi:foreign-slot-value objc-super '(:struct objc_super) 'reciever) (ns-object-ptr content-view)
	  (cffi:foreign-slot-value objc-super '(:struct objc_super) 'super_class) #@NSView)
    (objc_msgSendSuper objc-super @(updateTrackingAreas))))

    

(defconstant NSWindowStyleMaskMiniaturizable (ash 1 2))
(defconstant NSWindowStyleMaskBorderless 0)
(defconstant NSWindowStyleMaskTitled (ash 1 0))
(defconstant NSWindowStyleMaskClosable (ash 1 1))
(defconstant NSWindowStyleMaskResizable (ash 1 3))
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

(cffi:defcallback content-view-dealloc-callback :void ((self :pointer) (_cmd :pointer))
  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       (content-view->clos-content-view-table *app*))))
    (when content-view
      (content-view-dealloc content-view))))

(defun content-view-dealloc (content-view)
  (declare (ignorable content-view))
  ;; todo
  )
  

(cffi:defcallback content-view-is-opaque-callback :boolean ((self :pointer) (_cmd :pointer))
  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       (content-view->clos-content-view-table *app*))))
    (when content-view
      (content-view-is-opaque content-view))))

(defun content-view-is-opaque (content-view) 
  (let ((window (content-view-owner content-view)))
    (when window
      [(ns-object-ptr window) @(isOpaque)])))

(cffi:defcallback content-view-can-become-key-view-callback :boolean ((self :pointer) (_cmd :pointer))
  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       (content-view->clos-content-view-table *app*))))
    (when content-view
      (content-view-can-become-key-view content-view))))

(defun content-view-can-become-key-view (content-view)
  (declare (ignore content-view))
  t)

(cffi:defcallback content-view-accepts-first-responder-callback :boolean ((self :pointer) (_cmd :pointer))
  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       (content-view->clos-content-view-table *app*))))
    (when content-view
      (content-view-accepts-first-responder content-view))))

(defun content-view-accepts-first-responder (content-view)
  (declare (ignore content-view))
  t)

(cffi:defcallback content-view-wants-update-layer-callback :boolean ((self :pointer) (_cmd :pointer))
  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       (content-view->clos-content-view-table *app*))))
    (when content-view
      (content-view-wants-update-layer content-view))))

(defun content-view-wants-update-layer (content-view)
  (declare (ignore content-view))
  t)

(cffi:defcallback content-view-update-layer-callback :void ((self :pointer) (_cmd :pointer))
  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       (content-view->clos-content-view-table *app*))))
    (when content-view
      (content-view-update-layer content-view))))

(defun content-view-update-layer (content-view)
  (input-window-damage (content-view-owner content-view))
  (values))

(cffi:defcallback content-view-cursor-update-callback :void ((self :pointer) (_cmd :pointer) (event :pointer))
  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       (content-view->clos-content-view-table *app*))))
    (when content-view
      (content-view-cursor-update content-view event))))

(defun content-view-cursor-update (content-view event)
  (declare (ignore event))
  (update-cursor-image (content-view-owner content-view))
  (values))

(defun update-cursor-image (window)
  (declare (ignore window))
  (values))

(cffi:defcallback content-view-accepts-first-mouse-callback :boolean ((self :pointer) (_cmd :pointer))
  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       (content-view->clos-content-view-table *app*))))
    (when content-view
      (content-view-accepts-first-mouse content-view))))

(defun content-view-accepts-first-mouse (content-view)
  (declare (ignore content-view))
  t)

(cffi:defcallback content-view-mouse-down-callback :void ((self :pointer) (_cmd :pointer) (event :pointer))
  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       (content-view->clos-content-view-table *app*))))
    (when content-view
      (content-view-mouse-down content-view event))))

(defun content-view-mouse-down (content-view event)
  (input-mouse-clicked (content-view-owner content-view)
		     :mouse-button-left :press
		     (translate-flags [event @(modifierFlags)]))
  (values))

(defun translate-flags (event-modifier-flags)
  event-modifier-flags)

(cffi:defcallback content-view-mouse-dragged-callback :void ((self :pointer) (_cmd :pointer) (event :pointer))
  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       (content-view->clos-content-view-table *app*))))
    (when content-view
      (content-view-mouse-dragged content-view event))))

(defun content-view-mouse-dragged (content-view event)
  [(ns-object-ptr content-view) @(mouseMoved) :pointer event]
  (values))

(cffi:defcallback content-view-mouse-up-callback :void ((self :pointer) (_cmd :pointer) (event :pointer))
  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       (content-view->clos-content-view-table *app*))))
    (when content-view
      (content-view-mouse-up content-view event))))

(defun content-view-mouse-up (content-view event)
  (input-mouse-clicked (content-view-owner content-view)
		     :mouse-button-left :release
		     (translate-flags [event @(modifierFlags)]))
  (values))

(cffi:defcallback content-view-mouse-moved-callback :void ((self :pointer) (_cmd :pointer) (event :pointer))
  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       (content-view->clos-content-view-table *app*))))
    (when content-view
      (content-view-mouse-moved content-view event))))



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

(cffi:defcallback content-view-right-mouse-down-callback :void ((self :pointer) (_cmd :pointer) (event :pointer))
  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       (content-view->clos-content-view-table *app*))))
    (when content-view
      (content-view-right-mouse-down content-view event))))

(defun content-view-right-mouse-down (content-view event)
  (input-mouse-clicked (content-view-owner content-view)
		       :mouse-button-right
		       :press
		       (translate-flags [event @(modifierFlags)])))

(cffi:defcallback content-view-right-mouse-dragged-callback :void ((self :pointer) (_cmd :pointer) (event :pointer))
  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       (content-view->clos-content-view-table *app*))))
    (when content-view
      (content-view-right-mouse-dragged content-view event))))

(defun content-view-right-mouse-dragged (content-view event)
  [(ns-object-ptr content-view) @(mouseMoved) :pointer event])
  
(cffi:defcallback content-view-right-mouse-up-callback :void ((self :pointer) (_cmd :pointer) (event :pointer))
  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       (content-view->clos-content-view-table *app*))))
    (when content-view
      (content-view-right-mouse-up content-view event))))

(defun content-view-right-mouse-up (content-view event)
  (input-mouse-clicked (content-view-owner content-view)
		       :mouse-button-right
		       :release
		       (translate-flags [event @(modifierFlags)])))

(cffi:defcallback content-view-other-mouse-down-callback :void ((self :pointer) (_cmd :pointer) (event :pointer))
  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       (content-view->clos-content-view-table *app*))))
    (when content-view
      (content-view-other-mouse-down content-view event))))

(defun content-view-other-mouse-down (content-view event)
  (input-mouse-clicked (content-view-owner content-view)
		       (aref #(:mouse-button-right :mouse-button-left :mouse-button-middle)
			     (cffi:mem-aref [event @(buttonNumber)] :int))
		       :press
		       (translate-flags [event @(modifierFlags)])))

(cffi:defcallback content-view-other-mouse-dragged-callback :void ((self :pointer) (_cmd :pointer) (event :pointer))
  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       (content-view->clos-content-view-table *app*))))
    (when content-view
      (content-view-other-mouse-dragged content-view event))))

(defun content-view-other-mouse-dragged (content-view event)
  [(ns-object-ptr content-view) @(mouseMoved) :pointer event])
  
(cffi:defcallback content-view-other-mouse-up-callback :void ((self :pointer) (_cmd :pointer) (event :pointer))
  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       (content-view->clos-content-view-table *app*))))
    (when content-view
      (content-view-other-mouse-up content-view event))))

(defun content-view-other-mouse-up (content-view event)
  (input-mouse-clicked (content-view-owner content-view)
		       (aref #(:mouse-button-right :mouse-button-left :mouse-button-middle)
			     (cffi:mem-aref [event @(buttonNumber)] :int))
		       :release
		       (translate-flags [event @(modifierFlags)])))

(cffi:defcallback content-view-other-mouse-exited-callback :void ((self :pointer) (_cmd :pointer) (event :pointer))
  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       (content-view->clos-content-view-table *app*))))
    (when content-view
      (content-view-other-mouse-exited content-view event))))

(defun content-view-other-mouse-exited (content-view event)
  (declare (ignore event))
  (let ((window (content-view-owner content-view)))
    (when (eq (window-cursor-mode window) :hidden)
      (show-cursor window))
    (input-cursor-enter window nil)))

(cffi:defcallback content-view-other-mouse-entered-callback :void ((self :pointer) (_cmd :pointer) (event :pointer))
  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       (content-view->clos-content-view-table *app*))))
    (when content-view
      (content-view-other-mouse-entered content-view event))))

(defun content-view-other-mouse-entered (content-view event)
  (declare (ignore event))
  (let ((window (content-view-owner content-view)))
    (when (eq (window-cursor-mode window) :hidden)
      (hide-cursor window))
    (input-cursor-enter window t)))

(cffi:defcallback content-view-view-did-change-backing-properties-callback :void ((self :pointer) (_cmd :pointer))
  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       (content-view->clos-content-view-table *app*))))
    (when content-view
      (content-view-view-did-change-backing-properties content-view))))

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

(cffi:defcallback content-view-draw-rect-callback :void ((self :pointer) (_cmd :pointer) (rect :pointer))
  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       (content-view->clos-content-view-table *app*))))
    (when content-view
      (content-view-draw-rect content-view rect))))

(defun content-view-draw-rect (content-view rect)
  (declare (ignorable rect))
  (input-window-damaged (content-view-owner content-view)))

(cffi:defcallback content-view-update-tracking-areas-callback :void ((self :pointer) (_cmd :pointer))
  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       (content-view->clos-content-view-table *app*))))
    (when content-view
      (content-view-update-tracking-areas content-view))))

(defun content-view-update-tracking-areas (content-view)
  (declare (ignorable content-view))
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
      (super-update-tracking-areas content-view)
      (values))))

#+sbcl
(defmacro new-msg-send (selector ((&rest arg-types) return-type))
  (let ((arg-syms (mapcar (lambda (_) _ (gensym))
                          arg-types)))
    `(lambda ,(cons 'target arg-syms)
       (cffi:foreign-funcall "objc_msgSend"
                             :pointer target
                             :pointer ,selector
                             ,@(mapcan #'list arg-types arg-syms)
                             ,return-type))))

#+CCL
(defmacro new-msg-send (selector ((&rest arg-types) return-type))
  (let ((arg-syms (mapcar (lambda (_) _ (gensym))
                          arg-types)))
    `(lambda ,(cons 'target arg-syms)
       (ccl::%ff-call (CCL:%REFERENCE-EXTERNAL-ENTRY-POINT
		       (LOAD-TIME-VALUE (CCL:EXTERNAL "objc_msgSend")))
			  :address target
			  :address ,selector
			  ,@(mapcan #'list arg-types arg-syms)
			  ,return-type))))

#+SBCL
(defmacro new-msg-send-stret (selector ((&rest arg-types) return-type))
  (let ((arg-syms (mapcar (lambda (_) _ (gensym))
                          arg-types)))
    `(lambda ,(cons 'target arg-syms)
       (cffi:foreign-funcall "objc_msgSend_stret"
			     :pointer target
			     :pointer ,selector
			     ,@(mapcan #'list arg-types arg-syms)
			     ,return-type))))

#+CCL
(defmacro new-msg-send-stret (selector ((&rest arg-types) return-type))
  (let ((arg-syms (mapcar (lambda (_) _ (gensym))
                          arg-types)))
    `(lambda ,(cons 'target arg-syms)
       (ccl::%ff-call (CCL:%REFERENCE-EXTERNAL-ENTRY-POINT
		       (LOAD-TIME-VALUE (CCL:EXTERNAL "objc_msgSend_stret")))
			  :address target
			  :address ,selector
			  ,@(mapcan #'list arg-types arg-syms)
			  ,return-type))))

(defun order-front (thing &optional (sender nil))
  (let ((selector (new-msg-send @(orderFront:)
				#+SBCL((:pointer) :void)
				#+CCL((:address) :void))))
    (funcall selector (ns-object-ptr thing) (ns-object-ptr sender))))

(defun make-key-and-order-front (thing &optional (sender nil))
  (let ((selector (new-msg-send @(makeKeyAndOrderFront:)
				#+SBCL((:pointer) :void)
				#+CCL((:address) :void))))
    (funcall selector (ns-object-ptr thing) (ns-object-ptr sender))))

(defun order-out (thing &optional (sender nil))
  (let ((selector (new-msg-send @(orderOut:)
				#+SBCL((:pointer) :void)
				#+CCL((:address) :void))))
    (funcall selector (ns-object-ptr thing) (ns-object-ptr sender))))

#+SBCL
(defun init-with-content-rect (thing content-rect style-mask backing defer)
  (let ((selector (new-msg-send @(initWithContentRect:styleMask:backing:defer:)
				(((:struct NSRect) :uint64 :uint64 :int) :pointer))))
    (funcall selector thing
	     (list 'height (print (cffi:mem-aref content-rect :double 3))
		   'width (print (cffi:mem-aref content-rect :double 2))
		   'y (print (cffi:mem-aref content-rect :double 1))
		   'x (print (cffi:mem-aref content-rect :double 0)))
	     style-mask backing defer)))

#+CCL
(defun init-with-content-rect (thing content-rect style-mask backing defer)
  (let ((selector (new-msg-send @(initWithContentRect:styleMask:backing:defer:)
				((32 :unsigned-doubleword :unsigned-doubleword :signed-fullword) :address))))
    (funcall selector thing
	     content-rect
	     style-mask backing defer)))

(defmethod ns-app ((app ns-application-mixin))
  objc-runtime::ns-app)

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
				#+SBCL(((:struct NSRect) :uint64 :pointer :pointer) :pointer)
				#+CCL((32 :unsigned-doubleword :address :address) :address))))
    (funcall selector (ns-object-ptr thing) bounds options (ns-object-ptr owner) user-info)))

(defun convert-rect-to-backing (content-view rect)
  ;; rect is plist. retval is plist (under cffi)
  (let ((selector (new-msg-send-stret @(convertRectToBacking:)
				      #+SBCL(((:struct NSRect)) (:struct NSRect))
				      #+CCL((32) :address))))
    (funcall selector (ns-object-ptr content-view) rect)))

(defun get-frame (content-view)
  (let ((selector (new-msg-send-stret @(frame) (() #+SBCL(:struct NSRect) #+CCL :address))))
    (funcall selector (ns-object-ptr content-view))))

(defun get-bounds (content-view)
  (let ((selector (new-msg-send-stret @(bounds) (() (:struct NSRect)))))
    (funcall selector (ns-object-ptr content-view))))

(defun cascade-top-left-from-point (thing point)
  ;; point is plist.
  (let ((selector (new-msg-send @(cascadeTopLeftFromPoint:)
				#+SBCL(((:struct NSPoint)) (:struct NSPoint))
				#+CCL((32) :address))))
    (funcall selector thing point)))

(defun is-visible? (thing)
  (let ((selector (new-msg-send @(isVisible) (() :boolean))))
    (funcall selector (ns-object-ptr thing))))

(defun is-resizable? (thing)
  (let ((selector (new-msg-send @(isResizable) (() :boolean))))
    (funcall selector (ns-object-ptr thing))))

(defun is-restorable? (thing)
  (let ((selector (new-msg-send @(isRestorable) (() :boolean))))
    (funcall selector (ns-object-ptr thing))))

(defun is-sheet? (thing)
  (let ((selector (new-msg-send @(isSheet) (() :boolean))))
    (funcall selector (ns-object-ptr thing))))

(defun is-movable? (thing)
  (let ((selector (new-msg-send @(isMovable) (() :boolean))))
    (funcall selector (ns-object-ptr thing))))

(defun is-tabbed? (thing)
  (let ((selector (new-msg-send @(isTabbed) (() :boolean))))
    (funcall selector (ns-object-ptr thing))))

(defun is-on-active-space? (thing)
  (let ((selector (new-msg-send @(isOnActiveSpace) (() :boolean))))
    (funcall selector (ns-object-ptr thing))))

(defun is-miniaturizable? (thing)
  (let ((selector (new-msg-send @(isMiniaturizable) (() :boolean))))
    (funcall selector (ns-object-ptr thing))))

(defun is-miniaturized? (thing)
  (let ((selector (new-msg-send @(isMiniaturized) (() :boolean))))
    (funcall selector (ns-object-ptr thing))))

(defun is-key-window? (thing)
  (let ((selector (new-msg-send @(isKeyWindow) (() :boolean))))
    (funcall selector (ns-object-ptr thing))))

(defun is-main-window? (thing)
  (let ((selector (new-msg-send @(isMainWindow)	(() :boolean))))
    (funcall selector (ns-object-ptr thing))))

(defun is-excluded-from-windows-menu? (thing)
  (let ((selector (new-msg-send @(isExcludedFromWindowsMenu)
				(() :boolean))))
    (funcall selector (ns-object-ptr thing))))

(defun is-in-key-window? (thing)
  (let ((selector (new-msg-send @(isInKeyWindow) (() :boolean))))
    (funcall selector (ns-object-ptr thing))))

(defun is-zoomable? (thing)
  (let ((selector (new-msg-send @(isZoomable) (() :boolean))))
    (funcall selector (ns-object-ptr thing))))

(defun is-autodisplay? (thing)
  (let ((selector (new-msg-send @(isAutodisplay) (() :boolean))))
    (funcall selector (ns-object-ptr thing))))

(defun is-floating-panel? (thing)
  (let ((selector (new-msg-send @(isFloatingPanel) (() :boolean))))
    (funcall selector (ns-object-ptr thing))))

(defun is-one-shot? (thing)
  (let ((selector (new-msg-send @(isOneShot) (() :boolean))))
    (funcall selector (ns-object-ptr thing))))

(defun is-zoomed? (thing)
  (let ((selector (new-msg-send @(isZoomed) (() :boolean))))
    (funcall selector (ns-object-ptr thing))))

(defun is-modal-panel? (thing)
  (let ((selector (new-msg-send @(isModalPanel)	(() :boolean))))
    (funcall selector (ns-object-ptr thing))))

(defun is-in-full-screen-mode? (view)
  (let ((selector (new-msg-send @(isInFullScreenMode) (() :boolean))))
    (funcall selector (ns-object-ptr view))))

(defun has-close-box? (thing)
  (let ((selector (new-msg-send @(hasCloseBox) (() :boolean))))
    (funcall selector (ns-object-ptr thing))))

(defun has-title-bar? (thing)
  (let ((selector (new-msg-send @(hasTitleBar) (() :boolean))))
    (funcall selector (ns-object-ptr thing))))

(defun can-enter-full-screen-mode? (thing)
  (let ((selector (new-msg-send @(canEnterFullScreenMode)
				(() :boolean))))
    (funcall selector (ns-object-ptr thing))))

(defun enter-full-screen-mode (thing screen &optional (options nil))
  (let ((selector (new-msg-send @(enterFullScreenMode:)
				((:pointer :boolean) :boolean))))
    (funcall selector (ns-object-ptr thing) screen options)))

(defun exit-full-screen-mode (thing &optional (options nil))
  (let ((selector (new-msg-send @(exitFullScreenMode:)
				((:boolean) :boolean))))
    (funcall selector (ns-object-ptr thing) options)))

(defun get-screen (thing)
  (let ((selector (new-msg-send @(screen) (() :pointer))))
    (funcall selector (ns-object-ptr thing))))

(defun can-become-key-window? (thing)
  (let ((selector (new-msg-send @(canBecomeKeyWindow) (() :boolean))))
    (funcall selector (ns-object-ptr thing))))

(defun can-become-main-window? (thing)
  (let ((selector (new-msg-send @(canBecomeMainWindow) (() :boolean))))
    (funcall selector (ns-object-ptr thing))))

(defun can-represent-display-gamut? (thing)
  (let ((selector (new-msg-send @(canRepresentDisplayGamut)
				(() :boolean))))
    (funcall selector (ns-object-ptr thing))))

(defun can-store-color? (thing)
  (let ((selector (new-msg-send @(canStoreColor) (() :boolean))))
    (funcall selector (ns-object-ptr thing))))

(defun can-hide? (thing)
  (let ((selector (new-msg-send @(canHide) (() :boolean))))
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

(defun create-native-window (window &rest args
			     &key (xpos nil) (ypos nil)
			       (width 640) (height 480)
			       (title "Abstract OS")
			       (maximized? nil) 
			       (resizable? t)
			       (floating? nil)
			       (transparent? nil)
			       (frame-name "Abstract OS")
			       (clear-color #xffffffff)
			       (retina? t)
			       &allow-other-keys)
  (declare (ignorable args))
  
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
	    (init-with-content-rect [(window-class *app*) @(alloc)]
				    content-rect
				    style-mask NSBackingStoreBuffered 0))
	
      (when (cffi:null-pointer-p (ns-object-ptr window))
	(error "Cocoa: Failed to create window."))

      [(ns-object-ptr window) @(setTitle:) :pointer (or (and title (objc-runtime::make-nsstring title))
							(objc-runtime::make-nsstring "Abstract OS"))
      ]

      (setf (window-delegate window)
	    (make-instance 'window-delegate
			   :ptr [[(window-delegate-class *app*) @(alloc)] @(init)]
			   :owner window))

      [(ns-object-ptr window) @(setDelegate:) :pointer (ns-object-ptr (window-delegate window))]
	  
      (setf (window-content-view window)
	    (make-instance 'content-view
			   :ptr [[(content-view-class *app*) @(alloc)] @(init)]
			   :owner window
			   :marked-text [[#@NSMutableAttributedString @(alloc)] @(init)]))

      [(ns-object-ptr window) @(setContentView:) :pointer (ns-object-ptr (window-content-view window))]
      [(ns-object-ptr window) @(makeFirstResponder:) :pointer (ns-object-ptr (window-content-view window))]
	
      [(ns-object-ptr (window-content-view window)) @(updateTrackingAreas)]
      ;;[(ns-object-ptr (window-content-view window)) @(registerForDraggedTypes:) :pointer @(NSPasteboardTypeURL)]
	
      (if (window-monitor window)
	    
	  [(ns-object-ptr window) @(setLevel:) :int (1+ NSMainMenuWindowLevel)]
	    
	  (progn
	      
	    (when (or (null xpos) (null ypos))
	      (setf (cascade-point *app*) (cascade-top-left-from-point (ns-object-ptr window) (cascade-point *app*))))
	      
	    (if resizable?
		  
		(let ((behavior (logior NSWindowCollectionBehaviorFullScreenPrimary
					NSWindowCollectionBehaviorManaged)))
		  [(ns-object-ptr window) @(setCollectionBehavior:) :int behavior])
		  
		(let ((behavior NSWindowCollectionBehaviorFullScreenNone))
		  [(ns-object-ptr window) @(setCollectionBehavior:) :int behavior]))
	      
	    (when floating?
	      [(ns-object-ptr window) @(setLevel:) :int NSFloatingWindowLevel])
	      
	    (when maximized?
	      [(ns-object-ptr window) @(zoom:) :boolean nil])))
	  
      (when (and frame-name (not (string= frame-name "")))
	[(ns-object-ptr window) @(setFrameAutosaveName:) :pointer (objc-runtime::make-nsstring frame-name)])
	  
      (setf (window-retina? window) retina?)

      (when transparent?
	[(ns-object-ptr window) @(setOpaque:) :boolean nil]
	[(ns-object-ptr window) @(setHasShadow:) :boolean nil]
	[(ns-object-ptr window) @(setBackgroundColor:) :pointer [#@NSColor clear-color]])
	  
      [(ns-object-ptr window) @(setAcceptsMouseMovedEvents:) :boolean t]
      [(ns-object-ptr window) @(setRestorable:) :boolean nil]

      (multiple-value-bind (width height)
	  (get-cocoa-window-size window)
	(setf (width window) width
	      (height window) height))

      (multiple-value-bind (fb-width fb-height)
	  (get-cocoa-framebuffer-size window)
	(setf (window-fb-width window) fb-width
	      (window-fb-height window) fb-height))

      t)))

(defun get-cocoa-window-size (window)
  (let ((frame (get-frame (window-content-view window))))
    (values (getf frame 'width) (getf frame 'height))))

(defun get-cocoa-framebuffer-size (window)
  (let* ((content-rect (get-frame (window-content-view window)))
	 (fb-rect (convert-rect-to-backing (window-content-view window) content-rect)))
    (values (getf fb-rect 'width) (getf fb-rect 'height))))    

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
  [(ns-app *app*) @(activateIgnoringOtherApps:) :boolean t]
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



(defun make-os-window (&rest args)
  #+sbcl
  (sb-int:set-floating-point-modes :traps '())
  (apply #'make-instance 'essential-os-window-mixin args))

;;

(cffi:defcallback window-delegate-window-should-close-callback :void ((self :pointer) (sender :pointer))
  (declare (ignore sender))
  (let ((window (gethash (sap-int self) (delegate->clos-window-table *app*))))
    (when window
      (ns-window-should-close window))))

(defun ns-window-should-close (window)
  (input-window-close-request window))

(cffi:defcallback window-delegate-window-did-resize-callback :void ((self :pointer) (notification :pointer))
  (declare (ignore notification))
  (let ((window (gethash (sap-int self) (delegate->clos-window-table *app*))))
    (when window
      (window-did-resize window))))

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

(cffi:defcallback window-delegate-window-did-move-callback :void ((self :pointer) (notification :pointer))
  (declare (ignore notification))
  (let ((window (gethash (sap-int self) (delegate->clos-window-table *app*))))
    (when window
      (window-did-move window))))

(defun window-did-move (window)
  (declare (ignorable window))
  (values))

(cffi:defcallback window-delegate-window-did-miniaturize-callback :void ((self :pointer) (notification :pointer))
  (declare (ignore notification))
  (let ((window (gethash (sap-int self) (delegate->clos-window-table *app*))))
    (when window
      (window-did-miniaturize window))))

(defun window-did-miniaturize (window)
  (declare (ignorable window))
  #+NOTYET(release-monitor window)
  (values))

(cffi:defcallback window-delegate-window-did-deminiaturize-callback :void ((self :pointer) (notification :pointer))
  (declare (ignore notification))
  (let ((window (gethash (sap-int self) (delegate->clos-window-table *app*))))
    (when window
      (window-did-deminiaturize window))))

(defun window-did-deminiaturize (window)
  (declare (ignorable window))
  (acquire-monitor window)
  (values))

(cffi:defcallback window-delegate-window-did-become-key-callback :void ((self :pointer) (notification :pointer))
  (declare (ignore notification))
  (let ((window (gethash (sap-int self) (delegate->clos-window-table *app*))))
    (when window
      (window-did-become-key window))))

(defun window-did-become-key (window)
  (declare (ignorable window))
  (values))

(cffi:defcallback window-delegate-window-did-resign-key-callback :void ((self :pointer) (notification :pointer))
  (declare (ignore notification))
  (let ((window (gethash (sap-int self) (delegate->clos-window-table *app*))))
    (when window
      (window-did-resign-key window))))

(defun window-did-resign-key (window)
  (declare (ignorable window))
  (values))

(cffi:defcallback window-delegate-window-did-change-occlusion-state-callback :void ((self :pointer) (notification :pointer))
  (declare (ignore notification))
  (let ((window (gethash (sap-int self) (delegate->clos-window-table *app*))))
    (when window
      (window-did-change-occlusion-state window))))

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
				    "B@:")
    (objc-runtime::class-add-method content-view-class @(canBecomeKeyView)
				    (cffi:callback content-view-can-become-key-view-callback)
				    "B@:")
    (objc-runtime::class-add-method content-view-class @(acceptsFirstResponder)
				    (cffi:callback content-view-accepts-first-responder-callback)
				    "B@:")
    (objc-runtime::class-add-method content-view-class @(wantsUpdateLayer)
				    (cffi:callback content-view-wants-update-layer-callback)
				    "B@:")
    (objc-runtime::class-add-method content-view-class @(updateLayer)
				    (cffi:callback content-view-update-layer-callback)
				    "v@:")
    (objc-runtime::class-add-method content-view-class @(cursorUpdate)
				    (cffi:callback content-view-cursor-update-callback)
				    "v@:@")
    (objc-runtime::class-add-method content-view-class @(acceptsFirstMouse)
				    (cffi:callback content-view-accepts-first-mouse-callback)
				    "B@:")
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

(cffi:defcallback window-can-become-key-window-callback :boolean ((self :pointer))
  (declare (ignore self))
  t)

(cffi:defcallback window-can-become-main-window-callback :boolean ((self :pointer))
  (declare (ignore self))
  t)

(defun make-window-class ()
  (let ((window-class
	 (objc-runtime::objc-allocate-class-pair
	  #@NSWindow "AbstractOSWindow" 0)))
    (objc-runtime::class-add-method window-class @(canBecomeKeyWindow)
				    (cffi:callback window-can-become-key-window-callback)
				    "B@:")
    (objc-runtime::class-add-method window-class @(canBecomeMainWindow)
				    (cffi:callback window-can-become-main-window-callback)
				    "B@:")
    window-class))

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
