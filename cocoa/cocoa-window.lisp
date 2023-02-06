(in-package :clui)
(named-readtables:in-readtable :objc-readtable)

(defvar *trace-callbacks* t)

(defvar w)

(defun get-cocoa-window-fullscreen (window)
  (ns::|isInFullScreenMode| window))

(defun set-cocoa-window-fullscreen (window value)
  (ns::|toggleFullScreen:| window value))

(defun get-cocoa-window-closable (window)
  (with-autorelease-pool (pool)
    (logtest NSWindowStyleMaskClosable (ns:|styleMask| window))))

(defun set-cocoa-window-closable (window value)
  (with-autorelease-pool (pool)
    (let ((style (ns:|styleMask| window)))
      (if value
	  (setq style (logior style NSWindowStyleMaskClosable))
	  (setq style (logand style (lognot NSWindowStyleMaskClosable))))
      (ns:|setStyleMask:| window style)
      (values))))

(defun get-cocoa-window-title (window)
  (with-autorelease-pool (pool)
    (let ((ptr (ns:|title| window)))
      (unless (cffi:null-pointer-p ptr)
	(objc-runtime::extract-nsstring ptr)))))

(defun set-cocoa-window-title (window string)
  (with-autorelease-pool (ppol)
    (ns:|setTitle:| window (objc-runtime::make-nsstring string))
    (values)))

(defun cocoa-window-titled? (window)
  (with-autorelease-pool (pool)
    (logtest NSWindowStyleMaskTitled (ns:|styleMask| window))))

(defun set-cocoa-window-titled (window value)
  (with-autorelease-pool (pool)
    (let ((style (ns:|styleMask| window)))
      (if value
	  (setq style (logior style NSWindowStyleMaskTitled))
	  (setq style (logand style (lognot NSWindowStyleMaskTitled))))
      (ns:|setStyleMask:| window style)
      (values))))

(defun get-cocoa-window-pos (window)
  (with-autorelease-pool (pool)
    (let ((content-rect
	   (ns::|contentRectForFrameRect:| window (ns:|frame| window))))
      (let ((x (ns-get-x content-rect))
	    (y (cocoa-transform-y (1- (+ (ns-get-y content-rect) (ns-get-height content-rect))))))
	(values x y)))))

(defun set-cocoa-window-pos (window x y)
  (let* ((content-rect (ns:|frame| (window-content-view window)))
	 (dummy-rect (make-nsrect x (cocoa-transform-y (1- (+ y (ns-get-height content-rect)))) 0 0))
	 (frame-rect (ns:|frameRectForContentRect:| window dummy-rect)))
    (ns:|setFrameOrigin:| window (make-nspoint (ns-get-x frame-rect) (ns-get-y frame-rect)))))

(defun get-cocoa-window-size (window)
  (with-autorelease-pool (pool)
    (let ((content-rect (ns::|contentRectForFrameRect:| window (ns:|frame| window))))
      (values (ns-get-width content-rect) (ns-get-height content-rect)))))

(defun set-cocoa-window-size (window width height)
  (with-autorelease-pool (pool)
    (let ((monitor (window-monitor window)))
      (if monitor
	  (when (eq (monitor-window monitor) window)
	    (acquire-cocoa-monitor window monitor))
	  (let* ((content-rect (ns:|contentRectForFrameRect:| window (ns:|frame| window)))
		 (y (+ (ns-get-y content-rect) (- (ns-get-height content-rect) height)))
		 (new-content-rect (make-nsrect (ns-get-x content-rect) y width height)))
	    (ns:|setFrame:display:| window (ns:|frameRectForContentRect:| window new-content-rect) t)))
      (values))))

(defun set-cocoa-window-size-limits (window min-width min-height max-width max-height)
  (with-autorelease-pool (pool)
    (unless (and min-width min-height)
      (ns:|setContentMinSize:| window (make-nssize 0 0)))
    (ns:|setContentMinSize:| window (make-nssize min-width min-height))
    (unless (and max-width max-height)
      (ns:|setContentMaxSize:| window (make-nssize most-positive-single-float most-positive-single-float)))
    (ns:|setContentMaxSize:| window (make-nssize max-width max-height))
    (values)))

(defun get-cocoa-window-aspect-ratio (window)
  (multiple-value-bind (width height) (get-cocoa-window-size window)
    (/ width height)))

(defun set-cocoa-window-aspect-ratio (window numer denom)
  (with-autorelease-pool (pool)
    (unless (and numer denom)
      (ns:|setResizeIncrements:| window (make-nssize 1 1)))
    (ns:|setContentAspectRatio:| window (make-nssize numer denom))))

(defun get-cocoa-window-framebuffer-size (window)
  (let ((view (window-content-view window)))
    (with-autorelease-pool (pool)
      (let* ((content-rect (ns:|frame| view)))
	(let ((fb-rect (ns:|convertRectToBacking:| view content-rect)))
	  (values (ns-get-width fb-rect) (ns-get-height fb-rect)))))))

(defun get-cocoa-window-frame-size (window)
  (with-autorelease-pool (pool)
    (let* ((content-rect (ns:|frame| (window-content-view window)))
	   (frame-rect (ns:|frameRectForContentRect:| window content-rect)))
      (values (- (ns-get-x content-rect) (ns-get-x frame-rect))
	      (- (+ (ns-get-y frame-rect) (ns-get-height frame-rect))
		 (ns-get-y content-rect) (ns-get-height content-rect))
	      (- (+ (ns-get-x frame-rect) (ns-get-width frame-rect))
		 (ns-get-x content-rect) (ns-get-width content-rect))
	      (- (ns-get-y content-rect) (ns-get-y frame-rect))))))

(defun get-cocoa-window-content-scale (window)
  (with-autorelease-pool (pool)
    (let* ((view (window-content-view window))
	   (points (ns:|frame| view))
	   (pixels (ns:|convertRectToBacking:| view points)))
      (values (/ (ns-get-width pixels) (ns-get-width points))
	      (/ (ns-get-height pixels) (ns-get-height points))))))

(defun cocoa-window-maximized? (window)
  (with-autorelease-pool (pool)
    (ns:|isZoomed| window)))

(defun maximize-cocoa-window (window)
  (with-autorelease-pool (pool)
    (ns:|zoom:| window nil)
    (values)))

(defun restore-cocoa-window (window)
  (if (cocoa-window-iconified? window)
      (deiconfify-cocoa-window window)
      (maximize-cocoa-window window)))

(defun set-cocoa-window-maximized (window value)
  (when value
    (maximize-cocoa-window window))
  value)

(defun show-cocoa-window (window)
  (with-autorelease-pool (pool)
    (ns:|orderFront:| window nil)))

(defun hide-cocoa-window (window)
  (with-autorelease-pool (pool)
    (ns:|orderOut:| window nil)))

(defun set-cocoa-window-shown (window value)
  (if value
      (show-cocoa-window window)
      (hide-cocoa-window window))
  value)

(defun set-cocoa-window-hidden (window value)
  (if value
      (hide-cocoa-window window)
      (show-cocoa-window window))
  value)

(defun request-cocoa-window-attention (window)
  (declare (ignore window))
  (ns:|requestUserAttention:| (window-display window) NSInformationalRequest)
  (values))

(defun cocoa-window-focused? (window)
  (with-autorelease-pool (pool)
    (ns:|isKeyWindow| window)))

(defun unfocus-cocoa-window (window)
  (with-autorelease-pool (pool)
    (ns:|deactivate| window)
    (hide-cocoa-window window)))

(defun focus-cocoa-window (window)
  (with-autorelease-pool (pool)
    (ns:|activateIgnoringOtherApps:| (window-display window) t)
    (ns:|makeKeyAndOrderFront:| window nil))
  (values))

(defun set-cocoa-window-focused (window value)
  (if value
      (focus-cocoa-window window)
      (unfocus-cocoa-window window))
  value)

(defun cocoa-window-iconified? (window)
  (with-autorelease-pool (pool)
    (ns:|isMiniaturized| window)))

(defun iconify-cocoa-window (window)
  (with-autorelease-pool (pool)
    (ns:|miniaturize:| window nil)
    (values)))

(defun deiconify-cocoa-window (window)
  (with-autorelease-pool (pool)
    (ns:|deminiaturize:| window nil)
    (values)))

(defun set-cocoa-window-iconified (window value)
  (if value
      (iconify-cocoa-window window)
      (deiconify-cocoa-window window))
  value)

(defun cocoa-window-visible? (window)
  (with-autorelease-pool (pool)
    (ns:|isVisible| window)))

(defun set-cocoa-window-visible (window value)
  (with-autorelease-pool (pool)
    (ns:|setIsVisible:| window value)
    value))

(defun make-cocoa-window-visible (window)
  (set-cocoa-window-visible window t)
  (values))

(defun make-cocoa-window-invisible (window)
  (set-cocoa-window-visible window nil)
  (values))

(defun cocoa-window-hovered? (window)
  (with-autorelease-pool (pool)
    (let ((point (ns:|mouseLocation| #@NSEvent)))
      (unless (eq (ns:|windowNumberAtPoint:belowWindowWithWindowNumber:| #@NSWindow point 0)
		  (ns:|windowNumber| window))
	(Return-From cocoa-window-hovered? nil))
      (ns-mouse-in-rect point (ns:|convertRectToScreen:| window (ns:|frame| (window-content-view window)))))))

(defun cocoa-window-framebuffer-transparent? (window)
  (with-autorelease-pool (pool)
    (not (or (ns:|isOpaque| window) (ns:|isOpaque| (window-content-view window))))))

(defun cocoa-window-resizable? (window)
  (with-autorelease-pool (pool)
    (ns:|isResizable| window)))

(defun set-cocoa-window-resizable (window value)
  (with-autorelease-pool (pool)
    (let ((style (ns:|styleMask| window))
	  (behavior))
      (if value
	  (progn (setq style (logior style NSWindowStyleMaskResizable))
		 (setq behavior (logior NSWindowCollectionBehaviorFullScreenPrimary
					NSWindowCollectionBehaviorManaged)))
	  (progn (setq style (logand style (lognot NSWindowStyleMaskResizable)))
		 (setq behavior NSWindowCollectionBehaviorFullScreenNone)))
      (ns:|setStyleMask:| window style)
      (ns:|setCollectionBehavior:| window behavior)
      (values))))

(defun cocoa-window-decorated? (window)
  (with-autorelease-pool (pool)
    (let ((style (ns:|styleMask| window)))
      (and (or (logtest style NSWindowStyleMaskTitled)
	       (logtest style NSWindowStyleMaskClosable)
	       (not (logtest style NSWindowStyleMaskBorderless)))))))

(defun set-cocoa-window-decorated (window value)
  (with-autorelease-pool (pool)
    (let ((style (ns:|styleMask| window)))
      (if value
	  (setq style (logior style NSWindowStyleMaskTitled NSWindowStyleMaskClosable))
	  (setq style (logior style NSWindowStyleMaskBorderless)))
      (ns:|setStyleMask:| window style)
      (values))))

(defun cocoa-window-floating? (window)
  (with-autorelease-pool (pool)
    (ns:|isFloating| window)))

(defun set-cocoa-window-floating (window value)
  (with-autorelease-pool (pool)
    (ns:|setFloating:| window value)))

(defun get-cocoa-window-opacity (window)
  (with-autorelease-pool (pool)
    (ns:|alphaValue| window)))

(defun set-cocoa-window-opacity (window alpha)
  (setq alpha (coerce alpha 'single-float))
  (with-autorelease-pool (pool)
    (unless (= 1.0f0 alpha)
      (ns:|setOpaque:| window nil)
      (ns:|setOpaque:| (window-content-view window) nil))
    (ns:|setAlphaValue:| window alpha)))

(defun get-cocoa-view-size (view)
  (with-autorelease-pool (pool)
    (let ((frame (ns:|frame| view)))
      (values (ns-get-width frame) (ns-get-height frame)))))

(defun cocoa-raw-mouse-motion-supported? ()
  nil)

(defun get-cocoa-window-cursor-pos (window)
  (with-autorelease-pool (pool)
    (let ((content-rect (ns:|frame| (window-content-view window)))
	  (pos (ns:|mouseLocationOutsideOfEventStream| window)))
      (values (ns-get-x pos)
	      (- (ns-get-height content-rect) (ns-get-y pos))))))

(defun set-cocoa-window-cursor-pos (window x y)
  (with-autorelease-pool (pool)
    (update-cursor-image window)
    (let ((content-rect (ns:|frame| (window-content-view window)))
	  (pos (ns:|mouseLocationOutsideOfEventStream| window)))
      (setf (cursor-warp-delta-x window) (+ (cursor-warp-delta-x window) x (- (ns-get-x pos)))
	    (cursor-warp-delta-y window) (+ (cursor-warp-delta-y window) y (- (ns-get-height content-rect)) (ns-get-y pos)))

      (if (window-monitor window)
	  (CGDisplayMoveCursorToPoint (monitor-display-id (window-monitor window)) (make-nspoint x y))
	  (let* ((local-rect (make-nsrect x (- (ns-get-height content-rect) y 1) 0 0))
		 (global-rect (ns:|convertRectToScreen:| window local-rect))
		 (global-point (make-nspoint (ns-get-x global-rect) (ns-get-y global-rect))))

	    (CGWarpMouseCursorPosition (make-nspoint (ns-get-x global-point)
						     (cocoa-transform-y (ns-get-y global-point))))))

      (when (eq (cursor-mode window) :disabled)
	(CGAssociateMouseAndMouseCursorPosition t))

      (values))))
















(deftraceable-callback content-view-dealloc-callback :void ((self :pointer) (_cmd :pointer))
;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       *content-view->clos-content-view-table*)))
    (when content-view
      (ns:|dealloc| content-view))
    (values)))

(deftraceable-callback content-view-is-opaque-callback :char ((self :pointer) (_cmd :pointer))
;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       *content-view->clos-content-view-table*)))
    (if content-view
	(content-view-is-opaque content-view)
	*yes*)))

(defun content-view-is-opaque (view)
  (let ((window (content-view-owner view)))
    (when window
      (if (ns:|isOpaque| window)
	  *yes*
	  *no*))))

(deftraceable-callback content-view-can-become-key-view-callback :char ((self :pointer) (_cmd :pointer))
;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       *content-view->clos-content-view-table*)))
    (when content-view
      (content-view-can-become-key-view content-view))))

(defun content-view-can-become-key-view (content-view)
  (if (cocoa-window-can-become-key (content-view-owner content-view))
      *yes*
      *no*))

(defmethod cocoa-window-can-become-key ((window window-mixin))
  nil)

(defmethod cocoa-window-can-become-key ((window os-window-mixin))
  t)

(deftraceable-callback content-view-accepts-first-responder-callback :char ((self :pointer) (_cmd :pointer))
;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       *content-view->clos-content-view-table*)))
    (when content-view
      (content-view-accepts-first-responder content-view))))

(defun content-view-accepts-first-responder (view)
  (if (cocoa-window-accepts-first-responder (content-view-owner view))
      *yes*
      *no*))

(defmethod cocoa-window-accepts-first-responder ((window os-window-mixin))
  t)

(defmethod cocoa-window-accepts-first-responder ((window window-mixin))
  nil)

(deftraceable-callback content-view-wants-update-layer-callback :char ((self :pointer) (_cmd :pointer))
;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       *content-view->clos-content-view-table*)))
    (if content-view
      (content-view-wants-update-layer content-view)
      (progn (break)
      *no*))))

(defun content-view-wants-update-layer (view)
  ;; rather than making a separate content-view class for each window class and dispatching on content-view type
  ;; just dispatch on window type
  (if (cocoa-window-wants-update-layer (content-view-owner view))
      *yes*
      *no*))

(defmethod cocoa-window-wants-update-layer ((window os-window-mixin))
  ;; for example, a method for this generic function for a metal window would answer differently
  nil)

(deftraceable-callback content-view-draw-in-mtkview-callback :void ((self :pointer) (_cmd :pointer) (view :pointer))
;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       *content-view->clos-content-view-table*)))
    (when content-view
      (content-view-draw-in-mtkview content-view))
    (values)))

(defun content-view-draw-in-mtkview (mtkview)
  (declare (ignorable mtkview))
  )

(deftraceable-callback content-view-update-layer-callback :void ((self :pointer) (_cmd :pointer))
  ;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       *content-view->clos-content-view-table*)))
    (when content-view
      (content-view-update-layer content-view))))

(defun content-view-update-layer (content-view)
  (cocoa-window-update-view-layer (content-view-owner content-view))
  (values))

(defmethod cocoa-window-update-view-layer ((window window-mixin))
  #+NIL(input-window-damage window))

(deftraceable-callback content-view-cursor-update-callback :void ((self :pointer) (_cmd :pointer) (event :pointer))
;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       *content-view->clos-content-view-table*)))
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
			       *content-view->clos-content-view-table*)))
    (when content-view
      (content-view-accepts-first-mouse content-view))))

(defun content-view-accepts-first-mouse (content-view)
  (declare (ignore content-view))
  *yes*)

(deftraceable-callback content-view-mouse-down-callback :void ((self :pointer) (_cmd :pointer) (event :pointer))
;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       *content-view->clos-content-view-table*)))
    (when content-view
      (content-view-mouse-down content-view event))
    (values)))

(defun content-view-mouse-down (content-view event)
  (let ((clui-event (make-instance 'pointer-button-press-event
				   :button +pointer-left-button+
				   :modifier-state (translate-flags (ns:|modifierFlags| event)))))
    (handle-event (content-view-owner content-view) clui-event)))

(defun translate-flags (event-modifier-flags)
  event-modifier-flags)

(deftraceable-callback content-view-mouse-dragged-callback :void ((self :pointer) (_cmd :pointer) (event :pointer))
;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       *content-view->clos-content-view-table*)))
    (when content-view
      (content-view-mouse-dragged content-view event))
    (values)))

(defun content-view-mouse-dragged (content-view event)
  (ns:|mouseMoved:| content-view event)
  (values))

(deftraceable-callback content-view-mouse-up-callback :void ((self :pointer) (_cmd :pointer) (event :pointer))
;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       *content-view->clos-content-view-table*)))
    (when content-view
      (content-view-mouse-up content-view event))
    (values)))

(defun content-view-mouse-up (content-view event)
  (let ((clui-event (make-instance 'pointer-button-release-event
				   :button +pointer-left-button+
				   :modifier-state (translate-flags (ns:|modifierFlags| event)))))
    (handle-event (content-view-owner content-view) clui-event))
  (values))

(deftraceable-callback content-view-mouse-moved-callback :void ((self :pointer) (_cmd :pointer) (event :pointer))
;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       *content-view->clos-content-view-table*)))
    (when content-view
      (content-view-mouse-moved content-view event))
    (values)))



(defun content-view-mouse-moved (content-view event)
  (let ((window (content-view-owner content-view)))

    (if (eq (window-cursor-mode window) :disabled)
	(let ((dx (- (ns:|deltaX| event) (cursor-warp-delta-x window)))
	      (dy (- (ns:|deltaY| event) (cursor-warp-delta-y window))))

	  (input-cursor-pos window
			    (+ (virtual-cursor-pos-x window) dx)
			    (+ (virtual-cursor-pos-y window) dy)))
	(let* ((content-rect (ns:|frame| content-view))
	       (loc (ns:|locationInWindow| event))
	       (x (ns-get-x loc))
	       (y (- (ns-get-y content-rect) (ns-get-y loc)))
	       (clui-event (make-instance 'pointer-motion-event
					  :x x
					  :y y
					  :native-x x
					  :native-y y)))
	      (handle-event window clui-event)))

    (setf (cursor-warp-delta-x window) 0
	  (cursor-warp-delta-y window) 0)
    (values)))

(deftraceable-callback content-view-right-mouse-down-callback :void ((self :pointer) (_cmd :pointer) (event :pointer))
;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       *content-view->clos-content-view-table*)))
    (when content-view
      (content-view-right-mouse-down content-view event))
    (values)))

(defun content-view-right-mouse-down (content-view event)
  (let ((clui-event (make-instance 'pointer-button-press-event
				   :button +pointer-right-button+
				   :modifier-state (translate-flags (ns:|modifierFlags| event)))))
    (handle-event (content-view-owner content-view) clui-event)))

(deftraceable-callback content-view-right-mouse-dragged-callback :void ((self :pointer) (_cmd :pointer) (event :pointer))
;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       *content-view->clos-content-view-table*)))
    (when content-view
      (content-view-right-mouse-dragged content-view event))
    (values)))

(defun content-view-right-mouse-dragged (content-view event)
  (ns:|mouseMoved:| content-view event)
  (values))
  
(deftraceable-callback content-view-right-mouse-up-callback :void ((self :pointer) (_cmd :pointer) (event :pointer))
;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       *content-view->clos-content-view-table*)))
    (when content-view
      (content-view-right-mouse-up content-view event))
    (values)))

(defun content-view-right-mouse-up (content-view event)
  (let ((clui-event (make-instance 'pointer-button-release-event
				   :button +pointer-right-button+
				   :modifier-state (translate-flags (ns:|modifierFlags| event)))))
    (handle-event (content-view-owner content-view) clui-event))
  (values))

(deftraceable-callback content-view-other-mouse-down-callback :void ((self :pointer) (_cmd :pointer) (event :pointer))
;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       *content-view->clos-content-view-table*)))
    (when content-view
      (content-view-other-mouse-down content-view event))
    (values)))

(defun content-view-other-mouse-down (content-view event)
  (ns:|toggleFullScreen:| (content-view-owner content-view) nil)
  (let ((clui-event (make-instance 'pointer-button-press-event
				   :button (aref #(+pointer-button-right+ +pointer-left-button+ +pointer-middle-button+)
						 (ns:|buttonNumber| event))
				   :modifier-state (translate-flags (ns:|modifierFlags| event)))))
    (handle-event (content-view-owner content-view) clui-event)))

(deftraceable-callback content-view-other-mouse-dragged-callback :void ((self :pointer) (_cmd :pointer) (event :pointer))
;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       *content-view->clos-content-view-table*)))
    (when content-view
      (content-view-other-mouse-dragged content-view event))
    (values)))

(defun content-view-other-mouse-dragged (content-view event)
  (ns:|mouseMoved:| content-view event)
  (values))
  
(deftraceable-callback content-view-other-mouse-up-callback :void ((self :pointer) (_cmd :pointer) (event :pointer))
;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       *content-view->clos-content-view-table*)))
    (when content-view
      (content-view-other-mouse-up content-view event))
    (values)))

(defun content-view-other-mouse-up (content-view event)
  (let ((clui-event (make-instance 'pointer-button-release-event
				   :button (aref #(+pointer-button-right+ +pointer-left-button+ +pointer-middle-button+)
						 (ns:|buttonNumber| event))
				   :modifier-state (translate-flags (ns:|modifierFlags| event)))))
    (handle-event (content-view-owner content-view) clui-event)))

(deftraceable-callback content-view-other-mouse-exited-callback :void ((self :pointer) (_cmd :pointer) (event :pointer))
;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       *content-view->clos-content-view-table*)))
    (when content-view
      (content-view-other-mouse-exited content-view event))
    (values)))

(defun content-view-other-mouse-exited (view event)
  (let ((window (content-view-owner view)))
    (on-cocoa-other-mouse-exited window event)))

(defmethod on-cocoa-other-mouse-exited ((window window-mixin) event)
  (when (eq (window-cursor-mode window) :hidden)
    (show-cursor window))
  (input-cursor-enter window nil))
    

(deftraceable-callback content-view-other-mouse-entered-callback :void ((self :pointer) (_cmd :pointer) (event :pointer))
;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       *content-view->clos-content-view-table*)))
    (when content-view
      (content-view-other-mouse-entered content-view event))
    (values)))

(defun content-view-other-mouse-entered (view event)

  (let ((window (content-view-owner view)))
    (on-cocoa-other-mouse-entered window event)))

(defmethod on-cocoa-other-mouse-entered ((window window-mixin) event)
  (declare (ignore event))
  (when (eq (window-cursor-mode window) :hidden)
    (hide-cursor window))
  (input-cursor-enter window t))

(deftraceable-callback content-view-view-did-change-backing-properties-callback :void ((self :pointer) (_cmd :pointer))
;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       *content-view->clos-content-view-table*)))
    (when content-view
      (content-view-view-did-change-backing-properties content-view))
    (values)))

(defun content-view-view-did-change-backing-properties (view)
  (cocoa-window-did-change-backing-properties (content-view-owner view)))

(defmethod cocoa-window-did-change-backing-properties ((window window-mixin))
  (values))

(defmethod cocoa-window-did-change-backing-properties ((window constant-refresh-os-window-mixin))
  #+NOTYET
  (let* ((window (content-view-owner content-view))
	 (content-rect [(objc-object-id content-view) @(frame)])
	 (fb-rect [(objc-object-id content-view) @(convertRectToBacking:) :pointer content-rect])
	 (x-scale (/ (cffi:mem-aref [[fb-rect @(size)] @(width)] :int) (cffi:mem-aref [[content-rect @(size)] @(width)] :int)))
	 (y-scale (/ (cffi:mem-aref [[fb-rect @(size)] @(height)] :int) (cffi:mem-aref [[content-rect @(size)] @(height)] :int))))

    (when (or (/= x-scale (window-x-scale window)) (/= y-scale (window-y-scale window)))
      (when (and (window-retina? window) (window-layer window))
	[(objc-object-id (window-layer)) @(setContentsScale:) :integer [(objc-object-id window) @(backingScaleFactor)]])

      (setf (window-x-scale window) x-scale
	    (window-y-scale window) y-scale)
      (input-window-content-scale window x-scale y-scale))

    (when (or (/= (cffi:mem-aref [[fb-rect @(size)] @(width)] :int) (framebuffer-width window))
	      (/= (cffi:mem-aref [[fb-rect @(size)] @(height)] :int) (framebuffer-height window)))
      (input-framebuffer-size window
			      (cffi:mem-aref [[fb-rect @(size)] @(width)] :int)
			      (cffi:mem-aref [[fb-rect @(size)] @(height)] :int))))
    
  (values))

(cffi:defcallback content-view-draw-rect-callback :void ((self :pointer) (_cmd :pointer) (rect :pointer))
  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       *content-view->clos-content-view-table*)))
    (when content-view
      (content-view-draw-rect (content-view-owner content-view) content-view rect))
    (values)))

(defmethod content-view-draw-rect ((window os-window-mixin) (view content-view) rect)
  (declare (ignorable rect))
  (format t "~%running draw-rect")
  (finish-output)
  (ns:|set| (ns:|whiteColor| #@NSColor))
  (ns:|strokeRect:| #@NSBezierPath (make-nsrect 0 0 1 1))
  (NSRectFill (ns:|bounds| view))
  (ns:|flushGraphics| (window-graphics-context window))
  #+NIL(input-window-damaged window)
  )

(defmethod content-view-draw-rect ((window constant-refresh-os-window-mixin) (view content-view) rect)
  (declare (ignorable view))
  (values))

(deftraceable-callback content-view-update-tracking-areas-callback :void ((self :pointer) (_cmd :pointer))
;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self) *content-view->clos-content-view-table*)))
    (when content-view
      (content-view-update-tracking-areas content-view))
    (values)))

(defun content-view-update-tracking-areas (view)
  (cocoa-window-update-tracking-areas (content-view-owner view)))

(defmethod cocoa-window-update-tracking-areas ((window os-window-mixin))
  (let ((content-view (window-content-view window)))
    (let ((tracking-area (content-view-tracking-area content-view)))
    
      (when tracking-area
	(ns:|removeTrackingArea:| content-view tracking-area)
	(ns:|release| tracking-area))

      (let ((options (logior NSTrackingMouseEnteredAndExited
			     NSTrackingActiveInKeyWindow
			     NSTrackingEnabledDuringMouseDrag
			     NSTrackingCursorUpdate
			     NSTrackingInVisibleRect
			     NSTrackingAssumeInside)))

	(setq tracking-area
	      (setf (content-view-tracking-area content-view)
		    (ns::|initWithRect:options:owner:userInfo:|
			 (alloc #@NSTrackingArea) (ns:|bounds| content-view) options content-view nil)))

	(ns:|addTrackingArea:| content-view tracking-area)
	(super-update-tracking-areas content-view))))
  (values))



(defun cocoa-transform-y (y)
  (declare (type real y))
  (coerce (1- (- (ns-get-height (CGDisplayBounds (CGMainDisplayID))) y)) 'double-float))




(defun create-native-window (window &rest args
			     &key (xpos nil) (ypos nil)
			       (width 640) (height 480)
			       (title "clui")
			       (decorated? t)
			       (maximized? nil) 
			       (resizable? t)
			       (floating? nil)
			       (transparent? nil)
			       (frame-name "clui")
			       (retina? t)
			       &allow-other-keys)

  #+sbcl
  (sb-int:set-floating-point-modes :traps '())
  
  (let ((content-rect))
    (if (window-monitor window)
	
	(multiple-value-bind (xpos ypos) (get-cocoa-monitor-pos (window-monitor window))
	  (let* ((mode (get-cocoa-video-mode (window-monitor window)))
		 (width (video-mode-width mode))
		 (height (video-mode-height mode)))

	    (setq content-rect (make-nsrect xpos ypos width height))))

	(if (or (null xpos) (null ypos))
	    (setq content-rect (make-nsrect 0 0 (or width 640) (or height 480)))

	    (setq content-rect (make-nsrect xpos
					     (cocoa-transform-y (1- (+ ypos (or height 480.0d0))))
					     (or width 640) (or height 480)))))

    (let ((style-mask NSWindowStyleMaskMiniaturizable))

      (if (or (window-monitor window) (not decorated?))

	  (progn
	    (setq style-mask (logior style-mask NSWindowStyleMaskBorderless)))
	  
	  (progn
	    (setq style-mask (logior style-mask NSWindowStyleMaskTitled NSWindowStyleMaskClosable))
 	    
	    (when resizable?
	      (setq style-mask (logior style-mask NSWindowStyleMaskResizable)))))

      (setf (objc-object-id window)
	    (NS:|initWithContentRect:styleMask:backing:defer:|
		 (alloc (objc-window-class (window-display window)))
		 content-rect
		 #+NIL(logior NSWindowStyleMaskTitled
			     NSWindowStyleMaskClosable
			     NSWindowStyleMaskMiniaturizable
			     NSWindowStyleMaskResizable)
		 style-mask
		 NSBackingStoreBuffered nil))

      (when (cffi:null-pointer-p (objc-object-id window))
	(error "Cocoa: Failed to create window."))

      (setf (window-delegate window)
	    (make-instance 'window-delegate
			   :ptr (alloc-init (objc-window-delegate-class (window-display window)))
			   :owner window))

      (ns:|setDelegate:| window (window-delegate window))
	  
      (setf (window-content-view window)
	    (make-instance 'content-view
			   :ptr (alloc (objc-content-view-class (window-display window)))
			   :owner window
			   :marked-text (alloc-init #@NSMutableAttributedString)))

      (super-init-with-frame (window-content-view window) (ns:|frame| window))
      (ns:|updateTrackingAreas| (window-content-view window))
      (ns:|registerForDraggedTypes:| (window-content-view window) (array-with-objects (objc-runtime::make-nsstring "NSPasteboardTypeURL")))

      (ns:|setContentView:| window (window-content-view window))

      (apply #'initialize-window-devices window args)
	
      (if (window-monitor window)

	  (ns:|setLevel:| window (1+ NSMainMenuWindowLevel))
	    
	  (progn
	      
	    (when (or (null xpos) (null ypos))
	      (setf (cascade-point (window-display window))
		    (ns:|cascadeTopLeftFromPoint:| window (cascade-point (window-display window)))))

	    (let ((behavior (if resizable?
				(logior NSWindowCollectionBehaviorFullScreenPrimary
					NSWindowCollectionBehaviorManaged)
				NSWindowCollectionBehaviorFullScreenNone)))
		  
	      (ns:|setCollectionBehavior:| window behavior))

	    (when floating?
	      (ns:|setLevel:| window NSFloatingWindowLevel))
	      
	    (when maximized?
	      (ns:|zoom:| window t))))

      (when (and frame-name (not (string= frame-name "")))
	(ns:|setFrameAutosaveName:| window (objc-runtime::make-nsstring frame-name)))
	  
      (setf (window-retina? window) retina?)

      (when transparent?
	(ns:|setOpaque:| window t)
	(ns:|setHasShadow:| window t)
	(ns:|setBackgroundColor:| window (ns:|clearColor| #@NScolor)))

      (ns:|makeFirstResponder:| window (window-content-view window))
      (set-cocoa-window-title window (or title "clui"))
      (ns:|setAcceptsMouseMovedEvents:| window t)
      (ns:|setRestorable:| window nil)

      #+NIL
      (let ((window-controller (alloc (objc-window-controller-class (window-display window)))))

	(setf (display-window-controller (window-display window)) window-controller)
	;;(super-init-with-window window-controller window)

	;;(ns:|setTouchBar:| window (alloc-init #@NSTouchBar))
	;;(ns:|makeTouchBar| window-controller)
	)

      t)))

(defmethod initialize-window-devices ((window os-window-mixin) &rest args &key &allow-other-keys)
  (declare (ignore args))
  (setf (window-graphics-context window)
	(ns:|graphicsContextWithWindow:| #@NSGraphicsContext window))
  (values))

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
	  (acquire-cocoa-monitor window (window-monitor window))
	  (when center-cursor?
	    #+NOTYET
	    (center-cursor-in-content-area window)))
	(when visible?
	  (show-cocoa-window window)
	  (when focused?
	    (focus-cocoa-window window))))
    t))



(defun acquire-cocoa-monitor (window monitor)
  (set-cocoa-video-mode monitor (window-video-mode window))
  (with-autorelease-pool (pool)
    (let* ((bounds (CGDisplayBounds (monitor-display-id monitor)))
	   (frame (make-nsrect (ns-get-x bounds)
			       (cocoa-transform-y (1- (+ (ns-get-y bounds) (ns-get-height bounds))))
			       (ns-get-width bounds)
			       (ns-get-height bounds))))
      (ns:|setFrame:| frame t)
      ;;(input-monitor-window monitor window)
      (values))))

(defun release-cocoa-monitor (window monitor)
  (unless (eq (monitor-window monitor) window)
    (return-from release-cocoa-monitor (values)))
  ;;(input-monitor-window (window-monitor window) nil)
  (restore-cocoa-video-mode (window-monitor window))
  (values))


;;


(deftraceable-callback observe-value-for-key-path-callback :void
    ((self :pointer) (_cmd :pointer) (key-path :pointer)
     (of-object :pointer) (change :pointer) (context :pointer))
  (values))


(deftraceable-callback window-delegate-window-should-close-callback :void ((self :pointer) (_cmd :pointer) (sender :pointer))
;;  (declare (ignore sender))
  (let ((window (gethash (sap-int self) *delegate->clos-window-table*)))
    (when window
      (ns-window-should-close window))
    (values)))


(defun ns-window-should-close (window)
  (input-window-close-request window))

(deftraceable-callback window-delegate-window-did-resize-callback :void ((self :pointer) (_cmd :pointer) (notification :pointer))
  (let ((window (gethash (sap-int self) *delegate->clos-window-table*)))
    (when window
      (cocoa-window-did-resize window notification))
    (values)))

(defun cocoa-window-did-resize (window notification)
  (declare (ignorable notification))
  #+NIL
  (when (eq window (disabled-cursor-window (window-display window)))
    (center-cursor-in-content-area window))
  (let* ((content-rect (ns:|frame| window)))
    (let* ((maximized? (ns:|isZoomed| window))
	   (event (make-instance (cond (maximized? 'window-restore-event)
				       ((not maximized?) 'window-resize-event))
				 :timestamp (get-internal-real-time)
				 :new-x (ns-get-x content-rect)
				 :new-y (ns-get-y content-rect)
				 :new-width (ns-get-width content-rect)
				 :new-height (ns-get-height content-rect))))
      (handle-event window event))))

(deftraceable-callback window-delegate-window-did-move-callback :void ((self :pointer) (_cmd :pointer) (notification :pointer))
  (let ((window (gethash (sap-int self) *delegate->clos-window-table*)))
    (when window
      (cocoa-window-did-move window notification))
    (values)))

(defun cocoa-window-did-move (window notification)
  (declare (ignorable notification))
  (multiple-value-bind (x y) (get-cocoa-window-pos window)
    (let ((event (make-instance 'window-move-event
				:timestamp (get-internal-real-time)
				:new-x x
				:new-y y)))
      (handle-event window event)))
  (values))

(deftraceable-callback window-delegate-window-did-miniaturize-callback :void ((self :pointer) (_cmd :pointer) (notification :pointer))
  (let ((window (gethash (sap-int self) *delegate->clos-window-table*)))
    (when window
      (cocoa-window-did-miniaturize window notification))
    (values)))

(defun cocoa-window-did-miniaturize (window notification)
  (declare (ignorable notification))
  (let ((event (make-instance 'window-iconify-event
			      :timestamp (get-internal-real-time)
			      :new-x nil
			      :new-y nil
			      :new-width 0
			      :new-height 0)))
    (handle-event window event))
  (values))

;;(trace release-monitor acquire-monitor release-cocoa-monitor acquire-cocoa-monitor)

(deftraceable-callback window-delegate-window-did-deminiaturize-callback :void ((self :pointer) (_cmd :pointer) (notification :pointer))
  (let ((window (gethash (sap-int self) *delegate->clos-window-table*)))
    (when window
      (cocoa-window-did-deminiaturize window notification))
    (values)))

(defun cocoa-window-did-deminiaturize (window notification)
  (declare (ignorable notification))
  (let* ((content-rect (ns:|frame| (window-content-view window)))
	 (event (make-instance 'window-deiconify-event
			       :timestamp (get-internal-real-time)
			       :new-x (ns-get-x content-rect)
			       :new-y (ns-get-y content-rect)
			       :new-width (ns-get-width content-rect)
			       :new-height (ns-get-height content-rect))))
    (handle-event window event))
  (values))

(deftraceable-callback window-delegate-window-did-become-key-callback :void ((self :pointer) (_cmd :pointer) (notification :pointer))
;;  (declare (ignore notification))
  (let ((window (gethash (sap-int self) *delegate->clos-window-table*)))
    (when window
      (window-did-become-key window))
    (values)))

(defun window-did-become-key (window)
  (declare (ignorable window))
  (values))

(deftraceable-callback window-delegate-window-did-resign-key-callback :void ((self :pointer) (_cmd :pointer) (notification :pointer))
;;  (declare (ignore notification))
  (let ((window (gethash (sap-int self) *delegate->clos-window-table*)))
    (when window
      (window-did-resign-key window))
    (values)))

(defun window-did-resign-key (window)
  (declare (ignorable window))
  (values))

(deftraceable-callback window-delegate-window-did-change-occlusion-state-callback :void ((self :pointer) (_cmd :pointer) (notification :pointer))
;;  (declare (ignore notification))
  (let ((window (gethash (sap-int self) *delegate->clos-window-table*)))
    (when window
      (window-did-change-occlusion-state window))
    (values)))

(defun window-did-change-occlusion-state (window)
  (declare (ignorable window))
  (values))


    
(defun make-window-delegate-class ()
  (let ((window-delegate-class (objc-runtime::objc-allocate-class-pair
				#@NSObject "CluiWindowDelegate" 0)))
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
    (objc_registerClassPair window-delegate-class)
    window-delegate-class))

(deftraceable-callback window-make-touch-bar-callback :pointer ((self :pointer) (_cmd :pointer))
  (let ((tb (alloc-init #@NSTouchBar)))
    (ns:|setDelegate:| tb self)
    tb))

(deftraceable-callback window-touch-bar-callback :pointer ((self :pointer) (_cmd :pointer))
  (alloc-init #@NSTouchBar))

(deftraceable-callback window-touch-bar-make-item-for-identifier-callback :pointer ((self :pointer) (_cmd :pointer) (touch-bar :pointer) (identifier :pointer))
		       (cffi:null-pointer))

(deftraceable-callback content-view-responds-to-selector-callback :char ((self :pointer) (_cmd :pointer) (selector :pointer))
  (print (objc-runtime::sel-get-name selector))
  
  (prog1 (print (super-responds-to-selector self selector))
    (finish-output)))



(deftraceable-callback content-view-resolve-class-method-callback :char ((self :pointer) (_cmd :pointer) (selector :pointer))
  (print (objc-runtime::sel-get-name selector))
  (finish-output)
  (prog1 (print (super-resolve-class-method self selector))
    (finish-output)))

(deftraceable-callback content-view-key-paths-for-values-affecting-touch-bar-callback :pointer ((self :pointer) (_cmd :pointer))
  (array-with-objects))


(defun make-content-view-class (&optional (super #@NSView))
  (let ((content-view-class
	 (objc-runtime::objc-allocate-class-pair
	  super "CluiContentView" 0)))

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
    (objc-runtime::class-add-method content-view-class @(drawInMTKView:)
				    (cffi:callback content-view-draw-in-mtkview-callback)
				    "v@:@")


    (objc-runtime::class-add-method content-view-class @(makeTouchBar)
				    (cffi:callback window-make-touch-bar-callback)
				    "@@:")

    #+NIL
    (objc-runtime::class-add-method content-view-class @(touchBar)
				    (cffi:callback window-touch-bar-callback)
				    "@@:")

    (objc-runtime::class-add-method content-view-class @(touchBar:makeItemForIdentifier:)
				    (cffi:callback window-touch-bar-make-item-for-identifier-callback)
				    "@@:@@")

    #+NIL
    (objc-runtime::class-add-method content-view-class @(respondsToSelector:)
				    (cffi:callback content-view-responds-to-selector-callback)
				    "c@:@")
    #+NIL
    (objc-runtime::class-add-method content-view-class @(resolveClassMethod:)
				    (cffi:callback content-view-resolve-class-method-callback)
				    "c@:@")

    (objc-runtime::class-add-method content-view-class @(keyPathsForValuesAffectingTouchBar)
				    (cffi:callback content-view-key-paths-for-values-affecting-touch-bar-callback)
				    "@@:")

    (objc_registerClassPair content-view-class)
    
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
	  #@NSWindow "CluiWindow" 0)))
    (objc-runtime::class-add-method window-class @(canBecomeKeyWindow)
				    (cffi:callback window-can-become-key-window-callback)
				    "c@:")
    (objc-runtime::class-add-method window-class @(canBecomeMainWindow)
				    (cffi:callback window-can-become-main-window-callback)
				    "c@:")

    (objc_registerClassPair window-class)
    
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

(defun small-test ()
  #+sbcl
  (sb-int:set-floating-point-modes :traps '())
  (print objc-runtime::ns-app)
  (with-autorelease-pool (pool)
    (let ((window (ns:|initWithContentRect:styleMask:backing:defer:|
		       [#@NSWindow @(alloc)]
		       (make-nsrect 0 0 300 300)
		       (logior NSWindowStyleMaskTitled
			       NSWindowStyleMaskClosable
			       NSWindowStyleMaskMiniaturizable
			       NSWindowStyleMaskResizable)
		       NSBackingStoreBuffered nil)))
      (print objc-runtime::ns-app)
      (ns:|setTitle:| window (objc-runtime::make-nsstring "Look! A Window"))
      (print objc-runtime::ns-app)
      (ns:|setIsVisible:| window t)
      (print objc-runtime::ns-app)
      (ns:|makeKeyAndOrderFront:| window nil)
      (print objc-runtime::ns-app)
      (ns:|run| objc-runtime::ns-app)
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

(defun set-cocoa-window-monitor (window monitor &key xpos ypos width height refresh-rate)
  (declare (ignorable refresh-rate))
  (with-autorelease-pool (pool)
    
    (when (eq (window-monitor window) monitor)
      (if monitor
	  (when (eq (monitor-window monitor) window)
	    (acquire-cocoa-monitor window monitor))
	  (let* ((content-rect (make-nsrect xpos (cocoa-transform-y (1- (+ ypos height))) width height))
		 (style-mask (ns:|styleMask| window))
		 (frame-rect (ns:|frameRectForContentRect:styleMask:| window content-rect style-mask)))
	    (ns:|setFrame:display:| window frame-rect t)))
      (return-from set-cocoa-window-monitor (values)))

    (when (window-monitor window)
      (release-cocoa-monitor window (window-monitor window)))

    ;;(input-window-monitor window monitor)

    (poll-cocoa-events (window-display window))

    (let ((style-mask (ns:|styleMask| window)))

      (if (window-monitor window)
	  
	  (progn
	    (setq style-mask (logand style-mask (lognot (logior NSWindowStyleMaskTitled NSWindowStyleMaskClosable))))
	    (setq style-mask (logior style-mask NSWindowStyleMaskBorderless)))
	  
	  (progn
	    
	    (when (decorated? window)
	      (setq style-mask (logand style-mask (lognot NSWindowStyleMaskBorderless)))
	      (setq style-mask (logior style-mask NSWindowStyleMaskTitled NSWindowStyleMaskClosable)))
	    
	    (if (resizable? window)
		
		(setq style-mask (logior style-mask NSWindowStyleMaskResizable))
		
		(setq style-mask (logand style-mask (lognot NSWindowStyleMaskResizable))))))

      (ns:|setStyleMask:| window style-mask)

      (ns:|makeFirstResponder:| window (window-content-view window))

      (if (window-monitor window)

	  (progn
	    (ns:|setLevel:| window (1+ NSMainMenuWindowLevel))
	    (ns:|setHasShadow:| window nil)

	    (acquire-cocoa-monitor window (window-monitor window)))

	  (let* ((content-rect (make-nsrect xpos (cocoa-transform-y (1- (+ ypos height))) width height))
		 (frame-rect (ns:|frameRectForContentRect:styleMask:| window content-rect style-mask)))

	    (ns:|setFrame:display:| window frame-rect t)

	    (unless (or (eq (window-aspect-numer window) :dont-care)
			(eq (window-aspect-denom window) :dont-care))
	      (ns:|setContentAspectRatio:| window (make-nssize (window-numer window) (window-denom window))))

	    (unless (or (eq (window-min-width window) :dont-care)
			(eq (window-min-height window) :dont-care))
	      (ns:|setContentMinSize:| window (make-nssize (window-min-width window) (window-min-height window))))

	    (unless (or (eq (window-max-width window) :dont-care)
			(eq (window-max-height window) :dont-care))
	      (ns:|setContentMaxSize:| window (make-nssize (window-max-width window) (window-max-height window))))

	    (if (floating? window)
		(ns:|setLevel:| window NSFloatingWindowLevel)
		(ns:|setLevel:| window NSNormalWindowLevel))

	    (let ((behavior))
	      (if (resizable? window)
		  (setq behavior (logior NSWindowCollectionBehaviorFullScreenPrimary
					 NSWindowCollectionBehaviorManaged))
		  (setq behavior NSWindowCollectionBehaviorFullScreenNone))
	      
	      (ns:|setCollectionBehavior:| window behavior))

	    (ns:|setHasShadow:| window t)

	    (ns:|setTitle:| window (ns:|miniwindowTitle| window)))))))





	    

	      
