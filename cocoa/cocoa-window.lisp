(in-package :clui)
(named-readtables:in-readtable :objc-readtable)

(defvar *trace-callbacks* t)

(defvar w)

(defun translate-cocoa-key (window key)
  (let ((keycodes (display-keycodes (window-display window))))
    (when (< key (length keycodes))
      (aref keycodes key))))

(defun translate-cocoa-flags (window flags)
  (let ((mods 0)
	(lock-mods 0))
    (when (logtest flags NSEventModifierFlagShift)
      (setq mods (logior mods +shift-modifier+)))
    (when (logtest flags NSEventModifierFlagControl)
      (setq mods (logior mods +ctrl-modifier+)))
    (when (eq (aref (window-keys window) +key-left-alt+) :press)
      (when (logtest flags NSEventModifierFLagOption)
	(setq mods (logior mods +meta-modifier+))))
    (when (logtest flags NSEventModifierFlagCommand)
      (setq mods (logior mods +super-modifier+)))
    (when (logtest flags NSEventModifierFlagCapsLock)
      (setq lock-mods (logior lock-mods +caps-lock-modifier+)))
    
    (values mods lock-mods)))

(defun translate-key-to-cocoa-modifier-flag (key)
  (cond
    ((or (eq key +key-left-shift+) (eq key +key-right-shift+)) NSEventModifierFlagShift)
    ((or (eq key +key-left-ctrl+) (eq key +key-right-ctrl+)) NSEventModifierFlagControl)
    ((or (eq key +key-left-alt+) (eq key +key-right-alt+)) NSEventModifierFlagOption)
    ((or (eq key +key-left-GUI+) (eq key +key-right-GUI+)) NSEventModifierFlagCommand)
    ((eq key +key-caps-lock+) NSEventModifierFlagCapsLock)
    (t 0)))

(defun create-cocoa-standard-cursor (shape)
  (with-autorelease-pool (pool)
    (let ((cursor-selector nil)
	  (cursor-id nil))
      (setq cursor-selector
	    (case shape
	      (:ew @(_windowResizeEastWestCursor))
	      (:ns @(_windowResizeNorthSouthCursor))
	      (:nwse @(_windowResizeNorthWestSouthEastCursor))
	      (:nesw @(_windowResizeNorthEastSouthWestCursor))))

      (when (and cursor-selector (ns:|respondsToSelector:| #@NSCursor cursor-selector))
	(let ((object (ns:|performSelector:| #@NSCursor cursor-selector)))
	  (when (ns:|isKindOfClass:| object (ns:|class| #@NSCursor))
	    (setf cursor-id object))))

      (unless cursor-id
	(setq cursor-id
	      (case shape
		(:arrow (ns:|arrowCursor| #@NSCursor))
		(:ibeam (ns:|IBeamCursor| #@NSCursor))
		(:crosshair (ns:|crosshairCursor| #@NSCursor))
		(:pointing-hand (ns:|pointingHandCursor| #@NSCursor))
		(:ew (ns:|resizeLeftRightCursor| #@NSCursor))
		(:ns (ns:|resizeUpDownCursor| #@NSCursor))
		(:closed-hand (ns:|closedHandCursor| #@NSCursor))
		(:not-allowed (ns:|operationNotAllowedCursor| #@NSCursor)))))

      (when cursor-id
	(ns:|retain| cursor-id)
	cursor-id))))

(defun set-cocoa-window-cursor (window cursor)
  (declare (ignore cursor))
  (with-autorelease-pool (pool)
    (when (cocoa-cursor-in-content-area? window)
      (update-cocoa-cursor-image window)))
  (values))      

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

(defun get-cocoa-window-cursor-pos (window)
  (with-autorelease-pool (pool)
    (let ((content-rect (ns:|frame| (window-content-view window)))
	  (pos (ns:|mouseLocationOutsideOfEventStream| window)))
      (multiple-value-bind (xscale yscale) (get-cocoa-window-content-scale window)
	;; always return cursor pos in framebuffer coordinates (for your sanity)
	(values (* xscale (ns-get-x pos))
		(* yscale (- (ns-get-height content-rect) (ns-get-y pos))))))))

(defun set-cocoa-window-cursor-pos (window x y)
  ;; take x, y in framebuffer coordinates
  (multiple-value-bind (xscale yscale) (get-cocoa-window-content-scale window)
    ;; and convert to os window coordinates
    (with-autorelease-pool (pool)
      (update-cocoa-cursor-image window)
      (let ((content-rect (ns:|frame| (window-content-view window)))
	    (pos (ns:|mouseLocationOutsideOfEventStream| window)))
	;; cursor warp delta is in framebuffer coordinates
	(setf (cursor-warp-delta-x window) (+ (cursor-warp-delta-x window) x (* xscale (- (ns-get-x pos))))
	      (cursor-warp-delta-y window) (+ (cursor-warp-delta-y window) y (* yscale (- (ns-get-height content-rect)) (ns-get-y pos))))

	(if (window-fullscreen-monitor window)
	    (CGDisplayMoveCursorToPoint (monitor-display-id (window-fullscreen-monitor window)) (make-nspoint (/ x xscale) (/ y yscale)))
	    (let* ((local-rect (make-nsrect x (- (ns-get-height content-rect) y 1) 0 0))
		   (global-rect (ns:|convertRectToScreen:| window local-rect))
		   (global-point (make-nspoint (ns-get-x global-rect) (ns-get-y global-rect))))

	      (CGWarpMouseCursorPosition (make-nspoint (ns-get-x global-point)
						       (cocoa-transform-y (ns-get-y global-point))))))

	(when (eq (window-cursor-mode window) :disabled)
	  (CGAssociateMouseAndMouseCursorPosition t))

	(values)))))

(defun cocoa-cursor-in-content-area? (window)
  (let ((pos (ns:|mouseLocationOutsideOfEventStream| window))
	(view (window-content-view window)))
    (ns:|mouse:inRect:| view pos (ns:|frame| view)))) 

(defun get-cocoa-window-size (window)
  (with-autorelease-pool (pool)
    (let ((content-rect (ns::|contentRectForFrameRect:| window (ns:|frame| window))))
      (values (ns-get-width content-rect) (ns-get-height content-rect)))))

(defun set-cocoa-window-size (window width height)
  (with-autorelease-pool (pool)
    (let ((monitor (window-fullscreen-monitor window)))
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
      (deiconify-cocoa-window window)
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



(defun update-cocoa-cursor-image (window)
  (if (eq (window-cursor-mode window) :normal)
      (progn
	(show-cocoa-cursor window)
	(if (window-cursor window)
	    (ns:|set| (window-cursor window))
	    (ns:|set| (ns:|arrowCursor| #@NSCursor))))
      (hide-cocoa-cursor window)))

(defun update-cocoa-cursor-mode (window)
  (let ((display (window-display window)))
    (cond ((eq (window-cursor-mode window) :disabled)
	   (setf (disabled-cursor-window display) window)
	   (multiple-value-bind (x y) (get-cocoa-window-cursor-pos window)
	     (setf (restore-cursor-pos-x display) x)
	     (setf (restore-cursor-pos-y display) y)
	     (center-cursor-in-content-area window)
	     (CGAssociateMouseAndMouseCursorPosition nil)))
	  ((eq (disabled-cursor-window display) window)
	   (setf (disabled-cursor-window display) nil)
	   (set-cocoa-window-cursor-pos window
				 (restore-cursor-pos-x display)
				 (restore-cursor-pos-y display))))
    (when (cocoa-cursor-in-content-area? window)
      (update-cocoa-cursor-image window))
    (values)))
				 


















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

(defmethod cocoa-window-can-become-key ((window cocoa:window-mixin))
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

(defmethod cocoa-window-accepts-first-responder ((window cocoa:window-mixin))
  t)

(defmethod cocoa-window-accepts-first-responder ((window window-mixin))
  nil)

(deftraceable-callback content-view-wants-update-layer-callback :char ((self :pointer) (_cmd :pointer))
;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       *content-view->clos-content-view-table*)))
    (if content-view
      (content-view-wants-update-layer content-view)
      *no*)))

(defun content-view-wants-update-layer (view)
  ;; rather than making a separate content-view class for each window class and dispatching on content-view type
  ;; just dispatch on window type
  (if (cocoa-window-wants-update-layer (content-view-owner view))
      *yes*
      *no*))

(defmethod cocoa-window-wants-update-layer ((window cocoa:window-mixin))
  ;; for example, a method for this generic function for a metal window would answer differently
  nil)

(defmethod cocoa-window-wants-update-layer ((window cocoa::helper-window))
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

(defmethod cocoa-window-update-view-layer ((window window-mixin)))

(deftraceable-callback content-view-cursor-update-callback :void ((self :pointer) (_cmd :pointer) (event :pointer))
;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       *content-view->clos-content-view-table*)))
    (when content-view
      (content-view-cursor-update content-view event))
    (values)))

(defun content-view-cursor-update (content-view event)
  (declare (ignore event))
  (update-cocoa-cursor-image (content-view-owner content-view))
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
  (let ((time (ns:|timestamp| event))
	(window (content-view-owner content-view)))
    (multiple-value-bind (x y) (cocoa-event-position window event)
      (multiple-value-bind (mods lock-mods) (translate-cocoa-flags window (ns:|modifierFlags| event))
	(input-mouse-click (content-view-owner content-view)
			   +pointer-left-button+
			   :press
			   x y
			   mods lock-mods
			   time))))
  (values))

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
  (let ((time (ns:|timestamp| event))
	(window (content-view-owner content-view)))
    (multiple-value-bind (x y) (cocoa-event-position window event)
      (multiple-value-bind (mods lock-mods) (translate-cocoa-flags window (ns:|modifierFlags| event))
      (input-mouse-click (content-view-owner content-view)
			 +pointer-left-button+
			 :release
			 x y
			 mods lock-mods
			 time))))
  (values))

(deftraceable-callback content-view-mouse-moved-callback :void ((self :pointer) (_cmd :pointer) (event :pointer))
;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       *content-view->clos-content-view-table*)))
    (when content-view
      (content-view-mouse-moved content-view event))
    (values)))

(defun cocoa-event-position (window event)
  (multiple-value-bind (xscale yscale) (get-cocoa-window-content-scale window)
    (if (eq (window-cursor-mode window) :disabled)

	;; virtual-cursor-pos is in framebuffer coordinates
	(values (+ (virtual-cursor-pos-x window)
		   (- (* xscale (ns:|deltaX| event)) (cursor-warp-delta-x window)))
		(+ (virtual-cursor-pos-y window)
		   (- (* yscale (ns:|deltaY| event)) (cursor-warp-delta-y window))))
      
	(let* ((content-rect (ns:|frame| (window-content-view window)))
	       ;; returned position uses base 0,1 not 0,0
	       (loc (ns:|locationInWindow| event)))
	  ;; event position is in framebuffer coordinates
	  (values (* xscale (ns-get-x loc))
		  (* yscale (- (ns-get-height content-rect) (ns-get-y loc))))))))
      

(defun content-view-mouse-moved (content-view event)
  (let ((window (content-view-owner content-view))
	(timestamp (ns:|timestamp| event)))
    (multiple-value-bind (mods lock-mods) (translate-cocoa-flags window (ns:|modifierFlags| event))
      (multiple-value-bind (x y) (cocoa-event-position window event)
	(restart-bind ((ignore (lambda (&optional c)
				 (declare (ignorable c))
				 (throw :ignore nil))))
	  (catch :ignore
	    (clim:handle-event window (make-instance 'pointer-motion-event
						:window window
						:input-code +pointer-move+
						:x x
						:y y
						:native-x x
						:native-y y
						:modifier-state mods
						:lock-modifier-state lock-mods
						:timestamp timestamp
						)))))

      (setf (cursor-warp-delta-x window) 0
	    (cursor-warp-delta-y window) 0)
      
      (values))))

(deftraceable-callback content-view-right-mouse-down-callback :void ((self :pointer) (_cmd :pointer) (event :pointer))
;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       *content-view->clos-content-view-table*)))
    (when content-view
      (content-view-right-mouse-down content-view event))
    (values)))

(defun content-view-right-mouse-down (content-view event)
  (let ((time (ns:|timestamp| event))
	(window (content-view-owner content-view)))
    (multiple-value-bind (x y) (cocoa-event-position window event)
      (multiple-value-bind (mods lock-mods) (translate-cocoa-flags window (ns:|modifierFlags| event))
	(input-mouse-click (content-view-owner content-view)
			   +pointer-right-button+
			   :press
			   x y
			   mods lock-mods
			   time))))
  (values))

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
  (let ((time (ns:|timestamp| event))
	(window (content-view-owner content-view)))
    (multiple-value-bind (x y) (cocoa-event-position window event)
      (multiple-value-bind (mods lock-mods) (translate-cocoa-flags window (ns:|modifierFlags| event))
	(input-mouse-click (content-view-owner content-view)
			   +pointer-right-button+
			   :release
			   x y
			   mods lock-mods
			   time))))
  (values))

(deftraceable-callback content-view-other-mouse-down-callback :void ((self :pointer) (_cmd :pointer) (event :pointer))
;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       *content-view->clos-content-view-table*)))
    (when content-view
      (content-view-other-mouse-down content-view event))
    (values)))

(defun content-view-other-mouse-down (content-view event)
  (let ((time (ns:|timestamp| event))
	(window (content-view-owner content-view)))
    (multiple-value-bind (x y) (cocoa-event-position window event)
      (multiple-value-bind (mods lock-mods) (translate-cocoa-flags window (ns:|modifierFlags| event))
	(input-mouse-click (content-view-owner content-view)
			   (aref (vector +pointer-right-button+
					 +pointer-left-button+
					 +pointer-middle-button+
					 +pointer-button-4+
					 +pointer-button-5+)
				 (ns:|buttonNumber| event))
			   :press
			   x y
			   mods lock-mods
			   time))))
  (values))

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
  (let ((time (ns:|timestamp| event))
	(window (content-view-owner content-view)))
    (multiple-value-bind (x y) (cocoa-event-position window event)
      (multiple-value-bind (mods lock-mods) (translate-cocoa-flags window (ns:|modifierFlags| event))
	(input-mouse-click (content-view-owner content-view)
			   (aref (vector +pointer-right-button+
					 +pointer-left-button+
					 +pointer-middle-button+
					 +pointer-button-4+
					 +pointer-button-5+)
				 (ns:|buttonNumber| event))
			   :release
			   x y
			   mods lock-mods
			   time))))
  (values))

(defun show-cocoa-cursor (window)
  (let ((display (window-display window)))
    (when (cursor-hidden? display)
      (ns:|unhide| #@NSCursor)
      (setf (cursor-hidden? display) nil))
    (values)))

(defun hide-cocoa-cursor (window)
  (let ((display (window-display window)))
    (unless (cursor-hidden? display)
      (ns:|hide| #@NSCursor)
      (setf (cursor-hidden? display) t))
    (values)))

(deftraceable-callback content-view-mouse-exited-callback :void ((self :pointer) (_cmd :pointer) (event :pointer))
;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       *content-view->clos-content-view-table*)))
    (when content-view
      (content-view-mouse-exited content-view event))
    (values)))

(defun content-view-mouse-exited (view event)
  (let ((window (content-view-owner view)))
    (on-cocoa-mouse-exited window event)))



(defmethod on-cocoa-mouse-exited ((window window-mixin) event)
  (when (eq (window-cursor-mode window) :hidden)
    (show-cocoa-cursor window))
  (let* ((content-rect (ns:|frame| (window-content-view window)))
	 (loc (ns:|locationInWindow| event))
	 (x (ns-get-x loc))
	 (y (- (ns-get-y content-rect) (ns-get-y loc))))
    (multiple-value-bind (mods lock-mods) (translate-cocoa-flags window (ns:|modifierFlags| event))
      (restart-bind ((ignore (lambda (&optional c)
			       (declare (ignorable c))
			       (throw :ignore nil))))
	(catch :ignore
	  (clim:handle-event window (make-instance 'pointer-exit-event
					      :window window
					      :input-code +pointer-move+
					      :x x
					      :y y
					      :native-x x
					      :native-y y
					      :modifier-state mods
					      :lock-modifier-state lock-mods))))))
  (values))



(deftraceable-callback content-view-mouse-entered-callback :void ((self :pointer) (_cmd :pointer) (event :pointer))
;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       *content-view->clos-content-view-table*)))
    (when content-view
      (content-view-mouse-entered content-view event))
    (values)))

(defun content-view-mouse-entered (view event)

  (let ((window (content-view-owner view)))
    (on-cocoa-mouse-entered window event)))

(defmethod on-cocoa-mouse-entered ((window cocoa:window-mixin) event)
  (when (eq (window-cursor-mode window) :hidden)
    (hide-cocoa-cursor window))
  (let* ((content-rect (ns:|frame| (window-content-view window)))
	 (loc (ns:|locationInWindow| event))
	 (x (ns-get-x loc))
	 (y (- (ns-get-y content-rect) (ns-get-y loc))))
    (multiple-value-bind (mods lock-mods) (translate-cocoa-flags window (ns:|modifierFlags| event))
      (restart-bind ((ignore (lambda (&optional c)
			       (declare (ignorable c))
			       (throw :ignore nil))))
	(catch :ignore
	  (clim:handle-event window (make-instance 'pointer-enter-event
					      :window window
					      :input-code +pointer-move+
					      :x x
					      :y y
					      :native-x x
					      :native-y y
					      :modifier-state mods
					      :lock-modifier-state lock-mods))))))
  (values))

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

(defmethod cocoa-window-did-change-backing-properties ((window cocoa::helper-window))
  (values))

(defmethod cocoa-window-did-change-backing-properties ((window constant-refresh-os-window-mixin))
  (let* ((content-view (window-content-view window))
	 (content-rect (ns:|frame| content-view))
	 (fb-rect (ns:|convertRectToBacking:| content-view content-rect))
	 (x-scale (/ (ns-get-width fb-rect) (ns-get-width content-rect)))
	 (y-scale (/ (ns-get-height fb-rect) (ns-get-height content-rect))))

    (when (or (/= x-scale (last-xscale window))
	      (/= y-scale (last-yscale window)))
      (when (and (window-retina? window) (window-layer window))
	(ns:|setContentsScale:| (window-layer window) (ns:|backingScaleFactor| window)))

      (setf (last-xscale window) x-scale
	    (last-yscale window) y-scale)
      (input-window-content-scale window x-scale y-scale))

    (when (or (/= (ns-get-width fb-rect) (last-fbwidth window))
	      (/= (ns-get-height fb-rect) (last-fbheight window)))
      (input-framebuffer-size window (ns-get-width fb-rect) (ns-get-height fb-rect))))
    
  (values))

(cffi:defcallback content-view-draw-rect-callback :void ((self :pointer) (_cmd :pointer) (rect :pointer))
  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       *content-view->clos-content-view-table*)))
    (when content-view
      (content-view-draw-rect (content-view-owner content-view) content-view rect))
    (values)))

(defmethod content-view-draw-rect ((window cocoa:window-mixin) (view content-view) rect)
  (declare (ignorable rect))
  (format t "~%running draw-rect")
  (finish-output)
  (ns:|set| (ns:|whiteColor| #@NSColor))
  (ns:|strokeRect:| #@NSBezierPath (make-nsrect 0 0 1 1))
  (NSRectFill (ns:|bounds| view))
  (ns:|flushGraphics| (window-graphics-context window))
  )

(defmethod content-view-draw-rect ((window constant-refresh-os-window-mixin) (view content-view) rect)
  (declare (ignorable view rect))
  (values))

(deftraceable-callback content-view-update-tracking-areas-callback :void ((self :pointer) (_cmd :pointer))
;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self) *content-view->clos-content-view-table*)))
    (when content-view
      (content-view-update-tracking-areas content-view))
    (values)))

(defun content-view-update-tracking-areas (view)
  (cocoa-window-update-tracking-areas (content-view-owner view)))

(defmethod cocoa-window-update-tracking-areas ((window cocoa:window-mixin))
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

(defmethod cocoa-window-update-tracking-areas ((window cocoa::helper-window))
  (values))

(deftraceable-callback content-view-key-down-callback :void ((self :pointer) (_cmd :pointer) (event :pointer))
;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       *content-view->clos-content-view-table*)))
    (when content-view
      (content-view-key-down content-view event))
    (values)))

(defun content-view-key-down (view event)
  (cocoa-window-key-down (content-view-owner view) event))



(defmethod cocoa-window-key-down ((window cocoa:window-mixin) event)
  (let ((key (translate-cocoa-key window (ns:|keyCode| event))))
    (let ((time (ns:|timestamp| event))
	  (pos (ns:|locationInWindow| event)))
      (multiple-value-bind (mods lock-mods) (translate-cocoa-flags window (ns:|modifierFlags| event))
	(input-key window
		   key :press
		   (ns-get-x pos) (ns-get-y pos)
		   mods lock-mods
		   time)))
    (ns:|interpretKeyEvents:| (window-content-view window) (array-with-objects event))))

(deftraceable-callback content-view-flags-changed-callback :void ((self :pointer) (_cmd :pointer) (event :pointer))
;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       *content-view->clos-content-view-table*)))
    (when content-view
      (content-view-flags-changed content-view event))
    (values)))



(defun content-view-flags-changed (view event)
  (let* ((modifier-flags (logand (ns:|modifierFlags| event) NSEventModifierFlagDeviceIndependentFlagsMask))
	 (window (content-view-owner view))
	 (key (translate-cocoa-key window (ns:|keyCode| event)))
	 (time (ns:|timestamp| event))
	 (action))
    (print key)
    (finish-output)
    (multiple-value-bind (x y) (cocoa-event-position window event)
      (multiple-value-bind (mods lock-mods) (translate-cocoa-flags window modifier-flags)
	(let ((key-flag (translate-key-to-cocoa-modifier-flag key)))

	  (if (logtest modifier-flags key-flag)
	      (if (eq (aref (window-keys window) key) :press)
		  (setq action :release)
		  (setq action :press))
	      (setq action :release))
	  (input-key window key action x y mods lock-mods time))))
    (values)))

(deftraceable-callback content-view-key-up-callback :void ((self :pointer) (_cmd :pointer) (event :pointer))
;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       *content-view->clos-content-view-table*)))
    (when content-view
      (content-view-key-up content-view event))
    (values)))

(defun content-view-key-up (view event)
  (cocoa-window-key-up (content-view-owner view) event))

(defmethod cocoa-window-key-up ((window cocoa:window-mixin) event)
  (let ((key (translate-cocoa-key window (ns:|keyCode| event))))
    (let ((time (ns:|timestamp| event))
	  (pos (ns:|locationInWindow| event)))
      (multiple-value-bind (mods lock-mods) (translate-cocoa-flags window (ns:|modifierFlags| event))
	(input-key window
		   key :release
		   (ns-get-x pos) (ns-get-y pos)
		   mods lock-mods
		   time))))
  (values))

(defun cocoa-transform-y (y)
  (declare (type real y))
  (coerce (1- (- (ns-get-height (CGDisplayBounds (CGMainDisplayID))) y)) 'double-float))

(deftraceable-callback content-view-scroll-wheel-callback :void ((self :pointer) (_cmd :pointer) (event :pointer))
;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       *content-view->clos-content-view-table*)))
    (when content-view
      (content-view-scroll-wheel content-view event))
    (values)))

(defun content-view-scroll-wheel (view event)
  (let ((time (ns:|timestamp| event))
	(pos (ns:|locationInWindow| event))
	(window (content-view-owner view)))
    (multiple-value-bind (mods lock-mods) (translate-cocoa-flags window (ns:|modifierFlags| event))
      (clim:handle-event window
		    (make-instance 'pointer-wheel-event
				   :window window
				   :input-code +pointer-wheel+
				   :x (ns-get-x pos)
				   :y (ns-get-y pos)
				   :xoffset (ns:|scrollingDeltaX| event)
				   :yoffset (ns:|scrollingDeltaY| event)
				   :modifier-state mods
				   :lock-modifier-state lock-mods
				   :timestamp time))))
  (values))

(deftraceable-callback content-view-has-marked-text-callback :char ((self :pointer) (_cmd :pointer))
;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       *content-view->clos-content-view-table*)))
    (if content-view
	(content-view-has-marked-text content-view)
	*no*)))

(defun content-view-has-marked-text (view)
  (plusp (ns:|length| (content-view-marked-text view))))

(deftraceable-callback content-view-marked-range-callback :unsigned-long-long ((self :pointer) (_cmd :pointer))
;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       *content-view->clos-content-view-table*)))
    (if content-view
	(content-view-marked-range content-view)
	0)))

(defun content-view-marked-range (view)
  (declare (ignore view))
  0)

(deftraceable-callback content-view-selected-range-callback :unsigned-long-long ((self :pointer) (_cmd :pointer))
;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       *content-view->clos-content-view-table*)))
    (if content-view
	(content-view-selected-range content-view)
	0)))

(defun content-view-selected-range (view)
  (declare (ignore view))
  0)

(deftraceable-callback content-view-set-marked-text-selected-range-replacement-range-callback :void ((self :pointer) (_cmd :pointer)
												     (string :pointer))
;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       *content-view->clos-content-view-table*)))
    (if content-view
	(content-view-set-marked-text-selected-range-replacement-range content-view string)
	(values))))

(defun content-view-set-marked-text-selected-range-replacement-range (view string)
  (ns:|release| (content-view-marked-text view))
  (setf (content-view-marked-text view)
	(if (ns:|isKindOfClass:| string (ns::|class| #@NSAttributedString))
	    (ns::|initWithAttributedString:| (ns:|alloc| #@NSMutableAttributedString) string)
	    (ns:|initWithString:| (ns:|alloc| #@NSMutableAttributedString) string)))
  (values))

(deftraceable-callback content-view-unmark-text-callback :void ((self :pointer) (_cmd :pointer))
  ;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       *content-view->clos-content-view-table*)))
    (if content-view
	(content-view-unmark-text content-view)
	(values))))

(defun content-view-unmark-text (view)
  (setf (content-view-marked-text view) (alloc-init #@NSMutableAttributedString)))

(deftraceable-callback content-view-valid-attributes-for-marked-text-callback :pointer ((self :pointer) (_cmd :pointer))
  ;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       *content-view->clos-content-view-table*)))
    (if content-view
	(content-view-valid-attributes-for-marked-text content-view)
	(cffi:null-pointer))))

(defun content-view-valid-attributes-for-marked-text (view)
  (declare (ignore view))
  (ns:|array| #@NSArray))

(deftraceable-callback content-view-attributed-substring-for-proposed-range-actual-range-callback :pointer
    ((self :pointer) (_cmd :pointer))
  ;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       *content-view->clos-content-view-table*)))
    (if content-view
	(content-view-attributed-substring-for-proposed-range-actual-range content-view)
	(cffi:null-pointer))))

(defun content-view-attributed-substring-for-proposed-range-actual-range (view)
  (declare (ignore view))
  (cffi:null-pointer))

(deftraceable-callback content-view-character-index-for-point-callback :unsigned-long-long ((self :pointer) (_cmd :pointer))
  ;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       *content-view->clos-content-view-table*)))
    (if content-view
	(content-view-character-index-for-point content-view)
	0)))

(defun content-view-character-index-for-point (view)
  (declare (ignore view))
  0)

(deftraceable-callback content-view-first-rect-for-character-range-actual-range-callback :unsigned-long-long
    ((self :pointer) (_cmd :pointer))
  ;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       *content-view->clos-content-view-table*)))
    (if content-view
	(content-view-first-rect-for-character-range-actual-range content-view)
	0)))

(defun content-view-first-rect-for-character-range-actual-range (view)
  (declare (ignore view))
  0)

(deftraceable-callback content-view-insert-text-replacement-range-callback :void ((self :pointer) (_cmd :pointer) (string :pointer)
										  (replacement-range :unsigned-long-long #+NIL (:struct ns::|_NSRange|)))
;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       *content-view->clos-content-view-table*)))
    (when content-view
      (content-view-insert-text content-view string replacement-range))
    (values)))

(deftraceable-callback content-view-insert-text-callback :void ((self :pointer) (_cmd :pointer) (string :pointer))
;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       *content-view->clos-content-view-table*)))
    (when content-view
      (content-view-insert-text content-view string))
    (values)))

(defun content-view-insert-text (view string &optional replacement-range)
  (declare (ignore replacement-range))
  (let* ((characters)
	 (window (content-view-owner view))
	 (event (ns:|currentEvent| (window-display window))))
    (multiple-value-bind (mods lock-mods) (translate-cocoa-flags window (ns:|modifierFlags| event))
      (let ((plain? (not (logtest mods +super-modifier+))))

	(if (ns:|isKindOfClass:| string (ns:|class| #@NSAttributedString))
	    (setq characters (ns:|string| string))
	    (setq characters string))

	(let ((range))
	  (cffi:with-foreign-objects ((&codepoint :uint32)
				      (&remaining-range '(:struct ns::|_NSRange|)))
	    (setf (cffi:foreign-slot-value &remaining-range '(:struct ns::|_NSRange|) 'ns::location) 0
		  (cffi:foreign-slot-value &remaining-range '(:struct ns::|_NSRange|) 'ns::length) (ns:|length| characters))
	    (loop until (zerop (cffi:foreign-slot-value &remaining-range '(:struct ns::|_NSRange|) 'ns::length))
	       do (setf (cffi:mem-aref &codepoint :uint32) 0)
		 (setq range (make-nsrange (cffi:foreign-slot-value &remaining-range '(:struct ns::|_NSRange|) 'ns::location)
					   (cffi:foreign-slot-value &remaining-range '(:struct ns::|_NSRange|) 'ns::length)))
		 (when (ns:|getBytes:maxLength:usedLength:encoding:options:range:remainingRange:|
			   characters &codepoint (load-time-value (cffi:foreign-type-size :uint32))
			   nil NSUTF32StringEncoding 0 range &remaining-range)
		   (unless (and (>= (cffi:mem-aref &codepoint :uint32) #xf700)
				(<= (cffi:mem-aref &codepoint :uint32) #xf7ff))
		     (input-char window (cffi:mem-aref &codepoint :uint32) mods lock-mods plain?)))))))))
  (values))

(deftraceable-callback content-view-do-command-by-selector-callback :void ((self :pointer) (_cmd :pointer) (selector :pointer))
;;  (declare (ignorable _cmd))
  (let ((content-view (gethash (sap-int self)
			       *content-view->clos-content-view-table*)))
    (when content-view
      (content-view-do-command-by-selector content-view selector))
    (values)))

(defun content-view-do-command-by-selector (view selector)
  (declare (ignore view selector))
  (values))


(defun %create-native-cocoa-window (window &rest args
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
    (if (window-fullscreen-monitor window)
	
	(multiple-value-bind (xpos ypos) (get-cocoa-monitor-pos (window-fullscreen-monitor window))
	  (let* ((mode (get-cocoa-monitor-video-mode (window-fullscreen-monitor window)))
		 (width (video-mode-width mode))
		 (height (video-mode-height mode)))

	    (setq content-rect (make-nsrect xpos ypos width height))))

	(if (or (null xpos) (null ypos))
	    (setq content-rect (make-nsrect 0 0 (or width 640) (or height 480)))

	    (setq content-rect (make-nsrect xpos
					    (cocoa-transform-y (1- (+ ypos (or height 480.0d0))))
					    (or width 640) (or height 480)))))

    (let ((style-mask NSWindowStyleMaskMiniaturizable))

      (if (or (window-fullscreen-monitor window) (not decorated?))

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

      (setf (last-cocoa-screen window) (ns::|screen| window))

      (ns::|addObserver:selector:name:object:|
	   (ns::|defaultCenter| #@NSNotificationCenter)
	   (window-content-view window)
	   @(windowDidMove:)
	   (objc-runtime::make-nsstring
	    "NSWindowDidMoveNotification")
	   nil)

      (apply #'initialize-window-devices window args)
	
      (if (window-fullscreen-monitor window)

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

      t)))

(defmethod initialize-window-devices ((window cocoa:window-mixin) &rest args &key &allow-other-keys)
  (declare (ignore args))
  (setf (window-graphics-context window)
	(ns:|graphicsContextWithWindow:| #@NSGraphicsContext window))
  (values))

(defun create-native-cocoa-window (window &rest initargs
				   &key (visible? t)
				     (focused? t)
				     (auto-iconify? t)
				     (focus-on-show? t)
				     (center-cursor? t)
				     (mouse-passthrough? nil)
				   &allow-other-keys)
  (declare (ignorable auto-iconify? focus-on-show?))
  
  (with-autorelease-pool (pool)
  
    (apply #'%create-native-cocoa-window window initargs)

    (when mouse-passthrough?
      #+NOTYET
      (set-cocoa-window-mouse-passthrough window t))

    (if (window-fullscreen-monitor window)
	(progn
	  (show-cocoa-window window)
	  (focus-cocoa-window window)
	  (acquire-cocoa-monitor window (window-fullscreen-monitor window))
	  (when center-cursor?
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
      (setf (monitor-window monitor) window)
      (values))))

(defun release-cocoa-monitor (window)
  (unless (eq (monitor-window (window-fullscreen-monitor window)) window)
    (return-from release-cocoa-monitor (values)))
  (setf (monitor-window (window-fullscreen-monitor window)) nil)
  (restore-cocoa-monitor-video-mode (window-fullscreen-monitor window))
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
      (restart-bind ((ignore (lambda (&optional c)
			       (declare (ignorable c))
			       (throw :ignore nil))))
	(catch :ignore
	  (clim:handle-event window (make-instance 'window-close-event
					      :window window
					      :timestamp (get-internal-real-time))))))
    (values)))



  

(deftraceable-callback window-delegate-window-did-resize-callback :void ((self :pointer) (_cmd :pointer) (notification :pointer))
  (let ((window (gethash (sap-int self) *delegate->clos-window-table*)))
    (when window
      (cocoa-window-did-resize window notification))
    (values)))

(defun cocoa-window-did-resize (window notification)
  (declare (ignorable notification))
  (when (eq window (disabled-cursor-window (window-display window)))
    (center-cursor-in-content-area window))
  (let* ((view (window-content-view window))
	 (content-rect (ns:|frame| view))
	 (fb-rect (ns:|convertRectToBacking:| view content-rect)))
    (let* ((maximized? (ns:|isZoomed| window))
	   (event (make-instance (cond (maximized? 'window-restore-event)
				       ((not maximized?) 'window-resize-event))
				 :timestamp (get-internal-real-time)
				 :new-x (ns-get-x fb-rect)
				 :new-y (ns-get-y fb-rect)
				 :new-width (ns-get-width fb-rect)
				 :new-height (ns-get-height fb-rect))))
      (restart-bind ((ignore (lambda (&optional c)
			       (declare (ignorable c))
			       (throw :ignore nil))))
	(catch :ignore
	  (clim:handle-event window event))))))

(deftraceable-callback window-delegate-window-did-move-callback :void ((self :pointer) (_cmd :pointer) (notification :pointer))
  (let ((window (gethash (sap-int self) *delegate->clos-window-table*)))
    (when window
      (cocoa-window-did-move window notification))
    (values)))

(defun cocoa-window-did-move (window notification)
  (declare (ignorable notification))
  (let ((timestamp (get-internal-real-time)))
     
    (let ((current-screen (ns::|screen| window))
	  (last-screen (last-cocoa-screen window)))
      (setf (last-cocoa-screen window) current-screen)
      (when last-screen
	(unless (= (sap-int current-screen) (sap-int last-screen))
	  (clim:handle-event window (make-instance 'window-monitor-switched-event
						   :window window
						   :timestamp timestamp)))))
  
    (multiple-value-bind (x y) (get-cocoa-window-pos window)
      (restart-bind ((ignore (lambda (&optional c)
			       (declare (ignorable c))
			       (throw :ignore nil))))
	(catch :ignore
	  (let ((event (make-instance 'window-move-event
				      :timestamp timestamp
				      :window window
				      :new-x x
				      :new-y y)))
	    (clim:handle-event window event)))))
    
    (values)))

(deftraceable-callback window-delegate-window-did-miniaturize-callback :void ((self :pointer) (_cmd :pointer) (notification :pointer))
  (let ((window (gethash (sap-int self) *delegate->clos-window-table*)))
    (when window
      (cocoa-window-did-miniaturize window notification))
    (values)))

(defun cocoa-window-did-miniaturize (window notification)
  (declare (ignorable notification))
  (restart-bind ((ignore (lambda (&optional c)
			   (declare (ignorable c))
			   (throw :ignore nil))))
    (catch :ignore
      (let ((event (make-instance 'window-iconify-event
				  :timestamp (get-internal-real-time)
				  :new-x nil
				  :new-y nil
				  :new-width 0
				  :new-height 0)))
	(clim:handle-event window event))))
  (values))

(deftraceable-callback window-delegate-window-did-deminiaturize-callback :void ((self :pointer) (_cmd :pointer) (notification :pointer))
  (let ((window (gethash (sap-int self) *delegate->clos-window-table*)))
    (when window
      (cocoa-window-did-deminiaturize window notification))
    (values)))

(defun cocoa-window-did-deminiaturize (window notification)
  (declare (ignorable notification))
  (let* ((view (window-content-view window))
	 (content-rect (ns:|frame| view))
	 (fb-rect (ns:|convertRectToBacking:| view content-rect))
	 (event (make-instance 'window-deiconify-event
			       :timestamp (get-internal-real-time)
			       :new-x (ns-get-x fb-rect)
			       :new-y (ns-get-y fb-rect)
			       :new-width (ns-get-width fb-rect)
			       :new-height (ns-get-height fb-rect))))
    (restart-bind ((ignore (lambda (&optional c)
			     (declare (ignorable c))
			     (throw :ignore nil))))
      (catch :ignore
	(clim:handle-event window event))))
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

#+NIL
(deftraceable-callback content-view-window-did-move-callback :void ((self :pointer) (_cmd :pointer)
									  (notification :pointer))

  (window-did-move self))

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
    (objc-runtime::class-add-method content-view-class @(acceptsFirstMouse:)
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
    (objc-runtime::class-add-method content-view-class @(mouseExited:)
				    (cffi:callback content-view-mouse-exited-callback)
				    "v@:@")
    (objc-runtime::class-add-method content-view-class @(mouseEntered:)
				    (cffi:callback content-view-mouse-entered-callback)
				    "v@:@")
    (objc-runtime::class-add-method content-view-class @(viewDidChangeBackingProperties)
				    (cffi:callback content-view-view-did-change-backing-properties-callback)
				    "v@:")
    (objc-runtime::class-add-method content-view-class @(drawRect:)
				    (cffi:callback content-view-draw-rect-callback)
				    "v@:@")
    (objc-runtime::class-add-method content-view-class @(updateTrackingAreas)
				    (cffi:callback content-view-update-tracking-areas-callback)
				    "v@:")
    (objc-runtime::class-add-method content-view-class @(keyDown:)
				    (cffi:callback content-view-key-down-callback)
				    "v@:@")
    (objc-runtime::class-add-method content-view-class @(flagsChanged:)
				    (cffi:callback content-view-flags-changed-callback)
				    "v@:@")
    (objc-runtime::class-add-method content-view-class @(keyUp:)
				    (cffi:callback content-view-key-up-callback)
				    "v@:@")
    (objc-runtime::class-add-method content-view-class @(scrollWheel:)
				    (cffi:callback content-view-scroll-wheel-callback)
				    "v@:@")

    (objc-runtime::class-add-method content-view-class @(hasMarkedText)
				    (cffi:callback content-view-has-marked-text-callback)
				    "c@:")

    (objc-runtime::class-add-method content-view-class @(markedRange)
				    (cffi:callback content-view-marked-range-callback)
				    "{_NSRange=QQ}@:")
    (objc-runtime::class-add-method content-view-class @(selectedRange)
				    (cffi:callback content-view-selected-range-callback)
				    "{_NSRange=QQ}@:")
    (objc-runtime::class-add-method content-view-class @(setMarkedText:selectedRange:replacementRange:)
				    (cffi:callback content-view-set-marked-text-selected-range-replacement-range-callback)
				    "v@:@{_NSRange=QQ}{_NSRange=QQ}")
    (objc-runtime::class-add-method content-view-class @(unmarkText)
				    (cffi:callback content-view-unmark-text-callback)
				    "v@:")
    (objc-runtime::class-add-method content-view-class @(validAttributesForMarkedText)
				    (cffi:callback content-view-valid-attributes-for-marked-text-callback)
				    "@@:")
    (objc-runtime::class-add-method content-view-class @(attributedSubstringForProposedRange:actualRange:)
				    (cffi:callback content-view-attributed-substring-for-proposed-range-actual-range-callback)
				    "@@:{_NSRange=QQ}@")
    (objc-runtime::class-add-method content-view-class @(characterIndexForPoint:)
				    (cffi:callback content-view-character-index-for-point-callback)
				    "Q@:{CGPoint=dd}")
    (objc-runtime::class-add-method content-view-class @(firstRectForCharacterRange:actualRange:)
				    (cffi:callback content-view-first-rect-for-character-range-actual-range-callback)
				    "{CGRect={CGPoint=dd}{CGSize=dd}}@:{_NSRange=QQ}@")
    (objc-runtime::class-add-method content-view-class @(insertText:replacementRange:)
				    (cffi:callback content-view-insert-text-replacement-range-callback)
				    "v@:@{_NSRange=QQ}")
    (objc-runtime::class-add-method content-view-class @(insertText:)
				    (cffi:callback content-view-insert-text-callback)
				    "v@:@")
    (objc-runtime::class-add-method content-view-class @(doCommandBySelector:)
				    (cffi:callback content-view-do-command-by-selector-callback)
				    "v@::")

    
    
    (objc-runtime::class-add-method content-view-class @(drawInMTKView:)
				    (cffi:callback content-view-draw-in-mtkview-callback)
				    "v@:@")
    #+NIL
    (objc-runtime::class-add-method content-view-class @(windowDidMove:)
				    (cffi:callback content-view-window-did-move-callback)
				    "v@:@")



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

(defun create-cocoa-helper-window (display)
  (make-instance (helper-window-class display)
		 :h (ns:|initWithContentRect:styleMask:backing:defer:|
			(ns:|alloc| #@NSWindow)
			(make-nsrect 0 0 1 1)
			0
			NSBackingStoreBuffered nil)))


(defun small-test ()
  #+sbcl
  (sb-int:set-floating-point-modes :traps '())
  (print objc-runtime::ns-app)
  (finish-output)
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

(defun set-cocoa-window-fullscreen-monitor (window monitor &key xpos ypos width height refresh-rate)
  (declare (ignorable refresh-rate))
  (with-autorelease-pool (pool)
    
    (when (eq (window-fullscreen-monitor window) monitor)
      (if monitor
	  (when (eq (monitor-window monitor) window)
	    (acquire-cocoa-monitor window monitor))
	  (let* ((content-rect (make-nsrect xpos (cocoa-transform-y (1- (+ ypos height))) width height))
		 (style-mask (ns:|styleMask| window))
		 (frame-rect (ns:|frameRectForContentRect:styleMask:| window content-rect style-mask)))
	    (ns:|setFrame:display:| window frame-rect t)))
      (return-from set-cocoa-window-fullscreen-monitor (values)))

    (when (window-fullscreen-monitor window)
      (release-cocoa-monitor window))

    (setf (window-fullscreen-monitor window) monitor)

    (poll-cocoa-events (window-display window))

    (let ((style-mask (ns:|styleMask| window)))

      (if (window-fullscreen-monitor window)
	  
	  (progn
	    (setq style-mask (logand style-mask (lognot (logior NSWindowStyleMaskTitled NSWindowStyleMaskClosable))))
	    (setq style-mask (logior style-mask NSWindowStyleMaskBorderless)))
	  
	  (progn
	    
	    (when (last-decorated? window)
	      (setq style-mask (logand style-mask (lognot NSWindowStyleMaskBorderless)))
	      (setq style-mask (logior style-mask NSWindowStyleMaskTitled NSWindowStyleMaskClosable)))
	    
	    (if (last-resizable? window)
		
		(setq style-mask (logior style-mask NSWindowStyleMaskResizable))
		
		(setq style-mask (logand style-mask (lognot NSWindowStyleMaskResizable))))))

      (ns:|setStyleMask:| window style-mask)

      (ns:|makeFirstResponder:| window (window-content-view window))

      (if (window-fullscreen-monitor window)

	  (progn
	    (ns:|setLevel:| window (1+ NSMainMenuWindowLevel))
	    (ns:|setHasShadow:| window nil)

	    (acquire-cocoa-monitor window (window-fullscreen-monitor window)))

	  (let* ((content-rect (make-nsrect xpos (cocoa-transform-y (1- (+ ypos height))) width height))
		 (frame-rect (ns:|frameRectForContentRect:styleMask:| window content-rect style-mask)))

	    (ns:|setFrame:display:| window frame-rect t)

	    (unless (or (eq (window-aspect-numer window) :dont-care)
			(eq (window-aspect-denom window) :dont-care))
	      (ns:|setContentAspectRatio:| window (make-nssize (window-aspect-numer window) (window-aspect-denom window))))

	    (unless (or (eq (window-min-width window) :dont-care)
			(eq (window-min-height window) :dont-care))
	      (ns:|setContentMinSize:| window (make-nssize (window-min-width window) (window-min-height window))))

	    (unless (or (eq (window-max-width window) :dont-care)
			(eq (window-max-height window) :dont-care))
	      (ns:|setContentMaxSize:| window (make-nssize (window-max-width window) (window-max-height window))))

	    (if (last-floating? window)
		(ns:|setLevel:| window NSFloatingWindowLevel)
		(ns:|setLevel:| window NSNormalWindowLevel))

	    (let ((behavior))
	      (if (last-resizable? window)
		  (setq behavior (logior NSWindowCollectionBehaviorFullScreenPrimary
					 NSWindowCollectionBehaviorManaged))
		  (setq behavior NSWindowCollectionBehaviorFullScreenNone))
	      
	      (ns:|setCollectionBehavior:| window behavior))

	    (ns:|setHasShadow:| window t)

	    (ns:|setTitle:| window (ns:|miniwindowTitle| window)))))))

(defun get-cocoa-window-monitor (window)
  (let* ((screen (ns::|screen| window))
	 (description (ns::|deviceDescription| screen)) ;; is this unique
	 (screen-number-object
	  (ns::|objectForKey:| description (objc-runtime::make-nsstring "NSScreenNumber")))
	 (screen-number (ns::|unsignedIntValue| screen-number-object))
	 (unit-number (CGDisplayUnitNumber screen-number))
	 (display (window-display window)))
    (unless (display-monitors display)
      (poll-cocoa-monitors display))
    (mapcar #'(lambda (monitor)
		(when (= (monitor-unit-number monitor) unit-number)
		  (return-from get-cocoa-window-monitor monitor)))
	    (display-monitors display))))    

(defun destroy-cocoa-window (window)
  (with-autorelease-pool (pool)

    (let ((display (window-display window)))
    
      (when (eq (disabled-cursor-window display) window)
	(setf (disabled-cursor-window display) nil))
      
      (ns:|orderOut:| window nil)
    
      (when (window-fullscreen-monitor window)
	(release-cocoa-monitor window))
      
      (ns:|setDelegate:| window nil)
      (ns:|release| (window-delegate window))
      (setf (window-delegate window) nil)
      
      (ns:|release| (window-content-view window))
      (setf (window-content-view window) nil)
      
      (ns:|close| window)
      
      (poll-cocoa-events display)

      (values))))
