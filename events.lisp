(in-package :clui)

(defgeneric handle-event (client event))

(defclass event-mixin ()
  ((timestamp :initarg :timestamp
	      :accessor event-timestamp)))

(defmethod handle-event ((window window-mixin) (event event-mixin))
  (values))

#+NOTYET
(defmethod handle-event ((window desktop-mixin) (event event-mixin))
  (values))

;; timer event

(defclass clui.v0:timeout-event (event-mixin)
  ())

;; window-event

(defclass window-event-mixin (event-mixin)
  ((region :initarg :region
	   :initform nil
	   :accessor window-event-region)
   
   (native-region :initarg :region
		  :initform nil
		  :accessor window-event-native-region)))

;; window-configuration-event

(defclass window-configuration-event-mixin (window-event-mixin)
  ())

;; window-configuration-event
;; window-position-event-mixin

(defclass window-position-event-mixin (window-configuration-event-mixin)
  ((new-x :initarg :new-x
	  :initform nil
	  :accessor window-position-event-new-x)
   
   (new-y :initarg :new-y
	  :initform nil
	  :accessor window-position-event-new-y)))


(defclass clui.v0:window-move-event (window-position-event-mixin)
  ())

;; window-resize-event-mixin

(defclass window-resize-event-mixin (window-position-event-mixin)
  ((new-width :initarg :new-width
	      :accessor window-resize-event-new-width)
   
   (new-height :initarg :new-height
	       :accessor window-resize-event-new-height)))

(defclass clui.v0:window-resize-event (window-resize-event-mixin)
  ())

(defclass window-iconify-event-mixin (window-resize-event-mixin)
  ())

(defclass clui.v0:window-iconify-event (window-iconify-event-mixin)
  ())

(defmethod handle-event ((window window-mixin) (event window-iconify-event-mixin))
  (maybe-release-monitor window)
  (call-next-method)
  (values))

(defclass window-deiconify-event-mixin (window-resize-event-mixin)
  ())

(defclass clui.v0:window-deiconify-event (window-deiconify-event-mixin)
  ())

(defmethod handle-event ((window window-mixin) (event window-deiconify-event-mixin))
  (maybe-acquire-monitor window)
  (call-next-method)
  (values))

(defclass window-maximize-event-mixin (window-resize-event-mixin)
  ())

(defclass clui.v0:window-maximize-event (window-maximize-event-mixin)
  ())

(defclass window-restore-event-mixin (window-resize-event-mixin)
  ())

(defclass clui.v0:window-restore-event (window-restore-event-mixin)
  ())

(defclass window-fullscreen-event-mixin (window-resize-event-mixin)
  ())

(defclass clui.v0:window-fullscreen-event (window-fullscreen-event-mixin)
  ())

;; window-event

(defclass window-show-event-mixin (window-event-mixin)
  ())

(defclass clui.v0:window-show-event (window-show-event-mixin)
  ())

(defclass window-focus-event-mixin (window-show-event-mixin)
  ())

(defclass clui.v0:window-focus-event (window-focus-event-mixin)
  ())

(defclass window-defocus-event-mixin (window-event-mixin)
  ())

(defclass clui.v0:window-defocus-event (window-defocus-event-mixin)
  ())

(defclass window-hide-event-mixin (window-event-mixin)
  ())

(defclass clui.v0:window-hide-event (window-hide-event-mixin)
  ())

(defclass window-repaint-event-mixin (window-event-mixin)
  ())

(defclass clui.v0:window-repaint-event (window-repaint-event-mixin)
  ())

;; window-manager-event

(defclass window-manager-event-mixin (event-mixin)
  ((window :initarg :window)))


(defclass window-created-event-mixin (window-manager-event-mixin)
  ())

(defclass clui.v0:window-created-event (window-created-event-mixin)
  ())

(defclass window-close-event-mixin (window-manager-event-mixin)
  ())

(defclass clui.v0:window-close-event (window-close-event-mixin)
  ())
  

(defclass window-destroyed-event-mixin (window-manager-event-mixin)
  ())

(defclass clui.v0:window-destroyed-event (window-destroyed-event-mixin)
  ())

(defclass window-monitor-switched-event-mixin (window-manager-event-mixin)
  ())

(defclass clui.v0:window-monitor-switched-event (window-monitor-switched-event-mixin)
  ())

;; device-event

(defclass device-event-mixin (event-mixin)
  ())

(defclass input-event-mixin (device-event-mixin)
  ((window :initarg :window
	   :accessor event-window)
   
   (modifier-state :initarg :modifier-state
		   :accessor event-modifier-state)))

(defconstant +pointer-left-button+ (ash 1 0))
(defconstant +pointer-middle-button+ (ash 1 1))
(defconstant +pointer-right-button+ (ash 1 2))

(defclass pointer-event-mixin (input-event-mixin)
  ((pointer :initarg :pointer
	    :accessor pointer-event-pointer)
   (button :initarg :button
	   :accessor pointer-event-button)
   (local-x :initarg :x
	    :accessor pointer-event-x)
   (local-y :initarg :y
	    :accessor pointer-event-y)
   (native-x :initarg :native-x
	     :accessor pointer-event-native-x)
   (native-y :initarg :native-y
	     :accessor pointer-event-native-y)))

(defclass pointer-button-event-mixin (pointer-event-mixin)
  ())

(defclass pointer-button-press-event-mixin (pointer-button-event-mixin)
  ())

(defclass clui.v0:pointer-button-press-event (pointer-button-press-event-mixin)
  ())

(defclass pointer-button-release-event-mixin (pointer-button-event-mixin)
  ())

(defclass clui.v0:pointer-button-release-event (pointer-button-release-event-mixin)
  ())

(defclass pointer-button-hold-event-mixin (pointer-button-event-mixin)
  ())

(defclass clui.v0:pointer-button-hold-event (pointer-button-hold-event-mixin)
  ())

(defclass pointer-click-event-mixin (pointer-button-event-mixin)
  ())

(defclass clui.v0:pointer-click-event (pointer-click-event-mixin)
  ())

(defclass pointer-double-click-event-mixin (pointer-button-event-mixin)
  ())

(defclass clui.v0:pointer-double-click-event (pointer-double-click-event-mixin)
  ())

(defclass pointer-button-hold-and-drag-event-mixin (pointer-click-and-hold-mixin)
  ())

(defclass clui.v0:pointer-button-hold-and-drag-event (pointer-click-hold-and-drag-mixin)
  ())

(defclass pointer-wheel-event-mixin (pointer-event-mixin)
  ((offset :initarg :offset
	   :initarg :yoffset
	   :initform 0
	   :accessor pointer-wheel-event-offset
	   :accessor pointer-wheel-event-yoffset)
   (xoffset :initarg :xoffset
	    :initform 0
	    :accessor pointer-wheel-event-xoffset)))

(defclass clui.v0:pointer-wheel-event (pointer-wheel-event-mixin)
  ())

(defclass pointer-motion-event-mixin (pointer-event-mixin)
  ())

(defclass clui.v0:pointer-motion-event (pointer-motion-event-mixin)
  ())  

(defclass pointer-boundary-event-mixin (pointer-event-mixin)
  ((kind :initarg :kind
	 :initform nil
	 :accessor pointer-boundary-event-kind)))

(defclass pointer-enter-event-mixin (pointer-boundary-event-mixin)
  ())

(defclass clui.v0:pointer-enter-event (pointer-enter-event-mixin)
  ())

(defclass pointer-exit-event-mixin (pointer-boundary-event-mixin)
  ())

(defclass clui.v0:pointer-exit-event (pointer-exit-event-mixin)
  ())

(defconstant +shift-key+ (ash 1 0))
(defconstant +control-key+ (ash 1 1))
(defconstant +alt-key+ (ash 1 2))
(defconstant +meta-key+ (ash 1 3))
(defconstant +super-key+ (ash 1 4))
(defconstant +hyper-key+ (ash 1 5))

(defclass keyboard-event-mixin (input-event-mixin)
  ((key-name :initarg :key-name
	     :accessor keyboard-event-key-name)
   
   (character :initform nil
	      :initarg :character
	      :accessor keyboard-event-character)))

(defclass key-press-event-mixin (keyboard-event-mixin)
  ())

(defclass key-release-event-mixin (keyboard-event-mixin)
  ())

(defclass joystick-event-mixin (input-event-mixin)
  ())

(defclass spaceball-event-mixin (joystick-event-mixin)
  ())

(trace handle-event)
