(in-package :abstract-os)

(defgeneric handle-event (client event))

(defclass event ()
  ((timestamp :initarg :timestamp
	      :accessor event-timestamp)))

(defmethod handle-event (window (event event))
  (values))

;; timer event

(defclass timeout-event (event)
  ())

;; window-event

(defclass window-event (event)
  ((region :initarg :region
	   :initform nil
	   :accessor pointer-event-region)
   
   (native-region :initarg :region
		  :initform nil
		  :accessor pointer-event-native-region)))

;; window-configuration-event

(defclass window-configuration-event (window-event)
  ())

;; window-configuration-event
;; window-position-event-mixin

(defclass window-position-event-mixin ()
  ((new-x :initarg :new-x
	  :initform nil
	  :accessor window-position-event-new-x)
   
   (new-y :initarg :new-y
	  :initform nil
	  :accessor window-position-event-new-y)))


(defclass window-move-event (window-position-event-mixin
			     window-configuration-event)
  ())

;; window-resize-event-mixin

(defclass window-resize-event-mixin (window-position-event-mixin)
  ((new-width :initarg :new-width
	      :accessor window-resize-event-new-width)
   
   (new-height :initarg :new-height
	       :accessor window-resize-event-new-height)))

(defclass window-iconify-event (window-resize-event-mixin
				window-configuration-event)
  ())

(defmethod handle-event ((window window-mixin) (event window-iconify-event))
  (maybe-release-monitor window)
  (call-next-method)
  (values))

(defclass window-deiconify-event (window-resize-event-mixin
				  window-configuration-event)
  ())

(defmethod handle-event ((window window-mixin) (event window-deiconify-event))
  (maybe-acquire-monitor window)
  (call-next-method)
  (values))

(defclass window-maximize-event (window-resize-event-mixin
				 window-configuration-event)
  ())

(defclass window-restore-event (window-resize-event-mixin
				window-configuration-event)
  ())

(defclass window-resize-event (window-resize-event-mixin
			       window-configuration-event)
  ())

(defclass window-fullscreen-event (window-resize-event-mixin
				   window-configuration-event)
  ())

;; window-event

(defclass window-show-event (window-event)
  ())

(defclass window-focus-event (window-show-event)
  ())

(defclass window-defocus-event (window-event)
  ())

(defclass window-hide-event (window-event)
  ())

(defclass window-repaint-event (window-event)
  ())

;; window-manager-event

(defclass window-manager-event (event)
  ((window :initarg :window)))

(defclass window-created-event (window-manager-event)
  ())

(defclass window-close-event (window-manager-event)
  ())
  

(defclass window-destroyed-event (window-manager-event)
  ())

(defclass window-monitor-switched-event (window-manager-event)
  ())

;; device-event

(defclass device-event (event)
  ())

(defclass input-event (device-event)
  ((window :initarg :window
	   :accessor event-window)
   
   (modifier-state :initarg :modifier-state
		   :accessor event-modifier-state)))

(defconstant +pointer-left-button+ (ash 1 0))
(defconstant +pointer-middle-button+ (ash 1 1))
(defconstant +pointer-right-button+ (ash 1 2))

(defclass pointer-event (input-event)
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

(defclass pointer-button-event (pointer-event)
  ())

(defclass pointer-button-press-event (pointer-button-event)
  ())

(defclass pointer-button-release-event (pointer-button-event)
  ())

(defclass pointer-button-hold-event (pointer-button-event)
  ())

(defclass pointer-click-event (pointer-button-event)
  ())

(defclass pointer-double-click-event (pointer-button-event)
  ())

(defclass pointer-click-and-hold-event (pointer-button-event)
  ())

(defclass pointer-wheel-event (pointer-event)
  ((offset :initarg :offset
	   :initform 0
	   :accessor pointer-wheel-event-offset)))

(defclass pointer-motion-event (pointer-event)
  ())

(defclass pointer-boundary-event (pointer-event)
  ((kind :initarg :kind
	 :initform nil
	 :accessor pointer-boundary-event-kind)))

(defclass pointer-enter-event (pointer-boundary-event)
  ())

(defclass pointer-exit-event (pointer-boundary-event)
  ())

(defconstant +shift-key+ (ash 1 0))
(defconstant +control-key+ (ash 1 1))
(defconstant +alt-key+ (ash 1 2))
(defconstant +meta-key+ (ash 1 3))
(defconstant +super-key+ (ash 1 4))
(defconstant +hyper-key+ (ash 1 5))

(defclass keyboard-event (input-event)
  ((key-name :initarg :key-name
	     :accessor keyboard-event-key-name)
   
   (character :initform nil
	      :initarg :character
	      :accessor keyboard-event-character)))

(defclass key-press-event (keyboard-event)
  ())

(defclass key-release-event (keyboard-event)
  ())

(defclass joystick-event (input-event)
  ())

(defclass spaceball-event (joystick-event)
  ())

(trace handle-event)
