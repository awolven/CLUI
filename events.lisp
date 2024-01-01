(in-package :clui)

(defgeneric clim:handle-event (client event))

(defclass event-mixin ()
  ((timestamp :initarg :timestamp
	      :accessor event-timestamp)))

(defmethod clim:handle-event :after (object (event clui::event-mixin))
  (declare (ignore object))
  (setf (display-last-event (default-display)) event))

(defmethod clim:handle-event (object event)
  (declare (ignorable object event))
  (values))

;; timer event

(defclass clim:timer-event () ())

(defclass timer-event-mixin (event-mixin) ())

(defclass clui.v0:timer-event (clim:timer-event timer-event-mixin)
  ())

;; window-event

(defclass clim:window-event () ())

(defclass window-event-mixin (clim:window-event event-mixin)
  ((window :initarg :window
	   :accessor event-window)
   
   (region :initarg :region
	   :initform nil
	   :accessor window-event-region)
   
   (native-region :initarg :region
		  :initform nil
		  :accessor window-event-native-region)))

;; window-configuration-event

(defclass clim:window-configuration-event () ())

(defclass window-configuration-event-mixin (clim:window-configuration-event window-event-mixin)
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

(defmethod clim:handle-event :before ((window window-mixin) (event window-iconify-event-mixin))
  (maybe-release-monitor window)
  (values))

(defclass window-deiconify-event-mixin (window-resize-event-mixin)
  ())

(defclass clui.v0:window-deiconify-event (window-deiconify-event-mixin)
  ())

(defmethod clim:handle-event :before ((window window-mixin) (event window-deiconify-event-mixin))
  (maybe-acquire-monitor window)
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

(defclass clim:window-repaint-event () ())

(defclass window-repaint-event-mixin (clim:window-repaint-event window-event-mixin)
  ())

(defclass clui.v0:window-repaint-event (window-repaint-event-mixin)
  ())

;; window-manager-event

(defclass clim:window-manager-event () ())

(defclass window-manager-event-mixin (clim:window-manager-event event-mixin)
  ((window :initarg :window)))


(defclass window-created-event-mixin (window-manager-event-mixin)
  ())

(defclass clui.v0:window-created-event (window-created-event-mixin)
  ())

(defclass window-close-event-mixin (window-manager-event-mixin)
  ())

(defmethod clim:handle-event :before ((window os-window-mixin) (event window-close-event-mixin))
  (setf (should-close? window) t)
  (when (or (null (display-window-list-head (window-display window))) ;; pathological case
	    (and (eql window (display-window-list-head (window-display window)))
		 (or (eql window (window-next (display-window-list-head (window-display window))))
		     (null (window-next (display-window-list-head (window-display window)))))))
    ;; this is the last window
    (setf (run-loop-exit? (window-display window)) t))
  (values))

(defmethod clim:handle-event :after ((window os-window-mixin) (event window-close-event-mixin))
  (when (should-close? window)
    (destroy-window window))
  (values))

(defclass clui.v0:window-close-event (window-close-event-mixin)
  ())

(defclass clim:window-manager-delete-event () ())

(defclass window-destroyed-event-mixin (clim:window-manager-delete-event window-manager-event-mixin)
  ())

(defclass clui.v0:window-destroyed-event (window-destroyed-event-mixin)
  ())

(defclass window-monitor-switched-event-mixin (window-manager-event-mixin)
  ())

(defclass clui.v0:window-monitor-switched-event (window-monitor-switched-event-mixin)
  ())

;; device-event

(defclass clim:device-event () ())

(defclass device-event-mixin (clim:device-event event-mixin)
  ())

(defclass input-event-mixin (device-event-mixin)
  ((window :initarg :window
	   :accessor event-window)

   (input-code :initarg :input-code
	       :initform nil
	       :accessor input-event-code)

   (character :initarg :character
	      :initform nil
	      :accessor input-event-character)
   
   (modifier-state :initarg :modifier-state
		   :initform 0
		   :accessor event-modifier-state)
   
   (lock-modifier-state :initarg :lock-modifier-state
			:initform 0
			:accessor event-lock-modifier-state)
   (local-x :initarg :x
	    :accessor pointer-event-x)
   
   (local-y :initarg :y
	    :accessor pointer-event-y)))

(defclass button-press-event-mixin ()
  ())

(defclass button-release-event-mixin ()
  ())

(defmethod print-object ((event input-event-mixin) stream)
  (print-unreadable-object (event stream :type t :identity t)
    (when (input-event-code event)
      (princ (aref *keysyms* (input-event-code event)) stream))
    event))

(defclass clim:pointer-event () ())

(defclass pointer-event-mixin (clim:pointer-event input-event-mixin)
  ((pointer :initarg :pointer
	    :accessor pointer-event-pointer)
   (native-x :initarg :native-x
	     :accessor pointer-event-native-x)
   (native-y :initarg :native-y
	     :accessor pointer-event-native-y)))

(defclass clim:pointer-button-event () ())

(defclass pointer-button-event-mixin (clim:pointer-button-event pointer-event-mixin)
  ())

(defclass clim:pointer-button-press-event () ())

(defclass pointer-button-press-event-mixin
    (clim:pointer-button-press-event button-press-event-mixin pointer-button-event-mixin)
  ())

(defclass clui.v0:pointer-button-press-event (pointer-button-press-event-mixin)
  ())

(defclass clim:pointer-button-release-event () ())

(defclass pointer-button-release-event-mixin
    (clim:pointer-button-release-event button-release-event-mixin pointer-button-event-mixin)
  ())

(defclass clui.v0:pointer-button-release-event (pointer-button-release-event-mixin)
  ())

(defclass clim:pointer-button-hold-event () ())

;; todo: make this at the gesture level
(defclass pointer-button-hold-event-mixin (clim:pointer-button-hold-event pointer-button-event-mixin)
  ())

(defclass clui.v0:pointer-button-hold-event (pointer-button-hold-event-mixin)
  ())

;; todo: make this be a gesture instead of an event
(defclass pointer-click-event-mixin (pointer-button-event-mixin)
  ())

(defmethod pointer-click-event-p ((event pointer-click-event-mixin))
  t)

(defclass clui.v0:pointer-click-event (pointer-click-event-mixin)
  ())

;; todo: make this be a gesture instead of an event
(defclass pointer-double-click-event-mixin (pointer-button-event-mixin)
  ())

(defclass clui.v0:pointer-double-click-event (pointer-double-click-event-mixin)
  ())

;; todo: make this be a gesture instead of an event
(defclass pointer-button-hold-and-drag-event-mixin (pointer-button-hold-event-mixin)
  ((delta-x :initarg :delta-x :accessor pointer-drag-delta-x)
   (delta-y :initarg :delta-x :accessor pointer-drag-delta-y)))

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

(defclass clim:pointer-motion-event () ())

(defclass pointer-motion-event-mixin (clim:pointer-motion-event pointer-event-mixin)
  ())

(defclass clui.v0:pointer-motion-event (pointer-motion-event-mixin)
  ())  

(defclass clim:pointer-boundary-event () ())

(defclass pointer-boundary-event-mixin (clim:pointer-boundary-event pointer-event-mixin)
  ((kind :initarg :kind
	 :initform nil
	 :accessor pointer-boundary-event-kind)))

(defclass clim:pointer-enter-event () ())

(defclass pointer-enter-event-mixin (clim:pointer-enter-event pointer-boundary-event-mixin)
  ())

(defclass clui.v0:pointer-enter-event (pointer-enter-event-mixin)
  ())

(defclass clim:pointer-exit-event () ())

(defclass pointer-exit-event-mixin (clim:pointer-exit-event pointer-boundary-event-mixin)
  ())

(defclass clui.v0:pointer-exit-event (pointer-exit-event-mixin)
  ())


(defclass clim:keyboard-event () ())

(defclass keyboard-event-mixin (clim:keyboard-event input-event-mixin)
  ())

(defclass clim:key-press-event () ())

(defclass key-press-event-mixin (clim:key-press-event button-press-event-mixin keyboard-event-mixin)
  ())



(defclass clui.v0:key-press-event (key-press-event-mixin)
  ())

(defclass key-repeat-event-mixin (key-press-event-mixin)
  ())

(defclass clui.v0::key-repeat-event (key-repeat-event-mixin)
  ())

(defclass clim:key-release-event () ())

(defclass key-release-event-mixin (clim:key-release-event button-release-event-mixin keyboard-event-mixin)
  ())

(defclass clui.v0:key-release-event (key-release-event-mixin)
  ())

(defclass character-event-mixin (keyboard-event-mixin)
  ())

(defmethod print-object ((event character-event-mixin) stream)
  (print-unreadable-object (event stream :type t :identity t)
    (format stream "~S" (input-event-character event))))

(defclass clui.v0:character-event (character-event-mixin)
  ())

(defclass joystick-event-mixin (input-event-mixin)
  ())

(defclass spaceball-event-mixin (joystick-event-mixin)
  ())


;; codes and names (keysyms) for inputs:

(defvar *input-code/keysym-alist* '())
(defvar *keysym-matcher->keysym* (make-hash-table :test #'equal))
(defvar *keysym-matcher->input-code* (make-hash-table :test #'equal))

(defmacro define-input-code (symbol input-code keysym)
  `(progn
     (defconstant ,symbol ,input-code)
     (pushnew (cons ,input-code ,keysym) *input-code/keysym-alist* :key #'car)
     (setf (gethash (keysym-matcher ,keysym) *keysym-matcher->keysym*)
	   ,keysym)
     (setf (gethash (keysym-matcher ,keysym) *keysym-matcher->input-code*)
	   ,input-code)))

(defun keysym-matcher (string)
  (string-upcase string))

(defun name-keysym (string)
  (gethash (keysym-matcher string) *keysym-matcher->keysym*))

(defun keysym-input-code (string)
  (gethash (keysym-matcher string) *keysym-matcher->input-code*))

;; these are the names we're giving the USB scan codes

(define-input-code +key-A+ 4 "a") ;; vk 65
(define-input-code +key-B+ 5 "b") ;; vk 66
(define-input-code +key-C+ 6 "c") ;; vk 67
(define-input-code +key-D+ 7 "d") ;; vk 68
(define-input-code +key-E+ 8 "e") ;; vk 69
(define-input-code +key-F+ 9 "f") ;; vk 70
(define-input-code +key-G+ #x0A "g") ;; vk 71
(define-input-code +key-H+ #x0B "h") ;; vk 72
(define-input-code +key-I+ #x0C "i") ;; vk 73
(define-input-code +key-J+ #x0D "j") ;; vk 74
(define-input-code +key-K+ #x0E "k") ;; vk 75
(define-input-code +key-L+ #x0F "l") ;; vk 76
(define-input-code +key-M+ #x10 "m") ;; vk 77
(define-input-code +key-N+ #x11 "n") ;; vk 78
(define-input-code +key-O+ #x12 "o") ;; vk 79
(define-input-code +key-P+ #x13 "p") ;; vk 80
(define-input-code +key-Q+ #x14 "q") ;; vk 81
(define-input-code +key-R+ #x15 "r") ;; vk 82
(define-input-code +key-S+ #x16 "s") ;; vk 83
(define-input-code +key-T+ #x17 "t") ;; vk 84
(define-input-code +key-U+ #x18 "u") ;; vk 85
(define-input-code +key-V+ #x19 "v") ;; vk 86
(define-input-code +key-W+ #x1A "w") ;; vk 87
(define-input-code +key-X+ #x1B "x") ;; vk 88
(define-input-code +key-Y+ #x1C "y") ;; vk 89
(define-input-code +key-Z+ #x1D "z") ;; vk 90

(define-input-code +key-1+ #x1E "1") ;; vk 49
(define-input-code +key-2+ #x1F "2") ;; vk 50
(define-input-code +key-3+ #x20 "3") ;; vk 51
(define-input-code +key-4+ #x21 "4") ;; vk 52
(define-input-code +key-5+ #x22 "5") ;; vk 53
(define-input-code +key-6+ #x23 "6") ;; vk 54
(define-input-code +key-7+ #x24 "7") ;; vk 55
(define-input-code +key-8+ #x25 "8") ;; vk 56
(define-input-code +key-9+ #x26 "9") ;; vk 57
(define-input-code +key-0+ #x27 "0") ;; vk 48

(define-input-code +key-enter+ #x28 "Enter") ;; VK_RETURN
(define-input-code +key-escape+ #x29 "Escape") ;; VK_ESCAPE
(define-input-code +key-backspace+ #x2A "Backspace") ;; VK_BACK
(define-input-code +key-tab+ #x2B "Tab") ;; VK_TAB
(define-input-code +key-spacebar+ #x2C "Spacebar") ;; VK_SPACE
(define-input-code +key-minus+ #x2D "-")
(define-input-code +key-equals+ #x2E "=")
(define-input-code +key-left-bracket+ #x2F "[")
(define-input-code +key-right-bracket+ #x30 "]")
(define-input-code +key-backslash+ #x31 "\\")
(define-input-code +key-international-hash-and-tilde+ #x32 "InternationalHash")
(define-input-code +key-semicolon+ #x33 ";")
(define-input-code +key-apostrophe+ #x34 "'")
(define-input-code +key-grave-accent+ #x35 "`")
(define-input-code +key-comma+ #x36 ",")
(define-input-code +key-period+ #x37 ".") ;; VK_OEM_PERIOD
(define-input-code +key-slash+ #x38 "/")
(define-input-code +key-caps-lock+ #x39 "CapsLock") ;; VK_CAPITAL
(define-input-code +key-F1+ #x3A "F1") ;; VK_F1
(define-input-code +key-F2+ #x3B "F2") ;; VK_F2
(define-input-code +key-F3+ #x3C "F3") ;; VK_F3
(define-input-code +key-F4+ #x3D "F4") ;; VK_F4
(define-input-code +key-F5+ #x3E "F5") ;; VK_F5
(define-input-code +key-F6+ #x3F "F6") ;; VK_F6
(define-input-code +key-F7+ #x40 "F7") ;; VK_F7
(define-input-code +key-F8+ #x41 "F8") ;; VK_F8
(define-input-code +key-F9+ #x42 "F9") ;; VK_F9
(define-input-code +key-F10+ #x43 "F10") ;; VK_F10
(define-input-code +key-F11+ #x44 "F11") ;; VK_F11
(define-input-code +key-F12+ #x45 "F12") ;; VK_F12
(define-input-code +key-print-screen+ #x46 "PrintScr") ;; VK_SNAPSHOT
(define-input-code +key-scroll-lock+ #x47 "ScrollLock")
(define-input-code +key-pause+ #x48 "Pause") ;; VK_PAUSE
(define-input-code +key-insert+ #x49 "Insert") ;; VK_INSERT
(define-input-code +key-home+ #x4A "Home") ;; VK_HOME
(define-input-code +key-page-up+ #x4B "PageUp") ;; VK_PRIOR
(define-input-code +key-delete+ #x4C "Delete") ;; VK_DELETE
(define-input-code +key-end+ #x4D "End") ;; VK_END
(define-input-code +key-page-down+ #x4E "PageDown") ;; VK_NEXT
(define-input-code +key-right-arrow+ #x4F "RightArrow") ;; VK_RIGHT
(define-input-code +key-left-arrow+ #x50 "LeftArrow") ;; VK_LEFT
(define-input-code +key-down-arrow+ #x51 "DownArrow") ;; VK_DOWN
(define-input-code +key-up-arrow+ #x52 "UpArrow") ;; VK_UP
(define-input-code +key-num-lock+ #x53 "NumLock")
(define-input-code +key-pad-slash+ #x54 "KPDivide") ;; VK_DIVIDE
(define-input-code +key-pad-asterisk+ #x55 "KPMultiply") ;; VK_MULTIPLY
(define-input-code +key-pad-minus+ #x56 "KPSubtract") ;; VK_SUBTRACT
(define-input-code +key-pad-plus+ #x57 "KPAdd") ;; VK_ADD
(define-input-code +key-pad-enter+ #x58 "KPEnter")
(define-input-code +key-pad-1+ #x59 "KP1") ;; VK_NUMPAD1
(define-input-code +key-pad-2+ #x5A "KP2") ;; VK_NUMPAD2
(define-input-code +key-pad-3+ #x5B "KP3") ;; VK_NUMPAD3
(define-input-code +key-pad-4+ #x5C "KP4") ;; VK_NUMPAD4
(define-input-code +key-pad-5+ #x5D "KP5") ;; VK_NUMPAD5
(define-input-code +key-pad-6+ #x5E "KP6") ;; VK_NUMPAD6
(define-input-code +key-pad-7+ #x5F "KP7") ;; VK_NUMPAD7
(define-input-code +key-pad-8+ #x60 "KP8") ;; VK_NUMPAD8
(define-input-code +key-pad-9+ #x61 "KP9") ;; VK_NUMPAD9
(define-input-code +key-pad-0+ #x62 "KP0") ;; VK_NUMPAD0
(define-input-code +key-pad-decimal-point+ #x63 "KPDecimal") ;; VK_DECIMAL
(define-input-code +key-us-vertical-bar+ #x64 "|")

(define-input-code +key-application+ #x65 "Application") ;; VK_APPS
(define-input-code +key-power+ #x66 "Power")
(define-input-code +key-pad-equals+ #x67 "KPEquals")

(define-input-code +key-F13+ #x68 "F13") ;; VK_F13
(define-input-code +key-F14+ #x69 "F14") ;; VK_F14
(define-input-code +key-F15+ #x6A "F15") ;; VK_F15
(define-input-code +key-F16+ #x6B "F16") ;; VK_F16
(define-input-code +key-F17+ #x6C "F17") ;; VK_F17
(define-input-code +key-F18+ #x6D "F18") ;; VK_F18
(define-input-code +key-F19+ #x6E "F19") ;; VK_F19
(define-input-code +key-F20+ #x6F "F20") ;; VK_F20
(define-input-code +key-F21+ #x70 "F21") ;; VK_F21
(define-input-code +key-F22+ #x71 "F22") ;; VK_F22
(define-input-code +key-F23+ #x72 "F23") ;; VK_F23
(define-input-code +key-F24+ #x73 "F24") ;; VK_F24
(define-input-code +key-execute+ 116 "Execute") ;; VK_EXECUTE
(define-input-code +key-help+ 117 "Help") ;; VK_HELP
(define-input-code +key-menu+ 118 "Menu")
(define-input-code +key-select+ 119 "Select") ;; VK_SELECT
(define-input-code +key-stop+ 120 "Stop")
(define-input-code +key-again+ 121 "Again")
(define-input-code +key-undo+ 122 "Undo")
(define-input-code +key-cut+ 123 "Cut")
(define-input-code +key-copy+ 124 "Copy")
(define-input-code +key-paste+ 125 "Paste")
(define-input-code +key-find+ 126 "Find")
(define-input-code +key-mute+ 127 "Mute")
(define-input-code +key-volume-up+ 128 "VolumeUp")
(define-input-code +key-volume-down+ 129 "VolumeDown")
(define-input-code +key-locking-caps-lock+ 130 "KLCapsLock")
(define-input-code +key-locking-num-lock+ 131 "KLNumLock") ;; VK_NUMLOCK
(define-input-code +key-locking-scroll-lock+ 132 "KLScrollLock") ;; VK_SCROLL
(define-input-code +key-pad-comma+ 133 "KPComma") 
(define-input-code +key-pad-equals2+ 113 "KPEquals2")
(define-input-code +key-international-1+ 135 "International1")
(define-input-code +key-international-2+ 136 "International2") ;; VK_KANA
(define-input-code +key-international-3+ 137 "International3")
(define-input-code +key-international-4+ 138 "International4")
(define-input-code +key-international-5+ 139 "International5")
(define-input-code +key-international-6+ 140 "International6")
(define-input-code +key-international-7+ 141 "International7")
(define-input-code +key-international-8+ 142 "International8")
(define-input-code +key-international-9+ 143 "International9")
(define-input-code +key-language-1+ 144 "Hangul") ;; VK_HANGUL
(define-input-code +key-language-2+ 145 "Hanja") ;; VK_HANJA
(define-input-code +key-language-3+ 146 "Language3") 
(define-input-code +key-language-4+ 147 "Language4")
(define-input-code +key-language-5+ 148 "Language5")
(define-input-code +key-language-6+ 149 "Language6")
(define-input-code +key-language-7+ 150 "Language7")
(define-input-code +key-language-8+ 151 "Language8")
(define-input-code +key-language-9+ 152 "Language9")
(define-input-code +key-alt-erase+ 153 "AltErase")
(define-input-code +key-sys-req+ 154 "SysReq") ;; VK_ATTN
(define-input-code +key-cancel+ 155 "Cancel")
(define-input-code +key-clear+ 156 "Clear")
(define-input-code +key-prior+ 157 "Prior")
(define-input-code +key-return+ 158 "Return")
(define-input-code +key-separator+ 159 "Separator")
(define-input-code +key-out+ 160 "Out")
(define-input-code +key-oper+ 161 "Oper")
(define-input-code +key-clear-again+ 162 "ClearAgain")
(define-input-code +key-cr-sel-props+ 163 "CRSel") ;; VK_CRSEL
(define-input-code +key-ex-sel+ 164 "ExSel") ;; VK_EXSEL
(define-input-code +key-165+ 165 "Key165")
(define-input-code +key-166+ 166 "Key166")
(define-input-code +key-167+ 167 "Key167")

(define-input-code +key-00+ #xB0 "00")
(define-input-code +key-000+ #xB1 "000")
(define-input-code +key-thousands-separator+ #xB2 "ThousandsSeparator")
(define-input-code +key-decimal-separator+ #xB3 "DecimalSeparator")
(define-input-code +key-currency-unit+ #xB4 "CurrencyUnit")
(define-input-code +key-currency-subunit+ #xB5 "CurrencySubunit")

(define-input-code +key-pad-left-paren+ #xB6 "(")
(define-input-code +key-pad-right-paren+ #xB7 ")")
(define-input-code +key-pad-left-brace+ #xB8 "{")
(define-input-code +key-pad-right-brace+ #xB9 "}")
(define-input-code +key-pad-tab+ #xBA "KPTab")
(define-input-code +key-pad-backspace+ #xBB "KPBackspace")
(define-input-code +key-pad-A+ #xBC "KPA")
(define-input-code +key-pad-B+ #xBD "KPB")
(define-input-code +key-pad-C+ #xBE "KPC")
(define-input-code +key-pad-D+ #xBF "KPD")
(define-input-code +key-pad-E+ #xC0 "KPE")
(define-input-code +key-pad-F+ #xC1 "KPF")
(define-input-code +key-pad-XOR+ #xC2 "KPXor")
(define-input-code +key-pad-carrot+ #xC3 "KPCarrot")
(define-input-code +key-pad-percent+ #xC4 "KPPercent")
(define-input-code +key-pad-less-than+ #xC5 "KPLessThan")
(define-input-code +key-pad-greater-than+ #xC6 "KPGreaterThan")
(define-input-code +key-pad-ampersand+ #xC7 "KPAmpersand")
(define-input-code +key-pad-double-ampersand+ #xC8 "KPDoubleAmpersand")
(define-input-code +key-pad-vertical-bar+ #xC9 "KPVerticalBar")
(define-input-code +key-pad-double-vertical-bar+ #xCA "KPDoubleVerticalBar")
(define-input-code +key-pad-colon+ #xCB "KPColon")
(define-input-code +key-pad-hash+ #xCC "KPHash")
(define-input-code +key-pad-space+ #xCD "KPSpace")
(define-input-code +key-pad-at-sign+ #xCE "KPAtSign")
(define-input-code +key-pad-exclamation-point+ #xCF "KPExclamation")
(define-input-code +key-pad-memory-store+ #xD0 "KPMemoryStore")
(define-input-code +key-pad-memory-recall+ #xD1 "KPMemoryRecall")
(define-input-code +key-pad-memory-clear+ #xD2 "KPMemoryClear")
(define-input-code +key-pad-memory-add+ #xD3 "KPMemoryAdd")
(define-input-code +key-pad-memory-subtract+ #xD4 "KPMemorySubtract")
(define-input-code +key-pad-memory-multiply+ #xD5 "KPMemoryMultiply")
(define-input-code +key-pad-memory-divide+ #xD6 "KPMemoryDivide")
(define-input-code +key-pad-plus-or-minus+ #XD7 "KPPlusOrMinus")
(define-input-code +key-pad-clear+ #xD8 "KPClear")
(define-input-code +key-pad-clear-entry+ #xD9 "KPClearEntry")
(define-input-code +key-pad-binary+ #xDA "KPBinary")
(define-input-code +key-pad-octal+ #xDB "KPOctal")
(define-input-code +key-pad-decimal+ #xDC "KPDecimal")
(define-input-code +key-pad-hexidecimal+ #xDD "KPHexidecimal")

(define-input-code +key-left-ctrl+ #xE0 "LeftCtrl") ;; VK_CONTROL, VK_LCONTROL
(define-input-code +key-left-shift+ #xE1 "LeftShift") ;; VK_SHIFT, VK_LSHIFT
(define-input-code +key-left-alt+ #xE2 "LeftAlt") ;; VK_MENU, VK_LMENU
(define-input-code +key-left-GUI+ #xE3 "LeftGUI") ;; windows or command key left ;; VK_LWIN
(define-input-code +key-right-ctrl+ #xE4 "RightCtrl") ;; VK_CONTROL, VK_RCONTROL
(define-input-code +key-right-shift+ #xE5 "RightShift") ;; VK_SHIFT, VK_RSHIFT
(define-input-code +key-right-alt+ #xE6 "RightAlt") ;; VK_MENU, VK_RMENU
(define-input-code +key-right-GUI+ #xE7 "RightGUI") ;; windows or command key right ;; VK_RWIN

(define-input-code +pointer-move+ 249 "MouseMove")
(define-input-code +pointer-left-button+ 250 "LeftMouse")
(define-input-code +pointer-right-button+ 251 "RightMouse")
(define-input-code +pointer-middle-button+ 242 "MiddleMouse")
(define-input-code +pointer-button-4+ 253 "Mouse4")
(define-input-code +pointer-button-5+ 254 "Mouse5")
(define-input-code +pointer-wheel+ 255 "MouseWheel")

#+NIL(
(define-input-code +ms-ext-key-junja+ #x10000 "Junja")
(define-input-code +ms-ext-key-final+ #x10001 "Final")
(define-input-code +ms-ext-key-hanja+ #x10002 "Hanja")
(define-input-code +ms-ext-key-kanji+ #x10003 "Kanji")
      )

(defparameter *keysyms*
  (let ((keysyms (make-array 256 :initial-element nil)))
    (loop for (input-code . keysym) in *input-code/keysym-alist*
	  when (< input-code 256)
	    do (setf (aref keysyms input-code) keysym)
	  finally (return keysyms))))

;; modifier codes and names

(defvar *modifiers-to-internal-masks* ())

(defvar *modifier-count* 0
  "The number of modifiers that are currently defined.")

(defvar *all-modifier-names* ()
  "A list of all the names of defined modifiers.")

(defmacro define-input-event-modifier (symbol long-name short-name)
  "This establishes long-name and short-name as modifier names for purposes
   of specifying input-event-ids in #k syntax.  The names are case-insensitive and
   must be strings.  If either name is already defined, this signals an error."
  `(let ((new-bits (ash 1 *modifier-count*)))
     (push (cons ,long-name new-bits) *modifiers-to-internal-masks*)
       (push (cons ,short-name new-bits) *modifiers-to-internal-masks*)
       (pushnew ,long-name *all-modifier-names* :test #'string-equal)
       ;; Sometimes the long-name is the same as the short-name.
       (pushnew ,short-name *all-modifier-names* :test #'string-equal)
       (eval `(defconstant ,',symbol ,new-bits))
       (incf *modifier-count*)))

(define-input-event-modifier +hyper-modifier+ "Hyper" "H")
(define-input-event-modifier +num-lock-modifier+ "NumLock" "NumL")
(define-input-event-modifier +caps-lock-modifier+ "CapsLock" "CapsL")
(define-input-event-modifier +super-modifier+ "Super" "S")
(define-input-event-modifier +meta-modifier+ "Meta" "M")
(define-input-event-modifier +alt-modifier+ "Alt" "Alt")
(define-input-event-modifier +ctrl-modifier+ "Ctrl" "C")
(define-input-event-modifier +shift-modifier+ "Shift" "Shift")









