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
  ((window :initarg :window
	   :accessor event-window)
   
   (region :initarg :region
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

(defmethod handle-event :before ((window window-mixin) (event window-iconify-event-mixin))
  (maybe-release-monitor window)
  (values))

(defclass window-deiconify-event-mixin (window-resize-event-mixin)
  ())

(defclass clui.v0:window-deiconify-event (window-deiconify-event-mixin)
  ())

(defmethod handle-event :before ((window window-mixin) (event window-deiconify-event-mixin))
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

(defmethod handle-event :before ((window os-window-mixin) (event window-close-event-mixin))
  (setf (should-close? window) t)
  (values))

(defmethod handle-event :after ((window os-window-mixin) (event window-close-event-mixin))
  (when (should-close? window)
    (destroy-window window))
  (values))

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

(defconstant +shift-modifier+ (ash 1 0))
(defconstant +ctrl-modifier+ (ash 1 1))
(defconstant +alt-modifier+ (ash 1 2))
(defconstant +meta-modifier+ (ash 1 3))
(defconstant +super-modifier+ (ash 1 4))
(defconstant +caps-lock-modifier+ (ash 1 5))
(defconstant +num-lock-modifier+ (ash 1 6))
(defconstant +hyper-modifier+ (ash 1 7))

(defclass keyboard-event-mixin (input-event-mixin)
  ((key-name :initarg :key-name
	     :accessor keyboard-event-key-name)
   
   (character :initform nil
	      :initarg :character
	      :accessor keyboard-event-character)))

(defclass key-press-event-mixin (keyboard-event-mixin)
  ())

(defclass clui.v0:key-press-event (key-press-event-mixin)
  ())

(defclass key-release-event-mixin (keyboard-event-mixin)
  ())

(defclass clui.v0:key-release-event (key-release-event-mixin)
  ())

(defclass character-event-mixin (keyboard-event-mixin)
  ())

(defclass clui.v0:character-event (character-event-mixin)
  ())

(defclass joystick-event-mixin (input-event-mixin)
  ())

(defclass spaceball-event-mixin (joystick-event-mixin)
  ())

(trace handle-event)


;; these are the names we're giving the USB scan codes

(defconstant +key-A+ 4) ;; vk 65
(defconstant +key-B+ 5) ;; vk 66
(defconstant +key-C+ 6) ;; vk 67
(defconstant +key-D+ 7) ;; vk 68
(defconstant +key-E+ 8) ;; vk 69
(defconstant +key-F+ 9) ;; vk 70
(defconstant +key-G+ #x0A) ;; vk 71
(defconstant +key-H+ #x0B) ;; vk 72
(defconstant +key-I+ #x0C) ;; vk 73
(defconstant +key-J+ #x0D) ;; vk 74
(defconstant +key-K+ #x0E) ;; vk 75
(defconstant +key-L+ #x0F) ;; vk 76
(defconstant +key-M+ #x10) ;; vk 77
(defconstant +key-N+ #x11) ;; vk 78
(defconstant +key-O+ #x12) ;; vk 79
(defconstant +key-P+ #x13) ;; vk 80
(defconstant +key-Q+ #x14) ;; vk 81
(defconstant +key-R+ #x15) ;; vk 82
(defconstant +key-S+ #x16) ;; vk 83
(defconstant +key-T+ #x17) ;; vk 84
(defconstant +key-U+ #x18) ;; vk 85
(defconstant +key-V+ #x19) ;; vk 86
(defconstant +key-W+ #x1A) ;; vk 87
(defconstant +key-X+ #x1B) ;; vk 88
(defconstant +key-Y+ #x1C) ;; vk 89
(defconstant +key-Z+ #x1D) ;; vk 90

(defconstant +key-1+ #x1E) ;; vk 49
(defconstant +key-2+ #x1F) ;; vk 50
(defconstant +key-3+ #x20) ;; vk 51
(defconstant +key-4+ #x21) ;; vk 52
(defconstant +key-5+ #x22) ;; vk 53
(defconstant +key-6+ #x23) ;; vk 54
(defconstant +key-7+ #x24) ;; vk 55
(defconstant +key-8+ #x25) ;; vk 56
(defconstant +key-9+ #x26) ;; vk 57
(defconstant +key-0+ #x27) ;; vk 48

(defconstant +key-enter+ #x28) ;; VK_RETURN
(defconstant +key-escape+ #x29) ;; VK_ESCAPE
(defconstant +key-backspace+ #x2A) ;; VK_BACK
(defconstant +key-tab+ #x2B) ;; VK_TAB
(defconstant +key-spacebar+ #x2C) ;; VK_SPACE
(defconstant +key-minus+ #x2D)
(defconstant +key-equals+ #x2E)
(defconstant +key-left-bracket+ #x2F)
(defconstant +key-right-bracket+ #x30)
(defconstant +key-backslash+ #x31)
(defconstant +key-international-hash-and-tilde+ #x32)
(defconstant +key-semicolon+ #x33)
(defconstant +key-apostrophe+ #x34)
(defconstant +key-grave-accent+ #x35)
(defconstant +key-comma+ #x36)
(defconstant +key-period+ #x37) ;; VK_OEM_PERIOD
(defconstant +key-slash+ #x38)
(defconstant +key-caps-lock+ #x39) ;; VK_CAPITAL
(defconstant +key-F1+ #x3A) ;; VK_F1
(defconstant +key-F2+ #x3B) ;; VK_F2
(defconstant +key-F3+ #x3C) ;; VK_F3
(defconstant +key-F4+ #x3D) ;; VK_F4
(defconstant +key-F5+ #x3E) ;; VK_F5
(defconstant +key-F6+ #x3F) ;; VK_F6
(defconstant +key-F7+ #x40) ;; VK_F7
(defconstant +key-F8+ #x41) ;; VK_F8
(defconstant +key-F9+ #x42) ;; VK_F9
(defconstant +key-F10+ #x43) ;; VK_F10
(defconstant +key-F11+ #x44) ;; VK_F11
(defconstant +key-F12+ #x45) ;; VK_F12
(defconstant +key-print-screen+ #x46) ;; VK_SNAPSHOT
(defconstant +key-scroll-lock+ #x47)
(defconstant +key-pause+ #x48) ;; VK_PAUSE
(defconstant +key-insert+ #x49) ;; VK_INSERT
(defconstant +key-home+ #x4A) ;; VK_HOME
(defconstant +key-page-up+ #x4B) ;; VK_PRIOR
(defconstant +key-delete+ #x4C) ;; VK_DELETE
(defconstant +key-end+ #x4D) ;; VK_END
(defconstant +key-page-down+ #x4E) ;; VK_NEXT
(defconstant +key-right-arrow+ #x4F) ;; VK_RIGHT
(defconstant +key-left-arrow+ #x50) ;; VK_LEFT
(defconstant +key-down-arrow+ #x51) ;; VK_DOWN
(defconstant +key-up-arrow+ #x52) ;; VK_UP
(defconstant +key-num-lock+ #x53)
(defconstant +key-pad-slash+ #x54) ;; VK_DIVIDE
(defconstant +key-pad-asterisk+ #x55) ;; VK_MULTIPLY
(defconstant +key-pad-minus+ #x56) ;; VK_SUBTRACT
(defconstant +key-pad-plus+ #x57) ;; VK_ADD
(defconstant +key-pad-enter+ #x58)
(defconstant +key-pad-1+ #x59) ;; VK_NUMPAD1
(defconstant +key-pad-2+ #x5A) ;; VK_NUMPAD2
(defconstant +key-pad-3+ #x5B) ;; VK_NUMPAD3
(defconstant +key-pad-4+ #x5C) ;; VK_NUMPAD4
(defconstant +key-pad-5+ #x5D) ;; VK_NUMPAD5
(defconstant +key-pad-6+ #x5E) ;; VK_NUMPAD6
(defconstant +key-pad-7+ #x5F) ;; VK_NUMPAD7
(defconstant +key-pad-8+ #x60) ;; VK_NUMPAD8
(defconstant +key-pad-9+ #x61) ;; VK_NUMPAD9
(defconstant +key-pad-0+ #x62) ;; VK_NUMPAD0
(defconstant +key-pad-decimal-point+ #x63) ;; VK_DECIMAL
(defconstant +key-us-vertical-bar+ #x64)

(defconstant +key-application+ #x65) ;; VK_APPS
(defconstant +key-power+ #x66)
(defconstant +key-pad-equals+ #x67)

(defconstant +key-F13+ #x68) ;; VK_F13
(defconstant +key-F14+ #x69) ;; VK_F14
(defconstant +key-F15+ #x6A) ;; VK_F15
(defconstant +key-F16+ #x6B) ;; VK_F16
(defconstant +key-F17+ #x6C) ;; VK_F17
(defconstant +key-F18+ #x6D) ;; VK_F18
(defconstant +key-F19+ #x6E) ;; VK_F19
(defconstant +key-F20+ #x6F) ;; VK_F20
(defconstant +key-F21+ #x70) ;; VK_F21
(defconstant +key-F22+ #x71) ;; VK_F22
(defconstant +key-F23+ #x72) ;; VK_F23
(defconstant +key-F24+ #x73) ;; VK_F24
(defconstant +key-execute+ 116) ;; VK_EXECUTE
(defconstant +key-help+ 117) ;; VK_HELP
(defconstant +key-menu+ 118)
(defconstant +key-select+ 119) ;; VK_SELECT
(defconstant +key-stop+ 120)
(defconstant +key-again+ 121)
(defconstant +key-undo+ 122)
(defconstant +key-cut+ 123)
(defconstant +key-copy+ 124)
(defconstant +key-paste+ 125)
(defconstant +key-find+ 126)
(defconstant +key-mute+ 127)
(defconstant +key-volume-up+ 128)
(defconstant +key-volume-down+ 129)
(defconstant +key-locking-caps-lock+ 130)
(defconstant +key-locking-num-lock+ 131) ;; VK_NUMLOCK
(defconstant +key-locking-scroll-lock+ 132) ;; VK_SCROLL
(defconstant +key-pad-comma+ 133) 
(defconstant +key-pad-equals2+ 113)
(defconstant +key-international-1+ 135)
(defconstant +key-international-2+ 136) ;; VK_KANA
(defconstant +key-international-3+ 137)
(defconstant +key-international-4+ 138)
(defconstant +key-international-5+ 139)
(defconstant +key-international-6+ 140)
(defconstant +key-international-7+ 141)
(defconstant +key-international-8+ 142)
(defconstant +key-international-9+ 143)
(defconstant +key-language-1+ 144) ;; VK_HANGUL
(defconstant +key-language-2+ 145) ;; VK_HANJA
(defconstant +key-language-3+ 146) 
(defconstant +key-language-4+ 147)
(defconstant +key-language-5+ 148)
(defconstant +key-language-6+ 149)
(defconstant +key-language-7+ 150)
(defconstant +key-language-8+ 151)
(defconstant +key-language-9+ 152)
(defconstant +key-alt-erase+ 153)
(defconstant +key-sys-req+ 154) ;; VK_ATTN
(defconstant +key-cancel+ 155)
(defconstant +key-clear+ 156)
(defconstant +key-prior+ 157)
(defconstant +key-return+ 158)
(defconstant +key-separator+ 159)
(defconstant +key-out+ 160)
(defconstant +key-oper+ 161)
(defconstant +key-clear-again+ 162)
(defconstant +key-cr-sel-props+ 163) ;; VK_CRSEL
(defconstant +key-ex-sel+ 164) ;; VK_EXSEL
(defconstant +key-165+ 165)
(defconstant +key-166+ 166)
(defconstant +key-167+ 167)

(defconstant +key-00+ #xB0)
(defconstant +key-000+ #xB1)
(defconstant +key-thousands-separator+ #xB2)
(defconstant +key-decimal-separator+ #xB3)
(defconstant +key-currency-unit+ #xB4)
(defconstant +key-currency-subunit+ #xB5)

(defconstant +key-pad-left-paren+ #xB6)
(defconstant +key-pad-right-paren+ #xB7)
(defconstant +key-pad-left-brace+ #xB8)
(defconstant +key-pad-right-brace+ #xB9)
(defconstant +key-pad-tab+ #xBA)
(defconstant +key-pad-backspace+ #xBB)
(defconstant +key-pad-A+ #xBC)
(defconstant +key-pad-B+ #xBD)
(defconstant +key-pad-C+ #xBE)
(defconstant +key-pad-D+ #xBF)
(defconstant +key-pad-E+ #xC0)
(defconstant +key-pad-F+ #xC1)
(defconstant +key-pad-XOR+ #xC2)
(defconstant +key-pad-carrot+ #xC3)
(defconstant +key-pad-percent+ #xC4)
(defconstant +key-pad-less-than+ #xC5)
(defconstant +key-pad-greater-than+ #xC6)
(defconstant +key-pad-ampersand+ #xC7)
(defconstant +key-pad-double-ampersand+ #xC8)
(defconstant +key-pad-vertical-bar+ #xC9)
(defconstant +key-pad-double-vertical-bar+ #xCA)
(defconstant +key-pad-colon+ #xCB)
(defconstant +key-pad-hash+ #xCC)
(defconstant +key-pad-space+ #xCD)
(defconstant +key-pad-at-sign+ #xCE)
(defconstant +key-pad-exclamation-point+ #xCF)
(defconstant +key-pad-memory-store+ #xD0)
(defconstant +key-pad-memory-recall+ #xD1)
(defconstant +key-pad-memory-clear+ #xD2)
(defconstant +key-pad-memory-add+ #xD3)
(defconstant +key-pad-memory-subtract+ #xD4)
(defconstant +key-pad-memory-multiply+ #xD5)
(defconstant +key-pad-memory-divide+ #xD6)
(defconstant +key-pad-plus-or-minus+ #XD7)
(defconstant +key-pad-clear+ #xD8)
(defconstant +key-pad-clear-entry+ #xD9)
(defconstant +key-pad-binary+ #xDA)
(defconstant +key-pad-octal+ #xDB)
(defconstant +key-pad-decimal+ #xDC)
(defconstant +key-pad-hexidecimal+ #xDD)

(defconstant +key-left-ctrl+ #xE0) ;; VK_CONTROL, VK_LCONTROL
(defconstant +key-left-shift+ #xE1) ;; VK_SHIFT, VK_LSHIFT
(defconstant +key-left-alt+ #xE2) ;; VK_MENU, VK_LMENU
(defconstant +key-left-GUI+ #xE3) ;; windows or command key left ;; VK_LWIN
(defconstant +key-right-ctrl+ #xE4) ;; VK_CONTROL, VK_RCONTROL
(defconstant +key-right-shift+ #xE5) ;; VK_SHIFT, VK_RSHIFT
(defconstant +key-right-alt+ #xE6) ;; VK_MENU, VK_RMENU
(defconstant +key-right-GUI+ #xE7) ;; windows or command key right ;; VK_RWIN


(defconstant +ms-ext-key-junja+ #x10000)
(defconstant +ms-ext-key-final+ #x10001)
(defconstant +ms-ext-key-hanja+ #x10002)
(defconstant +ms-ext-key-kanji+ #x10003)

(defparameter *ms-vk->usb-table* (make-array 256 :initial-element nil))

#+win32
(progn
  (setf (aref *ms-vk->usb-table* #_VK_LBUTTON) nil
	(aref *ms-vk->usb-table* #_VK_RBUTTON) nil
	(aref *ms-vk->usb-table* #_VK_CANCEL) nil
	(aref *ms-vk->usb-table* #_VK_MBUTTON) nil
	(aref *ms-vk->usb-table* #_VK_XBUTTON1) nil
	(aref *ms-vk->usb-table* #_VK_XBUTTON2) nil
	(aref *ms-vk->usb-table* #_VK_BACK) +key-backspace+
	(aref *ms-vk->usb-table* #_VK_TAB) +key-tab+

	(aref *ms-vk->usb-table* #_VK_CLEAR) nil ;; clear key
	(aref *ms-vk->usb-table* #_VK_RETURN) +key-enter+

	(aref *ms-vk->usb-table* #_VK_SHIFT) +key-left-shift+
	(aref *ms-vk->usb-table* #_VK_CONTROL) +key-left-ctrl+
	(aref *ms-vk->usb-table* #_VK_MENU) +key-left-alt+
	(aref *ms-vk->usb-table* #_VK_PAUSE) +key-pause+
	(aref *ms-vk->usb-table* #_VK_CAPITAL) +key-caps-lock+
	(aref *ms-vk->usb-table* #_VK_KANA) +key-international-2+
	(aref *ms-vk->usb-table* #_VK_HANGUEL) +key-language-1+
	(aref *ms-vk->usb-table* #_VK_HANGUL) +key-language-1+
	(aref *ms-vk->usb-table* #_VK_JUNJA) +ms-ext-key-junja+
	(aref *ms-vk->usb-table* #_VK_FINAL) +ms-ext-key-final+
	(aref *ms-vk->usb-table* #_VK_HANJA) +ms-ext-key-hanja+
	(aref *ms-vk->usb-table* #_VK_KANJI) +ms-ext-key-kanji+
	(aref *ms-vk->usb-table* #_VK_ESCAPE) +key-escape+
	(aref *ms-vk->usb-table* #_VK_CONVERT) nil
	(aref *ms-vk->usb-table* #_VK_NONCONVERT) nil
	(aref *ms-vk->usb-table* #_VK_ACCEPT) nil
	(aref *ms-vk->usb-table* #_VK_MODECHANGE) nil
	(aref *ms-vk->usb-table* #_VK_SPACE) +key-spacebar+
	(aref *ms-vk->usb-table* #_VK_PRIOR) +key-page-up+
	(aref *ms-vk->usb-table* #_VK_NEXT) +key-page-down+
	(aref *ms-vk->usb-table* #_VK_END) +key-end+
	(aref *ms-vk->usb-table* #_VK_HOME) +key-home+
	(aref *ms-vk->usb-table* #_VK_LEFT) +key-left-arrow+
	(aref *ms-vk->usb-table* #_VK_UP) +key-up-arrow+
	(aref *ms-vk->usb-table* #_VK_RIGHT) +key-right-arrow+
	(aref *ms-vk->usb-table* #_VK_DOWN) +key-down-arrow+
	(aref *ms-vk->usb-table* #_VK_PRINT) nil
	(aref *ms-vk->usb-table* #_VK_EXECUTE) +key-execute+
	(aref *ms-vk->usb-table* #_VK_SNAPSHOT) +key-print-screen+
	(aref *ms-vk->usb-table* #_VK_INSERT) +key-insert+
	(aref *ms-vk->usb-table* #_VK_DELETE) +key-delete+
	(aref *ms-vk->usb-table* #_VK_HELP) +key-help+
	
	(aref *ms-vk->usb-table* 48) +key-0+
	(aref *ms-vk->usb-table* 49) +key-1+
	(aref *ms-vk->usb-table* 50) +key-2+
	(aref *ms-vk->usb-table* 51) +key-3+
	(aref *ms-vk->usb-table* 52) +key-4+
	(aref *ms-vk->usb-table* 53) +key-5+
	(aref *ms-vk->usb-table* 54) +key-6+
	(aref *ms-vk->usb-table* 55) +key-7+
	(aref *ms-vk->usb-table* 56) +key-8+
	(aref *ms-vk->usb-table* 57) +key-9+

	(aref *ms-vk->usb-table* 65) +key-A+
	(aref *ms-vk->usb-table* 66) +key-B+
	(aref *ms-vk->usb-table* 67) +key-C+
	(aref *ms-vk->usb-table* 68) +key-D+
	(aref *ms-vk->usb-table* 69) +key-E+
	(aref *ms-vk->usb-table* 70) +key-F+
	(aref *ms-vk->usb-table* 71) +key-G+
	(aref *ms-vk->usb-table* 72) +key-H+
	(aref *ms-vk->usb-table* 73) +key-I+
	(aref *ms-vk->usb-table* 74) +key-J+
	(aref *ms-vk->usb-table* 75) +key-K+
	(aref *ms-vk->usb-table* 76) +key-L+
	(aref *ms-vk->usb-table* 77) +key-M+
	(aref *ms-vk->usb-table* 78) +key-N+
	(aref *ms-vk->usb-table* 79) +key-O+
	(aref *ms-vk->usb-table* 80) +key-P+
	(aref *ms-vk->usb-table* 81) +key-Q+
	(aref *ms-vk->usb-table* 82) +key-R+
	(aref *ms-vk->usb-table* 83) +key-S+
	(aref *ms-vk->usb-table* 84) +key-T+
	(aref *ms-vk->usb-table* 85) +key-U+
	(aref *ms-vk->usb-table* 86) +key-V+
	(aref *ms-vk->usb-table* 87) +key-W+
	(aref *ms-vk->usb-table* 88) +key-X+
	(aref *ms-vk->usb-table* 89) +key-Y+
	(aref *ms-vk->usb-table* 90) +key-Z+

	(aref *ms-vk->usb-table* #_VK_LWIN) +key-left-GUI+
	(aref *ms-vk->usb-table* #_VK_RWIN) +key-right-GUI+
	(aref *ms-vk->usb-table* #_VK_APPS) +key-application+

	(aref *ms-vk->usb-table* #_VK_SLEEP) nil
	(aref *ms-vk->usb-table* #_VK_NUMPAD0) +key-pad-0+
	(aref *ms-vk->usb-table* #_VK_NUMPAD1) +key-pad-1+
	(aref *ms-vk->usb-table* #_VK_NUMPAD2) +key-pad-2+
	(aref *ms-vk->usb-table* #_VK_NUMPAD3) +key-pad-3+
	(aref *ms-vk->usb-table* #_VK_NUMPAD4) +key-pad-4+
	(aref *ms-vk->usb-table* #_VK_NUMPAD5) +key-pad-5+
	(aref *ms-vk->usb-table* #_VK_NUMPAD6) +key-pad-6+
	(aref *ms-vk->usb-table* #_VK_NUMPAD7) +key-pad-7+
	(aref *ms-vk->usb-table* #_VK_NUMPAD8) +key-pad-8+
	(aref *ms-vk->usb-table* #_VK_NUMPAD9) +key-pad-9+
	(aref *ms-vk->usb-table* #_VK_MULTIPLY) +key-pad-asterisk+
	(aref *ms-vk->usb-table* #_VK_ADD) +key-pad-plus+
	(aref *ms-vk->usb-table* #_VK_SEPARATOR) +key-separator+
	(aref *ms-vk->usb-table* #_VK_SUBTRACT) +key-pad-minus+
	(aref *ms-vk->usb-table* #_VK_DECIMAL) +key-pad-decimal-point+
	(aref *ms-vk->usb-table* #_VK_DIVIDE) +key-pad-slash+
	(aref *ms-vk->usb-table* #_VK_F1) +key-F1+
	(aref *ms-vk->usb-table* #_VK_F2) +key-F2+
	(aref *ms-vk->usb-table* #_VK_F3) +key-F3+
	(aref *ms-vk->usb-table* #_VK_F4) +key-F4+
	(aref *ms-vk->usb-table* #_VK_F5) +key-F5+
	(aref *ms-vk->usb-table* #_VK_F6) +key-F6+
	(aref *ms-vk->usb-table* #_VK_F7) +key-F7+
	(aref *ms-vk->usb-table* #_VK_F8) +key-F8+
	(aref *ms-vk->usb-table* #_VK_F9) +key-F9+
	(aref *ms-vk->usb-table* #_VK_F10) +key-F10+
	(aref *ms-vk->usb-table* #_VK_F11) +key-F11+
	(aref *ms-vk->usb-table* #_VK_F12) +key-F12+
	(aref *ms-vk->usb-table* #_VK_F13) +key-F13+
	(aref *ms-vk->usb-table* #_VK_F14) +key-F14+
	(aref *ms-vk->usb-table* #_VK_F15) +key-F15+
	(aref *ms-vk->usb-table* #_VK_F16) +key-F16+
	(aref *ms-vk->usb-table* #_VK_F17) +key-F17+
	(aref *ms-vk->usb-table* #_VK_F18) +key-F18+
	(aref *ms-vk->usb-table* #_VK_F19) +key-F19+
	(aref *ms-vk->usb-table* #_VK_F20) +key-F20+
	(aref *ms-vk->usb-table* #_VK_F21) +key-F21+
	(aref *ms-vk->usb-table* #_VK_F22) +key-F22+
	(aref *ms-vk->usb-table* #_VK_F23) +key-F23+
	(aref *ms-vk->usb-table* #_VK_F24) +key-F24+

	(aref *ms-vk->usb-table* #_VK_NUMLOCK) +key-num-lock+
	(aref *ms-vk->usb-table* #_VK_SCROLL) +key-scroll-lock+
	(aref *ms-vk->usb-table* #_VK_LCONTROL) +key-left-ctrl+
	(aref *ms-vk->usb-table* #_VK_RCONTROL) +key-right-ctrl+
	(aref *ms-vk->usb-table* #_VK_LMENU) +key-left-alt+
	(aref *ms-vk->usb-table* #_VK_RMENU) +key-right-alt+

	(aref *ms-vk->usb-table* #_VK_BROWSER_BACK) nil
	(aref *ms-vk->usb-table* #_VK_BROWSER_FORWARD) nil
	(aref *ms-vk->usb-table* #_VK_BROWSER_REFRESH) nil
	(aref *ms-vk->usb-table* #_VK_BROWSER_STOP) nil
	(aref *ms-vk->usb-table* #_VK_BROWSER_SEARCH) nil
	(aref *ms-vk->usb-table* #_VK_BROWSER_FAVORITES) nil
	(aref *ms-vk->usb-table* #_VK_BROWSER_HOME) nil
	(aref *ms-vk->usb-table* #_VK_VOLUME_MUTE) nil
	(aref *ms-vk->usb-table* #_VK_VOLUME_DOWN) nil
	(aref *ms-vk->usb-table* #_VK_VOLUME_UP) nil
	(aref *ms-vk->usb-table* #_VK_MEDIA_NEXT_TRACK) nil
	(aref *ms-vk->usb-table* #_VK_MEDIA_PREV_TRACK) nil
	(aref *ms-vk->usb-table* #_VK_MEDIA_STOP) nil
	(aref *ms-vk->usb-table* #_VK_MEDIA_PLAY_PAUSE) nil
	(aref *ms-vk->usb-table* #_VK_LAUNCH_MAIL) nil
	(aref *ms-vk->usb-table* #_VK_LAUNCH_MEDIA_SELECT) nil
	(aref *ms-vk->usb-table* #_VK_LAUNCH_APP1) nil
	(aref *ms-vk->usb-table* #_VK_LAUNCH_APP2) nil
	
	(aref *ms-vk->usb-table* #_VK_OEM_1) nil
	(aref *ms-vk->usb-table* #_VK_OEM_PLUS) nil
	(aref *ms-vk->usb-table* #_VK_OEM_MINUS) nil
	(aref *ms-vk->usb-table* #_VK_OEM_PERIOD) nil
	(aref *ms-vk->usb-table* #_VK_OEM_2) nil
	(aref *ms-vk->usb-table* #_VK_OEM_3) nil
	
	(aref *ms-vk->usb-table* #_VK_OEM_4) nil
	(aref *ms-vk->usb-table* #_VK_OEM_5) nil
	(aref *ms-vk->usb-table* #_VK_OEM_6) nil 
	(aref *ms-vk->usb-table* #_VK_OEM_7) nil
	(aref *ms-vk->usb-table* #_VK_OEM_8) nil

	(aref *ms-vk->usb-table* #_VK_OEM_102) nil
	(aref *ms-vk->usb-table* #_VK_PROCESSKEY) nil
	(aref *ms-vk->usb-table* #_VK_PACKET) nil

	(aref *ms-vk->usb-table* #_VK_ATTN) +key-sys-req+
	(aref *ms-vk->usb-table* #_VK_CRSEL) +key-cr-sel-props+
	(aref *ms-vk->usb-table* #_VK_EXSEL) +key-ex-sel+
	(aref *ms-vk->usb-table* #_VK_EREOF) nil
	(aref *ms-vk->usb-table* #_VK_PLAY) nil
	(aref *ms-vk->usb-table* #_VK_ZOOM) nil
	(aref *ms-vk->usb-table* #_VK_NONAME) nil
	(aref *ms-vk->usb-table* #_VK_PA1) nil
	(aref *ms-vk->usb-table* #_VK_OEM_CLEAR) nil))

