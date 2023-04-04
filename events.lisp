(in-package :clui)

(defgeneric handle-event (client event))

(defclass event-mixin ()
  ((timestamp :initarg :timestamp
	      :accessor event-timestamp)))

(defmethod clui:handle-event :after (object (event clui::event-mixin))
  (declare (ignore object))
  (setf (display-last-event (default-display)) event))

(defmethod handle-event (object event)
  (declare (ignorable object event))
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
			:accessor event-lock-modifier-state)))

(defmethod print-object ((event input-event-mixin) stream)
  (print-unreadable-object (event stream :type t :identity t)
    (princ (intern-input-event (input-event-code event) (event-modifier-state event)) stream)
    event))

(defclass pointer-event-mixin (input-event-mixin)
  ((pointer :initarg :pointer
	    :accessor pointer-event-pointer)
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

;; todo: make this at the gesture level
(defclass pointer-button-hold-event-mixin (pointer-button-event-mixin)
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


(defun input-char (window codepoint mods lock-mods plain?)
  
  (when (or (< codepoint 32)
	    (> 126 codepoint 160))
    (return-from input-char))
  
  (unless (lock-key-mods? window)
    (setq mods (logand mods (lognot (logior +caps-lock-modifier+ +num-lock-modifier+)))))

  (when plain?
    (handle-event window (make-instance 'character-event
					:window window
					:character (code-char codepoint)
					:modifier-state mods
					:lock-modifier-state lock-mods)))
  (values))

(defun input-cursor-pos (window xpos ypos mods lock-mods)
  (when (and (= (virtual-cursor-pos-x window) xpos)
	     (= (virtual-cursor-pos-y window) ypos))
    (return-from input-cursor-pos (values)))

  (handle-event window (make-instance 'pointer-motion-event
				      :window
				      :input-code +pointer-move+
				      :x xpos
				      :y ypos
				      :modifier-state mods
				      :lock-modifier-state lock-mods))
  (values))

(defclass keyboard-event-mixin (input-event-mixin)
  ())


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

(defmethod print-object ((event character-event-mixin) stream)
  (print-unreadable-object (event stream :type t :identity t)
    (format stream "~S" (input-event-character event))))

(defclass clui.v0:character-event (character-event-mixin)
  ())

(defclass joystick-event-mixin (input-event-mixin)
  ())

(defclass spaceball-event-mixin (joystick-event-mixin)
  ())

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

(define-input-code +key-A+ 4 "A") ;; vk 65
(define-input-code +key-B+ 5 "B") ;; vk 66
(define-input-code +key-C+ 6 "C") ;; vk 67
(define-input-code +key-D+ 7 "D") ;; vk 68
(define-input-code +key-E+ 8 "E") ;; vk 69
(define-input-code +key-F+ 9 "F") ;; vk 70
(define-input-code +key-G+ #x0A "G") ;; vk 71
(define-input-code +key-H+ #x0B "H") ;; vk 72
(define-input-code +key-I+ #x0C "I") ;; vk 73
(define-input-code +key-J+ #x0D "J") ;; vk 74
(define-input-code +key-K+ #x0E "K") ;; vk 75
(define-input-code +key-L+ #x0F "L") ;; vk 76
(define-input-code +key-M+ #x10 "M") ;; vk 77
(define-input-code +key-N+ #x11 "N") ;; vk 78
(define-input-code +key-O+ #x12 "O") ;; vk 79
(define-input-code +key-P+ #x13 "P") ;; vk 80
(define-input-code +key-Q+ #x14 "Q") ;; vk 81
(define-input-code +key-R+ #x15 "R") ;; vk 82
(define-input-code +key-S+ #x16 "S") ;; vk 83
(define-input-code +key-T+ #x17 "T") ;; vk 84
(define-input-code +key-U+ #x18 "U") ;; vk 85
(define-input-code +key-V+ #x19 "V") ;; vk 86
(define-input-code +key-W+ #x1A "W") ;; vk 87
(define-input-code +key-X+ #x1B "X") ;; vk 88
(define-input-code +key-Y+ #x1C "Y") ;; vk 89
(define-input-code +key-Z+ #x1D "Z") ;; vk 90

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
(define-input-code +key-page-down+ #x4E "PageDn") ;; VK_NEXT
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

(define-input-code +pointer-move+ 0 "MouseMove")
(define-input-code +pointer-left-button+ 1 "LeftMouse")
(define-input-code +pointer-right-button+ 2 "RightMouse")
(define-input-code +pointer-middle-button+ 3 "MiddleMouse")
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

(defvar *input-event-id-high-bytes* (make-array 256 :initial-element nil))

(defun get-input-event-id* (input-code bits)
  (let* ((high-byte (ash input-code -8))
         (low-byte-vector (svref *input-event-id-high-bytes* high-byte)))
    (unless low-byte-vector
      (let ((new-vector (make-array 256 :initial-element nil)))
        (setf (svref *input-event-id-high-bytes* high-byte) new-vector)
        (setf low-byte-vector new-vector)))
    (let* ((low-byte (ldb (byte 8 0) input-code))
           (bit-vector (svref low-byte-vector low-byte)))
      (unless bit-vector
        (let ((new-vector (make-array 256 :initial-element nil)))
          (setf (svref low-byte-vector low-byte) new-vector)
          (setf bit-vector new-vector)))
      (let ((input-event-id (svref bit-vector bits)))
        (if input-event-id
            input-event-id
            (setf (svref bit-vector bits) (%make-input-event-id input-code bits)))))))

(defun key-character-class (char)
  (case char
    (#\- :modifier-terminator)
    (#\Space :event-terminator)
    (#\" :eof)
    (t :unicode)))

(defconstant input-event-id-escape-char #\\
  "The escape character that #k uses.")

(defun get-key-char (stream)
  (let ((char (read-char stream t nil t)))
    (cond ((char= char input-event-id-escape-char)
           (let ((char (read-char stream t nil t)))
             (values char :escaped)))
          (t (values char (key-character-class char))))))

(defun skip-whitespace (&optional (stream *standard-input*))
  (peek-char t stream))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *modifiers-to-internal-masks* ())

  (defvar *modifier-count* 0
    "The number of modifiers that is currently defined.")

  (defvar *all-modifier-names* ()
    "A list of all the names of defined modifiers."))

(defmacro define-input-event-modifier (symbol long-name short-name)
  "This establishes long-name and short-name as modifier names for purposes
   of specifying input-event-ids in #k syntax.  The names are case-insensitive and
   must be strings.  If either name is already defined, this signals an error."
  (let ((new-bits (ash 1 *modifier-count*)))
    (flet ((frob (name)
             (when (assoc name *modifiers-to-internal-masks*
                          :test #'string-equal)
	       (return-from define-input-event-modifier nil))))
      (frob long-name)
      (frob short-name))
    (unwind-protect
	 (progn
	   (push (cons long-name new-bits) *modifiers-to-internal-masks*)
	   (push (cons short-name new-bits) *modifiers-to-internal-masks*)
           (pushnew long-name *all-modifier-names* :test #'string-equal)
	   ;; Sometimes the long-name is the same as the short-name.
           (pushnew short-name *all-modifier-names* :test #'string-equal))
      (incf *modifier-count*))
    `(defconstant ,symbol ,new-bits)))

(define-input-event-modifier +hyper-modifier+ "Hyper" "H")
(define-input-event-modifier +num-lock-modifier+ "NumLock" "NumL")
(define-input-event-modifier +caps-lock-modifier+ "CapsLock" "CapsL")
(define-input-event-modifier +super-modifier+ "Super" "S")
(define-input-event-modifier +meta-modifier+ "Meta" "M")
(define-input-event-modifier +alt-modifier+ "Alt" "Alt")
(define-input-event-modifier +ctrl-modifier+ "Ctrl" "C")
(define-input-event-modifier +shift-modifier+ "Shift" "Shift")








(defun input-event-id-modifier-mask (modifier-name)
  "This function returns a mask for modifier-name.  This mask is suitable
   for use with INPUT-EVENT-ID-BITS.  If modifier-name is undefined, this signals
   an error."
  (let ((res (cdr (assoc modifier-name *modifiers-to-internal-masks*
                         :test #'string-equal))))
    (unless res (error "Undefined input-event-id modifier -- ~S." modifier-name))
    res))

(defstruct (input-event-id (:print-function %print-input-event-id)
			       (:constructor %make-input-event-id (scancode modifier-bits)))
  (modifier-bits nil :type fixnum)
  (scancode nil :type fixnum))

(defun intern-input-event (object &optional (bits 0))
  "This returns a input-event-id described by object with bits.  Object is one of
   keysym, string, or input-event-id.  When object is a input-event-id, this uses
   INPUT-EVENT-ID-KEYSYM.  You can form bits with MAKE-INPUT-EVENT-ID-BITS or
   INPUT-EVENT-ID-MODIFIER-MASK."
  (etypecase object
    (integer
     (unless (aref *keysyms* object)
       (error "~S is an undefined input-code." object))
     (get-input-event-id* object bits))
    #|(character
     (let* ((name (char-name object))
            (keysym (name-keysym (or name (string object)))))
       (unless keysym
         (error "~S is an undefined keysym." object))
       (get-input-event-id* keysym bits)))|#
    (string
     (let ((input-code (keysym-input-code object)))
       (unless input-code
         (error "~S is an undefined keysym." object))
       (get-input-event-id* input-code bits)))
    (input-event-id
     (get-input-event-id* (input-event-id-scancode object) bits))))

(defun %print-input-event-id (object stream ignore)
  (declare (ignore ignore))
  (write-string "#k\"" stream)
  (print-pretty-input-event-id object stream)
  (write-char #\" stream))

(defun print-pretty-input-event-id (input-event-id &optional (stream *standard-output*)
							     long-names-p)
  "This prints input-event-id to stream.  Long-names-p indicates whether modifier
   names should appear using the long name or short name."
  (do ((map (if long-names-p
                (cdr *modifiers-to-internal-masks*)
                *modifiers-to-internal-masks*)
            (cddr map)))
      ((null map))
    (when (not (zerop (logand (cdar map) (input-event-id-modifier-bits input-event-id))))
      (write-string (caar map) stream)
      (write-char #\- stream)))
  (let* ((name (aref *keysyms* (input-event-id-scancode input-event-id)))
         (spacep (position #\space (the simple-string name))))
    (when spacep (write-char #\< stream))
    (write-string name stream)
    (when spacep (write-char #\> stream))))

(defun make-input-event-id-bits (&rest modifier-names)
  "This returns bits suitable for INTERN-INPUT-EVENT-ID from the supplied modifier
   names.  If any name is undefined, this signals an error."
  (let ((mask 0))
    (dolist (mod modifier-names mask)
      (let ((this-mask (cdr (assoc mod *modifiers-to-internal-masks*
                                   :test #'string-equal))))
        (unless this-mask (error "~S is an undefined modifier name." mod))
        (setf mask (logior mask this-mask))))))

(defun input-event-id-bits-modifiers (bits)
  "This returns a list of input-event-id modifier names, one for each modifier
   set in bits."
  (let ((res nil))
    (do ((map (cdr *modifiers-to-internal-masks*) (cddr map)))
        ((null map) res)
      (when (logtest bits (cdar map))
        (push (caar map) res)))))

(defun parse-input-event-id (stream sub-char count)
  (declare (ignore sub-char count))
  (let ((id-namestring
	  (make-array 30 :adjustable t :fill-pointer 0 :element-type 'character)))
    
    (prog ((bits 0)
           (input-event-ids ())
           char class)
       (unless (char= (read-char stream) #\")
         (error "Keys must be delimited by ~S." #\"))
       ;; Skip any leading spaces in the string.
       (skip-whitespace stream)
       (multiple-value-setq (char class) (get-key-char stream))
       (ecase class
         ((:unicode :other :escaped) (go ID))
         (:ISO-start (go ISOCHAR))
         (:ISO-end (error "Angle brackets must be escaped."))
         (:modifier-terminator (error "Dash must be escaped."))
         (:EOF (error "No key to read.")))
     ID
       (vector-push-extend char id-namestring)
       (multiple-value-setq (char class) (get-key-char stream))
       (ecase class
         ((:unicode :other :escaped) (go ID))
         (:event-terminator (go GOT-CHAR))
         (:modifier-terminator (go GOT-MODIFIER))
         ((:ISO-start :ISO-end) (error "Angle brackets must be escaped."))
         (:EOF (go GET-LAST-CHAR)))
     GOT-CHAR
       (push `(intern-input-event ,(copy-seq id-namestring) ,bits)
             input-event-ids)
       (setf (fill-pointer id-namestring) 0)
       (setf bits 0)
       ;; Skip any whitespace between characters.
       (skip-whitespace stream)
       (multiple-value-setq (char class) (get-key-char stream))
       (ecase class
         ((:unicode :other :escaped) (go ID))
         (:ISO-start (go ISOCHAR))
         (:ISO-end (error "Angle brackets must be escaped."))
         (:modifier-terminator (error "Dash must be escaped."))
         (:EOF (go FINAL)))
     GOT-MODIFIER
       (let ((modifier-name (car (assoc id-namestring
                                        *modifiers-to-internal-masks*
                                        :test #'string-equal))))
         (unless modifier-name
           (error "~S is not a defined modifier." id-namestring))
         (setf (fill-pointer id-namestring) 0)
         (setf bits (logior bits (input-event-id-modifier-mask modifier-name))))
       (multiple-value-setq (char class) (get-key-char stream))
       (ecase class
         ((:unicode :other :escaped) (go ID))
         (:ISO-start (go ISOCHAR))
         (:ISO-end (error "Angle brackets must be escaped."))
         (:modifier-terminator (error "Dash must be escaped."))
         (:EOF (error "Expected something naming a input-event-id, got EOF.")))
     ISOCHAR
       (multiple-value-setq (char class) (get-key-char stream))
       (ecase class
         ((:unicode :event-terminator :other :escaped)
          (vector-push-extend char id-namestring)
          (go ISOCHAR))
         (:ISO-start (error "Open Angle must be escaped."))
         (:modifier-terminator (error "Dash must be escaped."))
         (:EOF (error "Bad syntax in key specification, hit eof."))
         (:ISO-end (go GOT-CHAR)))
     GET-LAST-CHAR
       (push `(intern-input-event ,(copy-seq id-namestring) ,bits)
             input-event-ids)
     FINAL
       (return (if (cdr input-event-ids)
                   `(vector ,@(nreverse input-event-ids))
                   `,(car input-event-ids))))))

(set-dispatch-macro-character #\# #\k 'parse-input-event-id)

(defmethod input-event-id ((event input-event-mixin))
  (intern-input-event (input-event-code event) (event-modifier-state event)))

(defmethod input-event-id ((event character-event-mixin))
  (input-event-character event))
