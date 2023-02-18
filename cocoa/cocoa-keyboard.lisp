(in-package :clui)

(defun create-cocoa-key-tables (cocoa-desktop)
  (let ((keycodes (display-keycodes cocoa-desktop))
	(scancodes (display-scancodes cocoa-desktop)))
    
    ;; i don't know where these scancodes come from.
    ;; pre macos 10.12 maybe?

    (setf (aref keycodes #x1D) +key-0+
	  (aref keycodes #x12) +key-1+
	  (aref keycodes #x13) +key-2+
	  (aref keycodes #x14) +key-3+
	  (aref keycodes #x15) +key-4+
	  (aref keycodes #x17) +key-5+
	  (aref keycodes #x16) +key-6+
	  (aref keycodes #x1A) +key-7+
	  (aref keycodes #x1C) +key-8+
	  (aref keycodes #x19) +key-9+

	  (aref keycodes #x00) +key-A+
	  (aref keycodes #x0B) +key-B+
	  (aref keycodes #x08) +key-C+
	  (aref keycodes #x02) +key-D+
	  (aref keycodes #x0E) +key-E+
	  (aref keycodes #x03) +key-F+
	  (aref keycodes #x05) +key-G+
	  (aref keycodes #x04) +key-H+
	  (aref keycodes #x22) +key-I+
	  (aref keycodes #x26) +key-J+
	  (aref keycodes #x28) +key-K+
	  (aref keycodes #x25) +key-L+
	  (aref keycodes #x2E) +key-M+
	  (aref keycodes #x2D) +key-N+
	  (aref keycodes #x1F) +key-O+
	  (aref keycodes #x23) +key-P+
	  (aref keycodes #x0C) +key-Q+
	  (aref keycodes #x0F) +key-R+
	  (aref keycodes #x01) +key-S+
	  (aref keycodes #x11) +key-T+
	  (aref keycodes #x20) +key-U+
	  (aref keycodes #x09) +key-V+
	  (aref keycodes #x0D) +key-W+
	  (aref keycodes #x07) +key-X+
	  (aref keycodes #x10) +key-Y+
	  (aref keycodes #x06) +key-Z+

	  (aref keycodes #x27) +key-apostrophe+
	  (aref keycodes #x2A) +key-backslash+
	  (aref keycodes #x2B) +key-comma+
	  (aref keycodes #x18) +key-equals+
	  (aref keycodes #x32) +key-grave-accent+
	  (aref keycodes #x21) +key-left-bracket+
	  (aref keycodes #x1B) +key-minus+
	  (aref keycodes #x2F) +key-period+
	  (aref keycodes #x1E) +key-right-bracket+
	  (aref keycodes #x29) +key-semicolon+
	  (aref keycodes #x2C) +key-slash+
	  (aref keycodes #x0A) +key-international-1+

	  (aref keycodes #x33) +key-backspace+
	  (aref keycodes #x39) +key-caps-lock+
	  (aref keycodes #x75) +key-delete+
	  (aref keycodes #x7D) +key-down-arrow+
	  (aref keycodes #x77) +key-end+
	  (aref keycodes #x24) +key-enter+
	  (aref keycodes #x35) +key-escape+
	  
	  (aref keycodes #x7A) +key-F1+
	  (aref keycodes #x78) +key-F2+
	  (aref keycodes #x63) +key-F3+
	  (aref keycodes #x76) +key-F4+
	  (aref keycodes #x60) +key-F5+
	  (aref keycodes #x61) +key-F6+
	  (aref keycodes #x62) +key-F7+
	  (aref keycodes #x64) +key-F8+
	  (aref keycodes #x65) +key-F9+
	  (aref keycodes #x6D) +key-F10+
	  (aref keycodes #x67) +key-F11+
	  (aref keycodes #x6F) +key-F12+

	  (aref keycodes #x69) +key-print-screen+

	  (aref keycodes #x6B) +key-F14+
	  (aref keycodes #x71) +key-F15+
	  (aref keycodes #x6A) +key-F16+
	  (aref keycodes #x40) +key-F17+
	  (aref keycodes #x4F) +key-F18+
	  (aref keycodes #x50) +key-F19+
	  
	  (aref keycodes #x73) +key-home+
	  (aref keycodes #x72) +key-insert+
	  (aref keycodes #x7B) +key-left-arrow+
	  (aref keycodes #x3A) +key-left-alt+
	  (aref keycodes #x3B) +key-left-ctrl+
	  (aref keycodes #x38) +key-left-shift+
	  (aref keycodes #x37) +key-left-GUI+
	  (aref keycodes #x6E) +key-menu+
	  (aref keycodes #x47) +key-num-lock+
	  (aref keycodes #x79) +key-page-down+
	  (aref keycodes #x74) +key-page-up+
	  (aref keycodes #x7C) +key-right-arrow+
	  (aref keycodes #x3D) +key-right-alt+
	  (aref keycodes #x3E) +key-right-ctrl+
	  (aref keycodes #x3C) +key-right-shift+
	  (aref keycodes #x36) +key-right-GUI+
	  (aref keycodes #x31) +key-spacebar+
	  (aref keycodes #x30) +key-tab+
	  (aref keycodes #x7E) +key-up-arrow+

	  (aref keycodes #x52) +key-pad-0+
	  (aref keycodes #x53) +key-pad-1+
	  (aref keycodes #x54) +key-pad-2+
	  (aref keycodes #x55) +key-pad-3+
	  (aref keycodes #x56) +key-pad-4+
	  (aref keycodes #x57) +key-pad-5+
	  (aref keycodes #x58) +key-pad-6+
	  (aref keycodes #x59) +key-pad-7+
	  (aref keycodes #x5B) +key-pad-8+
	  (aref keycodes #x5C) +key-pad-9+
	  (aref keycodes #x45) +key-pad-plus+
	  (aref keycodes #x41) +key-pad-decimal+
	  (aref keycodes #x4B) +key-pad-slash+
	  (aref keycodes #x4C) +key-pad-enter+
	  (aref keycodes #x51) +key-pad-equals+
	  (aref keycodes #x43) +key-pad-asterisk+
	  (aref keycodes #x4E) +key-pad-minus+)

    (loop for scancode from 0 below 256
	  when (aref keycodes scancode)
	    do (setf (aref scancodes (aref keycodes scancode)) scancode))

    (values)))
	  
