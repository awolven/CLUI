(in-package :clui)


(defun create-win32-key-tables (win32-desktop)
  (let ((keycodes (display-keycodes win32-desktop))
	(scancodes (display-scancodes win32-desktop)))
    
    (setf (aref keycodes #_VK_LBUTTON) nil
	  (aref keycodes #_VK_RBUTTON) nil
	  (aref keycodes #_VK_CANCEL) nil
	  (aref keycodes #_VK_MBUTTON) nil
	  (aref keycodes #_VK_XBUTTON1) nil
	  (aref keycodes #_VK_XBUTTON2) nil
	  (aref keycodes #_VK_BACK) +key-backspace+
	  (aref keycodes #_VK_TAB) +key-tab+

	  (aref keycodes #_VK_CLEAR) nil ;; clear key
	  (aref keycodes #_VK_RETURN) +key-enter+

	  (aref keycodes #_VK_SHIFT) +key-left-shift+
	  (aref keycodes #_VK_CONTROL) +key-left-ctrl+
	  (aref keycodes #_VK_MENU) +key-left-alt+
	  (aref keycodes #_VK_PAUSE) +key-pause+
	  (aref keycodes #_VK_CAPITAL) +key-caps-lock+
	  (aref keycodes #_VK_KANA) +key-international-2+
	  ;;(aref keycodes #_VK_HANGUEL) +key-language-1+
	  (aref keycodes #_VK_HANGUL) +key-language-1+
	  ;;(aref keycodes #_VK_JUNJA) +ms-ext-key-junja+
	  ;;(aref keycodes #_VK_FINAL) +ms-ext-key-final+
	  ;;(aref keycodes #_VK_HANJA) +ms-ext-key-hanja+
	  ;;(aref keycodes #_VK_KANJI) +ms-ext-key-kanji+
	  (aref keycodes #_VK_ESCAPE) +key-escape+
	  (aref keycodes #_VK_CONVERT) nil
	  (aref keycodes #_VK_NONCONVERT) nil
	  (aref keycodes #_VK_ACCEPT) nil
	  (aref keycodes #_VK_MODECHANGE) nil
	  (aref keycodes #_VK_SPACE) +key-spacebar+
	  (aref keycodes #_VK_PRIOR) +key-page-up+
	  (aref keycodes #_VK_NEXT) +key-page-down+
	  (aref keycodes #_VK_END) +key-end+
	  (aref keycodes #_VK_HOME) +key-home+
	  (aref keycodes #_VK_LEFT) +key-left-arrow+
	  (aref keycodes #_VK_UP) +key-up-arrow+
	  (aref keycodes #_VK_RIGHT) +key-right-arrow+
	  (aref keycodes #_VK_DOWN) +key-down-arrow+
	  (aref keycodes #_VK_PRINT) nil
	  (aref keycodes #_VK_EXECUTE) +key-execute+
	  (aref keycodes #_VK_SNAPSHOT) +key-print-screen+
	  (aref keycodes #_VK_INSERT) +key-insert+
	  (aref keycodes #_VK_DELETE) +key-delete+
	  (aref keycodes #_VK_HELP) +key-help+
	
	  (aref keycodes 48) +key-0+
	  (aref keycodes 49) +key-1+
	  (aref keycodes 50) +key-2+
	  (aref keycodes 51) +key-3+
	  (aref keycodes 52) +key-4+
	  (aref keycodes 53) +key-5+
	  (aref keycodes 54) +key-6+
	  (aref keycodes 55) +key-7+
	  (aref keycodes 56) +key-8+
	  (aref keycodes 57) +key-9+

	  (aref keycodes 65) +key-A+
	  (aref keycodes 66) +key-B+
	  (aref keycodes 67) +key-C+
	  (aref keycodes 68) +key-D+
	  (aref keycodes 69) +key-E+
	  (aref keycodes 70) +key-F+
	  (aref keycodes 71) +key-G+
	  (aref keycodes 72) +key-H+
	  (aref keycodes 73) +key-I+
	  (aref keycodes 74) +key-J+
	  (aref keycodes 75) +key-K+
	  (aref keycodes 76) +key-L+
	  (aref keycodes 77) +key-M+
	  (aref keycodes 78) +key-N+
	  (aref keycodes 79) +key-O+
	  (aref keycodes 80) +key-P+
	  (aref keycodes 81) +key-Q+
	  (aref keycodes 82) +key-R+
	  (aref keycodes 83) +key-S+
	  (aref keycodes 84) +key-T+
	  (aref keycodes 85) +key-U+
	  (aref keycodes 86) +key-V+
	  (aref keycodes 87) +key-W+
	  (aref keycodes 88) +key-X+
	  (aref keycodes 89) +key-Y+
	  (aref keycodes 90) +key-Z+

	  (aref keycodes #_VK_LWIN) +key-left-GUI+
	  (aref keycodes #_VK_RWIN) +key-right-GUI+
	  (aref keycodes #_VK_APPS) +key-application+

	  (aref keycodes #_VK_SLEEP) nil
	  (aref keycodes #_VK_NUMPAD0) +key-pad-0+
	  (aref keycodes #_VK_NUMPAD1) +key-pad-1+
	  (aref keycodes #_VK_NUMPAD2) +key-pad-2+
	  (aref keycodes #_VK_NUMPAD3) +key-pad-3+
	  (aref keycodes #_VK_NUMPAD4) +key-pad-4+
	  (aref keycodes #_VK_NUMPAD5) +key-pad-5+
	  (aref keycodes #_VK_NUMPAD6) +key-pad-6+
	  (aref keycodes #_VK_NUMPAD7) +key-pad-7+
	  (aref keycodes #_VK_NUMPAD8) +key-pad-8+
	  (aref keycodes #_VK_NUMPAD9) +key-pad-9+
	  (aref keycodes #_VK_MULTIPLY) +key-pad-asterisk+
	  (aref keycodes #_VK_ADD) +key-pad-plus+
	  (aref keycodes #_VK_SEPARATOR) +key-separator+
	  (aref keycodes #_VK_SUBTRACT) +key-pad-minus+
	  (aref keycodes #_VK_DECIMAL) +key-pad-decimal-point+
	  (aref keycodes #_VK_DIVIDE) +key-pad-slash+
	  (aref keycodes #_VK_F1) +key-F1+
	  (aref keycodes #_VK_F2) +key-F2+
	  (aref keycodes #_VK_F3) +key-F3+
	  (aref keycodes #_VK_F4) +key-F4+
	  (aref keycodes #_VK_F5) +key-F5+
	  (aref keycodes #_VK_F6) +key-F6+
	  (aref keycodes #_VK_F7) +key-F7+
	  (aref keycodes #_VK_F8) +key-F8+
	  (aref keycodes #_VK_F9) +key-F9+
	  (aref keycodes #_VK_F10) +key-F10+
	  (aref keycodes #_VK_F11) +key-F11+
	  (aref keycodes #_VK_F12) +key-F12+
	  (aref keycodes #_VK_F13) +key-F13+
	  (aref keycodes #_VK_F14) +key-F14+
	  (aref keycodes #_VK_F15) +key-F15+
	  (aref keycodes #_VK_F16) +key-F16+
	  (aref keycodes #_VK_F17) +key-F17+
	  (aref keycodes #_VK_F18) +key-F18+
	  (aref keycodes #_VK_F19) +key-F19+
	  (aref keycodes #_VK_F20) +key-F20+
	  (aref keycodes #_VK_F21) +key-F21+
	  (aref keycodes #_VK_F22) +key-F22+
	  (aref keycodes #_VK_F23) +key-F23+
	  (aref keycodes #_VK_F24) +key-F24+

	  (aref keycodes #_VK_NUMLOCK) +key-num-lock+
	  (aref keycodes #_VK_SCROLL) +key-scroll-lock+
	  (aref keycodes #_VK_LCONTROL) +key-left-ctrl+
	  (aref keycodes #_VK_RCONTROL) +key-right-ctrl+
	  (aref keycodes #_VK_LMENU) +key-left-alt+
	  (aref keycodes #_VK_RMENU) +key-right-alt+

	  (aref keycodes #_VK_BROWSER_BACK) nil
	  (aref keycodes #_VK_BROWSER_FORWARD) nil
	  (aref keycodes #_VK_BROWSER_REFRESH) nil
	  (aref keycodes #_VK_BROWSER_STOP) nil
	  (aref keycodes #_VK_BROWSER_SEARCH) nil
	  (aref keycodes #_VK_BROWSER_FAVORITES) nil
	  (aref keycodes #_VK_BROWSER_HOME) nil
	  (aref keycodes #_VK_VOLUME_MUTE) nil
	  (aref keycodes #_VK_VOLUME_DOWN) nil
	  (aref keycodes #_VK_VOLUME_UP) nil
	  (aref keycodes #_VK_MEDIA_NEXT_TRACK) nil
	  (aref keycodes #_VK_MEDIA_PREV_TRACK) nil
	  (aref keycodes #_VK_MEDIA_STOP) nil
	  (aref keycodes #_VK_MEDIA_PLAY_PAUSE) nil
	  (aref keycodes #_VK_LAUNCH_MAIL) nil
	  (aref keycodes #_VK_LAUNCH_MEDIA_SELECT) nil
	  (aref keycodes #_VK_LAUNCH_APP1) nil
	  (aref keycodes #_VK_LAUNCH_APP2) nil
	
	  (aref keycodes #_VK_OEM_1) nil
	  (aref keycodes #_VK_OEM_PLUS) nil
	  (aref keycodes #_VK_OEM_MINUS) nil
	  (aref keycodes #_VK_OEM_PERIOD) nil
	  (aref keycodes #_VK_OEM_2) nil
	  (aref keycodes #_VK_OEM_3) nil
	
	  (aref keycodes #_VK_OEM_4) nil
	  (aref keycodes #_VK_OEM_5) nil
	  (aref keycodes #_VK_OEM_6) nil 
	  (aref keycodes #_VK_OEM_7) nil
	  (aref keycodes #_VK_OEM_8) nil

	  (aref keycodes #_VK_OEM_102) nil
	  (aref keycodes #_VK_PROCESSKEY) nil
	  (aref keycodes #_VK_PACKET) nil

	  (aref keycodes #_VK_ATTN) +key-sys-req+
	  (aref keycodes #_VK_CRSEL) +key-cr-sel-props+
	  (aref keycodes #_VK_EXSEL) +key-ex-sel+
	  (aref keycodes #_VK_EREOF) nil
	  (aref keycodes #_VK_PLAY) nil
	  (aref keycodes #_VK_ZOOM) nil
	  (aref keycodes #_VK_NONAME) nil
	  (aref keycodes #_VK_PA1) nil
	  (aref keycodes #_VK_OEM_CLEAR) nil)


    (loop for scancode from 0 below 256
	  when (aref keycodes scancode)
	    do (setf (aref scancodes (aref keycodes scancode)) scancode))
    
    (values)))


