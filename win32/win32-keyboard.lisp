(in-package :clui)


#+NIL
(defun create-win32-key-tables (win32-desktop)
  (let ((keycodes (display-keycodes win32-desktop))
	(scancodes (display-scancodes win32-desktop)))
    
    (setf 
	  
	  (aref keycodes #_VK_LBUTTON) nil
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

	  (aref keycodes #_VK_LSHIFT) +key-left-shift+
	  (aref keycodes #_VK_RSHIFT) +key-right-shift+	  
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
	
	  (aref keycodes #_VK_OEM_1) +key-semicolon+
	  (aref keycodes #_VK_OEM_PLUS) +key-equals+
	  (aref keycodes #_VK_OEM_MINUS) +key-minus+
	  (aref keycodes #_VK_OEM_PERIOD) +key-period+
	  (aref keycodes #_VK_OEM_2) +key-slash+
	  (aref keycodes #_VK_OEM_3) +key-grave-accent+
	
	  (aref keycodes #_VK_OEM_4) +key-left-bracket+
	  (aref keycodes #_VK_OEM_5) +key-backslash+
	  (aref keycodes #_VK_OEM_6) +key-right-bracket+
	  (aref keycodes #_VK_OEM_7) +key-apostrophe+
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
	  (aref keycodes #_VK_OEM_CLEAR) +key-clear+)


    (loop for scancode from 0 below 256
	  when (aref keycodes scancode)
	    do (setf (aref scancodes (aref keycodes scancode)) scancode))
    
    (values)))


(defun create-win32-key-tables (win32-desktop)
  (let ((keycodes (display-keycodes win32-desktop))
	(scancodes (display-scancodes win32-desktop)))

    (setf (aref keycodes #x00B) +key-0+
	  (aref keycodes #x002) +key-1+
	  (aref keycodes #x003) +key-2+
	  (aref keycodes #x004) +key-3+
	  (aref keycodes #x005) +key-4+
	  (aref keycodes #x006) +key-5+
	  (aref keycodes #x007) +key-6+
	  (aref keycodes #x008) +key-7+
	  (aref keycodes #x009) +key-8+
	  (aref keycodes #x00A) +key-9+
	  (aref keycodes #x01E) +key-A+
	  (aref keycodes #x030) +key-B+
	  (aref keycodes #x02E) +key-C+
	  (aref keycodes #x020) +key-D+
	  (aref keycodes #x012) +key-E+
	  (aref keycodes #x021) +key-F+
	  (aref keycodes #x022) +key-G+
	  (aref keycodes #x023) +key-H+
	  (aref keycodes #x017) +key-I+
	  (aref keycodes #x024) +key-J+
	  (aref keycodes #x025) +key-K+
	  (aref keycodes #x026) +key-L+
	  (aref keycodes #x032) +key-M+
	  (aref keycodes #x031) +key-N+
	  (aref keycodes #x018) +key-O+
	  (aref keycodes #x019) +key-P+
	  (aref keycodes #x010) +key-Q+
	  (aref keycodes #x013) +key-R+
	  (aref keycodes #x01F) +key-S+
	  (aref keycodes #x014) +key-T+
	  (aref keycodes #x016) +key-U+
	  (aref keycodes #x02F) +key-V+
	  (aref keycodes #x011) +key-W+
	  (aref keycodes #x02D) +key-X+
	  (aref keycodes #x015) +key-Y+
	  (aref keycodes #x02C) +key-Z+

	  (aref keycodes #x028) +key-apostrophe+
	  (aref keycodes #x02B) +key-backslash+
	  (aref keycodes #x033) +key-comma+
	  (aref keycodes #x00D) +key-equals+
	  (aref keycodes #x029) +key-grave-accent+
	  (aref keycodes #x01A) +key-left-bracket+
	  (aref keycodes #x00C) +key-minus+
	  (aref keycodes #x034) +key-period+
	  (aref keycodes #x01B) +key-right-bracket+
	  (aref keycodes #x027) +key-semicolon+
	  (aref keycodes #x035) +key-slash+
	  (aref keycodes #x056) +key-international-2+

	  (aref keycodes #x00E) +key-backspace+
	  (aref keycodes #x153) +key-delete+
	  (aref keycodes #x14F) +key-end+
	  (aref keycodes #x01C) +key-enter+
	  (aref keycodes #x001) +key-escape+
	  (aref keycodes #x147) +key-home+
	  (aref keycodes #x152) +key-insert+
	  (aref keycodes #x15D) +key-menu+
	  (aref keycodes #x151) +key-page-down+
	  (aref keycodes #x149) +key-page-up+
	  (aref keycodes #x045) +key-pause+
	  (aref keycodes #x039) +key-spacebar+
	  (aref keycodes #x00F) +key-tab+
	  (aref keycodes #x03A) +key-caps-lock+
	  (aref keycodes #x145) +key-num-lock+
	  (aref keycodes #x046) +key-scroll-lock+
	  (aref keycodes #x03B) +key-F1+
	  (aref keycodes #x03C) +key-F2+
	  (aref keycodes #x03D) +key-F3+
	  (aref keycodes #x03E) +key-F4+
	  (aref keycodes #x03F) +key-F5+
	  (aref keycodes #x040) +key-F6+
	  (aref keycodes #x041) +key-F7+
	  (aref keycodes #x042) +key-F8+
	  (aref keycodes #x043) +key-F9+
	  (aref keycodes #x044) +key-F10+
	  (aref keycodes #x057) +key-F11+
	  (aref keycodes #x058) +key-F12+
	  (aref keycodes #x064) +key-F13+
	  (aref keycodes #x065) +key-F14+
	  (aref keycodes #x066) +key-F15+
	  (aref keycodes #x067) +key-F16+
	  (aref keycodes #x068) +key-F17+
	  (aref keycodes #x069) +key-F18+
	  (aref keycodes #x06A) +key-F19+
	  (aref keycodes #x06B) +key-F20+
	  (aref keycodes #x06C) +key-F21+
	  (aref keycodes #x06D) +key-F22+
	  (aref keycodes #x06E) +key-F23+
	  (aref keycodes #x076) +key-F24+
	  (aref keycodes #x038) +key-left-alt+
	  (aref keycodes #x01D) +key-left-ctrl+
	  (aref keycodes #x02A) +key-left-shift+
	  (aref keycodes #x15B) +key-left-GUI+
	  (aref keycodes #x137) +key-print-screen+
	  (aref keycodes #x138) +key-right-alt+
	  (aref keycodes #x11D) +key-right-ctrl+
	  (aref keycodes #x036) +key-right-shift+
	  (aref keycodes #x15C) +key-right-GUI+
	  (aref keycodes #x150) +key-down-arrow+
	  (aref keycodes #x14B) +key-left-arrow+
	  (aref keycodes #x14D) +key-right-arrow+
	  (aref keycodes #x148) +key-up-arrow+

	  (aref keycodes #x052) +key-pad-0+
	  (aref keycodes #x04F) +key-pad-1+
	  (aref keycodes #x050) +key-pad-2+
	  (aref keycodes #x051) +key-pad-3+
	  (aref keycodes #x04B) +key-pad-4+
	  (aref keycodes #x04C) +key-pad-5+
	  (aref keycodes #x04D) +key-pad-6+
	  (aref keycodes #x047) +key-pad-7+
	  (aref keycodes #x048) +key-pad-8+
	  (aref keycodes #x049) +key-pad-9+

	  (aref keycodes #x04E) +key-pad-plus+
	  (aref keycodes #x053) +key-pad-decimal+
	  (aref keycodes #x135) +key-pad-slash+
	  (aref keycodes #x11C) +key-pad-enter+
	  (aref keycodes #x059) +key-pad-equals+
	  (aref keycodes #x037) +key-pad-asterisk+
	  (aref keycodes #x04A) +key-pad-minus+)

    (loop for scancode from 0 below 256
	  when (aref keycodes scancode)
	    do (setf (aref scancodes (aref keycodes scancode)) scancode))

    (values)))
		
