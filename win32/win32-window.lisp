(in-package :clui)
(noffi::noffi-syntax t)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (noffi::noffi-syntax t))

(defvar w)

(defcfun (my-exception-handler-callback #_<LONG>)
	 ((ExceptionInfo #_<EXCEPTION_POINTERS*>))
  (my-exception-handler ExceptionInfo))

(defun my-exception-handler (exception-info)
  (let ((exception-record (#_.ExceptionRecord exception-info))
	(context-record (#_.ContextRecord exception-info)))
    (declare (ignorable context-record))
    (let ((exception-code (#_.ExceptionCode exception-record))
	  (exception-flags (#_.ExceptionFlags exception-record)))
      (break "Win32 Exception, code: 0x~X, flags: 0x~X" exception-code exception-flags)
      0)))

(defun add-my-exception-handler ()
  (#_AddVectoredExceptionHandler 0 my-exception-handler-callback))

(add-my-exception-handler)

(defun noffi-ptr->ccl-ptr (ptr)
  (ccl::%inc-ptr (ptr-value ptr) (ptr-offset ptr)))


(defconstant +WM_COPYGLOBALDATA+ #x0049)




(defmacro with-lpcwstr ((var string) &body body)
  (let ((sap-sym (gensym)))
    `(ccl::with-native-utf-16-cstr (,sap-sym ,string)
       (let ((,var (%cons-ptr ,sap-sym 0 '#_<LPCWSTR>)))
	 ,@body))))

(defun lpcwstr (string)
  (let* ((len (length string)))
    (multiple-value-bind (data offset) (ccl::array-data-and-offset string)
      (let* ((end (+ offset len))
	     (noctets (ccl::utf-16-octets-in-string data offset end))
	     (octets (ccl::malloc (1+ noctets))))
	(ccl::native-utf-16-memory-encode data octets 0 offset end)
	(setf (ccl::%get-unsigned-word octets noctets) 0)
	(%cons-ptr octets 0 '#_<LPCWSTR>)))))

(defun lpcwstr->string (ptr)
  (ccl::%get-native-utf-16-cstring (noffi-ptr->ccl-ptr ptr)))


					     
			      
			    
 
	    

(defun apply-aspect-ratio (window edge area)
  (clet ((frame #_<RECT>))
    (let ((&frame (c-addr-of frame)))
      (setf (#_.left &frame) 0
	    (#_.top &frame) 0
	    (#_.right &frame) 0
	    (#_.bottom &frame) 0)
      (let ((ratio (coerce (/ (window-aspect-numer window) (window-aspect-denom window)) 'single-float))
	    (style (get-window-style window))
	    (ex-style (get-window-ex-style window)))

	(if (windows-10-version-1607-or-greater?)

	    (#_AdjustWindowRectExForDpi &frame style #_FALSE ex-style (#_GetDpiForWindow (h window)))

	    (#_AdjustWindowRectEx &frame style #_FALSE ex-style))

	(cond ((or (= edge #_WMSZ_LEFT)
		   (= edge #_WMSZ_BOTTOMLEFT)
		   (= edge #_WMSZ_RIGHT)
		   (= edge #_WMSZ_BOTTOMRIGHT))

	       (setf (#_.bottom area) (+ (#_.top area) (- (#_.bottom &frame) (#_.top &frame))
					 (round (/ (- (- (#_.right area) (#_.left area))
						      (- (#_.right &frame) (#_.left &frame)))
						   ratio)))))
	      
	      ((or (= edge #_WMSZ_TOPLEFT)
		   (= edge #_WMSZ_TOPRIGHT))

	       (setf (#_.top area) (- (#_.bottom area) (- (#_.bottom &frame) (#_.top &frame))
				      (round (/ (- (- (#_.right area) (#_.left area))
						   (- (#_.right &frame) (#_.left &frame)))
						ratio)))))

	      ((or (= edge #_WMSZ_TOP)
		   (= edge #_WMSZ_BOTTOM))

	       (setf (#_.right area) (+ (#_.left area) (- (#_.right &frame) (#_.left &frame))
					(round (* (- (- (#_.bottom area) (#_.top area))
						     (- (#_.bottom &frame) (#_.top &frame)))
						  ratio))))))
	(values)))))

(defun update-win32-cursor-image (window)
  (if (or (eq (window-cursor-mode window) :normal)
	  (eq (window-cursor-mode window) :captured))
      (if (window-cursor window)
	  (#_SetCursor (h (window-cursor window)))
	  (#_SetCursor (#_LoadCursor nil #_IDC_ARROW)))
      (#_SetCursor nil))
  (values))

(defun release-win32-cursor (app)
  (#_ClipCursor nil)
  (setf (captured-cursor-window app) nil)
  (values))

(defun capture-win32-cursor (window)
  (declare (type os-window-mixin window))
  (clet ((clip-rect #_<RECT>))
    (let ((&clip-rect (c-addr-of clip-rect)))
      (#_GetClientRect (h window) &clip-rect)
      (let ((left (c->-addr &clip-rect '#_left))
	    (right (c->-addr &clip-rect '#_right)))
	(#_ClientToScreen (h window) left)
	(#_ClientToScreen (h window) right))
      (#_ClipCursor &clip-rect)
      (setf (captured-cursor-window (window-display window)) window)
      (values))))

(defun get-win32-window-fullscreen (window)
  (declare (ignorable window))
  (values))

(defun set-win32-window-fullscreen (window)
  (declare (ignorable window))
  (values))

(defun get-win32-window-closable (window)
  (declare (ignorable window))
  (values))

(defun set-win32-window-closable (window value)
  (declare (ignorable window value))
  (values))

(defun get-win32-window-title (window)
  (declare (ignorable window))
  (values))

(defun set-win32-window-title (window value)
  (let ((wide (lpcwstr value)))
    (#_SetWindowTextW (h window) wide))
  (values))

(defun get-win32-window-titled (window)
  (declare (ignorable window))
  (values))

(defun set-win32-window-titled (window value)
  (declare (ignorable window value))
  (values))

(defun get-win32-window-pos (window)
  (clet ((pos #_<POINT>))
    (let ((&pos (c-addr-of pos)))
      (setf (#_.x &pos) 0
	    (#_.y &pos) 0)
      (#_ClientToScreen (h window) &pos)
      (values (#_.x &pos) (#_.y &pos)))))

(defun set-win32-window-pos (window xpos ypos)
  (clet ((rect #_<RECT>))
    (let ((&rect (c-addr-of rect)))
      (setf (#_.left &rect) xpos
	    (#_.top &rect) ypos
	    (#_.right &rect) xpos
	    (#_.bottom &rect) ypos)
      
    (if (windows-10-version-1607-or-greater?)
	
	(#_AdjustWindowRectExForDpi &rect
				    (get-window-style window)
				    #_FALSE
				    (get-window-ex-style window)
				    (#_GetDpiForWindow (h window)))
	
	(#_AdjustWindowRectEx &rect
			      (get-window-style window)
			      #_FALSE
			      (get-window-ex-style window)))

      (#_SetWindowPos (h window) 0
		    (#_.left &rect)
		    (#_.top &rect)
		    0 0
		    (logior #_SWP_NOACTIVATE #_SWP_NOZORDER #_SWP_NOSIZE))))
    (values))

(defun get-win32-window-size (window)
  (clet ((area #_<RECT>))
    (let ((&area (c-addr-of area)))
    (setf (#_.left &area) 0
	  (#_.top &area) 0
	  (#_.right &area) 0
	  (#_.top &area) 0)    
    (when (#_GetClientRect (h window) &area)
      (values (#_.right &area)
	      (#_.bottom &area))))))

(defun set-win32-window-size (window width height)
  (declare (ignorable window width height))
  (clet ((rect #_<RECT>))
    (let ((&rect (c-addr-of rect)))
      
      (setf (#_.left &rect) 0
	    (#_.top &rect) 0
	    (#_.right &rect) width
	    (#_.bottom &rect) height)
    
    (if (windows-10-version-1607-or-greater?)
	
	(#_AdjustWindowRectExForDpi &rect
				    (get-window-style window)
				    0
				    (get-window-ex-style window)
				    (#_GetDpiForWindow (h window)))
	
	(#_AdjustWindowRectEx &rect
			      (get-window-style window)
			      0
			      (get-window-ex-style window)))
    
    (#_SetWindowPos (h window) 0
		    0 0
		    (- (#_.right &rect) (#_.left &rect))
		    (- (#_.bottom &rect) (#_.top &rect))
		    (logior #_SWP_NOACTIVATE #_SWP_NOOWNERZORDER #_SWP_NOMOVE #_SWP_NOZORDER))))
  (values))

(defun get-win32-window-cursor-pos (window)
  (clet& ((&pos #_<POINT>))
    (if (zerop (#_GetCursorPos &pos))
	(values 0 0)
	(progn (#_ScreenToClient (h window) &pos)
	       (values (#_.x &pos) (#_.y &pos))))))

(defun set-win32-window-cursor-pos (window x y)
  (declare (ignorable window x y))
  (values))

(defun get-win32-window-maximized (window)
  (if (= 0 (#_IsZoomed (h window)))
      nil
      t))

(defun set-win32-window-maximized (window)
  (declare (ignorable window))
  (values))

(defun maximize-win32-window (window)
  (declare (ignorable window))
  (values))

(defun restore-win32-window (window)
  (declare (ignorable window))
  (values))

(defun show-win32-window (window)
  (declare (type os-window-mixin window))
  (#_ShowWindow (h window) #_SW_SHOW)
  (values))

(defun hide-win32-window (window)
  (declare (type os-window-mixin window))
  (#_ShowWindow (h window) #_SW_HIDE)
  (values))

(defun get-win32-window-shown (window)
  (declare (ignorable window))
  (values))

(defun set-win32-window-shown (window value)
  (declare (ignorable window value))
  (values))

(defun get-win32-window-hidden (window)
  (declare (ignorable window))
  (values))

(defun set-win32-window-hidden (window value)
  (declare (ignorable window value))
  (values))

(defun get-win32-window-focused (window)
  (= (sap-int (noffi-ptr->ccl-ptr (h window)))
     (sap-int (noffi-ptr->ccl-ptr (#_GetActiveWindow)))))

(defun set-win32-window-focused (window value)
  (declare (ignorable window value))
  (values))

(defun focus-win32-window (window)
  (declare (type os-window-mixin window))
  (let ((handle (h window)))
    (#_BringWindowToTop handle)
    (#_SetForegroundWindow handle)
    (#_SetFocus handle))
  (values))

(defun unfocus-win32-window (window)
  (declare (ignorable window))
  (values))

(defun get-win32-window-iconified (window)
  (if (= 0 (#_IsIconic (h window)))
      nil
      t))

(defun set-win32-window-iconified (window value)
  (declare (ignorable window value))
  (values))

(defun iconify-win32-window (window)
  (declare (ignorable window))
  (values))

(defun deiconify-win32-window (window)
  (declare (ignorable window))
  (values))

(defun get-win32-window-visible (window)
  (if (= 0 (#_IsWindowVisible (h window)))
      nil
      t))

(defun set-win32-window-visible (window value)
  (declare (ignorable window value))
  (values))

(defun make-win32-window-visible (window)
  (declare (ignorable window))
  (values))

(defun make-win32-window-invisible (window)
  (declare (ignorable window))
  (values))

(defun get-win32-window-hovered (window)
  (block nil
    (clet ((area #_<RECT>)
	   (pos #_<POINT>))
      (let ((&area (c-addr-of area))
	    (&pos (c-addr-of pos)))
	
	(when (= 0 (#_GetCursorPos &pos))
	  (return nil))

	(unless (= (sap-int (noffi-ptr->ccl-ptr (#_WindowFromPoint pos)))
		   (sap-int (noffi-ptr->ccl-ptr (h window))))
	  (return nil))

	(#_GetClientRect (h window) &area)
	(#_ClientToScreen (h window) (#_.left &area))
	(#_ClientToScreen (h window) (#_.right &area))

	(if (= 0 (#_PtInRect &area pos))
	    nil
	    t)))))

(defun get-win32-window-resizable (window)
  (declare (ignorable window))
  (values))

(defun set-win32-window-resizable (window value)
  (declare (ignorable window value))
  (values))

(defun make-win32-window-resizable (window)
  (declare (ignorable window))
  (values))

(defun make-win32-window-non-resizable (window)
  (declare (ignorable window))
  (values))

(defun get-win32-window-decorated (window)
  (declare (ignorable window))
  (values))

(defun set-win32-window-decorated (window value)
  (declare (ignorable window value))
  (values))

(defun get-win32-window-floating (window)
  (declare (ignorable window))
  (values))

(defun set-win32-window-floating (window value)
  (declare (ignorable window value))
  (values))

(defun get-win32-window-opaque (window)
  (declare (ignorable window))
  (values))

(defun set-win32-window-opaque (window value)
  (declare (ignorable window value))
  (values))

(defun get-win32-window-opacity (window)
  (declare (ignorable window))
  (values))

(defun set-win32-window-opacity (window alpha)
  (declare (ignorable window alpha))
  (values))

(defun set-win32-window-size-limits (window min-width min-height max-width max-height)
  (declare (ignorable window min-width min-height max-width max-height))
  (values))

(defun get-win32-window-aspect-ratio (window)
  (declare (ignorable window))
  (values))

(defun get-win32-window-framebuffer-size (window)
  (get-win32-window-size window))

(defun get-win32-window-frame-size (window)
  (declare (ignorable window))
  (values))

(defun get-win32-window-content-scale (window)
  (let ((mh (#_MonitorFromWindow (h window) #_MONITOR_DEFAULTTONEAREST)))
    (get-hmonitor-content-scale mh)))
		      
			       

(defun request-win32-window-attention (window)
  (declare (ignorable window))
  (values))


(defun destroy-win32-window (window)
  (when (window-monitor window)
    (release-win32-monitor window (window-monitor window)))
  (when (eq (disabled-cursor-window (window-display window)) window)
    (enable-cursor window))
  (when (eq (captured-cursor-window (window-display window)) window)
    (release-cursor window))
  (when (h window)
    (with-lpcwstr (string "CLUI")
      (#_RemovePropW (h window) string))
    (#_DestroyWindow (h window))
    (setf (h window) nil))
  #+NOTYETFIXME
  (when (big-icon window)
    (#_DestroyIcon (big-icon window)))
  #+NOYETFIXME
  (when (small-icon window)
    (#_DestroyIcon (small-icon window)))
  (values))


(defvar *window-count* 0)

(defun gen-window-id ()
  (incf *window-count*))

(defvar *id->window-table* (make-hash-table :test #'eq))


(defun get-window-style (win32-window)
  (declare (type os-window-mixin win32-window))
  (let ((style (logior #_WS_CLIPSIBLINGS #_WS_CLIPCHILDREN #_WS_VISIBLE)))
    
    (if (window-monitor win32-window)
	(setq style (logior style #_WS_POPUP))
	(progn
	  (setq style (logior style #_WS_SYSMENU #_WS_MINIMIZEBOX))
	  (if (last-decorated? win32-window)
	      (progn
		(setq style (logior style #_WS_CAPTION))
		(when (last-resizable? win32-window)
		  (setq style (logior style #_WS_MAXIMIZEBOX #_WS_THICKFRAME))))
	      (setq style (logior style #_WS_POPUP)))))
    style))


(defun get-window-ex-style (window)
  (let ((style #_WS_EX_APPWINDOW))

    (when (or (window-monitor window) (last-floating? window))
      (setq style (logior style #_WS_POPUP)))

    style))
      

(defun windows-version-or-greater? (major minor sp)
  (clet ((osvi #_<OSVERSIONINFOEXW>))
    (let ((posvi (c-addr-of osvi)))
      ;;(clet ((csd-version #_<WCHAR[1]>))
      (setf (#_.dwOSVersionInfoSize posvi) (c-sizeof-type '#_<OSVERSIONINFOEXW>)
	    (#_.dwMajorVersion posvi) major
	    (#_.dwMinorVersion posvi) minor
	    (#_.dwBuildNumber posvi) 0
	    (#_.dwPlatformId posvi) 0
	    ;;(#_.szCSDVersion posvi) csd-version
	    (#_.wServicePackMajor posvi) sp
	    (#_.wServicePackMinor posvi) 0
	    (#_.wSuiteMask posvi) 0
	    (#_.wProductType posvi) 0
	    (#_.wReserved posvi) 0)
      (let ((mask (logior #_VER_MAJORVERSION #_VER_MINORVERSION #_VER_SERVICEPACKMAJOR))
	    (condition (#_VerSetConditionMask 0 #_VER_MAJORVERSION #_VER_GREATER_EQUAL)))
	(setq condition (#_VerSetConditionMask 0 #_VER_MINORVERSION #_VER_GREATER_EQUAL))
	(setq condition (#_VerSetConditionMask 0 #_VER_SERVICEPACKMAJOR #_VER_GREATER_EQUAL))
	(not (zerop
	      (ccl::%ff-call (ccl::%reference-external-entry-point (ccl:external "RtlVerifyVersionInfo"))
			     :address (noffi-ptr->ccl-ptr posvi)
			     :unsigned-fullword mask :unsigned-doubleword condition
			     :unsigned-fullword)))))))


(defun windows-7-or-greater? ()
  (windows-version-or-greater? (#_HIBYTE #__WIN32_WINNT_WIN7) (#_LOBYTE #__WIN32_WINNT_WIN7) 0))


(defun windows-8.1-or-greater? ()
  #+NIL(windows-version-or-greater? (#_HIBYTE #__WIN32_WINNT_WIN81) (#_LOBYTE #__WIN32_WINNT_WIN81) 0)
  t)

(defun windows-11-or-greater? ()
  (windows-version-or-greater? (#_HIBYTE #__WIN32_WINNT_WIN11) (#_LOBYTE #__WIN32_WINNT_WIN11) 0))


(defun windows-10-build-or-later? (build)
  (clet ((osvi #_<OSVERSIONINFOEXW>))
    (let ((posvi (c-addr-of osvi)))
      (let ((pszCSDVersion (c->-addr posvi '#_szCSDVersion)))
	;;(clet ((foo #_<WCHAR*>))
	  ;;(setf (noffi::c-aref pszCSDVersion 0) foo))

	(setf (#_.dwOSVersionInfoSize posvi) (c-sizeof-type '#_<OSVERSIONINFOEXW>)
	      (#_.dwMajorVersion posvi) 10
	      (#_.dwMinorVersion posvi) 0
	      (#_.dwBuildNumber posvi) build
	      (#_.dwPlatformId posvi) 0
	      (#_.wServicePackMajor posvi) 0
	      (#_.wServicePackMinor posvi) 0
	      (#_.wSuiteMask posvi) 0
	      (#_.wProductType posvi) 0
	      (#_.wReserved posvi) 0)    
	(let ((mask (logior #_VER_MAJORVERSION #_VER_MINORVERSION #_VER_SERVICEPACKMAJOR))
	      (condition (#_VerSetConditionMask 0 #_VER_MAJORVERSION #_VER_GREATER_EQUAL)))
	  (setq condition (#_VerSetConditionMask 0 #_VER_MINORVERSION #_VER_GREATER_EQUAL))
	  (setq condition (#_VerSetConditionMask 0 #_VER_SERVICEPACKMAJOR #_VER_GREATER_EQUAL))
	  (not (zerop
		(ccl::%ff-call (ccl::%reference-external-entry-point (ccl:external "RtlVerifyVersionInfo"))
			       :address (noffi-ptr->ccl-ptr posvi)
			       :unsigned-fullword mask :unsigned-doubleword condition
			       :unsigned-fullword))))))))

(defun windows-11-build-or-later? (build)
  (clet ((osvi #_<OSVERSIONINFOEXW>))
    (let ((posvi (c-addr-of osvi)))
      (let ((pszCSDVersion (c->-addr posvi '#_szCSDVersion)))
	;;(clet ((foo #_<WCHAR*> #_NULL))
	  ;;(setf (noffi::c-aref pszCSDVersion 0) foo))

	(setf (#_.dwOSVersionInfoSize posvi) (c-sizeof-type '#_<OSVERSIONINFOEXW>)
	      (#_.dwMajorVersion posvi) 11
	      (#_.dwMinorVersion posvi) 0
	      (#_.dwBuildNumber posvi) build
	      (#_.dwPlatformId posvi) 0
	      (#_.wServicePackMajor posvi) 0
	      (#_.wServicePackMinor posvi) 0
	      (#_.wSuiteMask posvi) 0
	      (#_.wProductType posvi) 0
	      (#_.wReserved posvi) 0)    
	(let ((mask (logior #_VER_MAJORVERSION #_VER_MINORVERSION #_VER_SERVICEPACKMAJOR))
	      (condition (#_VerSetConditionMask 0 #_VER_MAJORVERSION #_VER_GREATER_EQUAL)))
	  (setq condition (#_VerSetConditionMask 0 #_VER_MINORVERSION #_VER_GREATER_EQUAL))
	  (setq condition (#_VerSetConditionMask 0 #_VER_SERVICEPACKMAJOR #_VER_GREATER_EQUAL))
	  (not (zerop
		(ccl::%ff-call (ccl::%reference-external-entry-point (ccl:external "RtlVerifyVersionInfo"))
			       :address (noffi-ptr->ccl-ptr posvi)
			       :unsigned-fullword mask :unsigned-doubleword condition
			       :unsigned-fullword))))))))


(defun windows-10-version-1607-or-greater? ()
  (prog1 (or (windows-11-build-or-later? 0)
	     (windows-10-build-or-later? 14393))
    (print 'dd)
    (print (#_GetLastError))
    (finish-output)))


(defun windows-10-version-1703-or-greater? ()
  (or (windows-11-build-or-later? 0)
      (windows-10-build-or-later? 15063)))



(defun get-dpi-for-monitor (hmonitor)
  (clet ((xdpi #_<UINT>)
	 (ydpi #_<UINT>))
    
    (if (zerop (ccl::%ff-call (ccl::%reference-external-entry-point
				      (ccl:external "GetDpiForMonitor"))
			      :address (noffi-ptr->ccl-ptr hmonitor)
			      :unsigned-fullword 0 ;;#_MDT_EFFECTIVE_DPI
			      :address (noffi-ptr->ccl-ptr (c-addr-of xdpi))
			      :address (noffi-ptr->ccl-ptr (c-addr-of ydpi))
			     :signed-fullword))
	(values (cval-value xdpi) (cval-value ydpi))
	nil)))
      

(defun get-hmonitor-content-scale (handle)
  (if (windows-8.1-or-greater?)
      (multiple-value-bind (xdpi ydpi) (get-dpi-for-monitor handle)
	(unless xdpi
	  (error "Win32: Failed to query monitor DPI."))
	(values (/ xdpi #_USER_DEFAULT_SCREEN_DPI)
		(/ ydpi #_USER_DEFAULT_SCREEN_DPI)))
      (let* ((dc (#_GetDC nil)))
	(unwind-protect
	     (clet ((xdpi #_<UINT> (#_GetDeviceCaps dc #_LOGPIXELSX))
		    (ydpi #_<UINT> (#_GetDeviceCaps dc #_LOGPIXELSY)))
	       (values (/ xdpi #_USER_DEFAULT_SCREEN_DPI)
		       (/ ydpi #_USER_DEFAULT_SCREEN_DPI)))
	  (#_ReleaseDC nil dc)))))












(eval-when (:compile-toplevel)
#_{
typedef struct wndconfig {
int id;
int maximized;
int scaletomonitor;
int decorated;
} wndconfig;
}
)

(defun default-paint-win32-window (window)
  (clet ((ps #_<PAINTSTRUCT>))
    (let* ((&ps (c-addr-of ps))
	   (rc-paint (c->-addr &ps '#_rcPaint))
	   (hdc (#_BeginPaint (h window) &ps)))

      (#_FillRect hdc rc-paint (noffi::cons-ptr (ccl::%int-to-ptr (1+ #_COLOR_WINDOW)) 0 '#_<HBRUSH>))

      (#_EndPaint (h window) &ps))))
      

(defun get-window-prop (hWnd)
  (with-lpcwstr (str "CLUI")
    (ccl::%ff-call (ccl::%reference-external-entry-point (ccl:external "GetPropW"))
		   :address (noffi-ptr->ccl-ptr hWnd) :address (noffi-ptr->ccl-ptr str)
		   :unsigned-doubleword)))

(defun set-window-prop (hWnd id)
  (with-lpcwstr (str "CLUI")
    (ccl::%ff-call
     (ccl::%reference-external-entry-point (ccl:external "SetPropW"))
     :address (noffi-ptr->ccl-ptr hWnd) :address (noffi-ptr->ccl-ptr str)
     :unsigned-doubleword id
     :void)))
  


(defun window-proc (hWnd uMsg wParam lParam)
  ;;(format t "~&window-proc running")

  (block nil
    
    (when (= uMsg #_WM_NCCREATE) ;; 129
	
      (when (windows-10-version-1607-or-greater?)
	  
	(let* ((cs (cons-ptr (ccl::%int-to-ptr lParam) 0 '#_<CREATESTRUCTW*>))
	       (wndconfig (c-cast '#_<wndconfig*> (#_.lpCreateParams cs)))
	       (id (#_.id wndconfig))
	       (scale-to-monitor (#_.scaletomonitor wndconfig)))
	  
	  (set-window-prop hWnd id)
	    
	  (unless (= 0 scale-to-monitor)
	    (#_EnableNonClientDpiScaling hWnd))))
	
      (return (#_DefWindowProcW hWnd uMsg wParam lParam)))


    (let ((id (get-window-prop hWnd))
	  (window))

      (when (= id 0)
	(warn "failed to get window id for msg: ~S" uMsg)
	(return (#_DefWindowProcW hWnd uMsg wParam lParam)))
	
      (setq window (gethash id *id->window-table*))
      
      (tagbody
	 (locally (declare (type os-window-mixin window))

	   (with-slots (frame-action? cursor-mode high-surrogate
			monitor auto-iconify? key-menu)
	       window

	     (case uMsg

	       (#.#_WM_NCCALCSIZE (go break)) ;; 131

	       (#.#_WM_CREATE
		(setf (h window) hWnd)
		(setf (gethash
		       (sap-int (ccl::%inc-ptr (ptr-value hWnd) (ptr-offset hWnd)))
		       *window-handle->window-table*)
		      window)
		(go break)) ; 1

	       (#.#_WM_SHOWWINDOW (go break)) ;; 24

	       (#.#_WM_WINDOWPOSCHANGING ;; 70
		(go break))

	       (#.#_WM_ACTIVATEAPP (go break)) ;; 28

	       (#.#_WM_NCHITTEST ;; 132
		(go break))

	       ((#.#_WM_NCACTIVATE #.#_WM_NCPAINT) ;; 133 and 134
		(unless (last-decorated? window)
		  (return #_TRUE))
		(go break))

	       (#.#_WM_GETICON (go break)) ;; 127

	       (#.#_WM_ACTIVATE ;; 6
		(go break))

	       (#.#_WM_PAINT ;; 15

		(handle-event window (make-instance 'window-repaint-event))

		(default-paint-win32-window window)

		(return 0)

		#+NIL
		(go break))

	       (#.#_WM_IME_SETCONTEXT (go break)) ;; 641

	       (#.#_WM_IME_NOTIFY (go break)) ;; 642

	       (#.#_WM_SETFOCUS ;; 7

		(handle-event window (make-instance 'window-focus-event))

		(when frame-action?
		  (go break))

		(if (eq cursor-mode :disabled)
		    (disable-cursor window)
		    (when (eq cursor-mode :captured)
		      (capture-win32-cursor window)))

		(return 0))

	       (#.#_WM_ERASEBKGND (return #_TRUE)) ;; 20

	       (#.#_WM_WINDOWPOSCHANGED (go break)) ;; 71

	       (#.#_WM_SIZE ;; 5

		(let ((width (#_LOWORD lParam))
		      (height (#_HIWORD lParam))
		      (iconified? (= wParam #_SIZE_MINIMIZED))
		      (maximized? (or (= wParam #_SIZE_MAXIMIZED)
				      (and (last-maximized? window)
					   (/= wParam #_SIZE_RESTORED)))))

		  (when (eq (captured-cursor-window (window-display window)) window)
		    (capture-win32-cursor window))

		  (unless (eq (last-iconified? window) iconified?)
		    (let ((event (make-instance 'window-iconify-event)))
		      (handle-event window event)))

		  (unless (eq (last-maximized? window) maximized?)
		    (let ((event (make-instance (if maximized?
						    'window-maximize-event
						    'window-restore-event))))
		      (handle-event window event)))

		  (when (or (/= width (round (or (last-width window) 0)))
			    (/= height (round (or (last-height window) 0))))

		    (let ((event (make-instance 'window-resize-event
						:window window
						:new-width width
						:new-height height)))
		      (handle-event window event)

		      (setf (last-width window) width
			    (last-height window) height)))

		  (when (and (window-monitor window)
			     (not (eq (last-iconified? window) iconified?)))
		    (if iconified?
			(release-win32-monitor window (window-monitor window))
			(progn
			  (acquire-win32-monitor window (window-monitor window))
			  (fit-to-monitor window))))

		  (setf (last-iconified? window) iconified?)
		  (setf (last-maximized? window) maximized?)

		  (terpri)
		  (princ 'success)
		  (finish-output)

		  (return 0)))

	       (#.#_WM_MOVE ;; 3

		(when (eq (captured-cursor-window (window-display window)) window)
		  (capture-win32-cursor window))

		(let ((event (make-instance 'window-move-event
					    :new-x (#_GET_X_LPARAM lParam)
					    :new-y (#_GET_Y_LPARAM lParam))))

		  (handle-event window event)

		  (setf (last-pos-x window) (#_GET_X_LPARAM lParam)
			(last-pos-y window) (#_GET_Y_LPARAM lParam))

		  (return 0)

		  #+NIL
		  (go break)))

	       (#.#_WM_GETMINMAXINFO ;; 36

		(when (window-monitor window)
		  (go break))

		(clet ((frame #_<RECT>))
		  (let ((&frame (c-addr-of frame)))
		    (setf (#_.left &frame) 0
			  (#_.top &frame) 0
			  (#_.right &frame) 0
			  (#_.bottom &frame) 0)
		    (let ((mmi (noffi::cons-ptr (ccl:%int-to-ptr lParam) 0 '#_<MINMAXINFO*>))
			  (style (get-window-style window))
			  (ex-style (get-window-ex-style window)))

		      (if (windows-10-version-1607-or-greater?)
			      
			  (#_AdjustWindowRectExForDpi &frame style #_FALSE ex-style
						      (#_GetDpiForWindow (h window)))
			      
			  (#_AdjustWindowRectEx &frame style #_FALSE ex-style))

		      (let ((p-pt-min-track-size (c->-addr mmi '#_ptMinTrackSize))
			    (p-pt-max-track-size (c->-addr mmi '#_ptMaxTrackSize))
			    (p-pt-max-position (c->-addr mmi '#_ptMaxPosition))
			    (p-pt-max-size (c->-addr mmi '#_ptMaxSize)))
			  
			(unless (or (eq (window-min-width window) :dont-care)
				    (eq (window-min-height window) :dont-care))
			      
			  (setf (#_.x p-pt-min-track-size) (+ (window-min-width window) (#_.right &frame) (- (#_.left &frame)))
				(#_.y p-pt-min-track-size) (+ (window-min-height window) (#_.bottom &frame) (- (#_.top &frame)))))

			(unless (or (eq (window-max-width window) :dont-care)
				    (eq (window-max-height window) :dont-care))
			      
			  (setf (#_.x p-pt-max-track-size) (+ (window-min-width window) (#_.right &frame) (- (#_.left &frame)))
				(#_.y p-pt-max-track-size) (+ (window-min-height window) (#_.bottom &frame) (- (#_.top &frame)))))

			(unless (last-decorated? window)
			  (clet ((mi #_<MONITORINFO>))
			    (let ((&mi (c-addr-of mi))
				  (size (c-sizeof-type '#_<MONITORINFO>))
				  (mh (#_MonitorFromWindow (h window) #_MONITOR_DEFAULTTONEAREST)))
			      (#_memset &mi size 0)

			      (setf (#_.cbSize &mi) size)

			      (#_GetMonitorInfo mh &mi)

			      (let ((p-rc-work (c->-addr &mi '#_rcWork))
				    (p-rc-monitor (c->-addr &mi '#_rcMonitor)))

				(setf (#_.x p-pt-max-position) (- (#_.left p-rc-work) (#_.left p-rc-monitor)))
				(setf (#_.y p-pt-max-position) (- (#_.top p-rc-work) (#_.top p-rc-monitor)))
				(setf (#_.x p-pt-max-size) (- (#_.right p-rc-work) (#_.left p-rc-work)))
				(setf (#_.y p-pt-max-size) (- (#_.bottom p-rc-work) (#_.top p-rc-work)))))))

			))))
		(return 0)

		#+NIL
		(go break))

	       (#.#_WM_MOUSEACTIVATE ;; 33
		(when (= (#_HIWORD lParam) #_WM_LBUTTONDOWN)
		  (when (/= (#_LOWORD lParam) #_HTCLIENT)
		    (setf frame-action? t)))
		(go break))

	       (#.#_WM_DWMNCRENDERINGCHANGED (go break)) ;; 799 ;; what am i?

	       (#.#_WM_CAPTURECHANGED ;; 533
		    
		(when (and (= lParam 0) frame-action?)
		  (if (eq (window-cursor-mode window) :disabled)
		      (disable-cursor window)
		      (when (eq (window-cursor-mode window) :captured)
			(capture-win32-cursor window)))
		  (setf frame-action? nil))
		    
		(go break))

	       (#.#_WM_SIZING ;; 532

		(when (or (eq (window-aspect-numer window) :dont-care)
			  (eq (window-aspect-denom window) :dont-care))
		  (go break))

		(apply-aspect-ratio window wparam lparam)

		(return 1))

	       (#.#_WM_KILLFOCUS ;; 8

		(if (eq cursor-mode :disabled)
		    (enable-cursor window)
		    (when (eq cursor-mode :captured)
		      (release-win32-cursor window)))

		(when (and monitor auto-iconify?)
		  (iconify-win32-window window))

		(handle-event window (make-instance 'window-defocus-event))
		(return 0))

	       (#.#_WM_SYSCOMMAND ;; 274
		#+NOTYET
		(case (logand wParam #xfff0)
		    
		  ((#.#_SC_SCREENSAVE #.#_SC_MONITORPOWER)

		   (if monitor
		       (return 0)
		       (go break)))

		  (#.#_SC_KEYMENU
		   (unless key-menu
		     (return 0))
		   (go break)))
		(go break))


	       (#.#_WM_CLOSE ;; 16

		(handle-event window (make-instance 'window-close-event
						    :window window))
		(return 0))

	       (#.#_WM_INPUTLANGCHANGE ;; 81
		#+NOTYET
		(win32-update-key-names (window-display window))
		(go break))

	       ((#.#_WM_CHAR #.#_WM_SYSCHAR) ;; 258 and 262
		(go break)
		#+NOTYET
		(if (and (>= wParam #xd800) (<= wParam #xdbff))
		    (setf high-surrogate wParam)
		    (let ((codepoint 0))
		      (if (and (>= wParam #xdc00) (<= wParam #xdfff))
			  (when high-surrogate
			    (incf codepoint (ash (- high-surrogate #xd800) 10))
			    (incf codepoint (- wParam #xdc00))
			    (incf codepoint #x10000))
			  (setq codepoint wParam))
		      (setq high-surrogate 0)
		      (let ((event (make-instance 'keyboard-event
						  :character (code-char codepoint)
						  :mods (get-key-mods (window-display window))
						  :syschar? (= uMsg #_WM_SYSCHAR))))
			(handle-event window event))))
		(when (and (= uMsg #_WM_SYSCHAR) key-menu)
		  (go break))
		(return 0))

	       (#.#_WM_UNICHAR ;; 265
		(go break)
		(when (= wParam #_UNICODE_NOCHAR)
		  (return 1))

		#+NIL
		(let ((event (make-instance 'keyboard-event
					    :character (code-char wparam)
					    :mods (get-key-mods (window-display window))
					    :syschar? nil)))
		  (handle-event window event))
		(return 0))

	       ((#.#_WM_KEYDOWN #.#_WM_SYSKEYDOWN #.#_WM_KEYUP #.#_WM_SYSKEYUP) ;; 256, 260, 257, 261
		#+NOTYET
		(let ((key)
		      (scancode)
		      (action (if (= 0 (logand (#_HIWORD lParam) +kf-up+)) :press :release))
		      (mods (get-key-mods (window-display window))))

		  (setq scancode (logand (#_HIWORD lParam) (logior +kf-extended+ #xff)))

		  (when (= 0 scancode)
		    (setq scancode (#_MapVirtualKeyW wParam +map-vk-to-vsc+)))

		  (when (= scancode #x54)
		    (setq scancode #x137))

		  (when (= scancode #x146)
		    (setq scancode #x45))

		  (when (= scancode #x136)
		    (setq scancode #x36))

		  (if (= wParam #_VK_CONTROL)
		      (if (logand (#_HIWORD lParam) +kf-extended+)
			  (setq key +key-right-control+)
			  (clet ((next #_<MSG>))
			    (setq time (#_GetMessageTime))
			    (when (#_PeekMessageW (c-addr-of next) nil 0 0 #_PM_NOREMOVE)
			      (case (#_.message (c-addr-of next))
				((#_WM_KEYDOWN #_WM_SYSKEYDOWN #_WM_KEYUP #_WMSYSKEYUP)
				 (when
				     (and (= (#_.wParam (c-addr-of next)) #_VK_MENU)
					  (and (not (zerop
						     (logand (HIWORD (#_.lParam (c-addr-of next)))
							     +kf-extended+)))
					       (= (#_.time (c-addr-of next)) time)))
				   (go break)))))
			    (setq key +key-left-control+)))
		      (when (= wParam #_VK_PROCESSKEY)
			(go break)))

		  (if (and (= action +release+) (= wParam #_VK_SHIFT))
		      (progn
			(input-key window +key-left-shift+ scancode action mods)
			(input-key window +key-right-shift+ scancode action mods))
		      (if (= wParam #_VK_SNAPSHOT)
			  (progn
			    (input-key window key scancode +press+ mods)
			    (input-key window key scancode +release+ mods))
			  (input-key window key scancode action mods))))
		(go break))

	       ((#.#_WM_LBUTTONDOWN
		 #.#_WM_RBUTTONDOWN #.#_WM_MBUTTONDOWN #.#_WM_XBUTTONDOWN
		 #.#_WM_LBUTTONUP #.#_WM_RBUTTONUP
		 #.#_WM_MBUTTONUP #.#_WM_XBUTTONUP)

		    
		(go break))

	       (#.#_WM_MOUSEMOVE ;; 512
		(let* ((x (#_GET_X_LPARAM lParam))
		       (y (#_GET_Y_LPARAM lParam))
		       (event (make-instance 'pointer-motion-event
					     :x x
					     :y y)))
		  (handle-event window event))
		(return 0))

	       (#.#_WM_NCMOUSEMOVE ;; 160
		(go break))

	       (#.#_WM_INPUT )

	       (#.#_WM_MOUSELEAVE ;; 675
		)

	       (#.#_WM_NCMOUSELEAVE ;; 674
		)

	       (#.#_WM_MOUSEWHEEL )

	       (#.#_WM_MOUSEHWHEEL )

	       ((#.#_WM_ENTERSIZEMOVE #.#_WM_ENTERMENULOOP))

	       ((#.#_WM_EXITSIZEMOVE #.#_WM_EXITMENULOOP))

	       ((#.#_WM_DWMCOMPOSITIONCHANGED #.#_WM_DWMCOLORIZATIONCOLORCHANGED))

	       (#.#_WM_GETDPISCALEDSIZE )

	       (#.#_WM_DPICHANGED )

	       (#.#_WM_SETCURSOR ;; 32
		(go break))

	       (#.#_WM_DROPFILES )
	       )))
	   
       break    
	 (finish-output))
      
      ;;(print 'heapchk)
      ;;(print (heapchk))
      ;;(finish-output)
      (return (#_DefWindowProcW hWnd uMsg wParam lParam)))))


(defcfun (window-proc-callback #_<LPARAM>)
	 ((hWnd #_<HWND>) (uMsg #_<UINT>) (wParam #_<WPARAM>) (lParam #_<LPARAM>))
  (window-proc hWnd uMsg wParam lParam))



(defmethod %create-native-win32-window (window &rest args
					&key (xpos nil) (ypos nil)
					  (width 640) (height 480)
					  (title "CLUI")
					  (decorated? t)
					  (maximized? nil)
					  (resizable? t)
					  (floating? nil)
					  (transparent? nil)
					  (scale-to-monitor? t)
					  (key-menu nil)
					&allow-other-keys)

  (declare (ignorable args transparent? resizable? floating? key-menu))
  
  (let* ((style (get-window-style window))
	 (ex-style (get-window-ex-style window))
	 (frame-x)
	 (frame-y)
	 (frame-width)
	 (frame-height))


    (with-slots (main-window-class) (window-display window)
      (let ((p-cls-nm (lpcwstr "CLUI")))
	(with-slots (handle monitor) window 
	  
	  (flet ((maybe-register-class ()
		   (unless main-window-class
		     (let ((p-icon-nm (lpcwstr "CLUI-ICON")))
		       (let* ((size (load-time-value (c-sizeof-type '#_<WNDCLASSEXW>))))
			 (clet ((wc #_<WNDCLASSEXW>))
			   (let ((&wc (c-addr-of wc)))

			     (setf (#_.cbSize &wc) size)
			     (setf (#_.style &wc) (logior #_CS_HREDRAW #_CS_VREDRAW #_CS_OWNDC))
			     (setf (#_.lpfnWndProc &wc) window-proc-callback)
			     (setf (#_.cbClsExtra &wc) 0)
			     (setf (#_.cbWndExtra &wc) 0)
			     (setf (#_.hInstance &wc) (#_GetModuleHandleW nil))
			     (setf (#_.hCursor &wc) (#_LoadCursorW nil #_IDC_ARROW))
			     (setf (#_.hbrBackground &wc) nil)
			     (setf (#_.lpszMenuName &wc) nil)
			     (setf (#_.lpszClassName &wc) p-cls-nm)
			     (setf (#_.hIconSm &wc) nil)

			     ;;(print 'bb)
			     ;;(print (#_GetLastError))

			     (setf (#_.hIcon &wc) (#_LoadImageW
						   (#_GetModuleHandleW nil)
						   p-icon-nm
						   #_IMAGE_ICON
						   0 0
						   (logior #_LR_DEFAULTSIZE #_LR_SHARED)))
			     #+NIL
			     (print (#_.hIcon &wc))
			     
			     (unless (#_.hIcon &wc)
			       (setf (#_.hIcon &wc) (#_LoadImageW
						     nil
						     #_IDI_APPLICATION
						     #_IMAGE_ICON
						     0 0 (logior #_LR_DEFAULTSIZE #_LR_SHARED))))

			     #+NIL
			     (unless main-window-class
			       (when(#_GetClassInfoExW (#_GetModuleHandleW nil) p-cls-nm &wc)
				 (warn "class already exists, resetting...")
				 (unless (#_UnregisterClassW p-cls-nm (#_GetModuleHandleW nil))
				   (warn "unable to unregister class."))))

			     ;;(print 'aa)
			     ;;(print (#_GetLastError))


			     (let ((result (#_RegisterClassExW &wc)))
			       (print result)
			       (if (= 0 result)
				   (let ((error (#_GetLastError)))
				     (error "Win32: Failed to register window class, error ~A." error))
				   (setf main-window-class result)))
			     #+NIL
			     (print 'a)
			     #+NIL
			     (print (#_GetLastError))))))))
	   
		 (create-window ()
			      
		   (if monitor
		     
		       (clet ((mi #_<MONITORINFO>))
			 (let ((pmi (c-addr-of mi)))
			   (setf (#_.cbSize pmi) (load-time-value (c-sizeof-type '#_<MONITORINFO>)))
	      
			   (#_GetMonitorInfoW (slot-value monitor 'handle) pmi)
		       
			   (let ((prcMonitor (c->-addr pmi '#_rcMonitor)))
			 
			     (setq frame-x (#_.left prcMonitor))
			     (setq frame-y (#_.top prcMonitor))
			     (setq frame-width (- (#_.right prcMonitor) (#_.left prcMonitor)))
			     (setq frame-height (- (#_.bottom prcMonitor) (#_.top prcMonitor))))))

		       (clet ((rect #_<RECT>))
			 (let ((&rect (c-addr-of rect)))

			   (setf (#_.left   &rect) 0
				 (#_.top    &rect) 0
				 (#_.right  &rect) (round width)
				 (#_.bottom &rect) (round height))

			   (setf (last-maximized? window) maximized?)
		       
			   (when maximized?
			     (setq style (logior style #_WS_MAXIMIZE)))

			   (setf (x window) (#_.left &rect)
				 (y window) (#_.top &rect))
			 
			   (setf (width window) (- (#_.right &rect) (#_.left &rect))
				 (height window) (- (#_.bottom &rect) (#_.top &rect)))
			 
			   (#_AdjustWindowRectEx &rect style #_FALSE ex-style)

			   (let ((left (#_.left &rect))
				 (top (#_.top &rect))
				 (right (#_.right &rect))
				 (bottom (#_.bottom &rect)))

			     (if (and (null xpos) (null ypos))
		  
				 (progn 
				   (setq frame-x #_CW_USEDEFAULT)
				   (setq frame-y #_CW_USEDEFAULT))
		  
				 (progn
				   (setq frame-x (+ xpos left))
				   (setq frame-y (+ ypos top))))

			     (setq frame-width (- right left))
			     (setq frame-height (- bottom top))))))

		   (let ((id (gen-window-id)))

		     (let ((wide-title (lpcwstr title)))
		       (clet ((wc #_<wndconfig>))
			 (let ((&wc (c-addr-of wc)))
			   (setf (#_.id &wc) id
				 (#_.maximized &wc) (if maximized? 1 0)
				 (#_.scaletomonitor &wc) (if scale-to-monitor? 1 0)
				 (#_.decorated &wc) (if decorated? 1 0))

		     
			   (print frame-x)
			   (print frame-y)
			   (print frame-width)
			   (print frame-height)
			   (print main-window-class)

			   (print 'b)
			   (print (#_GetLastError))
		       
			   (setf (gethash id *id->window-table*) window)
		       
			   (let ((result (#_CreateWindowExW ex-style
							    (#_MAKEINTATOM main-window-class)
							    wide-title
							    style
							    frame-x frame-y
							    frame-width frame-height
							    nil ;; no parent window
							    nil ;; no window menu
							    (#_GetModuleHandleW nil)
							    (c-cast '#_<LPVOID> &wc)
							    )))

			     (print 'c)
			     (print (#_GetLastError))
		       

			     (if result
				 (progn
				   (setf handle result) ;; wndproc should do this, but it should be ok to do again.

				   (unless (or (windows-11-build-or-later? 0)
					       (windows-10-version-1607-or-greater?))
				     ;; this is done in wndproc for windos-10-1607 or greater
				     (set-window-prop handle id))

				   (#_SetWindowPos handle #_HWND_TOP
						   frame-x
						   frame-y
						   frame-width
						   frame-height
						   (logior #_SWP_NOACTIVATE #_SWP_NOZORDER)))
				 (error "Win32: Failed to create window."))))))))
	       
		 (maybe-allow-messages ()
		   (when (windows-7-or-greater?)
		     (#_ChangeWindowMessageFilterEx handle #_WM_DROPFILES #_MSGFLT_ALLOW nil)
		     (#_ChangeWindowMessageFilterEx handle #_WM_COPYDATA #_MSGFLT_ALLOW nil)
		     (#_ChangeWindowMessageFilterEx handle +WM_COPYGLOBALDATA+ #_MSGFLT_ALLOW nil)))

		 (maybe-reposition-window ()

		   (unless monitor
		   
		     (clet ((rect #_<RECT>))
		       (let ((&rect (c-addr-of rect)))
			 (setf (#_.left &rect) 0
			       (#_.top &rect) 0
			       (#_.right &rect) (round width)
			       (#_.bottom &rect) (round height))

			 (clet ((wp #_<WINDOWPLACEMENT>))
			   (let ((p-wp (c-addr-of wp)))
			     (setf (#_.length p-wp) (load-time-value (c-sizeof-type '#_<WINDOWPLACEMENT>)))
		     
			     (let ((mh (#_MonitorFromWindow handle #_MONITOR_DEFAULTTONEAREST)))
			   
			       (when scale-to-monitor?
		      
				 (multiple-value-bind (xscale yscale)
				     (get-hmonitor-content-scale mh)
			
				   (when (and (> xscale 0.0f0) (> yscale 0.0f0))
			  
				     (setf (#_.right &rect) (round (* (#_.right &rect) xscale)))
				     (setf (#_.bottom &rect) (round (* (#_.bottom &rect) yscale))))))
		    
			       (if (windows-10-version-1607-or-greater?)
				   (#_AdjustWindowRectExForDpi &rect style #_FALSE ex-style
							       (#_GetDpiForWindow handle))
			       
				   (#_AdjustWindowRectEx &rect style #_FALSE ex-style))

			       (#_GetWindowPlacement handle p-wp)

			       (let ((prcNormalPosition (c->-addr p-wp '#_rcNormalPosition)))
			     
				 (#_OffsetRect &rect
					       (- (#_.left prcNormalPosition) (#_.left &rect))
					       (- (#_.top prcNormalPosition)  (#_.top &rect)))

				 (setf (#_.left prcNormalPosition) (#_.left &rect))
				 (setf (#_.top prcNormalPosition) (#_.top &rect))
				 (setf (#_.right prcNormalPosition) (#_.right &rect))
				 (setf (#_.bottom prcNormalPosition) (#_.bottom &rect))
		    			   
				 (setf (#_.showCmd p-wp) #_SW_HIDE)

				 (#_SetWindowPlacement handle p-wp))
		       
			       (when (and maximized? (not decorated?))

				 (clet ((mi #_<MONITORINFO>))
				   (let ((&mi (c-addr-of mi)))
				     (setf (#_.cbSize &mi) (load-time-value (c-sizeof-type '#_<MONITORINFO>)))
			   
				     (#_GetMonitorInfoW mh &mi)

				     (let ((rc-work (c->-addr &mi '#_rcWork)))
			       
				       (print rc-work)
				       (print (#_.left rc-work))
				       (print (#_.top rc-work))
				       (print (#_.right rc-work))
				       (print (#_.bottom rc-work))

				       (#_SetWindowPos handle #_HWND_TOP
						       (#_.left rc-work)
						       (#_.top rc-work)
						       (- (#_.right rc-work) (#_.left rc-work))
						       (- (#_.bottom rc-work) (#_.top rc-work))
						       (logior #_SWP_NOACTIVATE #_SWP_NOZORDER))))))))))))))


	    ;;(print 'aaa)
	    ;;(print (#_GetLastError))
	  
	    (maybe-register-class)

	    ;;(print 'bbb)
	    ;;(print (#_GetLastError))
	  
	    (create-window)

	    ;;(print 'ccc)
	    ;;(print (#_GetLastError))

	    (maybe-allow-messages)

	    (setf (slot-value window 'scale-to-monitor?) scale-to-monitor?)
	    
	    (maybe-reposition-window)
	    
	    (#_DragAcceptFiles handle #_TRUE)
	    
	    (multiple-value-bind (xpos ypos) (get-win32-window-pos window)
	      
	      (setf (last-pos-x window) xpos
		    (last-pos-y window) ypos)
	      
	      (multiple-value-bind (window-width window-height) (get-win32-window-size window)
	  
		(setf (last-width window) window-width
		      (last-height window) window-height)

		#+NOTNOW
		(apply #'initialize-window-devices window
		       :width window-width
		       :height window-height
		       args)))
	    t))))))

(defun heapchk ()
  (ccl::%ff-call (ccl::%reference-external-entry-point (ccl:external "_heapchk"))
		 :signed-fullword))



(defun acquire-win32-monitor (window monitor)
  (declare (type os-window-mixin window))
  
  (when (zerop (acquired-monitor-count (window-display window)))
    (#_SetThreadExecutionState (logior #_ES_CONTINUOUS #_ES_DISPLAY_REQUIRED))
    (clet ((mouseTrailSize #_<UINT> (mouse-trail-size (window-display window))))
      (#_SystemParametersInfoW #_SPI_GETMOUSETRAILS 0 (c-addr-of mouseTrailSize) 0)
      (setf (mouse-trail-size (window-display window)) (cval-value mouseTrailSize))
      (#_SystemParametersInfoW #_SPI_SETMOUSETRAILS 0 0 0)))
  
  (unless (monitor-window monitor)
    (incf (acquired-monitor-count (window-display window))))
  
  (set-win32-monitor-video-mode (window-monitor window) (window-video-mode window))
  (input-monitor-window (window-monitor window) window))


(defun release-win32-monitor (window monitor)
  (declare (type os-window-mixin window))
  (declare (type application-mixin (window-display window)))
  
  (unless (eq (monitor-window monitor) window)
    (return-from release-win32-monitor (values)))

  (decf (acquired-monitor-count (window-display window)))
  
  (when (zerop (acquired-monitor-count (window-display window)))
    (#_SetThreadExecutionState #_ES_CONTINUOUS)
    (clet ((mouseTrailSize #_<UINT> (mouse-trail-size (window-display window))))
      (#_SystemParametersInfoW #_SPI_SETMOUSETRAILS 0 (c-addr-of mouseTrailSize) 0)))
  
  (input-monitor-window monitor nil)
  (restore-win32-monitor-video-mode monitor)
  (values))

(defun fit-to-monitor (window)
  (declare (type os-window-mixin window))
  (clet ((mi #_<MONITORINFO>))
    (let* ((&mi (c-addr-of mi))
	   (rcMonitor (c->-addr &mi '#_rcMonitor)))
      
      (#_GetMonitorInfoW (h window) &mi)
      (#_SetWindowPos (h window) #_HWND_TOPMOST
		      (#_.left rcMonitor)
		      (#_.top rcMonitor)
		      (- (#_.right rcMonitor) (#_.left rcMonitor))
		      (- (#_.bottom rcMonitor) (#_.top rcMonitor))
		      (logior #_SWP_NOZORDER #_SWP_NOACTIVATE #_SWP_NOCOPYBITS))))
  (values))
  

(defun set-win32-window-monitor (window monitor xpos ypos width height refresh-rate)
  (declare (ignorable refresh-rate))
  (declare (type os-window-mixin window))
  (declare (type (or null monitor-mixin) monitor))
  
  (when (eq (window-monitor window) monitor)
    
    (if monitor
	
	(when (eq (monitor-window monitor) window)
	  (acquire-win32-monitor window monitor)
	  (fit-to-monitor window))
	
	(clet ((rect #_<RECT>))
	  (let ((&rect (c-addr-of rect)))
	    (setf (#_.left   &rect) (round xpos)
		  (#_.top    &rect) (round ypos)
		  (#_.right  &rect) (round (+ xpos width))
		  (#_.bottom &rect) (round (+ ypos height)))
	  
	    (if (windows-10-version-1607-or-greater?)
	      
		(#_AdjustWindowRectExForDpi &rect (get-window-style window)
					    #_FALSE (get-window-ex-style window)
					    (#_GetDpiForWindow (h window)))

		(#_AdjustWindowRectEx &rect (get-window-style window)
				      #_FALSE (get-window-ex-style window)))

	    (#_SetWindowPos (h window) #_HWND_TOP
			    (#_.left &rect) (#_.top &rect)
			    (- (#_.right &rect) (#_.left &rect))
			    (- (#_.bottom &rect) (#_.top &rect))
			    (logior #_SWP_NOCOPYBITS #_SWP_NOACTIVATE #_SWP_NOZORDER))
	  
	    (return-from set-win32-window-monitor (values))))))

  (when (window-monitor window)
    (release-win32-monitor window (window-monitor window)))

  (setf (%window-monitor window) monitor)

  (if (window-monitor window)
      
      (clet ((mi #_<MONITORINFO>))
	(let* ((&mi (c-addr-of mi))
	       (rcMonitor (c->-addr &mi '#_rcMonitor))
	       (flags (logior #_SWP_SHOWWINDOW #_SWP_NOACTIVATE #_SWP_NOCOPYBITS)))
	  
	  (when (last-decorated? window)
	    (let ((style (#_GetWindowLongW (h window) #_GWL_STYLE)))
	      (setq style (logand style (lognot #_WS_OVERLAPPEDWINDOW)))
	      (setq style (logior style (get-window-style window)))
	      (#_SetWindowLongW (h window) #_GWL_STYLE style)
	      (setq flags (logior flags #_SWP_FRAMECHANGED))))
	
	  (acquire-win32-monitor window monitor)

	  (#_GetMonitorInfoW (h (window-monitor window)) &mi)
	  (#_SetWindowPos (h window) #_HWND_TOPMOST
			  (#_.left rcMonitor)
			  (#_.top rcMonitor)
			  (- (#_.right rcMonitor) (#_.left rcMonitor))
			  (- (#_.bottom rcMonitor) (#_.top rcMonitor))
			  flags)))

      (let ((after))
	(clet ((rect #_<RECT>))
	  (let ((&rect (c-addr-of rect)))
	    (setf (#_.left   &rect) (round xpos)
		  (#_.top    &rect) (round ypos)
		  (#_.right  &rect) (round (+ xpos width))
		  (#_.bottom &rect) (round (+ ypos height)))
	    
	    (let ((style (#_GetWindowLongW (h window) #_GWL_STYLE))
		  (flags (logior #_SWP_NOACTIVATE #_SWP_NOCOPYBITS)))
	    
	      (when (last-decorated? window)
		(let ((style (logand style (lognot #_WS_POPUP))))
		  (setq style (logior style (get-window-style window)))
		  (#_SetWindowLongW (h window) #_GWL_STYLE style)
		
		  (setq flags (logior flags #_SWP_FRAMECHANGED))))
	    
	      (if (last-floating? window)
		  (setq after #_HWND_TOPMOST)
		  (setq after #_HWND_NOTOPMOST))

	      (if (windows-10-version-1607-or-greater?)
		  
		  (#_AdjustWindowRectExForDpi &rect (get-window-style window)
					      #_FALSE (get-window-ex-style window)
					      (#_GetDpiForWindow (h window)))
		  
		  (#_AdjustWindowRectEx &rect (get-window-style window)
					#_FALSE (get-window-ex-style window)))

	      (#_SetWindowPos (h window) after
			      (#_.left &rect) (#_.top &rect)
			      (- (#_.right &rect) (#_.left &rect))
			      (- (#_.bottom &rect) (#_.top &rect))
			      flags))))))
  (values))
	
(defun set-win32-window-mouse-passthrough (window enabled?)
  (declare (type win32-window-mixin window))
  (clet ((key #_<COLORREF> 0)
	 (alpha #_<BYTE> 0)
	 (flags #_<DWORD> 0))

    (let ((ex-style (#_GetWindowLongW (h window) (noffi::cons-cval #_GWL_EXSTYLE '#_<INT>))))

      (when (logtest ex-style #_WS_EX_LAYERED)
	(#_GetLayeredWindowAttributes (h window)
				      (c-addr-of key)
				      (c-addr-of alpha)
				      (c-addr-of flags)))


      (if enabled?
	  (setq ex-style (logior ex-style #_WS_EX_TRANSPARENT #_WS_EX_LAYERED))
	  (progn
	    (setq ex-style (logand ex-style (lognot #_WS_EX_TRANSPARENT)))

	    (unless (= 0 (logand ex-style #_WS_EX_LAYERED))
	      (when (= 0 (logand (cval-value flags) #_LWA_ALPHA))
		(setq ex-style (logand ex-style (lognot #_WS_EX_LAYERED)))))))



      (#_SetWindowLongW (h window) #_GWL_EXSTYLE (noffi::cons-cval ex-style '#_<LONG>))

      (when enabled?
	(#_SetLayeredWindowAttributes (h window) key alpha flags))
      (values))))
				      
(defun get-win32-desktop-workarea ()
  (clet ((desktop #_<RECT>))
    (let ((&desktop (c-addr-of desktop))
	  (screen (#_GetDesktopWindow)))

      (#_GetWindowRect screen &desktop)

      (values (#_.left &desktop)
	      (#_.top &desktop)
	      (- (#_.right &desktop) (#_.left &desktop))
	      (- (#_.bottom &desktop) (#_.top &desktop))))))
      

      

     
  
  
  


(defun create-native-win32-window (window &rest args
				   &key (visible? t)
				     (focused? t)
				     (mouse-passthrough? nil)
				   &allow-other-keys)
  (declare (type os-window-mixin window))
  (if (apply #'%create-native-win32-window window args)
      (progn
	(when mouse-passthrough?
	  (set-win32-window-mouse-passthrough window t))
	(if (window-monitor window)
	    (progn
	      (show-win32-window window)
	      (focus-win32-window window)
	      (acquire-win32-monitor window (window-monitor window))
	      (fit-to-monitor window))
	    (when visible?
	      (show-win32-window window)
	      (when focused?
		(focus-win32-window window))))
	(print 'heapchk)
	(print (heapchk))
	window)
      (error "Win32: failed to create native window.")))

(eval-when (:compile-toplevel)
#_{
typedef struct monitor_cons {
LPCWSTR adapterName;
HMONITOR handle;
} monitor_cons;
}
)


(defun wait-win32-events (app)
  (clet ((msg #_<MSG>))
    (let* ((&msg (c-addr-of msg))
	   (result)
	   (window))

      ;;(terpri)
      ;;(princ 1)
      ;;(finish-output)

      (unless (> (setq result (#_GetMessageW &msg nil 0 0)) 0)
	
	(error "GetMessage returned non positive"))

      ;;(format t "~%~A" (#_GetLastError))
      ;;(finish-output)

      ;;(terpri)
      ;;(princ 1.5)
      ;;(finish-output)
      
      (unless (zerop result)

	;;(terpri)
	;;(princ 2)
	;;(finish-output)

	(if (= (#_.message &msg) #_WM_QUIT)

	    (progn
	      ;;(terpri)
	      ;;(princ 6)
	;;      (finish-output)
		     
	      (setq window (display-window-list-head app))

	      (loop while window
		    do (handle-event window (make-instance 'window-close-event))
		       (setq window (window-next window))))

	    (progn
	      ;;(terpri)
	      ;;(princ 3)
	      ;;(finish-output)
	      (#_TranslateMessage &msg)
	      (#_DispatchMessageW &msg)
	      
	      ;;(terpri)
	      ;;(print 4)
	      (finish-output)))))))

(defun poll-win32-events (app)
  (clet ((msg #_<MSG>))
    (let ((&msg (c-addr-of msg))
	  (window))

      (loop until (progn
	;;	    (terpri)
	;;	    (princ 1)
	;;	    (finish-output)
		    (zerop (#_PeekMessageW &msg nil 0 0 #_PM_REMOVE)))

	    do ;;(terpri)
	       ;;(princ 2)
	       ;;(finish-output)

	       (if (= (#_.message &msg) #_WM_QUIT)

		   (progn
		 ;;    (terpri)
		   ;;  (princ 6)
		     ;;(finish-output)
		     
		     (setq window (display-window-list-head app))

		     (loop while window
			   do (handle-event window (make-instance 'window-close-event))
			      (setq window (window-next window))))

		   (progn
		     ;;(terpri)
		     ;;(princ 3)
		     ;;(finish-output)
		     (#_TranslateMessage &msg)
		     (#_DispatchMessageW &msg)

		     ;;(terpri)
		     ;;(princ 4)
		     ;;(finish-output)
		     )))
      ;;(terpri)
      ;;(princ 5)
      ;;(finish-output)
      )))

(defun run (&optional (display (default-display)))
  (loop ;;until (application-exit? app)
	do (wait-events display)))
      

(defun test ()
  (ccl::call-in-initial-process
   (lambda ()
     (make-instance 'window))))

