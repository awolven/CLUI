(in-package :abstract-os)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (noffi::noffi-syntax t)
  (defparameter noffi::*last-good-token* nil)
  )

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

(noffi::noffi-syntax t)
;;#_{#include "dependencies.i"}


(defconstant +WM_COPYGLOBALDATA+ #x0049)




(defmacro with-lpcwstr* ((var string) &body body)
  (let ((sap-sym (gensym)))
    `(ccl::with-native-utf-16-cstr (,sap-sym ,string)
       (let ((,var (%cons-ptr ,sap-sym 0 '#_<LPCWSTR*>)))
	 ,@body))))

(defun lpcwstr* (string)
  (let* ((len (length string)))
    (multiple-value-bind (data offset) (ccl::array-data-and-offset string)
      (let* ((end (+ offset len))
	     (noctets (ccl::utf-16-octets-in-string data offset end))
	     (octets (ccl::malloc (1+ noctets))))
	(ccl::native-utf-16-memory-encode data octets 0 offset end)
	(setf (ccl::%get-signed-byte octets end) 0)
	(%cons-ptr octets 0 '#_<LPCWSTR*>)))))
	    

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
				      
		  
  




(defun cursor-in-content-area (window)
  (block nil
    (clet ((area #_<RECT>)
	   (pos #_<POINT>))
      (let ((&area (c-addr-of area))
	    (&pos (c-addr-of pos)))
	
	(when (= 0 (#_GetCursorPos &pos))
	  (return nil))

	(unless (= (sap-int (ptr-value (#_WindowFromPoint pos)))
		   (sap-int (ptr-value (h window))))
	  (return nil))

	(#_GetClientRect (h window) &area)
	(#_ClientToScreen (h window) (#_.left &area))
	(#_ClientToScreen (h window) (#_.right &area))

	(#_PtInRect &area pos)))))

(defmethod initialize-instance :after ((app win32-application-mixin) &rest initargs)
  (apply #'init-win32-application app initargs))


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
  (let ((wide (lpcwstr* value)))
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
  (declare (ignorable window))
  (values))

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
  (= (sap-int (ptr-value (h window)))
     (sap-int (ptr-value (#_GetActiveWindow)))))

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

(defun set-win32-window-visible (window)
  (declare (ignorable window))
  (values))

(defun make-win32-window-visible (window)
  (declare (ignorable window))
  (values))

(defun make-win32-window-invisible (window)
  (declare (ignorable window))
  (values))

(defun get-win32-window-hovered (window)
  (cursor-in-content-area window))

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

#+NOTYET
(defun destroy-win32-window (window)
  (when (window-monitor window)
    (release-win32-monitor window))
  (when (eq (disabled-cursor-window *app*) window)
    (enable-cursor window))
  (when (eq (captured-cursor-window *app*) window)
    (release-cursor window))
  (when (h window)
    (with-lpcwstr* (string "ABSTRACT-OS")
      (#_RemovePropW (h window) string))
    (#_DestroyWindow (h window))
    (setf (h window) nil))
  (when (big-icon window)
    (#_DestroyIcon (big-icon window)))
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
    
    (if (slot-value win32-window 'monitor)
	(setq style (logior style #_WS_POPUP))
	(progn
	  (setq style (logior style #_WS_SYSMENU #_WS_MINIMIZEBOX))
	  (if (slot-value win32-window 'decorated?)
	      (progn
		(setq style (logior style #_WS_CAPTION))
		(when (slot-value win32-window 'resizable?)
		  (setq style (logior style #_WS_MAXIMIZEBOX #_WS_THICKFRAME))))
	      (setq style (logior style #_WS_POPUP)))))
    style))


(defun get-window-ex-style (window)
  (let ((style #_WS_EX_APPWINDOW))

    (when (or (window-monitor window) (currently-floating? window))
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
			     :address (ptr-value posvi)
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
      (let ((pszCSDVersion (noffi::cons-ptr (ccl::%inc-ptr (ptr-value posvi)
							   (offset-of '#_<OSVERSIONINFOEXW> '#_szCSDVersion))
					    0 '#_<WCHAR[128]>)))
	(setf (noffi::c-aref pszCSDVersion 0) 0)

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
			       :address (ptr-value posvi)
			       :unsigned-fullword mask :unsigned-doubleword condition
			       :unsigned-fullword))))))))

(defun windows-11-build-or-later? (build)
  (clet ((osvi #_<OSVERSIONINFOEXW>))
    (let ((posvi (c-addr-of osvi)))
      (let ((pszCSDVersion (noffi::cons-ptr (ccl::%inc-ptr (ptr-value posvi)
							   (offset-of '#_<OSVERSIONINFOEXW> '#_szCSDVersion))
					    0 '#_<WCHAR[128]>)))
	(setf (noffi::c-aref pszCSDVersion 0) 0)

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
			       :address (ptr-value posvi)
			       :unsigned-fullword mask :unsigned-doubleword condition
			       :unsigned-fullword))))))))


(defun windows-10-version-1607-or-greater? ()
  (or (windows-11-build-or-later? 0)
      (windows-10-build-or-later? 14393)))


(defun windows-10-version-1703-or-greater? ()
  (or (windows-11-build-or-later? 0)
      (windows-10-build-or-later? 15063)))



(defun get-dpi-for-monitor (hmonitor)
  (clet ((xdpi #_<UINT>)
	 (ydpi #_<UINT>))
    
    (if (zerop (print (ccl::%ff-call (ccl::%reference-external-entry-point
				      (ccl:external "GetDpiForMonitor"))
			      :address (ccl::%inc-ptr (ptr-value hmonitor) (ptr-offset hmonitor))
			      :unsigned-fullword 0 ;;#_MDT_EFFECTIVE_DPI
			      :address (ptr-value (c-addr-of xdpi))
			      :address (ptr-value (c-addr-of ydpi))
			     :signed-fullword)))
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






(defun capture-win32-cursor (window)
  (declare (type os-window-mixin window))
  (clet ((clip-rect #_<RECT>))
    (let ((&clip-rect (c-addr-of clip-rect)))
      (#_GetClientRect (h window) &clip-rect)
      (let ((left (%cons-ptr
		   (ccl::%inc-ptr (ptr-value &clip-rect) (offset-of '#_<RECT> '#_left))
		   0
		   '#_<POINT*>))
	    (right (%cons-ptr
		    (ccl::%inc-ptr (ptr-value &clip-rect) (offset-of '#_<RECT> '#_right))
		    0
		    '#_<POINT*>)))
	(#_ClientToScreen (h window) left)
	(#_ClientToScreen (h window) right))
      (#_ClipCursor &clip-rect)
      (setf (captured-cursor-window *app*) window)
      (values))))

(defun disable-cursor (window)
  (setf (disabled-cursor-window *app*) window)
  (multiple-value-bind (x y) (get-win32-window-cursor-pos window)
    (setf (restore-cursor-pos-x *app*) x
	  (restore-cursor-pos-y *app*) y)
    (update-cursor-image window)
    (center-cursor-in-client-area window)
    (capture-win32-cursor window)
    (when (window-raw-mouse-motion? window)
      (enable-raw-mouse-motion window))
    (values)))

(defun update-cursor-image (window)
  (if (or (eq (window-cursor-mode window) :normal)
	  (eq (window-cursor-mode window) :captured))
      (if (window-cursor window)
	  (#_SetCursor (h (window-cursor window)))
	  (#_SetCursor (#_LoadCursor nil #_IDC_ARROW)))
      (#_SetCursor nil))
  (values))

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

(defun get-window-prop (hWnd)
  (with-lpcwstr* (str "ABSTRACT-OS")
    (ccl::%ff-call (ccl::%reference-external-entry-point (ccl:external "GetPropW"))
		   :address (ptr-value hWnd) :address (ptr-value str)
		   :unsigned-doubleword)))

(defun set-window-prop (hWnd id)
  (with-lpcwstr* (str "ABSTRACT-OS")
    (ccl::%ff-call
     (ccl::%reference-external-entry-point (ccl:external "SetPropW"))
     :address (ptr-value hWnd) :address (ptr-value str)
     :unsigned-doubleword id
     :void)))
  


(defun window-proc (hWnd uMsg wParam lParam)
  (format t "~&window-proc running")

  (block nil

    (print uMsg)
    (finish-output)
      
    (when (= (cval-value uMsg) #_WM_NCCREATE) ;; 129
	
      (when (windows-10-version-1607-or-greater?)
	  
	(let* ((cs (c-cast '#_<CREATESTRUCTW*> lParam))
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
	   
	 (when window
	   (let ((uMsg (cval-value uMsg))
		 (lParam (cval-value lParam))
		 (wParam (cval-value wParam)))

	     (locally (declare (type os-window-mixin window))

	       (with-slots (frame-action? cursor-mode high-surrogate
			    monitor auto-iconify? key-menu)
		   window

		 (case uMsg

		   (#.#_WM_NCCALCSIZE (go break)) ;; 131

		   (#.#_WM_CREATE (go break)) ; 1

		   (#.#_WM_SHOWWINDOW (go break)) ;; 24

		   (#.#_WM_WINDOWPOSCHANGING ;; 70
		    (go break))

		   (#.#_WM_ACTIVATEAPP (go break)) ;; 28

		   (#.#_WM_NCHITTEST ;; 132
		    (go break))

		   ((#.#_WM_NCACTIVATE #.#_WM_NCPAINT) ;; 133 and 134
		    (unless (currently-decorated? window)
		      (return #_TRUE))
		    (go break))

		   (#.#_WM_GETICON (go break)) ;; 127

		   (#.#_WM_ACTIVATE ;; 6
		    (go break))

		   (#.#_WM_PAINT ;; 15

		    #+NIL
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
					  (and (currently-maximized? window)
					       (/= wParam #_SIZE_RESTORED)))))

		      (when (eq (captured-cursor-window *app*) window)
			(capture-win32-cursor window))

		      (unless (eq (currently-iconified? window) iconified?)
			(let ((event (make-instance 'window-iconify-event)))
			  (handle-event window event)))

		      (unless (eq (currently-maximized? window) maximized?)
			(let ((event (make-instance (if maximized?
							'window-maximize-event
							'window-restore-event))))
			  (handle-event window event)))

		      (when (or (/= width (round (width window)))
				(/= height (round (height window))))
			(setf (width window) width
			      (height window) height)
			(let ((event (make-instance 'window-resize-event)))
			  (handle-event window event)))

		      (when (and (window-monitor window)
				 (not (eq (currently-iconified? window) iconified?)))
			(if iconified?
			    (release-win32-monitor window (window-monitor window))
			    (progn
			      (acquire-win32-monitor window (window-monitor window))
			      (fit-to-monitor window))))

		      (setf (currently-iconified? window) iconified?)
		      (setf (currently-maximized? window) maximized?)

		      (terpri)
		      (princ 'success)
		      (finish-output)

		      (return 0)))

		   (#.#_WM_MOVE ;; 3

		    (when (eq (captured-cursor-window *app*) window)
		      (capture-win32-cursor window))

		    (let ((event (make-instance 'window-move-event
						:new-x (#_GET_X_LPARAM lParam)
						:new-y (#_GET_Y_LPARAM lParam))))

		      (handle-event window event)

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

			  (let ((p-pt-min-track-size (noffi::cons-ptr
						      (ccl::%inc-ptr (noffi::ptr-value mmi)
								     (offset-of '#_<MINMAXINFO> '#_ptMinTrackSize))
						      0 '#_<POINT*>))
				(p-pt-max-track-size (noffi::cons-ptr
						      (ccl::%inc-ptr (noffi::ptr-value mmi)
								     (offset-of '#_<MINMAXINFO> '#_ptMaxTrackSize))
						      0 '#_<POINT*>))
				(p-pt-max-position (noffi::cons-ptr
						    (ccl::%inc-ptr (noffi::ptr-value mmi)
								   (offset-of '#_<MINMAXINFO> '#_ptMaxPosition))
						    0 '#_<POINT*>))
				(p-pt-max-size (noffi::cons-ptr
						(ccl::%inc-ptr (noffi::ptr-value mmi)
							       (offset-of '#_<MINMAXINFO> '#_ptMaxSize))
						0 '#_<POINT*>)))
			  
			    (unless (or (eq (window-min-width window) :dont-care)
					(eq (window-min-height window) :dont-care))
			      
			      (setf (#_.x p-pt-min-track-size) (+ (window-min-width window) (#_.right &frame) (- (#_.left &frame)))
				    (#_.y p-pt-min-track-size) (+ (window-min-height window) (#_.bottom &frame) (- (#_.top &frame)))))

			    (unless (or (eq (window-max-width window) :dont-care)
					(eq (window-max-height window) :dont-care))
			      
			      (setf (#_.x p-pt-max-track-size) (+ (window-min-width window) (#_.right &frame) (- (#_.left &frame)))
				    (#_.y p-pt-max-track-size) (+ (window-min-height window) (#_.bottom &frame) (- (#_.top &frame)))))

			    (unless (currently-decorated? window)
			      (clet ((mi #_<MONITORINFO>))
				(let ((&mi (c-addr-of mi))
				      (size (c-sizeof-type '#_<MONITORINFO>))
				      (mh (#_MonitorFromWindow (h window) #_MONITOR_DEFAULTTONEAREST)))
				  (#_memset &mi size 0)

				  (setf (#_.cbSize &mi) size)

				  (#_GetMonitorInfo mh &mi)

				  (let ((p-rc-work (noffi::cons-ptr 
						    (ccl::%inc-ptr (ptr-value &mi)
								   (offset-of '#_<MONITORINFO> '#_rcWork))
						    0 '#_<RECT*>))
					(p-rc-monitor (noffi::cons-ptr
						       (ccl::%inc-ptr (ptr-value &mi)
								      (offset-of '#_<MONITORINFO> '#_rcMonitor))
						       0 '#_<RECT*>)))

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
			(enable-win32-cursor window)
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
		    (go break)
		    (handle-event *app* (make-instance 'window-close-event
						       :window window))
		    (return 0))

		   (#.#_WM_INPUTLANGCHANGE ;; 81
		    #+NOTYET
		    (win32-update-key-names *app*)
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
						      :mods (get-key-mods *app*)
						      :syschar? (= uMsg #_WM_SYSCHAR))))
			    (handle-event window event))))
		    (when (and (= uMsg #_WM_SYSCHAR) key-menu)
		      (go break))
		    (return 0))

		   (#.#_WM_UNICHAR ;; 265
		    (go break)
		    (when (= wParam #_UNICODE_NOCHAR)
		      (return 1))

		    (let ((event (make-instance 'keyboard-event
						      :character (code-char wparam)
						      :mods (get-key-mods *app*)
						      :syschar? nil)))
			    (handle-event window event))
		    (return 0))

		   ((#.#_WM_KEYDOWN #.#_WM_SYSKEYDOWN #.#_WM_KEYUP #.#_WM_SYSKEYUP) ;; 256, 260, 257, 261
		    #+NOTYET
		    (let ((key)
			  (scancode)
			  (action (if (= 0 (logand (#_HIWORD lParam) +kf-up+)) :press :release))
			  (mods (get-key-mods *app*)))

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
		     #.#_WM_MBUTTONUP #.#_WM_XBUTTONUP))

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
		   )))))
	   
       break    
	 (finish-output))
      

      (return (#_DefWindowProcW hWnd uMsg wParam lParam)))))


(defcfun (window-proc-callback #_<LPARAM>)
	 ((hWnd #_<HWND>) (uMsg #_<UINT>) (wParam #_<WPARAM>) (lParam #_<LPARAM>))
  (let ((retval (window-proc hWnd uMsg wParam lParam)))
    retval))



(defmethod create-native-window (window &rest args
				 &key (xpos nil) (ypos nil)
				   (width 640) (height 480)
				   (title "Abstract OS")
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
    
    (with-slots (main-window-class) *app*
      (with-slots (handle monitor) window 
	
	(flet ((maybe-register-class ()
		 (unless main-window-class
		   (let ((p-cls-nm (lpcwstr* "ABSTRACT-OS")))
		     (let ((p-icon-nm (lpcwstr* "ABSTRACT-OS-ICON")))
		       (let* ((size (load-time-value (c-sizeof-type '#_<WNDCLASSEXW>))))
			 (clet ((wc #_<WNDCLASSEXW>))
			   (let ((pwc (c-addr-of wc)))

			     (setf (#_.cbSize pwc) size)
			     (setf (#_.style pwc) (logior #_CS_HREDRAW #_CS_VREDRAW #_CS_OWNDC))
			     (setf (#_.lpfnWndProc pwc) window-proc-callback)
			     (setf (#_.cbClsExtra pwc) 0)
			     (setf (#_.cbWndExtra pwc) 0)
			     (setf (#_.hInstance pwc) (#_GetModuleHandleW nil))
			     (setf (#_.hCursor pwc) (#_LoadCursorW nil #_IDC_ARROW))
			     (setf (#_.hbrBackground pwc) nil)
			     (setf (#_.lpszClassName pwc) p-cls-nm)
			     (setf (#_.hIconSm pwc) nil)

			     (setf (#_.hIcon pwc) (#_LoadImageW
						   (#_GetModuleHandleW nil)
						   p-icon-nm
						   #_IMAGE_ICON
						   0 0
						   (logior #_LR_DEFAULTSIZE #_LR_SHARED)))
		       
			     (unless (#_.hIcon pwc)
			       (setf (#_.hIcon pwc) (#_LoadImageW
						     nil
						     #_IDI_APPLICATION
						     #_IMAGE_ICON
						     0 0 (logior #_LR_DEFAULTSIZE #_LR_SHARED))))

			     (unless main-window-class
			       (when(#_GetClassInfoExW (#_GetModuleHandleW nil) p-cls-nm pwc)
				 (warn "class already exists, resetting...")
				 (unless (#_UnregisterClassW p-cls-nm (#_GetModuleHandleW nil))
				   (warn "unable to unregister class."))))
			   
			     (let ((result (#_RegisterClassExW pwc)))
			       (if (= 0 result)
				   (let ((error (#_GetLastError)))
				     (error "Win32: Failed to register window class, error ~A." error))
				   (setf main-window-class result))))))))))
	   
	       (create-window ()
		 
		 (if monitor

		     (clet ((mi #_<MONITORINFO>))
		       (let ((pmi (c-addr-of mi)))
			 (setf (#_.cbSize pmi) (load-time-value (c-sizeof-type '#_<MONITORINFO>)))
	      
			 (#_GetMonitorInfoW (slot-value monitor 'handle) pmi)
		       
			 (let ((prcMonitor (%cons-ptr
					    (ccl::%inc-ptr (ptr-value pmi) (offset-of '#_<MONITORINFO> '#_rcMonitor))
					    0
					    '#_<RECT*>)))
			 
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

			 (setf (currently-maximized? window) maximized?)
		       
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

		   (let ((wide-title (lpcwstr* title)))
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


		       
			 (setf (gethash id *id->window-table*) window)
		       
			 (let ((result #+NIL(#_CreateWindowExW 0
							       (#_MAKEINTATOM main-window-class)
							       wide-title
							       style
							       #_CW_USEDEFAULT
							       #_CW_USEDEFAULT
							       #_CW_USEDEFAULT
							       #_CW_USEDEFAULT
							       nil
							       nil
							       (#_GetModuleHandleW nil)
							       (c-cast '#_<LPVOID> &wc))
						      
				       (#_CreateWindowExW ex-style
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
		       

			   (if result
			       (progn
				 (setf handle result)

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

			     (let ((prcNormalPosition (%cons-ptr
						       (ccl::%inc-ptr (ptr-value p-wp) (offset-of '#_<WINDOWPLACEMENT> '#_rcNormalPosition))
						       0
						       '#_<RECT*>)))
			     
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

				   (let ((rc-work
					   (%cons-ptr
					    (ccl::%inc-ptr (ptr-value &mi) (offset-of '#_<MONITORINFO> '#_rcWork))
					    0
					    '#_<RECT*>)))
			       
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


	  (maybe-register-class)
	  
	  (create-window)

	  (maybe-allow-messages)

	  (setf (slot-value window 'scale-to-monitor?) scale-to-monitor?)
	  ;;(setf (slot-value window 'key-menu) key-menu)

	  (maybe-reposition-window)

	  (#_DragAcceptFiles handle #_TRUE)

	  (multiple-value-bind (window-width window-height) (get-win32-window-size window)
	  
	    (setf (slot-value window 'width) window-width
		  (slot-value window 'height) window-height))
	  
	  t)))))


(defun connect-win32 ()
  #+CCL
  (progn (ccl:open-shared-library "user32.dll")
	 (ccl:open-shared-library "dinput8.dll")
	 (ccl:open-shared-library "xinput1_4.dll")
	 (ccl:open-shared-library "xinput1_3.dll")
	 (ccl:open-shared-library "xinput9_1_0.dll")
	 (ccl:open-shared-library "xinput1_2.dll")
	 (ccl:open-shared-library "xinput1_1.dll")))


(defun init-win32-application (win32-application &rest args)
  (declare (ignorable win32-application args)))


(defun terminate-win32 ())



(defun acquire-win32-monitor (window monitor)
  (declare (type application-mixin *app*))
  (declare (ignorable window))
  (unless (slot-value *app* 'acquired-monitor-count)
    (#_SetThreadExecutionState (logior #_ES_CONTINUOUS #_ES_DISPLAY_REQUIRED))
    (clet ((mouseTrailSize #_<UINT>))
      (#_SystemParametersInfoW #_SPI_GETMOUSETRAILS 0 (c-addr-of mouseTrailSize) 0)
      (setf (slot-value *app* 'mouse-trail-size) mouseTrailSize)
      (#_SystemParametersInfoW #_SPI_SETMOUSETRAILS 0 0 0)))
  (unless (monitor-window monitor)
    #+NOTYET
    (clet ((videoMode #_<UINT>))
      (set-win32-video-mode (window-monitor window) (c-addr-of videoMode))
      (setf (window-video-mode window) videoMode)
      (input-monitor-window (window-monitor window) window))))

;;(defmethod release-monitor ((window win32-window-mixin))
  ;;(release-win32-monitor window))


(defun release-win32-monitor (window monitor)
  (declare (type os-window-mixin window))
  (declare (type application-mixin *app*))
  (unless (eq (monitor-window monitor) window)
    (return-from release-win32-monitor (values)))
  
  (decf (slot-value *app* 'acquired-monitor-count))
  
  (when (zerop (slot-value *app* 'acquired-monitor-count))
    (#_SetThreadExecutionState #_ES_CONTINUOUS)
    (clet ((mouseTrailSize #_<UINT>))
      (#_SystemParametersInfoW #_SPI_GETMOUSETRAILS 0 (c-addr-of mouseTrailSize) 0)
      (setf (slot-value *app* 'mouse-trail-size) (cval-value mouseTrailSize))))

  #+NOTYET
  (input-monitor-window monitor nil)
  #+NOTYET
  (restore-win32-video-mode monitor)
  (values))

(defun fit-to-monitor (window)
  (declare (type os-window-mixin window))
  (clet ((mi #_<MONITORINFO>))
    (let* ((&mi (c-addr-of mi))
	   (rcMonitor (c-cast '#_<RECT*>
			      (%cons-ptr
			       (ccl::%inc-ptr
				(ccl::%inc-ptr (ptr-value &mi)
					       (ptr-offset &mi))
				(load-time-value (offset-of '#_<MONITORINFO> '#_rcMonitor)))
			       0
			       '#_<RECT*>))))
      
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
  (declare (type win32-window-mixin window))
  (declare (type (or null win32-monitor-mixin) monitor))
  
  (when (eq (window-monitor window) monitor)
    (if monitor
	(when (eq (monitor-window monitor) window)
	  (acquire-win32-monitor window monitor)
	  (fit-to-monitor window))
	(clet ((rect #_<RECT>))
	  (let ((&rect (c-addr-of rect)))
	  (setf (#_.left &rect) xpos
		(#_.top   &rect) ypos
		(#_.right  &rect) (+ xpos width)
		(#_.bottom &rect) (+ xpos height))
	  
	  (if (windows-10-version-1607-or-greater?)
	      
	      (#_AdjustWindowRectExForDpi rect (get-window-style window)
					  #_FALSE (get-window-ex-style window)
					  (#_GetDpiForWindow (h window)))

	      (#_AdjustWindowRectEx rect (get-window-style window)
				    #_FALSE (get-window-ex-style window)))

	  (#_SetWindowPos (h window) #_HWND_TOP
			  (#_.left &rect) (#_.top &rect)
			  (- (#_.right &rect) (#_.left &rect))
			  (- (#_.bottom &rect) (#_.top &rect))
			  (logior #_SWP_NOCOPYBITS #_SWP_NOACTIVATE #_SWP_NOZORDER))
	  
	  (return-from set-win32-window-monitor (values))))))

  (when (window-monitor window)
    (release-win32-monitor window (window-monitor window)))

  (input-window-monitor window monitor)

  (if (window-monitor window)
      
      (clet ((mi #_<MONITORINFO>))
	(let* ((&mi (c-addr-of mi))
	       (rcMonitor (c-cast '#_<RECT*>
				  (%cons-ptr
				   (ccl::%inc-ptr
				    (ccl::%inc-ptr (ptr-value &mi)
						   (ptr-offset &mi))
				    (load-time-value (offset-of '#_<MONITORINFO> '#_rcMonitor)))
				   0
				   '#_<RECT>))))
	  (let ((flags (logior #_SWP_SHOWWINDOW #_SWP_NOACTIVATE #_SWP_NOCOPYBITS)))
	    (when (currently-decorated? window)
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
			    flags))))

      (let ((after))
	(clet ((rect #_<RECT>))
	  (let ((&rect (c-addr-of rect)))
	    (setf (#_.left &rect) (round xpos)
		  (#_.top   &rect) (round ypos)
		  (#_.right  &rect) (round (+ xpos width))
		  (#_.bottom &rect) (round (+ ypos height)))
	    (let ((style (#_GetWindowLongW (h window) #_GL_STYLE))
		  (flags (logior #_SWP_NOACTIVATE #_SWP_NOCOPYBITS)))
	    
	      (when (currently-decorated? window)
		(let ((style (logand style (lognot #_WS_POPUP))))
		  (setq style (logior style (get-window-style window)))
		  (#_SetWindowLongW (h window) #_GWL_STYLE style)
		
		  (setq flags (logior flags #_SWP_FRAMECHANGED))))
	    
	      (if (currently-floating? window)
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
				      
	      

     
  
  
  


(defun create-win32-window (window &rest args
			    &key (visible? t)
			      (focused? t)
			      (mouse-passthrough? nil)
			    &allow-other-keys)
  (declare (type os-window-mixin window))
  (if (apply #'create-native-window window args)
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
	window)
      (error "Win32: failed to create native window.")))

(defun wait-win32-events (app)
  (clet ((msg #_<MSG>))
    (let* ((&msg (c-addr-of msg))
	   (result)
	   (window))

      (terpri)
      (princ 1)
      (finish-output)

      (when (= (setq result (#_GetMessage &msg nil 0 0)) -1)
	(error "GetMessage returned -1"))

      (terpri)
      (princ 1.5)
      (finish-output)
      
      (unless (zerop result)

	(terpri)
	(princ 2)
	(finish-output)

	(if (= (#_.message &msg) #_WM_QUIT)

	    (progn
	      (terpri)
	      (princ 6)
	      (finish-output)
		     
	      (setq window (application-window-list-head app))

	      (loop while window
		    do (handle-event window (make-instance 'window-close-event))
		       (setq window (window-next window))))

	    (progn
	      (terpri)
	      (princ 3)
	      (finish-output)
	      ;;(#_TranslateMessage &msg)
	      (#_DispatchMessageW &msg)
	      
	      (terpri)
	      (princ 4)
	      (finish-output)))))))

(defun poll-win32-events (app)
  (clet ((msg #_<MSG>))
    (let ((&msg (c-addr-of msg))
	  (window))

      (loop until (progn
		    (terpri)
		    (princ 1)
		    (finish-output)
		    (zerop (#_PeekMessageW &msg nil 0 0 #_PM_REMOVE)))

	    do (terpri)
	       (princ 2)
	       (finish-output)

	       (if (= (#_.message &msg) #_WM_QUIT)

		   (progn
		     (terpri)
		     (princ 6)
		     (finish-output)
		     
		     (setq window (application-window-list-head app))

		     (loop while window
			   do (handle-event window (make-instance 'window-close-event))
			      (setq window (window-next window))))

		   (progn
		     (terpri)
		     (princ 3)
		     (finish-output)
		     ;;(#_TranslateMessage &msg)
		     (#_DispatchMessageW &msg)

		     (terpri)
		     (princ 4)
		     (finish-output))))
      (terpri)
      (princ 5)
      (finish-output))))
      

(defun test ()
  (ccl::call-in-initial-process
   (lambda ()
     (unless *app* (make-instance 'application-mixin))
     (setq w (make-instance 'os-window))
     (create-win32-window w)
     (run *app*))))
