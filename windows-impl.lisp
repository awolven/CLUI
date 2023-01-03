(in-package :abstract-os)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (noffi::noffi-syntax t)
  )

(noffi::noffi-syntax t)
;;#_{#include "dependencies.i"}


(defconstant +WM_COPYGLOBALDATA+ #x0049)




(defmacro with-lpcwstr* ((var string) &body body)
  (let ((sap-sym (gensym)))
    `(ccl::with-native-utf-16-cstr (,sap-sym ,string)
       (let ((,var (%cons-ptr ,sap-sym 0 '#_<LPCWSTR*>)))
	 ,@body))))


(defmacro with-win32-monitorinfo ((mi-var rc-monitor-var) &body body)
  `(with-stack-allocated-structure (,mi-var '#_<MONITORINFO> '#_<MONITORINFO*>)
     (setf (#_.cbSize ,mi-var) (load-time-value (c-sizeof-type '#_<MONITORINFO>)))
     (let ((,rc-monitor-var (c-cast '#_<RECT*>
				    (%cons-ptr
				     (ccl::%inc-ptr
				      (ccl::%inc-ptr (ptr-value ,mi-var)
						     (ptr-offset ,mi-var))
				      (load-time-value (offset-of '#_<MONITORINFO> '#_rcMonitor)))
				     0
				     '#_<RECT>))))
       ,@body)))
      


(defmacro with-win32-rect ((var) &body body)
  `(with-stack-allocated-structure (,var '#_<RECT> '#_<RECT*>)
     ,@body))






(defmethod initialize-instance :after ((app win32-application-mixin) &rest initargs)
  (apply #'init-win32-application app initargs))





(defvar *window-count* 0)

(defun gen-window-id ()
  (incf *window-count*))

(defvar *id->window-table* (make-hash-table :test #'equalp))








(defun get-window-style (win32-window)
  (declare (type essential-os-window-mixin win32-window))
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


(defun get-window-ex-style (win32-window)
  (let ((style #_WS_EX_APPWINDOW))

    (when (or (slot-value win32-window 'monitor) (slot-value win32-window 'floating?))
      (setq style (logior style #_WS_POPUP)))

    style))
      

(defun windows-version-or-greater? (major minor sp)
  (with-stack-allocated-structure
      (posvi '#_<OSVERSIONINFOEXW> '#_<OSVERSIONINFOEXW*>)
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
			   :unsigned-fullword))))))


(defun windows-7-or-greater? ()
  (windows-version-or-greater? (#_HIBYTE #__WIN32_WINNT_WIN7) (#_LOBYTE #__WIN32_WINNT_WIN7) 0))


(defun windows-8.1-or-greater? ()
  #+NIL(windows-version-or-greater? (#_HIBYTE #__WIN32_WINNT_WIN81) (#_LOBYTE #__WIN32_WINNT_WIN81) 0)
  t)


(defun windows-10-build-or-later? (build)
  (with-stack-allocated-structure
      (posvi '#_<OSVERSIONINFOEXW> '#_<OSVERSIONINFOEXW*>)
    ;;(clet ((csd-version #_<WCHAR[]>))
    (setf (#_.dwOSVersionInfoSize posvi) (c-sizeof-type '#_<OSVERSIONINFOEXW>)
	  (#_.dwMajorVersion posvi) 10
	  (#_.dwMinorVersion posvi) 0
	  (#_.dwBuildNumber posvi) build
	  (#_.dwPlatformId posvi) 0
	  ;;(#_.szCSDVersion posvi) csd-version
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
			   :unsigned-fullword))))))


(defun windows-10-version-1607-or-greater? ()
  (windows-10-build-or-later? 14393))


(defun windows-10-version-1703-or-greater? ()
  (windows-10-build-or-later? 15063))



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
      



(defun get-hmonitor-content-scale (handle &optional (xscale nil) (yscale nil))
  (if (windows-8.1-or-greater?)
      (multiple-value-bind (xdpi ydpi) (get-dpi-for-monitor handle)
	(unless xdpi
	  (error "Win32: Failed to query monitor DPI."))
	(setq xdpi (if xscale
		       (/ xdpi #_USER_DEFAULT_SCREEN_DPI)
		       xdpi))
	(setq ydpi (if yscale
		       (/ ydpi #_USER_DEFAULT_SCREEN_DPI)
		       ydpi))
	(values xdpi ydpi))
      (let* ((dc (#_GetDC nil)))
	(unwind-protect
	     (clet ((xdpi #_<UINT>)
		    (ydpi #_<UINT>))
	       (setf (cval-value xdpi) (#_GetDeviceCaps dc #_LOGPIXELSX)
		     (cval-value ydpi) (#_GetDeviceCaps dc #_LOGPIXELSY))
	       (values (if xscale
			   (/ (cval-value xdpi) #_USER_DEFAULT_SCREEN_DPI)
			   (cval-value xdpi))
		       (if yscale
			   (/ (cval-value ydpi) #_USER_DEFAULT_SCREEN_DPI)
			   (cval-value ydpi))))
	  (#_ReleaseDC nil dc)))))



(defmethod get-window-size ((window win32-window-mixin))
  (get-win32-window-size window))


(defun get-win32-window-size (win32-window)
  (with-win32-rect (&area)
    (when (#_GetClientRect (slot-value win32-window 'handle) &area)

      (values (#_.right &area)
	      (#_.bottom &area)))))


(defmethod capture-cursor ((window win32-window-mixin))
  (capture-win32-cursor window))


(defun capture-win32-cursor (window)
  (declare (type win32-window-mixin window))
  (with-win32-rect (clip-rect)
    (#_GetClientRect (slot-value window 'handle) clip-rect)
    (let ((left (%cons-ptr
		 (ccl::%inc-ptr (ptr-value clip-rect) (offset-of '#_<RECT> '#_left))
		 0
		 '#_<POINT*>))
	  (right (%cons-ptr
		 (ccl::%inc-ptr (ptr-value clip-rect) (offset-of '#_<RECT> '#_right))
		 0
		 '#_<POINT*>)))
      (#_ClientToScreen (slot-value window 'handle) left)
      (#_ClientToScreen (slot-value window 'handle) right))
    (#_ClipCursor clip-rect)
    (setf (slot-value *app* 'captured-cursor-window) window)
    (values)))


(defun input-window-size (window width height)
  (if (null window)
      (warn "window was nil in input-window-size")
      (locally
	  (declare (type essential-os-window-mixin window))
	(when (slot-value window 'size-callback)
	  (funcall (slot-value window 'size-callback)
		   window width height))))
  (values))


(defun input-window-iconify (window iconified?)
  (if (null window)
      (warn "window was nil in input-window-iconify")
      (locally
	  (declare (type essential-os-window-mixin window))
	(when (slot-value window 'iconify-callback)
	  (funcall (slot-value window 'iconify-callback)
		   window iconified?))))
  (values))


(defun input-window-maximize (window maximized?)
  (if (null window)
      (warn "window was nil in input-window-maximize")
      (locally
	  (declare (type essential-os-window-mixin window))
	(when (slot-value window 'maximize-callback)
	  (funcall (slot-value window 'maximize-callback)
		   window maximized?))))
  (values))

#_{
typedef struct wndconfig {
int maximized;
int scaletomonitor;
int decorated;
} wndconfig;
}


(defun window-proc (hWnd uMsg wParam lParam)
  (format t "~&window-proc running")

  (block nil

    (let ((id (with-lpcwstr* (str "ABSTRACT-OS")
		(ccl::%ff-call (ccl::%reference-external-entry-point (ccl:external "GetPropW"))
			       :address (ptr-value hWnd) :address (ptr-value str)
			       :unsigned-doubleword)))
	  (window nil))

      (let ((*print-base* 16))
	(print uMsg))

      (if id
	  (let ()
	    (setq window (gethash id *id->window-table*))
	    (unless window
	      (if (= (cval-value uMsg) #_WM_NCCREATE)
		  (when (windows-10-version-1607-or-greater?)
		    (let* ((cs (c-cast '#_<CREATESTRUCTW*> lParam))
			   (wndconfig (c-cast '#_<wndconfig*> (#_.lpCreateParams cs)))
			   (scale-to-monitor (#_.scaletomonitor wndconfig)))
		      (unless (= 0 scale-to-monitor)
			(#_EnableNonClientDpiScaling hWnd))))		      
		  (warn "could not find lisp window object for id ~A." id))))
	  (warn "could not get window id."))


      (tagbody
	 (when window

	   (let ((uMsg (cval-value uMsg))
		 (lParam (cval-value lParam))
		 (wParam (cval-value wParam))
		 )

	     (locally (declare (type essential-os-window-mixin window))

	       (with-slots (frame-action? cursor-mode high-surrogate
			    monitor auto-iconify? keymenu)
		   window
	       
		 (case uMsg

		   (#_WM_MOUSEACTIVATE
		    (when (= (#_HIWORD lParam) #_WM_LBUTTONDOWN)
		      (when (/= (#_LOWORD lParam) #_HTCLIENT)
			(setf frame-action? t)))
		    (go break))

		   (#_WM_SIZE
		    (let ((width (#_LOWORD lParam))
			  (height (#_HIWORD lParam))
			  (iconified? (= wParam #_SIZE_MINIMIZED))
			  (maximized? (or (= wParam #_SIZE_MAXIMIZED)
					  (and (slot-value window 'maximized?)
					       (/= wParam #_SIZE_RESTORED)))))

		      (when (eq (slot-value *app* 'captured-cursor-window) window)
			(capture-cursor window))

		      (unless (eq (slot-value window 'iconified?) iconified?)
			(input-window-iconify window iconified?))

		      (unless (eq (slot-value window 'maximized?) maximized?)
			(input-window-maximize window maximized?))

		      (when (or (/= width (slot-value window 'width))
				(/= height (slot-value window 'height)))
			(setf (slot-value window 'width) width
			      (slot-value window 'height) height)
			(input-window-size window width height))

		      (when (and (slot-value window 'monitor)
				 (/= (slot-value window 'iconfied?) iconified?))
			(if iconified?
			    (release-monitor window)
			    (progn
			      (acquire-monitor window)
			      (fit-to-monitor window))))

		      (return 0)))
			
		    
			
			  

		   #+NOTYET
		   (#_WM_CAPTURECHANGED
		    (when (and (= lParam 0) frame-action?)
		      (if (eq (slot-value window 'cursor-mode) :disabled)
			  (disable-cursor window)
			  (when (eq cursor-mode :captured)
			    (capture-cursor window)))
		      (setf (slot-value window 'frame-action?) nil))
		    (go break))

		   #+NOTYET
		   (#_WM_SETFOCUS
		    (input-window-focus window t)
		    (when frame-action?
		      (go break))
		    (if (eq cursor-mode :disabled)
			(disable-cursor window)
			(when (eq cursor-mode :captured)
			  (capture-cursor window)))
		    (return 0))

		   #+NOTYET
		   (#_WM_KILLFOCUS
		    (if (eq cursor-mode :disabled)
			(enable-cursor window)
			(when (eq cursor-mode :captured)
			  (release-cursor window)))

		    (when (and monitor auto-iconify?)
		      (iconify-win32-window window))

		    (input-window-focus window nil)
		    (return 0))

		   #+NOYET
		   (#_WM_SYSCOMMAND
		    (case (logand wParam #xfff0)
		    
		      ((#_SC_SCREENSAVE #_SC_MONITORPOWER)

		       (if monitor
			   (return 0)
			   (go break)))

		      (#_SC_KEYMENU
		       (unless keymenu
			 (return 0))
		       (go break)))
		    (go break))

		   #+NOTYET
		   (#_WM_CLOSE
		    (input-window-close-request window)
		    (return 0))

		   #+NOTYET
		   (#_WM_INPUTLANGCHANGE
		    (win32-update-key-names)
		    (go break))

		   #+NOTYET
		   ((#_WM_CHAR #_WM_SYSCHAR)
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
			  (input-char window codepoint (get-key-mods) (/= uMsg #_WM_SYSCHAR))))
		    (when (and (= uMsg #_WM_SYSCHAR) keymenu)
		      (go break))
		    (return 0))

		   #+NOTYET
		   (#_WM_UNICHAR
		    (when (= wParam #_UNICODE_NOCHAR)
		      (return 1))

		    (input-char window wParam (get-key-mods) t)
		    (return 0))

		   #+NOTYET
		   ((#_WM_KEYDOWN #_WM_SYSKEYDOWN #_WM_KEYUP #_WM_SYSKEYUP)
		    (let ((key)
			  (scancode)
			  (action (= 0 (logand (#_HIWORD lParam) +kf-up+)) :press :release)
			  (mods (get-key-mods)))

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
			      (input-key window key scancode action mods)))
		      (go break)))

		   #+NOTYET
		   ((#_WM_LBUTTONDOWN
		     #_WM_RBUTTONDOWN #_WM_MBUTTONDOWN #_WM_XBUTTONDOWN
		     #_WM_LBUTTONUP #_WM_RBUTTONUP
		     #_WM_MBUTTONUP #_WM_XBUTTONUP)))))))
	 
       break    
	 (finish-output))

      (#_DefWindowProcW hWnd uMsg wParam lParam))))


(defcfun (window-proc-callback #_<LPARAM>)
	 ((hWnd #_<HWND>) (uMsg #_<UINT>) (wParam #_<WPARAM>) (lParam #_<LPARAM>))
  (window-proc hWnd uMsg wParam lParam))



(defmethod create-native-window ((application win32-application-mixin) (window win32-window-mixin)
				 &key (xpos nil) (ypos nil)
				   (width 640) (height 480)
				   (maximized? nil) (title "Abstract OS") (scale-to-monitor? t)
				   (decorated? nil) (key-menu nil)
				 &allow-other-keys)
  
  (let* ((style (get-window-style window))
	 (ex-style (get-window-ex-style window))
	 (frame-x)
	 (frame-y)
	 (frame-width)
	 (frame-height))
    
    (with-slots (main-window-class) application
      (with-slots (id handle monitor) window
	
	(flet ((maybe-register-class ()
		 (unless main-window-class
		   (with-lpcwstr* (p-cls-nm "ABSTRACT-OS")
		     (with-lpcwstr* (p-icon-nm "ABSTRACT-OS-ICON")
		       (let* ((size (load-time-value (c-sizeof-type '#_<WNDCLASSEXW>))))
			 (with-stack-allocated-structure (pwc '#_<WNDCLASSEXW> '#_<WNDCLASSEXW*>)

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
				 (setf main-window-class result)))))))))
	   
	       (create-window ()
		 
		 (if monitor

		     (with-stack-allocated-structure (pmi '#_<MONITORINFO> '#_<MONITORINFO*>)
		       (setf (#_.cbSize pmi) (load-time-value (c-sizeof-type '#_<MONITORINFO>)))
	      
		       (#_GetMonitorInfoW (slot-value monitor 'handle) pmi)
		       
		       (let ((prcMonitor (copy-ptr pmi)))
			 (setf (ptr-offset prcMonitor) (offset-of '#_<MONITORINFO> '#_rcMonitor))
			 
			 (setq frame-x (#_.left prcMonitor))
			 (setq frame-y (#_.top prcMonitor))
			 (setq frame-width (- (#_.right prcMonitor) (#_.left prcMonitor)))
			 (setq frame-height (- (#_.bottom prcMonitor) (#_.top prcMonitor)))))

		     (with-win32-rect (rect)

		       (setf (#_.left rect) 0.0f0
			     (#_.top   rect) 0.0f0
			     (#_.right  rect) (coerce width 'single-float)
			     (#_.bottom rect) (coerce height 'single-float))

		       (setf (slot-value window 'maximized?) maximized?)
		       
		       (when maximized?
			 (setq style (logior style #_WS_MAXIMIZE)))

		       (#_AdjustWindowRectEx rect style #_FALSE ex-style)

		       (let ((left (#_.left rect))
			     (top (#_.top rect))
			     (right (#_.right rect))
			     (bottom (#_.bottom rect)))

			 (if (and (null xpos) (null ypos))
		  
			     (progn 
			       (setq frame-x #_CW_USEDEFAULT)
			       (setq frame-y #_CW_USEDEFAULT))
		  
			     (progn
			       (setq frame-x (+ xpos left))
			       (setq frame-y (+ ypos top))))

			 (setq frame-width (- right left))
			 (setq frame-height (- bottom top)))
		       ))

		 (with-lpcwstr* (wide-title title)
		   (with-stack-allocated-structure (wc '#_<wndconfig> '#_<wndconfig*>)
		     (setf (#_.maximized wc) (if maximized? 1 0)
			   (#_.scaletomonitor wc) (if scale-to-monitor? 1 0)
			   (#_.decorated wc) (if decorated? 1 0))

		     
		       (print frame-x)
		       (print frame-y)
		       (print frame-width)
		     (print frame-height)
		     (print main-window-class)
			   
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
						      (c-cast '#_<LPVOID> wc))
						      
				   (#_CreateWindowExW ex-style
						      (#_MAKEINTATOM main-window-class)
						      wide-title
						      style
						      frame-x frame-y
						      frame-width frame-height
						      nil ;; no parent window
						      nil ;; no window menu
						      (#_GetModuleHandleW nil)
						      (c-cast '#_<LPVOID> wc)
						      )))

		       (if result
			   (progn
			     (setf handle result)
			     (setf (gethash id *id->window-table*) window)
			     (with-lpcwstr* (str "SILICA")
			       (ccl::%ff-call
				(ccl::%reference-external-entry-point (ccl:external "SetPropW"))
				:address (ptr-value handle) :address (ptr-value str)
				:unsigned-doubleword id
				:void))
			     (#_SetWindowPos handle #_HWND_TOP
					     frame-x
					     frame-y
					     frame-width
					     frame-height
					     (logior #_SWP_NOACTIVATE #_SWP_NOZORDER)))
			   (error "Win32: Failed to create window."))))))
	       
	       (maybe-filter-messages ()
		 (when (windows-7-or-greater?)
		   (#_ChangeWindowMessageFilterEx handle #_WM_DROPFILES #_MSGFLT_ALLOW nil)
		   (#_ChangeWindowMessageFilterEx handle #_WM_COPYDATA #_MSGFLT_ALLOW nil)
		   (#_ChangeWindowMessageFilterEx handle +WM_COPYGLOBALDATA+ #_MSGFLT_ALLOW nil)))

	       (maybe-reposition-window ()

		 (unless monitor

		   (with-win32-rect (rect)
		     (setf (#_.left rect) 0.0f0
			   (#_.top rect) 0.0f0
			   (#_.right rect) (coerce width 'single-float)
			   (#_.bottom rect) (coerce height 'single-float))

		     (with-stack-allocated-structure (p-wp '#_<WINDOWPLACEMENT> '#_<WINDOWPLACEMENT*>)
		       (setf (#_.length p-wp) (load-time-value (c-sizeof-type '#_<WINDOWPLACEMENT>)))
		     
		       (let ((mh (#_MonitorFromWindow handle #_MONITOR_DEFAULTTONEAREST)))
			   
			 (when scale-to-monitor?
		      
			   (multiple-value-bind (xscale yscale)
			       (get-hmonitor-content-scale mh)
			
			     (when (and (> xscale 0.0f0) (> yscale 0.0f0))
			  
			       (setf (#_.right rect) (* (#_.right rect) xscale))
			       (setf (#_.bottom rect) (* (#_.bottom rect) yscale)))))
		    
			 (if (windows-10-version-1607-or-greater?)
			     (#_AdjustWindowRectExForDpi rect style #_FALSE ex-style
							 (#_GetDpiForWindow handle))
			       
			     (#_AdjustWindowRectEx rect style #_FALSE ex-style))

			 (#_GetWindowPlacement handle p-wp)

			 (let ((prcNormalPosition (c-cast '#_<RECT*> (copy-ptr p-wp))))

			   (setf (ptr-offset prcNormalPosition) (offset-of '#_<WINDOWPLACEMENT>
									   '#_rcNormalPosition))
			     
			   (#_OffsetRect rect
					 (- (#_.left prcNormalPosition) (#_.left rect))
					 (- (#_.top prcNormalPosition)  (#_.top rect)))

			   (setf (#_.left prcNormalPosition) (#_.left rect))
			   (setf (#_.top prcNormalPosition) (#_.top rect))
			   (setf (#_.right prcNormalPosition) (#_.right rect))
			   (setf (#_.bottom prcNormalPosition) (#_.bottom rect))
		    			   
			   (setf (#_.showCmd p-wp) #_SW_HIDE)

			   (#_SetWindowPlacement handle p-wp))
		       
			 (when (and maximized? (not decorated?))

			   (with-stack-allocated-structure (mi '#_<MONITORINFO> '#_<MONITORINFO*>)
			     (setf (#_.cbSize mi) (load-time-value (c-sizeof-type '#_<MONITORINFO>)))
			   
			     (#_GetMonitorInfoW mh mi)

			     (let ((rc-work
				     (%cons-ptr
				      (ccl::%inc-ptr (ptr-value mi) (offset-of '#_<MONITORINFO> '#_rcWork))
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
					       (logior #_SWP_NOACTIVATE #_SWP_NOZORDER)))))))))))


	  (maybe-register-class)
	  
	  (create-window)

	  (maybe-filter-messages)

	  (setf (slot-value window 'scale-to-monitor?) scale-to-monitor?)
	  (setf (slot-value window 'key-menu) key-menu)

	  (maybe-reposition-window)

	  #+NIL(#_DragAcceptFile handle #_TRUE)

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


(defmethod acquire-monitor ((window win32-window-mixin))
  (acquire-win32-monitor window))



(defun acquire-win32-monitor (window)
  (declare (type win32-application-mixin *app*))
  (declare (type essential-os-window-mixin window))
  (unless (slot-value *app* 'acquired-monitor-count)
    (#_SetThreadExecutionState (logior #_ES_CONTINUOUS #_ES_DISPLAY_REQUIRED))
    (clet ((mouseTrailSize #_<UINT>))
      (#_SystemParametersInfoW #_SPI_GETMOUSETRAILS 0 (c-addr-of mouseTrailSize) 0)
      (setf (slot-value *app* 'mouse-trail-size) mouseTrailSize)
      (#_SystemParametersInfoW #_SPI_SETMOUSETRAILS 0 0 0)))
  (unless (monitor-window (window-monitor window))
    #+NOTYET
    (clet ((videoMode #_<UINT>))
      (set-win32-video-mode (window-monitor window) (c-addr-of videoMode))
      (setf (window-video-mode window) videoMode)
      (input-monitor-window (window-monitor window) window))))

(defmethod release-monitor ((window win32-window-mixin))
  (release-win32-monitor window))


(defun release-win32-monitor (window)
  (declare (type essential-os-window-mixin window))
  (declare (type win32-application-mixin *app*))
  (unless (eq (slot-value (slot-value window 'monitor) 'window) window)
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
  (declare (type essential-os-window-mixin window))
  (with-win32-monitorinfo (&mi rcMonitor)
    (#_GetMonitorInfoW (window-handle window) &mi)
    (#_SetWindowPos (window-handle window) #_HWND_TOPMOST
		    (#_.left rcMonitor)
		    (#_.top rcMonitor)
		    (- (#_.right rcMonitor) (#_.left rcMonitor))
		    (- (#_.bottom rcMonitor) (#_.top rcMonitor))
		    (logior #_SWP_NOZORDER #_SWP_NOACTIVATE #_SWP_NOCOPYBITS)))
  (values))
  

(defun set-win32-window-monitor (window monitor xpos ypos width height refresh-rate)
  (declare (ignorable refresh-rate))
  (declare (type win32-window-mixin window))
  (declare (type (or null win32-monitor-mixin) monitor))
  
  (when (eq (window-monitor window) monitor)
    (if monitor
	(when (eq (monitor-window monitor) window)
	  (acquire-monitor window)
	  (fit-to-monitor window))
	(with-win32-rect (rect)
	  (setf (#_.left rect) xpos
		(#_.top   rect) ypos
		(#_.right  rect) (+ xpos width)
		(#_.bottom rect) (+ xpos height))
	  
	  (if (windows-10-version-1607-or-greater?)
	      
	      (#_AdjustWindowRectExForDpi rect (get-window-style window)
					  #_FALSE (get-window-ex-style window)
					  (#_GetDpiForWindow (window-handle window)))

	      (#_AdjustWindowRectEx rect (get-window-style window)
				    #_FALSE (get-window-ex-style window)))

	  (#_SetWindowPos (window-handle window) #_HWND_TOP
			  (#_.left rect) (#_.top rect)
			  (- (#_.right rect) (#_.left rect))
			  (- (#_.bottom rect) (#_.top rect))
			  (logior #_SWP_NOCOPYBITS #_SWP_NOACTIVATE #_SWP_NOZORDER))
	  
	  (return-from set-win32-window-monitor (values)))))

  (when (window-monitor window)
    (release-monitor window))

  (input-window-monitor window monitor)

  (if (window-monitor window)
      (with-win32-monitorinfo (&mi rcMonitor)
	(let ((flags (logior #_SWP_SHOWWINDOW #_SWP_NOACTIVATE #_SWP_NOCOPYBITS)))
	  (when (decorated? window)
	    (let ((style (#_GetWindowLongW (window-handle window) #_GWL_STYLE)))
	      (setq style (logand style (lognot #_WS_OVERLAPPEDWINDOW)))
	      (setq style (logior style (get-window-style window)))
	      (#_SetWindowLongW (window-handle window) #_GWL_STYLE style)
	      (setq flags (logior flags #_SWP_FRAMECHANGED))))
	
	  (acquire-monitor window)

	  (#_GetMonitorInfoW (window-handle (window-monitor window)) &mi)
	  (#_SetWindowPos (window-handle window) #_HWND_TOPMOST
			  (#_.left rcMonitor)
			  (#_.top rcMonitor)
			  (- (#_.right rcMonitor) (#_.left rcMonitor))
			  (- (#_.bottom rcMonitor) (#_.top rcMonitor))
			  flags)))

      (let ((after))
	(with-win32-rect (rect)
	  (setf (#_.left rect) xpos
		(#_.top   rect) ypos
		(#_.right  rect) (+ xpos width)
		(#_.bottom rect) (+ ypos height))
	  (let ((style (#_GetWindowLongW (window-handle window) #_GL_STYLE))
		(flags (logior #_SWP_NOACTIVATE #_SWP_NOCOPYBITS)))
	    
	    (when (decorated? window)
	      (let ((style (logand style (lognot #_WS_POPUP))))
		(setq style (logior style (get-window-style window)))
		(#_SetWindowLongW (window-handle window) #_GWL_STYLE style)
		
		(setq flags (logior flags #_SWP_FRAMECHANGED))))
	    
	    (if (floating? window)
		(setq after #_HWND_TOPMOST)
		(setq after #_HWND_NOTOPMOST))

	    (if (windows-10-version-1607-or-greater?)
		(#_AdjustWindowRectExForDpi rect (get-window-style window)
					    #_FALSE (get-window-ex-style window)
					    (#_GetDpiForWindow (window-handle window)))
		(#_AdjustWindowRectEx rect (get-window-style window)
				      #_FALSE (get-window-ex-style window)))

	    (#_SetWindowPos (window-handle window) after
			    (#_.left rect) (#_.top rect)
			    (- (#_.right rect) (#_.left rect))
			    (- (#_.bottom rect) (#_.top rect))
			    flags)))))
  (values))
	
(defun set-win32-window-mouse-passthrough (window enabled?)
  (declare (type win32-window-mixin window))
  (clet ((key #_<COLORREF> 0)
	 (alpha #_<BYTE> 0)
	 (flags #_<DWORD> 0))
    (let ((ex-style (#_GetWindowLongW (window-handle window) #_GWL_EXSTYLE)))

      (unless (= 0 (logand ex-style #_WS_EX_LAYERED))
	(#_GetLayeredWindowAttributes (window-handle window)
				      (c-addr-of key)
				      (c-addr-of alpha)
				      (c-addr-of flags)))

      (if enabled?
	  (setq ex-style (logior ex-style #_WS_EX_TRANSPARENT #_WS_EX_LAYERED))
	  (progn
	    (setq ex-style (logand ex-style (lognot #_WS_EX_TRANSPARENT)))

	    (unless (= 0 (logand ex-style #_WS_EX_LAYERED))
	      (when (= 0 (logand flags #_LWA_ALPHA))
		(setq ex-style (logand ex-style (lognot #_WS_EX_LAYERED)))))))

      (#_SetWindowLongW (window-handle window) #_GWL_EXSTYLE ex-style)

      (when enabled?
	(#_SetLayeredWindowAttributes (window-handle window)
				      nil
				      (c-addr-of alpha)
				      (c-addr-of flags)))
      (values))))
				      
	      

     
  
  
  


(defun create-win32-window (win32-application concrete-class &rest args
			    &key (visible? t) (focused? t)
			      (mouse-passthrough? nil)
			    &allow-other-keys)
  (let ((window (apply #'make-instance concrete-class args)))
    (declare (type essential-os-window-mixin window))
    (if (apply #'create-native-window win32-application window args)
	(progn
	  (when mouse-passthrough?
	    (set-win32-window-mouse-passthrough window t))
	  (if (slot-value window 'monitor)
	      (progn
		(show-win32-window window)
		(focus-win32-window window)
		(acquire-monitor window)
		(fit-to-monitor window))
	      (when visible?
		(show-win32-window window)
		(when focused?
		  (focus-win32-window window))))
	  window)
	(error "Win32: failed to create native window."))))


(defun destroy-win32-window (win32-window)
  (declare (ignorable win32-window)))


(defun win32-window-title (win32-window)
  (declare (ignorable win32-window)))


(defun (setf win32-window-title) (string win32-window)
  (declare (ignorable string win32-window)))


(defun (setf win32-window-icon) (images win32-window)
  (declare (ignorable images win32-window)))


(defun win32-window-pos (win32-window)
  (declare (ignorable win32-window)))


(defun (setf win32-window-pos) (pos win32-window)
  (declare (ignorable win32-window pos)))


(defun win32-window-size (win32-window)
  (declare (ignorable win32-window)))


(defun (setf win32-window-size) (size win32-window)
  (declare (ignorable size win32-window)))


(defun (setf win32-window-size-limits) (rect win32-window)
  (declare (ignorable rect win32-window)))


(defun (setf win32-window-aspect-ratio) (ratio win32-window)
  (declare (ignorable ratio win32-window)))


(defun win32-window-frame-size (win32-window)
  (declare (ignorable win32-window)))


(defun win32-window-content-scale (win32-window)
  (declare (ignorable win32-window)))


(defun iconify-win32-window (win32-window)
  (declare (ignorable win32-window)))


(defun restore-win32-window (win32-window)
  (declare (ignorable win32-window)))


(defun maximize-win32-window (win32-window)
  (declare (ignorable win32-window)))


(defun show-win32-window (window)
  (declare (type win32-window-mixin window))
  (#_ShowWindow (slot-value window 'handle) #_SW_SHOW))


(defun hide-win32-window (win32-window)
  (declare (ignorable win32-window)))


(defun focus-win32-window (window)
  (declare (type win32-window-mixin window))
  (let ((handle (slot-value window 'handle)))
    (#_BringWindowToTop handle)
    (#_SetForegroundWindow handle)
    (#_SetFocus handle)))


(defun request-win32-window-attention (win32-window)
  (declare (ignorable win32-window)))




(defun (setf win32-window-monitor) (win32-monitor win32-window)
  (declare (ignorable win32-monitor win32-window)))


(defun win32-window-focused? (win32-window)
  (declare (ignorable win32-window)))


(defun win32-window-iconified? (win32-window)
  (declare (ignorable win32-window)))


(defun win32-window-visible? (win32-window)
  (declare (ignorable win32-window)))


(defun win32-window-maximized? (win32-window)
  (declare (ignorable win32-window)))


(defun win32-window-hovered? (win32-window)
  (declare (ignorable win32-window)))


(defun (setf win32-window-resizable?) (value win32-window)
  (declare (ignorable win32-window value)))


(defun (setf win32-window-decorated?) (value win32-window)
  (declare (ignorable win32-window value)))


(defun (setf win32-window-floating?) (value win32-window)
  (declare (ignorable win32-window value)))


(defun win32-window-opacity (win32-window)
  (declare (ignorable win32-window)))


(defun (setf win32-window-opacity) (opacity win32-window)
  (declare (ignorable win32-window opacity)))


(defun (setf win32-window-mouse-passthrough) (enabled? win32-window)
  (declare (ignorable win32-window enabled?)))


(defun win32-window-poll-events (win32-window)
  (declare (ignorable win32-window)))


(defun win32-window-wait-events (win32-window)
  (declare (ignorable win32-window)))


(defun win32-window-wait-events-timeout (win32-window timeout)
  (declare (ignorable timeout win32-window)))


(defun win32-window-post-empty-event (win32-window)
  (declare (ignorable win32-window)))


(defun win32-cursor-pos (win32-window)
  (declare (ignorable win32-window)))


(defun (setf win32-cursor-pos) (pos win32-window)
  (declare (ignorable pos win32-window)))


(defun (setf win32-cursor-mode) (mode win32-window)
  (declare (ignorable mode win32-window)))


(defun create-os-window (&rest args)
  (apply #'create-win32-window args))


(defun destroy-os-window (os-window)
  (destroy-win32-window os-window))


(defun os-window-title (os-window)
  (win32-window-title os-window))


(defun (setf os-window-title) (string os-window)
  (setf (win32-window-title os-window) string))


(defun (setf os-window-icon) (image os-window)
  (setf (win32-window-icon os-window) (list image)))


(defun os-window-pos (os-window)
  (win32-window-pos os-window))


(defun (setf os-window-pos) (pos os-window)
  (setf (win32-window-pos os-window) pos))


(defun os-window-size (os-window)
  (win32-window-size os-window))


(defun (setf os-window-size) (size os-window)
  (setf (win32-window-size os-window) size))


(defun (setf os-window-size-limits) (rect os-window)
  (setf (win32-window-size-limits os-window) rect))


(defun (setf os-window-aspect-ratio) (ratio os-window)
  (setf (win32-window-aspect-ratio os-window) ratio))


(defun os-window-frame-size (os-window)
  (win32-window-frame-size os-window))


(defun os-window-content-scale (os-window)
  (win32-window-content-scale os-window))


(defun iconify-os-window (os-window)
  (iconify-win32-window os-window))


(defun restore-os-window (os-window)
  (restore-win32-window os-window))


(defun maximize-os-window (os-window)
  (maximize-win32-window os-window))


(defun show-os-window (os-window)
  (show-win32-window os-window))


(defun hide-os-window (os-window)
  (hide-win32-window os-window))


(defun request-os-window-attention (os-window)
  (request-win32-window-attention os-window))


(defun focus-os-window (os-window)
  (focus-win32-window os-window))


(defun (setf os-window-monitor) (os-window monitor)
  (setf (win32-window-monitor os-window) monitor))


(defun os-window-focused? (os-window)
  (win32-window-focused? os-window))


(defun os-window-iconified? (os-window)
  (win32-window-iconified? os-window))


(defun os-window-visible? (os-window)
  (win32-window-visible? os-window))


(defun os-window-maximized? (os-window)
  (win32-window-maximized? os-window))


(defun os-window-hovered? (os-window)
  (win32-window-hovered? os-window))


(defun os-window-opacity (os-window)
  (win32-window-opacity os-window))


(defun (setf os-window-resizable?) (value os-window)
  (setf (win32-window-resizable? os-window) value))


(defun (setf os-window-decorated?) (value os-window)
  (setf (win32-window-decorated? os-window) value))


(defun (setf os-window-floating?) (value os-window)
  (setf (win32-window-floating? os-window) value))


(defun (setf os-window-opacity) (value os-window)
  (setf (win32-window-opacity os-window) value))


(defun (setf os-window-mouse-passthrough) (value os-window)
  (setf (win32-window-mouse-passthrough os-window) value))


(defun os-window-poll-events (os-window)
  (win32-window-poll-events os-window))


(defun os-window-wait-events (os-window)
  (win32-window-wait-events os-window))


(defun os-window-wait-events-timeout (os-window timeout)
  (win32-window-wait-events-timeout os-window timeout))


(defun os-window-post-empty-event (os-window)
  (win32-window-post-empty-event os-window))

