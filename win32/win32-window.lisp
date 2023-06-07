(in-package :clui)

(noffi-syntax)

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

#+NIL
(add-my-exception-handler)

(defconstant +WM_COPYGLOBALDATA+ #x0049)




(defun get-win32-standard-mods ()
  (let ((mods 0))
    (when (logtest (#_GetKeyState #_VK_SHIFT) #x8000)
      (setq mods (logior mods +shift-modifier+)))
    (when (logtest (#_GetKeyState #_VK_CONTROL) #x8000)
      (setq mods (logior mods +ctrl-modifier+)))
    (when (logtest (#_GetKeyState #_VK_MENU) #x8000)
      (setq mods (logior mods +meta-modifier+)))
    (when (or (logtest (#_GetKeyState #_VK_LWIN) #x8000)
	      (logtest (#_GetKeyState #_VK_RWIN) #x8000))
      (setq mods (logior mods +super-modifier+)))
    mods))

(defun get-win32-lock-mods ()
  (let ((mods 0))
    (when (logtest (#_GetKeyState #_VK_CAPITAL) #x1)
      (setq mods (logior mods +caps-lock-modifier+)))
    (when (logtest (#_GetKeyState #_VK_NUMLOCK) #x1)
      (setq mods (logior mods +num-lock-modifier+)))
    mods))	    

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

(defun create-win32-standard-cursor (shape)
  (let ((lpCursorName (case shape
			(:arrow #_IDC_ARROW)
			(:ibeam #_IDC_IBEAM)
			(:crosshair #_IDC_CROSS)
			(:pointing-hand)
			(:hand #_IDC_HAND)
			(:help #_IDC_HELP)
			(:wait #_IDC_WAIT)
			(:ew #_IDC_SIZEWE)
			(:ns #_IDC_SIZENS)
			(:nwse #_IDC_SIZENWSE)
			(:nesw #_IDC_SIZENESW)
			(:compass #_IDC_SIZEALL)
			(:up #_IDC_UPARROW)
			(:not-allowed #_IDC_NO))))
    (if lpCursorName
	(#_LoadCursorW nil lpCursorName)
	(#_LoadCursorW nil #_IDC_ARROW))))

(defun set-win32-window-cursor (window cursor)
  (declare (ignorable cursor))
  ;;(when (win32-cursor-in-content-area? window)
    (update-win32-cursor-image window));;)  

(defun update-win32-cursor-image (window)
  (if (or (eq (window-cursor-mode window) :normal)
	  (eq (window-cursor-mode window) :captured))
      (if (window-cursor window)
	  (#_SetCursor (h (window-cursor window)))
	  (#_SetCursor (#_LoadCursor nil #_IDC_ARROW)))
      (#_SetCursor nil))
  (values))

(defun win32-cursor-in-content-area? (window)
  (multiple-value-bind (x y) (get-win32-window-cursor-pos window)
      (clet& ((&rect #_<RECT>))
	(#_GetWindowRect (h window) &rect)

	(and (< (#_.left &rect) x (#_.right &rect))
	     (< (#_.top &rect) y (#_.bottom &rect))))))

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

(defun get-win32-window-framebuffer-size (window)
  (clet ((area #_<RECT>))
    (let ((&area (c-addr-of area)))
    (setf (#_.left &area) 0
	  (#_.top &area) 0
	  (#_.right &area) 0
	  (#_.top &area) 0)    
      (when (#_GetClientRect (h window) &area)
	(values (#_.right &area) (#_.bottom &area))))))

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
  (= (sap-int (ptr-effective-sap (h window)))
     (sap-int (ptr-effective-sap (#_GetActiveWindow)))))

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

#+NIL
(defun get-win32-window-hovered (window)
  (block nil
    (clet ((area #_<RECT>)
	   (pos #_<POINT>))
      (let ((&area (c-addr-of area))
	    (&pos (c-addr-of pos)))
	
	(when (= 0 (#_GetCursorPos &pos))
	  (return nil))

	(unless (= (sap-int (ptr-effective-sap (#_WindowFromPoint pos)))
		   (sap-int (ptr-effective-sap (h window))))
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

(defun set-win32-raw-mouse-motion (window value)
  (declare (ignorable value))
  (unless (eq (disabled-cursor-window (window-display window)) window)
    (return-from set-win32-raw-mouse-motion (values)))
  #+NOTYET
  (if value
      (enable-win32-raw-mouse-motion window)
      (disable-win32-raw-mouse-motion window))
  (values))

#+NOTYET
(defun enable-win32-raw-mouse-motion (window)
  (clet& ((&rid #_<RAWINPUTDEVICE>))
	 (setf (.usUsagePage &rid) #x01
	       (.usUsage &rid) #x02
	       (.dwFlags &rid) 0
	       (.hwndTarget &rid) (h window))

	 (if (#_RegisterRawInputDevices &rid 1 (c-sizeof-type '#_<RAWINPUTDEVICE>))
	     (setf (%raw-mouse-motion? window) t)
	     (error "Win32: Failed to register raw input device"))))

#+NOTYET
(defun disable-win32-raw-mouse-motion (window)
  (clet& ((&rid #_<RAWINPUTDEVICE>))
	 (setf (.usUsagePage &rid) #x01
	       (.usUsage &rid) #x02
	       (.dwFlags &rid) #_RIDEV_REMOVE
	       (.hwndTarget &rid) nil)
	 
	 (if (#_RegisterRawInputDevices &rid 1 (c-sizeof-type '#_<RAWINPUTDEVICE>))
	     (setf (%raw-mouse-motion window) nil)
	     (error "Win32: Failed to remove raw input device"))))
	 
  
	 


(defun set-win32-window-size-limits (window min-width min-height max-width max-height)
  (declare (ignorable window min-width min-height max-width max-height))
  (values))

(defun get-win32-window-aspect-ratio (window)
  (declare (ignorable window))
  (values))

(defun get-win32-window-size (window)
  (multiple-value-bind (xscale yscale) (get-win32-window-content-scale window)
    (multiple-value-bind (width height) (get-win32-window-framebuffer-size window)
      (values (float (/ width xscale)) (float (/ height yscale))))))
    

(defun get-win32-window-frame-size (window)
  (multiple-value-bind (width height) (get-win32-window-size window)
    (clet& ((&rect #_<RECT>))
      (if (windows-10-version-1607-or-greater?)
	  (#_AdjustWindowRectExForDpi &rect (get-window-style window)
				      #_FALSE (get-window-ex-style window)
				      (#_GetDpiForWindow (h window)))
	  (#_AdjustWindowRectEx &rect (get-window-style window)
				#_FALSE (get-window-ex-style window)))

      (values (- (#_.left &rect))
	      (- (#_.top &rect))
	      (- (#_.right &rect) width)
	      (- (#_.bottom &rect) height)))))

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



#+SBCL
(defun ff-get-dpi-for-monitor (hmonitor *xdpi *ydpi)
  (sb-alien:alien-funcall (sb-alien:extern-alien "GetDpiForMonitor" (sb-alien:function (sb-alien:signed 32)
										       sb-sys:system-area-pointer
										       (sb-alien:unsigned 32)
										       sb-sys:system-area-pointer
										       sb-sys:system-area-pointer))
			  (ptr-effective-sap hmonitor)
			  0 ;;#_MDT_EFFECTIVE_DPI
			  (ptr-effective-sap *xdpi)
			  (ptr-effective-sap *ydpi)))


#+CCL
(defun ff-get-dpi-for-monitor (hmonitor &xdpi &ydpi)
  (ccl::%ff-call (ccl::%reference-external-entry-point
		  (ccl:external "GetDpiForMonitor"))
		 :address (ptr-effective-sap hmonitor)
		 :unsigned-fullword 0 ;;#_MDT_EFFECTIVE_DPI
		 :address (ptr-effective-sap &xdpi)
		 :address (ptr-effective-sap &ydpi)
		 :signed-fullword))

(defun get-dpi-for-monitor (hmonitor)
  (clet ((xdpi #_<UINT>)
	 (ydpi #_<UINT>))
    
    (if (zerop (ff-get-dpi-for-monitor hmonitor (c-addr-of xdpi) (c-addr-of ydpi)))
	(values (cval-value xdpi) (cval-value ydpi))
	nil)))

(defun get-hmonitor-content-scale (handle)
  (if (windows-8.1-or-greater?)
      (multiple-value-bind (xdpi ydpi) (get-dpi-for-monitor handle)
	(unless xdpi
	  (error "Win32: Failed to query monitor DPI."))
	(values (/ xdpi #_USER_DEFAULT_SCREEN_DPI)
		(/ ydpi #_USER_DEFAULT_SCREEN_DPI)))
      #+NOTYET
      (let* ((dc (#_GetDC nil)))
	(unwind-protect
	     (clet ((xdpi #_<UINT> (#_GetDeviceCaps dc #_LOGPIXELSX))
		    (ydpi #_<UINT> (#_GetDeviceCaps dc #_LOGPIXELSY)))
	       (values (/ xdpi #_USER_DEFAULT_SCREEN_DPI)
		       (/ ydpi #_USER_DEFAULT_SCREEN_DPI)))
	  (#_ReleaseDC nil dc)))))












(eval-when (:compile-toplevel :load-toplevel :execute)
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

      (#_FillRect hdc rc-paint (noffi::cons-ptr (int-sap (1+ #_COLOR_WINDOW)) 0 '#_<HBRUSH>))

      (#_EndPaint (h window) &ps))))
      



#+SBCL
(defun get-window-prop (hWnd)
  (with-lpcwstr (str "CLUI")
    (sb-alien:alien-funcall (sb-alien:extern-alien "GetPropW" (sb-alien:function (sb-alien:unsigned 64)
										 sb-sys:system-area-pointer
										 sb-sys:system-area-pointer))
			    (ptr-effective-sap hWnd)
			    (ptr-effective-sap str))))

#+CCL
(defun get-window-prop (hWnd)
  (with-lpcwstr (str "CLUI")
    (ccl::%ff-call (ccl::%reference-external-entry-point (ccl:external "GetPropW"))
		   :address (ptr-effective-sap hWnd) :address (ptr-effective-sap str)
		   :unsigned-doubleword)))

#+SBCL
(defun set-window-prop (hWnd id)
  (with-lpcwstr (str "CLUI")
    (sb-alien:alien-funcall (sb-alien:extern-alien "SetPropW" (sb-alien:function sb-alien:void
										 sb-sys:system-area-pointer
										 sb-sys:system-area-pointer
										 (sb-alien:unsigned 64)))
			    (ptr-effective-sap hWnd)
			    (ptr-effective-sap str)
			    id)))

#+CCL
(defun set-window-prop (hWnd id)
  (with-lpcwstr (str "CLUI")
    (ccl::%ff-call
     (ccl::%reference-external-entry-point (ccl:external "SetPropW"))
     :address (ptr-effective-sap hWnd) :address (ptr-effective-sap str)
     :unsigned-doubleword id
     :void)))
  


(defun window-proc (hWnd uMsg wParam lParam)

  (block nil
    
    (when (= uMsg #_WM_NCCREATE) ;; 129
	
      (when (windows-10-version-1607-or-greater?)
	  
	(let* ((cs (cons-ptr (int-sap lParam) 0 '#_<CREATESTRUCTW*>))
	       (wndconfig (c-cast '#_<wndconfig*> (#_.lpCreateParams cs)))
	       (id (#_.id wndconfig))
	       (scale-to-monitor (#_.scaletomonitor wndconfig)))
	  
	  (set-window-prop hWnd id)

	  (let ((window (gethash id *id->window-table*)))
	    (setf (h window) hWnd))
	    
	  (unless (= 0 scale-to-monitor)
	    (#_EnableNonClientDpiScaling hWnd))))
	
      (return (#_DefWindowProcW hWnd uMsg wParam lParam)))

    (let ((time (get-internal-real-time))
	  (mods (get-win32-standard-mods))
	  (lock-mods (get-win32-lock-mods)))

      (let ((id (get-window-prop hWnd))
	    (window))

	(when (= id 0)
	  (warn "failed to get window id for msg: ~S" uMsg)
	  (return (#_DefWindowProcW hWnd uMsg wParam lParam)))
	
	(setq window (gethash id *id->window-table*))
      
	(tagbody
	   (locally (declare (type os-window-mixin window))

	     (with-slots (cursor-mode high-surrogate
			  monitor auto-iconify? key-menu)
		 window

	       (case uMsg

		 (#.#_WM_NCCALCSIZE (go break)) ;; 131

		 (#.#_WM_CREATE
		  (setf (h window) hWnd)
		  (setf (gethash
			 (sap-int (ptr-effective-sap hWnd))
			 *window-handle->window-table*)
			window)
		  (go break))		; 1

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

		  (handle-event window (make-instance 'window-repaint-event
						      :window window
						      :timestamp time))

		  (default-paint-win32-window window)

		  (return 0)

		  #+NIL
		  (go break))

		 (#.#_WM_IME_SETCONTEXT (go break)) ;; 641

		 (#.#_WM_IME_NOTIFY (go break)) ;; 642

		 (#.#_WM_SETFOCUS ;; 7

		  (handle-event window (make-instance 'window-focus-event
						      :window window
						      :timestamp time))

		  (when (frame-action? window)
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
		      (handle-event window (make-instance 'window-iconify-event
							  :window window
							  :new-width width
							  :new-height height
							  :timestamp time)))

		    (unless (eq (last-maximized? window) maximized?)
		      (let ((event (make-instance (if maximized?
						      'window-maximize-event
						      'window-restore-event)
						  :window window
						  :new-width width
						  :new-height height
						  :timestamp time)))
			(handle-event window event)))

		    (when (or (/= width (round (or (last-width window) 0)))
			      (/= height (round (or (last-height window) 0))))

		      (let ((event (make-instance 'window-resize-event
						  :window window
						  :new-width width
						  :new-height height
						  :timestamp time)))
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
					      :window window
					      :new-x (#_GET_X_LPARAM lParam)
					      :new-y (#_GET_Y_LPARAM lParam)
					      :timestamp time)))

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
		      (let ((mmi (cons-ptr (int-sap lParam) 0 '#_<MINMAXINFO*>))
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
		  (return 0))

		 (#.#_WM_MOUSEACTIVATE ;; 33
		  (when (= (#_HIWORD lParam) #_WM_LBUTTONDOWN)
		    (when (/= (#_LOWORD lParam) #_HTCLIENT)
		      (setf (frame-action? window) t)))
		  (go break))

		 (#.#_WM_DWMNCRENDERINGCHANGED (go break)) ;; 799 ;; what am i?

		 (#.#_WM_CAPTURECHANGED ;; 533
		    
		  (when (and (= lParam 0) (frame-action? window))
		    (if (eq (window-cursor-mode window) :disabled)
			(disable-cursor window)
			(when (eq (window-cursor-mode window) :captured)
			  (capture-win32-cursor window)))
		    (setf (frame-action? window) nil))
		    
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

		  (handle-event window (make-instance 'window-defocus-event
						      :window window
						      :timestamp time))
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
						      :window window
						      :timestamp time))
		  (return 0))

		 (#.#_WM_INPUTLANGCHANGE ;; 81
		  #+NOTYET
		  (win32-update-key-names (window-display window))
		  (go break))

		 ((#.#_WM_CHAR #.#_WM_SYSCHAR) ;; 258 and 262

		  (if (and (>= wParam #xd800) (<= wParam #xdbff))
		      (setf (high-surrogate window) wParam)
		      (let ((codepoint 0))
			(if (and (>= wParam #xdc00) (<= wParam #xdfff))
			    (when (high-surrogate window)
			      (incf codepoint (ash (- (high-surrogate window) #xd800) 10))
			      (incf codepoint (- wParam #xdc00))
			      (incf codepoint #x10000))
			    (setq codepoint wParam))
			(setf (high-surrogate window) nil)
			(input-char window codepoint mods lock-mods (not (= uMsg #_WM_SYSCHAR)))))
		  (when (and (= uMsg #_WM_SYSCHAR) (key-menu? window))
		    (go break))
		  (return 0))

		 (#.#_WM_UNICHAR ;; 265

		  (when (= wParam #_UNICODE_NOCHAR)
		    (return 1))

		  (input-char window wParam mods lock-mods t)
		  (return 0))

		 ((#.#_WM_KEYDOWN #.#_WM_SYSKEYDOWN #.#_WM_KEYUP #.#_WM_SYSKEYUP) ;; 256, 260, 257, 261
		  (let ((key)
			(scancode)
			(action (if (logtest (#_HIWORD lParam) #_KF_UP) :release :press))
			(time (#_GetMessageTime)))

		    (setq scancode (logand (#_HIWORD lParam) (logior #_KF_EXTENDED #xff)))

		    (when (= 0 scancode)
		      (setq scancode (#_MapVirtualKeyW wParam #_MAPVK_VK_TO_VSC)))

		    ;; ALT+PrtSc is different than PrtSc
		    (when (= scancode #x54)
		      (setq scancode #x137))

		    ;; Ctrl+Pause is different than Pause
		    (when (= scancode #x146)
		      (setq scancode #x45))

		    ;; CJK IME sets the extended bit for right shift
		    (when (= scancode #x136)
		      (setq scancode #x36))

		    (setq key (aref (display-keycodes (window-display window)) scancode))

		    (if (= wParam #_VK_CONTROL)
			(if (logtest (#_HIWORD lParam) #_KF_EXTENDED)
			    (setq key +key-right-ctrl+)
			    (progn
			      (clet& ((&next #_<MSG>))
				(when (#_PeekMessageW &next nil 0 0 #_PM_NOREMOVE)
				  (case (#_.message &next)
				    ((#_WM_KEYDOWN #_WM_SYSKEYDOWN #_WM_KEYUP #_WMSYSKEYUP)
				     (when (and (= (#_.wParam &next) #_VK_MENU)
						(logtest (#_HIWORD (#_.lParam &next)) #_KF_EXTENDED)
						(= (#_.time &next) time))
				       (go break)))))
				(setq key +key-left-ctrl+))))
		      
			(when (= wParam #_VK_PROCESSKEY)
			  (go break)))

		    (multiple-value-bind (x y) (window-cursor-position window)
		    
			(if (and (eq action :release) (= wParam #_VK_SHIFT))
			    (progn
			      (input-key window +key-left-shift+ :release x y mods lock-mods time)
			      (input-key window +key-right-shift+ :release x y mods lock-mods time))
			
			    (if (= wParam #_VK_SNAPSHOT)
				;; key down is not supported for the Print Screen key
				(progn
				  (input-key window key :press x y
					     mods
					     lock-mods
					     time)
				  (input-key window key :release x y
					     mods
					     lock-mods
					     time))
			      
				(input-key window key action x y mods lock-mods time))))		  
		  
		    (go break)))

		 ((#.#_WM_LBUTTONDOWN
		   #.#_WM_RBUTTONDOWN #.#_WM_MBUTTONDOWN #.#_WM_XBUTTONDOWN
		   #.#_WM_LBUTTONUP #.#_WM_RBUTTONUP
		   #.#_WM_MBUTTONUP #.#_WM_XBUTTONUP)

		  (let ((button)
			(action))

		    (cond ((= uMsg #_WM_LBUTTONDOWN)
			   (setq action :press)
			   (setq button +pointer-left-button+))
			  
			  ((= uMsg #_WM_LBUTTONUP)
			   (setq action :release)
			   (setq button +pointer-left-button+))

			  ((= uMsg #_WM_RBUTTONDOWN)
			   (setq action :press)
			   (setq button +pointer-right-button+))
			  
			  ((= uMsg #_WM_RBUTTONUP)
			   (setq action :release)
			   (setq button +pointer-right-button+))
			  
			  ((= uMsg #_WM_MBUTTONDOWN)
			   (setq action :press)
			   (setq button +pointer-middle-button+))
			  
			  ((= uMsg #_WM_LBUTTONUP)
			   (setq action :release)
			   (setq button +pointer-middle-button+))

			  ((and (= (#_GET_XBUTTON_WPARAM wParam) #_XBUTTON1)
				(= uMsg #_WM_XBUTTONDOWN))
			   (setq action :press)
			   (setq button +pointer-button-4+))
			  
			  ((and (= (#_GET_XBUTTON_WPARAM wParam) #_XBUTTON1)
				(= uMsg #_WM_XBUTTONUP))
			   (setq action :release)
			   (setq button +pointer-button-4+))
			  
			  ((= uMsg #_WM_XBUTTONUP)
			   (setq action :release)
			   (setq button +pointer-button-5+))
			  
			  (t (setq action :press)
			     (setq button +pointer-button-5+)))

		    (let ((found? nil)
			  (keys (window-keys window)))
		    
		      (loop for i in (list +pointer-left-button+
					   +pointer-right-button+
					   +pointer-middle-button+
					   +pointer-button-4+
					   +pointer-button-5+)
			    when (eq (aref keys i) :press)
			      do (setq found? t)
				 (return))

		      (when found?
			(#_SetCapture hWnd)))

		    (multiple-value-bind (x y) (window-cursor-position window)
		      (input-mouse-click window button action x y mods lock-mods time))

		    (let ((found? nil))
		      (loop for i from 0 below 5
			    when (eq (elt (window-keys window) i) :press)
			      do (setq found? t)
				 (return))

		      (when found?
			(#_ReleaseCapture)))

		    (when (or (= uMsg #_WM_XBUTTONDOWN)
			      (= uMsg #_WM_XBUTTONUP))
		      (return 1))

		    (return 0)))

		 (#.#_WM_MOUSEMOVE ;; 512
		  (let* ((x (#_GET_X_LPARAM lParam))
			 (y (#_GET_Y_LPARAM lParam)))

		    (unless (cursor-tracked? window)
		      (clet& ((&tme #_<TRACKMOUSEEVENT>))
			(let ((size (c-sizeof-type '#_<TRACKMOUSEEVENT>)))
			  (#_memset &tme 0 size)
			  (setf (#_.cbSize &tme) size
				(#_.dwFlags &tme) #_TME_LEAVE
				(#_.hwndTrack &tme) hWnd)
			  (#_TrackMouseEvent &tme)))

		      (setf (cursor-tracked? window) t)

		      (handle-event window (make-instance 'pointer-enter-event
							  :window window
							  :input-code +pointer-move+
							  :x x
							  :y y
							  :native-x x
							  :native-y y
							  :modifier-state mods
							  :lock-modifier-state lock-mods
							  :timestamp time)))

		    (if (eq (window-cursor-mode window) :disabled)
			(let ((dx (- x (last-cursor-pos-x window)))
			      (dy (- y (last-cursor-pos-y window))))
			  (unless (eq (disabled-cursor-window (window-display window)) window)
			    (go break))
			  (when (raw-mouse-motion? window)
			    (go break))
			
			  (handle-event window (make-instance 'pointer-motion-event
							      :window window
							      :input-code +pointer-move+
							      :x (+ (virtual-cursor-pos-x window) dx)
							      :y (+ (virtual-cursor-pos-y window) dy)
							      :native-x (+ (virtual-cursor-pos-x window) dx)
							      :native-y (+ (virtual-cursor-pos-y window) dy)
							      :modifier-state mods
							      :lock-modifier-state lock-mods
							      :timestamp time)))
			(progn
			  (handle-event window (make-instance 'pointer-motion-event
							      :window window
							      :input-code +pointer-move+
							      :x x
							      :y y
							      :native-x x
							      :native-y y
							      :modifier-state mods
							      :lock-modifier-state lock-mods
							      :timestamp time))
			  (setf (last-cursor-pos-x window) x
				(last-cursor-pos-y window) y)
			  (return 0)))))

		 (#.#_WM_NCMOUSEMOVE ;; 160
		  (go break))

		 (#.#_WM_INPUT (go break))

		 (#.#_WM_MOUSELEAVE ;; 675
		  (setf (cursor-tracked? window) nil)
		  (multiple-value-bind (x y) (window-cursor-position window)
		    (handle-event window (make-instance 'pointer-exit-event
							:window window
							:input-code +pointer-move+
							:x x :y y
							:native-x x :native-y y
							:modifier-state mods
							:lock-modifier-state lock-mods
							:timestamp time))))

		 (#.#_WM_NCMOUSELEAVE ;; 674
		  (go break))

		 (#.#_WM_MOUSEWHEEL
		  (multiple-value-bind (x y) (window-cursor-position window)
		    (handle-event window (make-instance 'pointer-wheel-event
							:window window
							:input-code +pointer-wheel+
							:yoffset (/ (cval-value (c-coerce (#_HIWORD wParam) '#_<short>))
								    #_WHEEL_DELTA)
							:x x :y y
							:native-x x :native-y y
							:modifier-state mods
							:lock-modifier-state lock-mods
							:timestamp time)))
		  (return 0))

		 (#.#_WM_MOUSEHWHEEL
		  (multiple-value-bind (x y) (window-cursor-position window)
		    (handle-event window (make-instance 'pointer-wheel-event
							:window window
							:input-code +pointer-wheel+
							:xoffset (- (/ (cval-value (c-coerce (#_HIWORD wParam) '#_<short>)) #_WHEEL_DELTA))
							:x x :y y
							:native-x x :native-y y
							:modifier-state mods
							:lock-modifier-state lock-mods
							:timestamp time)))
		  (return 0))

		 ((#.#_WM_ENTERSIZEMOVE #.#_WM_ENTERMENULOOP)
		
		  (when (frame-action? window)
		    (go break))

		  (let ((cursor-mode (window-cursor-mode window)))
		    (case cursor-mode
		      (:disabled (enable-cursor window))
		      (:captured (release-cursor (window-display window)))))

		  (go break))

		 ((#.#_WM_EXITSIZEMOVE #.#_WM_EXITMENULOOP)
		
		  (when (frame-action? window)
		    (go break))
		
		  (let ((cursor-mode (window-cursor-mode window)))
		    (case cursor-mode
		      (:disabled (disable-cursor window))
		      (:captured (capture-cursor (window-display window)))))
		
		  (go break))
	       
		 ((#.#_WM_DWMCOMPOSITIONCHANGED #.#_WM_DWMCOLORIZATIONCOLORCHANGED)
		  (when (last-transparent? window)
		    #+NOTYET
		    (update-framebuffer-transparency window))
		  (return 0))

		 (#.#_WM_GETDPISCALEDSIZE
		  (when (scale-to-monitor? window)
		    (go break))

		  (when (windows-10-version-1703-or-greater?)
		    (let ((size (cons-ptr (int-sap lParam) 0  '#_<SIZE*>))
			  (style (get-window-style window))
			  (ex-style (get-window-ex-style window)))
		    
		      (clet& ((&source #_<RECT>)
			      (&target #_<RECT>))
		    
			(#_AdjustWindowRectExForDpi &source style
						    #_FALSE ex-style
						    (#_GetDpiForWindow hWnd))
		      
			(#_AdjustWindowRectExForDpi &target style
						    #_FALSE ex-style
						    (#_LOWORD wParam))

			(setf (#_.cx size) (+ (#_.cx size) (- (- (#_.right &target) (#_.left &target))
							      (- (#_.right &source) (#_.left &source))))
			      (#_.cy size) (+ (#_.cy size) (- (- (#_.bottom &target) (#_.top &target))
							      (- (#_.bottom &source) (#_.top &source)))))

			(return #_TRUE))))

		  (go break))		    

		 (#.#_WM_DPICHANGED

		  #+NOTYET
		  (let ((xscale (coerce (/ (#_HIWORD wParam) #_USER_DEFAULT_SCREEN_DPI) 'single-float))
			(yscale (coerce (/ (#_LOWORD wParam) #_USER_DEFAULT_SCREEN_DPI) 'single-float)))

		    (when (and (not (window-monitor window))
			       (or (scale-to-monitor? window)
				   (windows-10-version-1703-or-greater?)))
		      (let ((suggested (cons-ptr (int-sap lParam) 0 '#_<RECT*>)))
			(#_SetWindowPos hWnd #_HWND_TOP
					(#_.left suggested)
					(#_.top suggested)
					(- (#_.right suggested) (#_.left suggested))
					(- (#_.bottom suggested) (#_.top suggested))
					(logior #_SWP_NOACTIVATE #_SWP_NOZORDER))))

		    (input-window-content-scale window xscale yscale)
		    (go break)))

		 (#.#_WM_SETCURSOR ;; 32
		  (when (= (#_LOWORD lParam) #_HTCLIENT)
		    (update-cursor-image window)
		    (return #_TRUE))
		  (go break))

		 (#.#_WM_DROPFILES
		  (let ((drop wParam)
			(paths ()))
		    (clet& ((&pt #_<POINT>))
		      (let ((count (#_DragQueryFileW drop #xffffffff nil 0)))
			(#_DragQueryPoint drop &pt)
			(input-cursor-pos window (#_.x &pt) (#_.y &pt) mods lock-mods)

			(loop for i from 0 below count
			      do (let ((length (#_DragQueryFileW drop i nil 0)))
				   (clet ((buffer #_<WCHAR[1024]>))
				     (#_memset buffer 0 (* (c-sizeof-type '#_<WCHAR>) 1024))
				     (#_DragQueryFileW drop i buffer (1+ length))
				     (let ((string (noffi::get-c-string buffer)))
				       (push string paths)))))
		      
			(handle-event window (make-instance 'drop-event
							    :window window
							    :timestamp time
							    :modifier-state mods
							    :lock-modifier-state lock-mods
							    :paths (nreverse paths)))

			(#_DragFinish drop)
			(return 0)))))

		    
		 )))
	   
	 break    
	   (finish-output))

	(return (#_DefWindowProcW hWnd uMsg wParam lParam))))))


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
			     (setf (#_.lpfnWndProc &wc) #+SBCL (noffi::callback 'window-proc-callback)
							#+CCL window-proc-callback)
			     (setf (#_.cbClsExtra &wc) 0)
			     (setf (#_.cbWndExtra &wc) 0)
			     (setf (#_.hInstance &wc) (#_GetModuleHandleW nil))
			     (setf (#_.hCursor &wc) (#_LoadCursorW nil #_IDC_ARROW))
			     (setf (#_.hbrBackground &wc) nil)
			     (setf (#_.lpszMenuName &wc) nil)
			     (setf (#_.lpszClassName &wc) p-cls-nm)
			     (setf (#_.hIconSm &wc) nil)

			     (setf (#_.hIcon &wc) (#_LoadImageW
						   (#_GetModuleHandleW nil)
						   p-icon-nm
						   #_IMAGE_ICON
						   0 0
						   (logior #_LR_DEFAULTSIZE #_LR_SHARED)))
			     
			     (unless (#_.hIcon &wc)
			       (setf (#_.hIcon &wc) (#_LoadImageW
						     nil
						     #_IDI_APPLICATION
						     #_IMAGE_ICON
						     0 0 (logior #_LR_DEFAULTSIZE #_LR_SHARED))))

			     (let ((result (#_RegisterClassExW &wc)))
			       (if (= 0 result)
				   (let ((error (#_GetLastError)))
				     (error "Win32: Failed to register window class, error ~A." error))
				   (setf main-window-class result)))))))))
	   
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
			 
			   (setf (last-width window) (- (#_.right &rect) (#_.left &rect))
				 (last-height window) (- (#_.bottom &rect) (#_.top &rect)))
			 
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
			       
				       (#_SetWindowPos handle #_HWND_TOP
						       (#_.left rc-work)
						       (#_.top rc-work)
						       (- (#_.right rc-work) (#_.left rc-work))
						       (- (#_.bottom rc-work) (#_.top rc-work))
						       (logior #_SWP_NOACTIVATE #_SWP_NOZORDER))))))))))))))

	    (maybe-register-class)
	  
	    (create-window)

	    (maybe-allow-messages)

	    (setf (scale-to-monitor? window) scale-to-monitor?)
	    
	    (maybe-reposition-window)
	    
	    (#_DragAcceptFiles handle #_TRUE)
	    
	    (multiple-value-bind (xpos ypos) (get-win32-window-pos window)
	      
	      (setf (last-pos-x window) xpos
		    (last-pos-y window) ypos)
	      
	      (multiple-value-bind (window-width window-height) (get-win32-window-size window)
	  
		(setf (last-width window) window-width
		      (last-height window) window-height)))
	    t))))))

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
  #+NOTYET
  (input-monitor-window (window-monitor window) window))


(defun release-win32-monitor (window monitor)
  (declare (type os-window-mixin window))
  
  (unless (eq (monitor-window monitor) window)
    (return-from release-win32-monitor (values)))

  (decf (acquired-monitor-count (window-display window)))
  
  (when (zerop (acquired-monitor-count (window-display window)))
    (#_SetThreadExecutionState #_ES_CONTINUOUS)
    (clet ((mouseTrailSize #_<UINT> (mouse-trail-size (window-display window))))
      (#_SystemParametersInfoW #_SPI_SETMOUSETRAILS 0 (c-addr-of mouseTrailSize) 0)))

  #+NOTYET
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
	window)
      (error "Win32: failed to create native window.")))




(defun wait-win32-events (app)
  (clet ((msg #_<MSG>))
    (let* ((&msg (c-addr-of msg))
	   (result)
	   (window))

      (unless (> (setq result (#_GetMessageW &msg nil 0 0)) 0)
	
	(error "GetMessage returned non positive"))
      
      (unless (zerop result)

	(if (= (#_.message &msg) #_WM_QUIT)

	    (progn

	      (setq window (display-window-list-head app))

	      (loop while window
		    do (handle-event window (make-instance 'window-close-event))
		       (setq window (window-next window))))

	    (progn
	      (#_TranslateMessage &msg)
	      (#_DispatchMessageW &msg)))))))

(defun poll-win32-events (app)
  (clet ((msg #_<MSG>))
    (let ((&msg (c-addr-of msg))
	  (window))

      (loop until (progn

		    (zerop (#_PeekMessageW &msg nil 0 0 #_PM_REMOVE)))

	    do
	       (if (= (#_.message &msg) #_WM_QUIT)

		   (progn
		     
		     (setq window (display-window-list-head app))

		     (loop while window
			   do (handle-event window (make-instance 'window-close-event))
			      (setq window (window-next window))))

		   (progn
		     (#_TranslateMessage &msg)
		     (#_DispatchMessageW &msg)))))))


      


