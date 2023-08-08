(in-package :clui)

;; to enable one logical px to be one physical px framebuffer for MS Windows:
;; (to be the same as it is on macos and kde)
;; In file explorer right click sbcl.exe or wx86cl64.exe for CCL
;; select Properties
;; click "Change settings for All Users"
;; click "Change High DPI Settings"
;; check "Override high DPI scaling behavior,"
;; select "scaling performed by Application"
;; click Ok, click Apply, click Ok

#+CCL
(defun lpcwstr-1 (string)
  (let* ((len (length string)))
    (multiple-value-bind (data offset) (ccl::array-data-and-offset string)
      (let* ((end (+ offset len))
	     (noctets (ccl::utf-16-octets-in-string data offset end))
	     (octets (ccl::malloc (1+ noctets))))
	(ccl::native-utf-16-memory-encode data octets 0 offset end)
	(setf (ccl::%get-unsigned-word octets noctets) 0)
	octets))))

#+(and SBCL little-endian)
(defun lpcwstr-1 (string)
  (let ((octets (SB-IMPL::OUTPUT-TO-C-STRING/UTF-16LE string)))
    (let ((poctets (noffi::sap-malloc (length octets))))
      (loop for i from 0 below (length octets)
	    do (setf (sb-sys::sap-ref-8 poctets i) (aref octets i)))
      poctets)))

(defun lpcwstr (string)
  (cons-ptr (lpcwstr-1 string) 0 '#_<LPCWSTR>))

#+CCL
(defmacro with-lpcwstr ((var string) &body body)
  (let ((ptr-sym (gensym)))
    `(let ((,var (noffi::make-gcable-c-utf-16-string ,string)))
       ,@body)))

#+SBCL
(defmacro with-lpcwstr ((var string) &body body)
  (let ((ptr-sym (gensym)))
    `(let ((,ptr-sym (lpcwstr ,string)))
       (unwind-protect (let ((,var ,ptr-sym))
			 ,@body)
	 (noffi::sap-free (ptr-effective-sap ,ptr-sym))))))

#+CCL
(defun lpcwstr->string (ptr)
  (ccl::%get-native-utf-16-cstring (ptr-effective-sap ptr)))

#+(and SBCL little-endian)
(defun lpcwstr->string (ptr)
  (sb-impl::read-from-c-string/utf-16le (ptr-effective-sap ptr) 'character))

#+CCL
(defun load-win32-libraries ()
  (values))

#+SBCL
(defun load-win32-libraries ()
  (sb-alien:load-shared-object "user32.dll")
  (sb-alien:load-shared-object "ntdll.dll")
  (sb-alien:load-shared-object "Gdi32.dll")
  (sb-alien:load-shared-object "Shcore.dll")
  (values))



#+SBCL
(defun rtl-verify-version-info (posvi mask condition)
  (sb-alien:alien-funcall (sb-alien:extern-alien "RtlVerifyVersionInfo" (sb-alien:function (sb-alien:unsigned 32)
											   sb-sys:system-area-pointer
											   (sb-alien:unsigned 32)
											   (sb-alien:unsigned 64)))
			  (ptr-effective-sap posvi) mask condition))

#+CCL
(defun rtl-verify-version-info (posvi mask condition)
  (ccl::%ff-call (ccl::%reference-external-entry-point (ccl:external "RtlVerifyVersionInfo"))
		 :address (ptr-effective-sap posvi)
		 :unsigned-fullword mask :unsigned-doubleword condition
		 :unsigned-fullword))      

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
	(not (zerop (rtl-verify-version-info posvi mask condition)))))))


(defun windows-7-or-greater? ()
  (windows-version-or-greater? (#_HIBYTE #__WIN32_WINNT_WIN7) (#_LOBYTE #__WIN32_WINNT_WIN7) 0))


(defun windows-8.1-or-greater? ()
  #+NIL(windows-version-or-greater? (#_HIBYTE #__WIN32_WINNT_WIN81) (#_LOBYTE #__WIN32_WINNT_WIN81) 0)
  t)

(defun windows-11-or-greater? ()
  #+NIL
  (windows-version-or-greater? (#_HIBYTE #__WIN32_WINNT_WIN11) (#_LOBYTE #__WIN32_WINNT_WIN11) 0)
  t)

(defun windows-10-build-or-later? (build)
  (clet ((osvi #_<OSVERSIONINFOEXW>))
    (let ((posvi (c-addr-of osvi)))
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
	  (not (zerop (rtl-verify-version-info posvi mask condition)))))))

(defun windows-11-build-or-later? (build)
  (clet ((osvi #_<OSVERSIONINFOEXW>))
    (let ((posvi (c-addr-of osvi)))
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
	  (not (zerop (rtl-verify-version-info posvi mask condition)))))))


(defun windows-10-version-1607-or-greater? ()
  (or (windows-11-build-or-later? 0)
      (windows-10-build-or-later? 14393)))


(defun windows-10-version-1703-or-greater? ()
  (or (windows-11-build-or-later? 0)
      (windows-10-build-or-later? 15063)))

(defun create-key-tables ())

(defun get-win32-display ()
  (let ((displays (get-displays)))
    (find-if #'(lambda (dpy)
		 (typep dpy 'win32:desktop-mixin))
	     displays)))

(defun helper-window-proc (hWnd uMsg wParam lParam)

  (case uMsg

    (#.#_WM_DISPLAYCHANGE (let ((d (get-win32-display)))
			    (when d (poll-win32-monitors (get-win32-display)))))

    (#.#_WM_DEVICECHANGE ))

  (#_DefWindowProcW hWnd uMsg wParam lParam))

(defcfun (helper-window-proc-callback #_<LPARAM>)
	 ((hWnd #_<HWND>) (uMsg #_<UINT>) (wParam #_<WPARAM>) (lParam #_<LPARAM>))
  (helper-window-proc hWnd uMsg wParam lParam))

(defun create-win32-helper-window (display)
  (clet& ((&msg #_<MSG>)
	  (&wc #_<WNDCLASSEXW>))
    (#_memset &wc 0 (c-sizeof-type '#_<WNDCLASSEXW>))
    (setf (#_.cbSize &wc) (c-sizeof-type '#_<WNDCLASSEXW>))
    (setf (#_.style &wc) #_CS_OWNDC)
    (setf (#_.lpfnWndProc &wc) helper-window-proc-callback)
    (setf (#_.hInstance &wc) (win32-instance display))
    (setf (#_.lpszClassName &wc) (lpcwstr #+CCL "CLUI Helper" #+SBCL "CLUI Helper SBCL"))

    (unless (slot-value display 'helper-window-class)
      (setf (slot-value display 'helper-window-class) 
	    (#_RegisterClassExW &wc)))

    (unless (slot-value display 'helper-window-class)
      (error "win32: Failed to register helper window class."))

    (let ((handle
	    (#_CreateWindowExW #_WS_EX_OVERLAPPEDWINDOW
			       (#_MAKEINTATOM (slot-value display 'helper-window-class))
			       (lpcwstr "CLUI Helper Window")
			       (logior #_WS_CLIPSIBLINGS #_WS_CLIPCHILDREN)
			       0 0 1 1
			       nil nil
			       (win32-instance display)
			       nil)))

      

      (unless handle
	(error "win32: Failed to create helper window."))

      (let ((helper-window
	      (make-instance (helper-window-class display)
			     :h handle)))

	(initialize-helper-window display helper-window)

	(setf (helper-window display) helper-window)

	(#_ShowWindow handle #_SW_HIDE)

	#+NOTYET
	(clet& ((&dbi #_<DEV_BROADCAST_DEVICEINTERFACE_W>))
	  (let ((size (c-sizeof-type '#_<DEV_BROADCAST_DEVICEINTERFACE_W>)))
	    (#_memset &dbi 0 size)
	    (setf (#_.dbcc_size &dbi) size
		  (#_.dbcc_devicetype &dbi) #_DBT_DEVTYP_DEVICEINTERFACE
		  (#_.dbcc_classguid &dbi) #_GUID_DEVINTERFACE_HID)

	    (setf (device-notification-handle display)
		  (#_RegisterDeviceNotificationW (device-notification-handle display)
						 &dbi
						 #_DEVICE_NOTIFY_WINDOW_HANDLE))))

	(loop until (zerop (#_PeekMessageW &msg handle 0 0 #_PM_REMOVE))
	      do (#_TranslateMessage &msg)
		 (#_DispatchMessage &msg))

	t))))
	  


(defmethod initialize-helper-window ((display win32:desktop-mixin) helper-window)
  helper-window)

    

(defun win32-init (display)
  (load-win32-libraries)

  (create-win32-key-tables display)
  ;;(update-win32-key-names)

  (cond ((windows-10-version-1703-or-greater?)
	 (#_SetProcessDpiAwarenessContext
	  #_DPI_AWARENESS_CONTEXT_PER_MONITOR_AWARE_V2))
	#+NIL
	((windows-8.1-or-greater?)
	 (#_SetProcessAwareness #_PROCESS_PER_MONITOR_DPI_AWARE))
	#+NIL
	((windows-vista-or-greater?)
	 (#_SetProcessDPIAware)))

  (setf (default-screen display) (make-instance 'screen :display display))

  (create-win32-helper-window display)

  t)

  
  
  
