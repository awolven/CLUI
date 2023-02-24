(in-package :clui)

(noffi::noffi-syntax t)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (noffi::noffi-syntax t)
  (defparameter noffi::*last-good-token* nil)
  )

(defun load-win32-libraries ()
  (ccl:open-shared-library "user32.dll")
  (ccl:open-shared-library "dinput8.dll")
  (ccl:open-shared-library "xinput1_4.dll")
  (ccl:open-shared-library "xinput1_3.dll")
  (ccl:open-shared-library "xinput9_1_0.dll")
  (ccl:open-shared-library "xinput1_2.dll")
  (ccl:open-shared-library "xinput1_1.dll")
  (values))

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
    (setf (#_.cbSize &wc) (c-sizeof-type '#_<WNDCLASSEXW>))
    (setf (#_.style &wc) #_CS_OWNDC)
    (setf (#_.lpfnWndProc &wc) helper-window-proc-callback)
    (setf (#_.hInstance &wc) (win32-instance display))
    (setf (#_.lpszClassName &wc) (lpcwstr "CLUI Helper"))

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
  ;;(load-win32-libraries)
  (create-win32-key-tables display)
  ;;(update-win32-key-names)

  (cond ((windows-10-version-1703-or-greater?)
	 (#_SetProcessDpiAwarenessContext
	  #_DPI_AWARENESS_CONTEXT_PER_MONITOR_AWARE_V2))
	((windows-8.1-or-greater?)
	 (#_SetProcessAwareness #_PROCESS_PER_MONITOR_DPI_AWARE))
	#+NOTYET
	((windows-vista-or-greater?)
	 (#_SepProcessDPIAware)))

  (setf (default-screen display) (make-instance 'screen :display display))

  (create-win32-helper-window display)

  t)

  
  
  
