(in-package :clui)
(noffi::noffi-syntax)

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

(defun lpcwstr (string)
  (let* ((len (length string)))
    (multiple-value-bind (data offset) (ccl::array-data-and-offset string)
      (let* ((end (+ offset len))
	     (noctets (ccl::utf-16-octets-in-string data offset end))
	     (octets (ccl::malloc (1+ noctets))))
	(ccl::native-utf-16-memory-encode data octets 0 offset end)
	(setf (ccl::%get-unsigned-word octets noctets) 0)
	(%cons-ptr octets 0 '#_<LPCWSTR>)))))

(defun paint-win32-window (hwnd)
  (clet ((ps #_<PAINTSTRUCT>))
    (let* ((&ps (c-addr-of ps))
	   (rc-paint #_(&((@ps).rcPaint)))
	   (hdc (#_BeginPaint hwnd &ps)))

      (#_FillRect hdc rc-paint (noffi::int-ptr (1+ #_COLOR_WINDOW) '#_<HBRUSH>))

      (#_EndPaint hwnd &ps))))

(defun window-proc (hWnd uMsg wParam lParam)

  (block nil
    (cond ((= (cval-value uMsg) #_WM_DESTROY)
	   (#_PostQuitMessage 0)
	   (return 0))
	  ((= (cval-value uMsg) #_WM_PAINT)
	   (paint-win32-window hWnd)
	   (return 0))
	  (t (return (#_DefWindowProc hWnd uMsg wParam lParam))))))

(defcfun (window-proc-callback #_<LPARAM>)
	 ((hWnd #_<HWND>) (uMsg #_<UINT>) (wParam #_<WPARAM>) (lParam #_<LPARAM>))
  (window-proc hWnd uMsg wParam lParam))

(defun main ()
  (let ((class-name (lpcwstr "Sample Window Class")))
    (clet ((wc #_<WNDCLASS>))
      (let ((&wc (c-addr-of wc)))

	(setf (#_.lpfnWndProc &wc) window-proc-callback
	      (#_.hInstance &wc) nil ;;(#_GetModuleHandleW nil)
	      (#_.lpszClassName &wc) class-name)

	(#_RegisterClassW &wc)

	(let ((hwnd (#_CreateWindowExW 0
				       class-name
				       (lpcwstr "Learn to program Windows")
				       #_WS_OVERLAPPEDWINDOW
				       #_CW_USEDEFAULT
				       #_CW_USEDEFAULT
				       #_CW_USEDEFAULT
				       #_CW_USEDEFAULT
				       nil
				       nil
				       nil ;;(#_GetModuleHandleW nil)
				       nil)))

	  (print hwnd)

	  (#_ShowWindow hwnd #_SW_SHOW)
	  (#_ShowWindow hwnd #_SW_SHOW)

	  (clet ((msg #_<MSG>))
	    (let ((&msg (c-addr-of msg)))

	      (loop while (prog1 (> (#_GetMessage &msg nil 0 0) 0)
			    (print (#_GetLastError))
			    (finish-output))
		    do (#_TranslateMessage &msg)
		       (#_DispatchMessage &msg)
		       (print 2)
		       (finish-output)))))))))

	
	
