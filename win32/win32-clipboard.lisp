(in-package :clui)

(defun win32-copy-string-to-clipboard (string)
  (when (stringp string)
    (unless (#_OpenClipboard nil)
      (return-from win32-copy-string-to-clipboard nil))

    (with-lpcwstr (lpcwstr string)
      (let ((size (* (1+ (* (length string))) (cval-value (c-sizeof-type '#_<WCHAR>)))))

	(when (zerop (#_EmptyClipboard))
	  (error "EmptyClipboard returned error code: ~S" (#_GetLastError)))
	
      
	(let ((hglb (#_GlobalAlloc #_GMEM_MOVEABLE size)))

	  (unless hglb
	    (#_CloseClipboard)
	    (return-from win32-copy-string-to-clipboard nil))

	  (let ((lpcwstrCopy (#_GlobalLock hglb)))

	    (#_memcpy lpcwstrCopy lpcwstr size)

	    (#_GlobalUnlock hglb)
	  
	    (unwind-protect (progn
			      (#_SetClipboardData #_CF_UNICODETEXT hglb)
			      t)
	    
	      (#_CloseClipboard))))))))

(defun win32-copy-string-from-clipboard ()
  (unless (#_OpenClipboard nil)
    (return-from win32-copy-string-from-clipboard nil))
  
  (let* ((hglb (#_GetClipboardData #_CF_UNICODETEXT)))
    (unless (noffi::ptr-nullptr-p hglb)
      (let ((hglbCopy (#_GlobalLock hglb)))
	(unless (noffi::ptr-nullptr-p hglbCopy)
	  (unwind-protect 
	       (noffi::with-stack-allocated-sap ((lpcwstr 4096))
		 (setq lpcwstr (cons-ptr lpcwstr 0 '#_<LPCWSTR>))
		 (#_memcpy lpcwstr hglbCopy 4096)
		 (#_GlobalUnlock hglb)
		 (get-native-utf16-string lpcwstr))
	    (#_CloseClipboard)))))))
	    
    
