(in-package :clui)

#+NIL
(defun win32-copy-string-to-clipboard (string)
  (when (stringp string)
    (when (> (length string) 0)
      
      (unless (#_OpenClipboard nil)
	(return-from win32-copy-string-to-clipboard nil))
      
      (with-lpcwstr (lpcwstr string)
	(let ((size (* (1+ (length string)) (c-sizeof-type '#_<WCHAR>))))
	  
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
		
		(#_CloseClipboard)))))))))

(defun win32-copy-string-to-clipboard (string &key (start 0) (end nil))
  (let ((end (or end (length string))))
    (check-type start unsigned-byte)
    (check-type end unsigned-byte)
    (assert (<= 0 start end (length string)))
    ;; Now, this is humble. We need to put UTF-16 data into memory allocated with
    ;; #_GlobalAlloc. noffi could provide more infrastructure here.
    (let ((buf-handle
           (let ((size (* (+ (* 2 (- end start)) ;up to two WCHAR per CL:CHARACTER
                             1)                  ;terminating zero WCHAR
                          2)))                   ;#_(sizeof(WCHAR))
             (let ((buf-handle (#_GlobalAlloc #_GMEM_MOVEABLE size)))
               (unless buf-handle
                 (error "#_GlobalAlloc failed"))
               (let ((buf (c-coerce (#_GlobalLock buf-handle) '#_<unsigned short *>)))
                 (unless buf
                   (error "#_GlobalLock failed"))
                 (unwind-protect
                      (progn
                        ;; Copy the string UTF-16 encoded using surrogates if needed.
                        (let ((i start) (j 0))
                          (do () ((= i end))
                            (let ((k (char-code (char string (1- (incf i))))))
                              (cond ((< k #x10000)
                                     (setf (c-aref buf (1- (incf j))) k))
                                    (t
                                     (decf k #x10000)
                                     (setf (c-aref buf (1- (incf j))) (+ #xD800 (ldb (byte 10 10) k))
                                           (c-aref buf (1- (incf j))) (+ #xDC00 (ldb (byte 10 0) k)))))))
                          (setf (c-aref buf (1- (incf j))) 0)))
                   (#_GlobalUnlock buf-handle)))
               buf-handle))))
      (when (zerop (#_OpenClipboard nil))
        (error "Cannot open clipboard"))
      (unwind-protect
           (progn
             (when (zerop (#_EmptyClipboard))
               (error "#_EmptyClipboard returned error code: ~S" (#_GetLastError)))
             (unless (#_SetClipboardData #_CF_UNICODETEXT buf-handle)
               (error "#_SetClipboardData doesn't like us")))
        (assert (not (zerop (#_CloseClipboard))))))))

(defun win32-copy-string-from-clipboard ()
  (unless (zerop (#_OpenClipboard nil)) ;zerop! a BOOL is not CL:BOOLEAN
    (unwind-protect
         (let* ((hglb (#_GetClipboardData #_CF_UNICODETEXT)))
           (when hglb                   ;C NULL is nil
             (let ((hglbCopy (#_GlobalLock hglb)))
               (when hglbCopy           ;same
                 (unwind-protect
                      ;; hglbCopy already is a void* and
                      ;; get-native-utf16-string is happy with it.
                      (get-native-utf16-string hglbCopy)
                   (#_GlobalUnlock hglb))))))
      (#_CloseClipboard)))) 

#+NIL
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
	    
    
