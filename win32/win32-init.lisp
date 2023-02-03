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

(defun win32-init (display-platform)
  ;;(load-win32-libraries)
  ;;(create-key-tables)
  ;;(update-win32-key-names)

  (cond ((windows-10-version-1703-or-greater?)
	 (#_SetProcessDpiAwarenessContext
	  #_DPI_AWARENESS_CONTEXT_PER_MONITOR_AWARE_V2))
	((windows-8.1-or-greater?)
	 (#_SetProcessAwareness #_PROCESS_PER_MONITOR_DPI_AWARE))
	#+NOTYET
	((windows-vista-or-greater?)
	 (#_SepProcessDPIAware)))

  t)

  
  
  
