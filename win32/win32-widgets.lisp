(in-package :clui)

(defclass view-mixin (window-mixin)
  ())

(defclass os-view-mixin (view-mixin)
  ())

(defvar *current-view-id* 1000)

(defun gen-view-id ()
  (incf *current-view-id*))

(defclass widget-mixin ()
  ())

(defclass button-mixin (widget-mixin)
  ())

(defclass win32::view-mixin (os-view-mixin handle-mixin)
  ((id :accessor view-id :initform (gen-view-id))))

(defmethod initialize-instance :after ((instance win32::view-mixin) &rest initargs
				       &key parent &allow-other-keys)
  (unless parent
    (error "a view must be initialized with a :parent."))
  (setf (gethash (view-id instance) *view-table*) instance)
  (apply #'%create-native-win32-window instance initargs)
  (values))

(defclass win32::widget-mixin (widget-mixin win32::view-mixin)
  ())

(defclass win32::button-mixin (button-mixin win32::widget-mixin)
  ())

(defmethod %create-native-win32-window ((button win32::button-mixin)
					&rest args
					&key
					  parent
					  (xpos 10) (ypos 10)
					  (width 100) (height 50)
					  (label "CLICK"))
  (declare (ignore args))
  (setf (window-parent button) parent)
  (setf (window-root button) (window-root parent))
  (clui::with-lpcwstr (pclass "BUTTON")
    (clui::with-lpcwstr (plabel label)
      (clet ((hinst #_<HINSTANCE> (cons-cval (#_GetWindowLongPtr (h parent) #_GWLP_HINSTANCE) #_<long long>)))
	(setf (h button)
	      (#_CreateWindow pclass
			      plabel
			      (logior #_WS_TABSTOP #_WS_VISIBLE #_WS_CHILD
				      #_BS_DEFPUSHBUTTON)
			      xpos
			      ypos
			      width
			      height
			      (h parent)
			      (c-cast '#_<HMENU> (cons-cval (view-id button) '#_<long long>))
			      hinst
			      nil))
	
	button))))

(defclass combobox-mixin (widget-mixin)
  ())

(defclass win32::combobox-mixin (combobox-mixin win32::widget-mixin)
  ())

(defmethod %create-native-win32-window ((button win32::combobox-mixin)
					&rest args
					&key
					  parent
					  (xpos 10) (ypos 10)
					  (width 100) (height 50)
					  (label "CLICK"))
  (declare (ignore args))
  (setf (window-parent button) parent)
  (setf (window-root button) (window-root parent))
  (clui::with-lpcwstr (pclass "COMBOBOX")
    (clui::with-lpcwstr (plabel label)
      (clet ((hinst #_<HINSTANCE> (cons-cval (#_GetWindowLongPtr (h parent) #_GWLP_HINSTANCE) #_<long long>)))
	(setf (h button)
	      (#_CreateWindow pclass
			      plabel
			      (logior #_WS_TABSTOP #_WS_VISIBLE #_WS_CHILD
				      #_CBS_DROPDOWNLIST)
			      xpos
			      ypos
			      width
			      height
			      (h parent)
			      nil
			      hinst
			      nil))
	
	button))))

(defun win32::combobox-add-string (cb string)
  (with-lpcwstr (str string)
    (#_ComboBox_AddString (h cb) str)))

(defclass text-edit-box-mixin ()
  ())

(defclass win32::text-edit-box-mixin (text-edit-box-mixin win32::widget-mixin)
  ())
(defmethod %create-native-win32-window ((button win32::text-edit-box-mixin)
					&rest args
					&key
					  parent
					  (xpos 10) (ypos 10)
					  (width 100) (height 25)
					  (initial-text "")
					  (hscroll? nil))
  (declare (ignore args))
  (setf (window-parent button) parent)
  (setf (window-root button) (window-root parent))
  (clui::with-lpcwstr (pclass "edit")
    (clui::with-lpcwstr (pinitial-text initial-text)
      (clet ((hinst #_<HINSTANCE> (cons-cval (#_GetWindowLongPtr (h parent) #_GWLP_HINSTANCE) #_<long long>)))
	(setf (h button)
	      (#_CreateWindow pclass
			      pinitial-text
			      (logior #_WS_TABSTOP #_WS_VISIBLE #_WS_CHILD
				      #_WS_BORDER
				      (if hscroll? #_WS_HSCROLL 0)
				      #_ES_AUTOHSCROLL)
			      xpos
			      ypos
			      width
			      height
			      (h parent)
			      nil
			      hinst
			      nil))
	
	button))))

(defclass multiline-text-edit-box-mixin ()
  ())

(defclass win32::multiline-text-edit-box-mixin (multiline-text-edit-box-mixin win32::widget-mixin)
  ())

(defmethod %create-native-win32-window ((button win32::multiline-text-edit-box-mixin)
					&rest args
					&key
					  parent
					  (xpos 10) (ypos 10)
					  (width 300) (height 150)
					  (initial-text "")
					  (hscroll? nil)
					  (vscroll? nil))
  (declare (ignore args))
  (setf (window-parent button) parent)
  (setf (window-root button) (window-root parent))
  (clui::with-lpcwstr (pclass "edit")
    (clui::with-lpcwstr (pinitial-text initial-text)
      (clet ((hinst #_<HINSTANCE> (cons-cval (#_GetWindowLongPtr (h parent) #_GWLP_HINSTANCE) #_<long long>)))
	(setf (h button)
	      (#_CreateWindow pclass
			      pinitial-text
			      (logior #_WS_TABSTOP #_WS_VISIBLE #_WS_CHILD
				      #_WS_BORDER
				      (if hscroll? #_WS_HSCROLL 0)
				      (if vscroll? #_WS_VSCROLL 0)
				      #_ES_MULTILINE #_ES_AUTOHSCROLL)
			      xpos
			      ypos
			      width
			      height
			      (h parent)
			      nil
			      hinst
			      nil))
	
	button))))

(defclass my-window (win32:window)
  ((views :initform nil :accessor window-views)))

(defun add-menus (hwnd)
  (let* ((h-menu-bar (#_CreateMenu))
	 (h-file-menu (#_CreatePopupMenu))
	 (h-help-menu (#_CreatePopupMenu)))

    (#_AppendMenu h-file-menu #_MF_STRING ID_FILE_EXIT "E&xit")
    (#_AppendMenu h-help-menu #_MF_STRING ID_HELP_ABOUT "&About")
    (#_AppendMenu h-menu-bar #_MF_POPUP h-file-menu "&File")
    (#_AppendMenu h-menu-bar #_MF_POPUP h-help-menu "&Help")
    (#_SetMenu hwnd h-menu-bar)
    (values)))

(defun test ()
  (let* ((win (make-instance 'my-window :root (default-screen (default-display))
			     :vscroll? t))
	 (b (make-instance 'win32::button-mixin :parent win))
	 (cb (make-instance 'win32::combobox-mixin :parent win :ypos 70))
	 (mle (make-instance 'win32::multiline-text-edit-box-mixin :parent win :ypos 120
								   :height 200 :width 400))
	 (te (make-instance 'win32::text-edit-box-mixin :parent win :ypos 330)))
	      
    (declare (ignorable b))
    (setq w win)
    (add-menus (h win))
    (pushnew b (window-views win))
    (pushnew cb (window-views win))
    (pushnew mle (window-views win))
    (pushnew te (window-views win))
    (win32::combobox-add-string cb "FOO")
    (run)))


