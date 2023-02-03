(in-package :clui)

(defclass win32:desktop-mixin (clui:display-mixin)
  ((instance
    :accessor win32-instance
    :initform (#_GetModuleHandle nil))

   (helper-window-handle)
   (helper-window-class)
   
   (main-window-class
    :accessor main-window-class
    :initform nil)

   (restore-cursor-pos-x
    :accessor restore-cursor-pos-x
    :initform nil)
   
   (restore-cursor-pos-y
    :accessor restore-cursor-pos-y
    :initform nil)
   
   (acquired-monitor-count
    :accessor acquired-monitor-count
    :initform nil)
   
   (keycodes)
   (scancodes)
   (keynames)

   (disabled-cursor-window
    :accessor disabled-cursor-window
    :initform nil)
   
   (captured-cursor-window
    :accessor captured-cursor-window
    :initform nil)
   
   (clipboard-string
    :accessor clipboard-string
    :initform "")
   
   (raw-input)
   
   (mouse-trail-size
    :accessor mouse-trail-size
    :initform 0)
   
   (device-notification-handle)))



(defclass win32:desktop-with-vulkan-mixin (clui:vulkan-support-mixin win32:desktop-mixin)
  ())

(defclass win32:desktop-with-opengl-mixin (clui:opengl-support-mixin win32:desktop-mixin)
  ())

(defclass win32:desktop-with-vulkan (win32:desktop-with-vulkan-mixin)
  ())

(defclass win32:desktop-with-opengl (win32:desktop-with-opengl-mixin)
  ())





(defclass win32:screen-mixin (clui:screen-mixin)
  ())


(defmethod initialize-instance ((instance win32:desktop-mixin) &rest initargs &key &allow-other-keys)
  (declare (ignorable initargs))

  (call-next-method)
  (win32-init instance)
  instance)



(defclass win32:monitor-mixin (clui:monitor-mixin handle-mixin)
  ((adapter-name :initarg :adapter-name :accessor adapter-name)
   (display-name :initarg :display-name :accessor display-name)
   (modes-pruned? :initarg :modes-pruned? :initform nil :type boolean :accessor modes-pruned?)
   (mode-changed? :initarg :mode-changed? :initform nil :type boolean :accessor mode-changed?)))

(defmethod initialize-instance ((instance win32:monitor-mixin) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (call-next-method))


(defclass win32:cursor-mixin (clui:cursor-mixin handle-mixin)
  ())

(defclass win32:window-mixin (clui:os-window-mixin handle-mixin)
  ((big-icon)
   (small-icon)
   (cursor-tracked? :type boolean :initform nil :accessor cursor-tracked?)
   (frame-action? :type boolean :initform nil :accessor frame-action?)
   (iconified? :type boolean :initform nil :accessor currently-iconified?)
   (maximized? :type boolean :initform nil :accessor currently-minimized?)
   (transparent? :type boolean :initform nil :accessor transparent?)
   (scale-to-monitor? :type boolean :initform nil :accessor scale-to-monitor?)
   (key-menu? :type boolean :initform nil :accessor key-menu?)
   (cursor :initform nil :accessor window-cursor)
   (last-cursor-pos-x)
   (last-cursor-pos-y)
   (high-surrogate)))

(defclass win32:vulkan-window-mixin (win32:window-mixin)
  ())

(defclass win32:wgl-window-mixin (win32:window-mixin)
  ())



(defclass win32:desktop (win32:desktop-mixin)
  ())

(defclass win32:screen (win32:screen-mixin)
  ())

(defclass win32:window (win32:window-mixin)
  ())

(defclass win32:cursor (win32:cursor-mixin)
  ())

(defclass win32:monitor (win32:monitor-mixin)
  ())

(defclass win32:vulkan-window (win32:vulkan-window-mixin)
  ())

(defclass win32:wgl-window (win32:wgl-window-mixin)
  ())
