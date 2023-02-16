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

   (acquired-monitor-count
    :accessor acquired-monitor-count
    :initform nil)
   
   (captured-cursor-window
    :accessor captured-cursor-window
    :initform nil)
   
   (keycodes)
   (scancodes)
   (keynames)
   
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
  ((%cursor-pos-x :initform nil :accessor last-cursor-pos-x)
   (%cursor-pos-y :initform nil :accessor last-cursor-pos-y)
   (big-icon)
   (small-icon)
   (cursor-tracked? :type boolean :initform nil :accessor cursor-tracked?)
   (frame-action? :type boolean :initform nil :accessor frame-action?)
   (transparent? :type boolean :initform nil :accessor transparent?)
   (scale-to-monitor? :type boolean :initform nil :accessor scale-to-monitor?)
   (key-menu? :type boolean :initform nil :accessor key-menu?)
   (cursor :initform nil :accessor window-cursor)
   (high-surrogate)))

(defmethod initialize-instance :after ((instance win32:window-mixin) &rest initargs &key &allow-other-keys)
  (apply #'create-win32-window instance initargs))

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
