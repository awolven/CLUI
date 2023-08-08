(in-package :clui)

#+SBCL
(defmacro noffi::callback (name)
  (eval `,name))

(defclass win32:desktop-mixin (clui:display-mixin)
  ((instance
    :accessor win32-instance
    :initform (#_GetModuleHandle nil))

   (helper-window-class
    :initform nil)
   
   (main-window-class
    :accessor main-window-class
    :initform nil)

   (acquired-monitor-count
    :accessor acquired-monitor-count
    :initform nil)
   
   (captured-cursor-window
    :accessor captured-cursor-window
    :initform nil)
   
   (raw-input)
   
   (mouse-trail-size
    :accessor mouse-trail-size
    :initform 0)
   
   (device-notification-handle
    :initform nil
    :accessor device-notification-handle)))

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

(defclass win32::arrow-cursor (arrow-cursor-mixin win32:cursor-mixin)
  ((handle :initform (create-win32-standard-cursor :arrow))))

(defclass win32::hand-cursor (hand-cursor-mixin win32:cursor-mixin)
  ((handle :initform (create-win32-standard-cursor :hand))))

(defclass win32::pointing-hand-cursor (pointing-hand-cursor-mixin win32:cursor-mixin)
  ())

(defclass win32::open-hand-cursor (open-hand-cursor-mixin win32:cursor-mixin)
  ())

(defclass win32::ibeam-cursor (ibeam-cursor-mixin win32:cursor-mixin)
  ((handle :initform (create-win32-standard-cursor :ibeam))))

(defclass win32::crosshair-cursor (crosshair-cursor-mixin win32:cursor-mixin)
  ((handle :initform (create-win32-standard-cursor :crosshair))))

(defclass win32::compass-cursor (compass-cursor-mixin win32:cursor-mixin)
  ((handle :initform (create-win32-standard-cursor :compass))))

(defclass win32::nwse-cursor (nwse-cursor-mixin win32:cursor-mixin)
  ((handle :initform (create-win32-standard-cursor :nwse))))

(defclass win32::nesw-cursor (nesw-cursor-mixin win32:cursor-mixin)
  ((handle :initform (create-win32-standard-cursor :nesw))))

(defclass win32::ew-cursor (ew-cursor-mixin win32:cursor-mixin)
  ((handle :initform (create-win32-standard-cursor :ew))))

(defclass win32::ns-cursor (ns-cursor-mixin win32:cursor-mixin)
  ((handle :initform (create-win32-standard-cursor :ns))))

(defclass win32::up-cursor (up-cursor-mixin win32:cursor-mixin)
  ((handle :initform (create-win32-standard-cursor :up))))

(defclass win32::down-cursor (down-cursor-mixin win32:cursor-mixin)
  ())

(defclass win32::wait-cursor (wait-cursor-mixin win32:cursor-mixin)
  ((handle :initform (create-win32-standard-cursor :wait))))

(defclass win32::not-allowed-cursor (not-allowed-cursor-mixin win32:cursor-mixin)
  ((handle :initform (create-win32-standard-cursor :not-allowed))))

(defclass win32:window-mixin (clui:os-window-mixin handle-mixin)
  ((%big-icon)
   
   (%small-icon)
   
   (%cursor-tracked?
    :type boolean
    :initform nil
    :accessor cursor-tracked?)
   
   (%frame-action?
    :type boolean
    :initform nil
    :accessor frame-action?)
   
   (%iconified?
    :type boolean
    :initform nil
    :accessor last-iconified?)
   
   (%maximized?
    :type boolean
    :initform nil
    :accessor last-maximized?)
   
   (%transparent?
    :type boolean
    :initform nil
    :accessor last-transparent?)
   
   (%scale-to-monitor?
    :type boolean
    :initform nil
    :accessor scale-to-monitor?)
   
   (%key-menu?
    :type boolean
    :initform nil
    :accessor key-menu?)

   (%pos-x
    :type (or null real)
    :initform 0
    :accessor last-pos-x)
   
   (%pos-y
    :type (or null real)
    :initform 0
    :accessor last-pos-y)

   (%width
    :type (or null real)
    :initform 0
    :accessor last-width)
   
   (%height
    :type (or null real)
    :initform 0
    :accessor last-height)
   
   (%cursor-pos-x
    :type (or null real)
    :initform nil
    :accessor last-cursor-pos-x)
   
   (%cursor-pos-y
    :type (or null real)
    :initform nil
    :accessor last-cursor-pos-y)
   
   (%high-surrogate
    :initform nil
    :accessor high-surrogate)))
		      

(defmethod initialize-instance :after ((instance win32:window-mixin) &rest initargs &key &allow-other-keys)
  (apply #'create-native-win32-window instance initargs))


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
