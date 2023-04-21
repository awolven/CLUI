(in-package :clui)
(named-readtables:in-readtable :objc-readtable)

(defclass objc-object-mixin ()
  ((id :initarg :ptr :accessor objc-object-id)))

(defclass ns-helper (objc-object-mixin)
  ())

(defvar *delegate->clos-window-table* (make-hash-table :test #'eq))
(defvar *content-view->clos-content-view-table* (make-hash-table :test #'eq))


(defclass cocoa:desktop-mixin (clui:display-mixin)
  ((kPropertyUnicodeKeyLayoutData)
   (LMGetKbdType)
   (TISCopyCurrentKeyboardLayoutInputSource)
   (TISGetInputSourceProperty)

   (window-controller-class
    :accessor objc-window-controller-class
    :initform (make-window-controller-class))

   (window-controller
    :accessor display-window-controller
    :initform nil)    
   
   (helper-class
    :accessor objc-helper-class
    :initform (make-helper-class))
   
   (helper :initform nil
	   :accessor application-helper)
   
   (application-delegate-class
    :accessor objc-application-delegate-class
    :initform (make-application-delegate-class))
   
   (window-class
    :accessor objc-window-class
    :initform (make-window-class))
   
   (window-delegate-class
    :accessor objc-window-delegate-class
    :initform (make-window-delegate-class))
   
   (content-view-class
    :accessor objc-content-view-class
    :initform (apply #'make-content-view-class (when (find-package '%vk)
						 (list #@MTKView))))

   (delegate :accessor application-delegate)

   (cursor-hidden?
    :initform nil
    :accessor cursor-hidden?)

   (cascade-point :initform (make-nspoint 0 0) :accessor cascade-point)
   
   (event-source :accessor desktop-event-source)

   (keynames)
   (keycodes)
   (scancodes)

   (key-up-monitor :accessor key-up-monitor)
   (unicode-data :initform nil :accessor desktop-unicode-data)
   (input-source :initform nil :accessor tis-input-source)
   (tis-bundle :initform nil :accessor tis-bundle)
   (hid-manager)
   (nib-objects)))

(defmethod objc-object-id ((app cocoa:desktop-mixin))
  objc-runtime::ns-app)

(defmethod initialize-instance ((instance cocoa:desktop-mixin) &rest initargs &key &allow-other-keys)
  (declare (ignorable initargs))
  (call-next-method)
  (init-cocoa instance)
  instance)

(defclass application-delegate (obj-object-mixin)
  ())

(defclass cocoa:screen-mixin (clui:screen-mixin objc-object-mixin)
  ())


(defclass cocoa:monitor-mixin (clui:monitor-mixin)
  ((display-id :initarg  :display-id
	       :accessor monitor-display-id)
   
   (previous-video-mode :initform nil
			:accessor monitor-previous-video-mode)
   
   (unit-number   :initarg  :unit-number
		  :reader   monitor-unit-number)
   
   (screen        :initarg  :screen
		  :accessor monitor-screen)
   
   (fallback-refresh-rate :initform 0.0d0
			  :accessor monitor-fallback-refresh-rate)))

(defmethod initialize-instance ((instance cocoa:monitor-mixin) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (call-next-method))


(defclass cocoa:cursor-mixin (clui:cursor-mixin)
  ((object)))

(defclass cocoa:window-mixin (clui:os-window-mixin objc-object-mixin)
  ((delegate
    :accessor
    window-delegate)
   
   (view
    :accessor
    window-content-view)
   
   (context
    :accessor window-graphics-context)
   
   (%maximized?
    :type boolean
    :initform nil
    :accessor last-maximized?)
   
   (%occluded?
    :type boolean
    :initform nil
    :accessor last-occluded?)
   
   (retina?
    :type boolean
    :initform nil
    :accessor window-retina?)
   
   (%width
    :type real
    :initform 0
    :accessor last-width)
   
   (%height
    :type real
    :initform 0
    :accessor last-height)
   
   (%fbwidth
    :type real
    :initform 0
    :accessor last-fbwidth)
   
   (%fbheight
    :type real
    :initform 0
    :accessor last-fbheight)
   
   (%xscale
    :type real
    :initform 0
    :accessor last-xscale)
   
   (%yscale
    :type real
    :initform 0
    :accessor last-yscale)
   
   (%cursor-warp-delta-x
    :type real
    :accessor cursor-warp-delta-x)
   
   (%cursor-warp-delta-y
    :type real
    :accessor cursor-warp-delta-y)))

(defclass cocoa::helper-window ()
  ((delegate
    :accessor
    window-delegate)
   
   (view
    :accessor
    window-content-view)))

(defclass cocoa:metal-window-mixin (cocoa:window-mixin)
  ((layer :accessor window-layer)))

(defclass cocoa:nsgl-window-mixin (cocoa:window-mixin)
  ())

(defclass window-delegate (objc-object-mixin)
  ((owner :initarg :owner :accessor window-delegate-owner)))

(defclass content-view (objc-object-mixin)
  ((owner :initarg :owner :accessor content-view-owner)
   (tracking-area :initform nil :accessor content-view-tracking-area)
   (marked-text :initarg :marked-text :accessor content-view-marked-text)))


(defmethod initialize-instance :after ((instance cocoa:window-mixin) &rest initargs &key &allow-other-keys)
  (apply #'create-native-cocoa-window instance initargs))

(defmethod initialize-instance :after ((instance window-delegate) &rest initargs
				       &key (owner (warn ":owner not passed to make-instance of window-delegate"))
					 &allow-other-keys)
  (declare (ignorable initargs))
  (when owner
    (setf (gethash (sap-int (objc-object-id instance)) *delegate->clos-window-table*)
	  owner))
  (values))

(defmethod initialize-instance :after ((instance content-view) &rest initargs &key &allow-other-keys)
  (declare (ignorable initargs))
  (setf (gethash (sap-int (objc-object-id instance)) *content-view->clos-content-view-table*)
	instance)
  (values))


(defclass cocoa:desktop (cocoa:desktop-mixin)
  ())
			  
(defclass cocoa:screen (cocoa:screen-mixin)
  ())

(defclass cocoa:window (cocoa:window-mixin)
  ())

(defclass cocoa:cursor (cocoa:cursor-mixin)
  ())

(defclass cocoa:monitor (cocoa:monitor-mixin)
  ())

(defclass cocoa:metal-window (cocoa:metal-window-mixin)
  ())

(defclass cocoa:vulkan-window-mixin (vulkan-window-mixin cocoa:metal-window-mixin)
  ())

(defclass cocoa:vulkan-window (cocoa:vulkan-window-mixin)
  ())

(defclass cocoa:nsgl-window (opengl-window-mixin cocoa:nsgl-window-mixin)
  ())



