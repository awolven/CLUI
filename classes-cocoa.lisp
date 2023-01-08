(in-package :abstract-os)
(named-readtables:in-readtable :objc-readtable)

(defclass ns-object-mixin ()
  ((ptr :initarg :ptr :accessor ns-object-ptr)))

(defclass ns-helper (ns-object-mixin)
  ())

(defclass ns-application-mixin (ns-object-mixin)
  ((helper-class :accessor helper-class)
   (application-delegate-class :accessor application-delegate-class)
   (window-class :accessor window-class)
   (window-delegate-class :accessor window-delegate-class)
   (content-view-class :accessor content-view-class)
   
   (delegate->clos-window-table	:accessor delegate->clos-window-table)
   (content-view->clos-content-view-table :accessor content-view->clos-content-view-table)
   
   (event-source :accessor application-event-source)
   (delegate :accessor application-delegate)
   (cursor-hidden?)
   (input-source :initform nil :accessor tis-input-source)
   (hid-manager)
   (unicode-data :initform nil :accessor application-unicode-data)
   (helper :initform nil :accessor application-helper)
   (key-up-monitor :accessor key-up-monitor)
   (nib-objects)
   (keynames)
   (keycodes)
   (scancodes)
   (clipboard-string)
   (cascade-point :initform (list 'y 0.0d0 'x 0.0d0) :accessor cascade-point)
   (restore-cursor-position-x)
   (restore-cursor-position-y)
   (disabled-cursor-window :accessor disabled-cursor-window)
   (tis-bundle :initform nil :accessor tis-bundle)
   (kTISPropertyUnicodeKeyLayoutData)
   (TISCopyCurrentKeyboardLayoutInputSource)
   (TISGetInputSourceProperty)
   (LMGetKeyboardType)
   
   
   ))

(defclass application-delegate (ns-object-mixin)
  ())

(defclass ns-monitor-mixin ()
  ((display-id :accessor monitor-display-id)
   (previous-mode)
   (unit-number)
   (screen)
   (fallback-refresh-rate :initform 0.0d0 :accessor monitor-fallback-refresh-rate)))


(defclass ns-cursor-mixin ()
  ((object)))

(defclass ns-object-mixin ()
  ((ptr :initarg :ptr :accessor ns-object-ptr)))

(defclass ns-window-mixin (essential-rect-mixin ns-object-mixin)
  ((delegate :accessor window-delegate)
   (view :accessor window-content-view)
   (layer)
   (content :accessor window-graphics-context)
   (maximized? :initform nil :accessor maximized?)
   (occluded? :initform nil :accessor occluded?)
   (retina? :initform nil :accessor window-retina?)
   (fbwidth :initform nil :accessor window-fb-width)
   (fbheight :initform nil :accessor window-fb-height)
   (xscale :accessor window-x-scale)
   (yscale :accessor window-y-scale)
   (cursor-warp-delta-x :accessor cursor-warp-delta-x)
   (cursor-warp-delta-y :accessor cursor-warp-delta-y)))

(defclass window-delegate (ns-object-mixin)
  ((owner :initarg :owner :accessor window-delegate-owner)))

(defclass content-view (ns-object-mixin)
  ((owner :initarg :owner :accessor content-view-owner)
   (tracking-area :initform nil :accessor content-view-tracking-area)
   (marked-text :initarg :marked-text :reader content-view-marked-text)))


(defmethod initialize-instance :after ((instance ns-window-mixin) &rest initargs &key &allow-other-keys)
  (apply #'create-cocoa-window instance initargs))

(defmethod initialize-instance :after ((instance window-delegate) &rest initargs
				       &key (owner (warn ":owner not passed to make-instance of window-delegate"))
					 &allow-other-keys)
  (declare (ignorable initargs))
  (when owner
    (setf (gethash (sap-int (ns-object-ptr instance))
		   (delegate->clos-window-table *app*))
	  owner))
  (values))

(defmethod initialize-instance :after ((instance content-view) &rest initargs &key &allow-other-keys)
  (declare (ignorable initargs))
  (setf (gethash (sap-int (ns-object-ptr instance))
		 (content-view->clos-content-view-table *app*))
	instance)
  (values))
  
(defclass ns-screen (ns-object-mixin)
  ())
