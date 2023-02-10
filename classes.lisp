(in-package :clui)

(defun sap-int (sap)
  #+sbcl(sb-sys:sap-int sap)
  #+ccl(ccl::%ptr-to-int sap))

(defvar *displays* ())

(defmacro get-displays ()
  ;; this way we can read, setf, and push (get-displays)
  `*displays*)

(defun default-display ()
  (let ((displays (get-displays)))
    (if displays
	(first displays)
	(apply #'make-instance 'display
	       #+win32 #+win32 :win32 t
	       #+cocoa #+cocoa :cocoa t
	       #+x11 #+x11 :x11 t
	       (append
		(when (find-package :%vk)
		  (list :vulkan t))
		#+darwin
		(when (find-package :%mtl)
		  (list :metal t))
		(when (find-package :%gl)
		  (list :opengl t)))))))

(defclass operating-system-mixin ()
  ())

(defclass unix-mixin (operating-system-mixin)
  ())

(defclass linux-mixin (unix-mixin)
  ())

(defclass bsd-mixin (unix-mixin)
  ())

(defclass ms-windows-mixin (operating-system-mixin)
  ())

(defclass macos-mixin (unix-mixin)
  ())

(defclass gui-mixin ()
  ())

(defclass x11-mixin (gui-mixin)
  ())

(defclass win32-mixin (gui-mixin)
  ())

(defclass cocoa-mixin (gui-mixin)
  ())

(defclass wayland-mixin (gui-mixin)
  ())

(defclass vulkan-support-mixin ()
  ())

(defclass opengl-support-mixin ()
  ())

(defclass display-mixin ()
  ((window-list-head
    :accessor display-window-list-head
    :initform nil)
   
   (monitors
    :accessor display-monitors
    :initform ())))

(defclass screen-mixin ()
  ())

(defclass handle-mixin ()
  ((handle :accessor h)))

(defmethod initialize-instance ((instance display-mixin)
				&rest initargs
				&key
				&allow-other-keys)
					
  (declare (ignore initargs))
  (call-next-method)
  (poll-monitors instance)
  (push instance (get-displays))  
  instance)

(defstruct gamma-ramp
  (red (make-array 8 :element-type '(unsigned-byte 16) :adjustable t :fill-pointer 0))
  (green (make-array 8 :element-type '(unsigned-byte 16) :adjustable t :fill-pointer 0))
  (blue (make-array 8 :element-type '(unsigned-byte 16) :adjustable t :fill-pointer 0)))

(defclass monitor-mixin ()
  ((name :initarg :name :reader monitor-name)
   (width-mm :initarg :width-mm :reader monitor-width-mm)
   (height-mm :initarg :height-mm :reader monitor-height-mm)
   ;; windows who's video mode is current on this monitor
   (window :accessor monitor-window :initform nil)
   (modes :initform nil :accessor monitor-modes)
   (current-mode :initform nil :accessor monitor-current-mode)
   (original-ramp :initform nil :accessor monitor-original-ramp)
   (current-ramp :initform nil :accessor monitor-current-ramp)))

(defclass cursor-mixin ()
  ())

(defclass region-mixin ()
  ())

(defclass rect-mixin (region-mixin)
  ((x :initarg :x
      :accessor x
      :type real)
   (y :initarg :y
      :accessor y
      :type real)
   (width :initarg :width
	  :accessor width
	  :type real)
   (height :initarg :height
	   :accessor height
	   :type real)))

(defstruct video-mode
  (width)
  (height)
  (red-bits)
  (green-bits)
  (blue-bits)
  (refresh-rate))

(defclass image-mixin ()
  ((width :initarg :width
	  :accessor image-width
	  :type real)
   (height :initarg :height
	   :accessor image-height
	   :type real)
   (pixels :initarg :pixels
	   :accessor image-pixels)))

(defclass basic-image (image-mixin)
  ())

(defmethod make-image (width height pixels)
  (make-instance 'basic-image :width width :height height :pixels pixels)) 
  
    

(defclass window-mixin (rect-mixin)
  ((display :initarg :display :reader window-display)
   (next :type (or null window-mixin) :accessor window-next)
   (maximized? :type boolean :initform nil :accessor currently-maximized?)
   (resizable? :type boolean :initform nil :accessor currently-resizable?)
   (decorated? :type boolean :initform nil :accessor currently-decorated?)
   (iconified? :type boolean :initform nil :accessor currently-iconified?)
   (auto-iconify? :type boolean :initform nil :accessor auto-iconify?)
   (floating? :type boolean :initform nil :accessor currently-floating?)
   (focus-on-show? :type boolean :initform t :accessor focus-on-show?)
   (mouse-passthrough? :type boolean :accessor mouse-passthrough?)
   (should-close? :type boolean :initform nil :accessor should-close?)
   (raw-mouse-motion? :type boolean :initform nil :accessor window-raw-mouse-motion?)
   (video-mode :accessor window-video-mode :initform (make-video-mode))
   (monitor :initform nil :reader window-monitor :writer (setf %window-monitor))
   (cursor)
   (min-width :initform :dont-care :accessor window-min-width)
   (min-heigh :initform :dont-care :accessor window-min-height)
   (max-width :initform :dont-care :accessor window-max-width)
   (max-height :initform :dont-care :accessor window-max-height)
   (numer :initform :dont-care :accessor window-aspect-numer)
   (denom :initform :dont-care :accessor window-aspect-denom)
   (sticky-keys? :type boolean :initform nil)
   (sticky-mouse-buttons? :type boolean :initform nil)
   (lock-key-modes? :type boolean :initform nil)
   (cursor-mode :initform :normal :accessor window-cursor-mode)
   (mouse-buttons)
   (keys)
   (virtual-cursor-pos-x :accessor virtual-cursor-pos-x)
   (virtual-cursor-pos-y :accessor virtual-cursor-pos-y))
  (:default-initargs :display (default-display)))
		  

(defclass os-window-mixin (window-mixin)
  ())

(defclass homemade-window-mixin (window-mixin)
  ())

(defclass homemade-view-mixin (homemade-window-mixin)
  ())

(defclass os-window-with-framebuffer-mixin (os-window-mixin)
  ((xscale :accessor window-xscale)
   (yscale :accessor window-yscale)))

(defclass vulkan-window-mixin (os-window-with-framebuffer-mixin)
  ())

(defclass opengl-window-mixin (os-window-with-framebuffer-mixin)
  ())

(defmethod initialize-instance ((window os-window-mixin)
				       &rest initargs
				       &key xpos ypos
					 width height
					 title
					 visible?
					 focused?
					 scale-to-monitor?
					 maximized?
					 resizable?
					 decorated?
					 floating?
					 transparent?
					 auto-iconify?
					 focus-on-show?
					 center-cursor?
					 mouse-passthrough?
					 monitor
					 share
					 frame-name
					 key-menu
					 clear-color
					 retina?
					 &allow-other-keys)
  
  (declare (ignorable xpos ypos width height title visible? focused?
		      scale-to-monitor? maximized? resizable?
		      decorated? floating? transparent? auto-iconify?
		      focus-on-show? center-cursor? mouse-passthrough?
		      monitor share frame-name key-menu clear-color
		      retina?))
  (apply #'initialize-os-window window initargs)
  (call-next-method)
  window)

(defclass constant-refresh-os-window-mixin ()
  ())

(defclass constant-refresh-os-window-with-framebuffer-mixin (constant-refresh-os-window-mixin
							     os-window-with-framebuffer-mixin)
  ())
