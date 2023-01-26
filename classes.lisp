(in-package :abstract-os)

(defun sap-int (sap)
  #+sbcl(sb-sys:sap-int sap)
  #+ccl(ccl::%ptr-to-int sap))

(defclass application-mixin (#+windows win32-application-mixin
			     #+x11 x11-application-mixin
			     #+wayland wayland-application-mixin
			     #+darwin ns-application-mixin)
  ((name :accessor application-name)
   (exit? :initform nil :accessor application-exit?)
   (window-list-head :initform nil :accessor application-window-list-head)
   (monitors :accessor application-monitors)))

(defclass abstract-os-application (application-mixin)
  ())

(defmethod initialize-instance :before ((instance application-mixin)
					&rest initargs
					&key (name "Abstract OS Application")
					  &allow-other-keys)
					
  (declare (ignore initargs))
  (setq *app* instance)
  (setf (application-name instance) (or name "Abstract OS Application"))
  #+darwin(init-cocoa instance)
  ;;#+windows(init-win32 instance)
  #+linux(init-linux instance)
  (values))

(defstruct gamma-ramp
  (red (make-array 8 :element-type '(unsigned-byte 16) :adjustable t :fill-pointer 0))
  (green (make-array 8 :element-type '(unsigned-byte 16) :adjustable t :fill-pointer 0))
  (blue (make-array 8 :element-type '(unsigned-byte 16) :adjustable t :fill-pointer 0)))

(defclass monitor-mixin (#+windows win32-monitor-mixin
			 #+x11 x11-monitor
			 #+darwin ns-monitor-mixin)
  ((name :initarg :name :reader monitor-name)
   (width-mm :initarg :width-mm :reader monitor-width-mm)
   (height-mm :initarg :height-mm :reader monitor-height-mm)
   ;; windows who's video mode is current on this monitor
   (window :accessor monitor-window :initform nil)
   (modes :initform nil :accessor monitor-modes)
   (current-mode :initform nil :accessor monitor-current-mode)
   (original-ramp :initform nil :accessor monitor-original-ramp)
   (current-ramp :initform nil :accessor monitor-current-ramp)))

(defclass monitor (monitor-mixin) ())

(defclass cursor-mixin (#+windows win32-cursor-mixin
			#+x11 x11-cursor
			#+darwin ns-cursor-mixin)
  ())

(defclass rect-mixin ()
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

(defclass region (rect-mixin)
  ())

(defclass hints ()
  ((refresh-rate :accessor hints-refresh-rate :initform 120)))

(defstruct video-mode
  (width)
  (height)
  (red-bits)
  (green-bits)
  (blue-bits)
  (refresh-rate))

(defclass window-mixin (rect-mixin)
  ((next :type (or null window-mixin) :accessor window-next)
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
   (monitor :initform nil :accessor window-monitor)
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
   (virtual-cursor-pos-y :accessor virtual-cursor-pos-y)
   (fully-created? :type boolean :accessor window-fully-created?)))
		  

(defclass os-window-mixin (window-mixin
			   #+windows win32-window-mixin
			   #+x11 x11-window-mixin
			   #+wayland wayland-window-mixin
			   #+darwin ns-window-mixin)
  ())

(defclass os-window-with-framebuffer-mixin (os-window-mixin)
  ((xscale :accessor window-xscale)
   (yscale :accessor window-yscale)))

(defmethod initialize-instance :before ((window os-window-mixin)
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
  (setf (window-fully-created? window) nil)
  (apply #'initialize-os-window window initargs)
  (values))

(defmethod initialize-instance :after ((window os-window-mixin) &rest initargs)
  (setf (window-fully-created? window) t))
	   

(defclass os-window-with-framebuffer (os-window-with-framebuffer-mixin)
  ())

(defclass os-window (os-window-mixin)
  ())

(defclass constant-refresh-os-window-mixin ()
  ())

(defclass constant-refresh-os-window-with-framebuffer-mixin (constant-refresh-os-window-mixin
							     os-window-with-framebuffer-mixin)
  ())

#+darwin
(defclass metal-window-mixin (constant-refresh-os-window-with-framebuffer-mixin
			      ns-metal-window-mixin)
			      
  ())

(defclass vulkan-window-mixin (#-darwin constant-refresh-os-window-with-framebuffer-mixin
					#+darwin metal-window-mixin
					#+windows win32-vulkan-window-mixin
					#+linux linux-vulkan-window-mixin)
  ())

(defclass opengl-window-mixin (constant-refresh-os-window-with-framebuffer-mixin)
  ())


