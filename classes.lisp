(in-package :clui)

#+NIL
(defun sap-int (sap)
  #+sbcl(sb-sys:sap-int sap)
  #+ccl(ccl::%ptr-to-int sap))

#+NIL
(defun int-sap (int)
  #+sbcl(sb-sys:int-sap int)
  #+ccl(ccl::%int-to-ptr int))

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

(defclass display-mixin (clim:port)
  ((window-list-head
    :accessor display-window-list-head
    :initform nil)

   (screens :initform nil
	    :accessor display-screens)
   
   (monitors
    :accessor display-monitors
    :initform ())

   (helper-window :initform nil
		  :accessor helper-window)

   (restore-cursor-pos-x :initform nil
			 :accessor restore-cursor-pos-x)

   (restore-cursor-pos-y :initform nil
			 :accessor restore-cursor-pos-y)

   (disabled-cursor-window
    :accessor disabled-cursor-window
    :initform nil)

   (keycodes :initform (make-array 512 :initial-element nil)
	     :reader display-keycodes)

   (scancodes :initform (make-array 256 :initial-element nil)
	      :reader display-scancodes)

   (last-event :initform nil
	       :accessor display-last-event)
   
   (exit? :initform nil
	  :accessor run-loop-exit?)))
			

(defun default-screen (display)
  (first (display-screens display)))

(defun (setf default-screen) (screen display)
  (progn
    (push screen (display-screens display))
    screen))

(defclass screen-mixin ()
  ((display :initarg :display
	    :reader screen-display
	    :reader window-display)))

(defmethod window-root ((screen screen-mixin))
  screen)

(defclass handle-mixin ()
  ((handle :initarg :h :initarg :handle :accessor h)))

(defmethod initialize-instance ((instance display-mixin)
				&rest initargs
				&key
				&allow-other-keys)
					
  (declare (ignore initargs))
  (push instance (get-displays)) ;; we do this first because some callbacks need *displays* set
  (handler-case (call-next-method)
    (error () (pop (get-displays))))
  
  instance)

(defmethod initialize-instance :after ((instance display-mixin)
				       &rest initargs
				       &key
				       &allow-other-keys)
  (declare (ignore initargs))
  (poll-monitors instance)
  (values))


(defstruct gamma-ramp
  (red)
  (green)
  (blue))

(defun gamma-ramp-size (gamma-ramp)
  (length (gamma-ramp-red gamma-ramp)))

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

(defmethod initialize-instance :after ((cursor cursor-mixin) &rest initargs &key display)
  (declare (ignore initargs display))
  ;; make :display an authorized initarg for cursor, even if there is no slot.
  (values))

(defclass arrow-cursor-mixin (cursor-mixin)
  ())

(defclass hand-cursor-mixin (cursor-mixin)
  ())

(defclass pointing-hand-cursor-mixin (cursor-mixin)
  ())

(defclass open-hand-cursor-mixin (cursor-mixin)
  ())

(defclass closed-hand-cursor-mixin (cursor-mixin)
  ())

(defclass ibeam-cursor-mixin (cursor-mixin)
  ())

(defclass crosshair-cursor-mixin (cursor-mixin)
  ())

(defclass compass-cursor-mixin (cursor-mixin)
  ())

(defclass nwse-cursor-mixin (cursor-mixin)
  ())

(defclass nesw-cursor-mixin (cursor-mixin)
  ())

(defclass ew-cursor-mixin (cursor-mixin)
  ())

(defclass ns-cursor-mixin (cursor-mixin)
  ())

(defclass resize-n-cursor-mixin (cursor-mixin)
  ())

(defclass resize-s-cursor-mixin (cursor-mixin)
  ())

(defclass resize-e-cursor-mixin (cursor-mixin)
  ())

(defclass resize-w-cursor-mixin (cursor-mixin)
  ())

(defclass resize-nw-cursor-mixin (cursor-mixin)
  ())

(defclass resize-ne-cursor-mixin (cursor-mixin)
  ())

(defclass resize-sw-cursor-mixin (cursor-mixin)
  ())

(defclass resize-se-cursor-mixin (cursor-mixin)
  ())

(defclass up-cursor-mixin (cursor-mixin)
  ())

(defclass down-cursor-mixin (cursor-mixin)
  ())

(defclass wait-cursor-mixin (cursor-mixin)
  ())

(defclass not-allowed-cursor-mixin (cursor-mixin)
  ())

(defclass region-mixin ()
  ())

(defclass rect-mixin (region-mixin)
  ())

(defclass rect (rect-mixin)
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
  ((display
    :initarg :display
    :reader window-display)
   
   (parent
    :initarg :parent
    :accessor window-parent)
   
   (root
    :initarg :root
    :accessor window-root)
   
   (min-width
    :initform :dont-care
    :accessor window-min-width)
   
   (min-height
    :initform :dont-care
    :accessor window-min-height)
   
   (max-width
    :initform :dont-care
    :accessor window-max-width)
   
   (max-height
    :initform :dont-care
    :accessor window-max-height)
   
   (numer
    :initform :dont-care
    :accessor window-aspect-numer)
   
   (denom
    :initform :dont-care
    :accessor window-aspect-denom))
  
  (:default-initargs :display (default-display)))

(defmethod window-p ((window window-mixin))
  t)

(defmethod window-p (object)
  (declare (ignore object))
  nil)

(defclass os-window-mixin (window-mixin clim:mirrored-sheet-mixin)
  ((next
    :type (or null window-mixin)
    :accessor window-next)
   
   (%resizable?
    :type boolean
    :initform nil
    :accessor last-resizable?)
   
   (%decorated?
    :type boolean
    :initform nil
    :accessor last-decorated?)
   
   (auto-iconify?
    :type boolean
    :initform nil
    :accessor auto-iconify?)
   
   (%floating?
    :type boolean
    :initform nil
    :accessor last-floating?)
   
   (focus-on-show?
    :type boolean
    :initform t
    :accessor focus-on-show?)
   
   (mouse-passthrough?
    :type boolean
    :accessor mouse-passthrough?)
   
   (should-close?
    :type boolean
    :initform nil
    :accessor should-close?)

   (video-mode
    :accessor window-video-mode
    :initform (make-video-mode))
   
   (monitor
    :initform nil
    :reader window-monitor
    :writer (setf %window-monitor))
   
   (cursor
    :initform nil
    :writer (setf %window-cursor)
    :reader window-cursor)

   (previous-cursor
    :initform nil
    :accessor window-previous-cursor)

   (sticky-keys?
    :type boolean
    :initform nil
    :accessor sticky-keys?)
   
   (sticky-mouse-buttons?
    :type boolean
    :initform nil
    :accessor sticky-mouse-buttons?)
   
   (lock-key-mods?
    :type boolean
    :initform nil
    :accessor lock-key-mods?)
   
   (cursor-mode
    :initform
    :normal
    :accessor window-cursor-mode)
   
   (keys
    :initform (make-array 256 :initial-element nil)
    :accessor window-keys)
   
   (virtual-cursor-pos-x
    :accessor virtual-cursor-pos-x)
   
   (virtual-cursor-pos-y
    :accessor virtual-cursor-pos-y)
   
   (raw-mouse-motion?
    :type boolean
    :initform nil
    :reader raw-mouse-motion?
    :writer (setf %raw-mouse-motion?))))

(defclass helper-window (handle-mixin)
  ())

(defmethod helper-window-class ((display display-mixin))
  'helper-window)

(defvar *window-handle->window-table* (make-hash-table :test #'eq))

(defmethod find-window ((handle integer))
  (gethash handle *window-handle->window-table*))

#+sbcl
(defmethod find-window ((handle sb-sys::system-area-pointer))
  (gethash (sap-int handle) *window-handle->window-table*))

#+ccl
(defmethod find-window ((handle ccl::macptr))
  (gethash (ccl::%ptr-to-int handle) *window-handle->window-table*))

#-darwin
(defmethod find-window ((handle noffi::cval))
  (find-window (cval-value handle)))

#+(and ccl (not darwin))
(defmethod find-window ((handle noffi::ptr))
  (find-window (ccl::%incf-ptr (ptr-value handle) (ptr-offset handle))))
   

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
  (call-next-method)
  (apply #'initialize-os-window window initargs)
  window)

(defmethod initialize-instance :after ((window os-window-mixin)
				&rest initargs
				&key display
				  &allow-other-keys)
  (declare (ignore initargs) (ignorable display))
  #+NIL(when (find-class 'arrow-cursor nil)
    (setf (%window-cursor window) (make-instance 'arrow-cursor :display display)))
  (values))

(defclass constant-refresh-os-window-mixin ()
  ())

(defclass constant-refresh-os-window-with-framebuffer-mixin (constant-refresh-os-window-mixin
							     os-window-with-framebuffer-mixin)
  ())
