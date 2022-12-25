(in-package :silica)

(defclass application-mixin (#+windows win32-application-mixin
			     #+x11 x11-application-mixin
			     #+wayland wayland-application-mixin
			     #+darwin ns-application-mixin)
  ((window-list-head :initform nil :accessor application-window-list-head)
   (monitors :initform (make-array 6 :adjustable t :fill-pointer 0)
	     :reader application-monitors)))
  

(defclass monitor-mixin (#+windows win32-monitor-mixin
			 #+x11 x11-monitor
			 #+darwin ns-monitor-mixin)
  ((name)
   (width-mm)
   (height-mm)
   ;; windows who's video mode is current on this monitor
   (window :accessor monitor-window :initform nil)
   (modes)
   (mode-count)
   (current-mode)
   (original-ramp)
   (current-ramp)))

(defclass cursor-mixin (#+windows win32-cursor-mixin
			#+x11 x11-cursor
			#+darwin ns-cursor-mixin)
  ())

(defclass essential-rect-mixin ()
  ((width :accessor width :type real)
   (height :accessor height :type real)))

(defclass hints ()
  ((refresh-rate :accessor hints-refresh-rate :initform 120)))

(defstruct video-mode
  (width)
  (height)
  (red-bits)
  (green-bits)
  (blue-bits)
  (refresh-rate))

(defclass essential-os-window-mixin (#+windows win32-window-mixin
				     #+x11 x11-window-mixin
				     #+wayland wayland-window-mixin
				     #+darwin ns-window-mixin)
  ((next :type essential-os-window-mixin :accessor window-next)
   (resizable? :type boolean :initform nil :accessor resizable?)
   (decorated? :type boolean :initform nil :accessor decorated?)
   (auto-iconify? :type boolean :initform nil :accessor auto-iconify?)
   (floating? :type boolean :initform nil :accessor floating?)
   (focus-on-show? :type boolean :initform t :accessor focus-on-show?)
   (mouse-passthrough? :type boolean :accessor mouse-passthrough?)
   (should-close? :type boolean :initform nil :accessor should-close?)
   (video-mode :accessor window-video-mode :initform (make-video-mode))
   (hints :accessor hints :initform (make-instance 'hints))
   (monitor :initform nil :accessor window-monitor)
   (cursor)
   (min-width :initform :dont-care :accessor window-min-width)
   (min-heigh :initform :dont-care :accessor window-min-height)
   (max-width :initform :dont-care :accessor window-max-width)
   (max-height :initform :dont-care :accessor window-max-height)
   (numer :initform :dont-care :accessor window-numer)
   (denom :initform :dont-care :accessor window-denom)
   (sticky-keys? :type boolean :initform nil)
   (sticky-mouse-buttons? :type boolean :initform nil)
   (lock-key-modes? :type boolean :initform nil)
   (cursor-mode :initform :normal :accessor window-cursor-mode)
   (mouse-buttons)
   (keys)
   (virtual-cursor-pos-x)
   (virtual-cursor-pos-y)
   (raw-mouse-motion? :type boolean :initform nil)
   (pos-callback :initform nil :type (or null function))
   (size-callback :initform nil :type (or null function))
   (close-callback :initform nil :type (or null function))
   (refresh-callback :initform nil :type (or null function))
   (focus-callback :initform nil :type (or null function))
   (iconify-callback :initform nil :type (or null function))
   (maximize-callback :initform nil :type (or null function))
   (content-scale-callback :initform nil :type (or null function))
   (mouse-button-callback :initform nil :type (or null function))
   (cursor-pos-callback :initform nil :type (or null function))
   (cursor-enter-callback :initform nil :type (or null function))
   (scroll-callback :initform nil :type (or null function))
   (key-callback :initform nil :type (or null function))
   (char-callback :initform nil :type (or null function))
   (char-mods-callback :initform nil :type (or null function))
   (drop-callback :initform nil :type (or null function))))

(defclass standard-os-window (essential-os-window-mixin) ())
