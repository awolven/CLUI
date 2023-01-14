(in-package :abstract-os)

(defun get-os-window-cursor-pos (window)
  #+darwin
  (get-cocoa-window-cursor-pos window)
  #+win32
  (get-win32-window-cursor-pos window)
  #+x11
  (get-x11-window-cursor-pos window)
  #+wayland
  (get-wayland-window-cursor-pos window))

(defun get-os-window-framebuffer-size (window)
  #+darwin
  (get-cocoa-window-framebuffer-size window)
  #+win32
  (get-win32-window-framebuffer-size window)
  #+x11
  (get-x11-window-framebuffer-size window)
  #+wayland
  (get-wayland-window-framebuffer-size window))

(defun get-os-window-content-scale (window)
  #+darwin
  (get-cocoa-window-content-scale window)
  #+win32
  (get-win32-window-content-scale window)
  #+x11
  (get-x11-window-content-scale window)
  #+wayland
  (get-wayland-window-content-scale window))

(defun os-window-should-close? (window)
  (should-close? window))

(defun (setf os-window-should-close?) (value window)
  (setf (should-close? window) value))

(defun os-window-title (window)
  #+darwin
  (cocoa-window-title window)
  #+win32
  (win32-window-title window)
  #+x11
  (x11-window-title window)
  #+wayland
  (wayland-window-title window))

(defun (setf os-window-title) (string window)
  #+darwin
  (setf (cocoa-window-title window) string)
  #+win32
  (setf (win32-window-title window) string)
  #+x11
  (setf (x11-window-title window) string)
  #+wayland
  (setf (wayland-window-title window) string))

(defun get-os-window-pos (window)
  #+darwin(get-cocoa-window-pos window)
  #+win32(get-win32-window-pos window)
  #+x11(get-x11-window-pos window)
  #+wayland(get-wayland-window-pos window))

(defun set-os-window-pos (window x y)
  #+darwin(set-cocoa-window-pos window x y)
  #+win32(set-win32-window-pos window x y)
  #+x11(set-x11-window-pos window x y)
  #+wayland(set-wayland-window-pos window x y))

(defun get-os-window-size (window)
  #+darwin(get-cocoa-window-size window)
  #+win32(get-win32-window-size window)
  #+x11(get-x11-window-size window)
  #+wayland(get-wayland-window-size window))

(defun focus-os-window (window)
  #+darwin(focus-cocoa-window window)
  #+win32(focus-win32-window window)
  #+x11(focus-x11-window window)
  #+wayland(focus-wayland-window window))

(defun hide-os-window (window)
  #+darwin(hide-cocoa-window window)
  #+win32(hide-win32-window window)
  #+x11(hide-x11-window window)
  #+wayland(hide-wayland-window window))

(defun show-os-window (window)
  #+darwin(show-cocoa-window window)
  #+win32(show-win32-window window)
  #+x11(show-x11-window window)
  #+wayland(show-wayland-window window))

(defun maximize-os-window (window)
  #+darwin(maximize-cocoa-window window)
  #+win32(maximize-win32-window window)
  #+x11(maximize-x11-window window)
  #+wayland(maximize-wayland-window window))

(defun restore-os-window (window)
  #+darwin(restore-cocoa-window window)
  #+win32(restore-win32-window window)
  #+x11(restore-x11-window window)
  #+wayland(restore-wayland-window window))

(defun iconify-os-window (window)
  #+darwin(iconify-cocoa-window window)
  #+win32(iconify-win32-window window)
  #+x11(iconify-x11-window window)
  #+wayland(iconify-wayland-window window))

(defun get-os-window-frame-size (window)
  #+darwin(get-cocoa-window-frame-size window)
  #+win32(get-win32-window-frame-size window)
  #+x11(get-x11-window-frame-size window)
  #+wayland(get-wayland-window-frame-size window))

(defun set-os-window-aspect-ratio (window numer denom)
  #+darwin(set-cocoa-window-aspect-ratio window numer denom)
  #+win32(set-win32-window-aspect-ratio window numer denom)
  #+x11(set-x11-window-aspect-ratio window numer denom)
  #+wayland(set-wayland-aspect-ratio window numer denom))

(defun set-os-window-size-limits (window min-width min-height max-width max-height)
  #+darwin(set-cocoa-window-size-limits window min-width min-height max-width max-height)
  #+win32(set-win32-window-size-limits window min-width min-height max-width max-height)
  #+x11(set-win32-window-size-limits window min-width min-height max-width max-height)
  #+wayland(set-win32-window-size-limits window min-width min-height max-width max-height))

  
(defun wait-application-events (app)
  #+darwin
  (wait-cocoa-events app)
  #+win32
  (wait-win32-events app)
  #+x11
  (wait-x11-events app)
  #+wayland
  (wait-wayland-events app))

(defun poll-application-events (app)
  #+darwin
  (poll-cocoa-events app)
  #+win32
  (poll-win32-events app)
  #+x11
  (poll-x11-events app)
  #+wayland
  (poll-wayland-events app))

(defun terminate-application (app)
  #+darwin
  (terminate-cocoa-application app)
  #+win32
  (terminate-win32-application app)
  #+x11
  (terminate-x11-application app)
  #+wayland
  (terminate-wayland-application app))

(defgeneric destroy-os-window (window))

(defgeneric shutdown-application (application))

#+vulkan
(defun get-required-instance-extensions ()
  #+darwin(get-cocoa-required-instance-extensions)
  #+win32(get-win32-required-instance-extensions)
  #+x11(get-x11-required-instance-extensions)
  #+wayland(get-wayland-required-instance-extensions))
  
