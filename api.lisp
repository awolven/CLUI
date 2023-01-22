(in-package :abstract-os)

#+vulkan
(defun get-required-instance-extensions ()
  #+darwin(get-cocoa-required-instance-extensions)
  #+win32(get-win32-required-instance-extensions)
  #+x11(get-x11-required-instance-extensions)
  #+wayland(get-wayland-required-instance-extensions))

(defgeneric window-fullscreen? (window))
(defgeneric (setf window-fullscreen?) (value window))

(defgeneric window-closable? (window))
(defgeneric (setf window-closable?) (value window))

(defgeneric window-title (window))
(defgeneric (setf window-title) (value window))

(defgeneric window-titled? (window))
(defgeneric (setf window-titled?) (value window))

(defgeneric window-position (window))
(defgeneric set-window-position (window x y))

(defgeneric window-size (window))
(defgeneric set-window-size (window width height))

(defgeneric window-cursor-position (window))
(defgeneric set-window-cursor-position (window x y))

(defgeneric window-maximized? (window))
(defgeneric (setf window-maximized?) (value window))

(defgeneric maximize-window (window))
(defgeneric restore-window (window))

(defgeneric show-window (window))
(defgeneric hide-window (window))

(defgeneric window-shown? (window))
(defgeneric (setf window-shown?) (value window))

(defgeneric window-hidden? (window))
(defgeneric (setf window-hidden?) (value window))

(defgeneric window-focused? (window))
(defgeneric (setf window-focused?) (value window))

(defgeneric focus-window (window))
(defgeneric unfocus-window (window))

(defgeneric window-iconifiable? (window))
(defgeneric (setf window-iconifiable?) (value window))

(defgeneric window-iconified? (window))
(defgeneric (setf window-iconified?) (value window))

(defgeneric iconify-window (window))
(defgeneric deiconify-window (window))

(defgeneric window-visible? (window))
(defgeneric (setf window-visible?) (value window))

(defgeneric make-window-visible (window))
(defgeneric make-window-invisible (window))

(defgeneric window-hovered? (window))

(defgeneric window-resizable? (window))
(defgeneric (setf window-resizable?) (value window))

(defgeneric make-window-resizable (window))
(defgeneric make-window-non-resizable (window))

(defgeneric window-decorated? (window))
(defgeneric (setf window-decorated?) (value window))

(defgeneric window-floating? (window))
(defgeneric (setf window-floating?) (value window))

(defgeneric window-opaque? (window))
(defgeneric (setf window-opaque?) (value window))

(defgeneric window-opacity (window))
(defgeneric (setf window-opacity) (alpha window))

(defgeneric set-window-size-limits (window min-width min-height max-width max-height))

(defgeneric window-aspect-ratio (window))

(defgeneric window-framebuffer-size (window))

(defgeneric window-frame-size (window))

(defgeneric window-content-scale (window))

(defgeneric request-window-attention (window))


(defun get-os-window-fullscreen (window)
  #+darwin(get-cocoa-window-fullscreen window)
  #+windows(get-win32-window-fullscreen window)
  #+linux(get-linux-window-fullscreen window))

(defun set-os-window-fullscreen (window value)
  #+darwin(set-cocoa-window-fullscreen window value)
  #+windows(set-win32-window-fullscreen window value)
  #+linux(set-linux-window-fullscreen window value))

(defun get-os-window-closable (window)
  #+darwin(get-cocoa-window-closable window)
  #+windows(get-win32-window-closable window)
  #+linux(get-linux-window-closable window))

(defun set-os-window-closable (window value)
  #+darwin(set-cocoa-window-closable window value)
  #+windows(set-win32-window-closable window value)
  #+linux(set-linux-window-closable window value))

(defun get-os-window-title (window)
  #+darwin(get-cocoa-window-title window)
  #+windows(get-win32-window-title window)
  #+linux(get-linux-window-title window))

(defun set-os-window-title (window value)
  #+darwin(set-cocoa-window-title window value)
  #+windows(set-win32-window-title window value)
  #+linux(set-linux-window-title window value))

(defun get-os-window-titled (window)
  #+darwin(get-cocoa-window-titled window)
  #+windows(get-win32-window-titled window)
  #+linux(get-linux-window-titled window))

(defun set-os-window-titled (window value)
  #+darwin(set-cocoa-window-titled window value)
  #+windows(set-win32-window-titled window value)
  #+linux(set-linux-window-titled window value))

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

(defun set-os-window-size (window value)
  #+darwin(set-cocoa-window-size window value)
  #+win32(set-win32-window-size window value)
  #+x11(set-x11-window-size window value)
  #+wayland(set-wayland-window-size window value))

(defun get-os-window-cursor-pos (window)
  #+darwin
  (get-cocoa-window-cursor-pos window)
  #+win32
  (get-win32-window-cursor-pos window)
  #+x11
  (get-x11-window-cursor-pos window)
  #+wayland
  (get-wayland-window-cursor-pos window))

(defun set-os-window-cursor-pos (window x y)
  #+darwin
  (set-cocoa-window-cursor-pos window x y)
  #+win32
  (set-win32-window-cursor-pos window x y)
  #+linux
  (get-linux-window-cursor-pos window x y))

(defun get-os-window-maximized (window)
  #+darwin(get-cocoa-window-maximized window)
  #+windows(get-win32-window-maximized window)
  #+linux(get-linux-window-maximized window))

(defun set-os-window-maximized (window value)
  #+darwin(set-cocoa-window-maximized window value)
  #+windows(set-win32-window-maximized window value)
  #+linux(set-linux-window-maximized window value))

(defun maximize-os-window (window)
  #+darwin(maximize-cocoa-window window)
  #+windows(maximize-win32-window window)
  #+linux(maximize-linux-window window))

(defun restore-os-window (window)
  #+darwin(restore-cocoa-window window)
  #+windows(restore-win32-window window)
  #+linux(restore-linux-window window))

(defun show-os-window (window)
  #+darwin(show-cocoa-window window)
  #+win32(show-win32-window window)
  #+x11(show-x11-window window)
  #+wayland(show-wayland-window window))

(defun hide-os-window (window)
  #+darwin(hide-cocoa-window window)
  #+windows(hide-win32-window window)
  #+linux(hide-linux-window window))

(defun get-os-window-shown (window)
  #+darwin(get-cocoa-window-shown window)
  #+windows(get-win32-window-shown window)
  #+linux(get-linux-window-shown window))

(defun set-os-window-shown (window value)
  #+darwin(set-cocoa-window-shown window value)
  #+windows(set-win32-window-shown window value)
  #+linux(set-linux-window-shown window value))

(defun get-os-window-hidden (window)
  #+darwin(get-cocoa-window-hidden window)
  #+windows(get-win32-window-hidden window)
  #+linux(get-linux-window-hidden window))

(defun set-os-window-hidden (window value)
  #+darwin(set-cocoa-window-hidden window value)
  #+windows(set-win32-window-hidden window value)
  #+linux(set-linux-window-hidden window value))

(defun get-os-window-focused (window)
  #+darwin(get-cocoa-window-focused window)
  #+windows(get-win32-window-focused window)
  #+linux(get-linux-window-focused window))

(defun set-os-window-focused (window value)
  #+darwin(set-cocoa-window-focused window value)
  #+windows(set-win32-window-focused window value)
  #+linux(set-linux-window-focused window value))

(defun focus-os-window (window)
  #+darwin(focus-cocoa-window window)
  #+windows(focus-win32-window window)
  #+linux(focus-linux-window window))

(defun unfocus-os-window (window)
  #+darwin(unfocus-cocoa-window window)
  #+windows(unfocus-win32-window window)
  #+linux(unfocus-linux-window window))

(defun get-os-window-iconifiable (window)
  #+darwin(get-cocoa-window-iconifiable window)
  #+windows(get-win32-window-iconifiable window)
  #+linux(get-linux-window-iconifiable window))

(defun set-os-window-iconifiable (window value)
  #+darwin(set-cocoa-window-iconifiable window value)
  #+windows(set-win32-window-iconifiable window value)
  #+linux(set-linux-window-iconifiable window value))

(defun get-os-window-iconified (window)
  #+darwin(get-cocoa-window-iconified window)
  #+windows(get-win32-window-iconified window)
  #+linux(get-linux-window-iconified window))

(defun set-os-window-iconified (window value)
  #+darwin(set-cocoa-window-iconified window value)
  #+windows(set-win32-window-iconified window value)
  #+linux(set-linux-window-iconified window value))

(defun iconify-os-window (window)
  #+darwin(iconify-cocoa-window window)
  #+windows(iconify-win32-window window)
  #+linux(iconify-linux-window window))

(defun deiconify-os-window (window)
  #+darwin(deiconify-cocoa-window window)
  #+windows(deiconify-win32-window window)
  #+linux(deiconify-linux-window window))

(defun get-os-window-visible (window)
  #+darwin(get-cocoa-window-visible window)
  #+windows(get-win32-window-visible window)
  #+linux(get-linux-window-visible window))

(defun set-os-window-visible (window value)
  #+darwin(set-cocoa-window-visible window value)
  #+windows(set-win32-window-visible window value)
  #+linux(set-linux-window-visible window value))

(defun make-os-window-visible (window)
  #+darwin(make-cocoa-window-visible window)
  #+windows(make-win32-window-visible window)
  #+linux(make-linux-window-visible window))

(defun make-os-window-invisible (window)
  #+darwin(make-cocoa-window-invisible window)
  #+windows(make-win32-window-invisible window)
  #+linux(make-linux-window-invisible window))

(defun get-os-window-hovered (window)
  #+darwin(get-cocoa-window-hovered window)
  #+win32(make-win32-window-hovered window)
  #+linux(make-linux-window-hovered window))

(defun get-os-window-resizable (window)
  #+darwin(get-cocoa-window-resizable window)
  #+windows(get-win32-window-resizable window)
  #+linux(get-linux-window-resizable window))

(defun set-os-window-resizable (window value)
  #+darwin(set-cocoa-window-resizable window value)
  #+windows(set-win32-window-resizable window value)
  #+linux(set-linux-window-resizable window value))

(defun make-os-window-resizable (window)
  #+darwin(make-cocoa-window-resizable window)
  #+windows(make-win32-window-resizable window)
  #+linux(make-linux-window-resizable window))

(defun make-os-window-non-resizable (window)
  #+darwin(make-cocoa-window-non-resizable window)
  #+windows(make-win32-window-non-resizable window)
  #+linux(make-linux-window-non-resizable window))

(defun get-os-window-decorated (window)
  #+darwin(get-cocoa-window-decorated window)
  #+windows(get-win32-window-decorated window)
  #+linux(get-linux-window-decorated window))

(defun set-os-window-decorated (window value)
  #+darwin(set-cocoa-window-decorated window value)
  #+windows(set-win32-window-decorated window value)
  #+linux(set-linux-window-decorated window value))

(defun get-os-window-floating (window)
  #+darwin(get-cocoa-window-floating window)
  #+windows(get-win32-window-floating window)
  #+linux(get-linux-window-floating window))

(defun set-os-window-floating (window value)
  #+darwin(set-cocoa-window-floating window value)
  #+windows(set-win32-window-floating window value)
  #+linux(set-linux-window-floating window value))

(defun get-os-window-opaque (window)
  #+darwin(get-cocoa-window-opaque window)
  #+windows(get-win32-window-opaque window)
  #+linux(get-linux-window-opaque window))

(defun set-os-window-opaque (window value)
  #+darwin(set-cocoa-window-opaque window value)
  #+windows(set-win32-window-opaque window value)
  #+linux(set-linux-window-opaque window value))

(defun get-os-window-opacity (window)
  #+darwin(get-cocoa-window-opacity window)
  #+windows(get-win32-window-opacity window)
  #+linux(get-linux-window-opacity window))

(defun set-os-window-opacity (window alpha)
  #+darwin(set-cocoa-window-opacity window alpha)
  #+windows(set-win32-window-opacity window alpha)
  #+linux(set-linux-window-opacity window alpha))

(defun set-os-window-size-limits (window min-width min-height max-width max-height)
  #+darwin(set-cocoa-window-size-limits window min-width min-height max-width max-height)
  #+windows(set-win32-window-size-limits window min-width min-height max-width max-height)
  #+linux(set-linux-window-size-limits window min-width min-height max-width max-height))

(defun get-os-window-aspect-ratio (window)
  #+darwin(get-cocoa-window-aspect-ratio window)
  #+windows(get-win32-window-aspect-ratio window)
  #+linux(get-win32-window-aspect-ratio window))

(defun get-os-window-framebuffer-size (window)
  #+darwin(get-cocoa-window-framebuffer-size window)
  #+windows(get-win32-window-framebuffer-size window)
  #+linux(get-linux-window-framebuffer-size window))

(defun get-os-window-frame-size (window)
  #+darwin(get-cocoa-window-frame-size window)
  #+windows(get-win32-window-frame-size window)
  #+linux(get-linux-window-frame-size window))

(defun get-os-window-content-scale (window)
  #+darwin(get-cocoa-window-content-scale window)
  #+windows(get-win32-window-content-scale window)
  #+linux(get-linux-window-content-scale window))

(defun request-os-window-attention (window)
  #+darwin(request-cocoa-window-attention window)
  #+windows(request-win32-window-attention window)
  #+linux(request-linux-window-attention window))


(defmethod window-fullscreen? ((window os-window-mixin))
  (get-os-window-fullscreen window))

(defmethod (setf window-fullscreen?) (value (window os-window-mixin))
  (set-os-window-fullscreen window value))

(defmethod window-closable? ((window os-window-mixin))
  (get-os-window-closable window))

(defmethod (setf window-closable?) (value (window os-window-mixin))
  (set-os-window-closable window value))

(defmethod window-title ((window os-window-mixin))
  (get-os-window-title window))

(defmethod (setf window-title) ((value string) (window os-window-mixin))
  (set-os-window-title window value))

(defmethod window-titled? ((window os-window-mixin))
  (get-os-window-titled window))

(defmethod (setf window-titled?) (value (window os-window-mixin))
  (set-os-window-titled window value))

(defmethod window-position ((window os-window-mixin))
  (get-os-window-pos window))

(defmethod set-window-position ((window os-window-mixin) x y)
  (set-os-window-pos window x y))

(defmethod window-maximized? ((window os-window-mixin))
  (get-os-window-maximized window))

(defmethod (setf window-maximized?) (value (window os-window-mixin))
  (set-os-window-maximized window value))

(defmethod maximize-window ((window os-window-mixin))
  (maximize-os-window window))

(defmethod restore-window ((window os-window-mixin))
  (restore-os-window window))

(defmethod show-window ((window os-window-mixin))
  (show-os-window window))

(defmethod hide-window ((window os-window-mixin))
  (hide-os-window window))

(defmethod window-shown? ((window os-window-mixin))
  (get-os-window-shown window))

(defmethod (setf window-shown?) (value (window os-window-mixin))
  (set-os-window-shown window value))

(defmethod window-hidden? ((window os-window-mixin))
  (get-os-window-hidden window))

(defmethod (setf window-hidden?) (value (window os-window-mixin))
  (set-os-window-hidden window value))

(defmethod window-focused? ((window os-window-mixin))
  (get-os-window-focused window))

(defmethod (setf window-focused?) (value (window os-window-mixin))
  (set-os-window-focused window value))

(defmethod focus-window ((window os-window-mixin))
  (focus-os-window window))

(defmethod unfocus-window ((window os-window-mixin))
  (unfocus-os-window window))

(defmethod window-iconifiable? ((window os-window-mixin))
  (get-os-window-iconifiable window))

(defmethod (setf window-iconifiable?) (value (window os-window-mixin))
  (set-os-window-iconifiable window value))

(defmethod window-iconified? ((window os-window-mixin))
  (get-os-window-iconified window))

(defmethod (setf window-iconified?) (value (window os-window-mixin))
  (set-os-window-iconified window value))

(defmethod iconify-window ((window os-window-mixin))
  (iconify-os-window window))

(defmethod deiconify-window ((window os-window-mixin))
  (deiconify-os-window window))

(defmethod window-visible? ((window os-window-mixin))
  (get-os-window-visible window))

(defmethod (setf window-visible?) (value (window os-window-mixin))
  (set-os-window-visible window value))

(defmethod make-window-visible ((window os-window-mixin))
  (make-os-window-visible window))

(defmethod make-window-invisible ((window os-window-mixin))
  (make-os-window-invisible window))

(defmethod window-hovered? ((window os-window-mixin))
  (get-os-window-hovered window))

(defmethod window-resizable? ((window os-window-mixin))
  (get-os-window-resizable window))

(defmethod (setf window-resizable?) (value (window os-window-mixin))
  (set-os-window-resizable window value))

(defmethod make-window-resizable ((window os-window-mixin))
  (make-os-window-resizable window))

(defmethod make-window-non-resizable ((window os-window-mixin))
  (make-os-window-resizable window))

(defmethod window-decorated? ((window os-window-mixin))
  (get-os-window-decorated window))

(defmethod (setf window-decorated?) (value (window os-window-mixin))
  (set-os-window-decorated window value))

(defmethod window-floating? ((window os-window-mixin))
  (get-os-window-floating window))

(defmethod (setf window-floating?) (value (window os-window-mixin))
  (set-os-window-floating window value))

(defmethod window-opaque? ((window os-window-mixin))
  (get-os-window-opaque window))

(defmethod (setf window-opaque?) (value (window os-window-mixin))
  (set-os-window-opaque window value))

(defmethod window-opacity ((window os-window-mixin))
  (get-os-window-opacity window))

(defmethod (setf window-opacity) ((alpha real) (window os-window-mixin))
  (set-os-window-opacity window alpha))

(defmethod set-window-size-limits ((window os-window-mixin)
				   min-width
				   min-height
				   max-width
				   max-height)
  (set-os-window-size-limits window min-width min-height max-width max-height))

(defmethod window-aspect-ratio ((window os-window-mixin))
  (get-os-window-aspect-ratio window))

(defmethod window-framebuffer-size ((window os-window-mixin))
  (get-os-window-framebuffer-size window))

(defmethod window-frame-size ((window os-window-mixin))
  (get-os-window-frame-size window))

(defmethod window-content-scale ((window os-window-mixin))
  (get-os-window-content-scale window))

(defmethod request-window-attention ((window os-window-mixin))
  (request-os-window-attention window))
