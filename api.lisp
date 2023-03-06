(in-package :clui)

(defgeneric destroy-window (window))  

(defgeneric window-fullscreen? (window))
(defgeneric (setf window-fullscreen?) (value window))

(defgeneric window-monitor (window))
(defgeneric (setf window-monitor) (monitor window))

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




(defun get-primary-monitor (&optional (display (default-display)))
  (car (display-monitors display)))


#+NIL
(defun set-window-monitor (window monitor &key xpos ypos width height (refresh-rate :dont-care))
  (declare (type os-window-mixin window))
  
  (when (or (minusp width) (minusp height))
    (warn "invalid window size: ~AX~A" width height)
    (return-from set-os-window-monitor (values)))

  (when (and (not (eq refresh-rate :dont-care))
	     (minusp refresh-rate))
    (warn "invalid refresh rate: ~A" refresh-rate)
    (return-from set-os-window-monitor (values)))
  
  (let ((video-mode (window-video-mode window)))
    (setf (video-mode-width video-mode) width)
    (setf (video-mode-height video-mode) height)
    (setf (video-mode-refresh-rate video-mode) refresh-rate))
  
  #+darwin(set-cocoa-window-monitor window monitor xpos ypos width height refresh-rate)
  #+windows(set-win32-window-monitor window monitor xpos ypos width height refresh-rate)
  #+linux(set-linux-window-monitor window monitor xpos ypos width height refresh-rate))

#+win32
(defmethod destroy-window ((window win32:window))
  (destroy-win32-window window))

#+cocoa
(defmethod destroy-window ((window cocoa:window))
  (destroy-cocoa-window window))

#+x11
(defmethod destroy-window ((window x11:window))
  (destroy-x11-window window))

#+wayland
(defmethod destroy-window ((window wayland:window))
  (destroy-wayland-window window))


#+win32
(defmethod get-default-screen-workarea ((display win32:desktop-mixin))
  #+windows(get-win32-desktop-workarea))

#+win32
(defmethod window-fullscreen? ((window win32:window-mixin))
  (get-win32-window-fullscreen window))

#+cocoa
(defmethod window-fullscreen? ((window cocoa:window-mixin))
  (get-cocoa-window-fullscreen window))

#+x11
(defmethod window-fullscreen? ((window x11:window-mixin))
  (get-x11-window-fullscreen window))

#+wayland
(defmethod window-fullscreen? ((window wayland:window-mixin))
  (get-x11-window-fullscreen window))

#+win32
(defmethod (setf window-fullscreen?) (value (window win32:window-mixin))
  (set-win32-window-fullscreen window value))

#+cocoa
(defmethod (setf window-fullscreen?) (value (window cocoa:window-mixin))
  (set-cocoa-window-fullscreen window value))

#+x11
(defmethod (setf window-fullscreen?) (value (window x11:window-mixin))
  (set-x11-window-fullscreen window value))

#+wayland
(defmethod (setf window-fullscreen?) (value (window wayland:window-mixin))
  (set-wayland-window-fullscreen window value))

(defmethod (setf window-monitor) ((monitor monitor-mixin) (window window-mixin))
  (multiple-value-bind (xpos ypos width height) (get-default-screen-workarea (monitor-display monitor))
    (let ((refresh-rate 60))
      (set-window-monitor window monitor :xpos xpos :ypos ypos :width width :height height :refresh-rate refresh-rate)
      monitor)))

#+win32
(defmethod window-closable? ((window win32:window-mixin))
  (get-win32-window-closable window))

#+cocoa
(defmethod window-closable? ((window cocoa:window-mixin))
  (get-cocoa-window-closable window))

#+x11
(defmethod window-closable? ((window x11:window-mixin))
  (get-x11-window-closable window))

#+wayland
(defmethod window-closable? ((window wayland:window-mixin))
  (get-wayland-window-closable window))

#+win32
(defmethod (setf window-closable?) (value (window win32:window-mixin))
  (set-win32-window-closable window value))

#+cocoa
(defmethod (setf window-closable?) (value (window cocoa:window-mixin))
  (set-cocoa-window-closable window value))

#+x11
(defmethod (setf window-closable?) (value (window x11:window-mixin))
  (set-x11-window-closable window value))

#+wayland
(defmethod (setf window-closable?) (value (window wayland:window-mixin))
  (set-wayland-window-closable window value))

#+win32
(defmethod window-title ((window win32:window-mixin))
  (get-win32-window-title window))

#+cocoa
(defmethod window-title ((window cocoa:window-mixin))
  (get-cocoa-window-title window))

#+x11
(defmethod window-title ((window x11:window-mixin))
  (get-x11-window-title window))

#+wayland
(defmethod window-title ((window wayland:window-mixin))
  (get-wayland-window-title window))

#+win32
(defmethod (setf window-title) ((value string) (window win32:window-mixin))
  (set-win32-window-title window value))

#+cocoa
(defmethod (setf window-title) ((value string) (window cocoa:window-mixin))
  (set-cocoa-window-title window value))

#+x11
(defmethod (setf window-title) ((value string) (window x11:window-mixin))
  (set-x11-window-title window value))

#+wayland
(defmethod (setf window-title) ((value string) (window wayland:window-mixin))
  (set-wayland-window-title window value))

#+win32
(defmethod window-titled? ((window win32:window-mixin))
  (get-win32-window-titled window))

#+cocoa
(defmethod window-titled? ((window cocoa:window-mixin))
  (get-cocoa-window-titled window))

#+x11
(defmethod window-titled? ((window x11:window-mixin))
  (get-x11-window-titled window))

#+wayland
(defmethod window-titled? ((window wayland:window-mixin))
  (get-wayland-window-titled window))

#+win32
(defmethod (setf window-titled?) (value (window win32:window-mixin))
  (set-win32-window-titled window value))

#+cocoa
(defmethod (setf window-titled?) (value (window cocoa:window-mixin))
  (set-cocoa-window-titled window value))

#+x11
(defmethod (setf window-titled?) (value (window x11:window-mixin))
  (set-x11-window-titled window value))

#+wayland
(defmethod (setf window-titled?) (value (window wayland:window-mixin))
  (set-wayland-window-titled window value))

#+win32
(defmethod window-position ((window win32:window-mixin))
  (get-win32-window-pos window))

#+cocoa
(defmethod window-position ((window cocoa:window-mixin))
  (get-cocoa-window-pos window))

#+x11
(defmethod window-position ((window x11:window-mixin))
  (get-x11-window-pos window))

#+wayland
(defmethod window-position ((window wayland:window-mixin))
  (get-wayland-window-pos window))

#+win32
(defmethod set-window-position ((window win32:window-mixin) x y)
  (set-win32-window-pos window x y))

#+cocoa
(defmethod set-window-position ((window cocoa:window-mixin) x y)
  (set-cocoa-window-pos window x y))

#+x11
(defmethod set-window-position ((window x11:window-mixin) x y)
  (set-x11-window-pos window x y))

#+wayland
(defmethod set-window-position ((window wayland:window-mixin) x y)
  (set-wayland-window-pos window x y))

#+win32
(defmethod window-size ((window win32:window-mixin))
  (get-win32-window-size window))

#+cocoa
(defmethod window-size ((window cocoa:window-mixin))
  (get-cocoa-window-size window))

#+x11
(defmethod window-size ((window x11:window-mixin))
  (get-x11-window-size window))

#+wayland
(defmethod window-size ((window wayland:window-mixin))
  (get-wayland-window-size window))

#+win32
(defmethod set-window-size ((window win32:window-mixin) width height)
  (set-win32-window-size window width height))

#+cocoa
(defmethod set-window-size ((window cocoa:window-mixin) width height)
  (set-cocoa-window-size window width height))

#+x11
(defmethod set-window-size ((window x11:window-mixin) width height)
  (set-x11-window-size window width height))

#+wayland
(defmethod set-window-size ((window wayland:window-mixin) width height)
  (set-wayland-window-size window width height))

#+win32
(defmethod set-window-monitor ((window win32:window-mixin) monitor &key xpos ypos width height refresh-rate)
  (set-win32-window-monitor window monitor xpos ypos width height refresh-rate))

#+cocoa
(defmethod set-window-monitor ((window cocoa:window-mixin) monitor &key xpos ypos width height refresh-rate)
  (set-cocoa-window-monitor window monitor :xpos xpos :ypos ypos :width width :height height :refresh-rate refresh-rate))

#+x11
(defmethod set-window-monitor ((window x11:window-mixin) monitor &key xpos ypos width height refresh-rate)
  (set-x11-window-monitor window monitor :xpos xpos :ypos ypos :width width :height height :refresh-rate refresh-rate))

#+wayland
(defmethod set-window-monitor ((window wayland:window-mixin) monitor &key xpos ypos width height refresh-rate)
  (set-wayland-window-monitor window monitor :xpos xpos :ypos ypos :width width :height height :refresh-rate refresh-rate))

#+win32
(defmethod window-cursor-position ((window win32:window-mixin))
  (get-win32-window-cursor-pos window))

#+cocoa
(defmethod window-cursor-position ((window cocoa:window-mixin))
  (get-cocoa-window-cursor-pos window))

#+x11
(defmethod window-cursor-position ((window x11:window-mixin))
  (get-x11-window-cursor-pos window))

#+wayland
(defmethod window-cursor-position ((window wayland:window-mixin))
  (get-x11-window-cursor-pos window))

#+win32
(defmethod set-window-cursor-position ((window win32:window-mixin) x y)
  (set-win32-window-cursor-pos window x y))

#+cocoa
(defmethod set-window-cursor-position ((window cocoa:window-mixin) x y)
  (set-cocoa-window-cursor-pos window x y))

#+x11
(defmethod set-window-cursor-position ((window x11:window-mixin) x y)
  (set-x11-window-cursor-pos window x y))

#+wayland
(defmethod set-window-cursor-position ((window wayland:window-mixin) x y)
  (set-wayland-window-cursor-pos window x y))

#+win32
(defmethod window-maximized? ((window win32:window-mixin))
  (get-win32-window-maximized window))

#+cocoa
(defmethod window-maximized? ((window cocoa:window-mixin))
  (get-cocoa-window-maximized window))

#+x11
(defmethod window-maximized? ((window x11:window-mixin))
  (get-x11-window-maximized window))

#+wayland
(defmethod window-maximized? ((window wayland:window-mixin))
  (get-wayland-window-maximized window))

#+win32nil
(defmethod (setf window-maximized?) (value (window win32:window-mixin))
  (set-win32-window-maximized window value))

#+cocoa
(defmethod (setf window-maximized?) (value (window cocoa:window-mixin))
  (set-cocoa-window-maximized window value))

#+x11
(defmethod (setf window-maximized?) (value (window x11:window-mixin))
  (set-x11-window-maximized window value))

#+wayland
(defmethod (setf window-maximized?) (value (window wayland:window-mixin))
  (set-wayland-window-maximized window value))

#+win32
(defmethod maximize-window ((window win32:window-mixin))
  (maximize-win32-window window))

#+cocoa
(defmethod maximize-window ((window cocoa:window-mixin))
  (maximize-cocoa-window window))

#+x11
(defmethod maximize-window ((window x11:window-mixin))
  (maximize-x11-window window))

#+wayland
(defmethod maximize-window ((window wayland:window-mixin))
  (maximize-wayland-window window))

#+win32
(defmethod restore-window ((window win32:window-mixin))
  (restore-win32-window window))

#+cocoa
(defmethod restore-window ((window cocoa:window-mixin))
  (restore-cocoa-window window))

#+x11
(defmethod restore-window ((window x11:window-mixin))
  (restore-x11-window window))

#+wayland
(defmethod restore-window ((window wayland:window-mixin))
  (restore-wayland-window window))

#+win32
(defmethod show-window ((window win32:window-mixin))
  (show-win32-window window))

#+cocoa
(defmethod show-window ((window cocoa:window-mixin))
  (show-cocoa-window window))

#+x11
(defmethod show-window ((window x11:window-mixin))
  (show-x11-window window))

#+wayland
(defmethod show-window ((window wayland:window-mixin))
  (show-wayland-window window))

#+win32
(defmethod hide-window ((window win32:window-mixin))
  (hide-win32-window window))

#+cocoa
(defmethod hide-window ((window cocoa:window-mixin))
  (hide-cocoa-window window))

#+x11
(defmethod hide-window ((window x11:window-mixin))
  (hide-x11-window window))

#+wayland
(defmethod hide-window ((window wayland:window-mixin))
  (hide-wayland-window window))

#+win32
(defmethod window-shown? ((window win32:window-mixin))
  (get-win32-window-shown window))

#+cocoa
(defmethod window-shown? ((window cocoa:window-mixin))
  (get-cocoa-window-shown window))

#+x11
(defmethod window-shown? ((window x11:window-mixin))
  (get-x11-window-shown window))

#+wayland
(defmethod window-shown? ((window wayland:window-mixin))
  (get-wayland-window-shown window))

#+win32
(defmethod (setf window-shown?) (value (window win32:window-mixin))
  (set-win32-window-shown window value))

#+cocoa
(defmethod (setf window-shown?) (value (window cocoa:window-mixin))
  (set-cocoa-window-shown window value))

#+x11
(defmethod (setf window-shown?) (value (window x11:window-mixin))
  (set-x11-window-shown window value))

#+wayland
(defmethod (setf window-shown?) (value (window wayland:window-mixin))
  (set-wayland-window-shown window value))

#+win32
(defmethod window-hidden? ((window win32:window-mixin))
  (get-win32-window-hidden window))

#+cocoa
(defmethod window-hidden? ((window cocoa:window-mixin))
  (get-cocoa-window-hidden window))

#+x11
(defmethod window-hidden? ((window x11:window-mixin))
  (get-x11-window-hidden window))

#+wayland
(defmethod window-hidden? ((window win32:window-mixin))
  (get-win32-window-hidden window))

#+win32
(defmethod (setf window-hidden?) (value (window win32:window-mixin))
  (set-win32-window-hidden window value))

#+cocoa
(defmethod (setf window-hidden?) (value (window cocoa:window-mixin))
  (set-cocoa-window-hidden window value))

#+x11
(defmethod (setf window-hidden?) (value (window x11:window-mixin))
  (set-x11-window-hidden window value))

#+wayland
(defmethod (setf window-hidden?) (value (window wayland:window-mixin))
  (set-wayland-window-hidden window value))

#+win32
(defmethod window-focused? ((window win32:window-mixin))
  (get-win32-window-focused window))

#+cocoa
(defmethod window-focused? ((window cocoa:window-mixin))
  (get-cocoa-window-focused window))

#+x11
(defmethod window-focused? ((window x11:window-mixin))
  (get-x11-window-focused window))

#+wayland
(defmethod window-focused? ((window wayland:window-mixin))
  (get-wayland-window-focused window))

#+win32
(defmethod (setf window-focused?) (value (window win32:window-mixin))
  (set-win32-window-focused window value))

#+cocoa
(defmethod (setf window-focused?) (value (window cocoa:window-mixin))
  (set-cocoa-window-focused window value))

#+x11
(defmethod (setf window-focused?) (value (window x11:window-mixin))
  (set-x11-window-focused window value))

#+wayland
(defmethod (setf window-focused?) (value (window wayland:window-mixin))
  (set-wayland-window-focused window value))

#+win32
(defmethod focus-window ((window win32:window-mixin))
  (focus-win32-window window))

#+cocoa
(defmethod focus-window ((window cocoa:window-mixin))
  (focus-cocoa-window window))

#+x11
(defmethod focus-window ((window x11:window-mixin))
  (focus-x11-window window))

#+wayland
(defmethod focus-window ((window wayland:window-mixin))
  (focus-wayland-window window))

#+win32
(defmethod defocus-window ((window win32:window-mixin))
  (defocus-win32-window window))

#+cocoa
(defmethod defocus-window ((window cocoa:window-mixin))
  (defocus-cocoa-window window))

#+x11
(defmethod defocus-window ((window x11:window-mixin))
  (defocus-x11-window window))

#+wayland
(defmethod defocus-window ((window wayland:window-mixin))
  (defocus-wayland-window window))

#+win32
(defmethod window-iconifiable? ((window win32:window-mixin))
  (get-win32-window-iconifiable window))

#+cocoa
(defmethod window-iconifiable? ((window cocoa:window-mixin))
  (get-cocoa-window-iconifiable window))

#+x11
(defmethod window-iconifiable? ((window x11:window-mixin))
  (get-x11-window-iconifiable window))

#+wayland
(defmethod window-iconifiable? ((window wayland:window-mixin))
  (get-wayland-window-iconifiable window))

#+win32
(defmethod (setf window-iconifiable?) (value (window win32:window-mixin))
  (set-win32-window-iconifiable window value))

#+cocoa
(defmethod (setf window-iconifiable?) (value (window cocoa:window-mixin))
  (set-cocoa-window-iconifiable window value))

#+x11
(defmethod (setf window-iconifiable?) (value (window x11:window-mixin))
  (set-x11-window-iconifiable window value))

#+wayland
(defmethod (setf window-iconifiable?) (value (window wayland:window-mixin))
  (set-wayland-window-iconifiable window value))

#+win32
(defmethod window-iconified? ((window win32:window-mixin))
  (get-win32-window-iconified window))

#+cocoa
(defmethod window-iconified? ((window cocoa:window-mixin))
  (get-cocoa-window-iconified window))

#+x11
(defmethod window-iconified? ((window x11:window-mixin))
  (get-x11-window-iconified window))

#+wayland
(defmethod window-iconified? ((window wayland:window-mixin))
  (get-wayland-window-iconified window))

#+win32
(defmethod (setf window-iconified?) (value (window win32:window-mixin))
  (set-win32-window-iconified window value))

#+cocoa
(defmethod (setf window-iconified?) (value (window cocoa:window-mixin))
  (set-cocoa-window-iconified window value))

#+x11
(defmethod (setf window-iconified?) (value (window x11:window-mixin))
  (set-x11-window-iconified window value))

#+wayland
(defmethod (setf window-iconified?) (value (window wayland:window-mixin))
  (set-wayland-window-iconified window value))

#+win32
(defmethod iconify-window ((window win32:window-mixin))
  (iconify-win32-window window))

#+cocoa
(defmethod iconify-window ((window cocoa:window-mixin))
  (iconify-cocoa-window window))

#+x11
(defmethod iconify-window ((window x11:window-mixin))
  (iconify-x11-window window))

#+wayland
(defmethod iconify-window ((window wayland:window-mixin))
  (iconify-wayland-window window))

#+win32
(defmethod deiconify-window ((window win32:window-mixin))
  (deiconify-win32-window window))

#+cocoa
(defmethod deiconify-window ((window cocoa:window-mixin))
  (deiconify-cocoa-window window))

#+x11
(defmethod deiconify-window ((window x11:window-mixin))
  (deiconify-x11-window window))

#+wayland
(defmethod deiconify-window ((window wayland:window-mixin))
  (deiconify-wayland-window window))

#+win32
(defmethod window-visible? ((window win32:window-mixin))
  (get-win32-window-visible window))

#+cocoa
(defmethod window-visible? ((window cocoa:window-mixin))
  (get-cocoa-window-visible window))

#+x11
(defmethod window-visible? ((window x11:window-mixin))
  (get-x11-window-visible window))

#+wayland
(defmethod window-visible? ((window wayland:window-mixin))
  (get-wayland-window-visible window))

#+win32
(defmethod (setf window-visible?) (value (window win32:window-mixin))
  (set-win32-window-visible window value))

#+cocoa
(defmethod (setf window-visible?) (value (window cocoa:window-mixin))
  (set-cocoa-window-visible window value))

#+x11
(defmethod (setf window-visible?) (value (window x11:window-mixin))
  (set-x11-window-visible window value))

#+wayland
(defmethod (setf window-visible?) (value (window wayland:window-mixin))
  (set-wayland-window-visible window value))

#+win32
(defmethod make-window-visible ((window win32:window-mixin))
  (make-win32-window-visible window))

#+cocoa
(defmethod make-window-visible ((window cocoa:window-mixin))
  (make-cocoa-window-visible window))

#+x11
(defmethod make-window-visible ((window x11:window-mixin))
  (make-x11-window-visible window))

#+wayland
(defmethod make-window-visible ((window wayland:window-mixin))
  (make-wayland-window-visible window))

#+win32
(defmethod make-window-invisible ((window win32:window-mixin))
  (make-win32-window-invisible window))

#+cocoa
(defmethod make-window-invisible ((window cocoa:window-mixin))
  (make-cocoa-window-invisible window))

#+x11
(defmethod make-window-invisible ((window x11:window-mixin))
  (make-x11-window-invisible window))

#+wayland
(defmethod make-window-invisible ((window wayland:window-mixin))
  (make-wayland-window-invisible window))

#+win32
(defmethod window-hovered? ((window win32:window-mixin))
  (get-win32-window-hovered window))

#+cocoa
(defmethod window-hovered? ((window cocoa:window-mixin))
  (get-cocoa-window-hovered window))

#+x11
(defmethod window-hovered? ((window x11:window-mixin))
  (get-x11-window-hovered window))

#+wayland
(defmethod window-hovered? ((window wayland:window-mixin))
  (get-wayland-window-hovered window))

#+win32
(defmethod window-resizable? ((window win32:window-mixin))
  (get-win32-window-resizable window))

#+cocoa
(defmethod window-resizable? ((window cocoa:window-mixin))
  (get-cocoa-window-resizable window))

#+x11
(defmethod window-resizable? ((window x11:window-mixin))
  (get-x11-window-resizable window))

#+wayland
(defmethod window-resizable? ((window wayland:window-mixin))
  (get-wayland-window-resizable window))

#+win32
(defmethod (setf window-resizable?) (value (window win32:window-mixin))
  (set-win32-window-resizable window value))

#+cocoa
(defmethod (setf window-resizable?) (value (window cocoa:window-mixin))
  (set-cocoa-window-resizable window value))

#+x11
(defmethod (setf window-resizable?) (value (window x11:window-mixin))
  (set-x11-window-resizable window value))

#+wayland
(defmethod (setf window-resizable?) (value (window wayland:window-mixin))
  (set-wayland-window-resizable window value))

#+win32
(defmethod make-window-resizable ((window win32:window-mixin))
  (make-win32-window-resizable window))

#+cocoa
(defmethod make-window-resizable ((window cocoa:window-mixin))
  (make-cocoa-window-resizable window))

#+x11
(defmethod make-window-resizable ((window x11:window-mixin))
  (make-x11-window-resizable window))

#+wayland
(defmethod make-window-resizable ((window wayland:window-mixin))
  (make-wayland-window-resizable window))

#+win32
(defmethod make-window-non-resizable ((window win32:window-mixin))
  (make-win32-window-resizable window))

#+cocoa
(defmethod make-window-non-resizable ((window cocoa:window-mixin))
  (make-cocoa-window-resizable window))

#+x11
(defmethod make-window-non-resizable ((window x11:window-mixin))
  (make-x11-window-resizable window))

#+wayland
(defmethod make-window-non-resizable ((window wayland:window-mixin))
  (make-wayland-window-resizable window))

#+win32
(defmethod window-decorated? ((window win32:window-mixin))
  (get-win32-window-decorated window))

#+cocoa
(defmethod window-decorated? ((window cocoa:window-mixin))
  (get-cocoa-window-decorated window))

#+x11
(defmethod window-decorated? ((window x11:window-mixin))
  (get-x11-window-decorated window))

#+wayland
(defmethod window-decorated? ((window wayland:window-mixin))
  (get-wayland-window-decorated window))

#+win32
(defmethod (setf window-decorated?) (value (window win32:window-mixin))
  (set-win32-window-decorated window value))

#+cocoa
(defmethod (setf window-decorated?) (value (window cocoa:window-mixin))
  (set-cocoa-window-decorated window value))

#+x11
(defmethod (setf window-decorated?) (value (window x11:window-mixin))
  (set-x11-window-decorated window value))

#+wayland
(defmethod (setf window-decorated?) (value (window wayland:window-mixin))
  (set-wayland-window-decorated window value))

#+win32
(defmethod window-floating? ((window win32:window-mixin))
  (get-win32-window-floating window))

#+cocoa
(defmethod window-floating? ((window cocoa:window-mixin))
  (get-cocoa-window-floating window))

#+x11
(defmethod window-floating? ((window x11:window-mixin))
  (get-x11-window-floating window))

#+wayland
(defmethod window-floating? ((window wayland:window-mixin))
  (get-wayland-window-floating window))

#+win32
(defmethod (setf window-floating?) (value (window win32:window-mixin))
  (set-win32-window-floating window value))

#+cocoa
(defmethod (setf window-floating?) (value (window cocoa:window-mixin))
  (set-cocoa-window-floating window value))

#+x11
(defmethod (setf window-floating?) (value (window x11:window-mixin))
  (set-x11-window-floating window value))

#+wayland
(defmethod (setf window-floating?) (value (window wayland:window-mixin))
  (set-wayland-window-floating window value))

#+win32
(defmethod window-opaque? ((window win32:window-mixin))
  (get-win32-window-opaque window))

#+cocoa
(defmethod window-opaque? ((window cocoa:window-mixin))
  (get-cocoa-window-opaque window))

#+x11
(defmethod window-opaque? ((window x11:window-mixin))
  (get-x11-window-opaque window))

#+wayland
(defmethod window-opaque? ((window wayland:window-mixin))
  (get-wayland-window-opaque window))

#+win32
(defmethod (setf window-opaque?) (value (window win32:window-mixin))
  (set-win32-window-opaque window value))

#+cocoa
(defmethod (setf window-opaque?) (value (window cocoa:window-mixin))
  (set-cocoa-window-opaque window value))

#+x11
(defmethod (setf window-opaque?) (value (window x11:window-mixin))
  (set-x11-window-opaque window value))

#+wayland
(defmethod (setf window-opaque?) (value (window wayland:window-mixin))
  (set-wayland-window-opaque window value))

#+win32
(defmethod window-opacity ((window win32:window-mixin))
  (get-win32-window-opacity window))

#+cocoa
(defmethod window-opacity ((window cocoa:window-mixin))
  (get-cocoa-window-opacity window))

#+x11
(defmethod window-opacity ((window x11:window-mixin))
  (get-x11-window-opacity window))

#+wayland
(defmethod window-opacity ((window wayland:window-mixin))
  (get-wayland-window-opacity window))

#+win32
(defmethod (setf window-opacity) ((alpha real) (window win32:window-mixin))
  (set-win32-window-opacity window alpha))

#+cocoa
(defmethod (setf window-opacity) ((alpha real) (window cocoa:window-mixin))
  (set-cocoa-window-opacity window alpha))

#+x11
(defmethod (setf window-opacity) ((alpha real) (window x11:window-mixin))
  (set-x11-window-opacity window alpha))

#+wayland
(defmethod (setf window-opacity) ((alpha real) (window wayland:window-mixin))
  (set-wayland-window-opacity window alpha))

#+win32
(defmethod set-window-size-limits ((window win32:window-mixin)
				   min-width
				   min-height
				   max-width
				   max-height)
  (set-win32-window-size-limits window min-width min-height max-width max-height))

#+cocoa
(defmethod set-window-size-limits ((window cocoa:window-mixin)
				   min-width
				   min-height
				   max-width
				   max-height)
  (set-cocoa-window-size-limits window min-width min-height max-width max-height))

#+x11
(defmethod set-window-size-limits ((window x11:window-mixin)
				   min-width
				   min-height
				   max-width
				   max-height)
  (set-x11-window-size-limits window min-width min-height max-width max-height))

#+wayland
(defmethod set-window-size-limits ((window wayland:window-mixin)
				   min-width
				   min-height
				   max-width
				   max-height)
  (set-wayland-window-size-limits window min-width min-height max-width max-height))

#+win32
(defmethod window-aspect-ratio ((window win32:window-mixin))
  (get-win32-window-aspect-ratio window))

#+cocoa
(defmethod window-aspect-ratio ((window cocoa:window-mixin))
  (get-cocoa-window-aspect-ratio window))

#+x11
(defmethod window-aspect-ratio ((window x11:window-mixin))
  (get-x11-window-aspect-ratio window))

#+wayland
(defmethod window-aspect-ratio ((window wayland:window-mixin))
  (get-wayland-window-aspect-ratio window))

#+win32
(defmethod window-framebuffer-size ((window win32:window-mixin))
  (get-win32-window-framebuffer-size window))

#+cocoa
(defmethod window-framebuffer-size ((window cocoa:window-mixin))
  (get-cocoa-window-framebuffer-size window))

#+x11
(defmethod window-framebuffer-size ((window x11:window-mixin))
  (get-x11-window-framebuffer-size window))

#+wayland
(defmethod window-framebuffer-size ((window wayland:window-mixin))
  (get-wayland-framebuffer-size window))

#+win32
(defmethod window-frame-size ((window win32:window-mixin))
  (get-win32-window-frame-size window))

#+cocoa
(defmethod window-frame-size ((window cocoa:window-mixin))
  (get-cocoa-window-frame-size window))

#+x11
(defmethod window-frame-size ((window x11:window-mixin))
  (get-x11-window-frame-size window))

#+wayland
(defmethod window-frame-size ((window wayland:window-mixin))
  (get-wayland-window-frame-size window))

#+win32
(defmethod window-content-scale ((window win32:window-mixin))
  (get-win32-window-content-scale window))

#+cocoa
(defmethod window-content-scale ((window cocoa:window-mixin))
  (get-cocoa-window-content-scale window))

#+x11
(defmethod window-content-scale ((window x11:window-mixin))
  (get-x11-window-content-scale window))

#+wayland
(defmethod window-content-scale ((window wayland:window-mixin))
  (get-wayland-window-content-scale window))

#+win32
(defmethod request-window-attention ((window win32:window-mixin))
  (request-win32-window-attention window))

#+cocoa
(defmethod request-window-attention ((window cocoa:window-mixin))
  (request-cocoa-window-attention window))

#+x11
(defmethod request-window-attention ((window x11:window-mixin))
  (request-x11-window-attention window))

(defun center-cursor-in-content-area (window)
  (multiple-value-bind (width height) (window-size window)
    (set-window-cursor-position window (/ width 2) (/ height 2))))

#+win32
(defmethod wait-events ((display win32:desktop-mixin) &optional (timeout nil))
  (wait-win32-events display))

#+cocoa
(defmethod wait-events ((display cocoa:desktop-mixin) &optional (timeout nil))
  (wait-cocoa-events display timeout))

#+x11
(defmethod wait-events ((display x11:server-mixin) &optional (timeout nil))
  (wait-x11-events display timeout))

#+wayland
(defmethod wait-events ((display wayland:desktop-mixin) &optional (timeout nil)
  (wait-wayland-events display) timeout))

#+win32
(defmethod poll-events ((display win32:desktop-mixin))
  (poll-win32-events display))

#+cocoa
(defmethod poll-events ((display cocoa:desktop-mixin))
  (poll-cocoa-events display))

#+x11
(defmethod poll-events ((display x11:server-mixin))
  (poll-x11-events display))

#+wayland
(defmethod poll-events ((display x11:desktop-mixin))
  (poll-wayland-events display))
