(in-package :clui)


(defclass x11-window-manager ()
  ((net-supported)
   (net-supporting-wm-check)
   (wm-protocols)
   (wm-state)
   (wm-delete-window)
   (net-wm-name)
   (net-wm-icon-name)
   (net-wm-icon)
   (net-wm-pid)
   (net-wm-ping)
   (net-wm-window-type)
   (net-wm-window-type-normal)
   (net-wm-state)
   (net-wm-state-above)
   (net-wm-state-fullscreen)
   (net-wm-state-maximized-vert)
   (net-wm-state-maximized-horiz)
   (net-wm-state-demands-attention)
   (net-mw-bypass-compositor)
   (net-wm-fullscreen-monitors)
   (net-wm-window-opacity)
   (net-wm-cm-sx)
   (net-workarea)
   (net-current-desktop)
   (net-active-window)
   (net-frame-extents)
   (net-request-frame-extents)
   (motif-wm-hints)))


(defclass xdnd ()
  ((xdnd-aware)
   (xdnd-enter)
   (xdnd-position)
   (xdnd-status)
   (xdnd-action-copy)
   (xdnd-drop)
   (xdnd-finished)
   (xdnd-selection)
   (xdnd-type-list)
   (text-uri-list)))


(defclass x11-clipboard ()
  ((targets)
   (multiple)
   (incr)
   (clipboard)
   (primary)
   (clipboard-manager)
   (save-targets)
   (utf8-string)
   (compound-string)
   (atom-pair)
   (selection)))


(defclass xrm ()
  ((destroy-database)
   (get-resource)
   (get-string-database)
   (unique-quark)))
  
(defclass x11-application-mixin ()
  ((display)
   (screen)
   (root)
   (content-scale-x)
   (content-scale-y)
   (helper-window-handle)
   (hidden-cursor-handle)
   (context)
   (im)
   (error-handler)
   (error-code)
   (primary-selection-string)
   (clipboard-string)
   (keynames)
   (keycodes)
   (scancodes)
   (restore-cursor-pos-x)
   (restore-cursor-pos-y)
   (disabled-cursor-window)
   (empty-event-pipe)
   (wm)
   (xdnd)
   (clipboard)
   (xlib)
   (xrm)
   (randr)
   (xkb)
   (saver)
   (xcursor)
   (xinerama)
   (x11xcb)
   (vidmode)
   (xi)
   (xrender)
   (xshape)))

(defclass x11-monitor ()
  ((output)
   (crtc)
   (old-mode)))

(defclass x11-cursor ()
  ((handle)))
   





(defclass x11-window-mixin (essential-rect-mixin)
  ((colormap)
   (handle)
   (parent)
   (ic)
   (override-redirect? :type boolean)
   (iconified? :type boolean)
   (maximized? :type boolean)
   (transparent? :type boolean)
   (pos-x)
   (pos-y)
   (last-cursor-pos-x)
   (last-cursor-pos-y)
   (warp-cursor-pos-x)
   (warp-cursor-pos-y)
   (key-press-times)))

(defun init-x11 ())
  
(defun connect-x11 (platform-id)
  (declare (ignorable platform-id)))

(defun terminate-x11 ())

(defun create-x11-window (&rest args &key window-config &allow-other-keys)
  (declare (ignorable args window-config)))

(defun destroy-x11-window (x11-window)
  (declare (ignorable x11-window)))

(defun (setf x11-window-title) (string x11-window)
  (declare (ignorable string x11-window)))

(defun (setf x11-window-icon) (images x11-window)
  (declare (ignorable images x11-window)))

(defun x11-window-pos (x11-window)
  (declare (ignorable x11-window)))

(defun (setf x11-window-pos) (pos x11-window)
  (declare (ignorable x11-window pos)))

(defun x11-window-size (x11-window)
  (declare (ignorable x11-window)))

(defun (setf x11-window-size) (size x11-window)
  (declare (ignorable size x11-window)))

(defun (setf x11-window-size-limits) (rect x11-window)
  (declare (ignorable rect x11-window)))

(defun (setf x11-window-aspect-ratio) (ratio x11-window)
  (declare (ignorable ratio x11-window)))

(defun x11-window-frame-size (x11-window)
  (declare (ignorable x11-window)))

(defun x11-window-content-scale (x11-window)
  (declare (ignorable x11-window)))

(defun iconify-x11-window (x11-window)
  (declare (ignorable x11-window)))

(defun restore-x11-window (x11-window)
  (declare (ignorable x11-window)))

(defun maximize-x11-window (x11-window)
  (declare (ignorable x11-window)))

(defun show-x11-window (x11-window)
  (declare (ignorable x11-window)))

(defun hide-x11-window (x11-window)
  (declare (ignorable x11-window)))

(defun request-x11-window-attention (x11-window)
  (declare (ignorable x11-window)))

(defun focus-x11-window (x11-window)
  (declare (ignorable x11-window)))

(defun (setf x11-window-fullscreen-monitor) (x11-monitor x11-window)
  (declare (ignorable x11-monitor x11-window)))

(defun x11-window-focused? (x11-window)
  (declare (ignorable x11-window)))

(defun x11-window-iconified? (x11-window)
  (declare (ignorable x11-window)))

(defun x11-window-visible? (x11-window)
  (declare (ignorable x11-window)))

(defun x11-window-maximized? (x11-window)
  (declare (ignorable x11-window)))

(defun x11-window-hovered? (x11-window)
  (declare (ignorable x11-window)))

(defun (setf x11-window-resizable?) (value x11-window)
  (declare (ignorable x11-window value)))

(defun (setf x11-window-decorated?) (value x11-window)
  (declare (ignorable x11-window value)))

(defun (setf x11-window-floating?) (value x11-window)
  (declare (ignorable x11-window value)))

(defun x11-window-opacity (x11-window)
  (declare (ignorable x11-window)))

(defun (setf x11-window-opacity) (opacity x11-window)
  (declare (ignorable x11-window opacity)))

(defun (setf x11-window-mouse-passthrough) (enabled? x11-window)
  (declare (ignorable x11-window enabled?)))

(defun x11-window-poll-events (x11-window)
  (declare (ignorable x11-window)))

(defun x11-window-wait-events (x11-window)
  (declare (ignorable x11-window)))

(defun x11-window-wait-events-timeout (x11-window timeout)
  (declare (ignorable timeout x11-window)))

(defun x11-window-post-empty-event (x11-window)
  (declare (ignorable x11-window)))

(defun x11-cursor-pos (x11-window)
  (declare (ignorable x11-window)))

(defun (setf x11-cursor-pos) (pos x11-window)
  (declare (ignorable pos x11-window)))

(defun (setf x11-cursor-mode) (mode x11-window)
  (declare (ignorable mode x11-window)))
  
(defun create-os-window (&rest args &key window-config &allow-other-keys)
  (let ((args (copy-list args)))
    (remf args :window-config)
    (apply #'create-x11-window :window-config window-config args)))

(defun destroy-os-window (os-window)
  (destroy-x11-window os-window))

(defun (setf os-window-title) (string os-window)
  (setf (x11-window-title os-window) string))

(defun (setf os-window-icon) (image os-window)
  (setf (x11-window-icon os-window) (list image)))

(defun os-window-pos (os-window)
  (x11-window-pos os-window))

(defun (setf os-window-pos) (pos os-window)
  (setf (x11-window-pos os-window) pos))

(defun os-window-size (os-window)
  (x11-window-size os-window))

(defun (setf os-window-size) (size os-window)
  (setf (x11-window-size os-window) size))

(defun (setf os-window-size-limits) (rect os-window)
  (setf (x11-window-size-limits os-window) rect))

(defun (setf os-window-aspect-ratio) (ratio os-window)
  (setf (x11-window-aspect-ratio os-window) ratio))

(defun os-window-frame-size (os-window)
  (x11-window-frame-size os-window))

(defun os-window-content-scale (os-window)
  (x11-window-content-scale os-window))

(defun iconify-os-window (os-window)
  (iconify-x11-window os-window))

(defun restore-os-window (os-window)
  (restore-x11-window os-window))

(defun maximize-os-window (os-window)
  (maximize-x11-window os-window))

(defun show-os-window (os-window)
  (show-x11-window os-window))

(defun hide-os-window (os-window)
  (hide-x11-window os-window))

(defun request-os-window-attention (os-window)
  (request-x11-window-attention os-window))

(defun focus-os-window (os-window)
  (focus-x11-window os-window))

(defun (setf os-window-fullscreen-monitor) (os-window monitor)
  (setf (x11-window-fullscreen-monitor os-window) monitor))

(defun os-window-focused? (os-window)
  (x11-window-focused? os-window))

(defun os-window-iconified? (os-window)
  (x11-window-iconified? os-window))

(defun os-window-visible? (os-window)
  (x11-window-visible? os-window))

(defun os-window-maximized? (os-window)
  (x11-window-maximized? os-window))

(defun os-window-hovered? (os-window)
  (x11-window-hovered? os-window))

(defun os-window-opacity (os-window)
  (x11-window-opacity os-window))

(defun (setf os-window-resizable?) (value os-window)
  (setf (x11-window-resizable? os-window) value))

(defun (setf os-window-decorated?) (value os-window)
  (setf (x11-window-decorated? os-window) value))

(defun (setf os-window-floating?) (value os-window)
  (setf (x11-window-floating? os-window) value))

(defun (setf os-window-opacity) (value os-window)
  (setf (x11-window-opacity os-window) value))

(defun (setf os-window-mouse-passthrough) (value os-window)
  (setf (x11-window-mouse-passthrough os-window) value))

(defun os-window-poll-events (os-window)
  (x11-window-poll-events os-window))

(defun os-window-wait-events (os-window)
  (x11-window-wait-events os-window))

(defun os-window-wait-events-timeout (os-window timeout)
  (x11-window-wait-events-timeout os-window timeout))

(defun os-window-post-empty-event (os-window)
  (x11-window-post-empty-event os-window))

