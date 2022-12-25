(in-package :abstract-os)

(defclass ns-application-mixin ()
  ((event-source)
   (delegate)
   (cursor-hidden?)
   (input-source)
   (hid-manager)
   (unicode-data)
   (helper)
   (key-up-monitor)
   (nib-objects)
   (keynames)
   (keycodes)
   (scancodes)
   (clipboard-string)
   (cascade-point)
   (restore-cursor-position-x)
   (restore-cursor-position-y)
   (disabled-cursor-window)
   (tis-bundle)
   (tis-copy-current-keyboard-layout-input-source)
   (tis-get-input-source-property)
   (tis-get-keyboard-type)
   (tis-k-property-unicode-key-layout-data)))

(defclass ns-monitor-mixin ()
  ((display-id)
   (previous-mode)
   (unit-number)
   (screen)
   (fallback-refresh-rate)))


(defclass ns-cursor-mixin ()
  ((object)))



(defclass ns-window-mixin (essential-rect-mixin)
  ((object)
   (delegate)
   (view)
   (layer)
   (xscale)
   (yscale)
   (cursor-warp-delta-x)
   (cursor-warp-delta-y)))


(defun create-os-window (&rest args &key &allow-other-keys)
  (declare (ignorable args)))

(defun destroy-os-window (os-window)
  (declare (ignorable os-window)))

(defun os-window-title (os-window)
  (declare (ignorable os-window)))

(defun (setf os-window-title) (string os-window)
  (declare (ignorable string os-window)))

(defun (setf os-window-icon) (image os-window)
  (declare (ignorable image os-window)))

(defun os-window-pos (os-window)
  (declare (ignorable os-window)))

(defun (setf os-window-pos) (pos os-window)
  (declare (ignorable pos os-window)))

(defun os-window-size (os-window)
  (declare (ignorable os-window)))

(defun (setf os-window-size) (size os-window)
  (declare (ignorable os-window size)))

(defun (setf os-window-size-limits) (rect os-window)
  (declare (ignorable os-window rect)))

(defun (setf os-window-aspect-ratio) (ratio os-window)
  (declare (ignorable ratio os-window)))

(defun os-window-frame-size (os-window)
  (declare (ignorable os-window)))

(defun os-window-content-scale (os-window)
  (declare (ignorable os-window)))

(defun iconify-os-window (os-window)
  (declare (ignorable os-window)))

(defun restore-os-window (os-window)
  (declare (ignorable os-window)))

(defun maximize-os-window (os-window)
  (declare (ignorable os-window)))

(defun show-os-window (os-window)
  (declare (ignorable os-window)))

(defun hide-os-window (os-window)
  (declare (ignorable os-window)))

(defun request-os-window-attention (os-window)
  (declare (ignorable os-window)))

(defun focus-os-window (os-window)
  (declare (ignorable os-window)))

(defun (setf os-window-monitor) (os-window monitor)
  (declare (ignorable os-window monitor)))

(defun os-window-focused? (os-window)
  (declare (ignorable os-window)))

(defun os-window-iconified? (os-window)
  (declare (ignorable os-window)))

(defun os-window-visible? (os-window)
  (declare (ignorable os-window)))

(defun os-window-maximized? (os-window)
  (declare (ignorable os-window)))

(defun os-window-hovered? (os-window)
  (declare (ignorable os-window)))

(defun os-window-opacity (os-window)
  (declare (ignorable os-window)))

(defun (setf os-window-resizable?) (value os-window)
  (declare (ignorable os-window value)))

(defun (setf os-window-decorated?) (value os-window)
  (declare (ignorable os-window value)))

(defun (setf os-window-floating?) (value os-window)
  (declare (ignorable value os-window)))

(defun (setf os-window-opacity) (value os-window)
  (declare (ignorable os-window value)))

(defun (setf os-window-mouse-passthrough) (value os-window)
  (declare (ignorable value os-window)))

(defun os-window-poll-events (os-window)
  (declare (ignorable os-window)))

(defun os-window-wait-events (os-window)
  (declare (ignorable os-window)))

(defun os-window-wait-events-timeout (os-window)
  (declare (ignorable os-window)))

(defun os-window-post-empty-event (os-window)
  (declare (ignorable os-window)))

