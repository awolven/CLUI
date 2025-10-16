(in-package :clui)

(defclass protocol-class (standard-class)
  ())

#+ccl
(defmethod ccl:validate-superclass ((class protocol-class) (super standard-class))
  t)

#+sbcl
(defmethod sb-mop:validate-superclass ((class protocol-class) (super standard-class))
  t)

#+ECL
(defmethod mop:validate-superclass ((class protocol-class) (super standard-class))
  t)

(defgeneric compute-concrete-class (protocol &rest args))

(defmethod make-instance ((class protocol-class) &rest initargs)
  (apply #'make-instance (apply #'compute-make-instance-arguments (allocate-instance class) initargs)))

(defclass display-dependent ()
  ())

(defclass display ()
  ()
  (:metaclass protocol-class))

(defclass medium (display-dependent)
  ()
  (:metaclass protocol-class))

(defclass region ()
  ()
  (:metaclass protocol-class))

(defclass window (region medium)
  ()
  (:metaclass protocol-class))

(defclass view (window)
  ()
  (:metaclass protocol-class))

(defclass screen (display-dependent)
  ()
  (:metaclass protocol-class))

(defclass monitor (display-dependent)
  ()
  (:metaclass protocol-class))

(defclass printer (display-dependent)
  ()
  (:metaclass protocol-class))

;; cursors
(defclass cursor (display-dependent)
  ()
  (:metaclass protocol-class))

(defclass arrow-cursor (cursor)
  ()
  (:metaclass protocol-class))

(defclass hand-cursor (cursor)
  ()
  (:metaclass protocol-class))

(defclass pointing-hand-cursor (hand-cursor)
  ()
  (:metaclass protocol-class))

(defclass open-hand-cursor (hand-cursor)
  ()
  (:metaclass protocol-class))

(defclass closed-hand-cursor (hand-cursor)
  ()
  (:metaclass protocol-class))

(defclass ibeam-cursor (cursor)
  ()
  (:metaclass protocol-class))

(defclass crosshair-cursor (cursor)
  ()
  (:metaclass protocol-class))

(defclass compass-cursor (cursor)
  ()
  (:metaclass protocol-class))

(defclass resize-N-cursor (cursor)
  ()
  (:metaclass protocol-class))

(defclass resize-S-cursor (cursor)
  ()
  (:metaclass protocol-class))

(defclass resize-E-cursor (cursor)
  ()
  (:metaclass protocol-class))

(defclass resize-W-cursor (cursor)
  ()
  (:metaclass protocol-class))

(defclass resize-NE-cursor (cursor)
  ()
  (:metaclass protocol-class))

(defclass resize-NW-cursor (cursor)
  ()
  (:metaclass protocol-class))

(defclass resize-SE-cursor (cursor)
  ()
  (:metaclass protocol-class))

(defclass resize-SW-cursor (cursor)
  ()
  (:metaclass protocol-class))

(defclass up-cursor (cursor)
  ()
  (:metaclass protocol-class))

(defclass down-cursor (cursor)
  ()
  (:metaclass protocol-class))

(defclass wait-cursor (cursor)
  ()
  (:metaclass protocol-class))

(defclass not-allowed-cursor (cursor)
  ()
  (:metaclass protocol-class))

(defclass activate-menu-cursor (cursor)
  ()
  (:metaclass protocol-class))

(defclass activate-help-cursor (cursor)
  ()
  (:metaclass protocol-class))

;; widgets
(defclass icon (view)
  ()
  (:metaclass protocol-class))

(defclass scrollbar (view)
  ()
  (:metaclass protocol-class))

(defclass label (view)
  ()
  (:metaclass protocol-class))

(defclass button (view)
  ()
  (:metaclass protocol-class))

(defclass image (view)
  ()
  (:metaclass protocol-class))

(defclass checkbox (view)
  ()
  (:metaclass protocol-class))

(defclass progress-bar (view)
  ()
  (:metaclass protocol-class))

(defclass bullet-point (label)
  ()
  (:metaclass protocol-class))

(defclass slider (view)
  ()
  (:metaclass protocol-class))

(defclass text-edit-box (view)
  ()
  (:metaclass protocol-class))

(defclass combo-box (view)
  ()
  (:metaclass protocol-class))

(defclass multiline-text-edit-box (view)
  ()
  (:metaclass protocol-class))

(defclass radio-button (view)
  ()
  (:metaclass protocol-class))

(defclass file-selector (view)
  ()
  (:metaclass protocol-class))

(defclass folder-selector (file-selector)
  ()
  (:metaclass protocol-class))

(defclass color-selector (view)
  ()
  (:metaclass protocol-class))

(defclass color-editor (view)
  ()
  (:metaclass protocol-class))

(defclass list-box (view)
  ()
  (:metaclass protocol-class))

(defclass dial (view)
  ()
  (:metaclass protocol-class))

(defclass spin-box (view)
  ()
  (:metaclass protocol-class))

(defclass menu (view)
  ()
  (:metaclass protocol-class))

(defclass menu-item (view)
  ()
  (:metaclass protocol-class))

(defclass tab-bar (view)
  ()
  (:metaclass protocol-class))

(defclass tab-page (view)
  ()
  (:metaclass protocol-class))

(defclass grid (view)
  ()
  (:metaclass protocol-class))

(defclass date-edit-box (view)
  ()
  (:metaclass protocol-class))

(defclass time-edit-box (view)
  ()
  (:metaclass protocol-class))

(defclass calendar (view)
  ()
  (:metaclass protocol-class))

(defclass contact-information-form (view)
  ()
  (:metaclass protocol-class))

(defclass payment-information-form (view)
  ()
  (:metaclass protocol-class))


;; events
(defclass timer-event (display-dependent)
  ()
  (:metaclass protocol-class))

;; window events
(defclass window-move-event (display-dependent)
  ()
  (:metaclass protocol-class))

(defclass window-resize-event (display-dependent)
  ()
  (:metaclass protocol-class))

(defclass window-iconify-event (display-dependent)
  ()
  (:metaclass protocol-class))

(defclass window-deiconify-event (display-dependent)
  ()
  (:metaclass protocol-class))

(defclass window-maximize-event (display-dependent)
  ()
  (:metaclass protocol-class))

(defclass window-restore-event (display-dependent)
  ()
  (:metaclass protocol-class))

(defclass window-fullscreen-event (display-dependent)
  ()
  (:metaclass protocol-class))

(defclass window-show-event (display-dependent)
  ()
  (:metaclass protocol-class))

(defclass window-focus-event (display-dependent)
  ()
  (:metaclass protocol-class))

(defclass window-defocus-event (display-dependent)
  ()
  (:metaclass protocol-class))

(defclass window-hide-event (display-dependent)
  ()
  (:metaclass protocol-class))

(defclass window-repaint-event (display-dependent)
  ()
  (:metaclass protocol-class))

;; window manager (desktop) events
(defclass window-created-event (display-dependent)
  ()
  (:metaclass protocol-class))

(defclass window-close-event (display-dependent)
  ()
  (:metaclass protocol-class))

(defclass window-destroyed-event (display-dependent)
  ()
  (:metaclass protocol-class))

(defclass window-monitor-switched-event (display-dependent)
  ()
  (:metaclass protocol-class))

;; pointer events
(defclass pointer-button-press-event (display-dependent)
  ()
  (:metaclass protocol-class))

(defclass pointer-button-release-event (display-dependent)
  ()
  (:metaclass protocol-class))

(defclass pointer-button-click-event (display-dependent)
  ()
  (:metaclass protocol-class))

(defclass pointer-button-double-click-event (display-dependent)
  ()
  (:metaclass protocol-class))

(defclass pointer-button-hold-event (display-dependent)
  ()
  (:metaclass protocol-class))

(defclass pointer-button-hold-and-drag-event (display-dependent)
  ()
  (:metaclass protocol-class))

(defclass pointer-wheel-event (display-dependent)
  ()
  (:metaclass protocol-class))

(defclass pointer-motion-event (display-dependent)
  ()
  (:metaclass protocol-class))

(defclass pointer-enter-event (display-dependent)
  ()
  (:metaclass protocol-class))

(defclass pointer-exit-event (display-dependent)
  ()
  (:metaclass protocol-class))

;; keyboard events
(defclass key-press-event (display-dependent)
  ()
  (:metaclass protocol-class))

(defclass key-repeat-event (display-dependent)
  ()
  (:metaclass protocol-class))

(defclass key-release-event (display-dependent)
  ()
  (:metaclass protocol-class))

(defclass character-event (display-dependent)
  ()
  (:metaclass protocol-class))

;; system level events
(defclass computer-shutdown-event ()
  ()
  (:metaclass protocol-class))

(defclass monitor-connected-event (display-dependent)
  ()
  (:metaclass protocol-class))

(defclass monitor-disconnected-event (display-dependent)
  ()
  (:metaclass protocol-class))

(defclass keyboard-connected-event (display-dependent)
  ()
  (:metaclass protocol-class))

(defclass keyboard-disconnected-event (display-dependent)
  ()
  (:metaclass protocol-class))

(defclass pointer-connected-event (display-dependent)
  ()
  (:metaclass protocol-class))

(defclass pointer-disconnected-event (display-dependent)
  ()
  (:metaclass protocol-class))

(defclass joystick-connected-event (display-dependent)
  ()
  (:metaclass protocol-class))

(defclass joystick-disconnected-event (display-dependent)
  ()
  (:metaclass protocol-class))

(defclass spaceball-connected-event (display-dependent)
  ()
  (:metaclass protocol-class))

(defclass spaceball-disconnected-event (display-dependent)
  ()
  (:metaclass protocol-class))

