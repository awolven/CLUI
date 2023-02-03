(in-package :clui)

(defclass protocol-class (standard-class)
  ())

#+ccl
(defmethod ccl:validate-superclass ((a protocol-class) (b standard-class))
  t)

(defgeneric compute-concrete-class (protocol &rest args))

(defmethod make-instance ((class protocol-class) &rest initargs)
  (apply #'make-instance (apply #'compute-concrete-class (allocate-instance class) initargs) initargs))

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

(defclass screen (window)
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

(defclass compass-cursor (cursor)
  ()
  (:metaclass protocol-class))

(defclass NESW-cursor (cursor)
  ()
  (:metaclass protocol-class))

(defclass EW-cursor (cursor)
  ()
  (:metaclass protocol-class))

(defclass NS-cursor (cursor)
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

(defclass no-select-cursor (cursor)
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
(defclass timeout-event ()
  ()
  (:metaclass protocol-class))

;; window events
(defclass window-move-event ()
  ()
  (:metaclass protocol-class))

(defclass window-resize-event ()
  ()
  (:metaclass protocol-class))

(defclass window-iconify-event ()
  ()
  (:metaclass protocol-class))

(defclass window-deiconify-event ()
  ()
  (:metaclass protocol-class))

(defclass window-maximize-event ()
  ()
  (:metaclass protocol-class))

(defclass window-restore-event ()
  ()
  (:metaclass protocol-class))

(defclass window-fullscreen-event ()
  ()
  (:metaclass protocol-class))

(defclass window-show-event ()
  ()
  (:metaclass protocol-class))

(defclass window-focus-event ()
  ()
  (:metaclass protocol-class))

(defclass window-defocus-event ()
  ()
  (:metaclass protocol-class))

(defclass window-hide-event ()
  ()
  (:metaclass protocol-class))

(defclass window-repaint-event ()
  ()
  (:metaclass protocol-class))

;; window manager (desktop) events
(defclass window-created-event ()
  ()
  (:metaclass protocol-class))

(defclass window-close-event ()
  ()
  (:metaclass protocol-class))

(defclass window-destroyed-event ()
  ()
  (:metaclass protocol-class))

(defclass window-monitor-switched-event ()
  ()
  (:metaclass protocol-class))

;; pointer events
(defclass pointer-button-press-event ()
  ()
  (:metaclass protocol-class))

(defclass pointer-button-release-event ()
  ()
  (:metaclass protocol-class))

(defclass pointer-button-click-event ()
  ()
  (:metaclass protocol-class))

(defclass pointer-button-double-click-event ()
  ()
  (:metaclass protocol-class))

(defclass pointer-button-hold-event ()
  ()
  (:metaclass protocol-class))

(defclass pointer-button-hold-and-drag-event ()
  ()
  (:metaclass protocol-class))

(defclass pointer-wheel-event ()
  ()
  (:metaclass protocol-class))

(defclass pointer-motion-event ()
  ()
  (:metaclass protocol-class))

(defclass pointer-enter-event ()
  ()
  (:metaclass protocol-class))

(defclass pointer-exit-event ()
  ()
  (:metaclass protocol-class))

;; keyboard events
(defclass key-press-event ()
  ()
  (:metaclass protocol-class))

(defclass key-release-event ()
  ()
  (:metaclass protocol-class))

(defclass character-event ()
  ()
  (:metaclass protocol-class))

;; system level events
(defclass computer-shutdown-event ()
  ()
  (:metaclass protocol-class))

(defclass monitor-connected-event ()
  ()
  (:metaclass protocol-class))

(defclass monitor-disconnected-event ()
  ()
  (:metaclass protocol-class))

(defclass keyboard-connected-event ()
  ()
  (:metaclass protocol-class))

(defclass keyboard-disconnected-event ()
  ()
  (:metaclass protocol-class))

(defclass pointer-connected-event ()
  ()
  (:metaclass protocol-class))

(defclass pointer-disconnected-event ()
  ()
  (:metaclass protocol-class))

(defclass joystick-connected-event ()
  ()
  (:metaclass protocol-class))

(defclass joystick-disconnected-event ()
  ()
  (:metaclass protocol-class))

(defclass spaceball-connected-event ()
  ()
  (:metaclass protocol-class))

(defclass spaceball-disconnected-event ()
  ()
  (:metaclass protocol-class))

