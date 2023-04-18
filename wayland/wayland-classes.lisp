(in-package :clui)

(defclass wayland:desktop-mixin (clui:display-mixin)
  ())

(defclass wayland:monitor-mixin (clui:monitor-mixin)
  ())

(defclass wayland:screen-mixin (clui:screen-mixin)
  ())

(defclass wayland:window-mixin (clui:os-window-mixin)
  ((%width
    :type real
    :initform 0
    :accessor last-width)
   
   (%height
    :type real
    :initform 0
    :accessor last-height)

   (%visible?
    :type boolean
    :accessor last-visible?)

   (%maximized?
    :type boolean
    :initform nil
    :accessor last-maximized?)

   (%activated?
    :type boolean
    :accessor last-activated?)

   (%fullscreen?
    :type boolean
    :accessor last-fullscreen?)

   (%hovered?
    :type boolean
    :accessor last-hovered?)

   (%transparent?
    :type boolean
    :initform nil
    :accessor last-transparent?)

   (%surface
    :accessor window-surface)

   (%callback
    :accessor window-callback)

   (%pending-width
    :accessor pending-width)

   (%pending-height
    :accessor pending-height)

   (%pending-maximized?
    :accessor pending-maximized?)

   (%pending-iconified?
    :accessor pending-iconified?)

   (%pending-activated?
    :accessor pending-activated?)

   (%pending-fullscreen
    :accessor pending-fullscreen?)

   (%xdg-surface
    :accessor xdg-surface)

   (%xgd-toplevel
    :accessor xgd-toplevel)

   (%xdg-decoration
    :accessor xdg-decoration)

   (%xdg-decoration-mode
    :accessor xdg-decoration-mode)

   (%current-cursor
    :accessor current-cursor)

   (%cursor-pos-x
    :accessor last-cursor-pos-x)

   (%cursor-pos-y
    :accessor last-cursor-pos-y)

   (%title
    :writer (setf %window-title))

   (%app-id
    :accessor window-app-id)

   (%scale
    :accessor %window-scale)

   (%monitors
    :accessor %window-monitors)

   (%relative-pointer
    :accessor relative-pointer)

   (%locked-pointer
    :accessor locked-pointer)

   (%confined-pointer
    :accessor confined-pointer)

   (%idle-inhibitor
    :accessor idle-inhibitor)

   (%decorations-buffer
    :accessor decorations-buffer)

   (%decorations-top
    :accessor decorations-top)

   (%decorations-left
    :accessor decorations-left)

   (%decorations-right
    :accessor decorations-right)

   (%decorations-bottom
    :accessor decorations-bottom)

   (%decorations-focus
    :accessor decorations-focus)))

   

(defclass wayland:cursor-mixin (clui:cursor-mixin)
  ())

(defclass wayland:desktop (wayland:desktop-mixin)
  ())

(defclass wayland:monitor (wayland:monitor-mixin)
  ())

(defclass wayland:screen (wayland:screen-mixin)
  ())

(defclass wayland:window (wayland:window-mixin)
  ())

(defclass wayland:cursor (wayland:cursor-mixin)
  ())


