(in-package :clui)

(defclass wayland:desktop-mixin (clui:display-mixin)
  ())

(defclass wayland:monitor-mixin (clui:monitor-mixin)
  ())

(defclass wayland:screen-mixin (clui:screen-mixin)
  ())

(defclass wayland:window-mixin (clui:os-window-mixin)
  ())

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


