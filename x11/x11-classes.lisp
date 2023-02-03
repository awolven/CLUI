(in-package :clui)

(defclass x11:server-mixin (clui:display-mixin)
  ((ip-addr)
   (display-id)))

(defclass x11:local-server-mixin (x11:server-mixin)
  ())

(defclass x11:remote-server-mixin (x11:server-mixin)
  ())

(defclass x11:local-server (x11:local-server-mixin)
  ())

(defclass x11:remote-server (x11:remote-server-mixin)
  ())

(defclass x11:screen-mixin (clui:screen-mixin)
  ((screen-id)))

(defclass x11:window-mixin (clui:window-mixin)
  ())

(defclass x11:cursor-mixin (clui:cursor-mixin)
  ())

(defclass x11:monitor-mixin (clui:monitor-mixin)
  ())

(defclass x11:screen (x11:screen-mixin)
  ())

(defclass x11:window (x11:window-mixin)
  ())

(defclass x11:cursor (x11:cursor-mixin)
  ())

(defclass x11:monitor (x11:monitor-mixin)
  ())
