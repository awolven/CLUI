(in-package :clui)

(defpackage win32
  (:export #:monitor
	   #:window
	   #:cursor
	   #:desktop
	   #:screen

	   #:monitor-mixin
	   #:window-mixin
	   #:cursor-mixin
	   #:desktop-mixin
	   #:screen-mixin

	   #:desktop-with-krma-mixin
	   #:desktop-with-opengl-mixin
	   #:desktop-with-krma
	   #:desktop-with-opengl

	   #:krma-enabled-window-mixin
	   #:wgl-enabled-window-mixin

	   #:wgl-enabled-window
	   #:krma-enabled-window))
	   
