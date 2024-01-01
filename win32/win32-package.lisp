(in-package :clui)

(defpackage win32
  (:export #:monitor
	   #:window
	   #:cursor
	   #:display
	   #:screen

	   #:monitor-mixin
	   #:window-mixin
	   #:cursor-mixin
	   #:display-mixin
	   #:screen-mixin

	   #:display-with-krma-mixin
	   #:display-with-opengl-mixin
	   #:display-with-krma
	   #:display-with-opengl

	   #:krma-enabled-window-mixin
	   #:wgl-enabled-window-mixin

	   #:wgl-enabled-window
	   #:krma-enabled-window))
	   
