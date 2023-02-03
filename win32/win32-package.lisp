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

	   #:desktop-with-vulkan-mixin
	   #:desktop-with-opengl-mixin
	   #:desktop-with-vulkan
	   #:desktop-with-opengl

	   #:vulkan-window-mixin
	   #:wgl-window-mixin

	   #:wgl-window
	   #:vulkan-window))
	   
