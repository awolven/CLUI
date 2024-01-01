(in-package :clui)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (ignore-errors (find-package :ns))
    (defpackage :ns)))

(export '(ns::|alloc|
	  ns::|init|
	  ns::|addLocalMonitorForEventsMatchingMask:handler:|)
	:ns)

(defpackage cocoa
  (:export #:monitor
	   #:window
	   #:cursor
	   #:platform
	   #:display
	   #:screen
	   #:view

	   #:metal-view
	   #:vulkan-view
	   #:nsgl-view
	   
	   #:platform-mixin
	   #:display-mixin
	   #:screen-mixin
	   #:window-mixin
	   #:view-mixin
	   #:cursor-mixin
	   #:monitor-mixin

	   #:vulkan-window-mixin
	   #:metal-window-mixin
	   #:nsgl-window-mixin

	   #:display-with-metal
	   #:display-with-opengl	   

	   #:metal-window
	   #:vulkan-window
	   #:nsgl-window

	   #:display-with-krma-mixin
	   #:display-with-krma
	   #:krma-enabled-window-mixin
	   #:krma-enabled-window))
