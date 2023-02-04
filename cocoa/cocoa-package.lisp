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
	   #:desktop
	   #:screen
	   #:view

	   #:metal-view
	   #:vulkan-view
	   #:nsgl-view
	   
	   #:platform-mixin
	   #:desktop-mixin
	   #:screen-mixin
	   #:window-mixin
	   #:view-mixin
	   #:cursor-mixin
	   #:monitor-mixin

	   #:vulkan-window-mixin
	   #:metal-window-mixin
	   #:nsgl-window-mixin

	   #:desktop-with-metal
	   #:desktop-with-opengl	   

	   #:metal-window
	   #:vulkan-window
	   #:nsgl-window))
