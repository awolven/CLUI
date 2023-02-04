(in-package :cl-user)

(defpackage x11
  (:export #:window
	   #:cursor
	   #:desktop
	   #:screen
	   #:monitor

	   #:server-mixin
	   #:local-server-mixin
	   #:remote-server-mixin
	   
	   #:window-mixin
	   #:cursor-mixin
	   #:client-mixin
	   #:server-mixin
	   #:screen-mixin
	   #:monitor-mixin

	   #:vulkan-window-mixin
	   #:glx-window-mixin

	   #:local-server
	   #:remote-server

	   #:vulkan-window
	   #:glx-window))
