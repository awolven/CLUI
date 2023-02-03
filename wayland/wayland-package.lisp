(in-package :cl-user)

(defpackage wayland
  (:export window
	   cursor
	   linux-platform
	   desktop
	   screen
	   monitor

	   vulkan-window
	   opengl-window

	   linux-platform-mixin
	   desktop-mixin
	   screen-mixin	   
	   monitor-mixin
	   window-mixin
	   cursor-mixin
	   
	   vulkan-window-mixin
	   opengl-window-mixin))
