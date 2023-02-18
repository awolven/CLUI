(in-package :cl-user)

(defpackage :x11)

(defpackage :clui.v0
  (:export #:timeout-event
	   #:window-move-event
	   #:window-resize-event
	   #:window-iconify-event
	   #:window-deiconify-event
	   #:window-maximize-event
	   #:window-restore-event
	   #:window-fullscreen-event
	   #:window-show-event
	   #:window-focus-event
	   #:window-defocus-event
	   #:window-hide-event
	   #:window-repaint-event
	   #:window-created-event
	   #:window-close-event
	   #:window-destroyed-event
	   #:window-monitor-switched-event
	   #:pointer-button-press-event
	   #:pointer-button-release-event
	   #:pointer-button-hold-event
	   #:pointer-click-event
	   #:pointer-double-click-event
	   #:pointer-button-hold-and-drag-event
	   #:pointer-wheel-event
	   #:pointer-motion-event
	   #:pointer-enter-event
	   #:pointer-exit-event
	   #:key-press-event
	   #:key-release-event
	   #:character-event))
	   

(defpackage :clui
  (:use :cl)
  (:export #:*app*
	   #:exit?
	   #:application-exit?
	   #:default-application-class-for-window
	   #:default-window-class-for-application
	   #:os-window-should-close?
	   #:poll-application-events
	   #:wait-application-events
	   #:get-window-content-scale
	   #:os-window-title
	   #:get-os-window-pos
	   #:set-os-window-pos
	   #:set-os-window-cursor-pos
	   #:get-os-window-size
	   #:focus-os-window
	   #:hide-os-window
	   #:show-os-window
	   #:maximize-os-window
	   #:restore-os-window
	   #:iconify-window
	   #:get-os-window-frame-size
	   #:get-os-window-framebuffer-size
	   #:set-os-window-size
	   #:set-os-window-aspect-ratio
	   #:set-os-window-size-limits
	   #:destroy-os-window
	   #:shutdown-application

	   #:operating-system-mixin
	   #:unix-mixin
	   #:linux-mixin
	   #:bsd-mixin
	   #:ms-windows-mixin
	   #:macos-mixin
	   #:gui-mixin
	   #:x11-mixin
	   #:win32-mixin
	   #:cocoa-mixin
	   #:wayland-mixin

	   #:homemade-window-mixin
	   #:homemade-view-mixin

	   #:vulkan-support-mixin
	   #:opengl-support-mixin
	   #:display
	   #:display-mixin
	   #:screen
	   #:screen-mixin
	   #:monitor
	   #:monitor-mixin
	   #:window
	   #:window-mixin
	   #:os-window
	   #:os-window-mixin
	   #:cursor
	   #:cursor-mixin
	   #:handle-mixin


	   )
	   

  



  #+vulkan
  (:export #:get-required-instance-extensions)
  #-darwin
  (:import-from :noffi
		#:copy-ptr
		#:%cons-ptr
		#:c-sizeof-type
		#:c-addr-of
		#:c-cast
		#:offset-of
		#:ptr-value
		#:cval-value
		#:ptr-offset
		#:c-aref
		#:c->-addr
		#:clet&
		#:cons-ptr
		#:ptr-nullptr-p

		#:noffi-syntax #:clet #:c-addr-of #:pkg-use #:c-coerce #:defcfun))
