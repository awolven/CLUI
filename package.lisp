(in-package :cl-user)

(defpackage :abstract-os
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
	   #:shutdown-application)
  #+vulkan
  (:export #:get-required-instance-extensions)
  #-darwin
  (:import-from :noffi
		#:copy-ptr
		#:%cons-ptr
		#:c-sizeof-type
		#:c-addr-of
		#:c-cast
		#:with-stack-allocated-structure
		#:offset-of
		#:ptr-value
		#:cval-value
		#:ptr-offset

		#:noffi-syntax #:clet #:c-addr-of #:pkg-use #:c-coerce #:defcfun))
