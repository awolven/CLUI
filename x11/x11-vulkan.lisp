(in-package :clui)

(defun get-x11-required-instance-extensions ()
  ;; need some logic here to see if XCB actually works or if we need Xlib surface
  (list vk::VK_KHR_SURFACE_EXTENSION_NAME
	vk::VK_KHR_XLIB_SURFACE_EXTENSION_NAME
	vk::VK_KHR_XCB_SURFACE_EXTENSION_NAME))


