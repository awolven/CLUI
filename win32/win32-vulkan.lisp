(in-package :clui)

(defun get-win32-required-instance-extensions ()
  (list (symbol-value (intern "VK_KHR_SURFACE_EXTENSION_NAME" :vk))
	(symbol-value (intern "VK_KHR_WIN32_SURFACE_EXTENSION_NAME" :vk))))


