(in-package :clui)

(cffi:defcstruct ns::|CATransform3D|) ;;fixme

(cffi:defcstruct ns::|_CARenderRendererInfo|) ;;fixme
(cffi:defcstruct NS::|_CAView|)
(cffi:defcstruct ns::|__CAView|) ;;fixme

#+vulkan
(defun get-cocoa-required-instance-extensions ()
  (list "VK_KHR_surface"
	(if (symbol-value (intern (symbol-name '*use-metal-surface*) :vk))
	    "VK_EXT_metal_surface"
	    "VK_MVK_macos_surface")))
