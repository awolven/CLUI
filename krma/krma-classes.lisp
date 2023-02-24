(in-package :clui)

#+win32
(defclass win32:desktop-with-krma-mixin (krma::krma-enabled-display-mixin win32:desktop-mixin)
  ())

#+win32
(defclass win32:desktop-with-krma (win32:desktop-with-krma-mixin)
  ())

#+win32
(defclass win32:krma-enabled-window-mixin (krma::krma-window-mixin win32:window-mixin)
  ())

#+win32
(defclass win32:krma-enabled-window (win32:krma-enabled-window-mixin)
  ())

#+cocoa
(defclass cocoa:desktop-with-krma-mixin (krma::krma-enabled-display-mixin cocoa:desktop-mixin)
  ())

#+cocoa
(defclass cocoa:desktop-with-krma (cocoa:desktop-with-krma-mixin)
  ())

#+cocoa
(defclass cocoa:krma-enabled-window-mixin (krma::krma-window-mixin cocoa:window-mixin)
  ())

#+cocoa
(defclass cocoa:krma-enabled-window (cocoa:krma-enabled-window-mixin)
  ())

#+x11
(defclass x11:local-server-with-krma-mixin (krma::krma-enabled-display-mixin x11:local-server-mixin)
  ())

#+x11
(defclass x11:local-server-with-krma (win32:local-server-with-krma-mixin)
  ())

#+x11
(defclass x11:krma-enabled-window-mixin (krma::krma-window-mixin x11:window-mixin)
  ())

#+x11
(defclass x11:krma-enabled-window (x11:krma-enabled-window-mixin)
  ())

#+wayland
(defclass wayland:desktop-with-krma-mixin (krma::krma-enabled-display-mixin wayland:desktop-mixin)
  ())

#+wayland
(defclass wayland:desktop-with-krma (wayland:desktop-with-krma-mixin)
  ())

#+wayland
(defclass wayland:krma-enabled-window-mixin (krma::krma-window-mixin wayland:window-mixin)
  ())

#+wayland
(defclass wayland:krma-enabled-window (wayland:krma-enabled-window-mixin)
  ())

#+win32
(defmethod vk::get-required-instance-extensions ((display win32:desktop-with-krma-mixin))
  (get-win32-required-instance-extensions))

#+cocoa
(defmethod vk::get-required-instance-extensions ((display cocoa:desktop-with-krma-mixin))
  (get-cocoa-required-instance-extensions))

#+x11
(defmethod vk::get-required-instance-extensions ((display x11:local-server-with-krma-mixin))
  (get-x11-required-instance-extensions))

#+wayland
(defmethod vk::get-required-instance-extensions ((display wayland:desktop-with-krma-mixin))
  (get-wayland-required-instance-extensions))

#+win32
(defmethod compute-make-display-instance-arguments ((protocol clui:display)
						    (cocoa null)
						    (metal null)
						    (opengl null)
						    (vulkan t)
						    (wayland null)
						    (win32 t)
						    (x11 null)
						    &rest initargs
						    &key &allow-other-keys)
  (list* (find-class 'win32:desktop-with-krma) initargs))

#+cocoa
(defmethod compute-make-display-instance-arugments (protocol
						    (cocoa t)
						    (metal null)
						    (opengl null)
						    (vulkan t)
						    (wayland null)
						    (win32 null)
						    (x11 null)
						    &rest initargs)
  (list* (find-class 'cocoa:desktop-with-krma) initargs))

#+x11
(defmethod compute-make-display-instance-arugments (protocol
						    (cocoa null)
						    (metal null)
						    (opengl null)
						    (vulkan t)
						    (wayland null)
						    (win32 null)
						    (x11 t)
						    &rest initargs)
  (list* (find-class 'x11:local-server-with-krma) initargs))

#+wayland
(defmethod compute-make-display-instance-arugments (protocol
						    (cocoa null)
						    (metal null)
						    (opengl null)
						    (vulkan t)
						    (wayland t)
						    (win32 null)
						    (x11 null)
						    &rest initargs)
  (list* (find-class 'wayland:desktop-with-krma) initargs))

#+win32
(defmethod get-a-win32-window-class ((display win32:desktop-with-krma-mixin) errorp &rest initargs
				     &key &allow-other-keys)
  (declare (ignore initargs))
  (find-class 'win32:krma-enabled-window errorp))

;; :animable? used to be a keyword here, but animable? affects whether the
;; run loop polls for messages or waits for messages, and vulkan can also
;; be used in a demand-refresh wait-for-messages manner


#+win32
(defmethod create-native-window-surface ((display win32:desktop-mixin)
					 instance window
					 &optional (allocator vk::+null-allocator+))
  (vk::create-win32-window-surface instance window allocator))

#+cocoa
(defmethod create-native-window-surface ((display cocoa:desktop-mixin)
					 instance (window cocoa:window-mixin)
					 &optional (allocator vk::+null-allocator+))
  (vk::create-cocoa-window-surface instance window allocator))


(defmethod initialize-helper-window ((display win32:desktop-with-krma-mixin) helper-window)
  (setf (vk::render-surface helper-window)
	(create-native-window-surface display
				      (vk::get-vulkan-instance display)
				      helper-window))
  (setf (vk::window (vk::render-surface helper-window)) helper-window)
  helper-window)

(defmethod helper-window-class ((display win32:desktop-with-krma-mixin))
  'vk::vulkan-helper-window)
