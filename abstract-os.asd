(defsystem abstract-os
  :serial t
  :depends-on
  (#+darwin :abstract-os/cocoa
	    #+windows :abstract-os/win32
	    #+linux :abstract-os/linux)
  :components
  (#-darwin
   (:file "noffi-patches")
))

#+darwin
(defsystem abstract-os/cocoa
  :depends-on (:objc-runtime)
  :serial t
  :components
  ((:file "package")
   (:file "app")
   (:file "classes")
   (:file "cocoa/cocoa-package")
   (:file "cocoa/objc-runtime")
   (:file "cocoa/objc-binding")
   (:file "cocoa/iokit")
   (:file "cocoa/core-foundation")
   (:file "cocoa/core-graphics")
   (:file "cocoa/core-animation")
   (:file "cocoa/ns-support")
   (:file "cocoa/ns-bindings")
   (:file "cocoa/cocoa-classes")
   (:file "cocoa/cocoa-app")
   (:file "cocoa/cocoa-event")
   (:file "cocoa/cocoa-monitor")
   (:file "cocoa/cocoa-window")
   (:file "monitor")
   (:file "abstract-os")
   (:file "api")))

#+windows
(defsystem abstract-os/win32
  :depends-on (:noffi)
  :serial t
  :components
  ((:file "noffi-patches")
   (:file "package")
   (:file "classes")
   (:file "win32/win32-impl")
   (:file "abstract-os")
   (:file "api")))

#+linux
(defsystem abstract-os/linux
  :depends-on (:noffi)
  :serial t
  :components
  ((:file "nffi-patches")
   (:file "package")
   (:file "classes")
   (:file "x11/x11-impl")
   (:file "wayland/wayland-impl")
   (:file "abstract-os")
   (:file "api")))
