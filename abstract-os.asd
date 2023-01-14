(defsystem abstract-os
  :depends-on (#+darwin :objc-runtime)
  :serial t
  :components
  (#-darwin
   (:file "noffi-patches")
   (:file "package")
   (:file "objc-binding")
   (:file "ns-bindings")
   (:file "app")
   #+windows(:file "classes-win32")
   #+darwin(:file "classes-cocoa")
   (:file "classes")
   (:file "abstract-os")
   #+windows(:file "windows-impl")
   #+darwin(:file "cocoa-impl")
   #+linux(:file "x11-impl")
   #+linux(:file "wayland-impl")
   (:file "api")))

