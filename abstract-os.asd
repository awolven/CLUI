(defsystem abstract-os
  :serial t
  :components
  (#-darwin
   (:file "noffi-patches")
   (:file "package")
   (:file "app")
   #+windows(:file "classes-win32")
   #+darwin(:file "classes-cocoa")
   (:file "classes")
   (:file "abstract-os")
   #+windows(:file "windows-impl")
   #+darwin(:file "cocoa-impl")
   #+linux(:file "x11-impl")
   #+linux(:file "wayland-impl")))

