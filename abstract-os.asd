(defsystem abstract-os
  :serial t
  :components
  ((:file "noffi-patches")
   (:file "package")
   #+windows(:file "classes-win32")
   (:file "classes")
   #+windows(:file "windows-impl")
   #+darwin(:file "cocoa-impl")
   #+linux(:file "x11-impl")
   #+linux(:file "wayland-impl")
   (:file "silica")))

