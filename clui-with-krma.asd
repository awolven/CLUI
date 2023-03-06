
(defsystem clui-with-krma
  :serial t
  :depends-on (:clui :krma)

  :components
  ((:file "krma/krma-classes")
   #+windows(:file "win32/win32-vulkan")
   #+linux(:file "x11/x11-vulkan")))
