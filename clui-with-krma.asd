
(defsystem clui-with-krma
  :serial t
  :depends-on (:clui :krma)

  :components
  ((:file "krma/krma-classes")
   (:file "win32/win32-vulkan")
   #+NIL(:file "krma/clui-with-krma")))
