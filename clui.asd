(defsystem clui
  :serial t
  :depends-on
  (#+darwin :clui/cocoa
	    #+windows :clui/win32
	    #+linux :clui/linux)
  :components
  (#-darwin
   (:file "noffi-patches")
))

#+darwin
(defsystem clui/cocoa
  :depends-on (:objc-runtime)
  :serial t
  :components
  ((:file "package")
   (:file "app")
   (:file "classes")
   (:file "events")
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
(defsystem clui/win32
  :depends-on ()
  :serial t
  :components
  (;;#+ccl(:file "../noffi/src/patch-ccl")
   (:file "../noffi/other/clex/src/clex")
   (:file "../noffi/other/lalr/lalr")
   (:file "../noffi/src/file")
   (:file "../noffi/src/compiler-warn")
   (:file "../noffi/src/package")
   (:file "../noffi/src/forward")
   (:file "../noffi/src/lispdep")
   (:file "../noffi/src/util")
   (:file "../noffi/src/string-table")
   (:file "../noffi/src/define-grammar")
   (:file "../noffi/src/lexer")
   (:file "../noffi/src/cpp")
   (:file "../noffi/src/parsing")
   (:file "../noffi/src/grammar")
   (:file "../noffi/src/syntax")
   (:file "../noffi/src/ffigen")
   (:file "../noffi/src/adt")
   (:file "../noffi/src/comp")
   (:file "../noffi/src/runtime")
   (:file "../noffi/src/abi-ms-amd64")
   (:file "win32/boot-ht")
	  

   (:file "noffi-patches")

   (:file "win32/win32")
   
   (:file "package")
   (:file "protocols")
   (:file "app")
   (:file "classes")
   (:file "events")
   (:file "win32/win32-package")
   (:file "win32/win32-classes")
   (:file "compute-concrete-class")
   (:file "win32/win32-init")
   (:file "win32/win32-window")
   (:file "win32/win32-monitor")
   (:file "monitor")
   (:file "abstract-os")
   (:file "api")

   #+NIL(:file "win32/test")))

#+linux
(defsystem clui/linux
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
