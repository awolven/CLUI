(in-package :noffi)

#.(or'
   ;; Darwin
   
   #+(OR (AND CCL DARWIN-TARGET X8664-TARGET)
         (AND SBCL DARWIN X86-64))
   (progn
     (setq *abi* +abi-amd64-sysv+)
     (setf de.bauhh.cpp::*cc* "cc"
           de.bauhh.cpp::*cc-args* '("-m64" "-ObjC" "-I" "/opt/X11/include")))

   #+(AND CCL DARWIN-TARGET X8632-TARGET)
   (progn
     (setq *abi* +abi-i386-sysv+)
     (setf de.bauhh.cpp::*cc* "cc"
           de.bauhh.cpp::*cc-args* '("-m32" "-ObjC" "-I" "/opt/X11/include")))

   ;; Linux

   #+(or (AND CCL LINUX-TARGET X8664-TARGET)
         (AND SBCL LINUX X86-64))
   (progn
     (setq *abi* +abi-amd64-sysv+)
     (setf de.bauhh.cpp::*cc* "cc"
           de.bauhh.cpp::*cc-args* '("-m64")))

   #+(or (AND CCL LINUX-TARGET X8632-TARGET)
         (AND SBCL LINUX X86))
   (progn
     (setq *abi* +abi-i386-sysv+)
     (setf de.bauhh.cpp::*cc* "cc"
           de.bauhh.cpp::*cc-args* '("-m32")))

   ;; Windows

   ;; This is a mess as there are several ABIs for Windows to pick from. We
   ;; generally want the ABI which matches the one that was used to build
   ;; your Lisp with.

   ;; See <https://www.msys2.org/docs/environments/>

   #+(OR (AND CCL WINDOWS-TARGET X8664-TARGET)
         (AND SBCL WIN32 X86-64))
   (progn
     (setq *abi* +abi-amd64-mingw64+)       ;Not quite right.
     (setf de.bauhh.cpp::*cc* "cc"
           de.bauhh.cpp::*cc-args* '("-m64" "-DUNICODE=1")))

   #+(AND CCL LINUX-TARGET X8632-TARGET)
   (progn
     (setq *abi* +abi-i386-mingw32+)
     (setf de.bauhh.cpp::*cc* "cc"
           de.bauhh.cpp::*cc-args* '("-m32" "-DUNICODE=1")))

   (eval-when (:compile-toplevel :load-toplevel :execute)
     (error "Configure noffi for your platform.")) )

(boot-ht)                               ;hmm
