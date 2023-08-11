CLUI is a Common Lisp version of GLFW.

It runs on MacOS, MS Windows and Linux.

To build and run CLUI you must register the system with ASDF:

Ex:

(push "~/clui/" asdf:*central-registry*)

On Linux you must install the following packages:

libXinerama-devel
libXrandr-devel
libXcursor-devel

(ql:quickload :clui)