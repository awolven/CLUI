(in-package :cl-user)

;;(eval-when (:compile-toplevel :load-toplevel :execute)
;;  (noffi::boot-ht)
  (noffi::noffi-syntax t)
  ;;(defparameter noffi::*last-good-token* nil))

;;(eval-when (:compile-toplevel)
  #_{
  #include <stdlib.h>
  #include <string.h>
  #include <unistd.h>
  #include <limits.h>
  #include <stdio.h>
  #include <locale.h>
  #include <fcntl.h>
  #include <errno.h>
  #define __USE_GNU
  #include <poll.h>
  #include <signal.h>
  #include <time.h>
  #include <X11/Xlib.h>
  #include <X11/Xlib-xcb.h>
  #include <X11/Xutil.h>
  #include <X11/cursorfont.h>
  #include <X11/Xmd.h>
  #include <X11/keysym.h>
  #include <X11/Xatom.h>
  #include <X11/Xresource.h>
  #include <X11/Xcursor/Xcursor.h>
  #include <X11/extensions/randr.h>
  #include <X11/extensions/Xrandr.h>
  #include <X11/XKBlib.h>
  #include <X11/extensions/Xinerama.h>
/*#include <X11/extensions/XInput2.h>*/
  #include <X11/extensions/XI2.h>
  #include <X11/extensions/shape.h>

  }
;;  )
