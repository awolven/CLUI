(in-package :cl-user)
(eval-when (:compile-toplevel :load-toplevel :execute) (use-package :noffi))

(noffi-syntax t)

(eval-when (:compile-toplevel :load-toplevel :execute)
  _{#include <X11/Xlib.h>})

(use-library "X11")

(defun run ()
  (let* ((w 800) (h 600)
         (dpy (_XOpenDisplay nil))
         (screen (_DefaultScreen dpy))
         (fg (_WhitePixel dpy screen))
         (bg (_BlackPixel dpy screen))
         (win
          (clet& ((attr _<XSetWindowAttributes>))
            (setf (_.bit_gravity attr)      _ForgetGravity
                  (_.background_pixel attr) bg
                  (_.border_pixel attr)     fg)
            (_XCreateWindow dpy (_DefaultRootWindow dpy)
                            0 0 800 600 1          ;x y w h border
                            #_CopyFromParent       ;depth
                            #_InputOutput          ;class
                            #_CopyFromParent       ;visual
                            (logior #_CWBitGravity #_CWBackPixel #_CWBorderPixel)
                            attr)))
         (gc (_XCreateGC dpy win 0 nil)))
    (_XSetForeground dpy gc fg)
    (_XSetBackground dpy gc bg)
    (_XSelectInput dpy win (logior _ExposureMask _StructureNotifyMask))
    (_XStoreName dpy win "Noffi Demo")
    (_XMapWindow dpy win)
    (labels ((repaint ()
               (loop for i from 1 to 19
                     do (loop for j from 1 to 19
                              do (_XDrawLine dpy win gc
                                             (* w (/ i 20))
                                             (* 1/10 h)
                                             (* w (/ j 20))
                                             (* 9/10 h))))))
      (clet& ((ev _<XEvent>))
        (unwind-protect
             (loop
               ;; Waiting for an event
               ;;
               ;; This is not trivial. In case of an interrupt we cannot really unwind
               ;; what ever needs to be unwound in foreign land. Hence CCL disables
               ;; interrupts while in foreign land for robustness. SBCL services
               ;; interrupts and when asked for non-local exit out of this function,
               ;; the second time it does so it unwinds _without_ any cleanup, which is
               ;; very nasty and thus Noffi uses WITHOUT-INTERRUPTS on its own.
               ;;
               ;; Hence, we need to wait for input available on the server connection
               ;; on our own here.
               ;;
               (when (zerop (_XPending dpy))
                 #+CCL
                 (do () ((ccl::fd-input-available-p (_ConnectionNumber dpy))))
                 #-CCL
                 (do () ((not (zerop (_XPending dpy)))) (sleep 0.01)))
               ;;
               (_XNextEvent dpy ev)
               (let ((type (_.type ev)))
                 (cond ((eql _ConfigureNotify type)
                        (setf (values w h)
                              (values (_.xconfigure.width ev)
                                      (_.xconfigure.height ev))))
                       ((eql _Expose type)
                        (repaint)))))
          (_XCloseDisplay dpy))))))
