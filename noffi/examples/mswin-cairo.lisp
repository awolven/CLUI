(in-package :noffi-user)

(noffi-syntax t)

_{#include <windows.h>}
_{#include <cairo/cairo.h>}
_{#include <cairo/cairo-win32.h>}

;; This is based on ccl:exampls;mswin.lisp

;;;
;;;   Copyright (C) 2008, Clozure Associates and contributors
;;;   This file is part of OpenMCL.
;;;
;;;   OpenMCL is licensed under the terms of the Lisp Lesser GNU Public
;;;   License , known as the LLGPL and distributed with OpenMCL as the
;;;   file "LICENSE".  The LLGPL consists of a preamble and the LGPL,
;;;   which is distributed with OpenMCL as the file "LGPL".  Where these
;;;   conflict, the preamble takes precedence.        
;;;
;;;   OpenMCL is referenced in the preamble as the "LIBRARY."
;;;
;;;   The LLGPL is also available online at
;;;   http://opensource.franz.com/preamble.html

;;; This is very preliminary and very much a work-in-progress.

;;; This is a simple demo that creates an almost totally uninteresting
;;; window and does limited event handling.  It's intended to excercise
;;; Clozure CL's FFI a little and to serve as a proof of the concept
;;; that Windows GUI programming is possible.  It's not necessarily
;;; a good way to create or manage a window or a good way to structure
;;; a program or much of anything else ...

(defmacro check-if-not (pred form)
  (let ((g (gensym)) (err (gensym)))
    `(let ((,g ,form))
       (when (funcall ,pred ,g)
         (let ((,err (_GetLastError)))
           (error "Windows Error: ~A~%While performing ~S"
                  (windows-error-string ,err)
                  ',form)))
       ,g)))

(defmacro check-non-zero (form)
  `(check-if-not #'zerop ,form))

(defmacro check-not-null (form)
  `(check-if-not #'null ,form))

(defun windows-error-string (error-code)
  (clet ((buf _<LPWSTR>))
    (cond ((and (not (zerop (_FormatMessage (logior _FORMAT_MESSAGE_ALLOCATE_BUFFER
                                                    _FORMAT_MESSAGE_FROM_SYSTEM
                                                    _FORMAT_MESSAGE_IGNORE_INSERTS
                                                    _FORMAT_MESSAGE_MAX_WIDTH_MASK)
                                            nil (abs error-code) 0
                                            (c-addr-of buf)
                                            0 nil)))
                buf)
           (prog1
               (get-native-utf16-string buf)
             (_LocalFree buf)))
          (t
           (format nil "Error ~D" (abs error-code))))))

(defcfun (simple-wndproc _<LRESULT>) ((hwnd _<HWND>)
                                      (msg _<UINT>)
                                      (wparam _<WPARAM>)
                                      (lparam _<LPARAM>))
  (cond ((eql msg _WM_DESTROY)
         (_PostQuitMessage 0)          ; exit status 0: all is well.
         0)                             ; and we'll return 0
        ((eql msg _WM_PAINT)
         (repaint hwnd)
         0)
        (t
         (_DefWindowProc hwnd msg wparam lparam))))

(defun rect-bounding-rectangle* (r)
  (values (_.left r) (_.top r) (_.right r) (_.bottom r)))

(defun window-bounding-rectangle* (hwnd)
  (clet& ((r _<RECT>))
    (check-non-zero (_GetClientRect hwnd r))
    (rect-bounding-rectangle* r)))

;;; Register a named window class. ("class" in this sense has nothing to
;;; do with CLOS or any other object system: windows of the same class
;;; share a common window procedure callback and other attributes, which
;;; we define here.)
;;; If the registration attempt is succesful, it'll return an "atom"
;;; (a small integer that identifies the registered class); otherwise,
;;;  it returns 0.
(defvar *simple-window-class-atom* nil)

(defun register-simple-window-class (class-name)
  ;; We'll use an ANSI version that accepts a simple C string as the
  ;; class name.
  (or *simple-window-class-atom*
      (setq *simple-window-class-atom*
            (clet& ((wc _<WNDCLASSEX>))
              (setf (_.cbSize wc)               _(sizeof(WNDCLASSEX))
                    (_.style wc)                (logior _CS_HREDRAW _CS_VREDRAW _CS_OWNDC)
                    (_.lpfnWndProc wc)          simple-wndproc
                    (_.cbClsExtra wc)           0
                    (_.cbWndExtra wc)           0
                    (_.hInstance wc)            (_GetModuleHandle nil)
                    (_.hIcon wc)                nil
                    (_.hCursor wc)              (_LoadCursor nil _IDC_ARROW)
                    (_.hbrBackground wc)        (_GetStockObject _BLACK_BRUSH)
                    (_.lpszMenuName wc)         nil
                    (_.lpszClassName wc)        class-name
                    (_.hIconSm wc)              nil)
              (check-non-zero (_RegisterClassEx wc))))))

;;; Main function: register a window class, make an instance of that
;;; class, handle events for that window until it's closed.
(defun make-simple-ms-window ()
  (let* ((class-atom (register-simple-window-class "very-simple2")))
    (let* ((hwnd (check-not-null
                  (_CreateWindowEx 0    ;extended style
                                   (int-ptr class-atom)
                                   (format nil "Look! A window! ~A" (code-char #x1F600))
                                   (logior _WS_EX_COMPOSITED _WS_OVERLAPPEDWINDOW) ; style
                                   _CW_USEDEFAULT  ; x pos
                                   _CW_USEDEFAULT  ; y pos
                                   600             ; width
                                   400             ; height
                                   nil             ;parent window
                                   nil             ; menu handle
                                   (_GetModuleHandle nil) ; us
                                   nil)))) ;info for MDI parents/children
      ;; Depending on how the lisp process was created, the first call
      ;; to _ShowWindow in that process might ignore its argument
      ;; (and instead use an argument specified in the STARTUPINFO
      ;; structure passed to _CreateProcess.)  SLIME under FSF Emacs
      ;; runs the lisp with this flag set, and it's possible to waste
      ;; a week or two trying to track this down.  (Trust me.)
      (_ShowWindow hwnd _SW_SHOW)
      ;; In case the parent process said to ignore _ShowWindow's argument
      ;; the first time it's called, call _ShowWindow again.  This seems
      ;; to be harmless, if a little strange ...
      (_ShowWindow hwnd _SW_SHOW)
      (_UpdateWindow hwnd)
      (clet& ((msg _<MSG>))
        (loop for ret = (check-if-not #'minusp (_GetMessage msg nil 0 0))
              do (cond ((= 0 ret)
                        (return (_.wParam msg)))
                       (t
                        (_TranslateMessage msg)
                        (_DispatchMessage msg))))))))

(defun repaint (hwnd)
  (clet& ((ps _<PAINTSTRUCT>))
    (let ((hdc (_BeginPaint hwnd ps)))
      (multiple-value-bind (cx1 cy1 cx2 cy2) (rect-bounding-rectangle* (_.rcPaint ps))
        (let* ((surface (_cairo_win32_surface_create hdc))
               (cr (_cairo_create surface)))
          (_cairo_set_line_width cr 1)
          (_cairo_set_source_rgba cr 1 1 1 1)
          (multiple-value-bind (x1 y1 x2 y2) (window-bounding-rectangle* hwnd)
            (multiple-value-bind (w h) (values (- x2 x1) (- y2 y1))
              (loop for i from 1 to 19
                    do (loop for j from 1 to 19
                             do (draw-line cr
                                           (* w (/ i 20)) (* 1/10 h)
                                           (* w (/ j 20)) (* 9/10 h))))))
          (_cairo_destroy cr)
          (_cairo_surface_destroy surface))))
    (_EndPaint hwnd ps)))

(defun draw-line (cr x1 y1 x2 y2)
  (_cairo_move_to cr x1 y1)
  (_cairo_line_to cr x2 y2)
  (_cairo_stroke cr)
  (values))

(defun cl-user::run ()
  (use-library "gdi32")
  (use-library "user32")
  (use-library "c:/msys64/mingw64/bin/libcairo-2.dll")
  (make-simple-ms-window))

