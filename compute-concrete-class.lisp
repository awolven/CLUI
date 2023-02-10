(in-package :clui)

(defmethod compute-concrete-class ((protocol display)
				   &rest initargs &key (x11 nil) (wayland nil)
						    #+darwin (metal nil)
						    (vulkan nil) (opengl nil)
				   &allow-other-keys)
  (declare (ignorable wayland initargs))
  (cond (x11 (find-class 'x11:local-server))
	(t #+windows (cond (vulkan (find-class 'win32:desktop-with-vulkan))
			   (opengl (find-class 'win32:desktop-with-opengl))
			   (t (find-class 'win32:desktop)))
	   #+darwin (cond ((or vulkan metal) (find-class 'cocoa:desktop-with-metal))
			  (opengl (find-class 'cocoa:desktop-with-opengl))
			  (t (find-class 'cocoa:desktop)))
	   #+linux (cond (wayland (cond (vulkan (find-class 'wayland:desktop-with-vulkan))
					(opengl (find-class 'wayland:desktop-with-opengl))
					(t (find-class 'wayland:desktop))))
			 (t (find-class 'x11::local-server))))))

(defmethod compute-concrete-class ((protocol display-dependent)
				   &rest initargs &key (display (default-display))  &allow-other-keys)
  (apply #'compute-concrete-class-with-display protocol display initargs))

#+windows
(defmethod compute-concrete-class-with-display ((protocol monitor) (display win32:desktop-mixin)
						&rest initargs
						&key
						&allow-other-keys)
  (declare (ignore initargs))
  (find-class 'win32:monitor))

#+darwin
(defmethod compute-concrete-class-with-display ((protocol monitor) (display cocoa:desktop-mixin)
						&rest initargs
						&key
						&allow-other-keys)
  (declare (ignore initargs))
  (find-class 'cocoa:monitor))


(defmethod compute-concrete-class-with-display ((protocol monitor) (display x11:server-mixin)
						&rest initargs
						&key
						&allow-other-keys)
  (declare (ignore initargs))
  (find-class 'x11:monitor))

#+linux
(defmethod compute-concrete-class-with-display ((protocol monitor) (display wayland:desktop-mixin)
						&rest initargs
						&key
						&allow-other-keys)
  (declare (ignore initargs))
  (find-class 'wayland:monitor))

(defmethod compute-concrete-class-with-display ((protocol window) display &rest initargs &key &allow-other-keys)
  (apply #'compute-concrete-window-class display (getf initargs :parent) initargs))

#+windows
(defmethod get-a-win32-window-class (display errorp &rest initargs &key &allow-other-keys)
  (declare (ignore display initargs))
  (find-class 'win32:window errorp))

#+windows
(defmethod get-a-win32-window-class ((display clui:vulkan-support-mixin) errorp &rest initargs &key (animable nil) &allow-other-keys)
  (declare (ignore initargs))
  (if animable
      (find-class 'win32:vulkan-window errorp)
      (find-class 'win32:window errorp)))

#+windows
(defmethod get-a-win32-window-class ((display clui:opengl-support-mixin) errorp &rest initargs &key (animable nil) &allow-other-keys)
  (declare (ignore initargs))
  (if animable
      (find-class 'win32:wgl-window errorp)
      (find-class 'win32:window errorp)))

#+darwin
(defmethod get-a-cocoa-window-class (display errorp &rest initargs &key &allow-other-keys)
  (declare (ignore display initargs))
  (find-class 'cocoa:window errorp))

#+darwin
(defmethod get-a-cocoa-window-class ((display clui:vulkan-support-mixin) errorp &rest initargs &key (animable nil) &allow-other-keys)
  (declare (ignore initargs))
  (if animable
      (or (find-class 'cocoa:vulkan-window nil)
	  (find-class 'cocoa:metal-window errorp))
      (find-class 'cocoa:window errorp)))

#+darwin
(defmethod get-a-cocoa-window-class ((display clui:opengl-support-mixin) errorp &rest initargs &key (animable nil) &allow-other-keys)
  (declare (ignore initargs))
  (if animable
      (find-class 'cocoa:nsgl-window errorp)
      (find-class 'cocoa:window errorp)))

#+linux
(defmethod get-a-wayland-window-class (display errorp &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (find-class 'wayland:window errorp))

#+linux
(defmethod get-a-wayland-window-class ((display clui:vulkan-support-mixin) errorp &rest initargs &key (animable nil) &allow-other-keys)
  (declare (ignore initargs))
  (if animable
      (find-class 'wayland:vulkan-window errorp)
      (find-class 'wayland:window errorp)))

#+linux
(defmethod get-a-wayland-window-class ((display clui:opengl-support-mixin) errorp &rest initargs &key (animable nil) &allow-other-keys)
  (declare (ignore initargs))
  (if animable
      (find-class 'wayland:opengl-window errorp)
      (find-class 'wayland:window errorp)))

#+unix
(defmethod get-an-x11-window-class (display errorp &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (find-class 'x11:window errorp))

#+unix
(defmethod get-an-x11-window-class ((display clui:vulkan-support-mixin) errorp &rest initargs &key (animable nil) &allow-other-keys)
  (declare (ignore initargs))
  (if animable
      (find-class 'x11:vulkan-window errorp)
      (find-class 'x11:window errorp)))

#+unix
(defmethod get-an-x11-window-class ((desktop clui:opengl-support-mixin) errorp &rest initargs &key (animable nil) &allow-other-keys)
  (declare (ignore initargs))
  (if animable
      (find-class 'x11:glx-window errorp)
      (find-class 'x11:window errorp)))

;; if a null passed in as parent, or if we get a screen object
;; then it is a top level window, which is an os-window on all platorms
;; the only question is, do we need a vulkan or opengl window, check for :animable on initargs

#+windows
(defmethod compute-concrete-window-class (desktop (parent win32:screen-mixin) &rest initargs)
  (apply #'get-a-win32-window-class desktop t initargs))

#+darwin
(defmethod compute-concrete-window-class (desktop (parent cocoa:screen-mixin) &rest initargs)
  (apply #'get-a-cocoa-window-class desktop t initargs))

#+linux
(defmethod compute-concrete-window-class (desktop (parent wayland:screen-mixin) &rest initargs)
  (declare (ignore desktop))
  (apply #'get-a-wayland-window-class desktop t initargs))

#+unix
(defmethod compute-concrete-window-class (display (parent x11:screen-mixin) &rest initargs)
  (apply #'get-an-x11-window-class display t initargs))

#+windows
(defmethod compute-concrete-window-class ((desktop win32:desktop-mixin) (parent null) &rest initargs)
  (apply #'get-a-win32-window-class desktop t initargs))

#+darwin
(defmethod compute-concrete-window-class ((desktop cocoa:desktop-mixin) (parent null) &rest initargs)
  (apply #'get-a-cocoa-window-class desktop t initargs))

#+linux
(defmethod compute-concrete-window-class ((desktop wayland:desktop-mixin) (parent null) &rest initargs)
  (apply #'get-a-wayland-window-class desktop t initargs))

#+unix
(defmethod compute-concrete-window-class ((display x11:server-mixin) (parent null) &rest initargs)
  ;; todo: maybe do somthing different in case of remote-server instead of local-server
  (apply #'get-an-x11-window-class display t initargs))

;; if it is not a toplevel window then:
;; on windows it is an os-window with a parent (this may be called a view for abstraction sake)
;; on macos it is a NSView (os-window)
;; on x11 it is an x11 window (which we may also call a "view")
;; with vulkan or opengl, there needs to be some kind of flag indicating
;; whether the user desires an os-window
;; if not desiring an os-window, and the window is contained within the bounds of the parent
;; lisp needs to generate a window looking thing with graphics and display in the parent's framebuffer
;; this way the user can definitely get transparent windows on all platforms
;; if desiring an os-window, ms-windows, macos, and x11 can all accomodate
;; in wayland, if the user doesn't care that the window doesn't actually have the parent as parent
;; we can create another os-window, otherwise we have to use some graphics library to paint a window
;; on the parent

;; maybe we should call a window painted by us a "homemade-window"
;; if the homemade window gets dragged outside the bounds of the parent
;; we then need to generate an os-window
;; we should be able to generate change-class machinery which would be as efficient as creating a new
;; os window from scratch, so that the window retains identity on the lisp side

;; if the parent is animable: (continuous refresh)
;; clui:homemade-animable-window-mixin
;; clui:homemade-vulkan-window
;; clui:homemade-opengl-window
;; clui:homemade-metal-window (only spawned on macos platform)

;; if the parent is non-animable: (demand refresh)
;; clui:homemade-window-mixin
;; clui:homemade-gdi-window (only spawned on ms windows)
;; clue:{vulkan|opengl|metal}-window (in a demand refresh environment)
;; or any other homemade window with any graphics library supported

;;                     child:     win32-window     cocoa-window         gtk-window         x11-window                        wayland-window               homemade-window
;; parent
;; win32-window animable:         animable-or-non   N/A                 N/A                     N/A                          N/A                          inherit (animable)
;; win32-window non-animable:     animable-or-non   N/A                 N/A                     N/A                          N/A                          inherit (non-animable)
;; cocoa-window animable:         N/A               animable-only       N/A                     N/A                          N/A                          inherit (animable)
;; cocoa-window non-animable:     N/A               non-animable-only   N/A                     N/A                          N/A                          inherit (non-animable)
;; gtk-window animable:           N/A               animable-only       N/A                     N/A                          N/A                          inherit (animable)
;; gtk-window non-animable:       N/A               non-animable-only   non-animable-only       N/A                          N/A                          inherit (non-animable)
;; x11-window animable:           N/A               N/A                 N/A                     animable-or-non              might-work-animable-or-non   inherit (animable)
;; x11-window non-animable:       N/A               N/A                 N/A                     animable-or-non              might-work-animable-or-non   inherit (non-animable)
;; wayland-window animable:       N/A               N/A                 N/A                     might-work-animable-or-non   animable-or-non              inherit (animable)
;; wayland-window non-animable:   N/A               N/A                 N/A                     might-work-animable-or-non   animable-or-non              inherit (non-animable)
;; homemade-window animable:      N/A               N/A                 N/A                     N/A                          N/A                          inherit (animable)
;; homemade-window non-animable:  N/A               N/A                 N/A                     N/A                          N/A                          inherit (non-animable)

;; for mixing wayland and x11, obviously only works if display (desktop) is wayland
;; for gtk, child and parent probably must be gtk, on any platform, and afaik is not animable

#+windows
(defmethod compute-concrete-window-class (display (parent win32:window-mixin) &rest initargs)
  (declare (ignorable display))
  (apply #'get-a-win32-window-class display t initargs))

#+darwin
(defmethod compute-concrete-window-class (display (parent cocoa:window-mixin) &rest initargs)
  (declare (ignorable display initargs))
  (find-class 'cocoa:view))

#+darwin
(defmethod compute-concrete-window-class (display (parent cocoa:vulkan-window-mixin) &rest initargs)
  (declare (ignorable display initargs))
  (find-class 'cocoa:vulkan-view))

#+darwin
(defmethod compute-concrete-window-class (display (parent cocoa:metal-window-mixin) &rest initargs)
  (declare (ignorable display initargs))
  (find-class 'cocoa:metal-view))

#+darwin
(defmethod compute-concrete-window-class (display (parent cocoa:nsgl-window-mixin) &rest initargs)
  (declare (ignorable display initargs))
  (find-class 'cocoa:nsgl-view))

#+unix
(defmethod compute-concrete-window-class ((display x11:server-mixin) (parent x11:window-mixin) &rest initargs)
  (apply #'get-an-x11-window-class t initargs))

#+linux
(defmethod compute-concrete-window-class ((display wayland:desktop-mixin) (parent x11:window-mixin) &rest initargs)
  (or (apply #'get-an-x11-window-class display nil initargs)
      (apply #'get-a-wayland-window-class display t initargs)))

#+linux
(defmethod compute-concrete-window-class (display (parent wayland:window-mixin) &rest initargs)
  (declare (ignorable display))
  (or (apply #'get-a-wayland-window-class display nil initargs)
      (apply #'get-an-x11-window-class display t initargs)))

(defmethod compute-concrete-window-class (display (parent clui:homemade-window-mixin) &rest initargs)
  (declare (ignore display initargs))
  (class-of parent))


(defmethod compute-concrete-class-with-display ((protocol timeout-event) display &rest initargs &key &allow-other-keys)
  (declare (ignore display initargs))
  (find-class 'clui.v0:timeout-event))

(defmethod compute-concrete-class-with-display ((protocol window-move-event) display &rest initargs &key &allow-other-keys)
  (declare (ignore display initargs))
  (find-class 'clui.v0:window-move-event))

(defmethod compute-concrete-class-with-display ((protocol window-resize-event) display &rest initargs &key &allow-other-keys)
  (declare (ignore display initargs))
  (find-class 'clui.v0:window-resize-event))

(defmethod compute-concrete-class-with-display ((protocol window-iconify-event) display &rest initargs &key &allow-other-keys)
  (declare (ignore display initargs))
  (find-class 'clui.v0:window-iconify-event))

(defmethod compute-concrete-class-with-display ((protocol window-deiconify-event) display &rest initargs &key &allow-other-keys)
  (declare (ignore display initargs))
  (find-class 'clui.v0:window-deiconify-event))

(defmethod compute-concrete-class-with-display ((protocol window-maximize-event) display &rest initargs &key &allow-other-keys)
  (declare (ignore display initargs))
  (find-class 'clui.v0:window-maximize-event))

(defmethod compute-concrete-class-with-display ((protocol window-restore-event) display &rest initargs &key &allow-other-keys)
  (declare (ignore display initargs))
  (find-class 'clui.v0:window-restore-event))

(defmethod compute-concrete-class-with-display ((protocol window-fullscreen-event) display &rest initargs &key &allow-other-keys)
  (declare (ignore display initargs))
  (find-class 'clui.v0:window-fullscreen-event))

(defmethod compute-concrete-class-with-display ((protocol window-show-event) display &rest initargs &key &allow-other-keys)
  (declare (ignore display initargs))
  (find-class 'clui.v0:window-show-event))

(defmethod compute-concrete-class-with-display ((protocol window-focus-event) display &rest initargs &key &allow-other-keys)
  (declare (ignore display initargs))
  (find-class 'clui.v0:window-focus-event))

(defmethod compute-concrete-class-with-display ((protocol window-defocus-event) display &rest initargs &key &allow-other-keys)
  (declare (ignore display initargs))
  (find-class 'clui.v0:window-defocus-event))

(defmethod compute-concrete-class-with-display ((protocol window-hide-event) display &rest initargs &key &allow-other-keys)
  (declare (ignore display initargs))
  (find-class 'clui.v0:window-hide-event))

(defmethod compute-concrete-class-with-display ((protocol window-repaint-event) display &rest initargs &key &allow-other-keys)
  (declare (ignore display initargs))
  (find-class 'clui.v0:window-repaint-event))

(defmethod compute-concrete-class-with-display ((protocol window-created-event) display &rest initargs &key &allow-other-keys)
  (declare (ignore display initargs))
  (find-class 'clui.v0:window-created-event))

(defmethod compute-concrete-class-with-display ((protocol window-close-event) display &rest initargs &key &allow-other-keys)
  (declare (ignore display initargs))
  (find-class 'clui.v0:window-close-event))

(defmethod compute-concrete-class-with-display ((protocol window-destroyed-event) display &rest initargs &key &allow-other-keys)
  (declare (ignore display initargs))
  (find-class 'clui.v0:window-destroyed-event))

(defmethod compute-concrete-class-with-display ((protocol window-monitor-switched-event) display &rest initargs &key &allow-other-keys)
  (declare (ignore display initargs))
  (find-class 'clui.v0:window-monitor-switched-event))

(defmethod compute-concrete-class-with-display ((protocol pointer-button-press-event) display &rest initargs &key &allow-other-keys)
  (declare (ignore display initargs))
  (find-class 'clui.v0:pointer-button-press-event))

(defmethod compute-concrete-class-with-display ((protocol pointer-button-release-event) display &rest initargs &key &allow-other-keys)
  (declare (ignore display initargs))
  (find-class 'clui.v0:pointer-button-release-event))

(defmethod compute-concrete-class-with-display ((protocol pointer-button-hold-event) display &rest initargs &key &allow-other-keys)
  (declare (ignore display initargs))
  (find-class 'clui.v0:pointer-button-hold-event))

(defmethod compute-concrete-class-with-display ((protocol pointer-button-hold-and-drag-event) display &rest initargs &key &allow-other-keys)
  (declare (ignore display initargs))
  (find-class 'clui.v0:pointer-button-hold-and-drag-event))

(defmethod compute-concrete-class-with-display ((protocol pointer-wheel-event) display &rest initargs &key &allow-other-keys)
  (declare (ignore display initargs))
  (find-class 'clui.v0:pointer-wheel-event))

(defmethod compute-concrete-class-with-display ((protocol pointer-motion-event) display &rest initargs &key &allow-other-keys)
  (declare (ignore display initargs))
  (find-class 'clui.v0:pointer-motion-event))

(defmethod compute-concrete-class-with-display ((protocol pointer-enter-event) display &rest initargs &key &allow-other-keys)
  (declare (ignore display initargs))
  (find-class 'clui.v0:pointer-enter-event))

(defmethod compute-concrete-class-with-display ((protocol pointer-exit-event) display &rest initargs &key &allow-other-keys)
  (declare (ignore display initargs))
  (find-class 'clui.v0:pointer-exit-event))

