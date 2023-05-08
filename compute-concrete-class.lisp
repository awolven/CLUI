(in-package :clui)

(defmethod compute-make-instance-arguments ((protocol display)
					    &rest initargs &key
							     (cocoa nil)
							     (metal nil)
							     (opengl nil)
							     (vulkan nil)
							     (wayland nil)
							     (win32 nil)
							     (x11 nil)
					    &allow-other-keys)

  (apply #'compute-make-display-instance-arguments protocol
	 cocoa
	 metal
	 opengl
	 vulkan
	 wayland
	 win32
	 x11
	 initargs))

(defgeneric compute-make-display-instance-arguments (protocol
						     cocoa 
						     metal 
						     opengl 
						     vulkan 
						     wayland 
						     win32 
						     x11 
						     &rest initargs
						     &key &allow-other-keys))

#+win32
(defmethod compute-make-display-instance-arguments ((protocol display)
						    (cocoa null)
						    (metal null)
						    (opengl null)
						    (vulkan null)
						    (wayland null)
						    (win32 t)
						    (x11 null)
						    &rest initargs
						    &key &allow-other-keys)
  (list* (find-class 'win32:desktop) initargs))

#+cocoa
(defmethod compute-make-display-instance-arguments (protocol
						    (cocoa t)
						    (metal null)
						    (opengl null)
						    (vulkan null)
						    (wayland null)
						    (win32 null)
						    (x11 null)
						    &rest initargs
						    &key &allow-other-keys)
  (declare (ignore protocol))
  (list* (find-class 'cocoa:desktop) initargs))


#+(and cocoa x11)
(defmethod compute-make-display-instance-arguments (protocol
						    (cocoa t)
						    (metal null)
						    (opengl null)
						    (vulkan null)
						    (wayland null)
						    (win32 null)
						    (x11 t)
						    &rest initargs
						    &key &allow-other-keys)
  (declare (ignore protocol))
  (list* (find-class 'cocoa:desktop) initargs))

#+x11
(defmethod compute-make-display-instance-arguments (protocol
						    (cocoa null)
						    (metal null)
						    (opengl null)
						    (vulkan null)
						    (wayland null)
						    (win32 null)
						    (x11 t)
						    &rest initargs
						    &key &allow-other-keys)
  (declare (ignore protocol))
  (list* (find-class 'x11:local-server) initargs))

#+x11
(defmethod compute-make-display-instance-arguments (protocol
						    (cocoa null)
						    (metal null)
						    (opengl null)
						    (vulkan t)
						    (wayland null)
						    (win32 null)
						    (x11 t)
						    &rest initargs
						    &key &allow-other-keys)
  (declare (ignorable protocol))
  (list* (find-class 'x11:local-server-with-krma) initargs))

#+wayland
(defmethod compute-make-display-instance-arugments (protocol
						    (cocoa t)
						    (metal null)
						    (opengl null)
						    (vulkan null)
						    (wayland null)
						    (win32 null)
						    (x11 null)
						    &rest initargs)

  (list* (find-class 'wayland:desktop) initargs))
  

(defmethod compute-make-instance-arguments ((protocol display-dependent)
					    &rest initargs &key (display (default-display))  &allow-other-keys)
  (apply #'compute-make-instance-arguments-with-display protocol display initargs))

(defmethod compute-make-instance-arguments ((protocol window)
					    &rest initargs
					    &key
					      (parent (default-screen (default-display)))
					      (root (window-root parent))
					      (display (window-display root))
					    &allow-other-keys)
  (setq initargs (copy-list initargs))
  (remf initargs :display)
  (remf initargs :root)
  (remf initargs :parent)
  (apply #'compute-make-instance-arguments-with-display protocol display
	 :display display :root root :parent parent initargs))

#+win32
(defmethod compute-make-instance-arguments-with-display ((protocol monitor) (display win32:desktop-mixin)
							 &rest initargs
							 &key
							 &allow-other-keys)
  (list* (find-class 'win32:monitor) initargs))

#+win32
(defmethod compute-make-instance-arguments-with-display ((protocol screen) (display win32:desktop-mixin)
							 &rest initargs
							 &key
							 &allow-other-keys)
  (list* (find-class 'win32:screen) initargs))

#+cocoa
(defmethod compute-make-instance-arguments-with-display ((protocol monitor) (display cocoa:desktop-mixin)
							 &rest initargs
							 &key
							 &allow-other-keys)
  (list* (find-class 'cocoa:monitor) initargs))

#+cocoa
(defmethod compute-make-instance-arguments-with-display ((protocol screen) (display cocoa:desktop-mixin)
							 &rest initargs
							 &key
							 &allow-other-keys)
  (list* (find-class 'cocoa:screen) initargs))

#+x11
(defmethod compute-make-instance-arguments-with-display ((protocol screen) (display x11:server-mixin)
						&rest initargs
						&key
						&allow-other-keys)
  (list* (find-class 'x11:screen) initargs))

#+x11
(defmethod compute-make-instance-arguments-with-display ((protocol cursor) (display x11:server-mixin)
							 &rest initargs
							 &key
							 &allow-other-keys)
  (list* (find-class 'x11:cursor) initargs))

#+x11
(defmethod compute-make-instance-arguments-with-display ((protocol monitor) (display x11:server-mixin)
						&rest initargs
						&key
						&allow-other-keys)
  (list* (find-class 'x11:monitor) initargs))

#+wayland
(defmethod compute-make-instance-arguments-with-display ((protocol monitor) (display wayland:desktop-mixin)
						&rest initargs
						&key
						&allow-other-keys)
  (list* (find-class 'wayland:monitor) initargs))

(defmethod compute-make-instance-arguments-with-display ((protocol window) display &rest initargs &key &allow-other-keys)
  (list* (apply #'compute-concrete-window-class display (getf initargs :parent) initargs) initargs))

#+win32
(defmethod get-a-win32-window-class (display errorp &rest initargs &key &allow-other-keys)
  (declare (ignore display initargs))
  (find-class 'win32:window errorp))

#+nil
(defmethod get-a-win32-window-class ((display clui:opengl-support-mixin) errorp &rest initargs &key (animable nil) &allow-other-keys)
  (declare (ignore initargs))
  (if animable
      (find-class 'win32:wgl-enabled-window errorp)
      (find-class 'win32:window errorp)))

#+cocoa
(defmethod get-a-cocoa-window-class (display errorp &rest initargs &key &allow-other-keys)
  (declare (ignore display initargs))
  (find-class 'cocoa:window errorp))

#+wayland
(defmethod get-a-wayland-window-class (display errorp &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (find-class 'wayland:window errorp))

#+wayland
(defmethod get-a-wayland-window-class ((display clui:vulkan-support-mixin) errorp &rest initargs &key (animable nil) &allow-other-keys)
  (declare (ignore initargs))
  (if animable
      (find-class 'wayland:vulkan-window errorp)
      (find-class 'wayland:window errorp)))

#+wayland
(defmethod get-a-wayland-window-class ((display clui:opengl-support-mixin) errorp &rest initargs &key (animable nil) &allow-other-keys)
  (declare (ignore initargs))
  (if animable
      (find-class 'wayland:opengl-window errorp)
      (find-class 'wayland:window errorp)))

#+x11
(defmethod get-an-x11-window-class (display errorp &rest initargs &key &allow-other-keys)
  (declare (ignore display initargs))
  (find-class 'x11:window errorp))



;; if a null passed in as parent, or if we get a screen object
;; then it is a top level window, which is an os-window on all platorms
;; the only question is, do we need a vulkan or opengl window, check for :animable on initargs

#+win32
(defmethod compute-concrete-window-class (desktop (parent win32:screen-mixin) &rest initargs &key &allow-other-keys)
  (apply #'get-a-win32-window-class desktop t initargs))

#+cocoa
(defmethod compute-concrete-window-class (desktop (parent cocoa:screen-mixin) &rest initargs &key &allow-other-keys)
  (apply #'get-a-cocoa-window-class desktop t initargs))

#+wayland
(defmethod compute-concrete-window-class (desktop (parent wayland:screen-mixin) &rest initargs &key &allow-other-keys)
  (declare (ignore desktop))
  (apply #'get-a-wayland-window-class desktop t initargs))

#+x11
(defmethod compute-concrete-window-class (display (parent x11:screen-mixin) &rest initargs &key &allow-other-keys)
  (apply #'get-an-x11-window-class display t initargs))

#+win32
(defmethod compute-concrete-window-class ((desktop win32:desktop-mixin) (parent null) &rest initargs &key &allow-other-keys)
  (apply #'get-a-win32-window-class desktop t initargs))

#+cocoa
(defmethod compute-concrete-window-class ((desktop cocoa:desktop-mixin) (parent null) &rest initargs &key &allow-other-keys)
  (apply #'get-a-cocoa-window-class desktop t initargs))

#+wayland
(defmethod compute-concrete-window-class ((desktop wayland:desktop-mixin) (parent null) &rest initargs &key &allow-other-keys)
  (apply #'get-a-wayland-window-class desktop t initargs))

#+x11
(defmethod compute-concrete-window-class ((display x11:server-mixin) (parent null) &rest initargs &key &allow-other-keys)
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

#+win32
(defmethod compute-concrete-window-class (display (parent win32:window-mixin) &rest initargs)
  (declare (ignorable display))
  (apply #'get-a-win32-window-class display t initargs))

#+cocoa
(defmethod compute-concrete-window-class (display (parent cocoa:window-mixin) &rest initargs)
  (declare (ignorable display initargs))
  (find-class 'cocoa:view))

#+cocoa
(defmethod compute-concrete-window-class (display (parent cocoa:vulkan-window-mixin) &rest initargs)
  (declare (ignorable display initargs))
  (find-class 'cocoa:vulkan-view))

#+cocoa
(defmethod compute-concrete-window-class (display (parent cocoa:metal-window-mixin) &rest initargs)
  (declare (ignorable display initargs))
  (find-class 'cocoa:metal-view))

#+cocoa
(defmethod compute-concrete-window-class (display (parent cocoa:nsgl-window-mixin) &rest initargs)
  (declare (ignorable display initargs))
  (find-class 'cocoa:nsgl-view))

#+x11
(defmethod compute-concrete-window-class ((display x11:server-mixin) (parent x11:window-mixin) &rest initargs &key &allow-other-keys)
  (apply #'get-an-x11-window-class display t initargs))

#+(and x11 wayland)
(defmethod compute-concrete-window-class ((display wayland:desktop-mixin) (parent x11:window-mixin) &rest initargs)
  (or (apply #'get-an-x11-window-class display nil initargs)
      (apply #'get-a-wayland-window-class display t initargs)))

#+(and x11 wayland)
(defmethod compute-concrete-window-class (display (parent wayland:window-mixin) &rest initargs)
  (declare (ignorable display))
  (or (apply #'get-a-wayland-window-class display nil initargs)
      (apply #'get-an-x11-window-class display t initargs)))

#+NIL
(defmethod compute-concrete-window-class (display (parent clui:homemade-window-mixin) &rest initargs)
  (declare (ignore display initargs))
  (class-of parent))


(defmethod compute-make-instance-arguments-with-display ((protocol timeout-event) display &rest initargs &key &allow-other-keys)
  (declare (ignore display))
  (list* (find-class 'clui.v0:timeout-event) initargs))

(defmethod compute-make-instance-arguments-with-display ((protocol window-move-event) display &rest initargs &key &allow-other-keys)
  (declare (ignore display))
  (list* (find-class 'clui.v0:window-move-event) initargs))

(defmethod compute-make-instance-arguments-with-display ((protocol window-resize-event) display &rest initargs &key &allow-other-keys)
  (declare (ignore display))
  (list* (find-class 'clui.v0:window-resize-event) initargs))

(defmethod compute-make-instance-arguments-with-display ((protocol window-iconify-event) display &rest initargs &key &allow-other-keys)
  (declare (ignore display))
  (list* (find-class 'clui.v0:window-iconify-event) initargs))

(defmethod compute-make-instance-arguments-with-display ((protocol window-deiconify-event) display &rest initargs &key &allow-other-keys)
  (declare (ignore display))
  (list* (find-class 'clui.v0:window-deiconify-event) initargs))

(defmethod compute-make-instance-arguments-with-display ((protocol window-maximize-event) display &rest initargs &key &allow-other-keys)
  (declare (ignore display))
  (list* (find-class 'clui.v0:window-maximize-event) initargs))

(defmethod compute-make-instance-arguments-with-display ((protocol window-restore-event) display &rest initargs &key &allow-other-keys)
  (declare (ignore display))
  (list* (find-class 'clui.v0:window-restore-event) initargs))

(defmethod compute-make-instance-arguments-with-display ((protocol window-fullscreen-event) display &rest initargs &key &allow-other-keys)
  (declare (ignore display))
  (list* (find-class 'clui.v0:window-fullscreen-event) initargs))

(defmethod compute-make-instance-arguments-with-display ((protocol window-show-event) display &rest initargs &key &allow-other-keys)
  (declare (ignore display))
  (list* (find-class 'clui.v0:window-show-event) initargs))

(defmethod compute-make-instance-arguments-with-display ((protocol window-focus-event) display &rest initargs &key &allow-other-keys)
  (declare (ignore display))
  (list* (find-class 'clui.v0:window-focus-event) initargs))

(defmethod compute-make-instance-arguments-with-display ((protocol window-defocus-event) display &rest initargs &key &allow-other-keys)
  (declare (ignore display))
  (list* (find-class 'clui.v0:window-defocus-event) initargs))

(defmethod compute-make-instance-arguments-with-display ((protocol window-hide-event) display &rest initargs &key &allow-other-keys)
  (declare (ignore display))
  (list* (find-class 'clui.v0:window-hide-event) initargs))

(defmethod compute-make-instance-arguments-with-display ((protocol window-repaint-event) display &rest initargs &key &allow-other-keys)
  (declare (ignore display))
  (list* (find-class 'clui.v0:window-repaint-event) initargs))

(defmethod compute-make-instance-arguments-with-display ((protocol window-created-event) display &rest initargs &key &allow-other-keys)
  (declare (ignore display))
  (list* (find-class 'clui.v0:window-created-event) initargs))

(defmethod compute-make-instance-arguments-with-display ((protocol window-close-event) display &rest initargs &key &allow-other-keys)
  (declare (ignore display))
  (list* (find-class 'clui.v0:window-close-event) initargs))

(defmethod compute-make-instance-arguments-with-display ((protocol window-destroyed-event) display &rest initargs &key &allow-other-keys)
  (declare (ignore display))
  (list* (find-class 'clui.v0:window-destroyed-event) initargs))

(defmethod compute-make-instance-arguments-with-display ((protocol window-monitor-switched-event) display &rest initargs &key &allow-other-keys)
  (declare (ignore display))
  (list* (find-class 'clui.v0:window-monitor-switched-event) initargs))

(defmethod compute-make-instance-arguments-with-display ((protocol pointer-button-press-event) display &rest initargs &key &allow-other-keys)
  (declare (ignore display))
  (list* (find-class 'clui.v0:pointer-button-press-event) initargs))

(defmethod compute-make-instance-arguments-with-display ((protocol pointer-button-release-event) display &rest initargs &key &allow-other-keys)
  (declare (ignore display))
  (list* (find-class 'clui.v0:pointer-button-release-event) initargs))

(defmethod compute-make-instance-arguments-with-display ((protocol pointer-button-hold-event) display &rest initargs &key &allow-other-keys)
  (declare (ignore display))
  (list* (find-class 'clui.v0:pointer-button-hold-event) initargs))

(defmethod compute-make-instance-arguments-with-display ((protocol pointer-button-hold-and-drag-event) display &rest initargs &key &allow-other-keys)
  (declare (ignore display))
  (list* (find-class 'clui.v0:pointer-button-hold-and-drag-event) initargs))

(defmethod compute-make-instance-arguments-with-display ((protocol pointer-wheel-event) display &rest initargs &key &allow-other-keys)
  (declare (ignore display))
  (list* (find-class 'clui.v0:pointer-wheel-event) initargs))

(defmethod compute-make-instance-arguments-with-display ((protocol pointer-motion-event) display &rest initargs &key &allow-other-keys)
  (declare (ignore display))
  (list* (find-class 'clui.v0:pointer-motion-event) initargs))

(defmethod compute-make-instance-arguments-with-display ((protocol pointer-enter-event) display &rest initargs &key &allow-other-keys)
  (declare (ignore display))
  (list* (find-class 'clui.v0:pointer-enter-event) initargs))

(defmethod compute-make-instance-arguments-with-display ((protocol pointer-exit-event) display &rest initargs &key &allow-other-keys)
  (declare (ignore display))
  (list* (find-class 'clui.v0:pointer-exit-event) initargs))

(defmethod compute-make-instance-arguments-with-display ((protocol key-press-event) display &rest initargs &key &allow-other-keys)
  (declare (ignore display))
  (list* (find-class 'clui.v0:key-press-event) initargs))

(defmethod compute-make-instance-arguments-with-display ((protocol key-repeat-event) display &rest initargs &key &allow-other-keys)
  (declare (ignore display))
  (list* (find-class 'clui.v0::key-repeat-event) initargs))

(defmethod compute-make-instance-arguments-with-display ((protocol key-release-event) display &rest initargs &key &allow-other-keys)
  (declare (ignore display))
  (list* (find-class 'clui.v0:key-release-event) initargs))

(defmethod compute-make-instance-arguments-with-display ((protocol character-event) display &rest initargs &key &allow-other-keys)
  (declare (ignore display))
  (list* (find-class 'clui.v0:character-event) initargs))

#+cocoa
(defmethod compute-make-instance-arguments-with-display ((protocol arrow-cursor) (display cocoa:desktop-mixin) &rest initargs &key &allow-other-keys)
  (remf initargs :display)
  (list* (find-class 'cocoa::arrow-cursor) initargs))

#+win32
(defmethod compute-make-instance-arguments-with-display ((protocol arrow-cursor) (display win32:desktop-mixin) &rest initargs &key &allow-other-keys)
  (remf initargs :display)
  (list* (find-class 'win32::arrow-cursor) initargs))

#+x11
(defmethod compute-make-instance-arguments-with-display ((protocol arrow-cursor) (display x11:server-mixin) &rest initargs &key &allow-other-keys)
  (remf initargs :display)
  (list* (find-class 'x11::arrow-cursor) initargs))

#+wayland
(defmethod compute-make-instance-arguments-with-display ((protocol arrow-cursor) (display wayland:desktop-mixin) &rest initargs &key &allow-other-keys)
  (remf initargs :display)
  (list* (find-class 'wayland::arrow-cursor) initargs))

#+cocoa
(defmethod compute-make-instance-arguments-with-display ((protocol hand-cursor) (display cocoa:desktop-mixin) &rest initargs &key &allow-other-keys)
  (remf initargs :display)
  (list* (find-class 'cocoa::hand-cursor) initargs))

#+win32
(defmethod compute-make-instance-arguments-with-display ((protocol hand-cursor) (display win32:desktop-mixin) &rest initargs &key &allow-other-keys)
  (remf initargs :display)
  (list* (find-class 'win32::hand-cursor) initargs))

#+x11
(defmethod compute-make-instance-arguments-with-display ((protocol hand-cursor) (display x11:server-mixin) &rest initargs &key &allow-other-keys)
  (remf initargs :display)
  (list* (find-class 'x11::hand-cursor) initargs))

#+wayland
(defmethod compute-make-instance-arguments-with-display ((protocol hand-cursor) (display wayland:desktop-mixin) &rest initargs &key &allow-other-keys)
  (remf initargs :display)
  (list* (find-class 'wayland::hand-cursor) initargs))

#+cocoa
(defmethod compute-make-instance-arguments-with-display ((protocol pointing-hand-cursor) (display cocoa:desktop-mixin) &rest initargs &key &allow-other-keys)
  (remf initargs :display)
  (list* (find-class 'cocoa::pointing-hand-cursor) initargs))

#+win32
(defmethod compute-make-instance-arguments-with-display ((protocol pointing-hand-cursor) (display win32:desktop-mixin) &rest initargs &key &allow-other-keys)
  (remf initargs :display)
  (list* (find-class 'win32::pointing-hand-cursor) initargs))

#+x11
(defmethod compute-make-instance-arguments-with-display ((protocol pointing-hand-cursor) (display x11:server-mixin) &rest initargs &key &allow-other-keys)
  (remf initargs :display)
  (list* (find-class 'x11::pointing-hand-cursor) initargs))

#+wayland
(defmethod compute-make-instance-arguments-with-display ((protocol pointing-hand-cursor) (display wayland:desktop-mixin) &rest initargs &key &allow-other-keys)
  (remf initargs :display)
  (list* (find-class 'wayland::pointing-hand-cursor) initargs))

#+cocoa
(defmethod compute-make-instance-arguments-with-display ((protocol open-hand-cursor) (display cocoa:desktop-mixin) &rest initargs &key &allow-other-keys)
  (remf initargs :display)
  (list* (find-class 'cocoa::open-hand-cursor) initargs))

#+win32
(defmethod compute-make-instance-arguments-with-display ((protocol open-hand-cursor) (display win32:desktop-mixin) &rest initargs &key &allow-other-keys)
  (remf initargs :display)
  (list* (find-class 'win32::open-hand-cursor) initargs))

#+x11
(defmethod compute-make-instance-arguments-with-display ((protocol open-hand-cursor) (display x11:server-mixin) &rest initargs &key &allow-other-keys)
  (remf initargs :display)
  (list* (find-class 'x11::open-hand-cursor) initargs))

#+wayland
(defmethod compute-make-instance-arguments-with-display ((protocol open-hand-cursor) (display wayland:desktop-mixin) &rest initargs &key &allow-other-keys)
  (remf initargs :display)
  (list* (find-class 'wayland::open-hand-cursor) initargs))

#+cocoa
(defmethod compute-make-instance-arguments-with-display ((protocol closed-hand-cursor) (display cocoa:desktop-mixin) &rest initargs &key &allow-other-keys)
  (remf initargs :display)
  (list* (find-class 'cocoa::closed-hand-cursor) initargs))

#+win32
(defmethod compute-make-instance-arguments-with-display ((protocol closed-hand-cursor) (display win32:desktop-mixin) &rest initargs &key &allow-other-keys)
  (remf initargs :display)
  (list* (find-class 'win32::closed-hand-cursor) initargs))

#+x11
(defmethod compute-make-instance-arguments-with-display ((protocol closed-hand-cursor) (display x11:server-mixin) &rest initargs &key &allow-other-keys)
  (remf initargs :display)
  (list* (find-class 'x11::closed-hand-cursor) initargs))

#+wayland
(defmethod compute-make-instance-arguments-with-display ((protocol closed-hand-cursor) (display wayland:desktop-mixin) &rest initargs &key &allow-other-keys)
  (remf initargs :display)
  (list* (find-class 'wayland::closed-hand-cursor) initargs))

#+cocoa
(defmethod compute-make-instance-arguments-with-display ((protocol ibeam-cursor) (display cocoa:desktop-mixin) &rest initargs &key &allow-other-keys)
  (remf initargs :display)
  (list* (find-class 'cocoa::ibeam-cursor) initargs))

#+win32
(defmethod compute-make-instance-arguments-with-display ((protocol ibeam-cursor) (display win32:desktop-mixin) &rest initargs &key &allow-other-keys)
  (remf initargs :display)
  (list* (find-class 'win32::ibeam-cursor) initargs))

#+x11
(defmethod compute-make-instance-arguments-with-display ((protocol ibeam-cursor) (display x11:server-mixin) &rest initargs &key &allow-other-keys)
  (remf initargs :display)
  (list* (find-class 'x11::ibeam-cursor) initargs))

#+wayland
(defmethod compute-make-instance-arguments-with-display ((protocol ibeam-cursor) (display wayland:desktop-mixin) &rest initargs &key &allow-other-keys)
  (remf initargs :display)
  (list* (find-class 'wayland::ibeam-cursor) initargs))

#+cocoa
(defmethod compute-make-instance-arguments-with-display ((protocol crosshair-cursor) (display cocoa:desktop-mixin) &rest initargs &key &allow-other-keys)
  (remf initargs :display)
  (list* (find-class 'cocoa::crosshair-cursor) initargs))

#+win32
(defmethod compute-make-instance-arguments-with-display ((protocol crosshair-cursor) (display win32:desktop-mixin) &rest initargs &key &allow-other-keys)
  (remf initargs :display)
  (list* (find-class 'win32::crosshair-cursor) initargs))

#+x11
(defmethod compute-make-instance-arguments-with-display ((protocol crosshair-cursor) (display x11:server-mixin) &rest initargs &key &allow-other-keys)
  (remf initargs :display)
  (list* (find-class 'x11::crosshair-cursor) initargs))

#+wayland
(defmethod compute-make-instance-arguments-with-display ((protocol crosshair-cursor) (display wayland:desktop-mixin) &rest initargs &key &allow-other-keys)
  (remf initargs :display)
  (list* (find-class 'wayland::crosshair-cursor) initargs))

#+cocoa
(defmethod compute-make-instance-arguments-with-display ((protocol compass-cursor) (display cocoa:desktop-mixin) &rest initargs &key &allow-other-keys)
  (remf initargs :display)
  (list* (find-class 'cocoa::compass-cursor) initargs))

#+win32
(defmethod compute-make-instance-arguments-with-display ((protocol compass-cursor) (display win32:desktop-mixin) &rest initargs &key &allow-other-keys)
  (remf initargs :display)
  (list* (find-class 'win32::compass-cursor) initargs))

#+x11
(defmethod compute-make-instance-arguments-with-display ((protocol compass-cursor) (display x11:server-mixin) &rest initargs &key &allow-other-keys)
  (remf initargs :display)
  (list* (find-class 'x11::compass-cursor) initargs))

#+wayland
(defmethod compute-make-instance-arguments-with-display ((protocol compass-cursor) (display wayland:desktop-mixin) &rest initargs &key &allow-other-keys)
  (remf initargs :display)
  (list* (find-class 'wayland::compass-cursor) initargs))

#+cocoa
(defmethod compute-make-instance-arguments-with-display ((protocol NWSE-cursor) (display cocoa:desktop-mixin) &rest initargs &key &allow-other-keys)
  (remf initargs :display)
  (list* (find-class 'cocoa::NWSE-cursor) initargs))

#+win32
(defmethod compute-make-instance-arguments-with-display ((protocol NWSE-cursor) (display win32:desktop-mixin) &rest initargs &key &allow-other-keys)
  (remf initargs :display)
  (list* (find-class 'win32::NWSE-cursor) initargs))

#+x11
(defmethod compute-make-instance-arguments-with-display ((protocol NWSE-cursor) (display x11:server-mixin) &rest initargs &key &allow-other-keys)
  (remf initargs :display)
  (list* (find-class 'x11::NWSE-cursor) initargs))

#+wayland
(defmethod compute-make-instance-arguments-with-display ((protocol NWSE-cursor) (display wayland:desktop-mixin) &rest initargs &key &allow-other-keys)
  (remf initargs :display)
  (list* (find-class 'wayland::NWSE-cursor) initargs))

#+cocoa
(defmethod compute-make-instance-arguments-with-display ((protocol NESW-cursor) (display cocoa:desktop-mixin) &rest initargs &key &allow-other-keys)
  (remf initargs :display)
  (list* (find-class 'cocoa::NESW-cursor) initargs))

#+win32
(defmethod compute-make-instance-arguments-with-display ((protocol NESW-cursor) (display win32:desktop-mixin) &rest initargs &key &allow-other-keys)
  (remf initargs :display)
  (list* (find-class 'win32::NESW-cursor) initargs))

#+x11
(defmethod compute-make-instance-arguments-with-display ((protocol NESW-cursor) (display x11:server-mixin) &rest initargs &key &allow-other-keys)
  (remf initargs :display)
  (list* (find-class 'x11::NESW-cursor) initargs))

#+wayland
(defmethod compute-make-instance-arguments-with-display ((protocol NESW-cursor) (display wayland:desktop-mixin) &rest initargs &key &allow-other-keys)
  (remf initargs :display)
  (list* (find-class 'wayland::NESW-cursor) initargs))

#+cocoa
(defmethod compute-make-instance-arguments-with-display ((protocol EW-cursor) (display cocoa:desktop-mixin) &rest initargs &key &allow-other-keys)
  (remf initargs :display)
  (list* (find-class 'cocoa::EW-cursor) initargs))

#+win32
(defmethod compute-make-instance-arguments-with-display ((protocol EW-cursor) (display win32:desktop-mixin) &rest initargs &key &allow-other-keys)
  (remf initargs :display)
  (list* (find-class 'win32::EW-cursor) initargs))

#+x11
(defmethod compute-make-instance-arguments-with-display ((protocol EW-cursor) (display x11:server-mixin) &rest initargs &key &allow-other-keys)
  (remf initargs :display)
  (list* (find-class 'x11::EW-cursor) initargs))

#+wayland
(defmethod compute-make-instance-arguments-with-display ((protocol EW-cursor) (display wayland:desktop-mixin) &rest initargs &key &allow-other-keys)
  (remf initargs :display)
  (list* (find-class 'wayland::EW-cursor) initargs))

#+cocoa
(defmethod compute-make-instance-arguments-with-display ((protocol NS-cursor) (display cocoa:desktop-mixin) &rest initargs &key &allow-other-keys)
  (remf initargs :display)
  (list* (find-class 'cocoa::NS-cursor) initargs))

#+win32
(defmethod compute-make-instance-arguments-with-display ((protocol NS-cursor) (display win32:desktop-mixin) &rest initargs &key &allow-other-keys)
  (remf initargs :display)
  (list* (find-class 'win32::NS-cursor) initargs))

#+x11
(defmethod compute-make-instance-arguments-with-display ((protocol NS-cursor) (display x11:server-mixin) &rest initargs &key &allow-other-keys)
  (remf initargs :display)
  (list* (find-class 'x11::NS-cursor) initargs))

#+wayland
(defmethod compute-make-instance-arguments-with-display ((protocol NS-cursor) (display wayland:desktop-mixin) &rest initargs &key &allow-other-keys)
  (remf initargs :display)
  (list* (find-class 'wayland::NS-cursor) initargs))

#+cocoa
(defmethod compute-make-instance-arguments-with-display ((protocol up-cursor) (display cocoa:desktop-mixin) &rest initargs &key &allow-other-keys)
  (remf initargs :display)
  (list* (find-class 'cocoa::up-cursor) initargs))

#+win32
(defmethod compute-make-instance-arguments-with-display ((protocol up-cursor) (display win32:desktop-mixin) &rest initargs &key &allow-other-keys)
  (remf initargs :display)
  (list* (find-class 'win32::up-cursor) initargs))

#+x11
(defmethod compute-make-instance-arguments-with-display ((protocol up-cursor) (display x11:server-mixin) &rest initargs &key &allow-other-keys)
  (remf initargs :display)
  (list* (find-class 'x11::up-cursor) initargs))

#+wayland
(defmethod compute-make-instance-arguments-with-display ((protocol up-cursor) (display wayland:desktop-mixin) &rest initargs &key &allow-other-keys)
  (remf initargs :display)
  (list* (find-class 'wayland::up-cursor) initargs))

#+cocoa
(defmethod compute-make-instance-arguments-with-display ((protocol down-cursor) (display cocoa:desktop-mixin) &rest initargs &key &allow-other-keys)
  (remf initargs :display)
  (list* (find-class 'cocoa::down-cursor) initargs))

#+win32
(defmethod compute-make-instance-arguments-with-display ((protocol down-cursor) (display win32:desktop-mixin) &rest initargs &key &allow-other-keys)
  (remf initargs :display)
  (list* (find-class 'win32::down-cursor) initargs))

#+x11
(defmethod compute-make-instance-arguments-with-display ((protocol down-cursor) (display x11:server-mixin) &rest initargs &key &allow-other-keys)
  (remf initargs :display)
  (list* (find-class 'x11::down-cursor) initargs))

#+wayland
(defmethod compute-make-instance-arguments-with-display ((protocol down-cursor) (display wayland:desktop-mixin) &rest initargs &key &allow-other-keys)
  (remf initargs :display)
  (list* (find-class 'wayland::down-cursor) initargs))

#+cocoa
(defmethod compute-make-instance-arguments-with-display ((protocol wait-cursor) (display cocoa:desktop-mixin) &rest initargs &key &allow-other-keys)
  (remf initargs :display)
  (list* (find-class 'cocoa::wait-cursor) initargs))

#+win32
(defmethod compute-make-instance-arguments-with-display ((protocol wait-cursor) (display win32:desktop-mixin) &rest initargs &key &allow-other-keys)
  (remf initargs :display)
  (list* (find-class 'win32::wait-cursor) initargs))

#+x11
(defmethod compute-make-instance-arguments-with-display ((protocol wait-cursor) (display x11:server-mixin) &rest initargs &key &allow-other-keys)
  (remf initargs :display)
  (list* (find-class 'x11::wait-cursor) initargs))

#+wayland
(defmethod compute-make-instance-arguments-with-display ((protocol wait-cursor) (display wayland:desktop-mixin) &rest initargs &key &allow-other-keys)
  (remf initargs :display)
  (list* (find-class 'wayland::wait-cursor) initargs))

#+cocoa
(defmethod compute-make-instance-arguments-with-display ((protocol not-allowed-cursor) (display cocoa:desktop-mixin) &rest initargs &key &allow-other-keys)
  (remf initargs :display)
  (list* (find-class 'cocoa::not-allowed-cursor) initargs))

#+win32
(defmethod compute-make-instance-arguments-with-display ((protocol not-allowed-cursor) (display win32:desktop-mixin) &rest initargs &key &allow-other-keys)
  (remf initargs :display)
  (list* (find-class 'win32::not-allowed-cursor) initargs))

#+x11
(defmethod compute-make-instance-arguments-with-display ((protocol not-allowed-cursor) (display x11:server-mixin) &rest initargs &key &allow-other-keys)
  (remf initargs :display)
  (list* (find-class 'x11::not-allowed-cursor) initargs))

#+wayland
(defmethod compute-make-instance-arguments-with-display ((protocol not-allowed-cursor) (display wayland:desktop-mixin) &rest initargs &key &allow-other-keys)
  (remf initargs :display)
  (list* (find-class 'wayland::not-allowed-cursor) initargs))
