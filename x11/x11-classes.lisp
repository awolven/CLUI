(in-package :clui)

(defclass x11-state ()
  ((initialized? :initform nil
		 :accessor x11-initialized?)
   
   (randr-available? :initform nil
		     :accessor randr-available?)

   (randr-event-base :initform nil
		     :accessor randr-event-base)

   (randr-error-base :initform nil
		     :accessor randr-error-base)

   (randr-major :initform nil
		:accessor randr-major)

   (randr-minor :initform nil
		:accessor randr-minor)

   (randr-gamma-broken? :initform nil
			:accessor randr-gamma-broken?)

   (randr-monitor-broken? :initform nil
			  :accessor randr-monitor-broken?)

   (xinerama-major :initform nil
		   :accessor xinerama-major)

   (xinerama-minor :initform nil
		   :accessor xinerama-minor)

   (xinerama-available? :initform nil
			:accessor xinerama-available?)

   (XdndVersion :initform nil
		:accessor xdnd-version)

   (xi-available? :initform nil
		  :accessor xi-available?)

   (xi-major-opcode :initform nil
		    :accessor xi-major-opcode)

   (xkb-available? :initform nil
		   :accessor xkb-available?)

   (xkb-major-opcode :initform nil
		    :accessor xkb-major-opcode)

   (xkb-event-base :initform nil
		   :accessor xkb-event-base)

   (xkb-error-base :initform nil
		   :accessor xkb-error-base)

   (xkb-major :initform nil
	      :accessor xkb-major)

   (xkb-minor :initform nil
	      :accessor xkb-minor)

   (xkb-detectable? :initform nil
		    :accessor xkb-detectable?)

   (xkb-group :initform nil
	      :accessor xkb-group)

   (xcb-available? :initform t
		   :accessor xcb-available?)

   (xcb-vulkan-surface? :initform t
			:accessor xcb-vulkan-surface?)))

  

(defclass window-manager ()
  ((NET_SUPPORTED :initform nil)
   (NET_SUPPORTING_WM_CHECK :initform nil)
   (WM_PROTOCOLS :initform nil)
   (WM_STATE :initform nil)
   (WM_DELETE_WINDOW :initform nil)
   (NET_WM_NAME :initform nil)
   (NET_WM_ICON_NAME :initform nil)
   (NET_WM_ICON :initform nil)
   (NET_WM_PID :initform nil)
   (NET_WM_PING :initform nil)
   (NET_WM_WINDOW_TYPE :initform nil)
   (NET_WM_WINDOW_TYPE_NORMAL :initform nil)
   (NET_WM_STATE :initform nil)
   (NET_WM_STATE_ABOVE :initform nil)
   (NET_WM_STATE_FULLSCREEN :initform nil)
   (NET_WM_STATE_MAXIMIZED_VERT :initform nil)
   (NET_WM_STATE_MAXIMIZED_HORZ :initform nil)
   (NET_WM_STATE_DEMANDS_ATTENTION :initform nil)
   (NET_WM_BYPASS_COMPOSITOR :initform nil)
   (NET_WM_FULLSCREEN_MONITORS :initform nil)
   (NET_WM_WINDOW_OPACITY :initform nil)
   (NET_WM_CM_Sx :initform nil)
   (NET_WORKAREA :initform nil)
   (NET_CURRENT_DESKTOP :initform nil)
   (NET_ACTIVE_WINDOW :initform nil)
   (NET_FRAME_EXTENTS :initform nil)
   (NET_REQUEST_FRAME_EXTENTS :initform nil)
   (MOTIF_WM_HINTS :initform nil)))

(defclass Xdnd ()
  ((version :initform 5)
   (XdndAware)
   (XdndEnter)
   (XdndPosition)
   (XdndStatus)
   (XdndActionCopy)
   (XdndDrop)
   (XdndFinished)
   (XdndSelection)
   (XdndTypeList)
   (text/uri-list)))

(defclass clipboard-manager ()
  ((TARGETS)
   (MULTIPLE)
   (INCR)
   (CLIPBOARD)
   (PRIMARY)
   (CLIPBOARD_MANAGER)
   (SAVE_TARGETS)
   (ATOM_PAIR)
   (COMPOUND_STRING)))

(defclass x11:server-mixin (clui:display-mixin clui:handle-mixin)
  ((display-name :initarg :display-name
		 :accessor display-name
		 :initform nil)
   
   (error-handler :initform nil
		  :accessor display-error-handler)

   (error-code :initform nil
	       :accessor display-error-code)

   (hidden-cursor :accessor hidden-cursor)
   
   (context :accessor unique-context)
   
   (content-scale :accessor display-content-scale)
   
   (input-method :initform nil
		 :accessor display-input-method)

   (primary-selection-string :initform ""
			     :accessor primary-selection-string)

   (empty-event-pipes :initform nil
		      :accessor empty-event-pipes)

   (x11-state :initform (make-instance 'x11-state)
	      :accessor display-x11-state)

   (window-manager :initform (make-instance 'window-manager)
		   :accessor display-window-manager)

   (Xdnd :initform (make-instance 'Xdnd)
	 :accessor display-drag-and-drop)

   (clipboard-manager :initform (make-instance 'clipboard-manager)
		      :accessor display-clipboard-manager)

   (UTF8_STRING :initform nil)
   (NULL :initform nil)
   (CLUI_SELECTION :initform nil)))   

(defun default-screen-id (display)
  (screen-id (default-screen display)))

(defun default-root-window-handle (display)
  (h (default-screen display)))

(defun hidden-cursor-handle (display)
  (h (hidden-cursor display)))

(defmethod initialize-instance ((instance x11:server-mixin) &rest initargs &key &allow-other-keys)
  (declare (ignorable initargs))
  (call-next-method)
  (init-and-connect-x11 instance)
  instance)

(defclass x11:local-server-mixin (x11:server-mixin)
  ())

(defclass x11:remote-server-mixin (x11:server-mixin)
  ())

(defclass x11:local-server (x11:local-server-mixin)
  ())

(defclass x11:remote-server (x11:remote-server-mixin)
  ())

(defclass x11:screen-mixin (clui:screen-mixin clui:handle-mixin)
  ((screen-id :initarg :screen-id :accessor screen-id)))

(defclass x11:window-mixin (clui:os-window-mixin clui:handle-mixin)
  ((colormap
    :initform nil
    :accessor window-colormap)
   
   (input-context
    :initform nil
    :accessor window-input-context)

   (override-redirect?
    :initform nil
    :accessor override-redirect?)

   (%iconified?
    :type boolean
    :initform nil
    :accessor last-iconified?)
   
   (%maximized?
    :type boolean
    :initform nil
    :accessor last-maximized?)

   (%transparent?
    :type boolean
    :initform nil
    :accessor last-transparent?)

   (%width
    :type real
    :initform 0
    :accessor last-width)
   
   (%height
    :type real
    :initform 0
    :accessor last-height)

   (%xpos
    :type real
    :initform 0
    :accessor last-pos-x)
   
   (%ypos
    :type real
    :initform 0
    :accessor last-pos-y)
   
   (%cursor-pos-x
    :initform nil
    :accessor last-cursor-pos-x)
   
   (%cursor-pos-y
    :initform nil
    :accessor last-cursor-pos-y)

   (cursor-warp-pos-x
    :initform nil
    :accessor cursor-warp-pos-x)

   (cursor-warp-pos-y
    :initform nil
    :accessor cursor-warp-pos-y)
   
   (key-press-times
    :initform (make-array 256 :initial-element 0)
    :reader key-press-times)))

(defmethod initialize-instance :after ((instance x11:window-mixin) &rest initargs &key &allow-other-keys)
  (apply #'create-native-x11-window instance initargs)
  (values))  



(defclass x11:cursor-mixin (clui:cursor-mixin clui:handle-mixin)
  ())

(defclass x11::arrow-cursor (arrow-cursor-mixin x11:cursor-mixin)
  ())

(defmethod initialize-instance :after ((instance x11::arrow-cursor) &rest initargs &key display)
  (declare (ignore initargs))
  (setf (h instance) (create-x11-standard-cursor display :arrow))
  (values))

(defclass x11::hand-cursor (hand-cursor-mixin x11:cursor-mixin)
  ())

(defmethod initialize-instance :after ((instance x11::hand-cursor) &rest initargs &key display)
  (declare (ignore initargs))
  (setf (h instance) (create-x11-standard-cursor display :hand))
  (values))

(defclass x11::pointing-hand-cursor (pointing-hand-cursor-mixin x11:cursor-mixin)
  ())

(defmethod initialize-instance :after ((instance x11::pointing-hand-cursor) &rest initargs &key display)
  (declare (ignore initargs))
  (setf (h instance) (create-x11-standard-cursor display :pointing-hand))
  (values))

(defclass x11::open-hand-cursor (open-hand-cursor-mixin x11:cursor-mixin)
  ())

(defclass x11::ibeam-cursor (ibeam-cursor-mixin x11:cursor-mixin)
  ())

(defmethod initialize-instance :after ((instance x11::ibeam-cursor) &rest initargs &key display)
  (declare (ignore initargs))
  (setf (h instance) (create-x11-standard-cursor display :ibeam))
  (values))

(defclass x11::crosshair-cursor (crosshair-cursor-mixin x11:cursor-mixin)
  ())

(defmethod initialize-instance :after ((instance x11::crosshair-cursor) &rest initargs &key display)
  (declare (ignore initargs))
  (setf (h instance) (create-x11-standard-cursor display :crosshair))
  (values))

(defclass x11::compass-cursor (compass-cursor-mixin x11:cursor-mixin)
  ())

(defmethod initialize-instance :after ((instance x11::compass-cursor) &rest initargs &key display)
  (declare (ignore initargs))
  (setf (h instance) (create-x11-standard-cursor display :compass))
  (values))



(defclass x11::resize-nw-cursor (resize-nw-cursor-mixin x11:cursor-mixin)
  ())

(defmethod initialize-instance :after ((instance x11::resize-nw-cursor) &rest initargs &key display)
  (declare (ignore initargs))
  (setf (h instance) (create-x11-standard-cursor display :resize-nw))
  (values))

(defclass x11::resize-se-cursor (resize-se-cursor-mixin x11:cursor-mixin)
  ())

(defmethod initialize-instance :after ((instance x11::resize-se-cursor) &rest initargs &key display)
  (declare (ignore initargs))
  (setf (h instance) (create-x11-standard-cursor display :resize-se))
  (values))



(defclass x11::resize-ne-cursor (resize-ne-cursor-mixin x11:cursor-mixin)
  ())

(defmethod initialize-instance :after ((instance x11::resize-ne-cursor) &rest initargs &key display)
  (declare (ignore initargs))
  (setf (h instance) (create-x11-standard-cursor display :resize-ne))
  (values))

(defclass x11::resize-sw-cursor (resize-sw-cursor-mixin x11:cursor-mixin)
  ())

(defmethod initialize-instance :after ((instance x11::resize-sw-cursor) &rest initargs &key display)
  (declare (ignore initargs))
  (setf (h instance) (create-x11-standard-cursor display :resize-sw))
  (values))



(defclass x11::resize-e-cursor (resize-e-cursor-mixin x11:cursor-mixin)
  ())

(defmethod initialize-instance :after ((instance x11::resize-e-cursor) &rest initargs &key display)
  (declare (ignore initargs))
  (setf (h instance) (create-x11-standard-cursor display :resize-e))
  (values))

(defclass x11::resize-w-cursor (resize-w-cursor-mixin x11:cursor-mixin)
  ())

(defmethod initialize-instance :after ((instance x11::resize-w-cursor) &rest initargs &key display)
  (declare (ignore initargs))
  (setf (h instance) (create-x11-standard-cursor display :resize-w))
  (values))



(defclass x11::resize-n-cursor (resize-n-cursor-mixin x11:cursor-mixin)
  ())

(defmethod initialize-instance :after ((instance x11::resize-n-cursor) &rest initargs &key display)
  (declare (ignore initargs))
  (setf (h instance) (create-x11-standard-cursor display :resize-n))
  (values))

(defclass x11::resize-s-cursor (resize-s-cursor-mixin x11:cursor-mixin)
  ())

(defmethod initialize-instance :after ((instance x11::resize-s-cursor) &rest initargs &key display)
  (declare (ignore initargs))
  (setf (h instance) (create-x11-standard-cursor display :resize-s))
  (values))

(defclass x11::up-cursor (up-cursor-mixin x11:cursor-mixin)
  ())

(defmethod initialize-instance :after ((instance x11::up-cursor) &rest initargs &key display)
  (declare (ignore initargs))
  (setf (h instance) (create-x11-standard-cursor display :resize-s))
  (values))

(defclass x11::down-cursor (down-cursor-mixin x11:cursor-mixin)
  ())

(defmethod initialize-instance :after ((instance x11::down-cursor) &rest initargs &key display)
  (declare (ignore initargs))
  (setf (h instance) (create-x11-standard-cursor display :down))
  (values))

(defclass x11::wait-cursor (wait-cursor-mixin x11:cursor-mixin)
  ())

(defmethod initialize-instance :after ((instance x11::wait-cursor) &rest initargs &key display)
  (declare (ignore initargs))
  (setf (h instance) (create-x11-standard-cursor display :wait))
  (values))

(defclass x11::not-allowed-cursor (not-allowed-cursor-mixin x11:cursor-mixin)
  ())

(defmethod initialize-instance :after ((instance x11::not-allowed-cursor) &rest initargs &key display)
  (declare (ignore initargs))
  (setf (h instance) (create-x11-standard-cursor display :not-allowed))
  (values))

(defclass x11:monitor-mixin (clui:monitor-mixin)
  ((display :initarg :display
	    :initform nil
	    :reader monitor-display)
   
   (index :initarg :x11-index
	  :initform nil
	  :accessor monitor-x11-index)
   
   (output :initarg :x11-output
	   :initform -1
	   :reader monitor-x11-output)

   (crtc :initarg :x11-crtc
	 :initform -1
	 :reader monitor-x11-crtc)

   (old-mode :initform nil
	     :accessor monitor-old-video-mode)))

(defclass x11:screen (x11:screen-mixin)
  ())

(defclass x11:window (x11:window-mixin)
  ())

(defclass x11:cursor (x11:cursor-mixin)
  ())

(defclass x11:monitor (x11:monitor-mixin)
  ())


