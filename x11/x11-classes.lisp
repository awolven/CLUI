(in-package :clui)

(defclass x11:server-mixin (clui:display-mixin)
  ((xdisplay :accessor display-xdisplay)
   
   (display-name :initarg :display-name
		 :accessor display-name
		 :initform nil)
   
   (screen-id :accessor default-screen-id)
   
   (root-window-handle :accessor default-root-window-handle)

   (helper-window-handle :accessor helper-window-handle)
   (hidden-cursor-handle :accessor hidden-cursor-handle)
   
   (xcursor-handle :accessor xcursor-handle
		   :initform nil)
   
   (context :accessor unique-context)
   
   (content-scale :accessor content-scale)
   
   (input-method :initform nil
		 :accessor display-input-method)

   (randr-available? :initform nil
		     :accessor display-randr-available?)

   (randr-monitor-broken? :initform nil
			  :accessor display-randr-monitor-broken?)

   (xinerama-available? :initform nil
			:accessor xinerama-available?)
   
   (ip-addr)
   (display-id)
   
   (empty-event-pipes :initform nil
		      :accessor empty-event-pipes)))

(defclass x11:local-server-mixin (x11:server-mixin)
  ())

(defclass x11:remote-server-mixin (x11:server-mixin)
  ())

(defclass x11:local-server (x11:local-server-mixin)
  ())

(defclass x11:remote-server (x11:remote-server-mixin)
  ())

(defclass x11:screen-mixin (clui:screen-mixin)
  ((screen-id)))

(defclass x11:window-mixin (clui:os-window-mixin)
  ())

(defclass x11:cursor-mixin (clui:cursor-mixin)
  ())

(defclass x11:monitor-mixin (clui:monitor-mixin)
  ((index :initarg :x11-index
	  :initform nil
	  :accessor monitor-x11-index)
   
   (output :initarg :x11-output
	   :initform nil
	   :reader monitor-x11-output)

   (crtc :initarg :x11-crtc
	 :initform nil
	 :reader monitor-x11-xrtc)))

(defclass x11:screen (x11:screen-mixin)
  ())

(defclass x11:window (x11:window-mixin)
  ())

(defclass x11:cursor (x11:cursor-mixin)
  ())

(defclass x11:monitor (x11:monitor-mixin)
  ())
