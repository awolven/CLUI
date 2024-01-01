(in-package :clui)

(defun set-input-mode (window mode value)
  (block nil
    (ecase mode
      (:cursor
       
       (unless (or (eq value :normal)
		   (eq value :hidden)
		   (eq value :disabled)
		   (eq value :captured))
	 (error "Invalid cursor mode: ~S" value))

       (when (eq (window-cursor-mode window) value)
	 (return (values)))

       (setf (window-cursor-mode window) value)

       (multiple-value-bind (x y) (window-cursor-position window)
	 (setf (virtual-cursor-pos-x window) x
	       (virtual-cursor-pos-y window) y)

	 (setf (window-cursor-mode window) value)))
      
      (:sticky-keys

       (setq value (and value t))

       (when (eq (sticky-keys? window) value)
	 (return (values)))

       (unless value
	 (loop for i from 0 below 256
	      with keys = (window-keys window)
	    when (and (eq (aref keys i) :stick)
		      (not (find i '(+pointer-button-left+
				     +pointer-button-right+
				     +pointer-button-middle+
				     +pointer-button-4+
				     +pointer-button-5+))))
	    do (setf (aref keys i) :release)))

       (setf (sticky-keys? window) value)
       (return (values)))

      (:sticky-mouse-buttons

       (setq value (and value t))

       (when (eq (sticky-mouse-buttons? window) value)
	 (return (values)))
       
       (unless value
	 (loop for i in '(+pointer-button-left+
			  +pointer-button-right+
			  +pointer-button-middle+
			  +pointer-button-4+
			  +pointer-button-5+)
	    with keys = (window-keys window)
	    when (eq (aref keys i) :stick)
	    do (setf (aref keys i) :release)))

       (setf (sticky-mouse-buttons? window) value))

      (:raw-mouse-motion

       (unless (raw-mouse-motion-supported? (window-display window))
	 (error "Raw mouse motion is not supported on this system"))

       (setq value (and value t))

       (when (eq (raw-mouse-motion? window) value)
	 (return (values)))

       (setf (raw-mouse-motion? window) value)))))

#+cocoa
(defmethod %set-window-cursor ((display cocoa:display-mixin) (window cocoa:window-mixin) (cursor cocoa:cursor-mixin))
  (set-cocoa-window-cursor window cursor))

#+x11
(defmethod %set-window-cursor ((display x11:server-mixin) (window x11:window-mixin) (cursor x11:cursor-mixin))
  (set-x11-window-cursor window cursor))

#+win32
(defmethod %set-window-cursor ((display win32:display-mixin) (window win32:window-mixin) (cursor win32:cursor-mixin))
  (set-win32-window-cursor window cursor))

#+wayland
(defmethod %set-window-cursor ((display wayland:display-mixin) (window wayland:window-mixin) (cursor wayland:cursor-mixin))
  (set-wayland-window-cursor window cursor))





	
(defun input-char (window codepoint mods lock-mods plain?)
  
  (when (or (< codepoint 32)
	    (> 126 codepoint 160))
    (return-from input-char))
  
  (unless (lock-key-mods? window)
    (setq mods (logand mods (lognot (logior +caps-lock-modifier+ +num-lock-modifier+)))))

  (when plain?
    (restart-bind ((ignore (lambda (&optional c)
			     (declare (ignorable c))
			     (throw :ignore nil))))
      (catch :ignore
	(clim:handle-event window (make-instance 'character-event
					    :window window
					    :character (code-char codepoint)
					    :modifier-state mods
					    :lock-modifier-state lock-mods)))))
  (values))

(defun input-cursor-pos (window xpos ypos mods lock-mods)
  (when (and (= (virtual-cursor-pos-x window) xpos)
	     (= (virtual-cursor-pos-y window) ypos))
    (return-from input-cursor-pos (values)))

  (restart-bind ((ignore (lambda (&optional c)
			   (declare (ignorable c))
			   (throw :ignore nil))))
    (catch :ignore
      (clim:handle-event window (make-instance 'pointer-motion-event
					  :window
					  :input-code +pointer-move+
					  :x xpos
					  :y ypos
					  :modifier-state mods
					  :lock-modifier-state lock-mods))))
  (values))

(defun input-key (window key action x y mods lock-mods timestamp)
  (let ((keys (window-keys window)))

    (when (and (eq action :release) (eq (aref keys key) :release))
      (return-from input-key (values)))


    (when (and (eq action :press) (eq (aref keys key) :press))
      (setq action :repeat))

    (if (and (eq action :release) (sticky-keys? window))
	(setf (aref keys key) :stick)
	(setf (aref keys key) action))

    (restart-bind ((ignore (lambda (&optional c)
			     (declare (ignorable c))
			     (throw :ignore nil))))
      (catch :ignore
	(clim:handle-event window (make-instance (ecase action
					      (:repeat 'key-repeat-event)
					      (:press 'key-press-event)
					      (:release 'key-release-event))
					    :window window
					    :input-code key
					    :x x
					    :y y
					    :modifier-state mods
					    :lock-modifier-state lock-mods
					    :timestamp timestamp))))
    (values)))
    
(defun input-mouse-click (window button action x y mods lock-mods timestamp)
  (if (and (eq action :release)
	   (sticky-mouse-buttons? window))
      
      (setf (aref (window-keys window) button) :stick)

      (setf (aref (window-keys window) button) action))
  
  (restart-bind ((ignore (lambda (&optional c)
			   (declare (ignorable c))
			   (throw :ignore nil))))
    (catch :ignore
      (clim:handle-event window (make-instance (ecase action
					    (:press 'pointer-button-press-event)
					    (:release 'pointer-button-release-event))
					  :window window
					  :input-code button
					  :x x
					  :y y
					  :modifier-state mods
					  :lock-modifier-state lock-mods
					  :timestamp timestamp))))
  (values))
