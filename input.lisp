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
	 (error "Raw mouse motion is not supported on this system")
	 (return (values)))

       (setq value (and value t))

       (when (eq (raw-mouse-motion? window) value)
	 (return (values)))

       (setf (raw-mouse-motion? window) value)))))


(defun get-window-cursor-pos (window)
  (if (eq (window-cursor-mode window) :disabled)
      (values (virtual-cursor-pos-x window)
	      (virtual-cursor-pos-y window))
      (%get-window-cursor-pos window)))
	

      

       
	      
