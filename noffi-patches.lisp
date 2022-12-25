(in-package :noffi)

(defmacro with-stack-allocated-structure ((var struct-type pointer-type) &body body)
  (let ((array-sym (gensym))
	(sap-sym (gensym)))
    `(let* ((,array-sym (make-array (load-time-value (c-sizeof-type ,struct-type))
				    :element-type '(unsigned-byte 8))))
       (declare (dynamic-extent ,array-sym))
       (ccl:with-pointer-to-ivector (,sap-sym ,array-sym)
	 (let ((,var (%cons-ptr ,sap-sym 0 ,pointer-type)))
	   ,@body)))))

(defun offset-of (record-type member &optional env)
  (assert (record-type-p record-type))
  (let ((members (record-type-members record-type env :errorp t))
        (kind (cond ((union-type-p record-type) :union)
                    ((struct-type-p record-type) :struct)
                    (t (error "Huh"))))
        (max-align 8)          ;overall structure alignment in bits
        (allo 0)               ;allocation pointer in bits
        (max-allo 0)           ;overall maximum allocation
                                        ; (may differ from `allo' in case of unions).
        (res nil)                       ;assembled list of member layouts
        )
    (dolist (candidate-member members)
      (when (eq candidate-member member)
	(return))
      (multiple-value-bind (member-name member-type)
          (values (record-member-name candidate-member) (record-member-type candidate-member))
        (cond ((bit-field-type-p member-type)
               ;; See above for what we do.
               (let ((width (c-eval (bit-field-type-width member-type) env)) ;eval?
                     (base-type (bit-field-type-base-type member-type env)))
                 (multiple-value-bind (base-size base-align)
                     (type-size-align base-type env)
		   (declare (ignore base-size))
                   ;; Try fitting it directly at `allo' first.
                   (unless (and (not (zerop width))
                                (= (floor allo base-align)
                                   (floor (+ allo width) base-align)))
                     ;; Doesn't fit. Align 'allo' first.
                     (setq allo (* base-align (ceiling allo base-align))))
                   ;; Place it at `allo'
                   (push (list member-name member-type width allo) res)
                   ;; Adjust overall align and size.
                   (setf max-align (max max-align base-align))
                   ;; (setq max-allo (max max-allo (+ allo base-size)))
                   (setq max-allo (max max-allo (+ allo width)))
                   (incf allo width))))
              (t
               (multiple-value-bind (member-size member-align)
                   (type-size-align member-type env)
                 ;; Update the overall alignment, if needed
                 (setq max-align (max max-align member-align))
                 ;; Bump the allocation pointer according to alignment.
                 (setq allo      (if (eq :union kind)
                                     0
                                     (* member-align (ceiling allo member-align))))
                 ;; Place it
                 (push (list member-name member-type member-size allo) res)
                 (incf allo member-size)
                 (setq max-allo (max max-allo allo)))))))
    max-align))
