(in-package :abstract-os)
(named-readtables:in-readtable :objc-readtable)

(cffi:define-foreign-library metalkit
  (:darwin (:framework "MetalKit")))

(cffi:use-foreign-library metalkit)

(defun sap-int (sap)
  #+sbcl(sb-sys:sap-int sap)
  #+ccl(ccl::%ptr-to-int sap))

(defvar *trace-callbacks* t)

(defmacro deftraceable-callback (name return-type (&rest args) &body body)
  `(cffi:defcallback ,name ,return-type (,@args)
     (declare (ignorable ,@(mapcar #'car args)))
     (when *trace-callbacks*
       (format t "~%~A" ',name)
       (finish-output))
     (locally 
	 ,@body)))

(cffi:defcfun (class_copyMethodList "class_copyMethodList") :pointer (cls :pointer) (out-count :pointer))
(cffi:defcfun (class_copyIvarList "class_copyIvarList") :pointer (cls :pointer) (out-count :pointer))
(cffi:defcfun (method_copyReturnType "method_copyReturnType") :pointer (m :pointer))
(cffi:defcfun (method_getNumberOfArguments "method_getNumberOfArguments") :unsigned-int (m :pointer))
(cffi:defcfun (method_getArgumentType "method_getArgumentType") :void (m :pointer) (index :unsigned-int) (dst :pointer) (dst_len :long-long))
(cffi:defcfun (method_copyArgumentType "method_copyArgumentType") :pointer (m :pointer) (index :unsigned-int))
(cffi:defcfun (class_getClassMethod "class_getClassMethod") :pointer (cls :pointer) (name :pointer))
(cffi:defcfun (class_getInstanceMethod "class_getInstanceMethod") :pointer (cls :pointer) (name :pointer))
(cffi:defcfun (method_getName "method_getName") :pointer (m :pointer))
(cffi:defcfun (ivar_getName "ivar_getName") :pointer (m :pointer))
(cffi:defcfun (class_getSuperclass "class_getSuperclass") :pointer (cls :pointer))
(cffi:defcfun (object_setInstanceVariable "object_setInstanceVariable") :pointer (object :pointer) (name :string) (value :pointer))
(cffi:defcfun (object_getClass "object_getClass") :pointer (object :pointer))
(cffi:defcfun (objc_msgSendSuper "objc_msgSendSuper") :pointer (obj :pointer) (selector :pointer) &rest)
(cffi:defcfun (class_getProperty "class_getProperty") :pointer (class :pointer) (name :string))

(cffi:defcstruct objc_super
  (reciever :pointer)
  (super_class :pointer))

(cffi:defcfun (set-uncaught-exception-handler "objc_setUncaughtExceptionHandler")
    :void
  (cb :pointer))

(cffi:defcallback exception-handler :void ((exception :pointer))
  (objc-runtime::with-selectors (reason)
    (error "~&objc exception: ~a~%" (objc-runtime::extract-nsstring [exception reason]))))

(set-uncaught-exception-handler (cffi:callback exception-handler))

(defmacro with-autorelease-pool ((var) &body body)
  `(let ((,var (ns::|new| #@NSAutoReleasePool)))
     (unwind-protect (progn ,@body)
       (ns::|release| ,var))))

(defun super-msg-send (thing selector &rest args)
  (cffi:with-foreign-object (objc-super '(:struct objc_super))
    (setf (cffi:foreign-slot-value objc-super '(:struct objc_super) 'reciever) (objc-object-id thing)
	  (cffi:foreign-slot-value objc-super '(:struct objc_super) 'super_class) (class_getSuperclass (object_getClass (objc-object-id thing))))
    (eval `(objc_msgSendSuper ,objc-super ,selector ,@args))))

(defmacro new-msg-send-super (selector ((&rest arg-types) return-type))
  (let ((arg-syms (mapcar (lambda (_) _ (gensym))
                          arg-types)))
    `(lambda ,(cons 'target arg-syms)
       (cffi:with-foreign-object (objc-super '(:struct objc_super))
	 (setf (cffi:foreign-slot-value objc-super '(:struct objc_super) 'reciever) (objc-object-id target)
	       (cffi:foreign-slot-value objc-super '(:struct objc_super) 'super_class) (class_getSuperclass (object_getClass (objc-object-id target))))
	 (cffi:foreign-funcall "objc_msgSendSuper"
                             :pointer objc-super
                             :pointer ,selector
                             ,@(mapcan #'list arg-types arg-syms)
                             ,return-type)))))

#+sbcl
(defmethod objc-object-id ((thing sb-sys:system-area-pointer))
  thing)

#+ccl
(defmethod objc-object-id ((thing ccl::macptr))
  thing)

(defmethod objc-object-id ((thing null))
  (cffi:null-pointer))

(defun ff-call (name return-type &rest args)
  (multiple-value-bind (types ctypes fargs rettype)
      (cffi::parse-args-and-types (append args (list return-type)))
    (let ((syms (cffi::make-gensym-list (length types))))
      (eval 
       (cffi::foreign-funcall-form/fsbv-with-libffi name
						    fargs
						    syms
						    types
						    rettype
						    ctypes
						    nil)))))

(defun send (object message return-type &rest args)
  (if (consp return-type)
      (apply #'ff-call "objc_msgSend_stret"
	     return-type
	     :pointer (objc-object-id object)
	     :pointer message
	     args)
      (apply #'ff-call "objc_msgSend"
	     return-type
	     :pointer (objc-object-id object)
	     :pointer message
	     args)))


(defun alloc-init (objc-class)
  (ns:|init| (alloc objc-class)))

(defun alloc (objc-class)
  (ns:|alloc| objc-class))

(defun init (objc-object)
  (ns:|init| objc-object))
