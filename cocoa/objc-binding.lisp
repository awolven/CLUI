(in-package :clui)
(named-readtables:in-readtable :objc-readtable)

(defparameter *wrapped-selector-table* (make-hash-table))

(defvar *export-list* ())

(defmacro make-message-lambda (selector ((&rest arg-types) return-type))
  (let ((arg-syms (mapcar (lambda (_) _ (gensym))
                          arg-types)))
    (if (and (consp return-type)
	     (or (and (eq (car return-type) :struct)
		      (not (eq (cadr return-type) 'ns::|CGPoint|)))
		 (eq (car return-type) :union)))
	`(lambda ,(cons 'target arg-syms)
	   (cffi:foreign-funcall "objc_msgSend_stret"
				 :pointer target
				 :pointer ,selector
				 ,@(mapcan #'list arg-types arg-syms)
				 ,return-type))
	`(lambda ,(cons 'target arg-syms)
	   (cffi:foreign-funcall "objc_msgSend"
				 :pointer target
				 :pointer ,selector
				 ,@(mapcan #'list arg-types arg-syms)
				 ,return-type)))))

(defmacro make-super-message-lambda (selector ((&rest arg-types) return-type))
  (let ((arg-syms (mapcar (lambda (_) _ (gensym))
                          arg-types)))
    (if (and (consp return-type)
	     (or (and (eq (car return-type) :struct)
		      (not (eq (cadr return-type) 'ns::|CGPoint|)))
		 (eq (car return-type) :union)))
	`(lambda ,(cons 'target arg-syms)
	   (cffi:with-foreign-object (objc-super '(:struct objc_super))
	     (setf (cffi:foreign-slot-value objc-super '(:struct objc_super) 'reciever) (objc-object-id target)
		   (cffi:foreign-slot-value objc-super '(:struct objc_super) 'super_class) (ns::|superclass| (ns::|class| (objc-object-id target))))
	     (cffi:foreign-funcall "objc_msgSendSuper_stret"
				   :pointer objc-super
				   :pointer ,selector
				   ,@(mapcan #'list arg-types arg-syms)
				   ,return-type)))
	`(lambda ,(cons 'target arg-syms)
	   (cffi:with-foreign-object (objc-super '(:struct objc_super))
	     (setf (cffi:foreign-slot-value objc-super '(:struct objc_super) 'reciever) (objc-object-id target)
		   (cffi:foreign-slot-value objc-super '(:struct objc_super) 'super_class) (ns::|superclass| (ns::|class| (objc-object-id target))))
	     (cffi:foreign-funcall "objc_msgSendSuper"
				   :pointer objc-super
				   :pointer ,selector
				   ,@(mapcan #'list arg-types arg-syms)
				   ,return-type))))))



(defun NS::|alloc| (class)
  (let ((message-lambda 
         (make-message-lambda @(alloc) (NIL :POINTER)))) 
   (funcall message-lambda (objc-object-id class))))

(defclass objc-method ()
  ((name :initarg :name :reader objc-method-name)
   (selector :initarg :selector :reader objc-method-selector)
   (return-type :initarg :return-type :reader objc-method-return-type)
   (number-of-args :initarg :number-of-args :reader objc-method-number-of-args)
   (args :initform nil :initarg :args :reader objc-method-args)))

(defmethod print-object ((obj objc-method) stream)
  (print-unreadable-object (obj stream :type t)
    (fresh-line stream)
    (princ ":name " stream)
    (princ (objc-method-selector obj) stream)
    (fresh-line stream)
    (princ ":return-type " stream)
    (format stream "~S" (objc-method-return-type obj))
    (fresh-line stream)
    (princ ":args (" stream)
    (loop for arg in (objc-method-args obj) for i from 0
       do (format stream ":name ~A :type ~S"
		  (objc-method-argument-name arg)
		  (objc-method-argument-type arg))
	 (fresh-line stream)
       finally (princ ")" stream))))
	   

(defclass objc-method-argument ()
  ((name :initarg :name :reader objc-method-argument-name)
   (type :initarg :type :reader objc-method-argument-type)))

(defun create-objc-method (m)
  (let* ((sel (method_getName m))
	 (rt (method_copyReturnType m))
	 (n (method_getNumberOfArguments m))
	 (parsed-method-name (parse-method-name (objc-runtime::sel-get-name sel))))
    (make-instance 'objc-method
		   :name (first parsed-method-name)
		   :selector (objc-runtime::sel-get-name sel)
		   :return-type (decode-objc-type (cffi:foreign-string-to-lisp rt))
		   :number-of-args n
		   :args (let ((arg-names ()))
			   (loop for j from 2 below n ;; -2 for self, selector
			      collect (let ((type (method_copyArgumentType m j)))
					(make-instance 'objc-method-argument
						       :name (if (= 2 j)
								 "_"
								 (let ((arg-name (or (nth (- j 3) (cdr parsed-method-name))
										     (format nil "arg~A" (- j 2)))))
								   (when (member arg-name arg-names :test #'string=)
								     (setq arg-name (concatenate 'string arg-name "x")))
								   (prog1 arg-name
								     (push arg-name arg-names))))
						       :type (let* ((type-string (cffi:foreign-string-to-lisp type)))
							       (decode-objc-type type-string)))))))))

(defun class-copy-method-list (class)
  (cffi:with-foreign-object (count :unsigned-int)
    (let ((list (class_copyMethodList class count)))
      (loop for i from 0 below (cffi:mem-aref count :unsigned-int)
	 collect (let* ((m (cffi:mem-aref list :pointer i)))
		   (create-objc-method m))))))
  
(defun class-copy-ivar-list (class)
  (cffi:with-foreign-object (count :unsigned-int)
    (let ((list (class_copyIvarList (objc-object-id class) count)))
      (loop for i from 0 below (cffi:mem-aref count :unsigned-int)
	 collect (ivar_getName (cffi:mem-aref list :pointer i))))))

(defvar *target-arch-register-size* :64-bit)

(defvar *indirection-level* 0)

(defmacro register-size-value (&key 64-bit 32-bit)
  `(ecase *target-arch-register-size*
     (:64-bit ,64-bit)
     (:32-bit ,32-bit)))

(defun parse-array (string &key start)
  (assert (> (length string) (+ 2 start)))
  (unless (char= (char string start) #\[)
    (error "~S is not an encoding of an array type." (subseq string start)))
  (multiple-value-bind (int end)
      (parse-integer string :start (1+ start) :junk-allowed t)
    (multiple-value-bind (type end)
	(decode-objc-type string :start end)
      ;; assume the #\[ is there.
      (values (list :array type int)
	      (+ 2 end)))))

(defun prep-type (type)
  (when type
    (if (stringp type)
	(read-from-string type)
	(if (listp type)
	    (list* (car type) (mapcar #'prep-type (cdr type)))
	    type))))

(defun parse-struct-or-union (string &key start kind)
  (multiple-value-bind (init term)
      (if (eq kind :union)
	  (values #\( #\))
	  (if (eq kind :struct)
	      (values #\{ #\})
	      (error "expected struct or union: ~S" kind)))
    (assert (> (length string) (+ 2 start)))
    (unless (char= (char string start) init)
      (error "~S is not an encoding of a struct/union type." string))
    (if (< *indirection-level* 2)
	(let ((break (position-next-thingamabob string :start (1+ start)))
	      (=-pos (position #\= string :start (1+ start))))
	  (if (and =-pos (< =-pos break))
	      (multiple-value-bind (name end) (parse-name string :start (1+ start) :terminator #\=)
		(declare (ignore end))
		(values (list kind name)
			(multiple-value-bind (types end)
			    (decode-objc-types string :start (1+ =-pos) :terminator term)
			  (declare (ignore types)) ;; discard types, all we need is the tag of the struct
			  end)))
	      (multiple-value-bind (types end)
		  (decode-objc-types string :start (1+ start) :terminator term)
		(values (if (eq kind :struct)
			    (list :array :char (reduce #'+ (mapcar #'cffi:foreign-type-size (mapcar #'prep-type types))))
			    (list :array :char (reduce #'max (mapcar #'cffi:foreign-type-size (mapcar #'prep-type types)))))
			(1+ end)))))
	;; indirection level is 2 or above, expecting only name
	(progn
	  (let ((=-pos (position #\= string :start (1+ start))))
	    (if =-pos
		(progn (warn "unexpected equals sign ~S" (subseq string (1+ start)))
		       (multiple-value-bind (name end) (parse-name (subseq string (1+ start) =-pos) :start 0 :terminator term) ;; definitely expecting a name
			 (declare (ignore end))
			 (values (list kind name)
				 (multiple-value-bind (types end)
				     (decode-objc-types string :start (1+ =-pos) :terminator term)
				   (declare (ignore types)) ;; discard types, all we need is the tag of the struct
				   end))))
		(multiple-value-bind (name end) (parse-name string :start (1+ start) :terminator term)
		  (when (not (char= (char string end) term))
		    (error "junk found while parsing ~A: ~S" kind (char string end)))
		  (values (list kind name)
			  (1+ end)))))))))

(defparameter *thingamabobs* (list #\* #\@ #\# #\: #\? #\] #\) #\} #\{ #\[ #\( #\^ #\- #\&))
      

(defun position-next-thingamabob (string &key (start 0))
  (flet ((%pos-list (char)
	   (let ((p (position char string :start start)))
	     (when p (list p)))))
    (let ((positions (apply #'append (mapcar #'%pos-list *thingamabobs*))))
      (when positions
	(apply #'min positions)))))

(defun parse-name (string &key start terminator)
  (let ((end (position terminator string :start start)))
    (values (concatenate 'string "ns::|" (subseq string start end) "|")
	    end)))

(defun parse-bitfield (string &key start)
  (assert (> (length string) (+ 1 start)))
  (unless (char= (char string start) #\b)
    (error "~S is not an encoding of a bitfield type." string))
  (multiple-value-bind (int end)
      (parse-integer string :start (1+ start) :junk-allowed t)
    (let ((aligned-size (+ 8 (mod 8 int))))
      ;; this is most likely misguided
      (values (or (case aligned-size
		    (8 :unsigned-char)
		    (16 :unsigned-short)
		    (24 :unsigned-int)
		    (32 :unsigned-int)
		    (otherwise nil))
		  :unsigned-long-long)
	      end))))

(defun parse-pointer (string &key start)
  (assert (> (length string) (+ 1 start)))
  (unless (char= (char string start) #\^)
    (error "~S is not an encoding of a pointer type." string))
  (multiple-value-bind (type end)
      (decode-objc-type string :start (+ start 1))
    (values (list :pointer type)
	    end)))

(defun parse-unknown-type (string &key start)
  (assert (> (length string) start))
  (unless (char= (char string start) #\?)
    (error "~S is not an encoding of an unknown pointer type." string))
  (if (= (length string) (1+ start)) ;; case where string ends on ?
      (values :pointer (1+ start))
      (let ((char (char string (1+ start)))) ;; there might be equals
	(if (char= char #\=)
	    (decode-objc-type string :start (+ 2 start))
	    (values :pointer (1+ start))))))

(defun decode-objc-types (string &key start (terminator nil))
  (values (loop until (or 
		       (= start (length string))
		       (and terminator (char= terminator (char string start))))
	     collect (progn (multiple-value-bind (type p) (decode-objc-type string :start start)
		      (setq start p)
		      type)))
	  start))

(defun decode-objc-type (string &key (start 0))
  (let ((char (char string start)))
    (ecase char
      (#\c (values :char (1+ start)))
      (#\i (values :int (1+ start)))
      (#\s (values :short (1+ start)))
      (#\l (values (register-size-value :64-bit :long-long :32-bit :long) (1+ start)))
      (#\q (values :long-long (1+ start)))
      (#\C (values :unsigned-char (1+ start)))
      (#\I (values :unsigned-int (1+ start)))
      (#\S (values :unsigned-short (1+ start)))
      (#\L (values (register-size-value :64-bit :unsigned-long-long :32-bit :unsigned-long) (1+ start)))
      (#\Q (values :unsigned-long-long (1+ start)))
      (#\f (values :float (1+ start)))
      (#\d (values :double (1+ start)))
      (#\B (values :bool (1+ start)))
      (#\v (values :void (1+ start)))
      (#\* (values :string (1+ start)))
      (#\@ (values :pointer (1+ start)))     ;; objc object pointer
      (#\# (values :pointer (1+ start)))     ;; objc class pointer
      (#\: (values :pointer (1+ start)))     ;; objc selector
      
      (#\? (parse-unknown-type string :start start))

      (#\[ (parse-array string :start start))
      (#\{ (parse-struct-or-union string :start start :kind :struct))
      (#\( (parse-struct-or-union string :start start :kind :union))
      (#\b (parse-bitfield string :start start))
      (#\^ (let ((*indirection-level* (1+ *indirection-level*)))
	     (parse-pointer string :start start)))

      ((#\r #\n #\N #\o #\O #\R #\V) (decode-objc-type string :start (1+ start)))))) ;; ignore these qualifiers
	      
(defun parse-method-name (string)
  (let ((basic-name (if (find #\: string)
			(subseq string 0 (1+ (position #\: string)))
			string)))
    (list* (lispize-name basic-name)
	   (let* ((p1 (position #\: string))
		  (p2))
	     (loop while p1
		do 
		  (setq p2 (unless (>= p1 (1- (length string)))
			     (position #\: string :start (1+ p1))))
		when p2
		collect
		  (if (= (1+ p1) p2) ;; some colon sequences in method names do not have a name
		      (symbol-name (gensym))
		      (lispize-name (subseq string (1+ p1) (and p2 (1+ p2)))))
		  do
		    (setq p1 p2))))))

(defun capital-char? (char)
  (<= 65 (char-code char) #.(+ 65 (1- 26))))

(defun lispize-name (name)
  (let ((string (make-array 1000 :element-type 'character :adjustable t :fill-pointer 0))
	(capital? nil))
    (loop for char across name
       when (capital-char? char)
       do (vector-push-extend #\- string)
	 (vector-push-extend char string)
	 (setq capital? t)
       when (char= char #\:)
       do (return string)
       unless capital?
       do (vector-push-extend (char-upcase char) string)
       when capital?
       do (setq capital? nil))
    string))



(defun write-package-file (class path)
  (let ((methods (class-copy-method-list class)))
    (with-open-file (stream path :direction :output :if-does-not-exist :create :if-exists :append)
      (loop for method in methods
	 initially
	   (terpri stream)
	   (princ "(export (list " stream)
	 unless (char= #\_ (char (objc-method-name method) 0))
	 do (princ "#:" stream)
	   (format stream "|~A|" (objc-method-selector method))
	   (terpri stream)
	 finally (princ ") :ns)" stream)))))

#+old
(defun process-arg (arg stream)
  (terpri stream)
  (princ "    " stream)
  (with-slots (name type) arg
    (format stream "'~S" type)
    (princ " " stream)
    (when (eq type :pointer)
      (princ "(objc-object-id " stream))
    (when (eq type :char) ;; assuming :char really means :bool (they would use :int otherwise)
      (princ "(if " stream))
    ;;(when (and (listp type) (or (eq :struct (car type)) (eq :union (car type))))
      ;;(princ "'" stream))
    (princ (string-downcase name) stream)
    (when (eq type :char) ;; assuming :char really means :bool (they would use :int otherwise)
      (princ " 1 0)" stream))
    (when (eq type :pointer)
      (princ ")" stream))))

#+OLD
(defun process-method (method stream)
  (terpri stream)
  (with-slots (name return-type args) method
    ;; guessing that in objc same selector different class will have the same signature
    (let ((sel-sym (intern (objc-method-selector method) :ns)))
      (unless (gethash sel-sym *wrapped-selector-table*)
	(princ "(defun " stream)
	(format stream "~S" sel-sym)
	(setf (gethash sel-sym *wrapped-selector-table*)
	      sel-sym)
	(princ " (thing" stream)
	(loop for argz on args by #'cdr
	   initially (when argz (princ " " stream))
	   do (with-slots (name) (car argz)
		(princ (string-downcase name) stream))
	   when (cdr argz)
	   do (princ " " stream))
	
	(princ ")" stream)
	(terpri stream)
	(when (eq return-type :char) ;; we'll assume returning a char actually means a bool
	  (princ "  (if (= 0" stream))
	(princ "  (send thing @(" stream)
	(princ (objc-method-selector method) stream)
	(princ ") " stream)
	(format stream "'~S" return-type)

	(loop for argz on args by #'cdr
	   initially (when argz (princ " " stream))
	   do (process-arg (car argz) stream)
	   when (cdr argz)
	   do (princ " " stream))

	(princ ")" stream)
	(when (eq return-type :char) ;; we'll assume returning a char actually means a bool
	  (princ ") nil t)" stream))
	(princ ")" stream)
	(terpri stream)))))

(defun process-arg-types-and-return-type (args ret-type stream)
  (princ "((" stream)
  (labels ((foo (type)
	     (princ " " stream)
	     (if (listp type)
		 (progn (princ "(" stream)
			(loop for typ in type
			   do (foo typ))
			(princ ")" stream))
		 (if (stringp type)
		     (princ type stream)
		     (format stream "~S" type)))))
    (loop for arg in args
       do (foo (objc-method-argument-type arg)))
  (princ ") " stream)
  (foo ret-type)
  (princ ")" stream)))

(defun process-arg-names1 (args stream)
  (loop for argz on args by #'cdr
     initially (when argz (princ " " stream))
     do (with-slots (name) (car argz)
	  (princ (string-downcase name) stream))
     when (cdr argz)
     do (princ " " stream)))

(defun process-arg-names2 (args stream)
  (loop for argz on args by #'cdr
     initially (when argz (princ " " stream))
     do (with-slots (name type) (car argz)
	  (when (or (eq type :pointer) (and (consp type) (eq (car type) :pointer)))
	    (princ "(objc-object-id " stream))
	  (when (eq type :char)
	    (princ "(if " stream))
	  (princ (string-downcase name) stream)
	  (when (eq type :char)
	    (princ " 1 0)" stream))
	  (when (or (eq type :pointer) (and (consp type) (eq (car type) :pointer)))
	    (princ ")" stream)))
     when (cdr argz)
     do (princ " " stream)))

(defun process-method (method stream &key (internal? nil))
  (when (or internal? (not (char= #\_ (char (objc-method-selector method) 0))))
    (terpri stream)
    (with-slots (name return-type args) method
      ;; guessing that in objc same selector different class will have the same signature
      (let ((sel-sym (intern (objc-method-selector method) :ns)))
	(unless (gethash sel-sym *wrapped-selector-table*)
	  (push (objc-method-selector method) *export-list*)
	  (princ "(defun " stream)
	  (format stream "ns::|~A|" (objc-method-selector method))
	  (setf (gethash sel-sym *wrapped-selector-table*)
		sel-sym)
	  (princ " (thing" stream) (process-arg-names1 args stream) (princ ")" stream)
	  (terpri stream)
	  (when (eq return-type :char) ;; we'll assume returning a char actually means a bool
	    (princ "  (if (= 0" stream))
	  (princ "  (let ((message-lambda " stream)
	  (terpri stream)
	  (princ "         (make-message-lambda @(" stream)
	  (princ (objc-method-selector method) stream)
	  (princ ") " stream) (process-arg-types-and-return-type args return-type stream) (princ "))) " stream)
	  (terpri stream)
	  (princ "   (funcall message-lambda (objc-object-id thing) " stream) (process-arg-names2 args stream) (princ "))" stream)
	  (when (eq return-type :char) ;; we'll assume returning a char actually means a bool
	    (princ ") nil t)" stream))
	  (princ ")" stream)
	  (terpri stream))))))

(defun process-class (class stream &key (with-internals? nil))
  (let ((methods (class-copy-method-list class)))
    (loop for method in methods
       do (process-method method stream :internal? with-internals?))))

(defun write-method-bindings (classes path extras &key (with-internals? nil))
  ;; for some reason there still sometimes exist methods which don't show up in class-copy-method-list
  ;; those method pointers go in extras       
  (with-open-file (stream path :direction :output :if-exists :supersede :if-does-not-exist :create)
    (princ "(in-package :clui)" stream)
    (terpri stream)
    (princ "(named-readtables:in-readtable :objc-readtable)" stream)
    (terpri stream)
    (terpri stream)
    
    (loop for class in classes
       for i from 0
	 do (print i)
       do (process-class class stream :with-internals? with-internals?))

    (loop for m in extras
       do (process-method (create-objc-method m) stream))

    (loop for export in *export-list*
       initially (princ "(export (list " stream)
       do (terpri stream)
	 (princ "          'ns::|" stream)
	 (princ export stream)
	 (princ "|" stream)
       finally (princ ") :ns)" stream))))
	 

(defun wrap-ns (&key (with-internals? nil))
  (clrhash *wrapped-selector-table*)
  (setq *export-list* nil)
  (write-method-bindings (list #@NSObject
			       #@NSBundle
			       #@NSNumber
			       #@NSApplication #@NSRunningApplication #@NSThread #@NSEvent #@NSUserDefaults
			       #@NSScreen
			       #@NSMenu
			       #@NSMenuItem
			       #@NSCursor
			       #@NSString
			       #@NSAttributedString
			       #@NSMutableString
			       #@NSArray
			       #@NSNotificationCenter
			       #@NSTrackingArea
			       ;;#@NSWorkspace #@NSWorkspaceOpenConfiguration #@NSAppKitVersion
			       ;;@NSUserActivity #@NSUserActivityRestoring
			       ;;#@NSSharingService #@NSSharingServicePicker #@NSPreviewRepresentableActivityItem
			       #@NSView #@NSControl #@NSCell #@NSActionCell #@NSSplitView #@NSStackView #@NSTabView
			       #@NSWindowController
			       #@NSViewController
			       #@NSScrollView #@NSScroller #@NSClipView #@NSRulerView #@NSRulerMarker
			       #@NSTextView ;; #@NSTextViewDelegate
			       #@NSWindow
			       #@NSColor #@NSColorList
			       #@NSScreen
			       #@NSGraphicsContext #@NSBezierPath
			       #@NSDate
			       #@MTKView
			       #@NSTouchBar
			       #@NSPasteboard
			       #@CALayer
			       #@CAMetalLayer)
			 "~/clui/cocoa/ns-bindings.lisp"
			 (list (class_getClassMethod #@NSThread @(detachNewThreadSelector:toTarget:withObject:))
			       (class_getClassMethod #@NSApplication @(sharedApplication))
			       ;;(class_getClassMethod #@NSEvent @(addLocalMonitorForEventsMatchingMask:handler:)) ;; bogus
			       (class_getClassMethod #@NSUserDefaults @(standardUserDefaults))
			       (class_getClassMethod #@NSNotificationCenter @(defaultCenter))
			       (class_getClassMethod #@NSRunningApplication @(currentApplication))
			       (class_getClassMethod #@NSBezierPath @(strokeRect:))
			       (class_getInstanceMethod #@NSGraphicsContext @(flushGraphics))
			       (class_getClassMethod #@NSGraphicsContext @(graphicsContextWithWindow:))
			       (class_getClassMethod #@NSColor @(whiteColor))
			       (class_getClassMethod #@NSColor @(clearColor))
			       (class_getClassMethod #@NSDate @(distantPast))
			       (class_getClassMethod #@NSDate @(distantFuture))
			       (class_getClassMethod
				#@NSEvent @(otherEventWithType:location:modifierFlags:timestamp:windowNumber:context:subtype:data1:data2:))
			       (class_getClassMethod
				#@NSEvent @(mouseLocation))
			       (class_getClassMethod #@NSAutoreleasePool @(new))
			       (class_getClassMethod #@NSAutoreleasePool @(release))
			       (class_getInstanceMethod #@CALayer @(setContentsScale:))
			       (class_getClassMethod #@NSBundle @(bundleWithPath:))
			       (class_getClassMethod #@NSScreen @(screens))
			       (class_getClassMethod #@NSWindow @(windowNumberAtPoint:belowWindowWithWindowNumber:))

			       (class_getClassMethod #@NSMenuItem @(separatorItem))
			       (class_getClassMethod #@NSMenu @(setMenuBarVisible:))
			       ;;(class_getClassMethod #@NSWindow @(setTouchBar:))
			       #+NIL(class_getInstanceMethod #@NSAttributedString @(initWithAttributedString:))
			       (class_getClassMethod #@NSArray @(array))
			       (class_getClassMethod #@NSCursor @(arrowCursor))
			       (class_getClassMethod #@NSCursor @(IBeamCursor))
			       (class_getClassMethod #@NSCursor @(crosshairCursor))
			       (class_getClassMethod #@NSCursor @(pointingHandCursor))
			       (class_getClassMethod #@NSCursor @(resizeLeftRightCursor))
			       (class_getClassMethod #@NSCursor @(resizeUpDownCursor))
			       (class_getClassMethod #@NSCursor @(closedHandCursor))
			       (class_getClassMethod #@NSCursor @(operationNotAllowedCursor))
			       (class_getClassMethod #@NSPasteboard @(pasteboardWithName:))
			       )
			 :with-internals? with-internals?))

