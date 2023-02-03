(in-package :clui)
(named-readtables:in-readtable :objc-readtable)

(defun make-other-event-with-type (type point modifier-flags timestamp window-number context subtype
				   &optional (data1 0) (data2 0))
  (ns:|otherEventWithType:location:modifierFlags:timestamp:windowNumber:context:subtype:data1:data2:|
      #@NSEvent type point modifier-flags timestamp
      window-number (if context
			(objc-object-id context)
			nil)
      subtype data1 data2))

(defun post-empty-event (app)
  (let ((event (make-other-event-with-type
		NSEventTypeApplicationDefined (list 'ns::y 0.0d0 'ns::x 0.0d0) 0 0.0d0 0 nil 0 0 0)))
    (ns:|postEvent:atStart:| app event t)))
