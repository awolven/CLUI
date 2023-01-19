(in-package :abstract-os)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (ignore-errors (find-package :ns))
    (defpackage :ns)))

(export '(ns::|alloc|
	  ns::|init|
	  ns::|addLocalMonitorForEventsMatchingMask:handler:|)
	:ns)
