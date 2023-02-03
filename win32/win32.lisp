(in-package :noffi)

(eval-when (:compile-toplevel)
  (noffi-syntax t)
  (defparameter *last-good-token* nil))


(eval-when (:compile-toplevel)
  #_{#include "dependencies.i"}
  )
