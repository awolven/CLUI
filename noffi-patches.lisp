(in-package :noffi)

(defun offset-of (record-type member &optional env)
  (/ (noffi::record-type-member-offset record-type member env) 8))
