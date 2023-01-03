(in-package :cl-user)

(defpackage :abstract-os
  (:use :cl)
  #-darwin
  (:import-from :noffi
		#:copy-ptr
		#:%cons-ptr
		#:c-sizeof-type
		#:c-addr-of
		#:c-cast
		#:with-stack-allocated-structure
		#:offset-of
		#:ptr-value
		#:cval-value
		#:ptr-offset

		#:noffi-syntax #:clet #:c-addr-of #:pkg-use #:c-coerce #:defcfun))
