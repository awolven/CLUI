(defpackage :noffi-ci
  (:use)
  (:export
   ;; Statements
   #:LABEL #:CASE #:DEFAULT #:NOP #:EXPR #:IF #:SWITCH #:WHILE #:DO-WHILE #:FOR #:FOR-DECL #:GOTO
   #:CONTINUE #:BREAK #:RETURN #:BEGIN
   ;;
   #:DECL))

(defpackage :noffi
  (:use
   :common-lisp
   #+SBCL :sb-gray
   ;;#+CMU :clim-lisp
   #+CMU :ext
   #+COMMON-LISP-WORKS :stream
   ;;
   ;; :named-readtables
   )
  (:local-nicknames ("!" "NOFFI-CI"))
  (:shadow #:declaration)
  #+CCL (:import-from :CCL #:QUIT)
  #+SBCL (:import-from :SB-EXT #:QUIT)
  ;;
  (:export #:noffi-syntax #:clet #:c-addr-of #:pkg-use #:c-coerce #:defcfun
           #:size-of-typ #:c-aref #:clet&
           #:get-c-string
           #:get-native-utf16-string
           #:int-ptr
           #:use-library
           #:use-include
           #:c-compile-file
           #:c-compile-file-pathname
           #:pkg-use)
  #+(OR CCL ECL CLISP)
  (:import-from #+CCL :ccl #+ECL :gray #+CLISP :gray
                #:fundamental-binary-input-stream
                #:fundamental-binary-output-stream
                #:fundamental-binary-stream
                #:fundamental-character-input-stream
                #:fundamental-character-output-stream
                #:fundamental-character-stream
                #:fundamental-input-stream
                #:fundamental-output-stream
                #:fundamental-stream
                #:stream-advance-to-column
                #:stream-clear-input
                #:stream-clear-output
                ;; #:stream-file-position
                #:stream-finish-output
                #:stream-force-output
                #:stream-fresh-line
                #:stream-line-column
                #+CCL        #:stream-line-length
                #:stream-listen
                #:stream-peek-char
                #:stream-read-byte
                #:stream-read-char
                #:stream-read-char-no-hang
                #:stream-read-line
                ;; #:stream-read-sequence
                #:stream-start-line-p
                #:stream-terpri
                #:stream-unread-char
                #:stream-write-byte
                #:stream-write-char
                ;; #:stream-write-sequence
                #:stream-write-string
                )
  (:import-from :de.bauhh.lalr
                #:$START
                ;;
                #:lalr-table
                #:cons-lalr-table
                #:lalr-table-p
                #:lalr-table-topcat
                #:lalr-table-states
                ;;
                #:lalr-state
                #:make-lalr-state
                #:lalr-state-p
                #:lalr-state-name
                #:lalr-state-transitions
                ;;
                #:lalr-action
                #:make-lalr-action
                #:lalr-action-p
                #:lalr-action-goto
                #:lalr-action-categories
                ;;
                #:reduce-action
                #:make-reduce-action
                #:reduce-action-p
                #:reduce-action-npop
                #:reduce-action-function
                ;;
                #:shift-action
                #:make-shift-action
                #:shift-action-p))

(defpackage :noffi-c
  ;; (:nicknames :c)
  (:use))

(defpackage :noffi-ffi
  ;; (:nicknames :c)
  (:use))

(defpackage :noffi-user
  (:use :noffi :cl))
