(in-package :asdf-user)

(defsystem "noffi"
  :author "Gilbert Baumann <gilbert@bauhh.de>"
  :license "MIT"
  :components
  ((:module "noffi-host"
            :pathname ""
            :components
            ((:file "noffi-host")))
   (:module
    "src"
    :serial t
    :depends-on ("noffi-host" "clex" "lalr")
    :components
    (#+CCL
     (:file "patch-ccl")
     (:file "file")
     (:file "compiler-warn")
     (:file "package")
     (:file "features")
     #+SBCL
     (:file "patch-sbcl")
     (:file "forward")
     #+EXCL (:file "lispdep-excl")
     (:file "lispdep")
     (:file "util")
     (:file "string-table")
     ;;
     (:file "abi")
     ;;
     (:file "define-grammar")
     (:file "lexer")
     (:file "cpp")
     (:file "parsing")
     (:file "grammar")
     (:file "adt")
     (:file "comp")
     #+EXCL (:file "backend-excl")
     (:file "runtime")
     (:file "syntax")
     ;;
     (:file "abi-amd64-sysv")
     (:file "abi-amd64-mingw64")
     ;;
     (:file "abi-amd64-sysv-cc")
     (:file "abi-amd64-ms-cc")
     ;;
     (:file "config")
     (:file "noffi-util")))
   (:module
    "clex"
    :pathname #.(make-pathname :directory '(:relative "other" "clex" "src"))
    :components
    ((:file "clex")))
   (:module
    "lalr"
    :pathname #.(make-pathname :directory '(:relative "other" "lalr"))
    :components
    ((:file "lalr")))))
