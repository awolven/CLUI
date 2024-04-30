
(pushnew :noffi *features*)

;;; The Operating System

#+(OR (AND CCL DARWIN) (AND SBCL DARWIN))
(pushnew :noffi-darwin *features*)

#+(OR (AND CCL WINDOWS) (AND SBCL WIN32))
(pushnew :noffi-windows *features*)

;;; The word size

#+(OR (AND CCL 64-BIT-TARGET) (AND SBCL 64-BIT))
(pushnew :noffi-64-bit-target *features*)
#+(OR (AND CCL 32-BIT-TARGET) (AND SBCL (NOT 64-BIT)))
(pushnew :noffi-32-bit-target *features*)



;;;; The Target ABI

#+(OR (AND CCL 64-BIT-TARGET (OR DARWIN LINUX))
      (AND SBCL 64-BIT (OR DARWIN LINUX)))
(pushnew :noffi-amd64-sysv-abi *features*)

#+(OR (AND CCL 64-BIT-TARGET WINDOWS)
      (AND SBCL 64-BIT WIN32))
(pushnew :noffi-amd64-ms-abi *features*)


;;;;

(pushnew :NOFFI-OBJC *features*)

