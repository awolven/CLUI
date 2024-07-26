;; -*- Mode: Lisp; -*-
;; ---------------------------------------------------------------------------
;;     Title: *features* for noffi
;;   Created: 2024-06-19
;;    Author: Gilbert Baumann <gilbert@lazurite.local>
;;   License: MIT style (see below)
;; ---------------------------------------------------------------------------
;;;  (c) copyright 2024 by Gilbert Baumann

;;  Permission is hereby granted, free of charge, to any person obtaining
;;  a copy of this software and associated documentation files (the
;;  "Software"), to deal in the Software without restriction, including
;;  without limitation the rights to use, copy, modify, merge, publish,
;;  distribute, sublicense, and/or sell copies of the Software, and to
;;  permit persons to whom the Software is furnished to do so, subject to
;;  the following conditions:
;; 
;;  The above copyright notice and this permission notice shall be
;;  included in all copies or substantial portions of the Software.
;; 
;;  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. 
;;  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;  CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;  TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;  SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(in-package :cl-user)

(pushnew :noffi *features*)

;;; The Operating System

;; Tests for macos
#+(OR (AND CCL DARWIN)
      (AND SBCL DARWIN))
(pushnew :noffi-darwin *features*)

#+(OR (AND CCL WINDOWS) (AND SBCL WIN32) (AND EXCL MSWINDOWS))
(pushnew :noffi-windows *features*)

;; Tell me, when you find that funny 36-bit platform still in use. Or
;; when you found that 128-bit as I would want some as well.
(pushnew (if (> most-positive-fixnum (ash 1 32))
             :noffi-64-bit-target
             :noffi-32-bit-target)
         *features*)


;;;; The Target ABI

;; This is too corse as it won't catch ARM.

#+(OR (AND CCL 64-BIT-TARGET (OR DARWIN LINUX))
      (AND SBCL 64-BIT (OR DARWIN LINUX)))
(pushnew :noffi-amd64-sysv-abi *features*)

#+(OR (AND CCL 64-BIT-TARGET WINDOWS)
      (AND SBCL 64-BIT WIN32)
      (AND EXCL MSWINDOWS 64BIT))
(pushnew :noffi-amd64-ms-abi *features*)


;;;;

(if nil
    (pushnew :NOFFI-OBJC *features*)
    (setq *features* (remove :NOFFI-OBJC *features*)))

