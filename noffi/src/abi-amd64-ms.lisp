;; -*- Mode: Lisp; -*-
;; ---------------------------------------------------------------------------
;;     Title: Microsoft AMD64 ABI
;;   Created: 2022-12-18
;;    Author: Gilbert Baumann <gilbert@bauhh.de>
;;   License: MIT style (see below)
;; ---------------------------------------------------------------------------
;;;  (c) copyright 2022 by Gilbert Baumann

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

(in-package :noffi)

;; References:
;;
;;     https://learn.microsoft.com/en-us/cpp/build/x64-calling-convention?view=msvc-170
;;     

(defclass abi-amd64-ms (iso-c17-mixin
                        msvc-abi-mixin
                        abi)
  ())

(defvar +abi-amd64-ms+ (make-instance 'abi-amd64-ms))

(defmethod abi-evaluate-character-constant ((abi abi-amd64-ms) string)
  ;; This is very perplexing. And we need to sign extend not the whole
  ;; literal, but that what was given.
  (declare (ignorable abi))
  (let ((n (min 4 (length string))))
    (let ((r 0))
      (loop for i from 0 below n
            for k = (1- (- n i))
            do (setf r (dpb (char-code (char string i)) (byte 8 (* 8 k)) r)))
      (sldb (byte (if (= n 1) 8 32) 0) r))))

(defmethod abi-integer-types-1 ((abi abi-amd64-ms))
  (declare (ignorable abi))
  ;; our name                   size  align signed?     rank    C Name
  ;; ----------------------------------------------------------------------------
  '((:bool                        1      8 nil          -3      "_Bool")

    (:char                        8      8 t            -2      "char")
    (:signed-char                 8      8 t            -2      "signed char")
    (:unsigned-char               8      8 nil          -2      "unsigned char")

    (:short                      16     16 t            -1      "short")
    (:int                        32     32 t            0       "int")
    (:long                       32     32 t            1       "long")
    (:long-long                  64     64 t            2       "long long")

    (:unsigned-short             16     16 nil          -1      "unsigned short")
    (:unsigned-int               32     32 nil          0       "unsigned int")
    (:unsigned-long              32     32 nil          1       "unsigned long")
    (:unsigned-long-long         64     64 nil          2       "unsigned long long")

    (:int-128                   128    128 t            3       "__int128")
    (:unsigned-int-128          128    128 nil          3       "unsigned __int128") ))

(defmethod abi-size_t-type ((abi abi-amd64-ms)) :unsigned-long-long)
(defmethod abi-ptrdiff_t-type ((abi abi-amd64-ms)) :long-long)

(defmethod abi-floating-point-types-1 ((abi abi-amd64-ms))
  (declare (ignorable abi))
  ;; our name                  size  align      Lisp Type       Rank    C Name
  ;; ------------------------------------------------------------------------------
  '((:float                      32     32      single-float    -1      "float")
    (:double                     64     64      double-float    0       "double")
    ;;### Here we are bust!
    (:long-double                64     64      double-float    1       "long double")
    ))

(defmethod abi-pointer-type-size-align-1 ((abi abi-amd64-ms) type)
  (declare (ignorable abi))
  (declare (ignore type))
  (values 64 64))

(defmethod abi-basic-type-specifier-lexemes append ((abi abi-amd64-ms))
  (declare (ignorable abi))
  '())

(defmethod abi-maybe-translate-base-type or ((abi msvc-abi-mixin) type-specifiers)
  (cadr (assoc type-specifiers
               '(((:__int8) :char)
                 ((:signed :__int8) :char)
                 ((:unsigned :__int8) :unsigned-char)
                 ((:__int16) :short)
                 ((:signed :__int16) :short)
                 ((:unsigned :__int16) :unsigned-short)
                 ((:__int32) :int)
                 ((:signed :__int32) :int)
                 ((:unsigned :__int32) :unsigned-int)
                 ((:__int64) :long-long)
                 ((:signed :__int64) :long-long)
                 ((:unsigned :__int64) :unsigned-long-long)
                 ((:__int128) :int-128)
                 ((:signed :__int128) :int-128)
                 ((:unsigned :__int128) :unsigned-int-128))
               :test #'multi-set-equal)))

