;; -*- Mode: Lisp; -*-
;; ---------------------------------------------------------------------------
;;     Title: Abstract ABI datatype
;;   Created: 2023-01-26
;;    Author: Gilbert Baumann <gilbert@bauhh.de>
;;   License: MIT style (see below)
;; ---------------------------------------------------------------------------
;;  (c) copyright 2023 by Gilbert Baumann

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

(defclass abi () ())

(defvar *abi*)

(defgeneric abi-evaluate-character-constant (abi string)
  (:documentation "Given a character constant that is already deescaped yield its value."))

(defgeneric abi-size_t-type (abi))
(defgeneric abi-ptrdiff_t-type (abi))
(defgeneric abi-integer-types-1 (abi))
(defgeneric abi-floating-point-types-1 (abi))
(defgeneric abi-pointer-type-size-align-1 (abi type))

(defgeneric abi-basic-type-specifier-lexemes (abi)
  (:method-combination append))

(defgeneric abi-maybe-translate-base-type (abi type-specifiers)
  (:method-combination or))

(defmethod abi-maybe-translate-base-type or ((abi abi) type-specifiers)
  (declare (ignorable abi type-specifiers)))


;;;; Trampolins using *abi*

;; ### Maybe we do this like (FOO .. &optional (abi *abi*))

(defun evaluate-character-constant (string) 
  "Given a character constant that is already deescaped yield its value."
  (abi-evaluate-character-constant *abi* string))
       
(defun size_t-type ()
  "The basic type that is used for `size_t', the result of `sizeof`, `alignof`, and `offsetof`."
  (abi-size_t-type *abi*))

(defun ptrdiff_t-type ()
  "The basic type that is used for `ptrdiff_t', the result computing a pointer difference."
  (abi-ptrdiff_t-type *abi*))

(defun abi-integer-types (&optional (abi *abi*))
  (abi-integer-types-1 abi))

(defun abi-floating-point-types (&optional (abi *abi*))
  (abi-floating-point-types-1 abi))

(defun abi-pointer-type-size-align (type &optional (abi *abi*))
  (abi-pointer-type-size-align-1 abi type))


;;; ISO C

(defclass iso-c17-mixin () ())

(defmethod abi-basic-type-specifier-lexemes append ((abi iso-c17-mixin))
  (declare (ignorable abi))
  '((:void                    "void")
    (:char                    "char")
    (:short                   "short")
    (:int                     "int")
    (:long                    "long")
    (:float                   "float")
    (:double                  "double")
    (:signed                  "signed")
    (:unsigned                "unsigned")
    (:Bool                    "_Bool")
    (:Complex                 "_Complex")))

(defclass gcc-abi-mixin () ())

(defmethod abi-basic-type-specifier-lexemes append ((abi gcc-abi-mixin))
  '((:__int128                "__int128" "__int128__")
    (:int-128                 "__int128_t")
    (:unsigned-int-128        "__uint128_t")
    ;;
    (:float-16                "_Float16")
    (:float-32                "_Float32") 
    (:float-64                "_Float64")
    (:float-128               "_Float128" "__float128")
    (:decimal-32              "_Decimal32")
    (:decimal-64              "_Decimal64")
    (:decimal-128             "_Decimal128")
    ;;
    (:__builtin_va_list       "__builtin_va_list")))

(defmethod abi-maybe-translate-base-type or ((abi gcc-abi-mixin) type-specifiers)
  (cadr (assoc type-specifiers
               '(((:float-16) :float-16)
                 ((:float-32) :float-32)
                 ((:float-64) :float-64)
                 ((:float-128) :float-128)
                 ((:complex :float-16) :complex-float-16)
                 ((:complex :float-32) :complex-float-32)
                 ((:complex :float-64) :complex-float-64)
                 ((:complex :float-128) :complex-float-128)
                 ((:decimal-16) :decimal-16)
                 ((:decimal-32) :decimal-32)
                 ((:decimal-64) :decimal-64)
                 ((:decimal-128) :decimal-128)
                 ((:complex :decimal-16) :complex-decimal-16)
                 ((:complex :decimal-32) :complex-decimal-32)
                 ((:complex :decimal-64) :complex-decimal-64)
                 ((:complex :decimal-128) :complex-decimal-128))
               :test #'multi-set-equal)))

(defun multi-set-equal (xs ys)
  (and (every (lambda (x) (eql (count x xs) (count x ys))) xs)
       (every (lambda (y) (eql (count y xs) (count y ys))) ys)))

(defclass msvc-abi-mixin () ())

(defmethod abi-basic-type-specifier-lexemes append ((abi msvc-abi-mixin))
  '((:__int8                  "int8"  "__int8")
    (:__int16                 "int16" "__int16")
    (:__int32                 "int32" "__int32")
    (:__int64                 "int64" "__int64")))

