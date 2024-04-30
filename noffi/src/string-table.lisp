;; -*- Mode: Lisp; -*-
;; ---------------------------------------------------------------------------
;;     Title: A very simple string hashtable serving as a cache in front of INTERN
;;   Created: 2022-12-07
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

(locally
    (declaim (optimize (safety 1) (speed 3)))

(declaim (inline string-table-count string-table-size string-table-table string-table-get))

(defstruct (string-table (:print-function print-string-table)
                         (:constructor cons-string-table (size table)))
  (count 0   :type fixnum)
  (size 0    :type fixnum)
  (table nil :type simple-vector))

(defun print-string-table (table stream depth)
  (declare (ignore depth))
  (print-unreadable-object (table stream :type t :identity t)
    (format stream "~D/~D" (string-table-count table) (string-table-size table)))
  table)

(defun make-string-table (&key (size 1003))
  (cons-string-table size (make-array size :initial-element nil)))

(declaim (inline cjb2))
(defun cjb2 (string start end)
  (declare (type (simple-array character (*)) string)
           (type fixnum start end)
           (optimize (speed 3) (safety 1)))
  (let ((hash 5381))
    (declare (type (unsigned-byte 24) hash))
    ;; We compute the hash as 24-bit value to keep the immediate result
    ;; within 30 bits in the hope that fixnums can be used even on 32-bit
    ;; hosts.
    (loop for i of-type fixnum from start below end
          do (setq hash (logand #xFFFFFF (+ (the (unsigned-byte 30) (* 33 hash))
                                            (char-code (schar string i)))))
          finally (return hash))))

(defmacro with-string-table-hash-bucket ((bucket table string start end) &body body)
  (multiple-value-bind (g.table g.string g.start g.end g.hash g.storage)
      (values (gensym "TABLE.") (gensym "STRING.") (gensym "START.") (gensym "END.")
              (gensym "HASH.") (gensym "STORAGE."))
    `(let* ((,g.table ,table)
            (,g.storage (string-table-table ,g.table))
            (,g.string ,string)
            (,g.start ,start)
            (,g.end ,end)
            (,g.hash (mod (cjb2 ,g.string ,g.start ,g.end) (string-table-size ,g.table))))
       (declare (type string-table ,g.table)
                (type simple-string ,g.string)
                (type fixnum ,g.start ,g.end ,g.hash)
                (type simple-vector ,g.storage))
       (symbol-macrolet ((,bucket (svref ,g.storage ,g.hash)))
         ,@body))))

(defmacro with-string-table-hash-bucket-and-cell ((bucket cell table string start end) &body body)
  `(with-string-table-hash-bucket (,bucket ,table ,string ,start ,end)
     (let ((len (- end start)))
       (declare (fixnum len))
       (let ((,cell (dolist (q bucket nil)
                      (let ((key (car q)))
                        (declare (type (simple-array character (*)) key))
                        (when (and (= len (length key))
                                   (loop for i of-type fixnum from start below end
                                         for j of-type fixnum from 0
                                         always (char= (schar string i) (schar key j))))
                          (return q))))))
         (declare (type (or null cons) ,cell))
         ,@body))))

(defun string-table-get (table string start end)
  (declare (optimize (speed 3) (safety 1)))
  (declare (type string-table table))
  (with-string-table-hash-bucket-and-cell (bucket cell table string start end)
    (if cell (values (cdr cell) t) (values nil nil))))

(defun (setf string-table-get) (new-value table string start end)
  (with-string-table-hash-bucket-and-cell (bucket cell table string start end)
    (cond (cell
           (setf (cdr cell) new-value))
          (t
           (cond ((> (* 4 (string-table-count table))
                     (* 2 (string-table-size table)))
                  (resize-string-table table (max 5 (ceiling (+ (string-table-size table)
                                                                (ceiling (string-table-size table) 2)))))
                  (setf (string-table-get table string start end) new-value))
                 (t
                  (incf (string-table-count table))
                  (cdar (push (cons (subseq string start end) new-value)
                              bucket))))))))

(defun resize-string-table (table new-size)
  ;; Hmm, make `new-size' a prime? Is that important?
  (let* ((old-storage (string-table-table table))
         (new-storage (make-array new-size :initial-element nil)))
    (loop for bucket across old-storage do
          (loop for q in bucket do
                (let* ((index (mod (cjb2 (car q) 0 (length (car q))) new-size)))
                  (push q (svref new-storage index)))))
    (setf (string-table-size table) new-size
          (string-table-table table) new-storage)
    table))

)                                       ;locally
