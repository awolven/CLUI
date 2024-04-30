;; -*- Mode: Lisp; -*-
;; ---------------------------------------------------------------------------
;;     Title: Minimal noffi example to use libcurl
;;   Created: 2024-02-18
;;    Author: Gilbert Baumann <gilbert@bauhh.de>
;;   License: MIT style (see below)
;; ---------------------------------------------------------------------------
;;  (c) copyright 2024 by Gilbert Baumann

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


;;;; Overview

;; This is a minimal example to show how to interface to libcurl. It is
;; assumed that pkg-config knowns about libcurl, it is beyond the scope of
;; this example to show how to install libcurl with your system.

;; This example is minimal and lacks any error handling and is for
;; illustration purposes only.


;;;; Implementation

(in-package :noffi-user)

;; Make syntax known
(noffi:noffi-syntax)

;; Use pkg-config to get at include and library search pathes, include the header
(pkg-use "libcurl"
  "#include <curl/curl.h>")

;; libcurl wants that we call curl_global_init before we start using it.
;; Provide an ENSURE-CURL-INITED routine to do so when needed.

(defvar *curl-inited-p* nil)
(defun ensure-curl-inited ()
  (unless *curl-inited-p*
    (#_curl_global_init #_CURL_GLOBAL_NOTHING)
    (setq *curl-inited-p* t)))

;; libcurl wants that we provide a callback that has a signature like
;; fwrite(3) and receives document data that it got from the HTTP server. We
;; use DEFCFUN to define such a callback, it's like DEFUN but with foreign
;; types mentioned for the result and the parameters.

(defcfun (easy-write #_<size_t>) ((buffer #_<char*>)
                                  (size #_<size_t>)
                                  (nitems #_<size_t>)
                                  (outstream #_<void*>))
  (declare (ignore outstream))
  ;; We just copy the data to *standard-output* assuming a plain 8-bit encoding.
  (let ((total (* size nitems)))
    (dotimes (i total)
      (write-char (code-char (ldb (byte 8 0) (c-aref buffer i)))))
    (force-output)
    total))

;; Routine to make an "easy handle", set the URL, set the write callback, and
;; make it fetch that resource.

(defun curl (&optional (url "https://lispcafe.org/noffi-curl-test.txt"))
  (ensure-curl-inited)
  (let ((handle (#_curl_easy_init)))
    (#_curl_easy_setopt handle #_CURLOPT_URL url)
    (#_curl_easy_setopt handle #_CURLOPT_WRITEFUNCTION easy-write)
    (#_curl_easy_perform handle)))
