; -*- Mode: Lisp; Syntax: Common-Lisp; Package: de.bauhh.cparse; -*-
;; --------------------------------------------------------------------------------------------
;;     Title: A C processor in Common Lisp
;;   Created: 2011-05-25
;;    Author: Gilbert Baumann <gilbert@bauhh.de>
;;   License: MIT style (see below)
;; --------------------------------------------------------------------------------------------

;;  (c) copyright 2011 by Gilbert Baumann

;;  Permission is hereby granted, free of charge, to any person obtaining
;;  a copy of this software and associated documentation files (the
;;  "Software"), to deal in the Software without restriction, including
;;  without limitation the rights to use, copy, modify, merge, publish,
;;  distribute, sublicense, and/or sell copies of the Software, and to
;;  permit persons to whom the Software is furnished to do so, subject to
;;  the following conditions:

;;  The above copyright notice and this permission notice shall be
;;  included in all copies or substantial portions of the Software.

;;  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. 
;;  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;  CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;  TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;  SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(defpackage :de.bauhh.cpp
  (:use :common-lisp
        #+SBCL :sb-gray))

(in-package :de.bauhh.cpp)

;; "cpp is inherently imperfect (in fact, it's downright awful), so the
;; preprocessor design should reflect that" --moon-child


;;;; -- Parameters ----------------------------------------------------------------------------

(defparameter *use-cc-for-includes-p* t
  "Whether to invoke the system's C compilert to process #include <foo.h> style includes.")

(defparameter *cc* "gcc")
;; (defparameter *cc-args* '("-I" "/opt/X11/include"))
(defparameter *cc-args* nil)

(defparameter *standard-include-directories*
  ;; ### is there a good way to query these?
  (list "/usr/local/include/" "/usr/include/"))

(defparameter *cpp-keep-comments-p*
  nil
  "Whether to keep comments from the input. Note however, that
   comments in preprocessor directives are discarded along with the
   directive.")

(defparameter *cpp-preserve-white-space-p*
  nil
  "When non-NIL, white space is preserved. Otherwise a run of multiple
   white space tokens is collapsed to a single space.")

(defparameter *debug-blue-paint*
  nil)

(defparameter *cpp-integer-length*
  32)

(defparameter *use-short-line-directives-p*
  nil)

(defparameter *cpp-default-file-encoding*
  (or
   #+(AND CCL WINDOWS)
   (ccl:make-external-format :character-encoding :utf-8 :line-termination :dos)
   :iso-8859-1))


;;;; -- Overview ------------------------------------------------------------------------------

(eval-when (compile)
  (defparameter +optimize-fast+
    '(optimize (speed 3) (safety 2) (debug 3)))

  '(defparameter +optimize-fast+
    '(optimize (speed 1) (safety 2) (debug 3))))

(eval-when (:compile-toplevel)
  (declaim (optimize (speed 3) (safety 2) (debug 3))))


;;;; -- TODO ----------------------------------------------------------------------------------

;; - At times we turn strings into PP-STREAMS, no point in alloating an 8kB
;;   buffer here.

;; - Why isn't a pp-stream a Gray stream?

;; - Don't process output of the system preprocessor again. For speed
;;   and for semantics.


;;;; -- Some Notes ----------------------------------------------------------------------------

;; The default external format is ISO-8859-1 on purpose. This encoding is
;; transparent to any 8-bit data.


;;;; -- Agenda --------------------------------------------------------------------------------

;; Get this into shape.

;;  0. Arrange for date and time being constant across a file?

;;  1. Run it against any test suite we could find.

;;  2. Ignore the idle mcpp rules of reexpansion

;;  3. Write an constant expression test suite

;;  4. Skip this *bol* business and write a top level loop

;;  5. Implement a #if cache

;;  6. In the top level loop write a recognizer for #ifndef XYZ ... #endif
;;     files and thus be able to touching that files at all on later reinclusion.

;;  7. Use the host cpp for top level #include's.

;;  8. Get line numbers correct, write a test suite for that.

;;  9. Define a sensible API and document it.

;; 10. Publish the thing.

;; Speed will not be our main concern, we later fix that by using the host
;; cpp for bulk material. Correctness is more important than speed.


;;;; -- TODO ----------------------------------------------------------------------------------

;;  - Is *flag-stack* still needed in any way?

;;  - For Objective C we would need #import

;;  0. Big rewrite:
;;
;;     Have a top level loop that does I/O, directive processing and
;;     such. Make this loop fast as most material is just copied over. Find
;;     a way to elegantly do it on an octet basis. **1)
;;
;;     If we are more clever, we could also integrate the C tokenizer
;;     here. There is a good overlap between CPP tokens and C token, thus we
;;     could save the double work.
;;
;;     Also rethink the idea of a hybrid approach, were we let the system
;;     CPP do the main work. The gnu cpp option -dD is just what we need
;;     for that. And that way our speed problems would go away.
;;     

;;  2. Skip *bol-p* busniness. Rather find another way to read
;;     preprocessing directives only from the source.
;;
;;     This correlates to task 0.

;;  1. Hmm under *cpp-keep-comments-p* = NIL and
;;     *cpp-preserve-white-space-p* = NIL:
;;     Collapse any run of white space into a single white space.

;;  3. Also distribute the mcpp test suite and run regression tests.

;; 14. two modes:
;;     a) as a pipe processor outputing C again
;;     b) as a front end to the parser

;; 16. Meaningful error messages.

;; 32. Think about meaingful tracing.

;; 34. We need an expression bonaza to test our evaluator.

;; 36. Have a mode, where line continuations [around wsp] are kept.
;;     This would make debugging complex macros easier.

;; 37. On #if/#else/#endif errors: Continue doing work.

;; 41. What happens if an input file does not end in newline?

;; 48. Warning on overflow in arithmetic exprssion.

;; 51. Sometimes (e.g. on error or such) files don't get closed.

;; 54. ".." is not a token

;; 100. Whitespace within directive may only be #\space or #\tab.

;; 101. Character constants in #if expressions.

;; 102. Garbage at #endif, #else

;; 103. #include recursion

;; 104. Garbage at end of #include

;; 107. My guess is, that we still don't get

;; | If the name of the macro being replaced is found during this scan of the
;; | replacement list (not including the rest of the source file's
;; | preprocessing tokens), it is not replaced. Furthermore, if any nested
;; | replacements encounter the name of the macro being replaced, it is not
;; | replaced. These nonreplaced macro name preprocessing tokens are no
;; | longer available for further replacement even if they are later
;; | (re)examined in contexts in which that macro name preprocessing token
;; | would otherwise have been replaced.

;; Entirely right. There is this bogus reference to "rest of source
;; file's [input]". This situation can only occur, when we have s.th. 
;; like:

;; #define f g
;; #define g(x) x

;; f(f)

;; Becasue the expansion of the first "f" is "g" and we have for
;; rescanning:
;;
;; "g" <file input pointer> "(" "f" ")"
;;
;; We solve that by clearing the flag stack, when doing macro argument
;; expansion. We need to find a tricky example, where this happens not
;; from source file, but from internal lookahead
;;
;; #define quux f(f)
;;
;; -------
;; #define f g
;; #define g(x) x
;; #define quux f(f)
;; f(f);
;; quux;
;;
;; mcpp expands this to: g; f;          => what is not consitent with what Douglas Gwyn wrote.
;; we do g; g; [as does gnu cpp]
;;
;; I believe the latter is saner, while the former comes from careful
;; reading of the spec. Also I believe we might as well stick to the
;; latter.

;; I am not so sure anymore that mcpp is correct here.

;;
;; I want further such examples, that go through another indirection.
;;
;; ------

;; **1)
;;
;; I still believe that fast, I mean really fast streams for Common Lisp
;; would be needed for. We could always Gray Stream them to make them
;; compatible with the rest of Common Lisp. But this is another story
;; altogether.

;;;; We pass all relevant tests now!

;;;; Recursion Handling

;; The article <u6-dnTCOxI4IVt3dRVn-sQ@comcast.com> by Douglas A. Gwyn
;; gives some insight about how to implement proper recursion handling.

;; http://groups.google.com/group/comp.std.c/browse_thread/thread/f295f717987cc3f1/3d0ecedfb2baa1?hl=en&q=
;; http://groups.google.com/group/comp.std.c/msg/003d0ecedfb2baa1?hl=en&dmode=source
;; also http://gcc.gnu.org/ml/gcc-prs/2001-q1/msg00495.html

;;;; Speed

;; When we want to have a fast cpp:

;; - First of all OPEN is for some reasons pretty slow.
;;
;; - Use fully buffered operation.
;;
;; - Most of the material of an include file is not preprocessing
;;   material. Thus have a top level loop, that is mostly copying the
;;   input to the output. Only carefully watching for directives and
;;   for identifiers that name macros.
;;
;;   That is: On the top level run a loop like this:
;;
;; - A clever hack for multiple inclusion of include files. Recognize,
;;   that an include file is fully warped by #ifdef ... #endif and
;;   when seeing that file again, check that #ifdef and skip file
;;   entirely, if possible.
;;
;; - Or: which is more easy: When seeing some #if remember the stream
;;   position of the matching #endif.

;;;; Hybrid

;; We might want to take a hybrid approach. Whenever we hit "#include
;; <foo.h>", we might want to start the system cpp (which then would
;; also get the include path right) and dump it our definition
;; database and then grok the output.
;;
;; This might be preferable, since it gets the include pathes right.
;;
;; Another benefit: If the system include files use some quirks, that
;; are not entirely according to the standard, we let the system cpp
;; handle that. Cons: On Windows it is not standard to have the
;; devoloping environment in place.
;;
;; For now, I want to handle that on my own.

;;;;

(defvar *bol-p*)
(defvar *lookahead*)
(defvar *defs*)


;;;; Macro Processor Environments

(defgeneric c-macro-definition-1 (env name))
(defgeneric (setf c-macro-definition-1) (value env name))
(defgeneric map-macro-definitions-1 (env fun))
(defgeneric header-file-seen-p-1 (env header-file-truename))
(defgeneric (setf header-file-seen-p-1) (new-value env header-file-truename))
(defgeneric map-seen-header-files (env fun))
(defgeneric predefines-known-p (env))
(defgeneric (setf predefines-known-p) (new-value env))

(defun c-macro-definition (name &optional (env *defs*))
  (c-macro-definition-1 env name))

(defun (setf c-macro-definition) (new-value name &optional env)
  (setf (c-macro-definition-1 env name) new-value))

(defun map-macro-definitions (fun &optional (env *defs*))
  (map-macro-definitions-1 env fun))

(defun header-file-seen-p (header-file-truename &optional (env *defs*))
  (header-file-seen-p-1 env header-file-truename))

(defun (setf header-file-seen-p) (new-value header-file-truename &optional (env *defs*))
  "Indicate that we have seen this header file _and_ that it has said #pragma once."
  (setf (header-file-seen-p-1 env header-file-truename) new-value))

(defclass c-macro-environment ()
  ((macro-hash :initform (make-hash-table :test 'eq)
               :reader c-macro-environment-macro-hash)
   (once-list :initform nil
              :accessor c-macro-environment-once-list)
   (predefines-known-p :initform nil
                       :accessor predefines-known-p)))

(defun make-macro-environment ()
  (make-instance 'c-macro-environment))

(defmethod c-macro-definition-1 ((env c-macro-environment) name)
  (gethash name (c-macro-environment-macro-hash env)))

(defmethod (setf c-macro-definition-1) (new-value (env c-macro-environment) name)
  (setf (gethash name (c-macro-environment-macro-hash env)) new-value)
  new-value)

(defmethod map-macro-definitions-1 ((env c-macro-environment) fun)
  (maphash (lambda (k v)
             (when v (funcall fun k v)))
           (c-macro-environment-macro-hash env)))

(defmethod header-file-seen-p-1 ((env c-macro-environment) header-file-truename)
  (member header-file-truename (c-macro-environment-once-list env) :test #'equal))

(defmethod (setf header-file-seen-p-1) (new-value env header-file-truename)
  (if new-value
      (pushnew header-file-truename (c-macro-environment-once-list env) :test #'equal)
      (setf (c-macro-environment-once-list env) (remove header-file-truename (c-macro-environment-once-list env) :test #'equal))))

(defmethod map-seen-header-files ((env c-macro-environment) fun)
  (mapc fun (c-macro-environment-once-list env)))

;;; Chained Preprocessor Environments

(defmethod c-macro-definition-1 ((env cons) name)
  (multiple-value-bind (res win) (c-macro-definition-1 (car env) name)
    (if win (values res win) (c-macro-definition-1 (cdr env) name))))

(defmethod c-macro-definition-1 ((env null) name)
  (declare (ignorable env name))
  (values nil nil))

(defmethod (setf c-macro-definition-1) (new-value (env cons) name)
  (setf (c-macro-definition-1 (car env) name) new-value))

(defmethod map-macro-definitions-1 ((env cons) fun)
  (map-macro-definitions-1 (car env) fun)
  (map-macro-definitions-1 (cdr env)
                           (lambda (k v)
                             (when (and v (not (nth-value 1 (c-macro-definition-1 (car env) k))))
                               (funcall fun k v)))))

(defmethod map-macro-definitions-1 ((env null) fun)
  (declare (ignorable env fun)))

(defmethod header-file-seen-p-1 ((env cons) header-file-truename)
  (or (header-file-seen-p-1 (car env) header-file-truename)
      (header-file-seen-p-1 (cdr env) header-file-truename)))

(defmethod header-file-seen-p-1 ((env null) header-file-truename)
  (declare (ignorable env header-file-truename)))

(defmethod (setf header-file-seen-p-1) (new-value (env cons) header-file-truename)
  (setf (header-file-seen-p-1 (car env) header-file-truename) new-value))

(defmethod map-seen-header-files ((env cons) fun)
  (map-seen-header-files (cdr env) fun)
  (map-seen-header-files (car env) fun))

(defmethod map-seen-header-files ((env null) fun)
  (declare (ignorable env fun)))

(defmethod predefines-known-p ((env cons))
  (or (predefines-known-p (car env))
      (predefines-known-p (cdr env))))

(defmethod predefines-known-p ((env null))
  (declare (ignorable env))
  nil)

(defmethod (setf predefines-known-p) (new-value (env cons))
  (setf (predefines-known-p (car env)) new-value))

;;;

(defparameter *kill-newlines-p* t)
(defparameter *newline-compress-threshold* 5)
(defparameter *files-read* nil
  "This is only for debugging")

(defstruct (pp-stream (:constructor cons-pp-stream) (:print-function print-pp-stream))
  stream
  (buffer nil :type (simple-array character (*)))
  (bufptr 0 :type fixnum)
  (bufsize 0 :type fixnum)
  file
  line
  (tempbuf (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)))

(defun print-pp-stream (object stream depth)
  (declare (ignore depth))
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S" (pp-stream-file object))))

(defparameter *pp-stream-pool*
  nil)

(defun make-pp-stream (stream &key (file (or (ignore-errors (namestring (pathname stream))) "<stdin>")))
  (let ((size (1+ 8192)) res)
    (cond ((setf res (pop *pp-stream-pool*))
           (setf (pp-stream-stream res) stream
                 (pp-stream-bufptr res) 1
                 (pp-stream-bufsize res) 1
                 (pp-stream-file res) file
                 (pp-stream-line res) 1)
           res)
          (t
           (cons-pp-stream
            :stream stream
            :buffer (make-array size :element-type 'character)
            :bufptr 1
            :bufsize 1
            :file file
            :line 1)))))

(defun destroy-pp-stream (stream)
  (push stream *pp-stream-pool*))

(defvar *pp-input-stack* nil)
(defvar *pp-input-stream* nil)
(defvar *pp-char-lookahead-tri* nil)
(defvar *if-stack*)
(defvar *flag-stack* nil)


;;;; -- Utils ---------------------------------------------------------------------------------

(defmacro with-string-collector ((collector) &body body)
  (let ((buf (gensym "BUF."))
        (item (gensym "ITEM.")))
    `(let ((,buf (make-array 10 :element-type 'base-char :fill-pointer 0 :adjustable t)))
      (labels ((,collector (,item)
                 (vector-push-extend ,item ,buf)))
        ,@body)
      ,buf)))


;;;; -- Errors --------------------------------------------------------------------------------

(defun verbatim (x)
  "A fix for the alternate interpretation of ~A with FORMAT which would
invoke *PRINT-CIRCLE* processing. When given a string argument, we copy that
string to defeat that."
  (if (stringp x) (copy-seq x) x))

(defun cpp-error (format-string &rest args)
  (format t "~&~A(~D) ERROR: ~?~%"
          (verbatim (pp-stream-file *pp-input-stream*))
          (pp-stream-line *pp-input-stream*)
          format-string args))

(defun cpp-fatal (format-string &rest args)
  (error "~A(~D) Fatal: ~?"
         (verbatim (pp-stream-file *pp-input-stream*))
         (pp-stream-line *pp-input-stream*)
         format-string args))

(defun cpp-warning (format-string &rest args)
  (format t "~&~A(~D) Warning: ~?~%"
          (pp-stream-file *pp-input-stream*)
          (pp-stream-line *pp-input-stream*)
          format-string args))


;;;; -- Character Input -----------------------------------------------------------------------

(declaim (inline pp-read-char-1 pp-read-char pp-unread-char))

(defun pp-underflow (input &optional (eof-error-p t) eof-value)
  ;; Lookahead.
  (setf (aref (pp-stream-buffer input) 0)
        (aref (pp-stream-buffer input) (1- (pp-stream-bufsize input))))
  (let ((n
         #+CMU
          (if (typep (pp-stream-stream input) 'concatenated-stream)
              1
              (read-sequence (pp-stream-buffer input) (pp-stream-stream input) :start 1))
          #-CMU
          (read-sequence (pp-stream-buffer input) (pp-stream-stream input) :start 1)))
    (cond ((= 1 n)
           (cond (eof-error-p
                  (error "End of file."))
                 (t
                  eof-value)))
          (t
           (setf (pp-stream-bufsize input) n
                 (pp-stream-bufptr input) 2)
           (aref (pp-stream-buffer input) 1)))))

(defun pp-read-char-1 (input &optional (eof-error-p t) eof-value)
  "Does not handle line continuations."
  (locally (declare (optimize (speed 3) (safety 0) (debug 1)))
    (cond ((< (pp-stream-bufptr input) (pp-stream-bufsize input))
           (prog1
               (aref (pp-stream-buffer input) (pp-stream-bufptr input))
             (setf (pp-stream-bufptr input) (the fixnum (+ (pp-stream-bufptr input) 1)))))
          (t
           (pp-underflow input eof-error-p eof-value)))))

(defun pp-read-char (input &optional (eof-error-p t) eof-value)
  (let ((c (pp-read-char-1 input eof-error-p eof-value)))
    (cond ((and (eql c #\\) (eql #\Newline (pp-peek-char input)))
           (incf (pp-stream-line input))
           (pp-read-char-1 input)
           (pp-read-char-1 input eof-error-p eof-value))
          ((eql c #\newline)
           (incf (pp-stream-line input))
           c)
          ((eql c #\return)
           (error "Stray CR. How did that happen?~%"))
          (t
           c))))

(defun pp-unread-char (char input)
  (decf (pp-stream-bufptr input))
  (cond ((eql char #\newline)
         (decf (pp-stream-line input))))
  char)

(defun pp-peek-char (input &optional (eof-error-p t) eof-value)
  (let ((c (pp-read-char input eof-error-p :eof)))
    (if (eql c :eof)
        eof-value
        (pp-unread-char c input))))


;;;; -- Tokens --------------------------------------------------------------------------------

(eval-when (eval compile load)
  (defstruct pp-token kind text)

  (defmethod print-object ((object pp-token) stream)
      (if *print-escape*
          (format stream "#~S" (pp-token-text object))
          (write-string (pp-token-text object) stream)) ))

;; We intern both identifier and punctuation tokens to be able to compare
;; those with using just EQ. Also there is a single newline token.

;; We then define compiler macros for PUNCT= and IDENT= which boil down to
;; EQ in case the string is constant. We also define an appropriate load
;; form for tokens, so that they are interned, when loaded.

(eval-when (eval compile load)
  (defvar *punct-token-hash*
    (make-hash-table :test #'equal))

  (defun make-punct-token (string)
    (or (gethash string *punct-token-hash*)
        (setf (gethash string *punct-token-hash*)
              (make-pp-token :kind :punctuation :text string)))))

(defun punct= (s tok)
  (and (pp-token-p tok)
       (eql :punctuation (pp-token-kind tok))
       (string= s (pp-token-text tok))))

(define-compiler-macro punct= (&whole whole string token)
  (cond ((constantp string)
         `(eq ',(make-punct-token (eval string)) ,token))
        (t
         whole)))

;; Dito for identifiers

(eval-when (eval compile load)
  (defparameter *ident-token-hash*
    (make-hash-table :test #'equal))

  (defun make-ident-token (string)
    (or (gethash string *ident-token-hash*)
        (setf (gethash string *ident-token-hash*)
              (make-pp-token :kind :ident :text string)))))

(defun ident= (s tok)
  (and (pp-token-p tok)
       (eql :ident (pp-token-kind tok))
       (string= s (pp-token-text tok))))

(define-compiler-macro ident= (&whole whole string token)
  (cond ((constantp string)
         `(eq ',(make-ident-token (eval string)) ,token))
        (t
         whole)))

;; load form for tokens

(eval-when (eval compile load)
  
  (defmethod make-load-form ((object pp-token) &optional env)
    (declare (ignore env))
    (cond ((eql :punctuation (pp-token-kind object))
           `(make-punct-token ',(pp-token-text object)))
          ((eql :ident (pp-token-kind object))
           `(make-ident-token ',(pp-token-text object)))
          (t
           `(make-pp-token
             :kind ',(pp-token-kind object)
             :text ',(pp-token-text object))))))

;;; constant tokens

(defvar +wsp-token+     (make-pp-token :kind :wsp :text " "))
(defvar +newline-token+ (make-pp-token :kind :newline :text (string #\newline)))
(defvar +eof-token+     (make-pp-token :kind :eof :text ""))

;;; token predicates

(declaim (inline newline-token-p white-token-p eof-token-p))

(defun white-token-p (q)
  (and (pp-token-p q)
       (member (pp-token-kind q) '(:wsp :newline :comment))))

(defun newline-token-p (q)
  (eq q +newline-token+))

(defun eof-token-p (q)
  (eq q +eof-token+))

(defun ident-token-p (q)
  (and (pp-token-p q) (eq (pp-token-kind q) :ident)))

(defun number-token-p (tok)
  (and (pp-token-p tok) (eql :number (pp-token-kind tok))))

(defun string-token-p (tok)
  (and (pp-token-p tok)
       (eql :string (pp-token-kind tok))))

;;;; string tokens

(defun make-string-token (string)
  (make-pp-token
   :kind :string
   :text (with-string-collector (bag)
           (bag #\")
           (loop for c across string do
                 (cond ((char= #\Newline c) (bag #\\) (bag #\n))
                       ((= 7 (char-code c)) (bag #\\) (bag #\a))
                       ((= 8 (char-code c)) (bag #\\) (bag #\b))
                       ((= 9 (char-code c)) (bag #\\) (bag #\t))
                       ((= 11 (char-code c)) (bag #\\) (bag #\v))
                       ((= 12 (char-code c)) (bag #\\) (bag #\f))
                       ((= 13 (char-code c)) (bag #\\) (bag #\r))
                       ((or (char= #\\ c) (char= #\" c))
                        (bag #\\)
                        (bag c))
                       ((char<= #\space c #\~)
                        (bag c))
                       ((< (char-code c) 256)
                        (map nil #'bag (format nil "\\~3,'0O" (char-code c))))
                       #+#.(cl:if (cl:> cl:char-code-limit #x100) '(and) '(or))
                       ((< (char-code c) (expt 16 4))
                        (map nil #'bag (format nil "\\u~4,'0X" (char-code c))))
                       #+#.(cl:if (cl:> cl:char-code-limit #x10000) '(and) '(or))
                       ((< (char-code c) (expt 16 8))
                        (map nil #'bag (format nil "\\u~8,'0X" (char-code c))))
                       #+#.(cl:if (cl:> cl:char-code-limit #x100000000) '(and) '(or))
                       (t
                        (error "Impressive!" c))))
           (bag #\"))))

(defun string-token-string (token)
  ;; ###
  ;; \{a,b,f,n,r,t,v}
  ;; \o, \oo, \ooo
  ;; \x h...
  ;; \u hhhh
  ;; \U hhhhhhhh
  (let ((str (pp-token-text token)))
    (unless (and (>= (length str) 2)
                 (char= (char str 0) #\")
                 (char= (char str (1- (length str))) #\"))
      (error "Not a string token: ~S." token))
    (deescape-c-string str :start 1 :end (1- (length str)))))

(defun deescape-c-string (string &key (start 0) (end nil))
  (with-string-collector (bag)
    (with-input-from-string (in string :start start :end end)
      (loop for c = (read-char in nil nil)
            while c do
            (cond ((char= c #\\)
                   (setf c (read-char in t))
                   (case c
                     (#\n (bag #\Newline))
                     (#\a (bag (code-char 7)))
                     (#\b (bag (code-char 8)))
                     (#\t (bag (code-char 9)))
                     (#\v (bag (code-char 11)))
                     (#\f (bag (code-char 12)))
                     (#\r (bag (code-char 13)))
                     ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7)
                      (unread-char c in)
                      (let ((value 0))
                        (loop for c = (read-char in nil nil)
                              repeat 3
                              while (and c (digit-char-p c 8))
                              do (setf value (+ (* 16 value) (digit-char-p c 8)))
                              finally (and c (unread-char c in)))
                        (bag (code-char value))))
                     (#\x
                      (let ((value 0))
                        (loop for c = (read-char in nil nil)
                              while (and c (digit-char-p c 16))
                              do (setf value (+ (* 16 value) (digit-char-p c 16)))
                              finally (and c (unread-char c in)))
                        (bag (code-char value))))
                     (#\u
                      (let ((value 0))
                        (loop for c = (read-char in)
                              repeat 4
                              do (setf value (+ (* 16 value) (digit-char-p c 16)))
                              finally (and c (unread-char c in)))
                        (bag (code-char value))))
                     (#\U
                      (let ((value 0))
                        (loop for c = (read-char in)
                              repeat 8
                              do (setf value (+ (* 16 value) (digit-char-p c 16)))
                              finally (and c (unread-char c in)))
                        (bag (code-char value))))
                     (t
                      (bag c)) ))
                  (t
                   (bag c)))))))

;;;; Number Tokens

(defun make-number-token (string)
  (make-pp-token :kind :number :text (prin1-to-string string)))

(defun pp-parse-c-integer (string)
  ;; ### lazy
  (let ((unsignedp nil))
    (loop while (and (> (length string) 1) (char= #\L (char string (1- (length string)))))
      do (setf string (subseq string 0 (1- (length string)))))
    (when (and (> (length string) 1) (char= #\U (char string (1- (length string)))))
      (setf string (subseq string 0 (1- (length string))))
      (setf unsignedp t))
    (multiple-value-bind (value unsignedp)
        (cond ((and (>= (length string) 2)
                    (string-equal "0x" string :end2 2))
               (values (parse-integer string :start 2 :radix 16) unsignedp))
              ((char= (char string 0) #\0)
               (values (parse-integer string :start 0 :radix 8) unsignedp))
              (t
               (values (parse-integer string  :junk-allowed t) ;xxx
                       unsignedp)))
      (cond (unsignedp
             `(unsigned ,value))
            (t
             value)))))

;;;; Header Name Tokens

(defun header-name-token-string (tok)
  ;; trim "<" and ">"
  (subseq (pp-token-text tok) 1 (1- (length (pp-token-text tok)))))

;;;; Token Utilities

(defun left-trim-whitespace (tokens)
  "Trims leading white space form 'tokens'."
  (member-if-not #'white-token-p tokens))

(defun trim-white-space (tokens)
  (reverse (left-trim-whitespace (reverse (left-trim-whitespace tokens)))))

(defun kill-white-space (tokens)
  (remove-if #'white-token-p tokens))

(defun kill-newlines (tokens)
  (loop for tok in tokens collect
        (if (newline-token-p tok)
            (make-pp-token :kind :wsp :text " ")
            tok)))

(defun pp-stringify (tokens)
  ;; Each run of white tokens must be converted to exactly one white space.
  (labels ((walk (tokens)
             (cond ((null tokens) "")
                   ((white-token-p (car tokens))
                    (let ((rest (left-trim-whitespace tokens)))
                      (if rest
                          (concatenate 'string " " (walk rest))
                          "")))
                   (t
                    (concatenate 'string (pp-token-text (car tokens)) (walk (cdr tokens)))))))
    (make-string-token (walk (left-trim-whitespace tokens)))))

(defun splice-tokens (a b)
  "Splices the tokens `a' and `b' into a new token. An error is thrown, if
the result is not a valid token."
  (let ((str (concatenate 'string (pp-token-text a) (pp-token-text b))))
    (let ((res (ignore-errors
                 (string-to-tokens str))))
      (cond ((= 1 (length res))
             res)
            (t
             (cpp-warning "Splicing ~S and ~S by ## does not yield a valid token."
                          (pp-token-text a) (pp-token-text b))
             res)))))

(defun chop-paren-balanced-group (tokens)
  "Collects a '(' and ')' balanced group from the token list `tokens`. The
second return value is the remaining tokens. Initial white space is skipped."
  (labels ((aux (q)
             (cond ((null q)
                    (cpp-error "Expected '(' facing ~S"
                               (tokens-string tokens)))
                   ((white-token-p (car q)) (aux (cdr q)))
                   ((punct= "(" (car q))
                    (aux-2 (cdr q) (cdr q) 1))))
           (aux-2 (q start nest)
             (cond ((null q)
                    (cpp-error "Missing ')' in ~S"
                               (tokens-string tokens)))
                   ((punct= ")" (car q))
                    (if (= nest 1)
                        (values (ldiff start q) (cdr q))
                        (aux-2 (cdr q) start (1- nest))))
                   ((punct= "(" (car q))
                    (aux-2 (cdr q) start (1+ nest)))
                   (t
                    (aux-2 (cdr q) start nest)))))
    (aux tokens)))


;;;; -- Tokenizer -----------------------------------------------------------------------------

(defvar *expect-header-name-p* nil)

(eval-when (eval compile load)
  (defparameter *c-punctuations* 
    '(">>=" "<<=" "+=" "-=" "*=" "/=" "%=" "&=" "^=" "|=" ">>" "<<" "++"
      "--" "->" "&&" "||" "<=" ">=" "==" "!=" ";" "{" "}" "," ":" "=" "("
      ")" "[" "]" "." "&" "!" "~" "-" "+" "*" "/" "%" "<" ">" "^" "|" "?" 
      "..."
      ".."                              ;XXX
      "#"                               ;for preprocessing
      "##")))                           ;for preprocessing

;; "..." is a special case, as ".." is not a punctuation

;; Token kinds:
;;
;; :string      "foo"
;; :char        'f'
;; :wsp         #\Space
;; :newline     #\Newline
;; :comment     /* ... */
;; :punctuation  +
;; :ident       foo
;; :header-name <foo.h>
;; :eof         end of file
;; :number      1234
;; :blue-painted        A blue painted identifier
;;

(eval-when (eval compile load)
  (defun make-punctuation-tree-form (strings &optional (prefix ""))
    (cond ((null strings)
           `',(make-punct-token prefix))
          (t
           (let ((firsts nil))
             (loop for s in strings do
                   (pushnew (char s 0) firsts))
             `(case ,(if (equal prefix "") 'c '(setf c (getc)))
                ,@(loop for f in firsts collect
                        (list f
                              (make-punctuation-tree-form
                               (loop for s in strings
                                     when (and (> (length s) 1)
                                               (char= (char s 0) f))
                                     collect (subseq s 1))
                               (concatenate 'string prefix (string f)))
                              #+NIL
                              (list 'otherwise (concatenate 'string prefix (string f)))
                              ))
                (otherwise
                 ,@(if (equal prefix "")
                       (list '(make-punct-token (string c)))
                       (list
                        `(ungetc c)
                        `',(make-punct-token prefix))))))))))

(defmacro dispatch-punctuation-tree ()
  (make-punctuation-tree-form *c-punctuations*))

(declaim (inline pp-ident-start-char-p pp-ident-char-p))

(defun pp-ident-start-char-p (c)
  (or (char<= #\A c #\Z)
      (char<= #\a c #\z)
      (char= c #\_)
      (char= c #\$)))

(defun pp-ident-char-p (c)
  (or (pp-ident-start-char-p c)
      (char<= #\0 c #\9)))

(declaim (inline pp-white-char-p))
(defun pp-white-char-p (c)
  (or (eql c #\space)
      (eql c #\tab)
      (eql c #\newline)
      (eql c #\return)
      (eql c #\page)
      (eql c #.(code-char 11))))

(defun pp-read-token (stream)
  (declare #.+optimize-fast+)
  (let ((c (pp-read-char stream nil :eof)))
    (multiple-value-bind (k)
        (cond ((eq :eof c)
               +eof-token+)
              ;; newlines
              ((eql #\newline c)
               +newline-token+)
              ((and *expect-header-name-p* (eql c #\<)) ;Bah!
               (make-pp-token
                :kind :header-name
                :text
                (with-string-collector (bag)
                  (bag c)
                  (loop for d = (pp-peek-char stream nil nil)
                        until (or (eql d #\>) (eql d #\newline))
                        do (bag d)
                        do (pp-read-char stream)
                        finally
                        (unless (eql d #\>)
                          (cpp-fatal "Bad header name"))
                        (bag d)
                        (pp-read-char stream) ))))
              ;; identifiers
              ((pp-ident-start-char-p c)
               (pp-read-ident stream c))
              
              ;; white space
              ((pp-white-char-p c)
               (cond (*cpp-preserve-white-space-p*
                      (make-pp-token
                       :kind :wsp
                       :text
                       (with-string-collector (bag)
                         (bag c)
                         (loop for d = (pp-peek-char stream nil nil)
                               while (and d (pp-white-char-p d) (not (eql #\newline d))) do
                               (pp-read-char stream)
                               (bag d)))))
                     (t
                      (let (d)
                        (loop
                            (setf d (pp-read-char stream nil nil))
                            (cond ((null d)
                                   (return))
                                  ((and (not (eql d #\newline))
                                        (pp-white-char-p d)))
                                  (t
                                   (pp-unread-char d stream)
                                   (return)))))
                      +wsp-token+)))
              ;; literals
              ((member c '(#\" #\'))
               ;; ### EOF while in literal!
               (make-pp-token
                :kind (if (eql c #\") :string :char)
                :text
                (with-string-collector (bag)
                  (bag c)
                  (loop for d = (pp-read-char stream)
                        do (bag d)
                        until (eql c d) do
                        (when (eql d #\\)
                          (bag (pp-read-char stream)))))))
              ;; C style comments
              ((and (eql c #\/) (eql #\* (pp-peek-char stream nil nil)))
               (cond ((not *cpp-keep-comments-p*)
                      (pp-read-char stream)
                      (let ((c1 nil) (c2 nil))
                        (loop
                            (setf c1 c2)
                            (setf c2 (pp-read-char stream))
                          (when (and (eql c1 #\*) (eql c2 #\/))
                            (return))))
                      +wsp-token+)
                     (t
                      (make-pp-token :kind
                                     :comment :text
                                     (with-string-collector (bag)
                                       (bag c)
                                       (bag (pp-read-char stream))
                                       (loop for d = (pp-read-char stream)
                                             do (bag d)
                                             until (and (eql #\* d)
                                                        (eql #\/ (pp-peek-char stream nil nil)))
                                             finally (bag (pp-read-char stream))))))))
              ;; C++ style comments
              ((and (eql c #\/) (eql #\/ (pp-peek-char stream nil nil)))
               ;; we keep the final #\Newline
               (cond ((not *cpp-keep-comments-p*)
                      (loop for d = (pp-peek-char stream)
                            until (eql d #\newline)
                            do (pp-read-char stream))
                      +wsp-token+)
                     (t
                      (make-pp-token :kind
                                     :comment :text
                                     (with-string-collector (bag)
                                       (bag c)
                                       (bag (pp-read-char stream))
                                       (loop for d = (pp-peek-char stream)
                                             until (eql d #\newline)
                                             do (bag (pp-read-char stream))))))))
              ;; numbers ###
              ((or (digit-char-p c)
                   (and (eql #\. c) (digit-char-p (pp-peek-char stream nil #\x))))
               (make-pp-token :kind :number :text (pp-read-number stream c)))

              ;; punctuations
              (t
               (labels ((getc () (pp-read-char stream nil nil))
                        (ungetc (c) (and c (pp-unread-char c stream))))
                 (declare (inline getc ungetc) (optimize (speed 3)))
                 (dispatch-punctuation-tree))))
      k)))

(defun pp-read-ident (stream c)
  (let ((buf (pp-stream-tempbuf stream)))
    (setf (fill-pointer buf) 0)
    (vector-push-extend c buf)
    (loop do
         (setf c (pp-read-char stream nil nil))
         (when (null c)
           (return))
         (unless (pp-ident-char-p c)
           (pp-unread-char c stream)
           (return))
         (vector-push-extend c buf))
    (or (gethash buf *ident-token-hash*)
        (setf (gethash (copy-seq buf) *ident-token-hash*)
              (make-pp-token :kind :ident :text (copy-seq buf))))))

(defun pp-read-number (stream c)
  (with-string-collector (bag)
    (bag c)
    (loop for d = (pp-read-char stream nil nil) do
          (cond ((null d)
                 (return))
                ((eql d #\.)
                 (bag d))
                ((not (pp-ident-char-p d))
                 (pp-unread-char d stream)
                 (return))
                ((and (find d "eEpP")
                      (find (pp-peek-char stream nil nil) "+-"))
                 (bag d)
                 (bag (pp-read-char stream)))
                (t
                 (bag d))))))

(defun unread-token (q)
  (setf *lookahead* (cons q *lookahead*)))
               
(defun peek-token ()
  (let ((q (read-token)))
    (unless (eof-token-p q)
      (unread-token q))
    q))


;;;; -- String to tokens and back -------------------------------------------------------------

(defun join-strings (separator strings)
  (with-output-to-string (res)
    (loop for string in strings
          for nil = nil then (write-string separator res)
          do (write-string string res))))

(defun tokens-string (tokens)
  (join-strings " " (mapcar #'(lambda (x)
                                (if (pp-token-p x)
                                    (pp-token-text x)
                                    (prin1-to-string x)))
                            (remove-if #'white-token-p tokens))))

(defun string-to-tokens (string)
  (with-input-from-string (input string)
    (let ((*pp-input-stream* (make-pp-stream input :file "<stdin>"))
          (*pp-input-stack* nil)
          (*pp-char-lookahead-tri* nil))
      (let ((*bol-p* nil)
            (*flag-stack* nil)
            (*lookahead* nil)
            (*defs* (make-defs-table))
            (*if-stack* nil))
        (prog1
            (loop for tok = (read-token) while (not (eof-token-p tok)) collect tok)
          (destroy-pp-stream *pp-input-stream*)))) ))


;;;; -- The Input Stack -----------------------------------------------------------------------

#+SBCL
(progn

  ;; SBCL has no support for CRLF line termination. Plug in a Gray stream
  ;; as a filter for us.

  (defclass crlf-input-stream (fundamental-character-input-stream)
    ((stream :initarg :stream :initform nil :accessor crlf-input-stream-stream)
     (c1 :initform nil)
     (c2 :initform nil)))

  (defun make-crlf-input-stream (stream)
    (make-instance 'crlf-input-stream :stream stream))

  (defmethod stream-read-char ((stream crlf-input-stream))
    (with-slots (c1 c2 stream) stream
      (or (shiftf c1 c2 nil)
          (let ((c (read-char stream nil :eof)))
            (case c
              (#\Return
               (let ((d (read-char stream nil :eof)))
                 (case d
                   (#\Newline d)
                   (t (shiftf c2 c1 d) c))))
              (t c))))))

  (defmethod stream-unread-char ((stream crlf-input-stream) character)
    (with-slots (c1 c2 stream) stream
      (shiftf c2 c1 character)
      character))

  (defmethod stream-peek-char ((stream crlf-input-stream))
    (with-slots (c1 c2 stream) stream
      (or c1
          (let ((c (read-char stream nil :eof)))
            (case c
              (:eof (setf c1 c))
              (#\Return
               (let ((d (or (shiftf c1 c2 nil) (read-char stream nil :eof))))
                 (case d
                   (#\Newline (setf c1 d))
                   (t (setf c2 d c1 c))))))))))

  (defmethod stream-listen ((stream crlf-input-stream))
    (with-slots (c1 c2 stream) stream
      (if c1 (not (eq :eof c1)) (listen stream))))

  ;; ### STREAM-READ-CHAR-NO-HANG, FILE-POSITION ...

  (defmethod close ((stream crlf-input-stream) &key abort)
    (with-slots (stream) stream
      (close stream :abort abort))) )

(defun push-input-file (filename)
  (push-input-stream
   (open filename :direction :input :external-format *cpp-default-file-encoding*)
   filename))

(defun push-input-stream (stream filename)
  #+(AND SBCL NOFFI-WINDOWS)
  (setq stream (make-crlf-input-stream stream))
  (push filename *files-read*)
  (push *pp-input-stream* *pp-input-stack*)
  (push *if-stack* *pp-input-stack*)
  (setf *pp-input-stream* (make-pp-stream stream :file filename)
        *if-stack* nil
        *bol-p* t))

(defun pop-input ()
  (close (pp-stream-stream *pp-input-stream*))
  (destroy-pp-stream *pp-input-stream*)
  (setf *if-stack* (pop *pp-input-stack*)
        *pp-input-stream* (pop *pp-input-stack*)
        *bol-p* t))


;;;; -- Definition Database -------------------------------------------------------------------

(defun make-defs-table ()
  (let ((env (make-macro-environment)))
    (let ((now (get-universal-time)))
      (multiple-value-bind (s m h day mon year) (decode-universal-time now)
        (setf (c-macro-definition (make-ident-token "__DATE__") env)
              (make-lambda-macro-definition
               (lambda ()
                              (list (make-string-token
                                     (format nil "~A ~2D ~D"
                                             (verbatim
                                              (aref #("Jan" "Feb" "Mar" "Apr" "May" "Jun"
                                                      "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
                                                    (1- mon)))
                                             day
                                             year))))))
        (setf (c-macro-definition (make-ident-token "__TIME__") env)
              (make-lambda-macro-definition
               (lambda ()
                 (list (make-string-token (format nil "~2,'0D:~2,'0D:~2,'0D" h m s))))))
        (setf (c-macro-definition (make-ident-token "__FILE__") env)
              (make-lambda-macro-definition
               (lambda ()
                 (list (make-string-token (pp-stream-file *pp-input-stream*))))))
        (setf (c-macro-definition (make-ident-token "__LINE__") env)
              (make-lambda-macro-definition
               (lambda ()
                 (list (make-number-token (pp-stream-line *pp-input-stream*))))))
        (setf (c-macro-definition (make-ident-token "__STDC__") env)
              (make-object-macro-definition (list (make-number-token 1))))
        (setf (c-macro-definition (make-ident-token "__NOFFI__") env)
              (make-object-macro-definition (list (make-number-token 1))))
        #+NIL (setf (gethash (make-ident-token "__STDC_HOSTED__") res))
        #+NIL (setf (gethash (make-ident-token "__STDC_MB_MIGHT_NEQ_WC__") res))
        #+NIL (setf (gethash (make-ident-token "__STDC_VERSION__") res))
        env))))

(defun pp-macro-definition (ident)
  (c-macro-definition ident *defs*))

#|
(defun make-function-macro-definition (parameters body)
  (list :function parameters body))

(defun make-lambda-macro-definition (body)
  (list :lambda body))

(defun make-object-macro-definition (body)
  (cons :object body))

(defun macro-definition-kind (macro-definition)
  (car macro-definition))

(defun macro-definition-body (macro-definition)
  (ecase (macro-definition-kind macro-definition)
    (:object (cdr macro-definition))
    (:function (caddr macro-definition))
    (:lambda (cadr macro-definition))))

(defun macro-definition-parameters (macro-definition)
  (if (eq (macro-definition-kind macro-definition) :object) nil (cadr macro-definition)))
|#

(defstruct macro-definition
  kind parameters body sloc)

(defun make-function-macro-definition (parameters body &key sloc)
  (make-macro-definition :kind :function :parameters parameters :body body :sloc sloc))

(defun make-object-macro-definition (body &key sloc)
  (make-macro-definition :kind :object :body body :sloc sloc))

(defun make-lambda-macro-definition (function)
  (make-macro-definition :kind :lambda :body function))

(defun (setf pp-macro-definition) (new-value ident)
  (when (equal "USE_OLD_TTY" (pp-token-text ident))
    (error "USE_OLD_TTY"))
  (labels ((blame (fmt &rest args)
             (apply #'cpp-error fmt args)
             (return-from pp-macro-definition new-value)))
    ;;
    (when new-value
      (let ((old (c-macro-definition ident *defs*))
            (body (macro-definition-body new-value))
            (params (macro-definition-parameters new-value)))
        (unless (or (null old) (macro-definitions-are-equal old new-value))
          (cpp-warning "Macro ~A redefined.~%  old = ~A~%  new = ~A~%"
                       (verbatim ident)
                       (verbatim (print-macro-def ident old nil))
                       (verbatim (print-macro-def ident new-value nil))))
        (when (and (member-if (lambda (q) (ident= "__VA_ARGS__" q)) body)
                   (not (member '&rest params)))
          (blame "__VA_ARGS__ may only appear in a variadic function macro."))
        (when (and (member-if (lambda (q) (ident= "__VA_OPT__" q)) body)
                   (member '&rest params))
          (blame "__VA_OPT__ may only appear in a variadic function macro."))
        (when (punct= "##" (car body))
          (blame "\"##\" at beginning of replacement list."))
        (when (and (cdr body) (punct= "##" (car (last body))))
          (blame "\"##\" at end of replacement list."))
        (when (eq :function (macro-definition-kind new-value))
          (do ((q body (cdr q)))
              ((null q))
            (when (and (punct= "#" (car q))
                       (not (member (car (left-trim-whitespace (cdr q))) params :test #'token=)))
              (blame "# not followed by macro parameter."))))))
    (setf (c-macro-definition ident *defs*) new-value)))

(declaim (inline token=))
(defun token= (x y) (eq x y))

(defun macro-definitions-are-equal (old new)
  (labels ((compare (xs ys)
             (cond ((null xs) (null ys))
                   ((null ys) nil)
                   ((white-token-p (car xs))
                    (cond ((white-token-p (car ys))
                           (compare (left-trim-whitespace xs)
                                    (left-trim-whitespace ys)))
                          (t
                           nil)))
                   ((white-token-p (car ys))
                    nil)
                   ((string= (pp-token-text (car xs)) (pp-token-text (car ys)))
                    (compare (cdr xs) (cdr ys))))))
    (and (eq (macro-definition-kind old)
             (macro-definition-kind new))
         (case (macro-definition-kind new)
           (:object
            (compare (macro-definition-body old)
                     (macro-definition-body new)))
           (:function
            (and (equal (macro-definition-parameters old)
                        (macro-definition-parameters new))
                 (compare (macro-definition-body old)
                          (macro-definition-body new))))
           (otherwise
            t)))))


;;;; -- Constant Expressions ------------------------------------------------------------------

;; Valid operators are (precedence in this order) :
;;  defined, (unary)+, (unary)-, ~, !,
;;  *, /, %,
;;  +, -,
;;  <<, >>,
;;  <, >, <=, >=,
;;  ==, !=,
;;  &,
;;  ^,
;;  |,
;;  &&,
;;  ||,
;;  ? :
;;

(defun asl (x y) (ash x y))
(defun asr (x y) (ash x (- y)))

;; We to expand the token sequence with a twist: Any of the two forms:

;; "defined" identifier
;; "defined" "(" identifier ")"

;; need to be replaced by "0" or "1" indicating whether the macro named by the
;; identifier is defined or not.

;; 6.10.1 paragraph 4 says:

;; | If the token "defined" is generated as a result of this replacement
;; | process or use of the "defined" unary operator does not match one of
;; | the two specified forms prior to macro replacement, the behavior is
;; | undefined.

;; gcc happily takes a 'defined' from expansions, clang does too but warns
;; about this. MSC also takes a 'defined' from expansions. And it actually is
;; used that way in some MS header files. So we better do as well.

;; There is a global flag *INSIDE-IF-DIRECTIVE-P*, when it is turned on the
;; argument to 'defined' is not expanded, leaving it for further expansion. It
;; then gets parsed to (DEFINE <ident>) to be handled by the evaluator.

(defvar *inside-if-directive-p* nil
  "A flag to make EXPAND-ONE aware of 'defined'.")

(defun pp-parse-expression (tokens)
  "Parse the token sequence /tokens/ as an expression, while expanding macros."
  (pp-parse-expression-1 (let ((*inside-if-directive-p* t))
                           (expand-tokens tokens))))

(defun pp-parse-expression-1 (tokens)
  "Parse the token sequence /tokens/ which already is expanded as an expression."
  ;; Get white space out of the way.
  (setf tokens (remove-if #'white-token-p tokens))
  ;; This is a vanilla recursive descent parser.
  (labels ((parse-binop (tokens lisp-op c-op down)
             (parse-binop* tokens (list (list lisp-op c-op)) down))
           ;;
           (parse-binop* (tokens mapping down &aux it res)
             (multiple-value-bind (x r) (funcall down tokens)
               (setf res x)
               (loop while (setf it (and r (find-if (lambda (kv)
                                                      (punct= (cadr kv) (car r)))
                                                    mapping)))
                 do
                 (setf (values x r) (funcall down (cdr r)))
                 (setf res (list (car it) res x)))
               (values res r)))
           ;;
           (parse-constant (tokens)
             (cond ((null tokens)
                    (cpp-error "Premature end of expression.")
                    (return-from pp-parse-expression-1 0))
                   ((punct= "(" (car tokens))
                    (multiple-value-bind (x rest) (parse-expr (cdr tokens))
                      (unless (punct= ")" (car rest))
                        (cpp-error "Unbalanced '(' in ~A" (verbatim (tokens-string tokens)))
                        (return-from pp-parse-expression-1 0))
                      (values x (cdr rest))))
                   ((ident-token-p (car tokens))
                    (values 0 (cdr tokens)))
                   ((number-token-p (car tokens))
                    (values (pp-parse-c-integer (pp-token-text (car tokens))) ;###
                            (cdr tokens)))
                   (t
                    (cpp-error "Unexpected: ~S." (pp-token-text (car tokens)))
                    (return-from pp-parse-expression-1 0))))

           (parse-unary (tokens)
             (cond ((and tokens (punct= "+" (car tokens)))
                    (multiple-value-bind (x r) (parse-unary (cdr tokens))
                      (values `(+ ,x) r)))
                   ((and tokens (punct= "-" (car tokens)))
                    (multiple-value-bind (x r) (parse-unary (cdr tokens))
                      (values `(- ,x) r)))
                   ((and tokens (punct= "!" (car tokens)))
                    (multiple-value-bind (x r) (parse-unary (cdr tokens))
                      (values `(not ,x) r)))
                   ((and tokens (punct= "~" (car tokens)))
                    (multiple-value-bind (x r) (parse-unary (cdr tokens))
                      (values `(lognot ,x) r)))
                   ((and tokens (ident= "defined" (car tokens)))
                    (cond ((ident-token-p (cadr tokens))
                           (values `(defined ,(cadr tokens)) (cddr tokens)))
                          ((and (punct= "(" (cadr tokens))
                                (ident-token-p (caddr tokens))
                                (punct= ")" (cadddr tokens)))
                           (values `(defined ,(caddr tokens)) (cddddr tokens)))
                          (t
                           ;; No luck. We just parse another constant expression, whine and return a
                           ;; zero.
                           (cpp-warning "Bad use of 'defined'")
                           (multiple-value-bind (x r) (parse-constant (cdr tokens))
                             (declare (ignore x))
                             (values 0 r)))))
                   (t
                    (parse-constant tokens))))
           
           (parse-product (tokens)
             (parse-binop* tokens '((* "*") (/ "/") (mod "%")) #'parse-unary))
           
           (parse-sum (tokens)
             (parse-binop* tokens '((+ "+") (- "-")) #'parse-product))
           
           (parse-shift (tokens)
             (parse-binop* tokens '((asl "<<") (asr ">>")) #'parse-sum))
           
           (parse-neql (tokens)
             (parse-binop* tokens '((< "<") (<= "<=") (> ">") (>= ">=")) #'parse-shift))
           
           (parse-eql (tokens)
             (parse-binop* tokens '((= "==") (/= "!=")) #'parse-neql))

           (parse-logand (tokens)
             (parse-binop tokens 'and "&" #'parse-eql))

           (parse-logxor (tokens)
             (parse-binop tokens 'logxor "^" #'parse-logand))

           (parse-logior (tokens)
             (parse-binop tokens 'logior "|" #'parse-logxor))

           (parse-and (tokens)
             (parse-binop tokens 'and "&&" #'parse-logior))

           (parse-or (tokens)
             (parse-binop tokens 'or "||" #'parse-and))

           (parse-cond (tokens)
             (multiple-value-bind (test rest) (parse-or tokens)
               (cond ((and rest (punct= "?" (car rest)))
                      (multiple-value-bind (cons rest) (parse-or (cdr rest))
                        (cond ((and rest (punct= ":" (car rest)))
                               (multiple-value-bind (alt rest) (parse-or (cdr rest))
                                 (values `(if ,test ,cons ,alt) rest)))
                              (t
                               (cpp-error "Expected ':'.")
                               (return-from pp-parse-expression-1 0)))))
                     (t
                      (values test rest)))))
           ;;
           (parse-expr (tokens)
             (parse-cond tokens)))
    (multiple-value-bind (res rest) (parse-expr tokens)
      (cond ((not (null rest))
             (cpp-error "Garbage at end of expression.")
             0)
            (t
             res)))))

;; Sigh, since we must short circuit, we figure out signess by analysis of
;; the expression.

(defun pp-ac-expr (expr)
  (cond ((numberp expr)
         (values expr nil))
        ((eql (car expr) 'unsigned)
         (values expr t))
        ((member (car expr) '(or and))
         (values (list* (car expr) (mapcar #'pp-ac-expr (cdr expr)))))
        ((eql (car expr) 'if)
         (multiple-value-bind (a au) (pp-ac-expr (third expr))
           (multiple-value-bind (b bu) (pp-ac-expr (fourth expr))
             (if (or au bu)
                 `(if ,(pp-ac-expr (second expr)) (unsigned ,a) (unsigned ,b))
                 `(if ,(pp-ac-expr (second expr)) ,a ,b)))))
        ((member (car expr) '(asl asr))
         (multiple-value-bind (a au) (pp-ac-expr (second expr))
           (multiple-value-bind (b bu) (pp-ac-expr (third expr))
             (declare (ignore bu))
             (values (list (car expr) a b)
                     au))))
        ((member (car expr) '(< <= >= > = /=))
         (multiple-value-bind (a au) (pp-ac-expr (second expr))
           (multiple-value-bind (b bu) (pp-ac-expr (third expr))
             (when (or au bu)
               (setf a `(unsigned ,a))
               (setf b `(unsigned ,b)))
             (values (list (car expr) a b)
                     nil))))
        ((and (null (cddr expr)) (member (car expr) '(+ - lognot)))
         (multiple-value-bind (a au) (pp-ac-expr (second expr))
           (if au
               (values `(,(car expr) (unsigned ,a)) au)
               (values `(,(car expr) ,a) au))))
        ((eq (car expr) 'defined)
         (values expr nil))
        ((member (car expr) '(+ - * / % logand logior logxor))
         (multiple-value-bind (a au) (pp-ac-expr (second expr))
           (multiple-value-bind (b bu) (pp-ac-expr (third expr))
             (if (or au bu)
                 (values (list (car expr) `(unsigned ,a) `(unsigned ,b)) t)
                 (values (list (car expr) a b) nil)))))
        ((eql (car expr) 'not)
         (values (list (car expr) (pp-ac-expr (second expr))) nil)) ))

(defun pp-eval-expr (expr)
  (cond ((numberp expr)
         expr)
        ((eql (car expr) 'unsigned)
         (conv-unsigned (pp-eval-expr (cadr expr))))
        ((eql (car expr) 'or)
         (if (zerop (pp-eval-expr (cadr expr)))
             (if (zerop (pp-eval-expr (caddr expr))) 0 1)
             1))
        ((eql (car expr) 'and)
         (if (zerop (pp-eval-expr (cadr expr)))
             0
             (if (zerop (pp-eval-expr (caddr expr))) 0 1)))
        ((eql (car expr) 'if)
         (if (zerop (pp-eval-expr (cadr expr)))
             (pp-eval-expr (cadddr expr))
             (pp-eval-expr (caddr expr))))
        ((and (eql (car expr) 'defined)
              (= (length expr) 2)
              (ident-token-p (cadr expr)))
         (if (pp-macro-definition (cadr expr)) 1 0))
        ((let ((x (and (cadr expr) (pp-eval-expr (cadr expr))))
               (y (and (caddr expr) (pp-eval-expr (caddr expr)))))
           (ecase (car expr)
             ;; unary
             ;; ((defined) 0)              ;###
             ((+) (+ x (or y 0)))
             ((-) (if y (- x y) (- x)))
             ((not) (if (zerop x) 1 0))
             ((lognot) (lognot x))
             ;; binary
             ((*) (* x y))
             ((/)
              (cond ((zerop y) (cpp-error "Division by zero.") 0)
                    (t (floor x y))))
             ((%)
              (cond ((zerop y) (cpp-error "Division by zero.") 0)
                    (t (mod x y))))
             ((asl) (ash x y))
             ((asr) (ash x (- y)))
             ((< <= > >= = /=) (if (funcall (car expr) x y) 1 0))
             ((logand logior logxor) (funcall (car expr) x y))
             ((and) (if (or (zerop x) (zerop y)) 0 1))
             ((or) (if (and (zerop x) (zerop y)) 0 1))
             ((if) (if (zerop x) (pp-eval-expr (cadddr expr)) y)))))))

(defun conv-unsigned (x)
  (cond ((< x 0)
         (cpp-warning "Converting ~D to unsigned." x)
         (ldb (byte *cpp-integer-length* 0) x))
        (t
         x)))


;;;; -- Directive Processing ------------------------------------------------------------------

;;
;; when no lookahead and bolp:
;;     peek into stream until either "#" or newline
;;     when "#" seen:
;;        process directive
;;     else put stuff on lookahead and continue
;; else
;;     q = read-token
;;     bolp <- newline-token-p (q)
;;     ...
;; end

(defun next-token ()
  (cond ((and *bol-p* (null *lookahead*))
         (let ((xs (loop for q = (read-token)
                         until (eof-token-p q)
                         collect q
                         until (or (not (white-token-p q))
                                   (newline-token-p q)))))
           (cond ((and xs (punct= "#" (car (last xs))))
                  (handle-directive (read-directive))
                  (setf *bol-p* t)
                  (next-token))         ;again
                 (t
                  (setf *lookahead* xs)
                  (setf *bol-p* nil)
                  (next-token)))))
        (t
         (let ((q (read-token)))
           (cond ((and (eof-token-p q) *if-stack*)
                  (cpp-fatal "unterminated #if"))
                 ((and (eof-token-p q) *pp-input-stack*)
                  (pop-input)
                  (next-token))
                 ((eof-token-p q)
                  q)
                 ;; compress whitespace / comment runs
                 ;; however we might want to keep leading white space.
                 ;; It'll get a bit tricky!
                 #+NIL
                 ((and (not *cpp-keep-comments-p*)
                       (not *cpp-preserve-white-space-p*)
                       (white-token-p q)
                       (not (newline-token-p q)))
                  (let (q)
                    (loop
                        (setf q (read-token))
                        (cond ((eof-token-p q)
                               (return))
                              ((newline-token-p q)
                               (return-from next-token q))
                              ((or (newline-token-p q) (not (white-token-p q)))
                               (unread-token q)
                               (return))))
                    +wsp-token+))
                 ;;
                 (t
                  (setf *bol-p* (newline-token-p q))  ;Hmm.
                  q))))))

(defvar *silent-sentinel* t)

(defun read-token ()
  ;; When we hit a sentinel, we look past it and return the first real
  ;; token behind the sentinel while keeping the sentinel in the look
  ;; ahead buffer.
  (cond ((and *silent-sentinel*
              (consp (car *lookahead*))
              (eq (caar *lookahead*) :sentinel))
         (let ((sentinel (pop *lookahead*)))
           (prog1
               (read-token)
             (push sentinel *lookahead*))))
        ((pop *lookahead*))
        ((pp-read-token *pp-input-stream*))))

(defun skip-if-section ()
  "Skip input until #endif or #else is seen."
  ;; ### #include <foo'.h> will spoil this
  ;; When we want to get real fancy, we write a special purpose
  ;; automaton, that will skip if sections fast.
  (let ((q)
        (nest 0))
    (tagbody
     q0
       (fast-skipper *pp-input-stream* t)
     q1
       ;; W* # seen
       (setf q (read-token))
       (cond ((white-token-p q)
              (go q1))
             ((eof-token-p q)
              (cpp-fatal "EOF"))
             ;;
             ((or (ident= "else" q)
                  (ident= "elif" q))
              (cond ((zerop nest) (go done))))
             ;;
             ((ident= "endif" q)
              (cond ((zerop nest) (go done))
                    (t (decf nest)))
              )
             ;;
             ((or (ident= "if" q)
                  (ident= "ifdef" q)
                  (ident= "ifndef" q))
              (incf nest)
              (fast-skipper *pp-input-stream* nil)
              (go q1))
             (t
              ;; something not white seen
              (fast-skipper *pp-input-stream* nil)
              (go q1)))
       (fast-skipper *pp-input-stream* nil)
       (go q1)
     done
       ;; done -- eat the rest of the line and return what we found
       (return-from skip-if-section
         (values q (loop for q = (read-token) until (or (eof-token-p q) (newline-token-p q)) collect q))))))

(defun read-directive ()
  (let ((res
         (progn
           ;; #\newline #\# have already been read
           ;; skip white space
           (loop for q = (peek-token)
                 until (or (newline-token-p q)
                           (eof-token-p q)
                           (not (white-token-p q)))
                 do (read-token))
           ;;
           (let ((id (peek-token)))
             (cond ((ident= "include" id)
                    ;; special case for include
                    (read-token)
                    (let ((*expect-header-name-p* t))
                      (cons id (loop for q = (peek-token)
                                     until (or (eof-token-p q) (newline-token-p q))
                                     collect q
                                     do (read-token)))))
                   (t
                    (loop for q = (read-token)
                          until (or (eof-token-p q) (newline-token-p q))
                          collect q
                          finally (unless (eof-token-p q) (unread-token q)))))))))
    ;;
    ;; Put the final newline back into the character stream, so that
    ;; line number information is correct for error messages regarding
    ;; directives.
    ;;
    (when (newline-token-p (car *lookahead*))
      (pop *lookahead*)
      (pp-unread-char #\newline *pp-input-stream*))
    ;;
    res))

(defun parse-define-function-macro (tokens)
  ;; '(' has already been read.
  (let* ((pivot (position-if #'(lambda (x) (punct= ")" x)) tokens)))
    (cond ((null pivot)
           (cpp-warning "Missing ')' is macro parameter list.")
           (return-from parse-define-function-macro nil))
          (t
           (let ((param-tokens (subseq tokens 0 pivot))
                 (body-tokens  (subseq tokens (1+ pivot)))
                 param-parts)
             ;; split by ","
             (let ((p 0) q)
               (loop
                 (setf q (position-if #'(lambda (x) (punct= "," x)) param-tokens :start p))
                 (let ((it (remove-if #'white-token-p (subseq tokens p (or q pivot)))))
                   (cond ((and (null it) (null q)))
                         ((and (= 1 (length it)) (ident-token-p (car it)))
                          (cond ((member (car it) param-parts)
                                 (cpp-warning "Duplicate parameter ~S." (tokens-string it))
                                 (return-from parse-define-function-macro nil))
                                (t
                                 (push (car it) param-parts))))
                         ((and (= 1 (length it)) (punct= "..." (car it)))
                          (cond ((null q)
                                 (push '&rest param-parts)
                                 (push (make-ident-token "__VA_ARGS__") param-parts))
                                (t
                                 (cpp-warning "'...' must come last in macro parameter list.")
                                 (return-from parse-define-function-macro nil))))
                         ((and (= 2 (length it))
                               (ident-token-p (car it))
                               (punct= "..." (cadr it)))
                          (cond ((null q)
                                 (push '&rest param-parts)
                                 (push (car it) param-parts))
                                (t
                                 (cpp-warning "'...' must come last in macro parameter list.")
                                 (return-from parse-define-function-macro nil))))
                         (t
                          (cpp-warning "Bad parameter name: ~S."
                                       (tokens-string it))
                          (return-from parse-define-function-macro nil))))
                 (when (null q) (return))
                 (setf p (+ q 1))))
             (values t (reverse param-parts) (trim-white-space body-tokens)))))))

(defun process-define (xs &aux sloc)
  ;; "#define" is already read.
  (setq sloc (list (pp-stream-file *pp-input-stream*)
                   (pp-stream-line *pp-input-stream*)))
  (block grok-define
    (setf xs (left-trim-whitespace xs))
    (cond ((and xs (ident-token-p (car xs)))
           (let ((id (pop xs)))
             (cond ((and xs (punct= "(" (car xs)))
                    (multiple-value-bind (successp params body)
                        (parse-define-function-macro (cdr xs))
                      (when successp
                        (setf (pp-macro-definition id)
                              (make-function-macro-definition params body :sloc sloc))) ))
                   (t
                    (unless (or (null xs) (white-token-p (car xs)))
                      (cpp-warning "There shall be white space between the identifier and the replacement list."))
                    (setf (pp-macro-definition id)
                          (make-object-macro-definition (trim-white-space xs) :sloc sloc))))))
          (t
           (cpp-warning "Malformed define.")))))

;;     next-token :=
;;
;;       when #if
;;         when condition is true
;;           push if-stack
;;           return next token
;;         else
;;           skip until #else or #endif
;;           when #else
;;             push if-stack
;;             return next token
;;           when #endif
;;             return next token
;;           end
;;       end
;;
;;       when #else
;;         when if-stack is empty => error "#else without #if"
;;         pop if-stack
;;         skip until #else or #endif
;;         when #else
;;           error "#else #else seen."
;;         when #endif
;;           return next-token
;;
;;       when #endif
;;         when if-stack is empty => error "#else without #if"
;;         pop if-stack
;;         return next-token
;;

;;;; fast skiping

;; a fast skipper would do
;;
;; c <- next char
;; on EOF barf!
;; on #\" => bol := nil; skip a string
;; on #\' => bol := nil; skip a character literal
;; on #\/ =>
;;    c <- next char
;;    on #\* => skip a comment, bol := nil
;;    on #\/ => skip a c++ comment, bol := t
;; on #\# and bol => we are at it!
;; on #\newline bol => t
;; else bol => nil
;;
;;

(defun handle-directive (dir)
  (setf dir (left-trim-whitespace dir))
  (cond ((null dir)
         ;; empty directive
         )
        (t
         ;; check for #if
         (multiple-value-bind (ifp bool) (if-directive-p dir)
           (cond (ifp
                  (labels ((frob (bool)
                             (cond (bool
                                    (push t *if-stack*))
                                   (t
                                    (multiple-value-bind (q rest) (skip-if-section)
                                      (cond ((ident= "else" q)
                                             (push nil *if-stack*))
                                            ((ident= "endif" q)
                                             nil)
                                            ((ident= "elif" q)
                                             ;; bool <- evaluate condition; do again
                                             (frob (not (zerop (pp-eval-expr (pp-ac-expr (pp-parse-expression rest)))))))
                                            (t
                                             (error "Something very wrong."))))))))
                    (frob bool)))
                 ;; 
                 ((ident= "else" (car dir))
                  (cond ((null *if-stack*)
                         (cpp-fatal "#else without #if"))
                        ((null (car *if-stack*))
                         (cpp-fatal "#else #else"))
                        (t
                         (pop *if-stack*)
                         (let ((terminating-token (skip-if-section)))
                           (cond ((ident= "endif" terminating-token)
                                  nil)
                                 ((ident= "else" terminating-token)
                                  (cpp-fatal "#else #else?"))
                                 ((ident= "elif" terminating-token)
                                  (error "#else ... #elif?"))
                                 (t
                                  (error "Something very wrong.")))))))
                 ;;
                 ((ident= "endif" (car dir))
                  (cond ((null *if-stack*)
                         (cpp-fatal "#endif without #if"))
                        (t
                         (pop *if-stack*))))
                 ;;
                 ((ident= "elif" (car dir))
                  (cond ((null *if-stack*)
                         (cpp-fatal "#elif without #if"))
                        (t
                         ;; we are comming from a true section, thus everything until endif is false.
                         ;; pop if-stack
                         ;; again:
                         ;;   skip if-section
                         ;;   on #else ->
                         ;;      skip if-section
                         ;;      on #endif -> all fine
                         ;;      on #else -> error "#else #else"
                         ;;      on #elif -> error "#else #elif"
                         ;;   on #endif ->
                         ;;      done
                         ;;   on #elif ->
                         ;;      goto again
                         (pop *if-stack*)
                         (loop
                             (let ((q (skip-if-section)))
                               (cond ((ident= "else" q)
                                      (let ((q (skip-if-section)))
                                        (cond ((ident= "endif" q)
                                               (return))
                                              ((ident= "else" q)
                                               (cpp-fatal "#else #else"))
                                              ((ident= "elif" q)
                                               (cpp-fatal "#else #elif")))))
                                     ((ident= "endif" q)
                                      (return))
                                     ((ident= "elif" q)
                                      ;; continue
                                      nil)))))))
                 ;;
                 ((ident= "undef" (car dir))
                  (let ((xs (left-trim-whitespace (cdr dir))))
                    (cond ((or (null xs) (not (ident-token-p (car xs))))
                           (cpp-fatal "#undef without an identifer"))
                          (t
                           (let ((ys (left-trim-whitespace (cdr xs))))
                             (when ys
                               (cpp-fatal "Garbage at end of #undef."))
                             (setf (pp-macro-definition (car xs)) nil))))))
                 ((ident= "define" (car dir))
                  (process-define (cdr dir)))
                 ((ident= "include" (car dir))
                  (handle-include-directive (cdr dir)))
                 ((ident= "line" (car dir))
                  (handle-line-directive (cdr dir)))
                 ((ident= "error" (car dir))
                  (handle-error-directive (cdr dir)))
                 ((ident= "pragma" (car dir))
                  (handle-pragma-directive (cdr dir)))
                 ((number-token-p (car dir))
                  (handle-line-directive dir))
                 (t
                  (cpp-warning "Unknown preprocessor directive ~S."
                               (pp-token-text (car dir)))))))))

(defun if-directive-p (dir)
  "Is the token sequence 'dir' an #if, #ifdef or #ifndef directive?
   Returns two values: A boolean indicating whether 'dir' is such a
   directive and as second value a boolean that is the evaluation result
   of the test part of the directive."
  (labels ((eval-ifdef (dir)
             (let ((xs (left-trim-whitespace (cdr dir))))
               (cond ((or (null xs) (not (ident-token-p (car xs))))
                      (cpp-fatal "#ifdef without an identifer")
                      nil)
                     (t
                      (let ((ys (left-trim-whitespace (cdr xs))))
                        (when ys
                          (cpp-fatal "Garbage at end of #ifdef."))
                        (not (null (pp-macro-definition (car xs))))))))))
    ;;
    (cond ((ident= "if" (car dir))
           (values t (not (zerop (pp-eval-expr (pp-ac-expr (pp-parse-expression (cdr dir))))))))
          ((ident= "ifdef" (car dir))
           (values t (eval-ifdef dir)))
          ((ident= "ifndef" (car dir))
           (values t (not (eval-ifdef dir))))
          (t
           nil))))

(defvar *ignore-line-directives-p* nil)

(defun handle-line-directive (tokens)
  (unless *ignore-line-directives-p*
    (labels ((fail (fmt &rest args)
               (cpp-warning "Bad #line directive: ~S; ~?" tokens fmt args)
               (return-from handle-line-directive nil)))
      (setf tokens (expand-tokens tokens))
      (setf tokens (remove-if #'white-token-p tokens))
      (let (lineno filename)
        (cond ((< (length tokens) 1)
               (fail "Short"))
              ((and (not (number-token-p (car tokens)))
                    (every #'digit-char-p (pp-token-text (car tokens))))
               (fail "#line directive expects an unsigned integer, got ~S" (car tokens)))
              ((and (setf lineno (parse-integer (pp-token-text (car tokens))))
                    ;; (<= 1 lineno 2147483647) ;Sic!
                    (or (null (cdr tokens))
                        (and
                         (string-token-p (cadr tokens))
                         (setf filename (string-token-string (cadr tokens))))))
               (setf (pp-stream-line *pp-input-stream*) (1- lineno))
               (and filename (setf (pp-stream-file *pp-input-stream*) filename))
               nil)
              (t
               (fail "") ))))))

(defun handle-error-directive (tokens)
  (cpp-fatal "#error ~A" (verbatim (tokens-string tokens))))

(defun handle-pragma-directive (tokens)
  (let ((tokens (left-trim-whitespace tokens)))
    (cond ((and tokens (ident= "once" (car tokens)))
           (let ((pn (pp-stream-pathname *pp-input-stream*)))
             (cond ((null pn)
                    (note "~@<Hmm, #pragma once w/o a pathname for current input ~S~@:>"
                          (pp-stream-file *pp-input-stream*)))
                   ((pragma-once/deja-vu-p (progn 'truename (pp-stream-pathname *pp-input-stream*)))
                    '(note "~@<~2I#pragma once: deja vu with ~S; Which shouldn't happen.~@:>"
                          (pp-stream-pathname *pp-input-stream*)))
                   (t
                    (pragma-once/note (progn 'truename (pp-stream-pathname *pp-input-stream*)))))))
          (t
           ;; No luck, pass directive as verbatim as possible.
           (push 
            (make-pp-token :kind :punctuation ;hmm
                           :text (format nil "#pragma ~A" (verbatim (tokens-string tokens))))
            *lookahead*)
           ;;
           ))))

;;; #pragma once

(defun pragma-once/note (pathname)
  "Indicate that we see a \"#pragma once\" with the file _pathname_."
  '(note "#pragma once for ~S" (pp-stream-pathname *pp-input-stream*))
  (setf (header-file-seen-p pathname *defs*) t))

(defun pragma-once/deja-vu-p (pathname)
  "Have we seen \"#pragma one\" for this pathname already?"
  (header-file-seen-p pathname *defs*))

(defun pp-stream-pathname (&optional (pp-stream *pp-input-stream*))
  "Return the pathname of an pp-stream, if any."
  (cond ((pathnamep (pp-stream-file pp-stream)) (pp-stream-file pp-stream))
        ((and (stringp (pp-stream-file pp-stream))
              (not (eql 0 (search "<" (pp-stream-file pp-stream)))))
         (progn 'pathname (pp-stream-file pp-stream)))))

(defun handle-include-directive (tokens)
  ;; macro expanding either a string literal or a header token does
  ;; not hurt.
  (setf tokens (expand-tokens tokens))

  ;; Now, the method by which header tokens are generated from
  ;; something that is the result of macro expansion is not specified. 
  ;; So it is safe to remove white space right away.
  (setf tokens (remove-if #'white-token-p tokens))

  ;; When we have more than one token and the first token is "<" and
  ;; the last token is ">", make a header token.
  (cond ((and (> (length tokens) 1)
              (punct= "<" (first tokens))
              (punct= ">" (car (last tokens))))
         (setf tokens
               (list
                (make-pp-token :kind :header-name
                               :text (join-strings "" (mapcar #'pp-token-text tokens)))))))

  ;; Now we need exactly one header token, or exactly one string
  ;; literal token.
  (cond ((and (= 1 (length tokens))
              (string-token-p (car tokens)))
         (handle-include-directive-2 (string-token-string (car tokens)) t))
        ((and (= 1 (length tokens))
              (eql :header-name (pp-token-kind (car tokens))))
         (handle-include-directive-2 (header-name-token-string (car tokens)) nil))
        (t
         (cpp-warning "Bad #include directive.~%~S" tokens))))

(defun handle-include-directive-2 (filename quotedp &aux it)
  '(note "~@<Want to include ~A~A~A from ~S~@:>"
    (verbatim (if quotedp #\" #\<))
    (verbatim filename)
    (verbatim (if quotedp #\" #\>))
    (pp-stream-file *pp-input-stream*))
  (cond ((and quotedp
              (probe-file (setq it (if (pp-stream-pathname *pp-input-stream*)
                                       (merge-pathnames filename (pp-stream-pathname *pp-input-stream*))
                                       filename))))
         ;; ok. found it
         (unless (pragma-once/deja-vu-p (truename it))
           (push-input-file it)))
        ;; Some system include, regardless of whether this is quoted or not
        ;;
        ;; Use CC if wanted.
        (*use-cc-for-includes-p*
         (handle-system-include filename))
        ;; Try on our own
        (t
         (let ((real-filename (search-include-file filename)))
           (cond ((null real-filename)
                  (cpp-fatal "Cannot find include file ~S." filename))
                 (t
                  (when (pragma-once/deja-vu-p (truename real-filename))
                    '(note "Deja vu: ~S" real-filename))
                  (unless (pragma-once/deja-vu-p (truename real-filename))
                    (push-input-file real-filename))))))))

(defun search-include-file (filename)
  (loop for directory in *standard-include-directories* 
        for probe = (merge-pathnames filename directory)
        do (when (probe-file probe)
             (return probe))))

#-WINDOWS
(defun handle-system-include (filename)
  (multiple-value-bind (proc output)
      (run-program *cc* (append *cc-args* (list "-dD" "-E" "-"))
                          :error *error-output*
                          :input (make-string-input-stream
                                  (with-output-to-string (bag)
                                    (dump-macros bag :guard-by-ifndef t)
                                    (when filename
                                      (format bag "#include <~A>~%" (verbatim filename)))
                                    (write-line "#line 1 \"<internal>\"" bag)))
                          :wait nil
                          :output :stream
                          :external-format *cpp-default-file-encoding*)
    (declare (ignore proc))
    ;; One means to check for this being fine would be to wait for any input
    ;; and then check the process status.
    (push-input-stream output (format nil "<~A>" (verbatim filename)))))

#+(or)
(defun run-program (program arguments
                    &rest rest
                    &key wait
                         input
                         output
                         error
                         (external-format :utf-8)
                    &allow-other-keys)
  (declare (ignorable wait input output error external-format))
  (format t "~&# ~{~A~^ ~}~%" (cons program arguments))
  (force-output)
  #+CCL
  (let ((proc
         (apply #'ccl:run-program program arguments
                :if-output-exists :supersede
                :external-format external-format
                rest)))
    (when (eq :error (ccl:external-process-status proc))
      (error "~@<Cannot run system command - ~A~:@>"
             (join-strings " " (cons program arguments))))
    (values
     proc
     (ccl:external-process-output-stream proc)
     (ccl:external-process-error-stream proc)
     (ccl:external-process-input-stream proc)))
  #+SBCL
  (let ((proc
         (apply #'sb-ext:run-program program arguments
                :search t
                :external-format external-format
                rest)))
    (values
     proc
     (sb-ext:process-output proc)
     (sb-ext:process-error proc)
     (sb-ext:process-input proc))))

(defun run-program (&rest args)
  (apply #'noffi::run-program args))


;;;; -- Macro Expansion -----------------------------------------------------------------------

;; With regard to blue paint macro expansion is implementated with
;; regard to <u6-dnTCOxI4IVt3dRVn-sQ@comcast.com>.

;;; Macro parameter substition

;; We handle the GCC extension ", ## __VA_ARGS__" and the C++20 __VA_OPT__()
;; before doing the rest of parameter substitution. However, note that there
;; is subtle difference in ,##__VA_ARGS__ and __VA_OPT__ with respect to empty
;; arguments.

;; With GCC only the &rest being empty results in ",##__VA_ARGS__" omitting
;; the comma. For C++20 the empty rest list and the empty argument are the
;; same.

(defun insert-arguments (macro-name tokens params args)
  ;;
  ;;
  ;; Inserts arguments into a macro body and also does the ## splicing.
  (let ((the-rest-parameter nil)
        (rest-arg-present-p nil))
    (labels ((construct-bindings (ps as)
               (cond ((eq (car ps) '&rest)
                      (setq rest-arg-present-p (not (null as)))
                      (list
                       (list (setq the-rest-parameter (cadr ps))
                             (trim-white-space
                              (loop for a on as
                                    append (car a)
                                    unless (null (cdr a))
                                    collect (make-punct-token ",")))
                             nil nil)))
                     ((null ps)
                      (cond ((and (null (trim-white-space (car as)))
                                  (null (cdr as)))
                             nil)
                            (t
                             (cpp-warning "Too many arguments to macro ~A." (verbatim (pp-token-text macro-name)))
                             nil)))
                     ((null as)
                      ;; Here is an ugly corner case. We treat "f()" as 'f' being invoked with no
                      ;; argumens at all. This is needed for __VA_OPT__, however for the purpose of
                      ;; a macro taking just one argument this should be ok.
                      (cond ((= 1 (length params))
                             (list (list (car ps) (trim-white-space (car as)) nil nil)))
                            (t
                             (cpp-warning "Too few arguments to macro ~A." (verbatim (pp-token-text macro-name))))))
                     ((cons (list (car ps) (trim-white-space (car as)) nil nil)
                            (construct-bindings (cdr ps) (cdr as)))))))
      (let ((bindings (construct-bindings params args)))
        ;;
        (labels ((find-param (tok)
                   (find tok bindings :key #'car))
                 ;;
                 (expand (tok verbatim-p)
                   (let ((it (find-param tok)))
                     (cond ((null it) (list tok))
                           (verbatim-p (cadr it))
                           ((null (third it))
                            (setf (third it) t
                                  (fourth it)
                                  (expand-tokens (second it)))) ;hmm
                           ((fourth it)))))
                 ;;
                 (insert-strings (xs &aux ys)
                   (cond ((null xs) nil)
                         ((and (punct= "#" (car xs))
                               (setf ys (left-trim-whitespace (cdr xs)))
                               (find-param (car ys)))
                          (cons (pp-stringify (expand (car ys) t))
                                (insert-strings (cdr ys))))
                         ((cons (car xs) (insert-strings (cdr xs))))))
                 ;;
                 (walk (xs &aux ys zs)
                   (cond ((null xs) nil)
                         ;; Are we at x W* ## ...
                         ((and (not (white-token-p (car xs)))
                               (setf ys (left-trim-whitespace (cdr xs)))
                               (punct= "##" (car ys)))
                          ;;
                          (setf zs (left-trim-whitespace (cdr ys)))
                          ;; Special case for ,##__VA_ARGS__
                          (cond ((and (punct= "," (car xs))
                                      (eq (car zs) the-rest-parameter))
                                 (cond (rest-arg-present-p
                                        (cons (car xs) (walk zs)))
                                       (t
                                        (walk zs))))
                                (t
                                 ;;
                                 (labels ((do-splice (x y rest)
                                            ;; Is there a next splicer?
                                            (let ((next (let ((q (left-trim-whitespace rest)))
                                                          (and (punct= "##" (car q))
                                                               (left-trim-whitespace (cdr q))))))
                                              (setf x (remove-if #'consp x))
                                              (setf y (remove-if #'consp y))
                                              (let ((this (cond ((null x) y)
                                                                ((null y) x)
                                                                ((append (butlast x)
                                                                         (splice-tokens (car (last x)) (car y))
                                                                         (cdr y))))))
                                                (cond ((not (null next))
                                                       (append (butlast this)
                                                               (do-splice
                                                                   (last this)
                                                                 (expand (car next) t) ;??? Why 't' here
                                                                 (cdr next))))
                                                      (t
                                                       (append this
                                                               (walk rest))))))))
                                   (do-splice (expand (car xs) t) (and zs (expand (car zs) t)) (cdr zs))))))
                         ((punct= "##" (car xs))
                          ;; this happens, when the macro body starts in "##"
                          (walk (cdr xs)))
                         (t
                          (append (expand (car xs) nil) (walk (cdr xs)))))))
          (setf tokens (insert-strings tokens))
          (when the-rest-parameter
            (setf tokens (expand-va-opt tokens rest-arg-present-p)))
          (walk tokens))))))

(defun expand-va-opt (tokens have-remaining-args-p)
  "Expand __VA_OPT__(..) in a macro body. `have-remaining-args-p' says
whether we assume that rest arguments are present or not."
  (cond ((null tokens) nil)
        ((ident= "__VA_OPT__" (car tokens))
         (multiple-value-bind (opt-arg more)
             (chop-paren-balanced-group (cdr tokens))
           (append (if have-remaining-args-p opt-arg nil) (expand-va-opt more have-remaining-args-p))))
        (t
         (cons (car tokens) (expand-va-opt (cdr tokens) have-remaining-args-p)))))

(defun expand-macro (macro-name dumper q def)
  ;; ### result versus calling dumper?
  (ecase (macro-definition-kind def)
    (:object
     (let ((exp (insert-arguments macro-name (macro-definition-body def) nil nil)))
       (if *kill-newlines-p* (kill-newlines exp) exp)))
    (:function
     (cond ((looking-at-lparen-p)
            (multiple-value-bind (is-call-p arguments)
                (read-macro-arguments q)
              (cond (is-call-p
                     (let ((exp (expand-function-macro macro-name def arguments)))
                       (setf exp (if *kill-newlines-p* (kill-newlines exp) exp))
                       exp))
                    (t
                     (when *debug-blue-paint*
                       (warn "Not a macro invokation, dumping ~S, returning ~S."
                             q arguments))
                     (funcall dumper q)
                     arguments))))
           (t
            (funcall dumper q)          ;hmm
            nil)))
    (:lambda
     (funcall (macro-definition-body def)))))

(defun read-macro-arguments (macro-name)
  (let ((xs nil) (nest 0) args)
    ;; look for "("
    (let ((ys nil))
      (loop for q = (next-token)
            do (push q ys)
            until (or (eof-token-p q) (and (not (consp q)) (not (white-token-p q)))))
      (cond ((punct= "(" (car ys))
             ;; read arguments
             (values
              t
              (labels ((proc-arg ()
                         (push (trim-white-space (reverse xs)) args)
                         (setf xs nil)))
                (loop for q = (next-token)
                      do (when (eof-token-p q)
                           (cpp-fatal "EOF in arguments to ~S" macro-name))
                      until (and (eql nest 0) (punct= ")" q))
                      do
                      (cond ((punct= "(" q)
                             (push q xs)
                             (incf nest))
                            ((punct= ")" q)
                             (push q xs)
                             (decf nest))
                            ((and (punct= "," q) (zerop nest))
                             (proc-arg))
                            (t
                             (push q xs))))
                (proc-arg)
                ;; Special case: f() does not have an empty argument, but none.
                (cond ((equal args '(nil))
                       nil)
                      (t
                       (reverse args))))))
            (t
             ;; no arguments
             (values nil (remove-if #'eof-token-p (reverse ys))))))))

(defun expand-tokens (tokens)
  ;; make sure directives are not processed again.
  (let ((*lookahead* tokens)
        (*pp-input-stream* (make-pp-stream (make-concatenated-stream)))
        (*pp-char-lookahead-tri* nil)
        (*if-stack* nil)
        (*pp-input-stack* nil)
        (*flag-stack* nil)
        (*bol-p* nil)
        res)
    (expand-loop #'(lambda (tok) (push tok res)))
    (setf res (reverse res))
    (destroy-pp-stream *pp-input-stream*)
    res))

(defun paint-blue (ident-token)
  "Apply blue paint to a token."
  (make-pp-token
   :kind :blue-painted
   :text (if *debug-blue-paint*
             (concatenate 'string (pp-token-text ident-token) "'")
             (pp-token-text ident-token))))

(defun expand-loop (dumper)
  (loop for q = (expand-one)
        until (and q (eof-token-p (car q)))
        do (mapc dumper q)))

(defun expand-one (&aux it)
  ;; -> list of tokens
  (when *debug-blue-paint*
    (warn "expand-one in: LA = ~S, stack = ~S." *lookahead* *flag-stack*))
  ;; 
  (let ((from-source-p (null *lookahead*))
        (q (next-token)))
    (cond ((ident-token-p q)
           (cond ((and *inside-if-directive-p*
                       (ident= "defined" q))
                  ;; This is actually UB
                  (let ((qs (fetch-defined-arg)))
                    ;; We leave the 'defined' as well as the argument intact for further
                    ;; processing.
                    (cond (qs (cons q qs))
                          (t (list q)))))
                 ;; I really wonder, if this is needed anymore.
                 ((member q *flag-stack*)
                  (when t ;; *debug-blue-paint*
                    (warn "painting ~S, from-source-p = ~S." q from-source-p))
                  ;; apply blue paint
                  (setf q (paint-blue q))
                  (list q))
                 ;;
                 ((setf it (pp-macro-definition q))
                  ;; Now, the hairy thing, What can happen, is that we move
                  ;; beyond a higher sentinel before re-expansion.
                  ;; expand macro
                  (let* ((*flag-stack* (cons q *flag-stack*))
                         (sentinel (cons :sentinel *flag-stack*)))
                    (let* ((res nil)
                           (exp (expand-macro q (lambda (q) (push q res)) q it)))
                      ;; NEW
                      (setq exp (mapcar (lambda (q)
                                          (if (member q *flag-stack*)
                                              (paint-blue q)
                                              q))
                                        exp))
                      (when *debug-blue-paint*
                        (warn "Macro ~S => ~S | ~S" q exp *flag-stack*))
                      ;;
                      (append (reverse res)
                              (progn
                                (setf *lookahead* (append exp (list sentinel) *lookahead*))
                                (prog1
                                    (loop until (eq (car *lookahead*) sentinel)
                                          append (expand-one))
                                  (pop *lookahead*)))))))
                 (t
                  (list q))))
          (t
           (list q)))))

(defun looking-at-lparen-p ()
  (let ((ys nil))
    (loop for q = (let ((*silent-sentinel* nil)) (next-token))
       do (push q ys)
       until (or (eof-token-p q)
                 (and (not (consp q))
                      (not (white-token-p q)))))
    (setf *lookahead* (append (reverse ys) *lookahead*))
    (punct= "(" (car ys))))

(defun fetch-defined-arg ()
  "Fetches either <ident> or '(' <ident> ')' and returns the ident.
When sth else is seen whine and return NIL."
  (let ((ys nil))
    (labels ((skip ()
               "Skip WSP, first non-white-space end up in (car ys)."
               (loop for q = (let (#+NIL(*silent-sentinel* nil)) (next-token))
                     do (push q ys)
                     until (or (eof-token-p q)
                               (and (not (consp q))
                                    (not (white-token-p q)))))))
      ;;
      (skip)
      (cond ((punct= "(" (car ys))
             (skip)
             (when (ident-token-p (car ys))
               (skip)
               (when (punct= ")" (car ys))
                 ;; We won, return what we got and put nothing back.
                 (return-from fetch-defined-arg (reverse ys)))))
            ((ident-token-p (car ys))
             ys)
            (t
             ;; No luck. Put back what we have and call it someone's else problem.
             (setf *lookahead* (append (reverse ys) *lookahead*))
             nil)))))

(defun expand-function-macro (macro-name definition args)
  (insert-arguments macro-name
                    (macro-definition-body definition)
                    (macro-definition-parameters definition)
                    args))


;;;; -- Driver --------------------------------------------------------------------------------

(defun cpp (input-filename
            &key (output
                  (make-pathname :type "i" :defaults input-filename))
                 (dump-macros nil)
                 (omit-line-numbers-p nil)
                 (keep-macros-p nil))
  (labels ((doit ()
             (with-open-file (input input-filename)
               (prog1
                   (cond ((eql t output)
                          (princ
                           (with-output-to-string (output)
                             (cpp-aux input output :omit-line-numbers-p omit-line-numbers-p)))
                          (values))
                         ((streamp output)
                          (cpp-aux input output :omit-line-numbers-p omit-line-numbers-p)
                          (values))
                         (t
                          (with-open-file (output output :direction :output :if-exists :supersede)
                            (cpp-aux input output :omit-line-numbers-p omit-line-numbers-p)
                            (fresh-line output)))) ))
             (when dump-macros
               (terpri)
               (dump-macros))))
    (if keep-macros-p
        (doit)
        (let ((*defs* (make-defs-table)))
          (doit))))
  output)

(defun dump-macros (&optional (output *standard-output*)
                    &key (guard-by-ifndef nil)
                         (guard-by-undef nil)
                         (include-lambda nil))
  (map-macro-definitions
   (lambda (k v)
     (when v                            ;Huh???
       (let ((name (pp-token-text k)))
         (when (or include-lambda (not (eql :lambda (macro-definition-kind v))))
           (when guard-by-undef
             (format output "#ifdef ~A~%#undef ~A~%#endif~%" (verbatim name) (verbatim name)))
           (when guard-by-ifndef
             (format output "#ifndef ~A~%" (verbatim name)))
           (print-macro-def name v output)
           (terpri output)
           (when guard-by-ifndef
             (format output "#endif~%"))))))
   *defs*))

(defun print-macro-def (name def &optional (output *standard-output*))
  (when (pp-token-p name)
    (setq name (pp-token-text name)))
  (labels ((aux (name v output)
             (let ((sloc (macro-definition-sloc def)))
               (when sloc
                 (format output "#line ~D ~S~%" (cadr sloc) (verbatim (car sloc)))))
             (ecase (macro-definition-kind v)
               (:object
                (format output "#define ~A ~A"
                        (verbatim name)
                        (join-strings "" (mapcar #'pp-token-text (macro-definition-body v)))))
               (:function
                (format output "#define ~A(" (verbatim name))
                (do ((q (macro-definition-parameters v) (cdr q))
                     (c nil t))
                    ((null q))
                  (and c (write-string ", " output))
                  (cond ((eq '&rest (car q))
                         (let ((p (cadr q)))
                           (pop q)
                           (unless (eq p (make-ident-token "__VA_ARGS__"))
                             (write-string (pp-token-text p) output))
                           (write-string "..." output)))
                        (t
                         (write-string (pp-token-text (car q)) output))))
                (format output ") ~A" (join-strings "" (mapcar #'pp-token-text (macro-definition-body v)))))
               ((:lambda)
                (format output "#define ~A ~A"
                        (verbatim name)
                        (join-strings "" (mapcar #'pp-token-text (funcall (macro-definition-body v)))))))))
    (etypecase output
      ((or stream string) (aux name def output))
      ((eql nil) (with-output-to-string (output) (aux name def output))))))

(defun cpp-reset ()
  "Use this to reset all definitions."
  (setf *defs* (make-defs-table)))

(defun cpp-do (input)
  (with-output-to-string (output)
    (etypecase input
      (string
       (with-input-from-string (input input)
         (cpp-aux input output :omit-line-numbers-p nil)))
      (pathname
       (with-open-file (input input)
         (cpp-aux input output :omit-line-numbers-p nil))))))

(defun cpp-do-1 (string)
  (with-input-from-string (input string)
    (cpp-aux input nil :omit-line-numbers-p t)))


;;;; ------------------------------------------------------------------------------------------

#-NIL
(progn
 #+CMU
 (defun bench ()
   (eval '(profile:profile-all :package :de.bauhh.cpp))
   (time (frob))
   (eval '(profile:report-time))
   (eval '(profile:unprofile)))

 #+CMU
 (defun bench-2 ()
   (eval '(profile:profile-all :package :de.bauhh.cpp))
   (test-2)
   (eval '(profile:report-time))
   (eval '(profile:unprofile)))

 (defun frob ()
   (cpp-reset)
   (let ((*standard-include-directories*
          (list*
           "/usr/include/gtk-2.0/"
           "/usr/lib/gtk-2.0/include/"
           "/usr/include/atk-1.0/"
           "/usr/include/cairo/"
           "/usr/include/pango-1.0/"
           "/usr/include/glib-2.0/"
           "/usr/lib/glib-2.0/include/"
           "/usr/include/freetype2/"
           "/usr/include/directfb/"
           "/usr/include/libpng12/"
           "/usr/include/pixman-1/"
           *standard-include-directories*)))
     (cpp-do-1 "#include <gtk/gtk.h>"))
   (values))
 )


;;;; ------------------------------------------------------------------------------------------

;; :punctuation  +
;; :ident       foo
;; :header-name <foo.h>
;; :number      1234

;; PP PI PH PN
;; IP II IH IN
;; HP HI HH HN
;; NP NI NH NN

;; PP    PH PN
;;          IN
;; HP    HH HN
;; NP NI NH NN

(defun white-space-needed-between-tokens (a b)
  ;; Bug: "/" "// foo"
  (let ((ak (pp-token-kind a))
        (bk (pp-token-kind b)))
    ;;
    (when (eql ak :blue-painted) (setf ak :ident))
    (when (eql bk :blue-painted) (setf bk :ident))
    ;;
    (cond
      ((and (punct= "#" a))
       nil)
      ((and (eql ak :ident) (punct= "(" b))
       nil)
      ((eql ak :newline)
       nil)
      ((eql bk :comment)
       t)
      ((or (member ak '(:string :char :wsp :newline :comment))
           (member bk '(:string :char :wsp :newline :comment)))
       ;; now white space is needed between any of these.
       nil)
      ((punct= "(" a) nil)
      ((punct= ")" b) nil)
      ((punct= "[" a) nil)
      ((punct= "]" b) nil)
      ((punct= ";" b) nil)
      ((punct= "," b) nil)
      ;;
      ((or (eql ak :punctuation) (eql bk :punctuation))
       t)
      ;;
      ((or (and (eql ak :ident) (member bk '(:punctuation :header-name)))
           (and (eql bk :ident) (member ak '(:punctuation :header-name))))
       ;; Identifier and punction never merge accidently
       nil)
      ((and (eql ak :ident) (eql bk :ident))
       ;; These definitly merge
       t)
      (t
       ;; The expensive version
       (eql 1 (length (string-to-tokens (concatenate 'string (pp-token-text a) (pp-token-text b))))) ))))

(defun cpp-aux (input output &key (omit-line-numbers-p nil))
  (let ((ofile nil)
        (oline -100)
        (last-tok nil)
        (last-nline 1)
        (last-nfile (or (ignore-errors (namestring (pathname input)))
                        "<stdin>")))
    (let ((*pp-input-stream* (make-pp-stream input :file last-nfile))
          (*pp-char-lookahead-tri* nil)
          (*pp-input-stack* nil))
      (let ((*bol-p* t)
            (*flag-stack* nil)
            (*lookahead* nil)
            (*if-stack* nil))
        (expand-loop #'(lambda (x)
                         (and output
                              (cond ((newline-token-p x)
                                     (cond (omit-line-numbers-p
                                            (terpri output))
                                           (t
                                            (setf last-nline (pp-stream-line *pp-input-stream*)
                                                  last-nfile (pp-stream-file *pp-input-stream*))))
                                     (setf last-tok x))
                                    ((eql :wsp (pp-token-kind x))
                                     (when *cpp-preserve-white-space-p*
                                       (write-string (pp-token-text x) output)))
                                    (t
                                     (let ((str (pp-token-text x)))
                                       (when (and last-nline (not omit-line-numbers-p))
                                         (fresh-line output)
                                         (incf oline)
                                         (cond ((not (eq ofile last-nfile))
                                                (if *use-short-line-directives-p*
                                                    (format output "# ~D ~S~%"
                                                            last-nline last-nfile)
                                                    (format output "#line ~D ~S~%"
                                                            last-nline last-nfile)))
                                               ((= last-nline oline))
                                               (t
                                                (let ((delta (- last-nline oline)))
                                                  (cond ((<= 0 delta *newline-compress-threshold*)
                                                         (loop repeat delta do (terpri output)))
                                                        (t
                                                         (if *use-short-line-directives-p*
                                                             (format output "# ~D ~S~%"
                                                                     last-nline last-nfile)
                                                             (format output "#line ~D ~S~%"
                                                                     last-nline last-nfile)))))))
                                         (setf ofile last-nfile
                                               oline last-nline))
                                       (when (and (not *cpp-preserve-white-space-p*)
                                                  last-tok
                                                  (white-space-needed-between-tokens last-tok x))
                                         (princ " " output))
                                       (write-string str output)
                                       (setf last-tok x)
                                       (setf last-nline nil) ))) ))))
      (destroy-pp-stream *pp-input-stream*))))

;;;; ------------------------------------------------------------------------------------------

(defun fast-skipper (stream &optional (bol t))
  ;; TODO: Actually we don't need to watch '...' and "..." as newlines are
  ;; not allowed there.
  (let (c)
    (tagbody
     loop
        (setf c (pp-read-char stream))
     again
        (cond #+NIL ((char= #\" c)
               (setf bol nil)
               (loop for c = (pp-read-char stream)
                     until (eql c #\")
                     do (when (eql c #\\)
                          (pp-read-char stream))))
              #+NIL ((char= #\' c)
               (setf bol nil)
               (loop for c = (pp-read-char stream)
                     until (eql c #\')
                     do (when (eql c #\\)
                          (pp-read-char stream))))
              ((char= #\/ c)
               (setf c (pp-read-char stream))
               (cond ((char= #\/ c)
                      (loop for c = (pp-read-char stream)
                            until (eql c #\newline))
                      (setf bol t))
                     ((char= #\* c)
                      (setf bol nil)
                      (loop for c = (pp-read-char stream) do
                            (when (and (eql c #\*)
                                       (eql #\/ (pp-peek-char stream)))
                              (pp-read-char stream)
                              (return))))
                     (t
                      (go again))))
              ((and bol (char= #\# c))
               ;; (unless bol (cpp-fatal "Missed BOLness?"))
               (go done))
              ((char= #\newline c) (setf bol t))
              ((or (char= c #\space) (char<= #\tab c #\return))) ;No change in BOLness
              (t
               (setf bol nil)))
       (go loop)
     done
        )))

;;;; ------------------------------------------------------------------------------------------

(cpp-reset)                             ;Hmm

(defparameter *read-files* nil)

;;;; ------------------------------------------------------------------------------------------

;; Let's start with just skipping comments. We have a buffer, with a
;; fill and read pointer. We want to copy as large chunks from the
;; buffer to the output as possible. We would have:

;; loop
;;   if looking at "/*" ?
;;       dump until the current read position
;;       kill dump pointers
;;       read until "*/" seen.
;;       set dump-start and dump-end to reading pointer
;;   else
;;       consume character
;;       increment dump-end
;; end

;; With BOL and directive detection

;; bol <- true
;;
;; loop
;;   if looking at "/*" ?
;;       dump until the current read position
;;       kill dump pointers
;;       read until "*/" seen.
;;       set dump-start and dump-end to reading pointer
;;   elif newline
;;       consume character
;;       increment dump-end
;;       bol <- true
;;   elif white space
;;       consume character
;;       increment dump-end
;;   elif '#' and bol
;;       enter what ever directive processing we do.
;;   else
;;       consume character
;;       increment dump-end
;;       bol <- false
;; end
;;

;; bol <- true                          // whether at beginning of line
;; ident <- nil                         // last identifier seen
;; wsp-flag <- false                    // when identifier seen, whether white space was seen

;; peekc() -> current char
;; consume()
;; dump_on()                            // consume will dump the character
;; dump_off()                           // consume will be mute

(defun note (format &rest args)
  (format *trace-output* "~&~<;; ~@;~?~:>~%" (list format args)))

;; For 'ident' + (white | comment)* + '(' it will get more
;; complicated:

;; Like 'bol', we need a flag 'identifier-seen' and keep it stable
;; while we see white or comment. Once we see '(' with identifier-seen
;; be true, we need possibly invoke macro processing. Do we see
;; something different, we clear identifier-seen.

;; However: We do not what to continue dumping until we know, if this
;; is a macro invokation or not. But: We are not obligated to preserve
;; any white space, but it will in general not hurt.

;; Therefore, we can safely just remember the identifier and whether
;; there was white and just dump as apropriate.

;; During #if skipping, we would even _only_ look for character or
;; string constants, white space and comments. Thus we would rather
;; quickly find the appropriate next #if, #else, #endif, #elif
;; directive.

;;;; ------------------------------------------------------------------------------------------

;; Why won't we use the regular C lexer? Mind you: The preprocessor
;; only is a transducer on the token sequence from the c-lexer. Our
;; new C lexer is quite fast.

;;;; -- The Main Loop -------------------------------------------------------------------------

;; Although the C preprocessor is specifed of a transducer, that is an
;; automation taking a token sequence and outputing another, we
;; implement the main loop with the idea, that the preprocessor would
;; mainly be copying characters from the input file to the output
;; file.

;; So the basic idea is, to have a FA, which just recognizes
;; directives and identifers to be expanded.



;; We have two situations, where we collect actual tokens:

;; 1. Within a directive, terminated by #\Newline
;; 2. Within a macro invokation, terminated by matching ')'

;; We could ease the task, when we have a three character lookahead, like

;; This is easy to have, we just fill our buffer in such a way that at
;; least three characters are available. Or, we do the hard way and
;; have one clause return more than one token.

;;;; -- System Include Files ------------------------------------------------------------------

(defparameter *n2176-punct*
  '("[" "]" "(" ")" "{" "}" "." "->" "++" "--" "&" "*" "+" "-" "~" "!" "/" "%"
    "<<" ">>" "<" ">" "<=" ">=" "==" "!=" "^" "|" "&&" "||" "?" ":" ";" "..."
    "=" "*=" "/=" "%=" "+=" "-=" "<<=" ">>=" "&=" "^=" "|=" "," "#" "##"
    "<:" ":>" "<%" "%>" "%:" "%:%:"))

;; The easiest is to handle __VA_OPT__ beforehand.


;;;; System Include Files

(defun random-string (&optional (size 20))
  (let ((*print-base* 36)) (format nil "~v,,,'0A" size (random (expt 36 size)))))

#||

;; work in progress

(defun handle-system-include (include-filename)
  (note "<~A>" include-filename)
  ;; Poor man's temponary file, just keep guessing a filename until we are fine.
  (multiple-value-bind (c-stream c-pathname i-pathname)
      (loop
        (let* ((guess (with-standard-io-syntax
                        (let ((*print-base* 36))
                          (format nil "~20,,,'0A" (random (expt 36 20))))))
               (c-pathname (make-pathname :name guess :type "c"))
               (i-pathname (make-pathname :name guess :type "i"))
               (c-stream (open c-pathname
                               :if-exists nil :if-does-not-exist :create :direction :output))
               (i-stream (open i-pathname
                               :if-exists nil :if-does-not-exist :create :direction :output)))
          (when (and c-stream i-stream)
            (close i-stream)
            (return (values c-stream c-pathname i-pathname)))))
    ;;
    (dump-macros c-stream :guard-by-ifndef t)
    (when include-filename
      (format c-stream "#include <~A>~%" include-filename))
    (close c-stream)
    ;;
    ;;    
    (run-program "cl.exe" (list "/Zc:preprocessor" "/E" "/P" "/PD"
                                (namestring (truename c-pathname)))
                 :error *error-output*
                 :output *standard-output*
                 :wait t
                 ;; :output :stream
                 ;; :if-output-exists :supersede
                 ;; :external-format *cpp-default-file-encoding*
                 )
    (ignore-errors (delete-file c-pathname))
    (push-input-stream (open i-pathname :external-format *cpp-default-file-encoding*)
                       (format nil "<~A>" "internal"))))

||#


;;;;

#||

(defparameter *search-include-file-cache*
  (make-hash-table :test #'equal))

(defun find-system-include-file (filename)
  (let* ((sentinel "_Noffi__sentinel__")
         (temp-file-name
          (loop repeat 100
                do (let ((n (format nil "T-~A.c" (random-string))))
                     (with-open-file (o n :direction :output :if-exists nil)
                       (unless o (return-from find-system-include-file nil))
                       (format o "~A~%" sentinel)
                       (format o "#include <~A>~%" filename))
                     (return n)))))
    (print (namestring (truename temp-file-name)))
    (let ((putative-line-dir
           (unwind-protect
                (multiple-value-bind (proc stream)
                    (run-program "cl.exe" (list "/nologo" "/Zc:preprocessor" "/E" (namestring (truename temp-file-name)))
                                 :output :stream
                                 :error (make-broadcast-stream)
                                 :wait nil)
                  (declare (ignore proc))
                  (unwind-protect
                       (prog1
                           (and (loop repeat 10
                                      for line = (remove #\return (read-line stream nil nil))
                                      do (cond ((null line) (return nil))
                                               ((string-equal line sentinel) (return t))))
                                (remove #\return (read-line stream nil nil)))
                         (close stream)
                         (loop while (eq :running (ccl:external-process-status proc)) do (sleep 1/1000))
                         (ignore-errors (delete-file temp-file-name)))
                    (close stream :abort t)))
             '(ccl:process-run-function "garbage man"
                                       (lambda ()
                                         (loop while (probe-file temp-file-name)
                                               do (ignore-errors (delete-file temp-file-name))
                                               do (sleep 1/100)))))))
      ;;
      (and putative-line-dir
           (clex2::with-match ((and "#line 1 " #\" (= it (* (- t #\"))) #\") putative-line-dir)
             (probe-file (deescape-c-string it)))))))

(defun search-include-file (filename quotedp)
  (cond ((and quotedp
              ;; Don't merge when this is magic <stdin>
              (eql 0 (search "<" (pp-stream-file *pp-input-stream*))))
         filename)
        ((and quotedp (probe-file (merge-pathnames filename (pp-stream-file *pp-input-stream*))))
         (merge-pathnames filename (pp-stream-file *pp-input-stream*)))
        (t
         #+NIL
         (loop for directory in *standard-include-directories* 
               for probe = (merge-pathnames filename directory)
               do (when (probe-file probe)
                    (return probe)))
         (or (gethash filename *search-include-file-cache*)
             (setf (gethash filename *search-include-file-cache*)
                   (find-system-include-file filename))))))

||#

(defun curry (fun &rest args) #'(lambda (&rest more) (apply fun (append args more))))
(defun rcurry (fun &rest args) #'(lambda (&rest more) (apply fun (append more args))))
(defun split-by-if (predicate seq &key (start 0) (nuke-empty-p nil))
  (let ((p0 (position-if predicate seq :start start)))
    (if p0
        (if (and nuke-empty-p (= start p0))
            (split-by-if predicate seq :start (+ p0 1) :nuke-empty-p nuke-empty-p)
          (cons (subseq seq start p0)
                (split-by-if predicate seq :start (+ p0 1) :nuke-empty-p nuke-empty-p)))
      (if (and nuke-empty-p (= start (length seq)))
          nil
        (list (subseq seq start))))))
(defun split-by (item &rest args) (apply #'split-by-if (curry #'eql item) args))
(defun split-by-member (items &rest args) (apply #'split-by-if (rcurry #'member items) args))


;;;; -- Microsoft Visual C --------------------------------------------------------------------

;; With Microsoft Visual C we face the problem that we cannot use "CL /E"
;; because of '#pragma once' which are not guarded by #ifndef/#define/#endif.
;; So we are on our own. We still need CL.EXE though to get at the predefines
;; #define's.

#+CCL
(defun mswin-boot-include-path ()
  (setq *standard-include-directories*
        (mapcar (lambda (s) (pathname (format nil "~A\\" (verbatim s))))
                (noffi::split-by #\; (ccl:getenv "INCLUDE") :nuke-empty-p t))))

(defun native-namestring (pathname)
  ;; There is a better implementation in UIOP
  #+CCL (ccl:native-translated-namestring (pathname pathname))
  #-CCL (namestring (pathname pathname)) ;we hope for the best.
  )

(defun native-namestring-for-command-arguments (pathname)
  (native-namestring pathname))

#+(AND CCL WINDOWS NOFFI-USES-MSVC)
(defun get-predefined-macros ()
  (let ((temp-stream (noffi::get-temporary-file-stream :type "c" :external-format *cpp-default-file-encoding*)))
    (unwind-protect
         (progn
           (close temp-stream)
           ;;
           (let* ((output (make-string-output-stream))
                  (command  (list "cl.exe" "/nologo" "/Zc:preprocessor" "/E" "/PD" (native-namestring-for-command-arguments temp-stream)))
                  (error-output (make-string-output-stream))
                  (proc (ccl:run-program (car command) (cdr command)
                                         :error error-output
                                         :output output
                                         :wait t
                                         :external-format *cpp-default-file-encoding*)))
             (tagbody
              :again
                (multiple-value-bind (status code) (ccl:external-process-status proc)
                  (case status
                    ((:exited)
                     (unless (eql 0 code)
                       (error "~<Running shell command ~S exited with status code ~D and said:~:@>~%~%~@<  ~@;~{~A~^~%~}~:>"
                              (list command code)
                              (mapcar #'verbatim
                                      (split-by #\newline (get-output-stream-string error-output))))))
                    ((:error)
                     (error "~@<Running shell command ~S failed entirely with code ~D. Make sure that ~S is in your PATH.~:@>"
                            command code (car command)))
                    ((:running)
                     (sleep 1/100)
                     (go :again))
                    (t
                     (error "~@<What now? Running shell command ~S failed somehow; status = ~S, status code ~D.~:@>"
                            command status code)))))
             (get-output-stream-string output)))
      (progn
        (ignore-errors (close temp-stream))
        (ignore-errors (delete-file temp-stream))))))

#+(AND WINDOWS CCL NOFFI-USES-MSVC)
(defun handle-system-include (filename)
  (unless (predefines-known-p *defs*)
    (cpp-aux (make-string-input-stream (get-predefined-macros)) (make-broadcast-stream))
    (setf (predefines-known-p *defs*) t))
  (unless (ccl:getenv "INCLUDE")
    (error "~@<Your environment is missing the %INCLUDE% environment variable.~:@>~%~%~
            ~@<This is a configuration error. Make sure that VCVARSALL.BAT is run to set up all needed environment variables ~
               before you invoke your Lisp.~:@>"))
  (let ((hfilename
         (let ((include-path
                (mapcar (lambda (s) (pathname (format nil "~A\\" (verbatim s))))
                        (noffi::split-by #\; (ccl:getenv "INCLUDE") :nuke-empty-p t))))
           (loop for directory in include-path
                 for probe = (merge-pathnames filename directory)
                 do (when (probe-file probe) (return probe))
                 finally
                 (error "I cannot find ~S in any of these include directories:~%~%~<    ~@;~@{~S~^~%~}~:>"
                        filename
                        include-path)))))
    (when (pragma-once/deja-vu-p (truename hfilename))
      (note "Deja vu: ~S" hfilename))
    (unless (pragma-once/deja-vu-p (truename hfilename))
      (push-input-file hfilename))))

#+(or)
(defun handle-system-include (filename)
  (unless (predefines-known-p *defs*)
    (cpp-aux (make-string-input-stream (get-predefined-macros)) (make-broadcast-stream))
    (setf (predefines-known-p *defs*) t))
  ;;
  (let ((temp-stream (noffi::get-temporary-file-stream :type "c" :external-format *cpp-default-file-encoding*))
        (sentinel "__noffi_sentinel__"))
    (unwind-protect
         (progn
           (write-line sentinel temp-stream)
           (format temp-stream "#include <~A>~%" (verbatim filename))
           (close temp-stream)
           ;;
           (let* ((command (list "cl.exe" "/nologo" "/Zc:preprocessor" "/E" (native-namestring-for-command-arguments temp-stream)))
                  (error-output (make-string-output-stream))
                  (proc (ccl:run-program (car command) (cdr command)
                                         :error error-output
                                         :output :stream
                                         :wait nil
                                         :external-format *cpp-default-file-encoding*))
                  line-line)
             (let ((istream (ccl:external-process-output-stream proc)))
               (unwind-protect
                    (progn
                      (loop for line = (read-line istream) until (equal line sentinel))
                      (setq line-line (read-line istream)))
                 (close istream)))
             (tagbody
              :again
                (multiple-value-bind (status code) (ccl:external-process-status proc)
                  (case status
                    ((:exited)
                     #+NIL
                     (unless (eql 0 code)
                       (error "~<Running shell command ~S exited with status ~S and code ~D and said:~:@>~%~%~@<  ~@;~{~A~^~%~}~:>"
                              (list command status code)
                              (mapcar #'verbatim
                                      (split-by #\newline (get-output-stream-string error-output))))))
                    ((:error)
                     (error "~@<Running shell command ~S failed entirely with code ~D. Make sure that ~S is in your PATH.~:@>"
                            command code (car command)))
                    ((:running)
                     (sleep 1/100)
                     (go :again))
                    (t
                     (error "~@<What now? Running shell command ~S failed somehow; status = ~S, status code ~D.~:@>"
                            command status code)))))
             (let ((toks
                    (let ((pp-stream (make-pp-stream (make-string-input-stream line-line))))
                      (loop for tok = (pp-read-token pp-stream) until (eq tok +eof-token+)
                            nconc (unless (white-token-p tok) (list tok))))))
               (unless (and (punct= "#" (elt toks 0))
                            (ident= "line" (elt toks 1))
                            (number-token-p (elt toks 2))
                            (string-token-p (elt toks 3)))
                 ;; (setq /toks toks)
                 (error "~A fooled us, we got~%~%~A~%~%but expected a #line directive."
                        (verbatim (car command))
                        (verbatim line-line)))
               (let ((hfilename
                      (string-token-string (elt toks 3))))
                 (unless (probe-file hfilename)
                   (error "Huh? ~S said <~A> would be at ~S, but this didn't happen here."
                          (car command)
                          (verbatim filename)
                          hfilename))
                 (when (pragma-once/deja-vu-p (truename hfilename))
                   (note "Deja vu: ~S" hfilename))
                 (unless (pragma-once/deja-vu-p (truename hfilename))
                   (push-input-file hfilename)) ))))
      (progn
        (ignore-errors (close temp-stream))
        (ignore-errors (delete-file temp-stream))))))


;;;; Temponary Files

(defun temponary-directory ()
  ;; ### uiop has something here.
  (or
   #+(AND CCL WINDOWS)
   (ccl:native-to-pathname (concatenate 'string (string-right-trim "\\" (ccl:getenv "TEMP")) "\\"))
   #+CCL
   (and (ccl:getenv "TMPDIR")
        (ccl:native-to-pathname (concatenate 'string (string-right-trim "/" (ccl:getenv "TMPDIR")) "/")))
   #p"/tmp/"))

(defun make-temponary-file (&key (type (or #+WINDOWS "$$$" "tmp")))
  (let ((stream (make-temponary-file-stream :type type)))
    (when stream (close stream))
    (pathname stream)))

(defun make-temponary-file-stream (&key (type (or #+WINDOWS "$$$" "tmp")))
  (let (pn)
    (loop
      (setf pn (make-pathname
                :name (with-standard-io-syntax
                        (let ((*print-base* 36))
                          (princ-to-string (random (expt 36 20)))))
                :type type
                :defaults (temponary-directory)))
      (let ((stream (open pn :if-exists nil :if-does-not-exist :create :direction :io)))
        (when stream
          (return stream))))))

#+WINDOWS
;; pipes are unbearable slow with Windows for some reason.
(defun handle-system-include (filename)
  (with-open-stream (fodder (make-temponary-file-stream :type "c"))
    (unwind-protect
         (progn
           (dump-macros fodder :guard-by-ifndef t)
           (when filename
             (format fodder "#include <~A>~%" (verbatim filename)))
           (write-line "#line 1 \"<internal>\"" fodder)
           (close fodder)
           (let ((output (make-temponary-file)))
             (multiple-value-bind (proc)
                 (run-program *cc* (append *cc-args* (list "-dD" "-E" (native-namestring fodder)))
                              :error *error-output*
                              :input nil
                              :wait t
                              :output output
                              :external-format *cpp-default-file-encoding*)
               (declare (ignore proc))
               (push-input-stream (open output :direction :input :external-format *cpp-default-file-encoding*)
                                  (format nil "<~A>" (verbatim filename))))))
      (delete-file fodder))))

