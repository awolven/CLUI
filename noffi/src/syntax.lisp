(in-package :noffi)

(defmacro noffi-syntax (&optional (aggressivep nil))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (when ,aggressivep
       (set-macro-character #\_
                            (lambda (stream char)
                              (read-hash-underscore stream char nil))
                            t))
     (set-dispatch-macro-character #\# #\_ 'read-hash-underscore)
     (set-pprint-dispatch '(satisfies valid-c-identifier-symbol-p)
                          (lambda (stream object)
                            (if *print-escape*
                                (progn
                                  (write-string "#_" stream)
                                  (write-string (symbol-name object) stream))
                                (write-string (string object) stream))
                            object))

     ))

(defun read-hash-underscore (stream subchar arg)
  (declare (ignore subchar))
  (funcall (if (eql 0 arg)
               #'invoke-with-cleared-database
               #'funcall)
           (lambda ()
             (let ((here (ignore-errors (merge-pathnames (make-pathname :name :unspecific :type :unspecific :version :unspecific)
                                                         (truename stream)))))
               (progv (and here '(*default-pathname-defaults*))
                   (and here (list here))
                 (let ((c (peek-char nil stream nil nil)))
                   (cond ((and (characterp c)
                               (or (alphanumericp c) (find c "_$")))
                          (let ((sym
                                 (with-output-to-string (bag)
                                   (loop for c = (peek-char nil stream nil nil)
                                         while (and (characterp c)
                                                    (or (alphanumericp c)
                                                        (find c "_$")))
                                         do
                                         (princ c bag)
                                         (read-char stream)))))
                            (read-hash-underscore-symbol (cintern sym))))
                         ((eql #\( c)
                          (read-char stream)
                          (hash-underscore-lparen stream))
                         ((eql #\< c)
                          (read-char stream)
                          (hash-underscore-lbracket stream))
                         ((eql #\{ c)
                          (read-char stream)
                          (hash-underscore-lbrace stream arg))
                         ((eql #\. c)
                          (read-char stream)
                          (read-hash-underscore-dot stream arg)))))))))

#+(or)
(defun read-hash-underscore-symbol (symbol)
  ;; #_xyz
  '(c-funcaller-form symbol)
  symbol)

(defun hash-underscore-lparen (stream)
  (let (#+CCL
        (pos (ignore-errors (ccl::stream-position stream))))
    ;; (incf pos 10)
    (multiple-value-bind (str map)
        (c-read-delimited #\) stream)
      (let ((*sloc-table* (make-hash-table :test #'eq)))
        ;; (@ ...) fails with sloc
        (let* ((expr (parse-expression (cpp str )))
               (whole (list 'c-form (sublis map expr)))
               (subforms nil))
          ;; I believe somehow we have to mess with subnotes
          #+CCL
          (maphash (lambda (form sloc)
                     (when sloc
                       (push
                        (ccl::record-source-note
                         :form form
                         :stream stream
                         :start-pos (+ pos (line-col-sloc-start-col sloc))
                         :end-pos (+ pos (line-col-sloc-end-col sloc))
                         ;; subforms?
                         )
                        subforms)))
                   *sloc-table*)
          #+CCL
          (and (boundp '*sloc-table*) *sloc-table*
               (let ((tpl-note
                      (let ((sloc (gethash expr *sloc-table*)))
                        (and sloc
                             (ccl::record-source-note
                              :form whole
                              :stream stream
                              :start-pos (+ pos (line-col-sloc-start-col sloc))
                              :end-pos (+ pos (line-col-sloc-end-col sloc))
                              :subform-notes subforms
                              )))))
                 (declare (ignorable tpl-note))
                 ))
          whole)))))

(defun read-hash-underscore-symbol (symbol)
  (progn ;;unless (macro-function symbol)
    (setf (macro-function symbol)
          (lambda (whole env)
            (declare (ignore env))
            (let ((temps (loop for i from 0
                               for nil in (cdr whole)
                               collect (cintern (format nil "__noffi_~D" i)))))
              `(as-lisp ,
              (comp-expr 
               (sublis (mapcar (lambda (temp form)
                                 (cons temp `(lisp ,form)))
                               temps (cdr whole))
                       (parse-expression
                        (cpp
                         (format nil "~A(~{(~A)~^,~})"
                                 (verbatim (symbol-name symbol))
                                 (mapcar #'verbatim temps)))))
               nil)))))
    (eval `(define-symbol-macro ,symbol
               (c-form ,(parse-expression (cpp (symbol-name symbol)))))))
  symbol)

#+(or)
(defun hash-underscore-lparen (stream)
  (let ((pos (ignore-errors (file-position stream))))
    (print `(pos = ,pos))
    (multiple-value-bind (str map)
        (c-read-delimited #\) stream)
      (let ((*sloc-table* (make-hash-table :test #'eq)))
        ;; (@ ...) fails with sloc
        (let ((expr (parse-expression (cpp str))))
          (list 'c-form (sublis map expr)))))))

(defun hash-underscore-lbracket (stream)
  (let ((str (c-read-delimited #\> stream)))
    (parse-type (cpp str))))

(defun hash-underscore-lbrace (stream arg)
  (multiple-value-bind (str map)
      (c-read-delimited #\} stream)
    (if (eql 1 arg)
        (sublis map (parse-statement (cpp (concatenate 'string "{" str "}"))))
        (c-toplevel-process-string str :from-blank (and arg (eql 0 arg)) :map map))))

(defun c-toplevel-process-string (string &key from-blank map)
  (let* ((old de.bauhh.cpp::*defs*)
         (new (if from-blank
                  (de.bauhh.cpp::make-defs-table)
                  ;; Hmm
                  (cons (de.bauhh.cpp::make-defs-table) de.bauhh.cpp::*defs*)))
         (de.bauhh.cpp::*defs* new))
    (let ((*global-env* (if from-blank (make-hash-table :test #'equal) *global-env*)))
      (let ((expr (parse-top-level (cpp string))))
        (cons-progn
         (nconc (let ((r nil))
                  (de.bauhh.cpp::map-seen-header-files
                   new (lambda (h)
                         (push `(c-header-seen ,h) r)))
                  (de.bauhh.cpp::map-macro-definitions
                   (lambda (k v)
                     (when v            ;Huh?
                       (unless (and (not from-blank)
                                    (nth-value 1 (de.bauhh.cpp::c-macro-definition k old)))
                         (unless (eq :lambda (de.bauhh.cpp::macro-definition-kind v))
                           (push `(c-macro ,(de.bauhh.cpp::print-macro-def (de.bauhh.cpp::pp-token-text k) v nil))
                                 r)))))
                   new)
                  (reverse r))
                (list
                 (if map (sublis map expr) expr))))))))

(defun c-toplevel-eval (string &key from-blank map)
  (eval (c-toplevel-process-string string :from-blank from-blank :map map)))

(defmacro c-include (&rest names)
  (c-toplevel-process-string (format nil "~{#include ~A~%~}" (mapcar #'verbatim names))))

(defun cpp (string)
  (let ((de.bauhh.cpp::*use-short-line-directives-p* t)
        (de.bauhh.cpp::*cpp-preserve-white-space-p* t)
        (de.bauhh.cpp::*cpp-keep-comments-p* nil))
    (de.bauhh.cpp::cpp-do string)))

(defun c-read-delimited (end-char stream)
  ;; ###
  ;; ### strings
  (let ((map nil)
        (n 0)
        (pos-map nil))
    (values
     (with-output-to-string (bag)
       (let ((level 0))
         (loop for c = (read-char stream)
               until (and (eql level 0) (eql c end-char))
               do (cond ((eql #\@ c)
                         (let ((lisp-form (read stream t nil t)))
                           (let ((c-form (format nil "___~D___" (1- (incf n)))))
                             (push (cons (cintern c-form) (list 'lisp lisp-form))
                                   map)
                             (princ c-form bag))))
                        (t
                         (princ c bag)
                         (case c
                           ((#\( #\{ #\[) (incf level))
                           ((#\) #\} #\]) (decf level))
                           ))))))
     map
     pos-map)))

(defun c-mangle-atsigns (string)
  (let ((map nil)
        (n 0))
    (values
     (with-output-to-string (bag)
       (with-input-from-string (stream string)
         (loop for c = (read-char stream nil nil)
            while c
            do (cond ((eql #\@ c)
                      (let ((lisp-form
                             (read stream t nil t)))
                        (let ((c-form (format nil "___~D___" (1- (incf n)))))
                          (push (cons (cintern c-form) (list 'lisp lisp-form))
                                map)
                          (princ c-form bag))))
                     (t
                      (princ c bag))))))
     map)))

(defun read-hash-underscore-dot (stream arg)
  (declare (ignore arg))
  (let ((name
         (with-output-to-string (bag)
           (loop for c = (peek-char nil stream nil nil)
                 while (and (characterp c)
                            (or (alphanumericp c)
                                (find c "_$.")))
                 do
                 (write-char c bag)
                 (read-char stream)))))
    ;; `(lambda (it) (c-ref it ,@(mapcar #'cintern (split-by #\. name))))
    (let ((symbol (intern (concatenate 'string "." name) *c-package*))
          (path (mapcar #'cintern (split-by #\. name))))
      (setf (macro-function symbol)
            (lambda (whole env)
              (declare (ignore env))
              (destructuring-bind (place) (cdr whole)
                `(c-ref ,place ,@path))))
      (eval `(define-symbol-macro ,symbol
                 (lambda (it)
                   (c-ref it ,@path))))
      symbol)))

;;;; -- Printer -------------------------------------------------------------------------------

(defmacro with-stream-arg ((stream-var) &body body)
  `(invoke-with-stream-arg (lambda (,stream-var) ,@body) ,stream-var))

(defun invoke-with-stream-arg (cont stream)
  (etypecase stream
    (null (with-output-to-string (stream) (funcall cont stream)))
    ((member t) (funcall cont *standard-output*))
    (stream (funcall cont stream))))

(defun valid-c-identifier-symbol-p (object)
  (and (symbolp object)
       (eq (symbol-package object) (find-package *c-package*))
       (every (lambda (c)
                (or (char<= #\a c #\z)
                    (char<= #\A c #\Z)
                    (char<= #\0 c #\9)
                    (find c "_$:")))    ;### Not quite yet
              (symbol-name object))
       (> (length (symbol-name object)) 0)
       (not (char<= #\0 (char (symbol-name object) 0) #\9))))

(defun render-identifier (identifier &optional env)
  "Returns a string suitable for a C parser to be read as the identifier _identifier_."
  ;; For funny characters or for reserved words we might chose to use \uhhhh and \UHHHHHHHH escapes.
  (if (valid-c-identifier-symbol-p identifier)
      (identifier-name identifier)
      (with-output-to-string (bag)
        (loop for c across (identifier-name identifier)
              for first = t then nil
              do (cond ((or (char<= #\a c #\z)
                            (char<= #\A c #\Z)
                            (char= c #\_)
                            (and (not first) (char<= #\0 c #\9)))
                        (write-char c bag))
                       ((<= (char-code c) #xFFFF)
                        (format bag "\\u~4,'0X" (char-code c)))
                       ((<= (char-code c) #xFFFFFFFF)
                        (format bag "\\U~8,'0X" (char-code c)))
                       (t
                        (error "Impressive!")))))))

;;;; ------------------------------------------------------------------------------------------

(defun syntax-overview ()
  (format t "Expressions:~%")
  (loop for s in
        '("0"
          "1L"
          "2LL"
          "3ULL"
          "0x10"
          "-100"
          "+100"
          "'a'"
          "\"foo\""
          "x + y"
          "+x"
          "x - y"
          "-y"
          "x * y" "x / y" "x % y"
          "x & y" "x | y" "x ^ y" "~x"
          "x == y" "x != y" "x < y" "x <= y" "x > y" "x >= y"
          "x || y" "x && y" "!x"
          "x ? y : z"
          "x = y"
          "x.y"
          "x->y"
          "*x"
          "x[i]"
          "&x"
          "x++" "x--" "++x" "--x"
          "x << y" "x >> y"
          "a, b, c"
          "(int)x"
          "x + y + z"
          "x += y" "x -= y" "x *= y" "x /= y" "x %= y"
          "x &= y" "x |= y" "x ^= y" "x <<= y" "x >>= y"
          "sizeof (x)"
          "sizeof (int)"
          "fun (a, b, c)")
        do
        (format t "    ~A~32T~S~%" (verbatim s) (parse-expression s)))
  '(progn
    (format t "~%Statements:~%")
    (loop for s in
          '("expression;"
            "if (cond) cons;"
            "if (cond) cons; else alt;"
            "while (cond) body;"
            "do{ body; }while(cond);"
            "break;"
            "continue;"
            "goto label;"
            "label: stmt;"
            "switch (expression) body;"
            "case expression: stmt;"
            "default: stmt;"
            "for (init; cond; step) body;"
            "for (int i = 0; i < n; i++) body;"
            "for (int i = 0, z; i < n; i++) body;"
            "return;"
            "return expression;"
            "{ a; b; c; }"
            "{ int x; a; }"
            )
          do
          (format t "    ~A~32T~S~~%" (verbatim s) (parse-statement s))))
  '(progn
    (format t "~%Types:~%")
    (loop for s in
          '("char" "unsigned char" "signed char"
            "short" "unsigned short"
            "int" "unsigned int"
            "long" "unsigned long"
            "long long" "unsigned long long"
            "float" "double" "long double"
            "int *"
            "int []"
            "int [5]"
            "int ()"
            "int (void)"
            "int (int)"
            "int (int x)"
            "int (int x, int y)"
            "int (int x, int y, ...)"
            "void (void)"
            "enum foo"
            ;; "enum foo { }"
            "enum foo { a, b }"
            "enum foo { a = 10, b }"
            "struct foo"
            ;; "struct foo { }" -- not allowd by ISO C99
            "struct foo { int x, y; }"
            "struct foo { int x:1; int :2; int z:3; }"
            "union foo"
            "union foo { int x; }"
            "const int"
            "const int *"
            "int const *"
            "int * const"
            "void"
            )
          do
          (format t "    ~A~32T~S~%" (verbatim s) (parse-type s))))
  '(progn
    (format t "~%Declarations:~%")
    (loop for s in
          '("int x;"
            "int x, y;"
            "int x = 10;"
            "int x[] = { 1, 2, 3};"
            "extern int fun(void);"
            "static int fun(void);"
            "inline int fun(void);"
            "register int x;"
            "auto int x;"
            ;;"typedef int foo;"
            "int bar (int x);"
            "int inc (int x) { return x + 1; }"
            )
          do
          (format t "    ~A~32T~S~%" (verbatim s) (parse-top-level s))))
  '(progn
    (format t "~%Array unsanity:~%")
    (loop for s in
          '("int x [const 10];"
            "int x [n];"
            "int x [n += 3];"
            "int x [static k];"
            )
          do
          (format t "    ~A~32T~S~%" (verbatim s) (cadr (parse-top-level s)))))
  )


;;;; -- CVAL printer --------------------------------------------------------------------------

(defvar *print-dispatch-prefix* "#_")

(defmethod print-object ((object cval) stream)
  (print-cval object stream :dispatch-prefix *print-dispatch-prefix*))

(defun print-cval (object stream &key (dispatch-prefix *print-dispatch-prefix*))
  (let ((*print-dispatch-prefix* nil))
    (let ((type (cval-type object)))
      (labels ((invoke-with-dispatch-prefix-if-needed (cont)
                 (cond (dispatch-prefix
                        (write-string dispatch-prefix stream)
                        (pprint-logical-block (stream nil :prefix "(" :suffix ")")
                          (funcall cont stream)))
                       (t
                        (funcall cont stream)))))
        (cond ((arithmetic-type-p type)
               (invoke-with-dispatch-prefix-if-needed
                (lambda (stream)
                  (pprint-logical-block (stream nil :prefix "(" :suffix ")")
                    (print-type type stream))
                  ;; ### float format
                  (prin1 (cval-value object) stream))))
              ((pointer-type-p type)
               (invoke-with-dispatch-prefix-if-needed
                (lambda (stream)
                  (pprint-logical-block (stream nil :prefix "(" :suffix ")")
                    (print-type type stream))
                  (cond
                    #+CCL
                    ((ccl::external-entry-point-p (cval-value object))
                     (let ((eep (cval-value object)))
                       (let ((addr (ccl::eep.address eep)))
                         (write-string (ccl::eep.name eep) stream)
                         (when addr (format stream " /* 0x~X */" addr)))))
                    ((eql ':char (bare-expanded-type (pointer-type-base type)))
                     (print-putative-c-string object stream))
                    (t
                     (format stream "0x~X" (ptr-int object)))))))
              ((record-type-p type)
               (invoke-with-dispatch-prefix-if-needed
                (lambda (stream) (print-record object stream))))
              (t
               (print-unreadable-object (object stream :type t :identity nil)
                 (format stream "~S ~S" type (cval-value object)))))))))

;; This is a heuristic for printing strings. This works on Lisps that
;; have proper support for catching segmentation faults. An alternative
;; would be to attempt to write(2) and then read(2) from a pipe and
;; looking for EFAULT.

(defun maybe-peek-u8 (ptr &optional (offset 0))
  "Try to peek an unsigned 8-bit integer from the given pointer and
  returns it. and returns NIL. If that doesn't happen for us because
  it is outside the address space.

  If it is not possible to catch invalid addresses always return NIL."
  (or
   #+CCL
   (handler-case
       (peek-u8 ptr offset)
     (ccl::invalid-memory-access ()
       nil))
   #+SBCL
   (handler-case
       (peek-u8 ptr offset)
     (sb-sys:memory-fault-error ()
       nil))))

(defun print-putative-c-string (ptr stream &aux (max 80))
  "Tries to print the pointer /ptr/ as a C string. If that fails, we
print a hexadecimal for the address in C syntax instead."
  (let ((bag (make-array 80 :element-type 'character :fill-pointer 0 :adjustable t)))
    (vector-push-extend #\" bag)
    (do* ((i 0 (+ i 1))
          (c (maybe-peek-u8 ptr i) (maybe-peek-u8 ptr i)))
        (nil)
      (cond ((null c)
             ;; no luck
             (format stream "0x~X" (ptr-int ptr))
             (return))
            ((zerop c)
             ;; we won
             (vector-push-extend #\" bag)
             (write-string bag stream)
             (return))
            ((> i max)
             (dotimes (k 3)
               (vector-push-extend #\\ bag)
               (vector-push-extend #\. bag))
             (vector-push-extend #\" bag)
             (write-string bag stream)
             (return))
            ((<= 32 c 126)
             (when (or (eql c #.(char-code #\")) (eql c #.(char-code #\\)))
               (vector-push-extend #\\ bag))
             (vector-push-extend (code-char c) bag))))))

(defun print-record (cval stream)
  (pprint-logical-block (stream nil :prefix "(" :suffix ")")
    (print-type (cval-type cval) stream))
  (let ((members (record-type-members (cval-type cval))))
    (pprint-logical-block (stream members :prefix "{ " :suffix " }")
      (when members
        (loop
          (let ((member (pprint-pop)))
            (format stream ".~A = " (verbatim (record-member-name member)))
            (handler-case
                (let ((val (ignore-errors (c-> cval (record-member-name member)))))
                  (if val
                      (prin1 (c-> cval (record-member-name member)) stream)
                      (write-string "0" stream)))
              (#+CCL CCL::INVALID-MEMORY-ACCESS
                #+SBCL SB-SYS:MEMORY-FAULT-ERROR
                #-(OR CCL SBCL)
                error ()
                (write-string "???" stream)))
            (pprint-exit-if-list-exhausted)
            (format stream ", ")
            (pprint-newline :linear stream)))))))


;;;; -- Type Printer --------------------------------------------------------------------------

(defun print-type (type &optional (stream *standard-output*) (inner nil))
  (let ((*print-circle* nil))
    (with-stream-arg (stream)
      (let ((qualifiers (type-qualifiers type)))
        (print-type-1 type stream
                      (if (and (null qualifiers) (null inner))
                          nil
                          (lambda (stream)
                            (do ((q qualifiers (cdr qualifiers)))
                                ((endp q))
                              (let ((it (cadr (assoc (car q) *type-qualifier-lexemes*))))
                                (format stream "~A" (verbatim (or it q))))
                              (when (or (cdr q) inner)
                                (princ " " stream)))
                            (etypecase inner
                              (null)
                              (function (funcall inner stream))
                              (string   (write-string inner stream))
                              (symbol   (princ inner stream))))))))))

(defun print-type-1 (type stream inner)
  (labels ((frob (name)
             (princ name stream)
             (when inner
               (princ " " stream)
               (funcall inner stream))))
    (cond ((named-type-p type)
           (frob (identifier-name (named-type-name type))))
          ((void-type-p type)
           (frob "void"))
          ((enum-type-p type)
           (write-string "enum" stream)
           (let ((name (enum-type-name type)))
             (when name
               (format stream " ~A" (verbatim (identifier-name name)))))
           (multiple-value-bind (members complete-p)
               (enum-type-members type nil :errorp nil)
             (when complete-p
               (princ " {" stream)
               (terpri stream)
               (pprint-logical-block (stream members :per-line-prefix "  ")
                 (let ((first t))
                 (loop
                   (pprint-exit-if-list-exhausted)
                   (unless (shiftf first nil)
                     (write-string "," stream)
                     (pprint-newline :mandatory stream))
                   (multiple-value-bind (name init)
                       (let ((member (pprint-pop)))
                         (values (enum-member-name member)
                                 (enum-member-value-form member)))
                     (print-expression (if init `(setf ,name ,init) name) stream)))))
               (terpri stream)
               (princ "}" stream)))
           (when inner
             (princ " " stream)
             (funcall inner stream)))
          ((integer-type-p type)
           (frob (integer-type-spelling type)))
          ((float-type-p type)
           (frob (float-type-spelling type)))
          ((pointer-type-p type)
           (let ((base (pointer-type-base type)))
             (print-type base stream
                         (lambda (stream)
                           (when (> (type-precedence base) (type-precedence type))
                             (princ "(" stream))
                           (princ "*" stream)
                           (and inner (funcall inner stream))
                           (when (> (type-precedence base) (type-precedence type))
                             (princ ")" stream))))))
          ((amp-pointer-type-p type)
           (let ((base (amp-pointer-type-base type)))
             (print-type base stream
                         (lambda (stream)
                           (when (> (type-precedence base) (type-precedence type))
                             (princ "(" stream))
                           (princ "&" stream)
                           (and inner (funcall inner stream))
                           (when (> (type-precedence base) (type-precedence type))
                             (princ ")" stream))))))
          ((array-type-p type)
           (let ((base (array-type-base type)))
             (print-type base stream
                         (lambda (stream)
                           (when (> (type-precedence base) (type-precedence type))
                             (princ "(" stream))
                           (and inner (funcall inner stream))
                           (princ "[" stream)
                           (let ((count (array-type-count type)))
                             (unless (eq count ':UNSPECIFIED)
                               (print-expression count stream)))
                           (princ "]" stream)
                           (when (> (type-precedence base) (type-precedence type))
                             (princ ")" stream))))))
          ((function-type-p type)
           (let ((res-type (function-type-result-type type)))
             (print-type res-type
                         stream
                         (lambda (stream)
                           (let ((parenp (> (type-precedence res-type) (type-precedence type))))
                             (when parenp (princ "(" stream))
                             (when inner
                               (funcall inner stream)
                               (princ " " stream))
                             (pprint-logical-block (stream (caddr type) :prefix "(" :suffix ")")
                               (loop for arg in (function-type-parameters type)
                                     for i from 0
                                     do
                                     (unless (zerop i)
                                       (princ ", " stream)
                                       (pprint-newline :linear stream))
                                     (cond ((eql arg '&rest)
                                            (princ "..." stream))
                                           (t
                                            (print-type (declaration-type arg) stream
                                                        (and (declaration-name arg)
                                                        (lambda (stream)
                                                          (when (declaration-name arg)
                                                            (princ (identifier-name (declaration-name arg))
                                                                   stream)))))))))
                             (when parenp (princ ")" stream)))))))
          ((record-type-p type)
           (let* ((name (record-type-name type))
                  (completep (record-type-complete-p type))
                  (members (and completep (record-type-members type))))
             (pprint-logical-block (stream members)
               (write-string (cond ((struct-type-p type) "struct")
                                   ((union-type-p type) "union")
                                   (t (assert nil)))
                             stream)
               (when name
                 (format stream " ~A" (verbatim (identifier-name name))))
               (when completep
                 (pprint-indent :block 4 stream)
                 (write-string " {" stream)
                 (unwind-protect
                      (loop
                        (pprint-exit-if-list-exhausted)
                        (pprint-newline :mandatory stream)
                        (print-declaration (pprint-pop) stream))
                   (pprint-indent :block 0 stream)
                   (pprint-newline :mandatory stream)
                   (princ "}" stream)))))
           (when inner
             (princ " " stream)
             (funcall inner stream)) )
          ((keywordp type)              ;###
           (frob (concatenate 'string "__noffi_unknown " (string-downcase type))))
          ((bit-field-type-p type)
           (let ((base (bit-field-type-base-type type)))
             (print-type base stream
                         (lambda (stream)
                           (and inner (funcall inner stream))
                           (princ " :" stream)
                           (print-expression (bit-field-type-width type) stream)))))
          (t
           (error "Huh? What kind of type is ~S" type)))))

(defun type-precedence (type)
  (cond ((named-type-p type)    100)
        ((arithmetic-type-p type) 100)
        ((void-type-p type)     100)
        ((record-type-p type)   100)
        ((enum-type-p type)     100)
        ((function-type-p type) 400)
        ((array-type-p type)    400)
        ((pointer-type-p type)  200)
        ((amp-pointer-type-p type)  200)
        (t 100)
        #+(or)
        (t (error "Huh? What kind of type is ~S" type))))


;;;; -- Expression Printing -------------------------------------------------------------------

(defvar print-expression-*last-token*)

(defun print-expression (expr &optional (stream *standard-output*) &key (precedence 2000))
  (let ((print-expression-*last-token* t))
    (if (eq 'nil stream)
        (with-output-to-string (bag) (print-expression-1 expr bag precedence))
        (print-expression-1 expr stream precedence))) )

;;; A precedence table just like with Prolog

;; "x" = lower precedence
;; "y" = no higher precedence
;; "z" = expression
;; "c" = expression of cast precedence
;; "t" = a type
;; "i" = a bare identifier

(defparameter +postfix-expression-precedence+ 100)
(defparameter +cast-expression-precedence+ 300)
(defparameter +assignment-expression-precedence+ 1500)
(defparameter +conditional-expression-precedence+ 1400)
(defparameter +postfix-expression-precedence+ 100)

(defparameter +expression-precedence-table+
  '( ;; expression
    (1600 "yfx" (progn ", "))

    ;; assignment-expression
    (1500 "xfy" (setf " = "))
    (1500 "xfy"
     (mulf " *= ")
     (divf " /= ")
     (modf " %= ")
     (incf " += ")
     (decf " -= ")
     (shlf " <<= ")
     (shrf " >>= ")
     (logandf " &= ")
     (logxorf " ^= ")
     (logiorf " |= "))

    ;; conditional expression
    (1400 "xfzfy" (if " ? " " : "))

    (1300 "yfx" (or " || "))
    (1200 "yfx" (and " && "))
    (1100 "yfx" (logior " | "))
    (1000 "yfx" (logxor " ^ "))
    (900 "yfx" (logand " & "))
    (800 "yfx" (= " == ") (/= " != "))
    (700 "yfx" (< " < ") (> " > ") (<= " <= ") (>= " >= "))
    (600 "yfx" (<< " << ") (>> " >> "))
    (500 "yfx" (+ " + ") (- " - "))
    (400 "yfx" (* " * ") (/ " / ") (% " % "))
    ;; cast expression
    (300 "ftfy" (the "(" ")"))

    ;; unary expression
    (200 "fy" (pre-inc "++") (pre-dec "--"))
    (200 "fc" (& "&") (aref "*") (+ "+") (- "-") (lognot "~") (not "!"))
    (200 "ftf" (alignof-type "__alignof(" ")"))
    (200 "fzf" (alignof-expr "__alignof(" ")")) ;### really __alignof <unary-expr>
    (200 "ftf" (sizeof-type "sizeof(" ")"))
    (200 "fzf" (sizeof-expr "sizeof(" ")")) ;### really __sizeof <unary-expr>
    (200 "ftf" (noffi-type "__noffi_type(" ")"))

    ;; postfix expression
    (100 "yfzf" (aref "[" "]"))
    (100 "ftfzf" (offsetof "__offsetof__(" ", " ")"))
    (100 "yfi" (\. "."))
    (100 "yfi" (-> "->"))
    (100 "yf" (post-inc "++"))
    (100 "yf" (post-dec "--"))
    ;; primary expression at 0
    ))

(defun expr-precedence-description (expr &aux it)
  (loop for (prec def . ops) in +expression-precedence-table+ 
        do (when (and (= (count-if (lambda (c) (find c "xyzitc"))  def) (length (cdr expr)))
                      (setq it (assoc (car expr) ops :test #'equal)))
             (return (values prec def (cdr it))))
        finally (error "Oops, we don't know the precedence of the C expression ~S" expr)))

;;; Conflicting Tokens

;; Trouble: (++(+x)) and (+(++x))

;; There is more, here are the pairs, provided we keep the space
;; characters around binary operators.

;; ("&" "&") 
;; ("+" "+") 
;; ("-" "-")    e.g. (- (- x))
;; ("-" "->")   
;; ("+" "++")   e.g. (+ (++ x))
;; ("-" "--")   e.g. (- (-- x))

;; There are other problematic cases like "100 .e2" which parses as
;; (\. 100 _e2) and must not be printed as "100.e2".

;; One idea is to demand a blank, or some other save chare before an
;; unary prefix or postfix operator.

(defparameter +print-expression-conflict-token-pairs+
  '((:- :--) (:+ :++) (:- :->) (:CONSTANT :|.|) (:- :-) (:+ :+) (:& :&)
    (:|.| :CONSTANT) (:CONSTANT :CONSTANT)
    (:IDENTIFIER :CONSTANT)
    (:CONSTANT :IDENTIFIER)
    (:IDENTIFIER :IDENTIFIER)
    (:IDENTIFIER :CONSTANT)
    (:CONSTANT :\.)))

(defun conflicting-token-pair-p (t1 t2)
  (find-if (lambda (x)
             (and (string= (car x) t1)
                  (string= (cadr x) t2)))
           +print-expression-conflict-token-pairs+))

(defun flatten-left-assoc (term)
  "Given a term made from binary expressions, turn it into a variadic term. Like (+ (+ 1 2) 3) => (+ 1 2 3)."
  (labels ((aux (q)
             (cond ((and (consp q)
                         (= 3 (length q))
                         (eq (car q) (car term)))
                    (nconc (aux (cadr q))
                           (aux (caddr q))))
                   (t
                    (list q)))))
    (cons (car term) (mapcan #'aux (cdr term)))))

(defun print-expression-1 (expr stream prec)
  (labels ((emit-token (token stream)
             (note-token token stream)
             (write-string token stream))
           (note-token (token stream)
             (when (conflicting-token-pair-p print-expression-*last-token* token)
               (write-string " " stream))
             (setq print-expression-*last-token* token)))
    (macrolet ((with-block ((list inner-prec) &body body
                            &aux (g (gensym)))
                 `(let ((,g (> ,inner-prec prec)))
                    (pprint-logical-block (stream ,list
                                                  :prefix (if ,g "(" "")
                                                  :suffix (if ,g ")" ""))
                      ,@body))))
      ;;
      (cond ((identifierp expr)
             (note-token :identifier stream)
             (with-block (nil 0)
               (format stream "~A" (verbatim (identifier-name expr)))))
            (t
             (case (car expr)
               (:integer-constant
                (note-token :constant stream)
                (with-block (nil 0)
                  (destructuring-bind (base value suffix) (cdr expr)
                    (ecase base
                      (:decimal (format stream "~D" value))
                      (:octal   (if (eql 0 value) (format stream "0") (format stream "0~O" value)))
                      (:hex     (format stream "0x~X" value)))
                    (when suffix
                      (format stream "~A" (verbatim suffix))))))
               (:floating-constant
                (note-token :constant stream)
                (with-block (nil 0)
                  (destructuring-bind (value type) (cdr expr)
                    (let ((*read-default-float-format* (type-of value)))
                      (prin1 value stream))
                    (ecase type
                      (:float (write-char #\F stream))
                      (:double)
                      (:long-double (write-char #\L stream))))))
               (:character-constant
                (note-token :constant stream)
                (with-block (nil 0)
                  (destructuring-bind (literal prefix) (cdr expr)
                    (when prefix (write-string prefix stream))
                    (write-char #\' stream)
                    (write-string literal stream)
                    (write-char #\' stream))))
               (:string-literal
                (note-token :constant stream)
                (with-block (nil 0)
                  (destructuring-bind (literal prefix) (cdr expr)
                    (when prefix (write-string prefix stream))
                    (write-char #\" stream)
                    ;; Note that the literal is already escaped.
                    (write-string literal stream)
                    (write-char #\" stream))))
               (:string-literal-list
                (note-token :constant stream)
                (with-block (nil 0)
                  (destructuring-bind (&rest literals) (cdr expr)
                    (pprint-logical-block (stream literals)
                      (loop
                        (print-expression-1 (pprint-pop) stream 0)
                        (pprint-exit-if-list-exhausted)
                        (write-string " " stream)
                        (pprint-newline :fill stream))))))
               (:pp-number
                (note-token :constant stream)
                (with-block (nil 0)
                  (destructuring-bind (text) (cdr expr)
                    (write-string text stream))))
               ((LISP)
                (note-token :lisp stream)
                (with-block (nil 0)
                  (destructuring-bind (form) (cdr expr)
                    (cond ((and (consp form) (eq (car form) 'c-form))
                           (destructuring-bind (expr) (cdr form)
                             (print-expression-1 expr stream prec)))
                          (t
                           (write-string "(@" stream)
                           (prin1 form stream)
                           (write-string ")" stream))))))
               ((FUNCALL)
                (with-block (nil +postfix-expression-precedence+)
                  (destructuring-bind (fun &rest args) (cdr expr)
                    (print-expression-1 fun stream +postfix-expression-precedence+)
                    (note-token :lparen stream)
                    (pprint-logical-block (stream args :prefix "(" :suffix ")")
                      (pprint-exit-if-list-exhausted)
                      (loop
                        (print-expression-1 (pprint-pop) stream +assignment-expression-precedence+)
                        (pprint-exit-if-list-exhausted)
                        (emit-token ", " stream)
                        (pprint-newline :linear stream))))))
               ((SETOPF)
                (destructuring-bind (operator place value) (cdr expr)
                  ;; A hack to make this go through our precedence table as well
                  (print-expression-1 `((SETOPF ,operator) ,place ,value) stream prec)))
               ((:LIST)
                (with-block (nil prec)
                  (pprint-logical-block (stream (cdr expr) :prefix "{ " :suffix " }")
                    (pprint-exit-if-list-exhausted)
                    (loop
                      (setq print-expression-*last-token* nil)
                      (let ((item (pprint-pop)))
                        (cond ((and (consp item) (eq :at (car item)))
                               (pprint-logical-block (stream nil)
                                 (destructuring-bind (path value) (cdr item)
                                   (dolist (k path)
                                     (ecase (car k)
                                       ((\.) (destructuring-bind (member) (cdr k)
                                               (format stream ".~A" (verbatim (symbol-name member)))))
                                       ((AREF) (destructuring-bind (index) (cdr k)
                                                 (pprint-logical-block (stream nil :prefix "[" :suffix "]")
                                                   (setq print-expression-*last-token* nil)
                                                   (print-expression-1 index stream +conditional-expression-precedence+))))))
                                   (write-string " = " stream)
                                   (pprint-indent :current 0 stream)
                                   '(pprint-newline :linear stream)           
                                   (setq print-expression-*last-token* nil)
                                   (print-expression-1 value stream +assignment-expression-precedence+) )))
                              (t
                               (print-expression-1 item stream +assignment-expression-precedence+))))
                      (pprint-exit-if-list-exhausted)
                      (write-string ", " stream)
                      (pprint-newline :linear stream)))
                  (setq print-expression-*last-token* nil)))
               ((:OBJECT-LITERAL)
                (with-block (nil prec)
                  (destructuring-bind (type init) (cdr expr)
                    (pprint-logical-block (stream nil :prefix "(" :suffix ")")
                      (print-type type stream))
                    (setq print-expression-*last-token* nil)
                    (print-expression-1 init stream +assignment-expression-precedence+)
                    (setq print-expression-*last-token* nil))))
               (otherwise
                (multiple-value-bind (inner-prec template ops) (expr-precedence-description expr)
                  (let ((need-parens-p (> inner-prec prec)))
                    (cond ((equal "yfx" template)
                           ;; special for left-assoc. We flatten it out first.
                           (let ((args (cdr (flatten-left-assoc expr))))
                             (pprint-logical-block (stream args
                                                           :prefix (if need-parens-p "(" "")
                                                           :suffix (if need-parens-p ")" ""))
                               ;; (unless need-parens-p (pprint-indent :current 2 stream))
                               (loop
                                 (print-expression-1 (pprint-pop) stream (1- inner-prec))
                                 (pprint-exit-if-list-exhausted)
                                 (emit-token (car ops) stream)
                                 (pprint-newline :linear stream)))))
                          (t
                           (let ((args (cdr expr)))
                             (pprint-logical-block (stream args
                                                           :prefix (if need-parens-p "(" "")
                                                           :suffix (if need-parens-p ")" ""))
                               ;; (unless need-parens-p (pprint-indent :current 2 stream))
                               (loop for c across template do
                                     (ecase c
                                       (#\x (print-expression-1 (pprint-pop) stream (1- inner-prec)))
                                       (#\y (print-expression-1 (pprint-pop) stream inner-prec))
                                       (#\z (print-expression-1 (pprint-pop) stream 2000))
                                       (#\c (print-expression-1 (pprint-pop) stream +cast-expression-precedence+))
                                       (#\t (print-type (pprint-pop) stream))
                                       (#\i (note-token :identifier stream)
                                            (format stream "~A" (verbatim (pprint-pop))))
                                       (#\f
                                        (let ((op (pop ops)))
                                          (emit-token op stream)
                                          ;; Hmm
                                          (when (char= #\space (char op (1- (length op))))
                                            (pprint-newline :linear stream)) )))))))))))))))))


;;;; -- Statement Printer ---------------------------------------------------------------------

(defun print-statement (stmt &optional (stream *standard-output*))
  (if (eq 'nil stream)
      (with-output-to-string (bag) (print-statement-1 stmt bag))
      (print-statement-1 stmt stream)))

(defun print-statement-1 (stmt stream)
  (select stmt
    (LABEL (tag body)
      (write-string (string tag) stream)
      (write-string ":" stream)
      (pprint-newline :mandatory stream)
      (print-statement-1 body stream))

    (CASE (key body)
      (pprint-logical-block (stream nil)
        (pprint-indent :block 2 stream)
        (write-string "case " stream)
        (print-expression key stream :precedence -1)
        (write-string ":" stream)
        (pprint-newline :mandatory stream)
        (print-statement-1 body stream)))

    (DEFAULT (body)
      (write-string "default:" stream)
      (pprint-newline :mandatory stream)
      (print-statement-1 body stream))

    (NOP ()
      (write-string ";" stream))

    (EXPR (expr)
      (print-expression expr stream)
      (write-string ";" stream))

    (IF (test cons &optional alt)
        (let ((newline-kind :mandatory))
          (pprint-logical-block (stream nil)
            (pprint-indent :block 2 stream)
            (write-string "if " stream)
            (print-expression test stream :precedence -1)
            (write-string " " stream)
            (pprint-newline newline-kind stream)
            (print-statement-1 cons stream)
            (when alt
              (pprint-indent :block 0 stream)
              (write-string " " stream)
              (pprint-newline newline-kind stream)
              (write-string "else " stream)
              (pprint-indent :block 2 stream)
              (pprint-newline newline-kind stream)
              (print-statement-1 alt stream)))))

    (SWITCH (key body)
      (pprint-logical-block (stream nil)
        (pprint-indent :block 2 stream)
        (write-string "switch " stream)
        (print-expression key stream :precedence -1)
        (write-string " " stream)
        (pprint-newline :mandatory stream)
        (print-statement-1 body stream)))
    
    (WHILE (test body)
      (pprint-logical-block (stream nil)
        (pprint-indent :block 2 stream)
        (write-string "while " stream)
        (print-expression test stream :precedence -1)
        (write-string " " stream)
        (pprint-newline :mandatory stream)
        (print-statement-1 body stream)))

    (DO-WHILE (body test)
      ;; ###
      (pprint-logical-block (stream nil)
        (pprint-indent :block 2 stream)
        (write-string "do " stream)
        (print-statement-1 body stream)
        (write-string " " stream)
        (pprint-newline :linear stream)
        (write-string "while " stream)
        (print-expression test stream :precedence -1)
        (write-string ";" stream)))

    (FOR ((init test step) body)
      (pprint-logical-block (stream nil)
        (pprint-indent :block 2 stream)
        (pprint-logical-block (stream (list init test step) :prefix "for (" :suffix ")")
          (loop
            (let ((x (pprint-pop))) (and x (print-expression x stream)))
            (pprint-exit-if-list-exhausted)
            (write-string "; " stream)
            (pprint-newline :linear stream)))
        (write-string " " stream)
        (pprint-newline :mandatory stream)
        (print-statement-1 body stream)))

    (FOR-DECL ((decl test step) body)
      )

    (GOTO (tag)
      (pprint-logical-block (stream nil)
        (write-string "goto " stream)
        (write-string (string tag) stream)
        (write-string ";" stream)))

    (CONTINUE ()
      (write-string "continue;" stream))

    (BREAK ()
      (write-string "break;" stream))

    (RETURN (&optional result)
            (pprint-logical-block (stream nil)
              (write-string "return" stream)
              (when result
                (write-string " " stream)
                (print-expression result stream))
              (write-string ";" stream)))

    (BEGIN (&rest body)
      (pprint-logical-block (stream body)
        (unwind-protect
             (progn
               (pprint-indent :block 2 stream)
               (write-string "{ " stream)
               (loop
                 (pprint-exit-if-list-exhausted)
                 (pprint-newline :mandatory stream)
                 (print-statement (pprint-pop) stream)))
          (pprint-indent :block 0 stream)
          (pprint-newline :mandatory stream)
          (write-string "}" stream))) )

    (DECL (&rest ignore)
      (declare (ignore ignore))
      (print-declaration stmt stream)) ))


;;;; -- Printing Declarations -----------------------------------------------------------------

(defun print-declaration (decl &optional stream)
  (if (eq 'nil stream)
      (with-output-to-string (bag) (print-declaration-1 decl bag))
      (print-declaration-1 decl stream)))

(defun print-declaration-1 (decl stream)
  (destructuring-bind (specifiers &rest things) (cdr decl)
    (let ((source-location (assoc :source-location specifiers)))
      (when source-location
        (format stream "~&/* ~A:~D: */~%"
                (princ-to-string (second source-location))
                (third source-location))))
    (pprint-logical-block (stream nil)
      (loop for thing in things
            for nil = nil then (pprint-newline :mandatory stream)
            do (destructuring-bind (name type &optional init) thing
                 (print-declaration-2 specifiers name type init stream))))))

(defun print-declaration-2 (specifiers name type init stream)
  (pprint-logical-block (stream nil)
    (dolist (k specifiers)
      (print-specifier k stream)
      (write-string " " stream))
    (print-type type stream name)
    (typecase init
      (null
       (write-string ";" stream))
      ((cons (member BEGIN))
       (pprint-newline :mandatory stream)
       (print-statement init stream))
      (T
       (write-string " = " stream)
       (print-expression init stream)
       (write-string ";" stream)))))

(defun print-specifier (specifier stream)
  (with-stream-arg (stream)
    (select specifier
      ((:storage-class) (storage-class)
       (write-string (storage-class-spelling storage-class) stream))
      ((:declaration-specifier) (spec)
       (print-declaration-specifier spec stream))
      (otherwise
       (format stream "/* ~S */" specifier)))))

(defun print-declaration-specifier (specifier stream)
  (with-stream-arg (stream)
    (select specifier
      ((:align) (n)
       (pprint-logical-block (stream nil :prefix "__noffi_align(" :suffix ")")
         (print-expression n stream)))
      ((:pack) (n)
       (format stream "__noffi_pack(~D)" n))
      (otherwise
       (format stream "/* ~S */" specifier)))))


;;;;

(defun ~decl (stream declaration colon at)
  (declare (ignore colon at))
  (print-declaration declaration stream))

(defun ~type (stream declaration colon at)
  (declare (ignore colon at))
  (print-type declaration stream))

(defun ~green (stream arg colon at)
  (declare (ignore colon at arg))
  (write-string #.(format nil "~A[32m" (code-char 27)) stream))

(defun ~red (stream arg colon at)
  (declare (ignore colon at arg))
  (write-string #.(format nil "~A[31m" (code-char 27)) stream))

(defun ~plain (stream arg colon at)
  (declare (ignore colon at arg))
  (write-string #.(format nil "~A[0m" (code-char 27)) stream))

