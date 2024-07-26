(in-package :noffi)

;; Random collection of some utils

(defun hash-table-alist (ht)
  (loop for k being the hash-keys of ht
        for v being the hash-values of ht
        collect (cons k v)))

;; *** meta-fun.lisp

(defun curry (fun &rest args)
  #'(lambda (&rest more)
      (apply fun (append args more))))

(defun rcurry (fun &rest args)
  #'(lambda (&rest more)
      (apply fun (append more args))))

;; *** split-by.lisp

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

(defun split-by (item &rest args)
  (apply #'split-by-if (curry #'eql item) args))

(defun split-by-member (items &rest args)
  (apply #'split-by-if (rcurry #'member items) args))

;; *** sldb.lisp

(declaim (inline sldb))

(defun sldb (byte-spec value)
  (- (logand
      (1- (ash 1 (byte-size byte-spec)))
      (+ (ldb byte-spec value)
         (ash 1 (1- (byte-size byte-spec)))))
     (ash 1 (1- (byte-size byte-spec)))))
(define-compiler-macro sldb (&whole whole byte-spec value &environment env)
  (cond ((or (constantp byte-spec env)
             (and (typep byte-spec '(cons (member byte) (cons t (cons t null))))
                  (constantp (cadr byte-spec) env)
                  (constantp (caddr byte-spec) env)))
         ;; Sigh, we need a proper compiler eventually, ours are not smart enough.
         (let* ((evaled-byte-spec (eval byte-spec #|env|#))
                (n (byte-size evaled-byte-spec))
                (p (byte-position evaled-byte-spec)))
           `(the 
             (signed-byte ,n)
             (- (the (unsigned-byte ,n)
                     (logand (the (unsigned-byte ,(1+ n))
                                  (+ ,(if (= 0 p)
                                          `(logand ,value ,(1- (ash 1 n)))
                                          `(ldb ,byte-spec ,value))
                                     ,(ash 1 (1- n))))
                             ,(1- (ash 1 n))))
                ,(ash 1 (1- n))))))
        (t
         whole)))

;; *** parse-macro.lisp

#+CCL
(defun parse-macro (name lambda-list body)
  (ccl:parse-macro name lambda-list body))

#-CCL
(defun parse-macro (name lambda-list body)
  (let ((whole-gensym (gensym "WHOLE."))
        (env-gensym (gensym "ENV."))
        (dummy-1 (gensym "DUMMY."))
        (dummy-2 (gensym "DUMMY.")))
    (multiple-value-bind (doc-string body)
        (let ((q (member-if #'stringp body)))
          (if (and q (cdr q) (every (lambda (x) (and (consp x) (eq (car x) 'declare))) (ldiff body q)))
              (values (car q) (append (ldiff body q) (cdr q)))
              (values nil body)))
      (multiple-value-bind (whole-var lambda-list)
          (if (eq '&whole (car lambda-list))
              (values (cadr lambda-list) (cddr lambda-list))
              (values nil lambda-list))
        (multiple-value-bind (env-var lambda-list)
            (let ((q (member '&environment lambda-list)))
              (if q
                  (values (cadr q) (append (ldiff lambda-list q) (cddr q)))
                  (values nil lambda-list)))
          `(lambda (,whole-gensym ,env-gensym)
             ,@(and doc-string (list doc-string))
             ,@(if (not env-var) (list `(declare (ignore ,env-gensym))))
             (block ,name
               (destructuring-bind (&whole
                                    (,@(if whole-var `(&whole ,whole-var))
                                       &rest ,dummy-1
                                       ,@(if env-var `(&aux (,env-var ,env-gensym))))
                                    ,dummy-2 
                                    ,@lambda-list)
                   ,whole-gensym
                 (declare (ignore ,dummy-1 ,dummy-2))
                 ,@body))))))))

;;;;

;; (put 'select 'common-lisp-indent-function '(4 &rest (&whole 2 4 &rest 2)))

#+CCL
(defun borrow-source-note (from to)
  (let ((old-note (ccl::nx-source-note from))
        (new-note (ccl::nx-source-note to)))
    (when (and ccl::*nx-source-note-map* old-note (not new-note))
      (setf (gethash to ccl::*nx-source-note-map*) old-note)))
  to)

#-CCL
(defun borrow-source-note (from to)
  (declare (ignore from))
  to)

(defmacro select (expr &rest clauses)
  (let ((g (gensym)))
    `(let ((,g ,expr))
       (,(if (member-if (lambda (x) (member (car x) '(t otherwise))) clauses) 'case 'ecase) (car ,g)
         ,@(mapcar (lambda (clause)
                     (cond ((member (car clause) '(t otherwise))
                            clause)
                           (t
                            (destructuring-bind (key lambda-list &body body) clause
                              `(,key
                                ,(borrow-source-note
                                  clause
                                  (list* 'destructuring-bind lambda-list `(cdr ,g) body)))))))
                   clauses)))))

(defun verbatim (x)
  "A fix for the alternate interpretation of ~A with FORMAT which
would invoke *PRINT-CIRCLE* processing even for strings. When given a
string argument, we copy that string to defeat that."
  (if (stringp x) (copy-seq x) x))


;;;;

(defun c-inspect (what &aux it)
  (fresh-line)
  (when (stringp what)
    (setq what (cintern what)))
  (when (typep what '(cons (member :enum :struct :union)))
    (setq what (cadr what)))
  (let ((decl (find-identifier-declaration what nil :errorp nil)))
    (when (declaration-p decl)
      (when (setq it (assoc :sloc (declaration-specifiers decl)))
        (prin1 it)
        (set'/sloc (cadr it))
        (terpri))
      (print-declaration decl *standard-output*)
      (return-from c-inspect nil)))
  (let ((decl (or (find-identifier-declaration `(:union ,what) nil :errorp nil)
                  (find-identifier-declaration `(:struct ,what) nil :errorp nil)
                  (find-identifier-declaration `(:enum ,what) nil :errorp nil))))
    (when decl
      (print-type decl *standard-output*)
      (princ ";")))
  (values))

(defun boolean= (a b)
  (if a b (not b)))

(defmacro ensure-type (value type)
  (let ((g (gensym)))
    `(LET ((,g ,value))
       (ETYPECASE ,g (,type ,g)))))

(defun max* (a b)
  (if a (if b (max a b) a) b))

(defvar *note-nest* -1)

(defun note (fmt &rest args)
  (format t
          (format nil "~~&~~<;; ~v<~>~~@;~~?~~:>" (* 2 (max 0 *note-nest*)))
          (list fmt args)))
