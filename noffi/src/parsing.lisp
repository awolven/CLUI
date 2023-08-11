(in-package :noffi)

;;; This is the driver for the grammar implementing backtracking.

(locally
    (declaim (optimize (safety 1) (speed 3)))

(defparameter *global-env-sloc*
  (make-hash-table :test #'eq))

(defvar *global-this-sloc* nil)
(defvar *current-machine* nil)
(defparameter *sloc-table* nil)
(defparameter *observe-line-directives-p* nil)
(defparameter *max-backtrack*
  (or #+WINDOWS 50 10))

(defvar *typedefs* (make-hash-table :test #'eq)
  ;; Maybe we make this part of a machine?
  "To be bound while parsing. A hash table of those symbols that are
   supposed to be `typedef`s.")

(define-condition noffi-parsing-error (simple-error)
  ((token :initarg :token :reader noffi-parsing-error-token))
  (:report report-noffi-parsing-error))

(defun report-noffi-parsing-error (condition stream)
  (multiple-value-bind (format-control format-args tok)
      (values (simple-condition-format-control condition)
              (simple-condition-format-arguments condition)
              (noffi-parsing-error-token condition))
    (cond ((and tok
                (token-sloc tok)
                (line-col-sloc-source (token-sloc tok))
                (ignore-errors (line-col-sloc-start-line (token-sloc tok)))
                (ignore-errors (line-col-sloc-start-col (token-sloc tok))))
           (let ((*standard-output* stream)) ;###
             (show-context-1 (line-col-sloc-source (token-sloc tok))
                             :line-col
                             (list (line-col-sloc-start-line (token-sloc tok))
                                   (line-col-sloc-start-col (token-sloc tok)))
                             format-control format-args) ))
          (t
           (apply #'format stream (simple-condition-format-control condition)
                  (simple-condition-format-arguments condition))))))

(defun noffi-parsing-error (&rest arguments)
  (let ((kws nil))
    (do () ((or (null arguments)
                (null (cdr arguments))
                (not (keywordp (car arguments)))))
      (push (pop arguments) kws)
      (push (pop arguments) kws))
    (apply #'error 'noffi-parsing-error
           :format-control (car arguments)
           :format-arguments (cdr arguments)
           (reverse kws)) ))

;;;; ------------------------------------------------------------------------------------------

(defmacro with-input ((input-var input-form) &body body)
  `(invoke-with-input (lambda (,input-var)
                        ,@body)
                      ,input-form))

(defun invoke-with-input (cont input)
  (etypecase input
    (pathname
     (with-open-file (input input :external-format :iso-8859-1)
       (funcall cont input)))
    #+NIL
    (string
     (DE.BAUHH.FILE:WITH-TEMPONARY-FILE (filename)
       (with-open-file (file filename :direction :output :if-exists :supersede)
         (write-string input file)
         (terpri file))
       (with-open-file (input filename)
         (funcall cont input))))
    (string
     (funcall cont input))
    (stream
     (funcall cont input))))

;;;; ------------------------------------------------------------------------------------------

;; The most tricky part is that C is context sensitive. The semantic meaning
;; of some token sequence depends on what whether some identifier is a typedef
;; or not. [E.g. "a*b" can either be a multiplication or a declaration.]

;; There is IS-TYPEDEF to decide, whether some symbol is a typedef or not. It
;; comes from three places: MACHINE-TYPEDEFS for the current parse, *TYPEDEFS*
;; for the current translation unit, and finally from our runtime global
;; databae.

(defstruct (machine (:constructor cons-machine))
  state la stack
  table
  (typedefs nil)
  env-stack
  (pack-stack '((nil nil)))
  (gcc-attribute-pile nil)
  tokens                                ;Input stream
  (serial 0))                           ;Token number, incremented with each step

(defmethod print-object ((object machine) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "~@<#~D ~S ~_~S~:>"
            (machine-serial object)
            (machine-state object)
            (mapcar (lambda (x) (subseq x 0 2))
                    (reverse (machine-stack object)))
            ;;(mapcar (lambda (x) (subseq x 0 2)) (machine-la object))
            )))

(defun announce-typedef (symbol)
  "Lexical tie-in to declare _symbol_ as a `typedef`."
  (push symbol (machine-typedefs *current-machine*))
  (values))

(defun start-block ()
  "Lexical tie-in to start a new block scope."
  (push (machine-typedefs *current-machine*)
        (machine-env-stack *current-machine*))
  (values))

(defun end-block ()
  "Lexical tie-in to end a block scope."
  (setf (machine-typedefs *current-machine*)
        (pop (machine-env-stack *current-machine*)))
  (values))

(defun is-typedef (symbol &optional (machine *current-machine*))
  "Says whether _symbol_ is a type name at the moment or not."
  (not (not
        (or (and machine
                 (member symbol (machine-typedefs *current-machine*) :test 'eq))
            ;;
            (gethash symbol *typedefs*)
            ;;
            (let ((q (gethash symbol *global-env*)))
              (and q (eq (declaration-storage-class q) :typedef))) ))))

(declaim (inline table-next))
(defun table-next (table state la-cat)
  (gethash la-cat (svref table state)))

(defun make-state-lookup-vector (lalr-table)
  (let ((res (make-array (length (lalr-table-states lalr-table)))))
    (loop for i from 0
          for q in (lalr-table-states lalr-table)
          do (assert (eql i (lalr-state-name q)))
          do (setf (svref res i)
                   (let ((ht (make-hash-table :test #'eq)))
                     (loop for action in (lalr-state-transitions q) do
                           (loop for cat in (lalr-action-categories action) do
                                 (setf (gethash cat ht) action))) ;???
                     ht)))
    res))

#+(or)
(progn
  (defun token-cat (tok)    (first tok))
  (defun token-val (tok)    (second tok))
  (defun token-sloc (tok)   (third tok)))
;; (defun token-serial (tok) (fourth tok))      ;obsolete
;; (defun token-source (tok) (sixth tok))       ;obsolete

#+(or)
(defun combine-slocs (slocs)
  (case (length slocs)
    ((0) nil)
    ((1) (car slocs))
    (t (make-line-col-sloc :start-line (reduce #'min (mapcar #'line-col-sloc-start-line slocs))
                           :start-col (reduce #'min (mapcar #'line-col-sloc-start-col slocs))
                           :end-line (reduce #'max (mapcar #'line-col-sloc-end-line slocs))
                           :end-col (reduce #'max (mapcar #'line-col-sloc-end-col slocs))
                           :source (line-col-sloc-source (car slocs))))))

(defun combine-slocs (slocs)
  (declare (ignore slocs))
  nil)



;; Trampolines for profiling.
(defun funcall-lexer (f) (funcall f))
(defun apply-action (f x) (apply f x))


;;;; Tokenizing

(defstruct line-col-sloc
  start-line start-col
  end-line end-col
  source)

(defmethod print-object ((object line-col-sloc) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "~A:~D:~D .. ~D:~D"
            (and (pathnamep (line-col-sloc-source object)) (namestring (line-col-sloc-source object)))
            (line-col-sloc-start-line object)
            (line-col-sloc-start-col object)
            (line-col-sloc-end-line object)
            (line-col-sloc-end-col object))))

(declaim (inline cons-token token-cat token-val token-sloc))
(locally (declare (optimize (speed 3) (safety 0)))
  (defstruct (token (:constructor cons-token (cat val sloc)))
    cat val sloc))

(defun make-token (&key cat val sloc)
  (assert sloc)
  (cons-token cat val sloc))

(defun make-token-2 (cat val sloc)
  (assert (not (null sloc)))
  (make-token :cat cat
              :val (if (eq cat 'multi-token)
                       (mapcar (lambda (x)
                                 (make-token :cat (car x) :val (cadr x) :sloc sloc))
                               val)
                       val)
              :sloc sloc))

(defun clone-token (prototype &key (cat nil cat-p) (val nil val-p) (sloc nil sloc-p))
  (let ((res (copy-token prototype)))
    (when cat-p (setf (token-cat res) cat))
    (when val-p (setf (token-val res) val))
    (when sloc-p (setf (token-sloc res) sloc))
    res))

(defmethod print-object ((token token) stream)
  (print-unreadable-object (token stream :type t :identity nil)
    (format stream "~S ~S" (token-cat token) (token-val token))))

(defun tokenize (orig-input)
  (let ((t1 (get-internal-real-time)))
    (declare (ignorable t1))
    (prog1
        (tokenize-1 orig-input)
      #+(or)
      (when (and (stringp orig-input) (> (length orig-input) 1000))
        (note ";; Tokenizing took ~:Dms~%"
              (round (/ (- (get-internal-real-time) t1) internal-time-units-per-second) 1/1000))))))

(defun tokenize-1 (orig-input)
  (let (last-sloc
        cur-file
        (cur-line-offset 0))
    (with-input (input orig-input)
      (let ((scanner (make-c-lexer input)))
        (loop for seq from 0
              for tok = (labels ((adjusted-sloc (sloc)
                                   (and sloc
                                        (let ((new (copy-line-col-sloc sloc)))
                                          (incf (line-col-sloc-start-line new) cur-line-offset)
                                          (incf (line-col-sloc-end-line new) cur-line-offset)
                                          (setf (line-col-sloc-source new) orig-input)
                                          ;; ???
                                          (when cur-file
                                            (setf (line-col-sloc-source new) cur-file))
                                          new)) ))
                          (declare (inline adjusted-sloc))
                          (multiple-value-bind (cat val sloc)
                              (handler-bind 
                                  ((error (lambda (cond)
                                            (show-context-1 orig-input :line-col
                                                            (list (line-col-sloc-start-line last-sloc)
                                                                  (line-col-sloc-start-col last-sloc)
                                                                  #+NIL
                                                                  (line-col-sloc-source last-sloc)
                                                                  )
                                                            "Scanner error: ~A" (list cond))
                                            (error cond))))
                                (funcall-lexer scanner))
                            (setq last-sloc sloc)
                            (cond ((eq cat :line-directive)
                                   ;; (print (list cat val))
                                   (when *observe-line-directives-p*
                                     (destructuring-bind (no file) val
                                       ;; (note "file ~S line ~D. We're at line ~D atm" file no (line-col-sloc-start-line sloc))
                                       (setq cur-file (pathname file)
                                             cur-line-offset (- no (line-col-sloc-start-line sloc) 1))
                                       ))
                                   nil)
                                  ((eq cat 'multi-token)
                                   (make-token :cat 'multi-token
                                               :val (mapcar (lambda (x)
                                                              (make-token :cat (car x) :val (cadr x) :sloc sloc))
                                                            val)
                                               :sloc (adjusted-sloc sloc)))
                                  (t
                                   (make-token :cat cat :val val :sloc (adjusted-sloc sloc))))))
              when tok collect tok
              until (and tok (eq :eof (token-cat tok))))))))


;;;; ------------------------------------------------------------------------------------------

(defgeneric show-context-1 (source sloc-kind sloc-args format-control format-args))

(defmethod show-context-1 ((source pathname) sloc-kind sloc-args format-control format-args)
  (with-open-file (input source)
    (show-context-1 input sloc-kind sloc-args format-control format-args)))

(defmethod show-context-1 ((source file-stream) (sloc-kind (eql :line-col)) sloc-args format-control format-args)
  (labels ((aux (source)
             (let ((old (file-position source)))
               (unwind-protect
                    (progn
                      (file-position source 0)
                      (show-context-line-col-from-start source sloc-kind sloc-args format-control format-args))
                 (file-position source old)))))
    (with-open-file (input source)
      (aux input))))

(defmethod show-context-1 ((source string) (sloc-kind (eql :line-col)) sloc-args format-control format-args)
  (show-context-line-col-from-start (make-string-input-stream source) sloc-kind sloc-args format-control format-args))

(defvar +uparrow-char+
  (or (code-char 8593) #\^))

(defun show-context-line-col-from-start (source sloc-kind sloc-args format-control format-args)
  (declare (ignore sloc-kind))
  (destructuring-bind (line-no col-no) sloc-args
    (format t "~&;; **** ~?~%" format-control format-args)
    (let ((pn (ignore-errors (pathname source))))
      (when pn (format t "~&;; * ~S~%" pn)))
    (let ((start-line (- line-no 3))
          (end-line (+ line-no 3)))
      (loop for line = (read-line source nil nil)
            for cur-line-no from 1
            while (and line (< line-no end-line))
            do (when (<= start-line cur-line-no end-line)
                 (if (= cur-line-no line-no)
                     (format t "~&;; ~6D:[1;31m ~A[0m~%" cur-line-no (verbatim line))
                     (format t "~&;; ~6D: ~A~%" cur-line-no (verbatim line)))
                 (when (= cur-line-no line-no)
                   (format t "~&;;         ~v<~A~>~%" (1+ col-no) (verbatim +uparrow-char+))
                   '(format t "~&;; **** ~?~%" format-control format-args)))))))


;;;; ------------------------------------------------------------------------------------------

(defun parse-expression (input)
  (values (bf-parse input :inject '(@expression))))

(defun parse-type (input)
  (values (bf-parse input :inject '(@type))))

(defun parse-statement (input)
  (values (bf-parse input :inject '(@statement))))

(defun parse-top-level (input)
  (values (bf-parse input :inject nil)))


;;;; -- Backtracking Parser Driver ------------------------------------------------------------

(defun tok-candidates (m tok &aux res)
  (cond ((eq 'multi-token (token-cat tok))
         (loop for x in (token-val tok)
               ;; do (assert (token-p tok))
               nconc (tok-candidates m x)))
        ((eq :identifier (token-cat tok))
         (let ((typedef-p (let ((*current-machine* m)) (is-typedef (token-val tok)))))
           (if typedef-p
               (progn (push tok res) (push (clone-token tok :cat :typedef-name) res))
               (progn (push (clone-token tok :cat :typedef-name) res) (push tok res))))
         res)
        (t
         (list tok))))

(defun balanced-member (item list &key (key #'identity) (nest 0))
  (cond ((null list) nil)
        (t
         (let ((q (funcall key (car list))))
           (cond ((and (eql q item) (eql 0 nest)) list)
                 ((eq q :\() (balanced-member item (cdr list) :key key :nest (1+ nest)))
                 ((eq q :\)) (balanced-member item (cdr list) :key key :nest (1- nest)))
                 (t          (balanced-member item (cdr list) :key key :nest nest)))))))

(defun machine-accpets-category (m cat)
  (and (integerp (machine-state m))
       (table-next (machine-table m) (machine-state m) cat)))

;;;

(defun reduce-action-function-1 (q)
  (reduce-action-function q))

(let ((hash (make-hash-table)))
  (defun lalr-table-state-vector (lalr-table)
    (or (gethash lalr-table hash)
        (setf (gethash lalr-table hash) (make-state-lookup-vector lalr-table)))))

(defun make-machine (table)
  (cons-machine :table (lalr-table-state-vector table)
                :state (lalr-state-name (first (lalr-table-states table)))
                :la nil :stack nil))


;;;; More Robust Parsing

;; This parsing technique is known as "machete parsing" according to ck_.

;; The idea is to first identify the toplevel declarations and then parse
;; these one for one. This allows us to skip some declarations that we don't
;; get.

;; The idea is that every ";" or "("...")" "{"..."}" ends one declaration.

(defun bf-parse (input &key inject (grammar 'c99-parser))
  (cond ((not (null inject))
         (parse-token-list (butlast (tokenize input)) :inject inject :grammar grammar))
        (t
         (let ((*typedefs* (make-hash-table :test #'eq))
               (machine (make-machine (get grammar 'lalr-table))))
           (values
            (cons-progn
             (mapcar (lambda (k)
                       (cond ((eq (token-val (car k)) 'noffi-c::|class|)
                              (warn "Skipping ~A ~A"
                                    (verbatim (token-val (car k)))
                                    (verbatim (token-val (cadr k))))
                              nil)
                             (t
                              (let ((*global-this-sloc* (token-sloc (car k))))
                                ;;(prin1 (line-col-sloc-start-line *global-this-sloc*)) (princ " ") (when (> (ccl:stream-line-column *standard-output*) 96) (terpri)) (force-output)
                                (multiple-value-bind (form out-machine)
                                    (handler-case
                                        (parse-token-list k :inject inject :grammar grammar :proto-machine machine)
                                      #+NIL
                                      (error ()
                                        (values nil nil)))
                                  (when out-machine
                                    (setf machine out-machine)
                                    (dolist (p (machine-typedefs out-machine)) (setf (gethash p *typedefs*) t)))
                                  '(ignore-errors(eval form))
                                  form)))))
                     (token-list-declaration-subseqs (tokenize input))))
            *typedefs*)))))

;; Another approach:

#+(or)
(defun token-list-declaration-subseqs (token-list)
  (let ((stack nil)                     ;stack of parens
        (cur nil)                       ;current subseq
        (res nil)                       ;all subseqs
        (last-was-paren-expr nil))
    (macrolet ((spill () `(when cur (push (reverse cur) res) (setq cur nil))))
      ;;
      (do ((q token-list (cdr q)))
          ((endp q))
        (let ((token (car q)))
          (push token cur)
          (case (token-main-cat token)
            ((:\( :\[ :\{) (push (token-cat token) stack))
            (:\) (assert (eq :\( (car stack))) (pop stack))
            (:\] (assert (eq :\[ (car stack))) (pop stack))
            (:\} (assert (eq :\{ (car stack)) nil "~S" stack) (pop stack)
                 (when (and (null stack) last-was-paren-expr)
                   (spill)))
            (:\; (when (and (null stack)
                            ;; This is here to catch K&R functions.
                            (or (null (cdr q))
                                (not (eql :\{ (token-cat (cadr q))))))
                   (spill)))
            ((:\+ :\-)
             (when (and (null stack))
               (pop cur)
               (spill)
               (push token cur)))
            ((:@interface :@protocol)
             ;; 
             (do ((p (cdr q) (cdr p)))
                 ((or (null p)
                      (eql (token-main-cat (car p)) :@end))
                  (unless p (error "No @end?"))
                  (when p (push (car p) cur))
                  (spill)
                  (setq q p))
               (push (car p) cur))
             '(let ((p (member :@end q :key #'token-main-cat)))
               (pop cur) (spill)
               (unless p (error "No @end?"))
               (push (ldiff q (cdr p)) res)
               (setq q p)))
            ((:@end)
             (blame-for-token (car q) "Stray @end?"))
            (:eof (pop cur)))
          (when (null stack)
            (setq last-was-paren-expr (eq :\) (token-cat token)))) ))
      ;;
      (spill)
      (reverse res))))

#+(or)
(defun blame-for-token (tok format-control &rest format-args)
  (cond
    ((and tok
          (token-sloc tok)
          (line-col-sloc-source (token-sloc tok))
          (ignore-errors (line-col-sloc-start-line (token-sloc tok)))
          (ignore-errors (line-col-sloc-start-col (token-sloc tok))))
     (show-context-1 (line-col-sloc-source (token-sloc tok))
                     :line-col
                     (list (line-col-sloc-start-line (token-sloc tok))
                           (line-col-sloc-start-col (token-sloc tok)))
                     format-control format-args))
    (t
     (warn "Some parsing error, but cannot tell. ~?~%token = ~S"
           format-control format-args
           tok))))

(defun blame-for-token (token format-control &rest format-args)
  (apply #'noffi-parsing-error :token token format-control format-args))
         


;;;;


;;;; On Declaration Specifiers and Type-Qualifers

;; We talk about things like __attribute__((aligned(n))) or __declspec(...).

;; Microsoft's __declspec is clean and applies some attribute the declaration
;; itself not to the type of the thing declared. However with a `typedef` this
;; might spill:

;;     typedef __declspec(align(16)) char a;

;;     struct foo { a b, c };

;; Makes the _b_ and _c_ members obey the alignment. However, when we say

;;    __declspec(align(16)) char a;
;;    decltype(a) b;            // alignment doesn't spill over

;; That is the type of _a_ here still is `char` and not some
;; `__declspec(align(16)) char` or some such.

;; With __attribute__(...) clang follows Microsoft here, only that
;; __attribute__(...) may appear anywhere, like:

;;    char ** __attribute__((aligned(16)) a;

;; This makes an aligned pointer and not a pointer to an aligned char.

;; GCC is the most messy. __attribute__((aligned(n))) may act like a type
;; qualifier in e.g.

;;    char * __attribute__((aligned(16))) * a;

;; This declares a pointer to an aligned pointer to a char. Not so with clang.

;; Conclusion: It's a mess!

;; We follow the majority here, that is clang and MSVC. A
;; __attribute__((aligned(16))) is a declaration specifier and applies to the
;; thing declared, no matter where it occurs.

;; Tricky are `typedef`s though. A declaration specifier spills over to
;; another declaration.


;;;; Backtracking Parser

;; Due to ambiguity introduced by the identifier / type-name dualism, we use a
;; simple backtracking parser. The grammar itself still is LALR(1) and
;; backtracking happens on the token level. Each token could be of multiple
;; categories. The algorithm does a depth-first search and for each token all
;; the categories are tried in order. Hence the order of the categories
;; reported for a token matters.

;; We report an identifier that is a defined type first as typedef-name and
;; only then as an identifier. For identifiers that don't name types, we do
;; the reverse.

;; This algorithm is formulated by keeping a queue of machines to further.
;; Machines that are more than *MAX-BACKTRACK* behind the most successful seen
;; sofar die off, in an attempt to limit the maximum range of backtracking.

(defun bt-init (input &optional (grammar 'c99-parser))
  (let ((m (make-machine (get grammar 'lalr-table))))
    (setf (machine-tokens m) (tokenize input))
    m))

(defun parse-token-list (tokens &key (proto-machine nil) (inject nil) (grammar 'c99-parser) source)
  (let ()
    (setq tokens (append
                  (mapcar (lambda (x)
                            (make-token :cat x :val x
                                        :sloc (and tokens
                                                   ;; ### Hmm
                                                   (token-sloc (car tokens)))))
                          inject)
                  tokens
                  (list
                   (make-token :cat :eof
                               :val :eof
                               :sloc (and tokens (token-sloc (car (last tokens))))
                               ))))
    (let ((m (make-machine (get grammar 'lalr-table))))
      ;; Hmm.
      (when proto-machine
        (setf (machine-pack-stack m) (machine-pack-stack proto-machine))
        '(setf (machine-typedefs m) (machine-typedefs proto-machine)))
      ;;
      (let ((m (bt-parse-1 m tokens)))
        (values (and m (cadar (machine-stack m)))
                m)))))

(defun bt-parse-1 (q0 input)
  (let* ((qs (list q0))
         (max-serial 0)
         (best-state nil)
         (best-machine nil)
         (best-tok nil)
         (err-qs nil))
    (setf(machine-tokens q0) input)
    (prog ()
     :loop
     (unless qs
       (when best-tok
         (let ((q (aref (machine-table q0) best-state)))
           (let ((putative-tokens nil))
             (maphash (lambda (k v) (declare (ignore v)) (push k putative-tokens)) q)
             (let ((expected
                    nil
                     #+(or)
                    (loop for tok in putative-tokens
                          when (bf-step (copy-machine best-machine)
                                        (make-token :cat tok)) ;###
                          collect tok)))
               ;; ### This must be a dry run w/o invoking actions?!
               (setq expected (mapcar (lambda (x)
                                        (or (cdr (assoc x (fancy-lalr-table-terminals (get 'c99-parser 'lalr-table))))
                                            x))
                                      expected))
               (blame-for-token best-tok "~@<Parse error in state ~D: Saw ~S.~_~@<Expected one of ~{~S~^, ~:_~}~:>~:>"
                                best-state
                                best-tok
                                expected))))))
     (let ((q (find :fin qs :key #'machine-state)))
       (when q (return q)))
     (let ((head (or (pop qs)
                     (pop err-qs))))
       (unless head (return nil))
       ;; (print `(head = ,head la = ,(machine-tokens head)))
       (when (<= (- max-serial (machine-serial head)) *max-backtrack*)
         (when (> (1+ (machine-serial head)) max-serial)
           (setq best-machine head)
           (setq best-state (machine-state head))
           (setq best-tok (car (machine-tokens head))))
         (setq max-serial (max (1+ (machine-serial head)) max-serial))
         (multiple-value-bind (nqs err-q)
             (bt-step head)
           (setq qs (nconc nqs qs))
           (when err-q (push err-q err-qs)))))
     (go :loop))))

(defun bt-step (m)
  (setq m (last-minute-transducer m))
  (cond ((null (machine-tokens m))
         (if (eq :fin (machine-state m)) (list m) nil))
        (t
         (let* ((tok (car (machine-tokens m)))
                #+(or) ;not yet
                (err-tok (list* :error :error (cddr tok)))
                (candidates* (tok-candidates m tok)))
           ;;
           (values
            (nconc (when (token-cat-p tok :>>)
                     (let ((m (copy-machine m)))
                       (setf (machine-tokens m)
                             (list* (clone-token tok :cat :>)
                                    (clone-token tok :cat :>)
                                    (cdr (machine-tokens m))))
                       (setq m (bf-step m (pop (machine-tokens m))))
                       (and m (list m))))
                   (loop for q on candidates*
                         for m* = (bf-step (let ((m (if '(cdr q) (copy-machine m) m)))
                                             (pop (machine-tokens m))
                                             m)
                                           (car q))
                         when m* collect m*)
                   (loop for tie in '(:--tie-- :--tie2-- :--tie3--)
                         nconc
                         (when (machine-accpets-category m tie)
                           (let ((m (copy-machine m)))
                             (setf (machine-tokens m)
                                   (list* (clone-token tok :cat tie)
                                          (machine-tokens m)))
                             (setq m (bf-step m (pop (machine-tokens m))))
                             (and m (list m))))))
            #+(or) ;Not there yet
            (bf-step (copy-machine m) err-tok))))))

(defun bf-step (m token)
  (handler-bind
      ((error (lambda (cond)
                (format t "~&ERROR: ~A~%" cond)
                (with-simple-restart (:backtrack "Backtrack and try something else")
                  (invoke-debugger cond))
                (return-from bf-step (values nil nil)))))
    (setq m (step-machine m token))
    (incf (machine-serial m))
    (cond ((eq :fail (machine-state m))
           (values nil nil))
          ((eq (token-cat token) :error)
           (let ((h (aref (machine-table m) (machine-state m))))
             (assert (= 1 (hash-table-count h)))
             (let (need)
               (maphash (lambda (k v) (declare (ignore v)) (setq need k)) h)
               (let ((putative-tokens (balanced-member need (machine-tokens m) :key #'token-cat)))
                 (cond ((null putative-tokens)
                        ;; no luck
                        (values nil nil))
                       (t
                        (blame-for-token token "Parse Error; recovering.")
                        (setf (machine-tokens m) putative-tokens)
                        (values m t)))))))
          (t
           (values m t)))))

(defun step-machine (m input)
  (declare (type machine m) (optimize (speed 3) (safety 0)))
  (let ((*current-machine* m))
    (let ((la (list (list (token-cat input) (token-val input) (token-sloc input))))
          (stack (machine-stack m))
          (state (machine-state m))
          (table (machine-table m))
          next)
      (prog ()
       :loop
       (when (null la) (return))
       (setq next (and (not (eq state :fail)) ;This should not happen in the first place.
                       (table-next table state (caar la))))
       :dispatch
       (unless next
         (setq state :fail)
         (return))
       (etypecase next
         (shift-action
          (push (cons state (cdr (pop la))) stack)
          (setq state (lalr-action-goto next)))
         (reduce-action
          (locally (declare (type reduce-action next))
            (let ((new-cat (lalr-action-goto next)))
              (cond ((eq '$start new-cat)
                     (setf state :fin)
                     (return))
                    (t
                     ;; The arguments are in reverse on the stack, so the end-sloc can be
                     ;; found at the top-most element.
                     (let* ((args nil)
                            (new-sloc nil)
                            (slocs nil))
                       (loop repeat (reduce-action-npop next)
                             do (let ((q (pop stack)))
                                  (setq state (car q))
                                  (when (third q)
                                    (push (third q) slocs))
                                  (push (cadr q) args)))
                       (setq new-sloc (combine-slocs slocs))
                       ;; (format t "~&**** invoking ~S with ~S" (reduce-action-function next) args)
                       (let ((semantic-value
                              (apply-action (reduce-action-function-1 next) args)))
                         (unless (atom semantic-value)
                           (when (and *sloc-table*
                                      (not (gethash semantic-value *sloc-table*)))
                             ;; ### gross hack
                             (when (and (consp semantic-value) (eq 'decl (car semantic-value)))
			       #+NIL
                               (when (pathnamep (line-col-sloc-source new-sloc))
                                 (setq semantic-value
                                       (list* (car semantic-value)
                                              (cons `(:source-location
                                                      ,(line-col-sloc-source new-sloc)
                                                      ,(line-col-sloc-start-line new-sloc)
                                                      ,(line-col-sloc-end-line new-sloc))
                                                    (cadr semantic-value))
                                              (cddr semantic-value)))))
                             (setf (gethash semantic-value *sloc-table*)
                                   new-sloc)))
                         ;; (setq next (table-next '(machine-table m) state new-cat))
                         (push (list new-cat
                                     semantic-value
                                     new-sloc)
                               la)))))))))
       (go :loop))
      (setf (machine-state m) state)
      (setf (machine-stack m) stack)
      (setf (machine-la m) la)))
  m)

(defun last-minute-transducer (m)
  "Destructive!"
  ;; This is a last minute transducer to be run right in front of STEP-MACHINE.
  (let ((tok (car (machine-tokens m))))
    (cond ((eq (token-cat tok) :line-directive)
           ;; This might happen inside __attribute__
           (pop (machine-tokens m))
           (last-minute-transducer m))
          ((eq (token-cat tok) :pragma)
           (pop (machine-tokens m))
           (let ((val (token-val tok)))
             ;; Use LALR(1) rather. Have a sub-grammar for #pragma.
             (ecase (car val)
               (:pack
                (labels ((set-pack (n)
                           (setf (machine-pack-stack m)
                                 (cons (list n (cadr (car (machine-pack-stack m))))
                                       (cdr (machine-pack-stack m))))))
                  (let ((op (cadr val)))
                    (ecase (car op)
                      (:push
                       (destructuring-bind (id? value) (cdr op)
                         (push (list (caar (machine-pack-stack m)) id?)
                               (machine-pack-stack m))
                         (when value (set-pack value)))) ;??
                      (:pop
                       (destructuring-bind (id? value) (cdr op)
                         (cond ((<= (length (machine-pack-stack m)) 1)
                                (warn "#pragma pack stack underflow with ~S" (machine-pack-stack m))
                                (setf (machine-pack-stack m) (list (list nil nil))))
                               (t
                                (pop (machine-pack-stack m))))
                         '(set-pack value)))
                      (:set
                       (destructuring-bind (value) (cdr op)
                         (set-pack value) ))))))))
           (last-minute-transducer m))
          ((eq (token-cat tok) :__attribute__)
           (pop (machine-tokens m))
           (setf (machine-tokens m)
                 (nconc (mapcar (lambda (attribute)
                                  ;; Fake the sloc. Can we do better?
                                  (cond ((eq (car attribute) 'multi-token)
                                         (make-token :cat 'multi-token
                                                     :val (mapcar (lambda (x)
                                                                    (make-token :cat (car x)
                                                                                :val (cadr x)
                                                                                ;; ###
                                                                                :sloc (token-sloc tok)))
                                                                  (cdr attribute))
                                                     :sloc (token-sloc tok) ))
                                        (t
                                         (make-token :cat (car attribute)
                                                     :val (cadr attribute)
                                                     ;; ###
                                                     :sloc (token-sloc tok)))))
                                (parse-gcc-attribute-from-tokens (token-val tok) m))
                        (machine-tokens m)))
           (last-minute-transducer m))
          (t
           m))))

(defmacro bench (form)
  `(progn
     (eval (read-from-string "(monitor:monitor-all :noffi)"))
     (progn ,form)
     (eval (read-from-string "(monitor:report)"))
     (eval (read-from-string "(monitor:unmonitor)"))))

(defvar *last-good-token*)              ;Obssolete

(defun blame (form format-control &rest format-arguments)
  (let ((sloc (or (and *sloc-table* (gethash form *sloc-table*))
                  (and (boundp '*last-good-token*)
                       *last-good-token*
                       (token-sloc *last-good-token*)))))
    (cond (sloc
           (show-context-1 (line-col-sloc-source sloc)
                           :line-col
                           (list (line-col-sloc-start-line sloc)
                                 (line-col-sloc-start-col sloc))
                           format-control format-arguments)
             (error "~S: ~?" form format-control format-arguments))
          (t
           (apply #'error format-control format-arguments)))))

) ;fin
