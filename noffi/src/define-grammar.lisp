(in-package :noffi)

(locally (declaim (optimize (speed 1) (safety 3)))

(defstruct (fancy-lalr-table (:include lalr-table))
  terminals)

(defmacro define-grammar (name &rest rules)
  (expand-define-grammar name rules))

(defun expand-define-grammar (grammar-name rules)
  (multiple-value-bind (rules tokens options)
      (edit-grammar rules)
    (let* ((defined     (remove-duplicates (mapcar #'car rules)))
           (used        (remove-duplicates (cons (caar rules) (mapcan #'copy-list (mapcar #'cadr rules)))))
           (undef       (remove-if (lambda (x) (or (assoc x tokens) (member x defined))) used))
           (unused      (set-difference defined used))
           (terminals   (set-difference used defined)))
      (when undef
        (warn "Undefined: ~S" undef))
      (when unused
        (warn "Unused non-terminals: ~S" unused))
      (let ((aux-defuns nil))
        (setq rules (mapcar (lambda (rule)
                              (destructuring-bind (lhs rhs lambda-form) rule
                                (let ((name
                                       (loop for k from 1
                                             for name = (intern (format nil "ACTION/~A ~A ~D" (verbatim grammar-name) (verbatim lhs) k))
                                             until (not (find name aux-defuns :key 'cadr))
                                             finally (return name)))
                                      (lambda-form
                                       (etypecase lambda-form
                                         ((cons (member LAMBDA) t) lambda-form)
                                         ((cons (member FUNCTION)
                                                (cons (cons (member LAMBDA) t)
                                                      null))
                                          (cadr lambda-form)))))
                                  (push `(DEFUN ,name ,@(cdr lambda-form)) aux-defuns)
                                  (list lhs rhs name))))
                            rules))
        (setq aux-defuns (reverse aux-defuns))
        (let ((lalr-table
               (grammar-lalr-table terminals `(,@options ,@rules))))
          ;;
          (setq lalr-table (make-fancy-lalr-table
                            :topcat (lalr-table-topcat lalr-table)
                            :states (lalr-table-states lalr-table)
                            :terminals tokens))
          ;;
          `(progn
             ,@aux-defuns
             (setf (get ',grammar-name 'lalr-table)
                   ',lalr-table)
             ',grammar-name))))))

(defun grammar-lalr-table (terminals rules)
  (de.bauhh.lalr::grammar-lalr-table
   terminals
   (mapcar (lambda (rule)
             (cond ((member (car rule) '(:precedence)) rule)
                   (t `(,(car rule) -> ,@(cadr rule) #',(caddr rule)))))
           rules)))



(defmethod make-load-form ((object lalr-table) &optional environment)
  (declare (ignore environment))
  `(cons-lalr-table :topcat ',(lalr-table-topcat object)
                    :states ',(lalr-table-states object)))

(defmethod make-load-form ((object fancy-lalr-table) &optional environment)
  (declare (ignore environment))
  `(make-fancy-lalr-table :topcat ',(lalr-table-topcat object)
                          :states ',(lalr-table-states object)
                          :terminals ',(fancy-lalr-table-terminals object)))

(defmethod make-load-form ((object lalr-state) &optional environment)
  (declare (ignore environment))
  `(make-lalr-state :name ',(lalr-state-name object)
                    :transitions ',(lalr-state-transitions object)))

(defmethod make-load-form ((object shift-action) &optional environment)
  (declare (ignore environment))
  `(make-shift-action :categories ',(lalr-action-categories object)
                      :goto ',(lalr-action-goto object)))

(defmethod make-load-form ((object reduce-action) &optional environment)
  (declare (ignore environment))
  `(make-reduce-action :categories ',(lalr-action-categories object)
                       :goto ',(lalr-action-goto object)
                       :npop ',(reduce-action-npop object)
                       :function ',(reduce-action-function-1 object)))


;;;;

(defun edit-grammar (rules)
  (let* ((queue nil)                             ;A queue of rules in (lhs -> rhs ..) form to process
        (bag nil)                               ;Result list as (lhs rhs-list . action-body)
        (taken-names nil)                       ;A list of all non-terminal names already present
         (user-tokens nil)
         (rules (remove-if (lambda (r)
                             (cond ((and (consp r) (member (car r) '(:tokens :token)))
                                    (setq user-tokens (append user-tokens (cdr r)))
                                    t)))
                           rules))
        (options (remove-if-not #'keywordp rules :key 'car))
        (rules   (remove-if #'keywordp rules :key 'car))
        ;; Memorizes rhs items, so that we have only one rule for multiple e.g. (? foo)'s
        (memo    (make-hash-table :test #'equal))
        (tokens  nil))                          ;A-list of token keywords
    (labels ((edit-rule (rule)
               (de.bauhh.compiler-warn:compiler-descend rule
                 (loop for (lhs rhs . actions) in (reverse (parse-rule rule)) do
                       (edit-rule-1 lhs rhs actions))))
             ;;
             (edit-rule-1 (lhs rhs actions)
               (unless actions
                 (setq actions (list (rule-default-action lhs rhs))))
               (edit-rhs-items lhs rhs actions
                               1 (form-all-$n-symbol-map actions)
                               nil nil nil))
             ;;
             (gen-$n (n) (make-symbol (format nil "$~D" n)))
             ;;
             (edit-rhs-items (rule-name items body n map yet ps bs)
               ;;
               ;; _rule-name_ is the name of the overall rule (its lhs)
               ;; _items_ is the tail of remaining rhs items
               ;; _body_ is the body of the rule.
               ;; _n_ counts up and is the (one-based) position in the rhs
               ;; _map_ is a map of rule positions to a list of user variables
               ;;
               ;; This routine gets the accumulated new rule in form of:
               ;;
               ;; _yet_ is the newly constructed rhs
               ;;
               ;; _ps_ is the newly constructed list of the parameters of the rule lambda
               ;;
               ;; _bs_ is a list of (<user-var> <form>) bindings for the rule lambda, the <form>
               ;;      usually is a parameter but could also be any form. E.g. NIL for (? ..) or
               ;;      (REVERSE ..) for lists.
               ;;
               ;; And finally pushes the resulting rule as (<lhs> <rhs-list> <lambda>) onto _bag_.
               ;;
               ;; Anything that can derive epsilon like (? ..) or (* ..) is implemented by
               ;; copying the rule, if needed. Once with the optional item, once w/o. This is
               ;; because otherwise adjacent optional item would result into conflicts.
               ;;
               (cond ((null items)
                      (push (list rule-name yet
                                  `(lambda ,ps
                                     (declare (ignorable ,@ps))
                                     (symbol-macrolet ,bs
                                       ,@body)))
                            bag))
                     (t
                      (destructuring-bind (item &rest more) items
                        (labels ((yield (new)
                                   (let ((p (gen-$n n)))
                                     (edit-rhs-items rule-name more body (1+ n) map
                                                     (append yet (list new))
                                                     (append ps (list p))
                                                     (append bs (mapcar (lambda (v) (list v p))
                                                                        (cdr (assoc n map))))))))
                          (cond ((keywordp item) (yield (intern-token item)))
                                ((symbolp item) (yield item)) 
                                ((stringp item) (yield (intern-token item)))
                                ((and (consp item) (symbol= (car item) '?))
                                 (edit-rhs-items rule-name (cons `(AND ,@(cdr item)) more) body
                                                 n map
                                                 yet ps bs)
                                 (edit-rhs-items rule-name more body
                                                 (1+ n) map yet
                                                 ps
                                                 (append bs (mapcar (lambda (v) (list v 'NIL))
                                                                    (cdr (assoc n map))))))
                                ((and (consp item) (symbol= (car item) 'or))
                                 (dolist (k (cdr item))
                                   (edit-rhs-items rule-name (cons k more) body
                                                   n map
                                                   yet ps bs)))
                                ((and (consp item) (symbol= (car item) 'and))
                                 (multiple-value-bind (item-rhs item-action-body)
                                     (parse-item-macro item (cdr item))
                                   (cond ((and (null item-action-body)
                                               (= 1 (length item-rhs)))
                                          (edit-rhs-items rule-name (cons (car item-rhs) more) body
                                                          n map
                                                          yet ps bs))
                                         (t
                                          (let ((aux-name
                                                 (or (gethash item memo)
                                                     (setf (gethash item memo)
                                                           (let ((aux-name (gen-name rule-name 'aux)))
                                                             (edit-rule-1 aux-name item-rhs item-action-body)
                                                             aux-name)))))
                                            (edit-rhs-items rule-name (cons aux-name more) body
                                                            n map
                                                            yet ps bs))))))
                                ;;
                                ((and (consp item) (symbol= (car item) '+))
                                 (let ((aux-name (or (gethash item memo)
                                                     (setf (gethash item memo)
                                                           (let ((aux-name (gen-name-2 rule-name (cdr item) 'list)))
                                                             (edit-rule-1 aux-name (list `(and ,@(cdr item))) '((LIST $1)))
                                                             (edit-rule-1 aux-name (list aux-name `(and ,@(cdr item))) '((CONS $2 $1)))
                                                             aux-name)))))
                                   (let ((p (gen-$n n)))
                                     (edit-rhs-items rule-name more body
                                                     (1+ n) map
                                                     (append yet (list aux-name))
                                                     (append ps (list p))
                                                     (append bs (mapcar (lambda (v) (list v `(reverse ,p)))
                                                                        (cdr (assoc n map))))))))
                                ;;
                                ((and (consp item) (symbol= (car item) '++))
                                 (let ((aux-name (or (gethash item memo)
                                                     (setf (gethash item memo)
                                                           (let ((aux-name (gen-name-2 rule-name (cddr item) 'list)))
                                                             (edit-rule-1 aux-name (list `(and ,@(cddr item))) '((LIST $1)))
                                                             (edit-rule-1 aux-name (list aux-name (cadr item) `(and ,@(cddr item))) '((CONS $3 $1)))
                                                             aux-name)))))
                                   (let ((p (gen-$n n)))
                                     (edit-rhs-items rule-name more body
                                                     (1+ n) map
                                                     (append yet (list aux-name))
                                                     (append ps (list p))
                                                     (append bs (mapcar (lambda (v) (list v `(reverse ,p)))
                                                                        (cdr (assoc n map))))))))
                                ;;
                                ((and (consp item) (symbol= (car item) '*))
                                 (edit-rhs-items rule-name (cons `(? (+ ,@(cdr item))) (cdr items)) body n map yet ps bs))
                                ;;
                                ((and (consp item) (symbol= (car item) '**))
                                 (edit-rhs-items rule-name (cons `(? (++ ,@(cdr item))) (cdr items)) body n map yet ps bs))
                                ;;
                                (t
                                 (de.bauhh.compiler-warn:compiler-warn item "Bad rhs item - ~S" item))))))))
             ;;
             (parse-item-macro (whole args)
               (when (or (> (count '=> args :test #'symbol=) 1)
                         (> (count '-> args :test #'symbol=) 0))
                 (error "Malformed item: ~S" whole))
               (let* ((q (member '=> args :test #'symbol=))
                      (rhs (ldiff args q))
                      (actions (cdr q)))
                 (values rhs actions)))                 
             ;;
             (gen-name (prefix suffix)
               (do* ((i 0 (+ i 1))
                     (s (format nil "~A-~A" (verbatim prefix) (verbatim suffix))
                        (format nil "~A-~A-~D" (verbatim prefix) (verbatim suffix) i)))
                    ((or (null (find-symbol s)) (not (member (intern s) taken-names)))
                     (setq s (intern s))
                     (pushnew s taken-names)
                     s)))
             ;;
             (gen-name-2 (rule-name item-rhs suffix)
               (gen-name (or (and item-rhs (symbolp (car item-rhs)) (car item-rhs))
                             (format nil "~A-AUX" (verbatim rule-name)))
                         suffix))
             ;;
             (intern-token (tok)
               (cond ((or (symbolp tok) #+NIL (stringp tok))
                      (pushnew (list tok) tokens :key #'car :test #'equal)
                      tok)
                     ((stringp tok)
                      (let* ((s (intern (string-upcase tok) :keyword))
                             (q (assoc s tokens :test #'equal)))
                        (cond (q (unless (equal (cdr q) tok)
                                   (error "Oops, different spelling found. ~S vs ~S" tok (cdr q))))
                              (t (push (cons s tok) tokens)))
                        s))
                     (t (error "Odd token: ~S" tok)))) )
      ;;
      (setq queue (reverse rules))
      (mapc #'intern-token user-tokens)
      (dolist (k queue) (pushnew (car k) taken-names))
      (do () ((null queue)) (edit-rule (pop queue)))
      ;;
      (values
       bag
       tokens
       (mapcar (lambda (option)
                 (destructuring-bind (key &rest more) option
                   (cond #+NIL
                         ((eq :tokens key)
                          (cons key (mapcar #'intern-token more)))
                         ((eq :precedence key)
                          (cons key (mapcar (lambda (q)
                                              (cons (car q) (mapcar #'intern-token (cdr q))))
                                            more)))
                         ((and (keywordp key) (symbol= (car more) '->))
                          (intern-token key)
                          option)
                         (t option))))
               options)))))

(defun parse-rule (rule &aux res)
  ;; Takes a rule in (<lhs> { -> <rhs> [ => <action> ] }* form and returns
  ;; a list of (<lhs> <rhs> . <body>)
  (labels ((blame (p what)
             (declare (ignore p))
             (de.bauhh.compiler-warn:compiler-error
              rule
              "~@<Bad rule: ~S; ~_~A~:>" rule (verbatim what))))
    (let ((p (position '-> rule :test #'symbol=)))
      (unless p (blame p "Missing ->"))
      (unless (and (eql p 1) (symbolp (car rule)))
        (blame p "Left hand side before first -> must be exactly one symbol."))
      (let ((name (car rule)) (p1 p) p2)
        (loop
          (setq p2 (position '-> rule :test #'symbol= :start (1+ p1)))
          (let ((p3 (position '=> rule :test #'symbol= :start (1+ p1) :end p2)))
            (push (list* name
                        (subseq rule (1+ p1) (or p3 p2))
                        (and p3 (subseq rule (1+ p3) p2))) res))
          (unless p2 (return))
          (setq p1 p2))
        (reverse res)))))

(defun symbol= (sym what)
  (and (symbolp sym) (not (keywordp sym))
       (symbolp what) (not (keywordp what))
       (string= sym what)))

(defun rule-default-action (lhs rhs)
  (declare (ignore lhs))
  (block nil
    (when (= 0 (length rhs)) nil)
    (when (= 1 (length rhs)) (return '$1))
    (ignore-errors
      (destructuring-bind (lhs op rhs) rhs
        (declare (ignore lhs rhs))
        (when (and (or (stringp op) (keywordp op)))
          (return `(list ',(intern (string-upcase (string op))) $1 $3)))))
    (ignore-errors
      (destructuring-bind (op rhs) rhs
        (declare (ignore rhs))
        (when (and (or (stringp op) (keywordp op)))
          (return `(list ',(intern (string-upcase (string op))) $2)))))
    (cons 'list
          (loop for i from 1 to (length rhs)
                collect (intern (format nil "$~D" i))))))


;;;; Borrowed from CLEX

(defun form-all-$n-symbol-map (form &aux res)
  "Returns an alist mapping integers `n' to all symbols named `$n' in the
s-expression `form'. For ease, we map `$$' as n=0."
  (loop for sym in (all-symbols-in-form form)
        for n = ($n-symbol-p sym) do
        (when n
          (pushnew sym (cdr (or (assoc n res)
                                (car (push (list n) res)))))))
  res)

(defun $n-symbol-p (symbol)
  "Does the symbol `symbol` have a name like $<n>, <n> some positive
integer? Returns that `n' or NIL."
  (and (symbolp symbol)
       (not (keywordp symbol))
       (let ((name (string symbol)))
         (cond ((and (> (length name) 1)
                     (char= #\$ (char name 0))
                     (char/= #\0 (char name 1))
                     (every #'digit-char-p (subseq name 1)))
                (parse-integer name :start 1))
               ((string= symbol "$$")
                0)))))

#+SBCL
(defun all-symbols-in-form (form &aux res (orig *print-pprint-dispatch*))
  "Collect all symbols in the s-expression `form'. Glorious hack needed
because of SBCL. Thanks."
  (handler-bind
      ((error (lambda (cond)
                (let ((*print-pprint-dispatch* orig))
                  (error cond)))))
    (with-standard-io-syntax 
      (let ((default-table (copy-pprint-dispatch nil))
            (*print-pprint-dispatch* (copy-pprint-dispatch nil))
            (*print-pretty* t)
            (*print-circle* t))
        (set-pprint-dispatch 't (lambda (stream object)
                                  (if (symbolp object)
                                      (pushnew object res)
                                      (funcall (pprint-dispatch object default-table) stream object))))
        (pprint form (make-broadcast-stream))
        res))))

#-SBCL
(defun all-symbols-in-form (form &aux res)
  "Sane version for sane Lisps."
  (labels ((walk (x)
             (cond ((symbolp x) (pushnew x res))
                   ((atom x))
                   (t
                    (walk (car x))
                    (walk (cdr x))))))
    (walk form)
    res))


#+(or)
(progn
  (set-pprint-dispatch '(cons symbol (cons (member ->) t))
                       'print-rule)
  (defun print-rule (stream object)
    (pprint-logical-block (stream object :prefix "(" :suffix ")")
      (loop
        (pprint-exit-if-list-exhausted)
        (let ((x (pprint-pop)))
          (cond ((or (symbol= x '->)
                     (symbol= x '=>))
                 (pprint-newline :mandatory stream)
                 (prin1 x stream)
                 (write-char #\space stream))
                (t
                 (prin1 x stream)
                 (write-char #\space stream))))))))

) ;fin
