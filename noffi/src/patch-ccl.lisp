(in-package :ccl)

'#.(setq *readtable* CCL::%INITIAL-READTABLE%)

(let ((ccl:*warn-if-redefine-kernel* nil))

  #+WINDOWS
  ;; Make this return _true_ on EOF as well.
  (defun data-available-on-pipe-p (hpipe)
    (rlet ((navail #>DWORD 0))
      (multiple-value-bind (ret err)
          (without-interrupts
            (let ((ret (#_PeekNamedPipe (if (typep hpipe 'macptr)
                                            hpipe
                                            (%int-to-ptr hpipe))
                                        (%null-ptr)
                                        0
                                        (%null-ptr)
                                        navail
                                        (%null-ptr))))
              (values ret (and (eql 0 ret) (#_GetLastError)))))
        (cond ((not (eql 0 ret))
               (values (not (eql 0 (pref navail #>DWORD))) (pref navail #>DWORD)))
              ((eql #$ERROR_BROKEN_PIPE err)
               (values t 0))
              (t
               (error "Windows error: (#_GetLastError) said ~D" err))))))

  #+X8664-TARGET
  (defun x862-box-s64 (seg node-dest s64-src)
    (with-x86-local-vinsn-macros (seg)
      (if (target-arch-case
           (:x8632 (error "bug"))
           (:x8664 (or *backend-use-linear-scan* *x862-open-code-inline*)))
          (let* ((no-overflow (backend-get-next-label)))
            (! %set-z-flag-if-s64-fits-in-fixnum node-dest s64-src)
            (! cbranch-true (aref *backend-labels* no-overflow) x86::x86-e-bits)
            (! setup-bignum-alloc-for-s64-overflow s64-src)
            (! %allocate-uvector node-dest)
            (! set-bigits-after-fixnum-overflow node-dest)
            (@ no-overflow))
          (let* ((arg_z ($ *x862-arg-z*))
                 (imm0 (make-wired-lreg *x862-imm0* :mode (get-regspec-mode s64-src))))
            (x862-copy-register seg imm0 s64-src)
            (! call-subprim (subprim-name->offset '.SPmakes64))
            (x862-copy-register seg node-dest arg_z))))) 

  #+windows-target
  (defun monitor-external-process (p)
    (let* ((in-fds (external-process-watched-fds p))
           (out-streams (external-process-watched-streams p))
           (token (external-process-token p))
           (terminated)
           (changed)
           (external-format (external-process-external-format p))
           (encoding (external-format-character-encoding external-format))
           (line-termination (external-format-line-termination external-format))
           (pairs (pairlis (mapcar (lambda (fd)
                                     (cons fd
                                           (make-fd-stream fd
                                                           :direction :input
                                                           :sharing :private
                                                           :encoding encoding
                                                           :interactive t
                                                           :line-termination line-termination)))
                                   in-fds)
                           out-streams))
           )
      (loop
        (when changed
          (setq pairs (delete nil pairs :key #'car)
                changed nil))
        (when (and terminated (null pairs))
          (without-interrupts
            (rlet ((code #>DWORD))
              (loop
                (#_GetExitCodeProcess (external-process-pid p) code)
                (unless (eql (pref code #>DWORD) #$STILL_ACTIVE)
                  (return))
                (#_SleepEx 10 #$TRUE))
              (setf (external-process-%exit-code p) (pref code #>DWORD)))
            (#_CloseHandle (external-process-pid p))
            (setf (external-process-pid p) nil)
            (setf (external-process-%status p) :exited)
            (let ((status-hook (external-process-status-hook p)))
              (when status-hook
                (funcall status-hook p)))
            (remove-external-process p)
            (signal-semaphore (external-process-completed p))
            (return)))
        #+NIL
        (dolist (p pairs)
          (let* ((in-fd (caar p))
                 (in-stream (cdar p))
                 (out-stream (cdr p)))
            (when (or terminated (data-available-on-pipe-p in-fd))
              (let* ((buf (make-string 1024)))
                (declare (dynamic-extent buf))
                (let* ((n (ignore-errors (read-sequence buf in-stream))))
                  (if (or (null n) (eql n 0))
                      (progn
                        (without-interrupts
                          (decf (car token))
                          (fd-close in-fd)
                          (setf (car p) nil changed t)))
                      (progn
                        (write-sequence buf out-stream :end n)
                        (force-output out-stream))))))))
        (dolist (p pairs)
          (let* ((in-fd (caar p))
                 (in-stream (cdar p))
                 (out-stream (cdr p)))
            (do ((c (read-char-no-hang in-stream nil ':eof)
                    (read-char-no-hang in-stream nil ':eof)))
                ((not (characterp c))
                 (if (eq ':eof c)
                     (without-interrupts
                       (decf (car token))
                       (fd-close in-fd)
                       (setf (car p) nil changed t))
                     (force-output out-stream)))
              (write-char c out-stream))))
        (unless terminated
          (setq terminated (eql (#_WaitForSingleObjectEx
                                 (external-process-pid p)
                                 1     ;<---- was: 1000. This is much better
                                 #$true)
                                #$WAIT_OBJECT_0))))))

  )
