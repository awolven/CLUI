(in-package :ccl)

(eval-when (:compile-toplevel)
  (setq *readtable* CCL::%INITIAL-READTABLE%))

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
            (x862-copy-register seg node-dest arg_z))))) )
