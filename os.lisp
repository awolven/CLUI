(in-package :clui)

#+(or windows os-windows)
(defun get-error-message (error-code)
  (clet& ((buffer-pointer #_<unsigned short[65536]>))
    (#_FormatMessageW (logior #_FORMAT_MESSAGE_IGNORE_INSERTS
			      #_FORMAT_MESSAGE_FROM_SYSTEM)
		      nil
		      error-code
		      0
		      buffer-pointer
		      65536
		      nil)
    (lpcwstr->string buffer-pointer)))

(define-condition system-error (error)
  ((code :reader system-error-code :initarg :code :type (or null integer)
         :documentation "Numeric error code, or NIL.")
   (identifier :reader system-error-identifier :initarg :identifier
               :type symbol :documentation "Keyword identifier, or NIL.")
   (message :reader system-error-message :initarg :message
            :documentation "Error description."))
  (:default-initargs :code nil
                     :identifier :unknown-error)
  (:documentation "Base class for errors signalled by Osicat."))

(defun system-error (control-string &rest args)
  (error 'system-error :message (format nil "~?" control-string args)))

;;; Subtypes of WIN32-ERROR correspond to errors detected through the
;;; GetLastError mechanism.  These are defined below.
#+(or windows os-windows)
(define-condition win32-error (system-error)
  ((syscall :initform nil :initarg :syscall :reader win32-error-syscall))
  (:documentation
   "WIN32-ERRORs are signalled whenever GetLastError is set by a call or where
the call signals an error via the return value.")
  (:report (lambda (condition stream)
             (format stream "Error calling ~S~%~%~A"
                     (win32-error-syscall condition)
                     (get-error-message (system-error-code condition))))))

#+(or windows os-windows)
(defun win32-error (&optional (error-code (#_GetLastError)) syscall)
  (error (make-instance 'win32-error :code error-code :syscall syscall)))

#+(or windows os-windows)
(defun get-file-type (file)
  (let ((result (#_GetFileType file)))
    (when (eql result #_FILE_TYPE_UNKNOWN)
      (let ((error-code (#_GetLastError)))
        (unless (= error-code #_ERROR_SUCCESS)
          (win32-error error-code 'get-file-type))))
    result))

(defmacro with-create-file ((handle-name
                             file-name desired-access share-mode security-attributes
                             creation-disposition flags-and-attributes
                             &key (template-file nil))
                            &body body)
  `(call-with-create-file (lambda (,handle-name) ,@body)
                          ,file-name ,desired-access ,share-mode ,security-attributes
                          ,creation-disposition ,flags-and-attributes
                          :template-file ,template-file))

#+(or windows os-windows)
(defun create-file (file-name desired-access share-mode security-attributes
                    creation-disposition flags-and-attributes
                    &key (template-file nil))
  (#_CreateFileW file-name desired-access share-mode security-attributes creation-disposition
                 flags-and-attributes template-file))

#+(or windows os-windows)
(defun call-with-create-file (thunk file-name desired-access share-mode security-attributes
                              creation-disposition flags-and-attributes
                              &key (template-file nil))
  (let ((handle (create-file file-name desired-access share-mode security-attributes
                             creation-disposition flags-and-attributes
                             :template-file template-file)))
    (unwind-protect
         (funcall thunk handle)
      (#_CloseHandle handle))))

#+(or windows os-windows)
(defconstant +error-file-not-found+ 2)

#+(or windows os-windows)
(defconstant IO_REPARSE_TAG_SYMLINK #xA000000C)

#+(or windows os-windows)
(defun reparse-data-buffer-is-symbolic-link-p (buffer)
  (let ((reparse-tag (#_.ReparseTag buffer)))
    (= reparse-tag IO_REPARSE_TAG_SYMLINK)))

#+(or windows os-windows)
(defun handle-is-symbolic-link-p (handle)
  (clet& ((buffer #_<REPARSE_DATA_BUFFER>))
    (#_DeviceIoControl handle #_FSCTL_GET_REPARSE_POINT
                            nil 0
                            buffer (c-sizeof-type '#_<REPARSE_DATA_BUFFER>)
			    nil nil)
    (reparse-data-buffer-is-symbolic-link-p buffer)))

#+(or windows os-windows)
(defun escaped-namestring (pn)
  (let ((native-namestring (native-namestring pn)))
    (if (and (>= (length native-namestring) 4)
             (string= "\\\\?\\" (subseq native-namestring 0 4)))
        native-namestring
        (concatenate 'string "\\\\?\\" native-namestring))))

#+(or windows os-windows)
(defun %get-file-kind (namestring follow-p)
  (handler-bind
      ((win32-error (lambda (c)
                      (when (= (system-error-code c) +error-file-not-found+)
                        (unless follow-p
                          (return-from %get-file-kind nil))
                        (if (eql (%get-file-kind namestring nil) :symbolic-link)
                            (return-from %get-file-kind (values :symbolic-link :broken))
                            (return-from %get-file-kind nil))))))
    (with-create-file (handle
                       (escaped-namestring namestring)
                       0
		       (logior #_FILE_SHARE_READ #_FILE_SHARE_WRITE)
                       nil
		       #_OPEN_EXISTING
		       (if follow-p
			   (logior #_FILE_FLAG_OPEN_REPARSE_POINT
				   #_FILE_FLAG_BACKUP_SEMANTICS)
			   #_FILE_FLAG_BACKUP_SEMANTICS))
      (clet& ((buff #_<BY_HANDLE_FILE_INFORMATION>))
	(#_GetFileInformationByHandle handle buff)
	(let* ((attributes (#_.dwFileAttributes buff)))
          (cond
	    ;; This goes first because symlinks to directories have both flags
	    ;;
	    ;; TODO: Figure out if it's possible to have block devices or sockets
	    ;; on windows.
            ((and (logtest #_FILE_ATTRIBUTE_REPARSE_POINT attributes)
                  (handle-is-symbolic-link-p handle))
             :symbolic-link)
            ((logtest #_FILE_ATTRIBUTE_DIRECTORY attributes)
             :directory)
            (t
             (ecase (get-file-type handle)
               (#.#_FILE_TYPE_DISK
		:regular-file)
               (#.#_FILE_TYPE_PIPE
		:pipe)
               (#.#_FILE_TYPE_CHAR
		:character-device)
	       (#.#_FILE_TYPE_REMOTE
		:remote)
	       (#.#_FILE_TYPE_UNKNOWN
		:unknown)))))))))

#-(or windows os-windows)
(defun %get-file-kind (namestring follow-p)
  (handler-case
      (let ((mode (nix:stat-mode
                   (if follow-p
                       (nix:stat namestring)
                       #-windows
                       (nix:lstat namestring)
                       #+windows
                       (nix:stat namestring)))))
        (case (logand nix:s-ifmt mode)
          (#.nix:s-ifdir  :directory)
          (#.nix:s-ifchr  :character-device)
          (#.nix:s-ifblk  :block-device)
          (#.nix:s-ifreg  :regular-file)
          #-windows ; KLUDGE
          (#.nix:s-iflnk  :symbolic-link)
          #-windows ; KLUDGE
          (#.nix:s-ifsock :socket)
          (#.nix:s-ififo  :pipe)
          (otherwise
           (bug "Unknown file mode: ~A." mode))))
    (nix:enoent ()
      (cond
        ;; stat() returned ENOENT: either FILE does not exist
        ;; or the end of the symlink chain is a broken symlink
        #-windows
        (follow-p
         (handler-case
             (nix:lstat namestring)
           (nix:enoent ())
           (:no-error (stat)
             (declare (ignore stat))
             (values :symbolic-link :broken))))
        ;; lstat() returned ENOENT: FILE does not exist
        (t nil)))))

(defun native-namestring (pathname)
  (cffi-sys:native-namestring pathname))

(defun get-file-kind (file follow-p)
  (%get-file-kind (native-namestring file) follow-p))

(defun file-kind (pathspec &key follow-symlinks)
  "Returns a keyword indicating the kind of file designated by PATHSPEC,
or NIL if the file does not exist.  Does not follow symbolic
links by default.

Possible file-kinds in addition to NIL are: :REGULAR-FILE,
:SYMBOLIC-LINK, :DIRECTORY, :PIPE, :SOCKET, :CHARACTER-DEVICE, and
:BLOCK-DEVICE.
If FOLLOW-SYMLINKS is non-NIL and PATHSPEC designates a broken symlink
returns :BROKEN as second value.

Signals an error if PATHSPEC is wild."
  (get-file-kind (merge-pathnames pathspec) follow-symlinks))
