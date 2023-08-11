(in-package :noffi-user)

;; Install noffi syntax. The `T' here says, that we want to clobber #\_
;; as a non-terminating character instead of the more polite dispatch
;; macro character for "#_".

(noffi-syntax t)

;; PKG-USE invokes pkg-config for us to figure out where to find
;; includes and libs. Bind *load-verbose* to T, so that we see the
;; libraries that got loaded.

(let ((*load-verbose* t))
  (pkg-use ("gtk+-2.0")
           "#include <gtk/gtk.h>"))

;; Define the callback function that we want to install on a button;
;; BUTTON-CLICKED-CALLBACK becomes something that can be passed around
;; as a Pascal^WC function pointer.

(defcfun (button-clicked-callback _<void>) ((widget _<GtkWidget*>) (data _<gpointer>))
  (declare (ignore data))
  (warn "~S was hit. Quch!" widget))

(defun cl-user::run ()
  ;; We need to call gtk_init with some fake argc/argv
  (clet ((argc _<int> 1)
         (argv _<char**> (list "noffi-demo")))
    (_gtk_init (c-addr-of argc) (c-addr-of argv)))
  ;; Make a window and a butten
  (let ((window (_gtk_window_new _GTK_WINDOW_TOPLEVEL))
        (button (_gtk_button_new_with_label "Hit me")))
    ;; Connect the "clicked" signal. Note that gtk_signal_connect really is
    ;; a C macro inside. As are those GTK_FOO() runtime checks.
    (_gtk_signal_connect (_GTK_OBJECT button)
                         "clicked"
                         (_GTK_SIGNAL_FUNC button-clicked-callback)
                         _NULL)
    (_gtk_container_add (#_GTK_CONTAINER window) button)
    (_gtk_widget_show button)
    (_gtk_widget_show window)
    ;; Let the main loop run in a different thread
    #+CCL       (ccl:process-run-function "gtk event loop" (lambda () (_gtk_main)))
    #+SBCL      (sb-thread:make-thread (lambda () (_gtk_main))
                                       :name "gtk event loop")
    #-(OR CCL SBCL) (bt:make-thread (lambda () (_gtk_main))
                                    :name "gtk event loop") ))


    
