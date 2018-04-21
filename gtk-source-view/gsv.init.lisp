(in-package :gsv)

(glib::at-init ()
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (define-foreign-library gsv
      (t "libgtksourceview-3.0.so")))
  (unless (foreign-library-loaded-p 'gsv)
    (use-foreign-library gsv)))
