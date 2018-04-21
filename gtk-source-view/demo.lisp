(in-package :gsv)

(defun gtk-source-view-demo ()
  (within-main-loop ()
    (let* ((window (make-instance 'gtk-window
                                  :type :toplevel
                                  :title "Example Source View"
                                  :default-width 800
                                  :default-height 600
                                  :border-width 6))
           (scrolled-window (make-instance 'gtk-scrolled-window))
           (source-view (make-instance 'gtk-source-view))
           (buffer (gtk-text-view-buffer source-view))
           (lm (gtk-source-language-manager-get-default)))
      (setf (gtk-text-buffer-text buffer)
            (format NIL "GtkSourceView Demo~%language-ids = ~A~%search-path = ~A~%"
                    (gtk-source-language-manager-language-ids lm)
                    (gtk-source-language-manager-search-path lm))
            (gtk-source-buffer-language buffer)
            (gtk-source-language-manager-get-language lm "scheme"))
      (gtk-container-add scrolled-window source-view)
      (gtk-container-add window scrolled-window)
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      (gtk-widget-show-all window)))
  (join-gtk-main))
