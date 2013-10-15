(def-suite gtk-dialog :in gtk-suite)
(in-suite gtk-dialog)

;;;   GtkDialog

(test gtk-dialog-class
  ;; Type checks
  (is-true  (g-type-is-object "GtkDialog"))
  (is-false (g-type-is-abstract "GtkDialog"))
  (is-true  (g-type-is-derived "GtkDialog"))
  (is-false (g-type-is-fundamental "GtkDialog"))
  (is-true  (g-type-is-value-type "GtkDialog"))
  (is-true  (g-type-has-value-table "GtkDialog"))
  (is-true  (g-type-is-classed "GtkDialog"))
  (is-true  (g-type-is-instantiatable "GtkDialog"))
  (is-true  (g-type-is-derivable "GtkDialog"))
  (is-true  (g-type-is-deep-derivable "GtkDialog"))
  (is-false (g-type-is-interface "GtkDialog"))

  ;; Check the registered name
  (is (eq 'gtk-dialog
          (registered-object-type-by-name "GtkDialog")))

  ;; Check infos about the C class implementation
  (let ((class (g-type-class-ref (gtype "GtkDialog"))))
    (is (equal (gtype "GtkDialog") (g-type-from-class class)))
    (is (equal (gtype "GtkDialog") (g-object-class-type class)))
    (is (equal "GtkDialog" (g-object-class-name class)))
    (is (equal (gtype "GtkDialog") (g-type-from-class  (g-type-class-peek "GtkDialog"))))
    (is (equal (gtype "GtkDialog") (g-type-from-class  (g-type-class-peek-static "GtkDialog"))))
    (g-type-class-unref class))

  ;; Check infos about the Lisp class implementation
  (let ((class (find-class 'gtk-dialog)))
    ;; Check the class name and type of the class
    (is (eq 'gtk-dialog (class-name class)))
    (is (eq 'gobject-class (type-of class)))
    (is (eq (find-class 'gobject-class) (class-of class)))
    ;; Properties of the metaclass gobject-class
    (is (equal "GtkDialog" (gobject-class-g-type-name class)))
    (is (equal "GtkDialog" (gobject-class-direct-g-type-name class)))
    (is (equal "gtk_dialog_get_type"
               (gobject-class-g-type-initializer class)))
    (is-false (gobject-class-interface-p class)))

  ;; Check some more GType information
  (is (equal (gtype "GtkWindow") (g-type-parent "GtkDialog")))
  (is (= 7 (g-type-depth "GtkDialog")))
  (is (equal (gtype "GtkContainer")
             (g-type-next-base "GtkDialog" "GtkWidget")))
  (is-true  (g-type-is-a "GtkDialog" "GtkWidget"))
  (is-true  (g-type-is-a "GtkDialog" "GtkContainer"))
  (is-false (g-type-is-a "GtkDialog" "gboolean"))
  (is-true  (g-type-is-a "GtkDialog" "GtkWindow"))

  ;; Check the children
  (is (equal '("GtkMessageDialog" "GtkAboutDialog" "GtkColorChooserDialog"
               "GtkColorSelectionDialog" "GtkFileChooserDialog" "GtkFontChooserDialog"
               "GtkFontSelectionDialog" "GtkRecentChooserDialog" "GtkPrintUnixDialog"
               "GtkPageSetupUnixDialog")
             (mapcar #'gtype-name (g-type-children "GtkDialog"))))
  ;; Check the interfaces
  (is (equal '("AtkImplementorIface" "GtkBuildable")
             (mapcar #'gtype-name (g-type-interfaces "GtkDialog"))))

  ;; Query infos about the class
  (with-foreign-object (query '(:struct g-type-query))
    (g-type-query "GtkDialog" query)
    (is (equal (gtype "GtkDialog")
               (foreign-slot-value query '(:struct g-type-query) :type)))
    (is (equal "GtkDialog"
               (foreign-slot-value query '(:struct g-type-query) :type-name)))
    (is (= 560 (foreign-slot-value query '(:struct g-type-query) :class-size)))
    (is (=  32 (foreign-slot-value query '(:struct g-type-query) :instance-size))))

  ;; Get the names of the class properties.
  (is (equal '("name" "parent" "width-request" "height-request" "visible"
               "sensitive" "app-paintable" "can-focus" "has-focus" "is-focus"
               "can-default" "has-default" "receives-default" "composite-child"
               "style" "events" "no-show-all" "has-tooltip" "tooltip-markup"
               "tooltip-text" "window" "double-buffered" "halign" "valign"
               "margin-left" "margin-right" "margin-top" "margin-bottom"
               "margin" "hexpand" "vexpand" "hexpand-set" "vexpand-set" "expand"
               "border-width" "resize-mode" "child" "type" "title" "role" "resizable" "modal"
               "window-position" "default-width" "default-height" "destroy-with-parent"
               "hide-titlebar-when-maximized" "icon" "icon-name" "screen" "type-hint"
               "skip-taskbar-hint" "skip-pager-hint" "urgency-hint" "accept-focus"
               "focus-on-map" "decorated" "deletable" "gravity" "transient-for" "attached-to"
               "opacity" "has-resize-grip" "resize-grip-visible" "application"
               "ubuntu-no-proxy" "is-active" "has-toplevel-focus" "startup-id"
               "mnemonics-visible" "focus-visible")
             (mapcar #'param-spec-name (g-object-class-list-properties "GtkDialog"))))

  ;; Get the names of the style properties.
  (is (equal '("cursor-aspect-ratio" "cursor-color" "focus-line-pattern"
               "focus-line-width" "focus-padding" "interior-focus" "link-color"
               "scroll-arrow-hlength" "scroll-arrow-vlength"
               "secondary-cursor-color" "separator-height" "separator-width"
               "text-handle-height" "text-handle-width" "visited-link-color"
               "wide-separators" "window-dragging" "resize-grip-height"
               "resize-grip-width" "action-area-border" "button-spacing"
               "content-area-border" "content-area-spacing")
             (mapcar #'param-spec-name
                     (gtk-widget-class-list-style-properties "GtkDialog"))))

  ;; Get the names to the child properties
  (is (equal '()
             (mapcar #'param-spec-name (gtk-container-class-list-child-properties "GtkDialog"))))

  ;; Get the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkDialog" GTK-DIALOG
                       (:SUPERCLASS GTK-WINDOW :EXPORT T :INTERFACES
                        ("AtkImplementorIface" "GtkBuildable")
                        :TYPE-INITIALIZER "gtk_dialog_get_type")
                       NIL)
             (get-g-type-definition "GtkDialog"))))

(test gtk-dialog-style-properties
  (let ((widget (make-instance 'gtk-dialog)))
    (is (= 0.04 (gtk-widget-style-get-property widget "cursor-aspect-ratio")))
    (is-false (gtk-widget-style-get-property widget "cursor-color"))
    (is (equal "" (gtk-widget-style-get-property widget "focus-line-pattern")))
    (is (= 1 (gtk-widget-style-get-property widget "focus-line-width")))
    (is (= 0 (gtk-widget-style-get-property widget "focus-padding")))
    (is-true (gtk-widget-style-get-property widget "interior-focus"))
    (is (eq 'gdk-color (type-of (gtk-widget-style-get-property widget "link-color"))))
    (is (= 16 (gtk-widget-style-get-property widget "scroll-arrow-hlength")))
    (is (= 16 (gtk-widget-style-get-property widget "scroll-arrow-vlength")))
    (is-false (gtk-widget-style-get-property widget "secondary-cursor-color"))
    (is (=  2 (gtk-widget-style-get-property widget "separator-height")))
    (is (=  2 (gtk-widget-style-get-property widget "separator-width")))
    (is (= 20 (gtk-widget-style-get-property widget "text-handle-height")))
    (is (= 16 (gtk-widget-style-get-property widget "text-handle-width")))
    (is (eq 'gdk-color (type-of (gtk-widget-style-get-property widget "visited-link-color"))))
    (is-false  (gtk-widget-style-get-property widget "wide-separators"))
    (is-false (gtk-widget-style-get-property widget "window-dragging"))
    (is (= 0 (gtk-widget-style-get-property widget "resize-grip-height")))
    (is (= 0 (gtk-widget-style-get-property widget "resize-grip-width")))
    (is (= 5 (gtk-widget-style-get-property widget "action-area-border")))
    (is (= 6 (gtk-widget-style-get-property widget "button-spacing")))
    (is (= 2 (gtk-widget-style-get-property widget "content-area-border")))
    (is (= 0 (gtk-widget-style-get-property widget "content-area-spacing")))))

;;;     GtkDialogFlags
;;;     GtkResponseType
;;;
;;;     gtk_dialog_new
;;;     gtk_dialog_new_with_buttons
;;;     gtk_dialog_run
;;;     gtk_dialog_response
;;;     gtk_dialog_add_button
;;;     gtk_dialog_add_buttons
;;;     gtk_dialog_add_action_widget
;;;     gtk_dialog_set_default_response
;;;     gtk_dialog_set_response_sensitive
;;;     gtk_dialog_get_response_for_widget
;;;     gtk_dialog_get_widget_for_response
;;;     gtk_dialog_get_action_area
;;;     gtk_dialog_get_content_area
;;;     gtk_alternative_dialog_button_order
;;;     gtk_dialog_set_alternative_button_order
;;;     gtk_dialog_set_alternative_button_order_from_array
