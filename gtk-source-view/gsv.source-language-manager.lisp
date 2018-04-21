;;; ----------------------------------------------------------------------------
;;; gsv.source-language-manager.lisp
;;;
;;; The documentation has been copied from the GTK+ 3 Reference Manual
;;; Version 3.6.4. See <http://www.gtk.org>. The API documentation of the
;;; Lisp Binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2018 Olof-Joachim Frahm
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License for Lisp
;;; as published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version and with a preamble to
;;; the GNU Lesser General Public License that clarifies the terms for use
;;; with Lisp programs and is referred as the LLGPL.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program and the preamble to the Gnu Lesser
;;; General Public License.  If not, see <http://www.gnu.org/licenses/>
;;; and <http://opensource.franz.com/preamble.html>.
;;; ----------------------------------------------------------------------------
;;;
;;; GtkSourceLanguageManager
;;;
;;; Provides access to GtkSourceLanguages
;;;
;;; Types and Values
;;;
;;;     GtkSourceLanguageManager
;;;
;;; Functions
;;;
;;;     gtk_source_language_manager_new
;;;     gtk_source_language_manager_get_default
;;;     gtk_list_box_get_activate_on_single_click          -> Accessor
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkSourceLanguageManager
;;; ----------------------------------------------------------------------------

(in-package :gsv)

;;; ----------------------------------------------------------------------------
;;; struct GtkSourceLanguageManager
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkSourceLanguageManager" gtk-source-language-manager
  (:superclass g-object
   :export t
   :type-initializer "gtk_source_language_manager_get_type")
  ((language-ids
    gtk-source-language-manager-language-ids
    "language-ids" "GStrv" t nil)
   (search-path
    gtk-source-language-manager-search-path
    "search-path" "GStrv" t t)))

;;; ----------------------------------------------------------------------------
;;; gtk_source_language_manager_new ()
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_source_language_manager_get_default ()
;;; ----------------------------------------------------------------------------

(defcfun gtk-source-language-manager-get-default
    (g-object gtk-source-language-manager))

(export 'gtk-source-language-manager-get-default)

;;; ----------------------------------------------------------------------------
;;; gtk_source_language_manager_get_language ()
;;; ----------------------------------------------------------------------------

(defcfun gtk-source-language-manager-get-language
    (g-object gtk-source-language :free-from-foreign nil)
  (lm (g-object gtk-source-language-manager))
  (id :string))

(export 'gtk-source-language-manager-get-language)

;;; --- End of file gsv.source-language-manager.lisp ---------------------------
