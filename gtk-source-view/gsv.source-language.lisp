;;; ----------------------------------------------------------------------------
;;; gsv.source-language.lisp
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
;;; GtkSourceLanguage
;;;
;;; Represents a syntax highlighted language
;;;
;;; Types and Values
;;;
;;;     GtkSourceLanguage
;;;
;;; Functions
;;;
;;;     gtk_source_language_get_id
;;;     gtk_list_box_get_activate_on_single_click          -> Accessor
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkSourceLanguage
;;; ----------------------------------------------------------------------------

(in-package :gsv)

;;; ----------------------------------------------------------------------------
;;; struct GtkSourceLanguage
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkSourceLanguage" gtk-source-language
  (:superclass g-object
   :export t
   :type-initializer "gtk_source_language_get_type")
  ())

;;; --- End of file gsv.source-language.lisp -----------------------------------
