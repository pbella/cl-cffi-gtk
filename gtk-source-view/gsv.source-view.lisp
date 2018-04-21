;;; ----------------------------------------------------------------------------
;;; gsv.source-view.lisp
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
;;; GtkSourceView
;;;
;;; Widget that displays a GtkSourceBuffer
;;;
;;; Types and Values
;;;
;;;     GtkSourceView
;;;
;;; Functions
;;;
;;;     gtk_source_view_new
;;;     gtk_list_box_get_activate_on_single_click          -> Accessor
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkContainer
;;;                 ╰── GtkTextView
;;;                     ╰── GtkSourceView
;;;
;;; Implemented Interfaces
;;;
;;; GtkSourceView implements AtkImplementorIface, GtkBuildable and
;;; GtkScrollable.
;;; ----------------------------------------------------------------------------

(in-package :gsv)

;;; ----------------------------------------------------------------------------
;;; struct GtkSourceView
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkSourceView" gtk-source-view
  (:superclass gtk-text-view
   :export t
   :interfaces ("AtkImplementorIface"
                "GtkBuildable"
                "GtkScrollable")
   :type-initializer "gtk_source_view_get_type")
  ())

;;; ----------------------------------------------------------------------------
;;; gtk_source_view_new ()
;;; ----------------------------------------------------------------------------

#+gtk-3-10
(declaim (inline gtk-source-view-new))

#+gtk-3-10
(defun gtk-source-view-new ()
  (make-instance 'gtk-source-view))

#+gtk-3-10
(export 'gtk-source-view-new)

;;; --- End of file gsv.source-view.lisp ---------------------------------------
