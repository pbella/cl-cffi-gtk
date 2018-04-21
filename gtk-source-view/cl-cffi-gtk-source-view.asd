;;; ----------------------------------------------------------------------------
;;; cl-cffi-gtk-source-view.asd
;;;
;;; Copyright (C) 2018 - Olof-Joachim Frahm
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

(defsystem :cl-cffi-gtk-source-view
  :name :cl-cffi-gtk
  :version "3.24.7"                    ; Version of the library
  :author "Olof-Joachim Frahm"
  :license "LLGPL"
  :description "A Lisp binding to GtkSourceView"
  :serial t
  :components
  ((:file "gsv.package")
   (:file "gsv.init")

   (:file "gsv.source-buffer")
   (:file "gsv.source-language")
   (:file "gsv.source-language-manager")
   (:file "gsv.source-view"))
  :depends-on (:cl-cffi-gtk))

;;; --- End of file cl-cffi-gtk-source-view.asd --------------------------------
