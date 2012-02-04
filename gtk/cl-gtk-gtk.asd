;;; ----------------------------------------------------------------------------
;;; cl-gtk-gtk.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See http://common-lisp.net/project/cl-gtk2/
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2012 Dr. Dieter Kaiser
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

(defpackage #:cl-gtk-gtk-system
  (:use #:cl #:asdf))

(in-package #:cl-gtk-gtk-system)

(defclass plain-file (static-file)
  ((type :initarg :type :reader plain-file-type :initform nil)))

(defmethod source-file-type ((c plain-file) (s module))
  (plain-file-type c))

(defsystem :cl-gtk-gtk
  :name :cl-gtk-gtk
  :version "0.0.0"
  :author "Dr. Dieter Kaiser"
  :license "LLGPL"
  :serial t
  :components ((:file "gtk.package")
               (:file "gtk.misc-lisp")
               (:file "gtk.child-properties")
               
               ;; Gtk+ Core
               (:file "gtk.version")          ; Version Information
               (:file "gtk.enumerations")     ; Standard Enumerations
               (:file "gtk.main-loop")        ; Main event loop, and events
               (:file "gtk.accel-group")      ; Accelerator Groups
               (:file "gtk.accel-map")        ; Loadable keyboard accelerator
               (:file "gtk.clipboard")        ; Storing data on clipboards
               (:file "gtk.drag-and-drop")    ; Controlling drag and drop
               (:file "gtk.settings")         ; Sharing settings
               (:file "gtk.selections")       ; Inter-process communication
               
               ;; Interface builder
               (:file "gtk.buildable")        ; GtkBuildable
               (:file "gtk.builder")          ; Build an interface
               
               ;; More Inferfaces
               (:file "atk.implementor-iface") ; AtkImplementorIface
               (:file "gtk.orientable")      ; Interface for flippable widgets
               (:file "gtk.activatable")     ; Interface for activatable widgets
               
               ;; Theming in Gtk+
               (:file "gtk.style-context")    ; Rendering UI elements
               (:file "gtk.stock-images")     ; Manipulating stock icons
               
               ;; Abstract Base Classes
               (:file "gtk.object")           ; GtkObject
               (:file "gtk.widget")           ; Base class for all widgets
               (:file "gtk.misc")             ; Base class for alignments
               (:file "gtk.container")        ; GtkContainer
               (:file "gtk.bin")              ; Container with just one child
               (:file "gtk.range")            ; Base class for adjustments
               (:file "gtk.menu-shell")       ; Base class for menu objects
               (:file "gtk.im-context")       ; Base class for input contexts
               
               ;; Layout Containers
               (:file "gtk.box")              ; GtkBox, GtkHBox, GtkVBox
               (:file "gtk.table")            ; GtkTable
               (:file "gtk.layout")           ; Infinite scrollable
               (:file "gtk.fixed")            ; Widgets at fixed coordinates
               (:file "gtk.notebook")         ; Tabbed notebook container
               (:file "gtk.paned")            ; Two adjustable panes
               (:file "gtk.expander")       ; Container which can hide its child
               
               ;; Ornaments
               (:file "gtk.separator")        ; Separator widget
               (:file "gtk.frame")            ; Decorative frame
               
               ;; Scrolling
               (:file "gtk.scrollbar")        ; GtkScrollbar
               (:file "gtk.scrolled-window")  ; Adds scrollbars
               
               ;; Windows
               (:file "gtk.dialog")           ; GtkDialog
               (:file "gtk.invisible")        ; GtkInvisible
               (:file "gtk.message-dialog")   ; GtkMessageDialog
               (:file "gtk.window")           ; GtkWindow
               (:file "gtk.window-group")     ; GtkWindowGroup
               (:file "gtk.about-dialog")     ; GtkAboutDialog
               (:file "gtk.assistant")        ; GtkAssistant
               ;; GtkOffscreenWindow not implemented
               
               ;; Display Widgets
               (:file "gtk.accel-label")      ; GtkAccelLabel
               (:file "gtk.image")            ; GtkImage
               (:file "gtk.label")            ; GtkLabel
               (:file "gtk.progress-bar")     ; GtkProgessBar
               (:file "gtk.statusbar")        ; GTKStatusbar
               ;; GtkInfoBar not implemented
               (:file "gtk.status-icon")      ; GtkStatusIcon
               ;; GtkSpinner not implemented
               
               ;; Buttons and Toggles
               (:file "gtk.button")           ; GtkButton
               (:file "gtk.toggle-button")    ; GtkToggleButton
               (:file "gtk.check-button")     ; GtkCheckButton
               (:file "gtk.radio-button")     ; GtkRadioButton
               (:file "gtk.link-button")      ; GtkLinkButton
               (:file "gtk.scale-button")     ; GtkScaleButton
               (:file "gtk.volume-button")    ; GtkVolumeButton
               ;; GtkSwitch not implemented
               ;; GtkLockButton not implemented
               
               ;; Multiline Text Editor
               (:file "gtk.text-iter")        ; GtkTextIter
               (:file "gtk.text-mark")        ; GtkTextMark
               (:file "gtk.text-tag")         ; GtkTextTag
               (:file "gtk.text-buffer")      ; GtkTextBuffer
               (:file "gtk.text-tag-table")   ; GtkTextTagTable
               (:file "gtk.text-view")        ; GtkTextView
               
               ;; Tree, List and Icon Grid Widgets
               (:file "gtk.tree-model")       ; Tree interface
               (:file "gtk.cell-layout")      ; Interface for packing cells
               (:file "gtk.tree-sortable")    
               (:file "gtk.tree-view-drag-and-drop")
               (:file "gtk.tree-model-sort")  ; GtkTreeModelSort
               (:file "gtk.tree-model-filter") ; GtkTreeModelFilter
               (:file "gtk.tree-view")        ; Displaying both trees and lists
               (:file "gtk.tree-view-column") ; Visible column in GtkTreeView
               (:file "gtk.tree-store")       ; A tree-like data structure
               (:file "gtk.tree-selection")   ; Selection object for GtkTreeView
               (:file "gtk.cell-editable")    ; GtkCellEditable
               (:file "gtk.cell-renderer")    ; Object for rendering a cell
               (:file "gtk.cell-renderer-text") ; Renders text in a cell
               (:file "gtk.cell-view")        ; Displaying a single row
               (:file "gtk.icon-view")        ; List of icons in a grid
               (:file "gtk.list-store")       ; list-like data structure
               
               ;; Numeric/Text Data Entry
               (:file "gtk.editable")         ; GtkEditable
               (:file "gtk.entry")            ; GtkEntry
               ;; GtkEntryBuffer not implemented
               (:file "gtk.entry-completion") ; GtkEntryCompletion
               (:file "gtk.scale")            ; GtkScale
               (:file "gtk.h-scale")          ; GtkHScale
               (:file "gtk.v-scale")          ; GtkVScale
               (:file "gtk.spin-button")      ; GtkSpinButton
               
               ;; Menus, Combo Box, Toolbar
               (:file "gtk.menu")             ; A menu widget
               (:file "gtk.combo-box")        ; GtkComboBox
               (:file "gtk.tool-shell")       ; Interface for GtkToolItem
               (:file "gtk.tool-item")        ; GtkToolItem
               (:file "gtk.toolbar")          ; Create bars of buttons
               (:file "gtk.tool-button")      ; GtkToolButton
               (:file "gtk.toggle-tool-button") ; GtkToggleToolButton
               (:file "gtk.radio-tool-button") ; GtkRadioToolButton
               
               ;; Action-based menus and toolbars
               (:file "gtk.ui-manager")       ; Constructing menus and toolbars
               (:file "gtk.action-group")     ; Group of actions
               (:file "gtk.action")           ; GtkAction
               (:file "gtk.toggle-action")    ; GtkToggleAction
               (:file "gtk.radio-action")     ; GtkRadioAction
               
               ;; Selectors
               (:file "gtk.color-button")     ; Launch a color selection dialog
               (:file "gtk.color-selection")  ; A widget used to select a color
               (:file "gtk.hsv")              ; GtkHSV
               
               ;; Miscellaneous
               (:file "gtk.adjustment")      ; Representation of a bounded value
               (:file "gtk.drawing-area")    ; Custom user interface elements
               (:file "gtk.event-box")       ; A widget used to catch events
               (:file "gtk.calendar")        ; Displays a calendar
               (:file "gtk.size-group")      ; GtkSizeGroup
               (:file "gtk.tooltip")         ; Add tips to your widgets
               (:file "gtk.arrow")           ; Displays an arrow
               
               ;; More definitions. The documentatin is not completed.
               (:file "gtk.generated-classes")
               (:file "gtk.generated-child-properties")
               (:file "gtk.selectors")
               (:file "gtk.timer")
               
               ;; Lisp
               (:file "gtk.high-level")
               (:file "gtk.init")
               )
  :depends-on (:cl-gtk-glib
               :cl-gtk-gobject
               :cl-gtk-gdk
               :cl-gtk-pango
               :cffi
               :bordeaux-threads
               :iterate))

;;; --- End of file cl-gtk2-gtk.asd --------------------------------------------