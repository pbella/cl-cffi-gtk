;;; ----------------------------------------------------------------------------
;;; gdk-pixbuf.utilities.lisp
;;;
;;; This file contains code from a fork of cl-gtk2.
;;; See <http://common-lisp.net/project/cl-gtk2/>.
;;;
;;; The documentation has been copied from the GDK-PixBuf Reference Manual
;;; Version 2.28.0. See <http://www.gtk.org>. The API documentation of the
;;; Lisp binding is available at <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2013 Dieter Kaiser
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
;;; Utilities
;;;
;;; Utility and miscellaneous convenience functions.
;;;
;;; Synopsis
;;;
;;;     gdk_pixbuf_add_alpha
;;;     gdk_pixbuf_copy_area
;;;     gdk_pixbuf_saturate_and_pixelate
;;;     gdk_pixbuf_apply_embedded_orientation
;;;     gdk_pixbuf_fill
;;;
;;; Description
;;;
;;; These functions provide miscellaneous utilities for manipulating pixbufs.
;;; The pixel data in pixbufs may of course be manipulated directly by
;;; applications, but several common operations can be performed by these
;;; functions instead.
;;; ----------------------------------------------------------------------------

(in-package :gdk-pixbuf)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_add_alpha ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_pixbuf_add_alpha" gdk-pixbuf-add-alpha) (g-object gdk-pixbuf)
 #+cl-cffi-gtk-documentation
 "@version{2013-4-14}
  @argument[pixbuf]{a @class{gdk-pixbuf} object}
  @argument[substitute-color]{whether to set a color to zero opacity. If this is
    @code{nil}, then the (@arg{red}, @arg{green}, @arg{blue}) arguments will be
    ignored}
  @argument[red]{red value to substitute}
  @argument[green]{green value to substitute}
  @argument[blue]{blue value to substitute}
  @return{A newly created pixbuf with a reference count of 1.}
  @begin{short}
    Takes an existing @arg{pixbuf} and adds an alpha channel to it. If the
    existing @arg{pixbuf} already had an alpha channel, the channel values are
    copied from the original; otherwise, the alpha channel is initialized to
    255 (full opacity).
  @end{short}

  If @arg{substitute-color} is @em{true}, then the color specified by
  (@arg{red}, @arg{green}, @arg{blue}) will be assigned zero opacity. That is,
  if you pass (255, 255, 255) for the substitute color, all white pixels will
  become fully transparent."
  (pixbuf (g-object gdk-pixbuf))
  (substitute-color :boolean)
  (red :uchar)
  (green :uchar)
  (blue :uchar))

(export 'gdk-pixbuf-add-alpha)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_copy_area ()
;;;
;;; void gdk_pixbuf_copy_area (const GdkPixbuf *src_pixbuf,
;;;                            int src_x,
;;;                            int src_y,
;;;                            int width,
;;;                            int height,
;;;                            GdkPixbuf *dest_pixbuf,
;;;                            int dest_x,
;;;                            int dest_y);
;;;
;;; Copies a rectangular area from src_pixbuf to dest_pixbuf. Conversion of
;;; pixbuf formats is done automatically.
;;;
;;; If the source rectangle overlaps the destination rectangle on the same
;;; pixbuf, it will be overwritten during the copy operation. Therefore, you can
;;; not use this function to scroll a pixbuf.
;;;
;;; src_pixbuf :
;;;     Source pixbuf.
;;;
;;; src_x :
;;;     Source X coordinate within src_pixbuf.
;;;
;;; src_y :
;;;     Source Y coordinate within src_pixbuf.
;;;
;;; width :
;;;     Width of the area to copy.
;;;
;;; height :
;;;     Height of the area to copy.
;;;
;;; dest_pixbuf :
;;;     Destination pixbuf.
;;;
;;; dest_x :
;;;     X coordinate within dest_pixbuf.
;;;
;;; dest_y :
;;;     Y coordinate within dest_pixbuf.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_saturate_and_pixelate ()
;;;
;;; void gdk_pixbuf_saturate_and_pixelate (const GdkPixbuf *src,
;;;                                        GdkPixbuf *dest,
;;;                                        gfloat saturation,
;;;                                        gboolean pixelate);
;;;
;;; Modifies saturation and optionally pixelates src, placing the result in
;;; dest. src and dest may be the same pixbuf with no ill effects. If saturation
;;; is 1.0 then saturation is not changed. If it's less than 1.0, saturation is
;;; reduced (the image turns toward grayscale); if greater than 1.0, saturation
;;; is increased (the image gets more vivid colors). If pixelate is TRUE, then
;;; pixels are faded in a checkerboard pattern to create a pixelated image. src
;;; and dest must have the same image format, size, and rowstride.
;;;
;;; src :
;;;     source image
;;;
;;; dest :
;;;     place to write modified version of src
;;;
;;; saturation :
;;;     saturation factor
;;;
;;; pixelate :
;;;     whether to pixelate
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_apply_embedded_orientation ()
;;;
;;; GdkPixbuf * gdk_pixbuf_apply_embedded_orientation (GdkPixbuf *src);
;;;
;;; Takes an existing pixbuf and checks for the presence of an associated
;;; "orientation" option, which may be provided by the jpeg loader (which reads
;;; the exif orientation tag) or the tiff loader (which reads the tiff
;;; orientation tag, and compensates it for the partial transforms performed by
;;; libtiff). If an orientation option/tag is present, the appropriate transform
;;; will be performed so that the pixbuf is oriented correctly.
;;;
;;; src :
;;;     A GdkPixbuf.
;;;
;;; Returns :
;;;     A newly-created pixbuf, or a reference to the input pixbuf (with an
;;;     increased reference count). [transfer full]
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_fill ()
;;;
;;; void gdk_pixbuf_fill (GdkPixbuf *pixbuf, guint32 pixel);
;;;
;;; Clears a pixbuf to the given RGBA value, converting the RGBA value into the
;;; pixbuf's pixel format. The alpha will be ignored if the pixbuf doesn't have
;;; an alpha channel.
;;;
;;; pixbuf :
;;;     a GdkPixbuf
;;;
;;; pixel :
;;;     RGBA pixel to clear to (0xffffffff is opaque white, 0x00000000
;;;     transparent black)
;;; ----------------------------------------------------------------------------

;;; --- End of file gdk-pixbuf.utilities.lisp ----------------------------------