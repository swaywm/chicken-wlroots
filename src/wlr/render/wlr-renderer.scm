;; Copyright 2019 Drew Thoreson
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to
;; deal in the Software without restriction, including without limitation the
;; rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
;; sell copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions-
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
;; IN THE SOFTWARE.

(foreign-declare "#include <wlr/render/wlr_renderer.h>")
(include "wlroots-types.scm")

(module (wlr render wlr-renderer)
        (wlr-renderer-read-pixels/y-invert
         wlr-renderer-impl
         wlr-renderer-rendering?
         wlr-renderer-events-destroy
         wlr-renderer-autocreate
         wlr-renderer-begin
         wlr-renderer-end
         wlr-renderer-clear
         wlr-renderer-scissor
         wlr-render-texture
         wlr-render-texture-with-matrix
         wlr-render-subtexture-with-matrix
         wlr-render-rect
         wlr-render-quad-with-matrix
         wlr-render-ellipse
         wlr-render-ellipse-with-matrix
         wlr-renderer-get-formats
         wlr-renderer-resource-is-wl-drm-buffer
         wlr-renderer-get-dmabuf-formats
         wlr-renderer-read-pixels
         wlr-renderer-blit-dmabuf
         wlr-renderer-format-supported
         wlr-renderer-init-wl-display
         wlr-renderer-destroy)
  (import (scheme)
          (chicken base))
  (include "ffi-helpers.scm")

  (bind-file "include/bind/wlr/render/wlr_renderer.h")
  (define wlr-renderer-rendering? wlr-renderer-rendering)

  (define-foreign-values wlr-renderer-read-pixels-flags
    (wlr-renderer-read-pixels/y-invert "WLR_RENDERER_READ_PIXELS_Y_INVERT"))

  (define-foreign-record-type (wlr-renderer* "struct wlr_renderer")
    ((struct "wl_signal") events.destroy wlr-renderer-events-destroy)))
