;; Copyright 2019 Drew Thoreson
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to
;; deal in the Software without restriction, including without limitation the
;; rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
;; sell copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
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

(foreign-declare "#include <wlr/types/wlr_compositor.h>")
(include "wlroots-types.scm")

(module (wlr types wlr-compositor)
        (wlr-compositor-destroy
         wlr-compositor-create
         wlr-surface-is-subsurface
         wlr-subsurface-from-wlr-surface)
  (import (scheme)
          (chicken base)
          (chicken foreign)
          (foreigners)
          (bind))

  (include "bind-options.scm")
  (bind-file "include/bind/wlr/types/wlr_compositor.h")

  (define-foreign-record-type (wlr-subcompositor* "struct wlr_subcompositor")
    ((struct "wl_list") resources wlr-subcompositor-resources)
    ((struct "wl_list") subsurface_resources wlr-subcompositor-subsurface-resources))

  (define-foreign-record-type (wlr-compositor* "struct wlr_compositor")
    ((struct "wl_list") resources wlr-compositor-resources)
    ((struct "wl_list") surface_resources wlr-compositor-surface-resources)
    ((struct "wl_list") region_resources wlr-compositor-region-resources)
    ((struct "wlr_subcompositor") subcompositor wlr-compositor-subcompositor)
    ((struct "wl_listener") display_destroy wlr-compositor-display-destroy)
    ((struct "wl_signal") events.new_surface wlr-compositor-events-new-surface)
    ((struct "wl_signal") events.destroy wlr-compositor-events-destroy)))
