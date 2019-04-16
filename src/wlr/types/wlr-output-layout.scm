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

(foreign-declare "#include <wlr/types/wlr_output_layout.h>")
(include "wlroots-types.scm")

(module (wlr types wlr-output-layout)
        (wlr-output-layout-outputs ; TODO: return as scheme list
         wlr-output-layout-state
         wlr-output-layout-events-add
         wlr-output-layout-events-change
         wlr-output-layout-events-destroy
         wlr-output-layout-data
         wlr-output-layout-output-output
         wlr-output-layout-output-x
         wlr-output-layout-output-y
         wlr-output-layout-output-link
         wlr-output-layout-output-state
         wlr-output-layout-output-events-destroy

         wlr-output-layout-create
         wlr-output-layout-destroy
         wlr-output-layout-get
         wlr-output-layout-output-at
         wlr-output-layout-add
         wlr-output-layout-move
         wlr-output-layout-remove
         wlr-output-layout-output-coords
         wlr-output-layout-contains-point
         wlr-output-layout-intersects
         wlr-output-layout-closest-point
         wlr-output-layout-get-box
         wlr-output-layout-add-auto
         wlr-output-layout-get-center-output
         wlr-output-layout-adjacent-output
         wlr-output-layout-farthest-output)
  (import (scheme)
          (chicken base))
  (include "ffi-helpers.scm")

  (bind-file "include/bind/wlr/types/wlr_output_layout.h")

  (define-foreign-record-type (wlr-output-layout* "struct wlr_output_layout")
    ((struct "wl_list") outputs wlr-output-layout-outputs)
    ((struct "wl_signal") events.add wlr-output-layout-events-add)
    ((struct "wl_signal") events.change wlr-output-layout-events-change)
    ((struct "wl_signal") events.destroy wlr-output-layout-events-destroy))

  (define-foreign-record-type (wlr-output-layout-output "struct wlr_output_layout_output")
    ((struct "wl_list") link wlr-output-layout-output-link)
    ((struct "wl_signal") events.destroy wlr-output-layout-output-events-destroy))

  (define-foreign-values wlr-direction
    (wlr-direction/up    "WLR_DIRECTION_UP")
    (wlr-direction/down  "WLR_DIRECTION_DOWN")
    (wlr-direction/left  "WLR_DIRECTION_LEFT")
    (wlr-direction/right "WLR_DIRECTION_RIGHT")))
