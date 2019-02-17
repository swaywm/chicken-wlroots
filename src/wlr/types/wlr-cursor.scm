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

(foreign-declare "#include <wlr/types/wlr_cursor.h>")
(include "wlroots-types.scm")

(module (wlr types wlr-cursor)
        (wlr-cursor-state
         wlr-cursor-x
         wlr-cursor-y
         wlr-cursor-events-motion
         wlr-cursor-events-motion-absolute
         wlr-cursor-events-button
         wlr-cursor-events-axis
         wlr-cursor-events-frame
         wlr-cursor-events-swipe-begin
         wlr-cursor-events-swipe-update
         wlr-cursor-events-swipe-end
         wlr-cursor-events-pinch-begin
         wlr-cursor-events-pinch-update
         wlr-cursor-events-pinch-end
         wlr-cursor-events-touch-up
         wlr-cursor-events-touch-down
         wlr-cursor-events-touch-motion
         wlr-cursor-events-touch-cancel
         wlr-cursor-events-tablet-tool-axis
         wlr-cursor-events-tablet-tool-proximity
         wlr-cursor-events-tablet-tool-tip
         wlr-cursor-events-tablet-tool-button
         wlr-cursor-data

         wlr-cursor-create
         wlr-cursor-destroy
         wlr-cursor-warp
         wlr-cursor-absolute-to-layout-coords
         wlr-cursor-warp-closest
         wlr-cursor-warp-absolute
         wlr-cursor-move
         wlr-cursor-set-image
         wlr-cursor-set-surface
         wlr-cursor-attach-input-device
         wlr-cursor-detach-input-device
         wlr-cursor-attach-output-layout
         wlr-cursor-map-to-output
         wlr-cursor-map-input-to-output
         wlr-cursor-map-to-region
         wlr-cursor-map-input-to-region)
  (import (scheme)
          (chicken base))
  (include "ffi-helpers.scm")

  (bind-file "include/bind/wlr/types/wlr_cursor.h")

  (define-foreign-record-type (wlr-cursor* "struct wlr_cursor")
    ((struct "wl_signal") events.motion wlr-cursor-events-motion)
    ((struct "wl_signal") events.motion_absolute wlr-cursor-events-motion-absolute)
    ((struct "wl_signal") events.button wlr-cursor-events-button)
    ((struct "wl_signal") events.axis wlr-cursor-events-axis)
    ((struct "wl_signal") events.frame wlr-cursor-events-frame)
    ((struct "wl_signal") events.swipe_begin wlr-cursor-events-swipe-begin)
    ((struct "wl_signal") events.swipe_update wlr-cursor-events-swipe-update)
    ((struct "wl_signal") events.swipe_end wlr-cursor-events-swipe-end)
    ((struct "wl_signal") events.pinch_begin wlr-cursor-events-pinch-begin)
    ((struct "wl_signal") events.pinch_update wlr-cursor-events-pinch-update)
    ((struct "wl_signal") events.pinch_end wlr-cursor-events-pinch-end)
    ((struct "wl_signal") events.touch_up wlr-cursor-events-touch-up)
    ((struct "wl_signal") events.touch_down wlr-cursor-events-touch-down)
    ((struct "wl_signal") events.touch_motion wlr-cursor-events-touch-motion)
    ((struct "wl_signal") events.touch_cancel wlr-cursor-events-touch-cancel)
    ((struct "wl_signal") events.tablet_tool_axis wlr-cursor-events-tablet-tool-axis)
    ((struct "wl_signal") events.tablet_tool_proximity wlr-cursor-events-tablet-tool-proximity)
    ((struct "wl_signal") events.tablet_tool_tip wlr-cursor-events-tablet-tool-tip)
    ((struct "wl_signal") events.tablet_tool_button wlr-cursor-events-tablet-tool-button)))
