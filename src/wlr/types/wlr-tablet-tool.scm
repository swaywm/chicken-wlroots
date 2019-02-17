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

(foreign-declare "#include <wlr/types/wlr_tablet_tool.h>")
(include "wlroots-types.scm")

(module (wlr types wlr-tablet-tool)
        (wlr-tablet-tool-type/pen
         wlr-tablet-tool-type/eraser
         wlr-tablet-tool-type/brush
         wlr-tablet-tool-type/pencil
         wlr-tablet-tool-type/airbrush
         wlr-tablet-tool-type/mouse
         wlr-tablet-tool-type/lens

         wlr-tablet-tool-type
         wlr-tablet-tool-hardware-serial
         wlr-tablet-tool-hardware-wacom
         wlr-tablet-tool-tilt
         wlr-tablet-tool-pressure
         wlr-tablet-tool-distance
         wlr-tablet-tool-rotation
         wlr-tablet-tool-slider
         wlr-tablet-tool-wheel
         wlr-tablet-tool-events-destroy
         wlr-tablet-tool-data

         wlr-tablet-impl
         wlr-tablet-events-axis
         wlr-tablet-events-proximity
         wlr-tablet-events-tip
         wlr-tablet-events-button
         wlr-tablet-name
         wlr-tablet-paths
         wlr-tablet-data

         wlr-tablet-tool-axis/x
         wlr-tablet-tool-axis/y
         wlr-tablet-tool-axis/distance
         wlr-tablet-tool-axis/pressure
         wlr-tablet-tool-axis/tilt-x
         wlr-tablet-tool-axis/tilt-y
         wlr-tablet-tool-axis/rotation
         wlr-tablet-tool-axis/slider
         wlr-tablet-tool-axis/wheel

         wlr-event-tablet-tool-axis-device
         wlr-event-tablet-tool-axis-tool
         wlr-event-tablet-tool-axis-time-msec
         wlr-event-tablet-tool-axis-updated-axes
         wlr-event-tablet-tool-axis-x
         wlr-event-tablet-tool-axis-y
         wlr-event-tablet-tool-axis-dx
         wlr-event-tablet-tool-axis-dy
         wlr-event-tablet-tool-axis-pressure
         wlr-event-tablet-tool-axis-distance
         wlr-event-tablet-tool-axis-tilt-x
         wlr-event-tablet-tool-axis-tilt-y
         wlr-event-tablet-tool-axis-rotation
         wlr-event-tablet-tool-axis-slider
         wlr-event-tablet-tool-axis-wheel-delta

         wlr-tablet-tool-proximity/out
         wlr-tablet-tool-proximity/in

         wlr-event-tablet-tool-proximity-device
         wlr-event-tablet-tool-proximity-tool
         wlr-event-tablet-tool-proximity-time-msec
         wlr-event-tablet-tool-proximity-x
         wlr-event-tablet-tool-proximity-y
         wlr-event-tablet-tool-proximity-state

         wlr-tablet-tool-tip/up
         wlr-tablet-tool-tip/down

         wlr-event-tablet-tool-tip-device
         wlr-event-tablet-tool-tip-tool
         wlr-event-tablet-tool-tip-time-msec
         wlr-event-tablet-tool-tip-x
         wlr-event-tablet-tool-tip-y
         wlr-event-tablet-tool-tip-state

         wlr-event-tablet-tool-button-device
         wlr-event-tablet-tool-button-tool
         wlr-event-tablet-tool-button-time-msec
         wlr-event-tablet-tool-button-button
         wlr-event-tablet-tool-button-state)
  (import (scheme)
          (chicken base))
  (include "ffi-helpers.scm")

  (bind-file "include/bind/wlr/types/wlr_tablet_tool.h")

  (define-foreign-values wlr-tablet-tool-type
    (wlr-tablet-tool-type/pen      "WLR_TABLET_TOOL_TYPE_PEN")
    (wlr-tablet-tool-type/eraser   "WLR_TABLET_TOOL_TYPE_ERASER")
    (wlr-tablet-tool-type/brush    "WLR_TABLET_TOOL_TYPE_BRUSH")
    (wlr-tablet-tool-type/pencil   "WLR_TABLET_TOOL_TYPE_PENCIL")
    (wlr-tablet-tool-type/airbrush "WLR_TABLET_TOOL_TYPE_AIRBRUSH")
    (wlr-tablet-tool-type/mouse    "WLR_TABLET_TOOL_TYPE_MOUSE")
    (wlr-tablet-tool-type/lens     "WLR_TABLET_TOOL_TYPE_LENS"))

  (define-foreign-record-type (wlr-tablet-tool* "struct wlr_tablet_tool")
    ((struct "wl_signal") events.destroy wlr-tablet-tool-events-destroy))

  (define-foreign-record-type (wlr-tablet* "struct wlr_tablet")
    ((struct "wl_signal") events.axis wlr-tablet-events-axis)
    ((struct "wl_signal") events.proximity wlr-tablet-events-proximity)
    ((struct "wl_signal") events.tip wlr-tablet-events-tip)
    ((struct "wl_signal") events.button wlr-tablet-events-button)
    ((struct "wl_list") paths wlr-tablet-paths))

  (define-foreign-values wlr-tablet-tool-axes
    (wlr-tablet-tool-axis/x        "WLR_TABLET_TOOL_AXIS_X")
    (wlr-tablet-tool-axis/y        "WLR_TABLET_TOOL_AXIS_Y")
    (wlr-tablet-tool-axis/distance "WLR_TABLET_TOOL_AXIS_DISTANCE")
    (wlr-tablet-tool-axis/pressure "WLR_TABLET_TOOL_AXIS_PRESSURE")
    (wlr-tablet-tool-axis/tilt-x   "WLR_TABLET_TOOL_AXIS_TILT_X")
    (wlr-tablet-tool-axis/tilt-y   "WLR_TABLET_TOOL_AXIS_TILT_Y")
    (wlr-tablet-tool-axis/rotation "WLR_TABLET_TOOL_AXIS_ROTATION")
    (wlr-tablet-tool-axis/slider   "WLR_TABLET_TOOL_AXIS_SLIDER")
    (wlr-tablet-tool-axis/wheel    "WLR_TABLET_TOOL_AXIS_WHEEL"))

  (define-foreign-values wlr-tablet-tool-proximity-state
    (wlr-tablet-tool-proximity/out "WLR_TABLET_TOOL_PROXIMITY_OUT")
    (wlr-tablet-tool-proximity/in  "WLR_TABLET_TOOL_PROXIMITY_IN"))

  (define-foreign-values wlr-tablet-tool-tip-state
    (wlr-tablet-tool-tip/up   "WLR_TABLET_TOOL_TIP_UP")
    (wlr-tablet-tool-tip/down "WLR_TABLET_TOOL_TIP_DOWN")))
