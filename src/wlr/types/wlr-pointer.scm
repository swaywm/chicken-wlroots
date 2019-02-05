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

(foreign-declare "#include <wlr/types/wlr_pointer.h>")
(include "wlroots-types.scm")

(module (wlr types wlr-pointer)
        (wlr-pointer-impl
         wlr-pointer-events-motion
         wlr-pointer-events-motion-absolute
         wlr-pointer-events-button
         wlr-pointer-events-axis
         wlr-pointer-events-frame
         wlr-pointer-events-swipe-begin
         wlr-pointer-events-swipe-update
         wlr-pointer-events-swipe-end
         wlr-pointer-events-pinch-begin
         wlr-pointer-events-pinch-update
         wlr-pointer-events-pinch-end
         wlr-pointer-data

         wlr-event-pointer-motion-device
         wlr-event-pointer-motion-time-msec
         wlr-event-pointer-motion-delta-x
         wlr-event-pointer-motion-delta-y
         wlr-event-pointer-motion-unaccel-dx
         wlr-event-pointer-motion-unaccel-dy

         wlr-event-pointer-motion-absolute-device
         wlr-event-pointer-motion-absolute-time-msec
         wlr-event-pointer-motion-absolute-x
         wlr-event-pointer-motion-absolute-y

         wlr-event-pointer-button-device
         wlr-event-pointer-button-time-msec
         wlr-event-pointer-button-button
         wlr-event-pointer-button-state

         wlr-axis-source/wheel
         wlr-axis-source/finger
         wlr-axis-source/continuous
         wlr-axis-source/wheel-tilt

         wlr-axis-orientation/veritcal
         wlr-axis-orientation/horizontal

         wlr-event-pointer-axis-device
         wlr-event-pointer-axis-time-msec
         wlr-event-pointer-axis-source
         wlr-event-pointer-axis-orientation
         wlr-event-pointer-axis-delta
         wlr-event-pointer-axis-delta-discrete

         wlr-event-pointer-swipe-begin-device
         wlr-event-pointer-swipe-begin-time-msec
         wlr-event-pointer-swipe-begin-fingers

         wlr-event-pointer-swipe-update-device
         wlr-event-pointer-swipe-update-time-msec
         wlr-event-pointer-swipe-update-fingers
         wlr-event-pointer-swipe-update-dx
         wlr-event-pointer-swipe-update-dy

         wlr-event-pointer-swipe-end-device
         wlr-event-pointer-swipe-end-time-msec
         wlr-event-pointer-swipe-end-cancelled?

         wlr-event-pointer-pinch-begin-device
         wlr-event-pointer-pinch-begin-time-msec
         wlr-event-pointer-pinch-begin-fingers

         wlr-event-pointer-pinch-update-device
         wlr-event-pointer-pinch-update-time-msec
         wlr-event-pointer-pinch-update-fingers
         wlr-event-pointer-pinch-update-dx
         wlr-event-pointer-pinch-update-dy
         wlr-event-pointer-pinch-update-scale
         wlr-event-pointer-pinch-update-rotation

         wlr-event-pointer-pinch-end-device
         wlr-event-pointer-pinch-end-time-msec
         wlr-event-pointer-pinch-end-cancelled?)
  (import (scheme)
          (chicken base)
          (chicken foreign)
          (foreigners)
          (ffi-helpers)
          (bind))

  (include "bind-options.scm")
  (bind-file "include/bind/wlr/types/wlr_pointer.h")

  (define-foreign-record-type (wlr-pointer* "struct wlr_pointer")
    ((struct "wl_signal") events.motion wlr-pointer-events-motion)
    ((struct "wl_signal") events.motion_absolute wlr-pointer-events-motion-absolute)
    ((struct "wl_signal") events.button wlr-pointer-events-button)
    ((struct "wl_signal") events.axis wlr-pointer-events-axis)
    ((struct "wl_signal") events.frame wlr-pointer-events-frame)
    ((struct "wl_signal") events.swipe_begin wlr-pointer-events-swipe-begin)
    ((struct "wl_signal") events.swipe_update wlr-pointer-events-swipe-update)
    ((struct "wl_signal") events.swipe_end wlr-pointer-events-swipe-end)
    ((struct "wl_signal") events.pinch_begin wlr-pointer-events-pinch-begin)
    ((struct "wl_signal") events.pinch_update wlr-pointer-events-pinch-update)
    ((struct "wl_signal") events.pinch_end wlr-pointer-events-pinch-end))

  (define-foreign-values wlr-axis-source
    (wlr-axis-source/wheel      "WLR_AXIS_SOURCE_WHEEL")
    (wlr-axis-source/finger     "WLR_AXIS_SOURCE_FINGER")
    (wlr-axis-source/continuous "WLR_AXIS_SOURCE_CONTINUOUS")
    (wlr-axis-source/wheel-tilt "WLR_AXIS_SOURCE_WHEEL_TILT"))

  (define-foreign-values wlr-axis-orientation
    (wlr-axis-orientation/veritcal   "WLR_AXIS_ORIENTATION_VERTICAL")
    (wlr-axis-orientation/horizontal "WLR_AXIS_ORIENTATION_HORIZONTAL"))

  (define wlr-event-pointer-swipe-end-cancelled? wlr-event-pointer-swipe-end-cancelled)
  (define wlr-event-pointer-pinch-end-cancelled? wlr-event-pointer-pinch-end-cancelled))
