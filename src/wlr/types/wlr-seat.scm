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

(foreign-declare "#include <wlr/types/wlr_seat.h>")
(include "wlroots-types.scm")

(module (wlr types wlr-seat)
        (wlr-seat-client-client
         wlr-seat-client-seat
         wlr-seat-client-link
         wlr-seat-client-resources
         wlr-seat-client-pointers
         wlr-seat-client-keyboards
         wlr-seat-client-touches
         wlr-seat-client-data-devices
         wlr-seat-client-events-destroy

         wlr-touch-point-touch-id
         wlr-touch-point-surface
         wlr-touch-point-client
         wlr-touch-point-focus-surface
         wlr-touch-point-focus-client
         wlr-touch-point-sx
         wlr-touch-point-sy
         wlr-touch-point-surface-destroy
         wlr-touch-point-focus-surface-destroy
         wlr-touch-point-events-destroy
         wlr-touch-point-link

         wl-pointer-grab-interface-enter
         wl-pointer-grab-interface-motion
         wl-pointer-grab-interface-button
         wl-pointer-grab-interface-axis
         wl-pointer-grab-interface-frame
         wl-pointer-grab-interface-cancel

         wl-keyboard-grab-interface-enter
         wl-keyboard-grab-interface-key
         wl-keyboard-grab-interface-modifiers
         wl-keyboard-grab-interface-cancel

         wl-touch-grab-interface-down
         wl-touch-grab-interface-up
         wl-touch-grab-interface-motion
         wl-touch-grab-interface-enter
         wl-touch-grab-interface-cancel

         wlr-seat-touch-grab-interface
         wlr-seat-touch-grab-seat
         wlr-seat-touch-grab-data

         wlr-seat-keyboard-grab-interface
         wlr-seat-keyboard-grab-seat
         wlr-seat-keyboard-grab-data

         wlr-seat-pointer-grab-interface
         wlr-seat-pointer-grab-seat
         wlr-seat-pointer-grab-data

         wlr-seat-pointer-state-seat
         wlr-seat-pointer-state-focused-client
         wlr-seat-pointer-state-focused-surface
         wlr-seat-pointer-state-sx
         wlr-seat-pointer-state-sy
         wlr-seat-pointer-state-grab
         wlr-seat-pointer-state-default-grab
         wlr-seat-pointer-state-button-count
         wlr-seat-pointer-state-grab-button
         wlr-seat-pointer-state-grab-serial
         wlr-seat-pointer-state-grab-time
         wlr-seat-pointer-state-surface-destroy
         wlr-seat-pointer-state-events-focus-change

         wlr-seat-keyboard-state-seat
         wlr-seat-keyboard-state-keyboard
         wlr-seat-keyboard-state-focused-client
         wlr-seat-keyboard-state-focused-surface
         wlr-seat-keyboard-state-keyboard-destroy
         wlr-seat-keyboard-state-keyboard-keymap
         wlr-seat-keyboard-state-keyboard-repeat-info
         wlr-seat-keyboard-state-surface-destroy
         wlr-seat-keyboard-state-grab
         wlr-seat-keyboard-state-default-grab
         wlr-seat-keyboard-state-events-focus-change

         wlr-seat-touch-state-seat
         wlr-seat-touch-state-touch-points
         wlr-seat-touch-state-grab-serial
         wlr-seat-touch-state-grab-id
         wlr-seat-touch-state-grab
         wlr-seat-touch-state-default-grab

         wlr-seat-global
         wlr-seat-display
         wlr-seat-clients
         wlr-seat-name
         wlr-seat-capabilities
         wlr-seat-last-event
         wlr-seat-selection-source
         wlr-seat-selection-serial
         wlr-seat-selection-offers
         wlr-seat-primary-selection-source
         wlr-seat-primary-selection-serial
         wlr-seat-drag
         wlr-seat-drag-source
         wlr-seat-drag-serial
         wlr-seat-drag-offers
         wlr-seat-pointer-state
         wlr-seat-keyboard-state
         wlr-seat-touch-state
         wlr-seat-display-destroy
         wlr-seat-selection-source-destroy
         wlr-seat-primary-selection-source-destroy
         wlr-seat-drag-source-destroy
         wlr-seat-events-pointer-grab-begin
         wlr-seat-events-pointer-grab-end
         wlr-seat-events-keyboard-grab-begin
         wlr-seat-events-keyboard-grab-end
         wlr-seat-events-touch-grab-begin
         wlr-seat-events-touch-grab-end
         wlr-seat-events-request-set-cursor
         wlr-seat-events-request-set-selection
         wlr-seat-events-set-selection
         wlr-seat-events-request-set-primary-selection
         wlr-seat-events-set-primary-selection
         wlr-seat-events-request-start-drag
         wlr-seat-events-start-drag
         wlr-seat-events-destroy
         wlr-seat-data

         wlr-seat-pointer-request-set-cursor-event-seat-client
         wlr-seat-pointer-request-set-cursor-event-surface
         wlr-seat-pointer-request-set-cursor-event-serial
         wlr-seat-pointer-request-set-cursor-event-hotspot-x
         wlr-seat-pointer-request-set-cursor-event-hotspot-y

         wlr-seat-request-set-selection-event-source
         wlr-seat-request-set-selection-event-serial

         wlr-seat-request-set-primary-selection-event-source
         wlr-seat-request-set-primary-selection-event-serial

         wlr-seat-request-start-drag-event-drag
         wlr-seat-request-start-drag-event-origin
         wlr-seat-request-start-drag-event-serial

         wlr-seat-pointer-focus-change-event-seat
         wlr-seat-pointer-focus-change-event-old-surface
         wlr-seat-pointer-focus-change-event-new-surface
         wlr-seat-pointer-focus-change-event-sx
         wlr-seat-pointer-focus-change-event-sy

         wlr-seat-keyboard-focus-change-event-seat
         wlr-seat-keyboard-focus-change-event-old-surface
         wlr-seat-keyboard-focus-change-event-new-surface

         wlr-seat-create
         wlr-seat-destroy
         wlr-seat-client-for-wl-client
         wlr-seat-set-capabilities
         wlr-seat-set-name
         wlr-seat-pointer-surface-has-focus?
         wlr-seat-pointer-enter
         wlr-seat-pointer-clear-focus
         wlr-seat-pointer-send-motion
         wlr-seat-pointer-send-button
         wlr-seat-pointer-send-axis
         wlr-seat-pointer-send-frame
         wlr-seat-pointer-start-grab
         wlr-seat-pointer-end-grab
         wlr-seat-pointer-notify-enter
         wlr-seat-pointer-notify-motion
         wlr-seat-pointer-notify-button
         wlr-seat-pointer-notify-axis
         wlr-seat-pointer-notify-frame
         wlr-seat-pointer-has-grab?
         wlr-seat-set-keyboard
         wlr-seat-get-keyboard
         wlr-seat-keyboard-start-grab
         wlr-seat-keyboard-end-grab
         wlr-seat-keyboard-send-key
         wlr-seat-keyboard-notify-key
         wlr-seat-keyboard-send-modifiers
         wlr-seat-keyboard-notify-modifiers
         wlr-seat-keyboard-notify-enter
         wlr-seat-keyboard-enter
         wlr-seat-keyboard-clear-focus
         wlr-seat-keyboard-has-grab?
         wlr-seat-touch-start-grab
         wlr-seat-touch-end-grab
         wlr-seat-touch-get-point
         wlr-seat-touch-notify-down
         wlr-seat-touch-notify-up
         wlr-seat-touch-notify-motion
         wlr-seat-touch-point-focus
         wlr-seat-touch-point-clear-focus
         wlr-seat-touch-send-down
         wlr-seat-touch-send-up
         wlr-seat-touch-send-motion
         wlr-seat-touch-num-points
         wlr-seat-touch-has-grab?
         wlr-seat-validate-grab-serial
         wlr-seat-validate-pointer-grab-serial
         wlr-seat-validate-touch-grab-serial
         wlr-seat-client-from-resource
         wlr-seat-client-from-pointer-resource)
  (import (scheme)
          (chicken base))
  (include "ffi-helpers.scm")

  (bind-file "include/bind/wlr/types/wlr_seat.h")

  (define-foreign-record-type (wlr-seat-client* "struct wlr_seat_client")
    ((struct "wl_list") link wlr-seat-client-link)
    ((struct "wl_list") resources wlr-seat-client-resources)
    ((struct "wl_list") pointers wlr-seat-client-pointers)
    ((struct "wl_list") keyboards wlr-seat-client-keyboards)
    ((struct "wl_list") touches wlr-seat-client-touches)
    ((struct "wl_list") data_devices wlr-seat-client-data-devices)
    ((struct "wl_signal") events.destroy wlr-seat-client-events-destroy))

  (define-foreign-record-type (wlr-touch-point* "struct wlr_touch_point")
    ((struct "wl_listener") surface_destroy wlr-touch-point-surface-destroy)
    ((struct "wl_listener") focus_surface_destroy wlr-touch-point-focus-surface-destroy)
    ((struct "wl_signal") events.destroy wlr-touch-point-events-destroy)
    ((struct "wl_link") link wlr-touch-point-link))

  (define-foreign-record-type (wlr-pointer-grab-interface* "struct wlr_pointer_grab_interface")
    ((function void ((c-pointer (struct "wlr_seat_pointer_grab"))
                     (c-pointer (struct "wlr_surface"))
                     double
                     double))
      enter wl-pointer-grab-interface-enter)
    ((function void ((c-pointer (struct "wlr_seat_pointer_grab"))
                     unsigned-int32
                     double
                     double))
      motion wl-pointer-grab-interface-motion)
    ((function unsigned-int32 ((c-pointer (struct "wlr_seat_pointer_grab"))
                               unsigned-int32
                               double
                               double))
      button wl-pointer-grab-interface-button)
    ((function void ((c-pointer (struct "wlr_seat_pointer_grab"))
                     unsigned-int32
                     (enum "wlr_axis_orientation")
                     double
                     int32
                     (enum "wlr_axis_source")))
      axis wl-pointer-grab-interface-axis)
    ((function void ((c-pointer (struct "wlr_seat_pointer_grab"))))
      frame wl-pointer-grab-interface-frame)
    ((function void ((c-pointer (struct "wlr_seat_pointer_grab"))))
      cancel wl-pointer-grab-interface-cancel))

  (define-foreign-record-type (wlr-keyboard-grab-interface* "struct wlr_keyboard_grab_interface")
    ((function void ((c-pointer (struct "wlr_seat_keyboard_grab"))
                     (c-pointer (struct "wlr_surface"))
                     (c-pointer unsigned-int32)
                     size_t
                     (c-pointer (struct "wlr_keyboard_modifiers"))))
      enter wl-keyboard-grab-interface-enter)
    ((function void ((c-pointer (struct "wlr_seat_keyboard_grab"))
                     unsigned-int32
                     unsigned-int32
                     unsigned-int32))
      key wl-keyboard-grab-interface-key)
    ((function void ((c-pointer (struct "wlr_seat_keyboard_grab"))
                     (c-pointer (struct "wlr_keyboard_modifiers"))))
      modifiers wl-keyboard-grab-interface-modifiers)
    ((function void ((c-pointer (struct "wlr_seat_keyboard_grab"))))
      cancel wl-keyboard-grab-interface-cancel))

  (define-foreign-record-type (wlr-touch-grab-interface* "struct wlr_touch_grab_interface")
    ((function unsigned-int32 ((c-pointer (struct "wlr_seat_touch_grab"))
                               unsigned-int32
                               (c-pointer (struct "wlr_touch_point"))))
      down wl-touch-grab-interface-down)
    ((function void ((c-pointer (struct "wlr_seat_touch_grab"))
                     unsigned-int32
                     (c-pointer (struct "wlr_touch_point"))))
      up wl-touch-grab-interface-up)
    ((function void ((c-pointer (struct "wlr_seat_touch_grab"))
                     unsigned-int32
                     (c-pointer (struct "wlr_touch_point"))))
      motion wl-touch-grab-interface-motion)
    ((function void ((c-pointer (struct "wlr_seat_touch_grab"))
                     unsigned-int32
                     (c-pointer (struct "wlr_touch_point"))))
      enter wl-touch-grab-interface-enter)
    ((function void ((c-pointer (struct "wlr_seat_touch_grab"))))
      cancel wl-touch-grab-interface-cancel))

  (define-foreign-record-type (wlr-seat-pointer-state* "struct wlr_seat_pointer_state")
    ((struct "wl_listener") surface_destroy wlr-seat-pointer-state-surface-destroy)
    ((struct "wl_signal") events.focus_change wlr-seat-pointer-state-events-focus-change))

  (define-foreign-record-type (wlr-seat-keyboard-state* "struct wlr_seat_keyboard_state")
    ((struct "wl_listener") keyboard_destroy wlr-seat-keyboard-state-keyboard-destroy)
    ((struct "wl_listener") keyboard_keymap wlr-seat-keyboard-state-keyboard-keymap)
    ((struct "wl_listener") keyboard_repeat_info wlr-seat-keyboard-state-keyboard-repeat-info)
    ((struct "wl_listener") surface_destroy wlr-seat-keyboard-state-surface-destroy)
    ((struct "wl_signal") events.focus_change wlr-seat-keyboard-state-events-focus-change))

  (define-foreign-record-type (wlr-seat-touch-state* "struct wlr_seat_touch_state")
    ((struct "wl_list") touch_points wlr-seat-touch-state-touch-points))

  (define-foreign-record-type (wlr-seat* "struct wlr_seat")
    ((struct "wl_list") clients wlr-seat-clients)
    ((struct "timespec") last_event wlr-seat-last-event)
    ((struct "wl_list") selection_offers wlr-seat-selection-offers)
    ((struct "wl_list") drag_offers wlr-seat-drag-offers)
    ((struct "wlr_seat_pointer_state") pointer_state wlr-seat-pointer-state)
    ((struct "wlr_seat_keyboard_state") keyboard_state wlr-seat-keyboard-state)
    ((struct "wlr_seat_touch_state") touch_state wlr-seat-touch-state)
    ((struct "wl_listener") display_destroy wlr-seat-display-destroy)
    ((struct "wl_listener") selection_source_destroy wlr-seat-selection-source-destroy)
    ((struct "wl_listener")
      primary_selection_source_destroy wlr-seat-primary-selection-source-destroy)
    ((struct "wl_listener") drag_source_destroy wlr-seat-drag-source-destroy)
    ((struct "wl_signal") events.pointer_grab_begin wlr-seat-events-pointer-grab-begin)
    ((struct "wl_signal") events.pointer_grab_end wlr-seat-events-pointer-grab-end)
    ((struct "wl_signal") events.keyboard_grab_begin wlr-seat-events-keyboard-grab-begin)
    ((struct "wl_signal") events.keyboard_grab_end wlr-seat-events-keyboard-grab-end)
    ((struct "wl_signal") events.touch_grab_begin wlr-seat-events-touch-grab-begin)
    ((struct "wl_signal") events.touch_grab_end wlr-seat-events-touch-grab-end)
    ((struct "wl_signal") events.request_set_cursor wlr-seat-events-request-set-cursor)
    ((struct "wl_signal") events.request_set_selection wlr-seat-events-request-set-selection)
    ((struct "wl_signal") events.set_selection wlr-seat-events-set-selection)
    ((struct "wl_signal") events.request_set_primary_selection
                          wlr-seat-events-request-set-primary-selection)
    ((struct "wl_signal") events.set_primary_selection wlr-seat-events-set-primary-selection)
    ((struct "wl_signal") events.request_start_drag wlr-seat-events-request-start-drag)
    ((struct "wl_signal") events.start_drag wlr-seat-events-start-drag)
    ((struct "wl_signal") events.destroy wlr-seat-events-destroy))
  
  (define wlr-seat-pointer-surface-has-focus? wlr-seat-pointer-surface-has-focus)
  (define wlr-seat-pointer-has-grab? wlr-seat-pointer-has-grab)
  (define wlr-seat-keyboard-has-grab? wlr-seat-keyboard-has-grab)
  (define wlr-seat-touch-has-grab? wlr-seat-touch-has-grab))
