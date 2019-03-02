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

(foreign-declare "#include <wlr/types/wlr_data_device.h>")
(include "wlroots-types.scm")

(module (wlr types wlr-data-device)
        (wlr-data-device-manager-global
         wlr-data-device-manager-resources
         wlr-data-device-manager-data-sources
         wlr-data-device-manager-display-destroy
         wlr-data-device-manager-events-destroy
         wlr-data-device-manager-data
         wlr-data-offer/selection
         wlr-data-offer/drag
         wlr-data-offer-resource
         wlr-data-offer-source
         wlr-data-offer-type
         wlr-data-offer-link
         wlr-data-offer-actions
         wlr-data-offer-preferred-action
         wlr-data-offer-in-ask?
         wlr-data-offer-source-destroy
         wlr-data-source-impl-send
         wlr-data-source-impl-accept
         wlr-data-source-impl-destroy
         wlr-data-source-impl-dnd-drop
         wlr-data-source-impl-dnd-finish
         wlr-data-source-impl-dnd-action
         wlr-data-source-impl
         wlr-data-source-mime-types
         wlr-data-source-actions
         wlr-data-source-accepted?
         wlr-data-source-current-dnd-action
         wlr-data-source-compositor-action
         wlr-data-source-events-destroy
         wlr-drag-icon-drag
         wlr-drag-icon-surface
         wlr-drag-icon-mapped
         wlr-drag-icon-events-map
         wlr-drag-icon-events-unmap
         wlr-drag-icon-events-destroy
         wlr-drag-icon-surface-destroy
         wlr-drag-icon-data
         wlr-drag-grab/keyboard
         wlr-drag-grab/keyboard-pointer
         wlr-drag-grab/keyboard-touch
         wlr-drag-grab-type
         wlr-drag-keyboard-grab
         wlr-drag-pointer-grab
         wlr-drag-touch-grab
         wlr-drag-seat
         wlr-drag-seat-client
         wlr-drag-focus-client
         wlr-drag-icon
         wlr-drag-focus
         wlr-drag-source
         wlr-drag-started?
         wlr-drag-dropped?
         wlr-drag-cancelling?
         wlr-drag-grab-touch-id
         wlr-drag-touch-id
         wlr-drag-events-focus
         wlr-drag-events-motion
         wlr-drag-events-drop
         wlr-drag-events-destroy
         wlr-drag-point-destroy
         wlr-drag-source-destroy
         wlr-drag-seat-client-destroy
         wlr-drag-icon-destroy
         wlr-drag-motion-event-drag
         wlr-drag-motion-event-time
         wlr-drag-motion-event-sx
         wlr-drag-motion-event-sy
         wlr-drag-drop-event-drag
         wlr-drag-drop-event-time

         wlr-data-device-manager-create
         wlr-data-device-manager-destroy
         wlr-seat-request-set-selection
         wlr-seat-set-selection
         wlr-drag-create
         wlr-seat-request-start-drag
         wlr-seat-start-drag
         wlr-seat-start-pointer-drag
         wlr-seat-start-touch-drag
         wlr-data-source-init
         wlr-data-source-send
         wlr-data-source-accept
         wlr-data-source-destroy
         wlr-data-source-dnd-drop
         wlr-data-source-dnd-finish
         wlr-data-source-dnd-action)
  (import (scheme)
          (chicken base))
  (include "ffi-helpers.scm")

  (bind-file "include/bind/wlr/types/wlr_data_device.h")
  (define wlr-data-offer-in-ask? wlr-data-offer-in-ask)
  (define wlr-data-source-accepted? wlr-data-source-accepted)
  (define wlr-drag-started? wlr-drag-started)
  (define wlr-drag-dropped? wlr-drag-dropped)
  (define wlr-drag-cancelling? wlr-drag-cancelling)

  (define-foreign-record-type (wlr-data-device-manager* "struct wlr_data_device_manager")
    ((struct "wl_list") resources wlr-data-device-manager-resources)
    ((struct "wl_list") data_sources wlr-data-device-manager-data-sources)
    ((struct "wl_listener") display_destroy wlr-data-device-manager-display-destroy)
    ((struct "wl_signal") events.destroy wlr-data-device-manager-events-destroy))

  (define-foreign-values wlr-data-offer-type
    (wlr-data-offer/selection "WLR_DATA_OFFER_SELECTION")
    (wlr-data-offer/drag      "WLR_DATA_OFFER_DRAG"))

  (define-foreign-record-type (wlr-data-offer* "struct wlr_data_offer")
    ((struct "wl_list") link wlr-data-offer-link)
    ((struct "wl_listener") source_destroy wlr-data-offer-source-destroy))

  (define-foreign-record-type (wlr-data-source-impl* "struct wlr_data_source_impl")
    ((function void ((c-pointer (struct "wlr_data_source")) (const c-string) int32))
      send wlr-data-source-impl-send)
    ((function void ((c-pointer (struct "wlr_data_source")) unsigned-int32 (const c-string)))
      accept wlr-data-source-impl-accept)
    ((function void ((c-pointer (struct "wlr_data_source"))))
      destroy wlr-data-source-impl-destroy)
    ((function void ((c-pointer (struct "wlr_data_source"))))
      dnd_drop wlr-data-source-impl-dnd-drop)
    ((function void ((c-pointer (struct "wlr_data_source"))))
      dnd_finish wlr-data-source-impl-dnd-finish)
    ((function void ((c-pointer (struct "wlr_data_source"))
                     (enum "wl_data_device_manager_dnd_action")))
      dnd_action wlr-data-source-impl-dnd-action))

  (define-foreign-record-type (wlr-data-source* "struct wlr_data_source")
    ((struct "wl_array") mime_types wlr-data-source-mime-types)
    ((struct "wl_signal") events.destroy wlr-data-source-events-destroy))

  (define-foreign-record-type (wlr-drag-icon* "struct wlr_drag_icon")
    ((struct "wl_signal") events.map wlr-drag-icon-events-map)
    ((struct "wl_signal") events.unmap wlr-drag-icon-events-unmap)
    ((struct "wl_signal") events.destroy wlr-drag-icon-events-destroy)
    ((struct "wl_listener") surface_destroy wlr-drag-icon-surface-destroy))

  (define-foreign-values wlr-drag-grab-type
    (wlr-drag-grab/keyboard "WLR_DRAG_GRAB_KEYBOARD")
    (wlr-drag-grab/keyboard-pointer "WLR_DRAG_GRAB_KEYBOARD_POINTER")
    (wlr-drag-grab/keyboard-touch "WLR_DRAG_GRAB_KEYBOARD_TOUCH"))

  (define-foreign-record-type (wlr-drag* "struct wlr_drag")
    ((struct "wlr_seat_pointer_grab") pointer_grab wlr-drag-pointer-grab)
    ((struct "wlr_seat_keyboard_grab") keyboard_grab wlr-drag-keyboard-grab)
    ((struct "wlr_seat_touch_grab") touch_grab wlr-drag-touch-grab)
    ((struct "wl_listener") point_destroy wlr-drag-point-destroy)
    ((struct "wl_listener") source_destroy wlr-drag-source-destroy)
    ((struct "wl_listener") seat_client_destroy wlr-drag-seat-client-destroy)
    ((struct "wl_listener") icon_destroy wlr-drag-icon-destroy)
    ((struct "wl_signal") events.focus wlr-drag-events-focus)
    ((struct "wl_signal") events.motion wlr-drag-events-motion)
    ((struct "wl_signal") events.drop wlr-drag-events-drop)
    ((struct "wl_signal") events.destroy wlr-drag-events-destroy)))
