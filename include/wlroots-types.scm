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

(include "wayland-types.scm")

(define-foreign-type pixman-box32 (struct "pixman_box32"))
(define-foreign-type pixman-box32* (c-pointer pixman-box32))

(define-foreign-type pixman-region32 (struct "pixman_region32"))
(define-foreign-type pixman-region32* (c-pointer pixman-region32))

(define-foreign-type wlr-backend (struct "wlr_backend"))
(define-foreign-type wlr-backend* (c-pointer wlr-backend))

(define-foreign-type wlr-box (struct "wlr_box"))
(define-foreign-type wlr-box* (c-pointer wlr-box))

(define-foreign-type wlr-client (struct "wlr_client"))
(define-foreign-type wlr-client* (c-pointer wlr-client))

(define-foreign-type wlr-compositor (struct "wlr_compositor"))
(define-foreign-type wlr-compositor* (c-pointer wlr-compositor))

(define-foreign-type wlr-cursor (struct "wlr_cursor"))
(define-foreign-type wlr-cursor* (c-pointer wlr-cursor))

(define-foreign-type wlr-data-device-manager (struct "wlr_data_device_manager"))
(define-foreign-type wlr-data-device-manager* (c-pointer wlr-data-device-manager))

(define-foreign-type wlr-data-source (struct "wlr_data_source"))
(define-foreign-type wlr-data-source* (c-pointer wlr-data-source))

(define-foreign-type wlr-data-source-impl (struct "wlr_data_source_impl"))
(define-foreign-type wlr-data-source-impl* (c-pointer wlr-data-source-impl))

(define-foreign-type wlr-dmabuf-attributes (struct "wlr_dmabuf_attributes"))
(define-foreign-type wlr-dmabuf-attributes* (c-pointer wlr-dmabuf-attributes))

(define-foreign-type wlr-egl (struct "wlr_egl"))
(define-foreign-type wlr-egl* (c-pointer wlr-egl))

(define-foreign-type wlr-event-pointer-axis (struct "wlr_event_pointer_axis"))
(define-foreign-type wlr-event-pointer-axis* (c-pointer wlr-event-pointer-axis))

(define-foreign-type wlr-event-pointer-button (struct "wlr_event_pointer_button"))
(define-foreign-type wlr-event-pointer-button* (c-pointer wlr-event-pointer-button))

(define-foreign-type wlr-event-pointer-motion (struct "wlr_event_pointer_motion"))
(define-foreign-type wlr-event-pointer-motion* (c-pointer wlr-event-pointer-motion))

(define-foreign-type wlr-event-pointer-motion-absolute
                     (struct "wlr_event_pointer_motion_absolute"))
(define-foreign-type wlr-event-pointer-motion-absolute*
                     (c-pointer wlr-event-pointer-motion-absolute))

(define-foreign-type wlr-event-pointer-pinch-begin (struct "wlr_event_pointer_pinch_begin"))
(define-foreign-type wlr-event-pointer-pinch-begin* (c-pointer wlr-event-pointer-pinch-begin))

(define-foreign-type wlr-event-pointer-pinch-end (struct "wlr_event_pointer_pinch_end"))
(define-foreign-type wlr-event-pointer-pinch-end* (c-pointer wlr-event-pointer-pinch-end))

(define-foreign-type wlr-event-pointer-pinch-update (struct "wlr_event_pointer_pinch_update"))
(define-foreign-type wlr-event-pointer-pinch-update* (c-pointer wlr-event-pointer-pinch-update))

(define-foreign-type wlr-event-pointer-swipe-begin (struct "wlr_event_pointer_swipe_begin"))
(define-foreign-type wlr-event-pointer-swipe-begin* (c-pointer wlr-event-pointer-swipe-begin))

(define-foreign-type wlr-event-pointer-swipe-end (struct "wlr_event_pointer_swipe_end"))
(define-foreign-type wlr-event-pointer-swipe-end* (c-pointer wlr-event-pointer-swipe-end))

(define-foreign-type wlr-event-pointer-swipe-update (struct "wlr_event_pointer_swipe_update"))
(define-foreign-type wlr-event-pointer-swipe-update* (c-pointer wlr-event-pointer-swipe-update))

(define-foreign-type wlr-input-device (struct "wlr_input_device"))
(define-foreign-type wlr-input-device* (c-pointer wlr-input-device))

(define-foreign-type wlr-keyboard (struct "wlr_keyboard"))
(define-foreign-type wlr-keyboard* (c-pointer wlr-keyboard))

(define-foreign-type wlr-keyboard-grab (struct "wlr_keyboard_grab"))
(define-foreign-type wlr-keyboard-grab* (c-pointer wlr-keyboard-grab))

(define-foreign-type wlr-keyboard-grab-interface (struct "wlr_keyboard_grab_interface"))
(define-foreign-type wlr-keyboard-grab-interface* (c-pointer wlr-keyboard-grab-interface))

(define-foreign-type wlr-keyboard-modifiers (struct "wlr_keyboard_modifiers"))
(define-foreign-type wlr-keyboard-modifiers* (c-pointer wlr-keyboard-modifiers))

(define-foreign-type wlr-keyboard-state (struct "wlr_keyboard_state"))
(define-foreign-type wlr-keyboard-state* (c-pointer wlr-keyboard-state))

(define-foreign-type wlr-output (struct "wlr_output"))
(define-foreign-type wlr-output* (c-pointer wlr-output))

(define-foreign-type wlr-output-cursor (struct "wlr_output_cursor"))
(define-foreign-type wlr-output-cursor* (c-pointer wlr-output-cursor))

(define-foreign-type wlr-output-layout (struct "wlr_output_layout"))
(define-foreign-type wlr-output-layout* (c-pointer wlr-output-layout))

(define-foreign-type wlr-output-layout-output (struct "wlr_output_layout_output"))
(define-foreign-type wlr-output-layout-output* (c-pointer wlr-output-layout-output))

(define-foreign-type wlr-output-mode (struct "wlr_output_mode"))
(define-foreign-type wlr-output-mode* (c-pointer wlr-output-mode))

(define-foreign-type wlr-pointer (struct "wlr_pointer"))
(define-foreign-type wlr-pointer* (c-pointer wlr-pointer))

(define-foreign-type wlr-pointer-grab (struct "wlr_pointer_grab"))
(define-foreign-type wlr-pointer-grab* (c-pointer wlr-pointer-grab))

(define-foreign-type wlr-pointer-grab-interface (struct "wlr_pointer_grab_interface"))
(define-foreign-type wlr-pointer-grab-interface* (c-pointer wlr-pointer-grab-interface))

(define-foreign-type wlr-pointer-state (struct "wlr_pointer_state"))
(define-foreign-type wlr-pointer-state* (c-pointer wlr-pointer-state))

(define-foreign-type wlr-renderer (struct "wlr_renderer"))
(define-foreign-type wlr-renderer* (c-pointer wlr-renderer))

(define-foreign-type wlr-seat (struct "wlr_seat"))
(define-foreign-type wlr-seat* (c-pointer wlr-seat))

(define-foreign-type wlr-seat-client (struct "wlr_seat_client"))
(define-foreign-type wlr-seat-client* (c-pointer wlr-seat-client))

(define-foreign-type wlr-seat-keyboard-focus-change-event
                     (struct "wlr_seat_keyboard_focus_change_event"))
(define-foreign-type wlr-seat-keyboard-focus-change-event*
                     (c-pointer wlr-seat-keyboard-focus-change-event))

(define-foreign-type wlr-seat-pointer-focus-change-event
                     (struct "wlr_seat_pointer_focus_change_event"))
(define-foreign-type wlr-seat-pointer-focus-change-event*
                     (c-pointer wlr-seat-pointer-focus-change-event))

(define-foreign-type wlr-seat-pointer-request-set-cursor-event
                     (struct "wlr_seat_pointer_request_set_cursor_event"))
(define-foreign-type wlr-seat-pointer-request-set-cursor-event*
                     (c-pointer wlr-seat-pointer-request-set-cursor-event))

(define-foreign-type wlr-seat-request-set-primary-selection-event
                     (struct "wlr_seat_request_set_primary_selection_event"))
(define-foreign-type wlr-seat-request-set-primary-selection-event*
                     (c-pointer wlr-seat-request-set-primary-selection-event))

(define-foreign-type wlr-seat (struct "wlr_seat"))
(define-foreign-type wlr-seat* (c-pointer wlr-seat))

(define-foreign-type wlr-session (struct "wlr_session"))
(define-foreign-type wlr-session* (c-pointer wlr-session))

(define-foreign-type wlr-surface (struct "wlr_surface"))
(define-foreign-type wlr-surface* (c-pointer wlr-surface))

(define-foreign-type wlr-surface-role (struct "wlr_surface_role"))
(define-foreign-type wlr-surface-role* (c-pointer wlr-surface-role))

(define-foreign-type wlr-subsurface (struct "wlr_subsurface"))
(define-foreign-type wlr-subsurface* (c-pointer wlr-subsurface))

(define-foreign-type wlr-texture (struct "wlr_texture"))
(define-foreign-type wlr-texture* (c-pointer wlr-texture))

(define-foreign-type wlr-touch-grab (struct "wlr_touch_grab"))
(define-foreign-type wlr-touch-grab* (c-pointer wlr-touch-grab))

(define-foreign-type wlr-touch-grab-interface (struct "wlr_touch_grab_interface"))
(define-foreign-type wlr-touch-grab-interface* (c-pointer wlr-touch-grab-interface))

(define-foreign-type wlr-touch-point (struct "wlr_touch_point"))
(define-foreign-type wlr-touch-point* (c-pointer wlr-touch-point))

(define-foreign-type wlr-touch-state (struct "wlr_touch_state"))
(define-foreign-type wlr-touch-state* (c-pointer wlr-touch-state))

(define-foreign-type wlr-xcursor (struct "wlr_xcursor"))
(define-foreign-type wlr-xcursor* (c-pointer wlr-xcursor))

(define-foreign-type wlr-xcursor-manager (struct "wlr_xcursor_manager"))
(define-foreign-type wlr-xcursor-manager* (c-pointer wlr-xcursor-manager))

(define-foreign-type wlr-xcursor-manager-theme (struct "wlr_xcursor_manager_theme"))
(define-foreign-type wlr-xcursor-manager-theme* (c-pointer wlr-xcursor-manager-theme))

(define-foreign-type wlr-xdg-client (struct "wlr_xdg_client"))
(define-foreign-type wlr-xdg-client* (c-pointer wlr-xdg-client))

(define-foreign-type wlr-xdg-popup (struct "wlr_xdg_popup"))
(define-foreign-type wlr-xdg-popup* (c-pointer wlr-xdg-popup))

(define-foreign-type wlr-xdg-popup-grab (struct "wlr_xdg_popup_grab"))
(define-foreign-type wlr-xdg-popup-grab* (c-pointer wlr-xdg-popup-grab))

(define-foreign-type wlr-xdg-positioner (struct "wlr_xdg_positioner"))
(define-foreign-type wlr-xdg-positioner* (c-pointer wlr-xdg-positioner))

(define-foreign-type wlr-xdg-shell (struct "wlr_xdg_shell"))
(define-foreign-type wlr-xdg-shell* (c-pointer wlr-xdg-shell))

(define-foreign-type wlr-xdg-surface (struct "wlr_xdg_surface"))
(define-foreign-type wlr-xdg-surface* (c-pointer wlr-xdg-surface))

(define-foreign-type wlr-xdg-surface-configure (struct "wlr_xdg_surface_configure"))
(define-foreign-type wlr-xdg-surface-configure* (c-pointer wlr-xdg-surface-configure))

(define-foreign-type wlr-xdg-toplevel (struct "wlr_xdg_toplevel"))
(define-foreign-type wlr-xdg-toplevel* (c-pointer wlr-xdg-toplevel))

(define-foreign-type wlr-xdg-toplevel-move-event (struct "wlr_xdg_toplevel_move_event"))
(define-foreign-type wlr-xdg-toplevel-move-event* (c-pointer wlr-xdg-toplevel-move-event))

(define-foreign-type wlr-xdg-toplevel-resize-event (struct "wlr_xdg_toplevel_resize_event"))
(define-foreign-type wlr-xdg-toplevel-resize-event* (c-pointer wlr-xdg-toplevel-resize-event))

(define-foreign-type wlr-xdg-toplevel-set-fullscreen-event
                     (struct "wlr_xdg_toplevel_set_fullscreen_event"))
(define-foreign-type wlr-xdg-toplevel-set-fullscreen-event*
                     (c-pointer wlr-xdg-toplevel-set-fullscreen-event))

(define-foreign-type wlr-xdg-toplevel-show-window-menu-event
                     (struct "wlr_xdg_toplevel_show_window_menu_event"))
(define-foreign-type wlr-xdg-toplevel-show-window-menu-evnet*
                     (c-pointer wlr-xdg-toplevel-show-window-menu-event))

(define-foreign-type wlr-xdg-toplevel-state (struct "wlr_xdg_toplevel_state"))
(define-foreign-type wlr-xdg-toplevel-state* (c-pointer wlr-xdg-toplevel-state))

(define-foreign-type wlr-axis-orientation (enum "wlr_axis_orientation"))
(define-foreign-type wlr-axis-source (enum "wlr_axis_source"))
(define-foreign-type wlr-button-state (enum "wlr_button_state"))
(define-foreign-type wlr-data-offer-type (enum "wlr_data_offer_type"))
(define-foreign-type wlr-direction (enum "wlr_direction"))
(define-foreign-type wlr-drag-grab-type (enum "wlr_drag_grab_type"))
(define-foreign-type wlr-edges (enum "wlr_edges"))
(define-foreign-type wlr-input-device-type (enum "wlr_input_device_type"))
(define-foreign-type wlr-key-state (enum "wlr_key_state"))
(define-foreign-type wlr-keyboard-led (enum "wlr_keyboard_led"))
(define-foreign-type wlr-keyboard-modifier (enum "wlr_keyboard_modifier"))
(define-foreign-type wlr-output-present-flag (enum "wlr_output_present_flag"))
(define-foreign-type wlr-renderer-read-pixels-flags (enum "wlr_renderer_read_pixels_flags"))
(define-foreign-type wlr-surface-state-field (enum "wlr_surface_state_field"))
(define-foreign-type wlr-tablet-tool-axes (enum "wlr_tablet_tool_axes"))
(define-foreign-type wlr-tablet-tool-proximity-state (enum "wlr_tablet_tool_proximity_state"))
(define-foreign-type wlr-tablet-tool-tip-state (enum "wlr_tablet_tool_tip_state"))
(define-foreign-type wlr-tablet-tool-type (enum "wlr_tablet_tool_type"))
(define-foreign-type wlr-xdg-surface-role (enum "wlr_xdg_surface_role"))

(define-foreign-type wlr-matrix (c-pointer float)) ; float[9]
(define-foreign-type wlr-rgba (c-pointer float))   ; float[4]
