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

(foreign-declare "#include <wlr/types/wlr_xdg_shell.h>")
(include "wlroots-types.scm")

#>
/* Defined in wlr/types/wlr-surface.scm */
void scheme_wlr_surface_iterator(struct wlr_surface*, int, int, void*);
<#

(module (wlr types wlr-xdg-shell)
        (wlr-xdg-shell-global
         wlr-xdg-shell-clients
         wlr-xdg-shell-popup-grabs
         wlr-xdg-shell-ping-timeout
         wlr-xdg-shell-display-destroy
         wlr-xdg-shell-events-new-surface
         wlr-xdg-shell-events-destroy
         wlr-xdg-shell-data

         wlr-xdg-client-shell
         wlr-xdg-client-resource
         wlr-xdg-client-client
         wlr-xdg-client-surfaces
         wlr-xdg-client-link
         wlr-xdg-client-ping-serial
         wlr-xdg-client-ping-timer

         wlr-xdg-positioner-resource
         wlr-xdg-positioner-anchor-rect
         wlr-xdg-positioner-anchor
         wlr-xdg-positioner-gravity
         wlr-xdg-positioner-constraint-adjustment
         wlr-xdg-positioner-size-width
         wlr-xdg-positioner-size-height
         wlr-xdg-positioner-offset-x
         wlr-xdg-positioner-offset-y

         wlr-xdg-popup-base
         wlr-xdg-popup-link
         wlr-xdg-popup-resource
         wlr-xdg-popup-committed?
         wlr-xdg-popup-parent
         wlr-xdg-popup-seat
         wlr-xdg-popup-geometry
         wlr-xdg-popup-positioner
         wlr-xdg-popup-grab-link

         wlr-xdg-popup-grab-client
         wlr-xdg-popup-grab-pointer-grab
         wlr-xdg-popup-grab-keyboard-grab
         wlr-xdg-popup-grab-seat
         wlr-xdg-popup-grab-popups
         wlr-xdg-popup-grab-link
         wlr-xdg-popup-grab-seat-destroy

         wlr-xdg-surface-role/none
         wlr-xdg-surface-role/toplevel
         wlr-xdg-surface-role/popup

         wlr-xdg-toplevel-state-maximized?
         wlr-xdg-toplevel-state-fullscreen?
         wlr-xdg-toplevel-state-resizing?
         wlr-xdg-toplevel-state-activated?
         wlr-xdg-toplevel-state-tiled
         wlr-xdg-toplevel-state-width
         wlr-xdg-toplevel-state-height
         wlr-xdg-toplevel-state-max-width
         wlr-xdg-toplevel-state-max-height
         wlr-xdg-toplevel-state-min-width
         wlr-xdg-toplevel-state-min-height

         wlr-xdg-toplevel-resource
         wlr-xdg-toplevel-base
         wlr-xdg-toplevel-parent
         wlr-xdg-toplevel-added?
         wlr-xdg-toplevel-client-pending
         wlr-xdg-toplevel-server-pending
         wlr-xdg-toplevel-current
         wlr-xdg-toplevel-title
         wlr-xdg-toplevel-app-id
         wlr-xdg-toplevel-events-request-maximize
         wlr-xdg-toplevel-events-request-fullscreen
         wlr-xdg-toplevel-events-request-minimize
         wlr-xdg-toplevel-events-request-move
         wlr-xdg-toplevel-events-request-resize
         wlr-xdg-toplevel-events-request-show-window-menu
         wlr-xdg-toplevel-events-set-parent
         wlr-xdg-toplevel-events-set-title
         wlr-xdg-toplevel-events-set-app-id

         wlr-xdg-surface-configure-surface
         wlr-xdg-surface-configure-link
         wlr-xdg-surface-configure-serial
         wlr-xdg-surface-configure-toplevel-state

         wlr-xdg-surface-client
         wlr-xdg-surface-resource
         wlr-xdg-surface-surface
         wlr-xdg-surface-link
         wlr-xdg-surface-role
         wlr-xdg-surface-toplevel
         wlr-xdg-surface-popup
         wlr-xdg-surface-popups
         wlr-xdg-surface-added?
         wlr-xdg-surface-configured?
         wlr-xdg-surface-mapped?
         wlr-xdg-surface-configure-serial
         wlr-xdg-surface-configure-idle
         wlr-xdg-surface-configure-next-serial
         wlr-xdg-surface-configure-list
         wlr-xdg-surface-has-next-geometry?
         wlr-xdg-surface-next-geometry
         wlr-xdg-surface-geometry
         wlr-xdg-surface-surface-destroy
         wlr-xdg-surface-surface-commit
         wlr-xdg-surface-events-destroy
         wlr-xdg-surface-events-ping-timeout
         wlr-xdg-surface-events-new-popup
         wlr-xdg-surface-events-map
         wlr-xdg-surface-events-unmap
         wlr-xdg-surface-events-configure
         wlr-xdg-surface-events-ack-configure
         wlr-xdg-surface-data

         wlr-xdg-toplevel-move-event-surface
         wlr-xdg-toplevel-move-event-seat
         wlr-xdg-toplevel-move-event-serial

         wlr-xdg-toplevel-resize-event-surface
         wlr-xdg-toplevel-resize-event-seat
         wlr-xdg-toplevel-resize-event-serial
         wlr-xdg-toplevel-resize-event-edges

         wlr-xdg-toplevel-set-fullscreen-event-surface
         wlr-xdg-toplevel-set-fullscreen-event-fullscreen?
         wlr-xdg-toplevel-set-fullscreen-event-output

         wlr-xdg-toplevel-show-window-menu-event-surface
         wlr-xdg-toplevel-show-window-menu-event-seat
         wlr-xdg-toplevel-show-window-menu-event-serial
         wlr-xdg-toplevel-show-window-menu-event-x
         wlr-xdg-toplevel-show-window-menu-event-y

         wlr-xdg-shell-create
         wlr-xdg-shell-destroy
         wlr-xdg-surface-from-resource
         wlr-xdg-surface-from-popup-resource
         wlr-xdg-surface-from-toplevel-resource
         wlr-xdg-surface-ping
         wlr-xdg-toplevel-set-size
         wlr-xdg-toplevel-set-activated
         wlr-xdg-toplevel-set-maximized
         wlr-xdg-toplevel-set-fullscreen
         wlr-xdg-toplevel-set-resizing
         wlr-xdg-toplevel-set-tiled
         wlr-xdg-toplevel-send-close
         wlr-xdg-popup-destroy
         wlr-xdg-positioner-get-geometry
         wlr-xdg-popup-get-anchor-point
         wlr-xdg-popup-get-toplevel-coords
         wlr-xdg-popup-unconstrain-from-box
         wlr-positioner-invert-x
         wlr-positioner-invert-y
         wlr-xdg-surface-surface-at
         wlr-surface-is-xdg-surface?
         wlr-xdg-surface-from-wlr-surface
         wlr-xdg-surface-get-geometry
         wlr-xdg-surface-for-each-surface
         wlr-xdg-surface-schedule-configure
         wlr-xdg-surface-for-each-popup)
  (import (scheme)
          (chicken base)
          (chicken foreign)
          (foreigners)
          (ffi-helpers)
          (bind)
          (wlr types wlr-box))

  (include "bind-options.scm")
  (bind-rename wlr_xdg_surface_get_geometry %wlr-xdg-surface-get-geometry)
  (bind-file "include/bind/wlr/types/wlr_xdg_shell.h")

  (define-foreign-record-type (wlr-xdg-shell* "struct wlr_xdg_shell")
    ((struct "wl_list") clients wlr-xdg-shell-clients)
    ((struct "wl_list") popup_grabs wlr-xdg-shell-popup-grabs)
    ((struct "wl_listener") display_destroy wlr-xdg-shell-display-destroy)
    ((struct "wl_signal") events.new_surface wlr-xdg-shell-events-new-surface)
    ((struct "wl_signal") events.destroy wlr-xdg-shell-events-destroy))

  (define-foreign-record-type (wlr-xdg-client* "struct wlr_xdg_client")
    ((struct "wl_list") surfaces wlr-xdg-client-surfaces)
    ((struct "wl_list") link wlr-xdg-client-link))

  (define-foreign-record-type (wlr-xdg-positioner* "struct wlr_xdg_positioner")
    ((struct "wlr_box") anchor_rect wlr-xdg-positioner-anchor-rect)
    (int32 size.width wlr-xdg-positioner-size-width)
    (int32 size.height wlr-xdg-positioner-size-height)
    (int32 offset.x wlr-xdg-positioner-offset-x)
    (int32 offset.y wlr-xdg-positioner-offset-y))

  (define-foreign-record-type (wlr-xdg-popup* "struct wlr_xdg_popup")
    ((struct "wl_list") link wlr-xdg-popup-link)
    ((struct "wlr_box") geometry wlr-xdg-popup-geometry)
    ((struct "wlr_xdg_positioner") positioner wlr-xdg-popup-positioner)
    ((struct "wl_list") grab_link wlr-xdg-popup-grab-link))

  (define-foreign-record-type (wlr-xdg-popup-grab* "struct wlr_xdg_popup_grab")
    ((struct "wlr_seat_pointer_grab") pointer_grab wlr-xdg-popup-grab-pointer-grab)
    ((struct "wlr_seat_keyboard_grab") keyboard_grab wlr-xdg-popup-grab-keyboard-grab)
    ((struct "wl_list") popups wlr-xdg-popup-grab-popups)
    ((struct "wl_list") link wlr-xdg-popup-grab-link)
    ((struct "wl_listener") seat_destroy wlr-xdg-popup-grab-seat-destroy))

  (define-foreign-values wlr-xdg-surface-role
    (wlr-xdg-surface-role/none     "WLR_XDG_SURFACE_ROLE_NONE")
    (wlr-xdg-surface-role/toplevel "WLR_XDG_SURFACE_ROLE_TOPLEVEL")
    (wlr-xdg-surface-role/popup    "WLR_XDG_SURFACE_ROLE_POPUP"))

  (define-foreign-record-type (wlr-xdg-toplevel* "struct wlr_xdg_toplevel")
    ((struct "wlr_xdg_toplevel_state") client_pending wlr-xdg-toplevel-client-pending)
    ((struct "wlr_xdg_toplevel_state") server_pending wlr-xdg-toplevel-server-pending)
    ((struct "wlr_xdg_toplevel_state") current wlr-xdg-toplevel-current)
    ((struct "wl_signal") events.request_maximize wlr-xdg-toplevel-events-request-maximize)
    ((struct "wl_signal") events.request_fullscreen wlr-xdg-toplevel-events-request-fullscreen)
    ((struct "wl_signal") events.request_minimize wlr-xdg-toplevel-events-request-minimize)
    ((struct "wl_signal") events.request_move wlr-xdg-toplevel-events-request-move)
    ((struct "wl_signal") events.request_resize wlr-xdg-toplevel-events-request-resize)
    ((struct "wl_signal") events.request_show_window_menu
                          wlr-xdg-toplevel-events-request-show-window-menu)
    ((struct "wl_signal") events.set_parent wlr-xdg-toplevel-events-set-parent)
    ((struct "wl_signal") events.set_title wlr-xdg-toplevel-events-set-title)
    ((struct "wl_signal") events.set_app_id wlr-xdg-toplevel-events-set-app-id))

  (define-foreign-record-type (wlr-xdg-surface-configure* "struct wlr_xdg_surface_configure")
    ((struct "wl_list") link wlr-xdg-surface-configure-link))

  (define-foreign-record-type (wlr-xdg-surface* "struct wlr_xdg_surface")
    ((struct "wl_list") link wlr-xdg-surface-link)
    ((c-pointer (struct "wlr_xdg_toplevel")) toplevel wlr-xdg-surface-toplevel)
    ((c-pointer (struct "wlr_xdg_popup")) popup wlr-xdg-surface-popup)
    ((struct "wl_list") popups wlr-xdg-surface-popups)
    ((struct "wl_list") configure_list wlr-xdg-surface-configure-list)
    ((struct "wlr_box") next_geometry wlr-xdg-surface-next-geometry)
    ((struct "wlr_box") geometry wlr-xdg-surface-geometry)
    ((struct "wl_listener") surface_destroy wlr-xdg-surface-surface-destroy)
    ((struct "wl_listener") surface_commit wlr-xdg-surface-surface-commit)
    ((struct "wl_signal") events.destroy wlr-xdg-surface-events-destroy)
    ((struct "wl_signal") events.ping_timeout wlr-xdg-surface-events-ping-timeout)
    ((struct "wl_signal") events.new_popup wlr-xdg-surface-events-new-popup)
    ((struct "wl_signal") events.map wlr-xdg-surface-events-map)
    ((struct "wl_signal") events.unmap wlr-xdg-surface-events-unmap)
    ((struct "wl_signal") events.configure wlr-xdg-surface-events-configure)
    ((struct "wl_signal") events.ack_configure wlr-xdg-surface-events-ack-configure))

  (define %wlr-xdg-positioner-get-geometry
    (foreign-lambda* void ((wlr-xdg-positioner* positioner) (wlr-box* box))
      "*box = wlr_xdg_positioner_get_geometry(positioner);"))

  (define (wlr-xdg-positioner-get-geometry positioner)
    (let ((box (make-wlr-box 0 0 0 0)))
      (%wlr-xdg-positioner-get-geometry positioner box)
      box))

  (define (wlr-xdg-surface-get-geometry xdg-surface)
    (let ((box (make-wlr-box 0 0 0 0)))
      (%wlr-xdg-surface-get-geometry xdg-surface box)
      box))

  (define wlr-xdg-surface-for-each-surface
    (foreign-safe-lambda* void ((wlr-xdg-surface* surface) (scheme-object callback))
      "void *root = CHICKEN_new_gc_root();"
      "CHICKEN_gc_root_set(root, callback);"
      "wlr_xdg_surface_for_each_surface(surface, scheme_wlr_surface_iterator, root);"
      "CHICKEN_delete_gc_root(root);"))

  (define wlr-xdg-surface-for-each-popup
    (foreign-safe-lambda* void ((wlr-xdg-surface* surface) (scheme-object callback))
      "void *root = CHICKEN_new_gc_root();"
      "CHICKEN_gc_root_set(root, callback);"
      "wlr_xdg_surface_for_each_popup(surface, scheme_wlr_surface_iterator, root);"
      "CHICKEN_delete_gc_root(root);"))

  (define wlr-xdg-popup-committed? wlr-xdg-popup-committed)
  (define wlr-xdg-toplevel-state-maximized? wlr-xdg-toplevel-state-maximized)
  (define wlr-xdg-toplevel-state-fullscreen? wlr-xdg-toplevel-state-fullscreen)
  (define wlr-xdg-toplevel-state-resizing? wlr-xdg-toplevel-state-resizing)
  (define wlr-xdg-toplevel-state-activated? wlr-xdg-toplevel-state-activated)
  (define wlr-xdg-toplevel-added? wlr-xdg-toplevel-added)
  (define wlr-xdg-surface-added? wlr-xdg-surface-added)
  (define wlr-xdg-surface-configured? wlr-xdg-surface-configured)
  (define wlr-xdg-surface-mapped? wlr-xdg-surface-mapped)
  (define wlr-xdg-surface-has-next-geometry? wlr-xdg-surface-has-next-geometry)
  (define wlr-xdg-toplevel-set-fullscreen-event-fullscreen?
          wlr-xdg-toplevel-set-fullscreen-event-fullscreen)
  (define wlr-surface-is-xdg-surface? wlr-surface-is-xdg-surface))
