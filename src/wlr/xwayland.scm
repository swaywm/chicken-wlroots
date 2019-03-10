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

; wlroots may be compiled without xwayland, so we need to allow disabling it.
(cond-expand
(xwayland

(foreign-declare "#include <wlr/xwayland.h>")
(include "wlroots-types.scm")

(module (wlr xwayland)
        (; struct wlr_xwayland
         wlr-xwayland-pid
         wlr-xwayland-client
         wlr-xwayland-sigusr1-source
         wlr-xwayland-xwm
         wlr-xwayland-cursor
         wlr-xwayland-wm-fd
         wlr-xwayland-wl-fd
         wlr-xwayland-server-start
         wlr-xwayland-display
         wlr-xwayland-display-name
         wlr-xwayland-x-fd
         wlr-xwayland-x-fd-read-event
         wlr-xwayland-lazy?
         wlr-xwayland-wl-display
         wlr-xwayland-compositor
         wlr-xwayland-seat
         wlr-xwayland-events-ready
         wlr-xwayland-events-new-surface
         wlr-xwayland-client-destroy
         wlr-xwayland-display-destroy
         wlr-xwayland-seat-destroy
         ;wlr-xwayland-user-event-handler ;FIXME: scheme interface
         wlr-xwayland-data
         ; enum wlr_xwayland_surface_decorations
         wlr-xwayland-surface-decorations/all
         wlr-xwayland-surface-decorations/no-border
         wlr-xwayland-surface-decorations/no-title
         ; struct wlr_xwayland_surface_hints
         wlr-xwayland-surface-hints-flags
         wlr-xwayland-surface-hints-input
         wlr-xwayland-surface-hints-initial-state
         wlr-xwayland-surface-hints-icon-pixmap
         wlr-xwayland-surface-hints-icon-window
         wlr-xwayland-surface-hints-icon-x
         wlr-xwayland-surface-hints-icon-y
         wlr-xwayland-surface-hints-icon-mask
         wlr-xwayland-surface-hints-window-group
         ; struct wlr_xwayland_surface_size_hints
         wlr-xwayland-surface-size-hints-flags
         wlr-xwayland-surface-size-hints-x
         wlr-xwayland-surface-size-hints-y
         wlr-xwayland-surface-size-hints-width
         wlr-xwayland-surface-size-hints-height
         wlr-xwayland-surface-size-hints-min-width
         wlr-xwayland-surface-size-hints-min-height
         wlr-xwayland-surface-size-hints-max-width
         wlr-xwayland-surface-size-hints-max-height
         wlr-xwayland-surface-size-hints-width-inc
         wlr-xwayland-surface-size-hints-height-inc
         wlr-xwayland-surface-size-hints-base-width
         wlr-xwayland-surface-size-hints-base-height
         wlr-xwayland-surface-size-hints-min-aspect-num
         wlr-xwayland-surface-size-hints-min-aspect-den
         wlr-xwayland-surface-size-hints-max-aspect-num
         wlr-xwayland-surface-size-hints-max-aspect-den
         wlr-xwayland-surface-size-hints-win-gravity
         ; struct wlr_xwayland_surface
         wlr-xwayland-surface-window-id
         wlr-xwayland-surface-xwm
         wlr-xwayland-surface-surface-id
         wlr-xwayland-surface-link
         wlr-xwayland-surface-unpaired-link
         wlr-xwayland-surface-surface
         wlr-xwayland-surface-x
         wlr-xwayland-surface-y
         wlr-xwayland-surface-width
         wlr-xwayland-surface-height
         wlr-xwayland-surface-saved-width
         wlr-xwayland-surface-saved-height
         wlr-xwayland-surface-override-redirect
         wlr-xwayland-surface-mapped
         wlr-xwayland-surface-title
         wlr-xwayland-surface-class
         wlr-xwayland-surface-instance
         wlr-xwayland-surface-role
         wlr-xwayland-surface-pid
         wlr-xwayland-surface-has-utf8-title
         wlr-xwayland-surface-children
         wlr-xwayland-surface-parent
         wlr-xwayland-surface-parent-link
         wlr-xwayland-surface-window-type
         wlr-xwayland-surface-window-type-len
         wlr-xwayland-surface-protocols
         wlr-xwayland-surface-protocols-len
         wlr-xwayland-surface-decorations
         wlr-xwayland-surface-hints
         wlr-xwayland-surface-hints-urgency
         wlr-xwayland-surface-size-hints
         wlr-xwayland-surface-pinging?
         wlr-xwayland-surface-ping-timer
         wlr-xwayland-surface-modal?
         wlr-xwayland-surface-fullscreen?
         wlr-xwayland-surface-maximized-vert?
         wlr-xwayland-surface-maximized-horz?
         wlr-xwayland-surface-has-alpha?
         wlr-xwayland-surface-events-destroy
         wlr-xwayland-surface-events-request-configure
         wlr-xwayland-surface-events-request-move
         wlr-xwayland-surface-events-request-resize
         wlr-xwayland-surface-events-request-maximize
         wlr-xwayland-surface-events-request-fullscreen
         wlr-xwayland-surface-events-request-activate
         wlr-xwayland-surface-events-map
         wlr-xwayland-surface-events-unmap
         wlr-xwayland-surface-events-set-title
         wlr-xwayland-surface-events-set-class
         wlr-xwayland-surface-events-set-role
         wlr-xwayland-surface-events-set-parent
         wlr-xwayland-surface-events-set-pid
         wlr-xwayland-surface-events-set-window-type
         wlr-xwayland-surface-events-set-hints
         wlr-xwayland-surface-events-set-decorations
         wlr-xwayland-surface-events-set-override-redirect
         wlr-xwayland-surface-events-ping-timeout
         wlr-xwayland-surface-surface-destroy
         wlr-xwayland-surface-data
         ; struct wlr_xwayland_surface_configure_event
         wlr-xwayland-surface-configure-event-surface
         wlr-xwayland-surface-configure-event-x
         wlr-xwayland-surface-configure-event-y
         wlr-xwayland-surface-configure-event-width
         wlr-xwayland-surface-configure-event-height
         ; struct wlr_xwayland_move_event
         wlr-xwayland-move-event-surface
         wlr-xwayland-resize-event-surface
         wlr-xwayland-resize-event-edges
         ; functions
         wlr-xwayland-create
         wlr-xwayland-destroy
         wlr-xwayland-set-cursor
         wlr-xwayland-surface-activate
         wlr-xwayland-surface-configure
         wlr-xwayland-surface-close
         wlr-xwayland-surface-set-maximized
         wlr-xwayland-surface-set-fullscreen
         wlr-xwayland-set-seat
         wlr-surface-is-xwayland-surface?
         wlr-xwayland-surface-from-wlr-surface
         wlr-xwayland-surface-ping
         wlr-xwayland-or-surface-wants-focus?)
  (import (scheme)
          (chicken base))
  (include "ffi-helpers.scm")

  (bind-rename wlr_surface_is_xwayland_surface wlr-surface-is-xwayland-surface?)
  (bind-rename wlr_xwayland_or_surface_wants_focus wlr-xwayland-or-surface-wants-focus?)
  (bind-file "include/bind/wlr/xwayland.h")
  (define wlr-xwayland-lazy? wlr-xwayland-lazy)
  (define wlr-xwayland-surface-pinging? wlr-xwayland-surface-pinging)
  (define wlr-xwayland-surface-modal? wlr-xwayland-surface-modal)
  (define wlr-xwayland-surface-fullscreen? wlr-xwayland-surface-fullscreen)
  (define wlr-xwayland-surface-maximized-vert? wlr-xwayland-surface-maximized-vert)
  (define wlr-xwayland-surface-maximized-horz? wlr-xwayland-surface-maximized-horz)
  (define wlr-xwayland-surface-has-alpha? wlr-xwayland-surface-has-alpha)

  (define-foreign-record-type (wlr-xwayland* "struct wlr_xwayland")
    (c-string display_name wlr-xwayland-display-name)
    ((struct "wl_signal") events.ready wlr-xwayland-events-ready)
    ((struct "wl_signal") events.new_surface wlr-xwayland-events-new-surface)
    ((struct "wl_listener") client_destroy wlr-xwayland-client-destroy)
    ((struct "wl_listener") display_destroy wlr-xwayland-display-destroy)
    ((struct "wl_listener") seat_destroy wlr-xwayland-seat-destroy))

  ; XXX: unsafe
  (define wlr-xwayland-wm-fd
    (foreign-lambda* int ((wlr-xwayland* obj) (int i))
      "C_return(obj->wm_fd[i]);"))

  ; XXX: unsafe
  (define wlr-xwayland-wl-fd
    (foreign-lambda* int ((wlr-xwayland* obj) (int i))
      "C_return(obj->wl_fd[i]);"))

  ; XXX: unsafe
  (define wlr-xwayland-x-fd
    (foreign-lambda* int ((wlr-xwayland* obj) (int i))
      "C_return(obj->x_fd[i]);"))

  ; XXX: unsafe
  (define wlr-xwayland-x-fd-read-event
    (foreign-lambda* wl-event-source* ((wlr-xwayland* obj) (int i))
      "C_return(obj->x_fd_read_event[i]);"))

  (define-foreign-values wlr-xwayland-surface-decorations
    (wlr-xwayland-surface-decorations/all       "WLR_XWAYLAND_SURFACE_DECORATIONS_ALL")
    (wlr-xwayland-surface-decorations/no-border "WLR_XWAYLAND_SURFACE_DECORATIONS_NO_BORDER")
    (wlr-xwayland-surface-decorations/no-title  "WLR_XWAYLAND_SURFACE_DECORATIONS_NO_TITLE"))

  (define-foreign-record-type (wlr-xwayland-surface* "struct wlr_xwayland_surface")
    ((struct "wl_list") link wlr-xwayland-surface-link)
    ((struct "wl_list") unpaired_link wlr-xwayland-surface-unpaired-link)
    ((struct "wl_list") children wlr-xwayland-surface-children)
    ((struct "wl_list") parent_link wlr-xwayland-surface-parent-link)
    ((struct "wl_signal") events.destroy wlr-xwayland-surface-events-destroy)
    ((struct "wl_signal") events.request_configure
                          wlr-xwayland-surface-events-request-configure)
    ((struct "wl_signal") events.request_move wlr-xwayland-surface-events-request-move)
    ((struct "wl_signal") events.request_resize wlr-xwayland-surface-events-request-resize)
    ((struct "wl_signal") events.request_maximize wlr-xwayland-surface-events-request-maximize)
    ((struct "wl_signal") events.request_fullscreen
                          wlr-xwayland-surface-events-request-fullscreen)
    ((struct "wl_signal") events.request_activate wlr-xwayland-surface-events-request-activate)
    ((struct "wl_signal") events.map wlr-xwayland-surface-events-map)
    ((struct "wl_signal") events.unmap wlr-xwayland-surface-events-unmap)
    ((struct "wl_signal") events.set_title wlr-xwayland-surface-events-set-title)
    ((struct "wl_signal") events.set_class wlr-xwayland-surface-events-set-class)
    ((struct "wl_signal") events.set_role wlr-xwayland-surface-events-set-role)
    ((struct "wl_signal") events.set_parent wlr-xwayland-surface-events-set-parent)
    ((struct "wl_signal") events.set_pid wlr-xwayland-surface-events-set-pid)
    ((struct "wl_signal") events.set_window_type wlr-xwayland-surface-events-set-window-type)
    ((struct "wl_signal") events.set_hints wlr-xwayland-surface-events-set-hints)
    ((struct "wl_signal") events.set_decorations wlr-xwayland-surface-events-set-decorations)
    ((struct "wl_signal") events.set_override_redirect
                          wlr-xwayland-surface-events-set-override-redirect)
    ((struct "wl_signal") events.ping_timeout wlr-xwayland-surface-events-ping-timeout)
    ((struct "wl_listener") surface_destroy wlr-xwayland-surface-surface-destroy))

  ; XXX: bind parses as C++, so it thinks "class" as a member name is a syntax error.
  (define wlr-xwayland-surface-class
    (getter-with-setter
      (foreign-lambda* c-string ((wlr-xwayland-surface* surface))
        "C_return(surface->class);")
      ; XXX: will GC break this?
      (foreign-lambda* void ((wlr-xwayland-surface* surface) (c-string str))
        "surface->class = str;")))))

(else (module (wlr xwayland) ())))
