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

(foreign-declare "#include <wlr/types/wlr_output.h>")
(include "wlroots-types.scm")

(module (wlr types wlr-output)
        (wlr-output-mode-width
         wlr-output-mode-refresh
         wlr-output-mode-link

         wlr-output-cursor-output
         wlr-output-cursor-x
         wlr-output-cursor-y
         wlr-output-cursor-enabled?
         wlr-output-cursor-visible?
         wlr-output-cursor-width
         wlr-output-cursor-height
         wlr-output-cursor-hotspot-x
         wlr-output-cursor-hotspot-y
         wlr-output-cursor-link
         wlr-output-cursor-texture
         wlr-output-cursor-surface
         wlr-output-cursor-surface-commit
         wlr-output-cursor-surface-destroy
         wlr-output-cursor-events-destroy

         wlr-output-adaptive-sync/disabled
         wlr-output-adaptive-sync/enabled
         wlr-output-adaptive-sync/unknown

         wlr-output-state/buffer
         wlr-output-state/damage
         wlr-output-state/mode
         wlr-output-state/enabled
         wlr-output-state/scale
         wlr-output-state/transform
         wlr-output-state/adaptive-sync-enabled
         wlr-output-state/gamma-lut
         
         wlr-output-state-buffer/render
         wlr-output-state-buffer/scanout
         
         wlr-output-state-mode/fixed
         wlr-output-state-mode/custom

         wlr-output-state-committed
         wlr-output-state-damage
         wlr-output-state-enabled?
         wlr-output-state-scale
         wlr-output-state-transform
         wlr-output-state-adaptive-sync-enabled?
         wlr-output-state-buffer-type
         wlr-output-state-buffer
         wlr-output-state-mode-type
         wlr-output-state-mode
         wlr-output-state-custom-mode-width
         wlr-output-state-custom-mode-height
         wlr-output-state-custom-mode-refresh
         wlr-output-state-gamma-lut
         wlr-output-state-gamma-lut-size

         wlr-output-impl
         wlr-output-backend
         wlr-output-display
         wlr-output-global
         wlr-output-resources
         wlr-output-name
         wlr-output-description
         wlr-output-make
         wlr-output-model
         wlr-output-serial
         wlr-output-phys-width
         wlr-output-phys-height
         wlr-output-%modes
         wlr-output-modes
         wlr-output-current-mode
         wlr-output-width
         wlr-output-height
         wlr-output-refresh
         wlr-output-enabled?
         wlr-output-scale
         wlr-output-subpixel
         wlr-output-transform
         wlr-output-adaptive-sync-status
         wlr-output-needs-frame?
         wlr-output-frame-pending?
         wlr-output-transform-matrix
         wlr-output-pending
         wlr-output-commit-seq
         wlr-output-events-frame
         wlr-output-events-damage
         wlr-output-events-needs-frame
         wlr-output-events-precommit
         wlr-output-events-commit
         wlr-output-events-present
         wlr-output-events-enable
         wlr-output-events-mode
         wlr-output-events-scale
         wlr-output-events-transform
         wlr-output-events-description
         wlr-output-events-destroy
         wlr-output-idle-frame
         wlr-output-idle-done
         wlr-output-attach-render-locks
         wlr-output-cursors
         wlr-output-hardware-cursor
         wlr-output-software-cursor-locks
         wlr-output-display-destroy
         wlr-output-data

         wlr-output-event-damage-output
         wlr-output-event-damage-damage

         wlr-output-event-precommit-output
         wlr-output-event-precommit-when

         wlr-output-present/vsync
         wlr-output-present/hw-clock
         wlr-output-present/hw-completion
         wlr-output-present/zero-copy

         wlr-output-event-present-output
         wlr-output-event-present-commit-seq
         wlr-output-event-present-when
         wlr-output-event-present-seq
         wlr-output-event-present-refresh
         wlr-output-event-present-flags

         wlr-output-enable
         wlr-output-create-global
         wlr-output-destroy-global
         wlr-output-preferred-mode
         wlr-output-set-mode
         wlr-output-set-custom-mode
         wlr-output-set-transform
         wlr-output-enable-adaptive-sync
         wlr-output-set-scale
         wlr-output-set-subpixel
         wlr-output-set-description
         wlr-output-schedule-done
         wlr-output-destroy
         wlr-output-transformed-resolution
         wlr-output-effective-resolution
         wlr-output-attach-render
         wlr-output-attach-buffer
         wlr-output-preferred-read-format
         wlr-output-set-damage
         wlr-output-test
         wlr-output-commit
         wlr-output-rollback
         wlr-output-schedule-frame
         wlr-output-get-gamma-size
         wlr-output-set-gamma
         wlr-output-export-dmabuf
         wlr-output-from-resource
         wlr-output-lock-attach-render
         wlr-output-lock-software-cursors
         wlr-output-render-software-cursors
         wlr-output-cursor-create
         wlr-output-cursor-set-image
         wlr-output-cursor-set-surface
         wlr-output-cursor-move
         wlr-output-cursor-destroy
         wlr-output-transform-invert
         wlr-output-transform-compose)
  (import (scheme)
          (chicken base)
          (wayland-server))
  (include "ffi-helpers.scm")

  (bind-file "include/bind/wlr/types/wlr_output.h")
  (define wlr-output-state-enabled? wlr-output-state-enabled)
  (define wlr-output-state-adaptive-sync-enabled? wlr-output-state-adaptive-sync-enabled)

  (define-foreign-record-type (wlr-output-mode* "struct wlr_output_mode")
    ((struct "wl_list") link    wlr-output-mode-link))

  (define-foreign-record-type (wlr-output-cursor* "struct wlr_output_cursor")
    ((struct "wl_list") link wlr-output-cursor-link)
    ((struct "wl_listener") surface_commit wlr-output-cursor-surface-commit)
    ((struct "wl_listener") surface_destroy wlr-output-cursor-surface-destroy)
    ((struct "wl_signal") events.destroy wlr-output-cursor-events-destroy))

  (define-foreign-values wlr-output-adaptive-sync-status
    (wlr-output-adaptive-sync/enabled "WLR_OUTPUT_ADAPTIVE_SYNC_ENABLED")
    (wlr-output-adaptive-sync/disabled "WLR_OUTPUT_ADAPTIVE_SYNC_DISABLED")
    (wlr-output-adaptive-sync/unknown "WLR_OUTPUT_ADAPTIVE_SYNC_UNKNOWN"))

  (define-foreign-values wlr-output-state-field
    (wlr-output-state/buffer "WLR_OUTPUT_STATE_BUFFER")
    (wlr-output-state/damage "WLR_OUTPUT_STATE_DAMAGE")
    (wlr-output-state/mode "WLR_OUTPUT_STATE_MODE")
    (wlr-output-state/enabled "WLR_OUTPUT_STATE_ENABLED")
    (wlr-output-state/scale "WLR_OUTPUT_STATE_SCALE")
    (wlr-output-state/transform "WLR_OUTPUT_STATE_TRANSFORM")
    (wlr-output-state/adaptive-sync-enabled "WLR_OUTPUT_STATE_ADAPTIVE_SYNC_ENABLED")
    (wlr-output-state/gamma-lut "WLR_OUTPUT_STATE_GAMMA_LUT"))

  (define-foreign-values wlr-output-state-buffer-type
    (wlr-output-state-buffer/render "WLR_OUTPUT_STATE_BUFFER_RENDER")
    (wlr-output-state-buffer/scanout "WLR_OUTPUT_STATE_BUFFER_SCANOUT"))

  (define-foreign-values wlr-output-state-mode-type
    (wlr-output-state-mode/fixed "WLR_OUTPUT_STATE_MODE_FIXED")
    (wlr-output-state-mode/custom "WLR_OUTPUT_STATE_MODE_CUSTOM"))

  (define-foreign-record-type (wlr-output-state* "struct wlr_output_state")
    ((struct "pixman_region32") damage wlr-output-state-damage)
    (int32 custom_mode.width wlr-output-state-custom-mode-width)
    (int32 custom_mode.height wlr-output-state-custom-mode-height)
    (int32 custom_mode.refresh wlr-output-state-custom-mode-refresh))

  (define-foreign-record-type (wlr-output* "struct wlr_output")
    ((struct "wl_list") resources wlr-output-resources)
    (c-string name wlr-output-name)
    (c-string make wlr-output-make)
    (c-string model wlr-output-model)
    (c-string serial wlr-output-serial)
    ((struct "wl_list") modes wlr-output-%modes)
    ((c-pointer float) transform_matrix wlr-output-transform-matrix)
    ((struct "wlr_output_state") pending wlr-output-pending)
    ((struct "wl_signal") events.frame wlr-output-events-frame)
    ((struct "wl_signal") events.damage wlr-output-events-damage)
    ((struct "wl_signal") events.needs_frame wlr-output-events-needs-frame)
    ((struct "wl_signal") events.precommit wlr-output-events-precommit)
    ((struct "wl_signal") events.commit wlr-output-events-commit)
    ((struct "wl_signal") events.present wlr-output-events-present)
    ((struct "wl_signal") events.enable wlr-output-events-enable)
    ((struct "wl_signal") events.mode wlr-output-events-mode)
    ((struct "wl_signal") events.scale wlr-output-events-scale)
    ((struct "wl_signal") events.transform wlr-output-events-transform)
    ((struct "wl_signal") events.description wlr-output-events-description)
    ((struct "wl_signal") events.destroy wlr-output-events-destroy)
    ((struct "wl_list") cursors wlr-output-cursors)
    ((struct "wl_listener") display_destroy wlr-output-display-destroy))

  (define (wlr-output-modes output)
    (map (foreign-lambda* wlr-output-mode* ((wl-list* node))
           "struct wlr_output_mode *mode;"
           "mode = wl_container_of(node, mode, link);"
           "C_return(mode);")
         (wl-list->list (wlr-output-%modes output))))

  (define-foreign-values wlr-output-present-flag
    (wlr-output-present/vsync         "WLR_OUTPUT_PRESENT_VSYNC")
    (wlr-output-present/hw-clock      "WLR_OUTPUT_PRESENT_HW_CLOCK")
    (wlr-output-present/hw-completion "WLR_OUTPUT_PRESENT_HW_COMPLETION")
    (wlr-output-present/zero-copy     "WLR_OUTPUT_PRESENT_ZERO_COPY"))
  
  (define wlr-output-cursor-enabled? wlr-output-cursor-enabled)
  (define wlr-output-cursor-visible? wlr-output-cursor-visible)
  (define wlr-output-enabled? wlr-output-enabled)
  (define wlr-output-needs-frame? wlr-output-needs-frame)
  (define wlr-output-frame-pending? wlr-output-frame-pending))
