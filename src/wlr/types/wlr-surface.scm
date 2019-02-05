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

(foreign-declare "#include <wlr/types/wlr_surface.h>")
(include "wlroots-types.scm")

#>
/* Invoke a Scheme callback passed in as a GC root. */
void scheme_wlr_surface_iterator(struct wlr_surface *surface, int sx, int sy, void *root)
{
  C_word *ptr = C_alloc(sizeof(C_word));
  C_save(C_fix(sy));
  C_save(C_fix(sx));
  C_save(C_mpointer_or_false(&ptr, surface));
  C_callback(CHICKEN_gc_root_ref(root), 3);
}
<#

(module (wlr types wlr-surface)
        (clock-gettime ; FIXME- this shouldn't be here

         wlr-surface-state/buffer
         wlr-surface-state/surface-damage
         wlr-surface-state/buffer-damage
         wlr-surface-state/opaque-region
         wlr-surface-state/input-region
         wlr-surface-state/transform
         wlr-surface-state/scale
         wlr-surface-state/frame-callback-list

         wlr-surface-state-committed
         wlr-surface-state-buffer-resource
         wlr-surface-state-dx
         wlr-surface-state-dy
         wlr-surface-state-surface-damage
         wlr-surface-state-buffer-damage
         wlr-surface-state-opaque
         wlr-surface-state-input
         wlr-surface-state-transform
         wlr-surface-state-scale
         wlr-surface-state-frame-callback-list
         wlr-surface-state-width
         wlr-surface-state-height
         wlr-surface-state-buffer-width
         wlr-surface-state-buffer-height
         wlr-surface-state-buffer-destroy

         wlr-surface-role-name
         wlr-surface-role-commit
         wlr-surface-role-precommit

         wlr-surface-resource
         wlr-surface-renderer
         wlr-surface-buffer
         wlr-surface-sx
         wlr-surface-sy
         wlr-surface-buffer-damage
         wlr-surface-opaque-region
         wlr-surface-input-region
         wlr-surface-current
         wlr-surface-pending
         wlr-surface-previous
         wlr-surface-role
         wlr-surface-role-data
         wlr-surface-events-commit
         wlr-surface-events-new-subsurface
         wlr-surface-events-destroy
         wlr-surface-subsurfaces
         wlr-surface-subsurface-pending-list
         wlr-surface-renderer-destroy
         wlr-surface-data

         wlr-subsurface-state-x
         wlr-subsurface-state-y

         wlr-subsurface-resource
         wlr-subsurface-surface
         wlr-subsurface-parent
         wlr-subsurface-current
         wlr-subsurface-pending
         wlr-subsurface-cached
         wlr-subsurface-has-cache?
         wlr-subsurface-synchronized?
         wlr-subsurface-reordered?
         wlr-subsurface-parent-link
         wlr-subsurface-parent-pending-link
         wlr-subsurface-surface-destroy
         wlr-subsurface-parent-destroy
         wlr-subsurface-events-destroy
         wlr-subsurface-data

         wlr-surface-create
         wlr-surface-set-role
         wlr-surface-has-buffer?
         wlr-surface-get-texture
         wlr-subsurface-create
         wlr-surface-get-root-surface
         wlr-surface-point-accepts-input?
         wlr-surface-surface-at
         wlr-surface-send-enter
         wlr-surface-send-leave
         wlr-surface-send-frame-done
         wlr-surface-get-extends
         wlr-surface-from-resource
         wlr-surface-for-each-surface
         wlr-surface-get-effective-damage)
  (import (scheme)
          (chicken base)
          (chicken foreign)
          (chicken gc)
          (chicken memory)
          (foreigners)
          (ffi-helpers)
          (bind))

  (include "bind-options.scm")
  (bind-file "include/bind/wlr/types/wlr_surface.h")

  (define-foreign-type timespec* (c-pointer (struct "timespec")))

  (define (make-timespec)
    (let ((ts ((foreign-lambda* timespec* ()
                 "struct timespec *ts = malloc(sizeof(struct timespec));"
                 "C_return(ts);"))))
      (set-finalizer! ts free)
      ts))

  (define (clock-gettime clock)
    (let ((clock (case clock
                   ((realtime) (foreign-value "CLOCK_REALTIME" int))
                   ((monotonic) (foreign-value "CLOCK_MONOTONIC" int))))
          (ts (make-timespec)))
      ((foreign-lambda int "clock_gettime" int timespec*) clock ts)
      ts))

  (define-foreign-values wlr-surface-state-field
    (wlr-surface-state/buffer              "WLR_SURFACE_STATE_BUFFER")
    (wlr-surface-state/surface-damage      "WLR_SURFACE_STATE_SURFACE_DAMAGE")
    (wlr-surface-state/buffer-damage       "WLR_SURFACE_STATE_BUFFER_DAMAGE")
    (wlr-surface-state/opaque-region       "WLR_SURFACE_STATE_OPAQUE_REGION")
    (wlr-surface-state/input-region        "WLR_SURFACE_STATE_INPUT_REGION")
    (wlr-surface-state/transform           "WLR_SURFACE_STATE_TRANSFORM")
    (wlr-surface-state/scale               "WLR_SURFACE_STATE_SCALE")
    (wlr-surface-state/frame-callback-list "WLR_SURFACE_STATE_FRAME_CALLBACK_LIST"))

  (define-foreign-record-type (wlr-surface-state* "struct wlr_surface_state")
    ((struct "pixman_region32") surface_damage wlr-surface-state-surface-damage)
    ((struct "pixman_region32") buffer_damage wlr-surface-state-buffer-damage)
    ((struct "pixman_region32") opaque wlr-surface-state-opaque)
    ((struct "pixman_region32") input wlr-surface-state-input)
    ((struct "wl_list") frame_callback_list wlr-surface-state-frame-callback-list)
    ((struct "wl_listener") buffer_destroy wlr-surface-state-buffer-destroy))

  (define-foreign-record-type (wlr-surface-role* "struct wlr_surface_role")
    ((const c-string) name wlr-surface-role:name)
    ((function void ((c-pointer (struct "wlr_surface")))) commit wlr-surface-role-commit)
    ((function void ((c-pointer (struct "wlr_surface")))) precommit wlr-surface-role-precommit))

  (define-foreign-record-type (wlr-surface* "struct wlr_surface")
    ((struct "pixman_region32") buffer_damage wlr-surface-buffer-damage)
    ((struct "pixman_region32") opaque_region wlr-surface-opaque-region)
    ((struct "pixman_region32") input_region wlr-surface-input-region)
    ((struct "wlr_surface_state") current wlr-surface-current)
    ((struct "wlr_surface_state") pending wlr-surface-pending)
    ((struct "wlr_surface_state") previous wlr-surface-previous)
    ((struct "wl_signal") events.commit wlr-surface-events-commit)
    ((struct "wl_signal") events.new_subsurface wlr-surface-events-new-subsurface)
    ((struct "wl_signal") events.destroy wlr-surface-events-destroy)
    ((struct "wl_list") subsurfaces wlr-surface-subsurfaces)
    ((struct "wl_list") subsurface_pending_list wlr-surface-subsurface-pending-list)
    ((struct "wl_listener") renderer_destroy wlr-surface-renderer-destroy))

  (define-foreign-record-type (wlr-subsurface* "struct wlr_subsurface")
    ((struct "wlr_subsurface_state") current wlr-subsurface-current)
    ((struct "wlr_subsurface_state") pending wlr-subsurface-pending)
    ((struct "wlr_surface_state") cached wlr-subsurface-cached)
    ((struct "wl_list") parent_link wlr-subsurface-parent-link)
    ((struct "wl_list") parent_pending_link wlr-subsurface-parent-pending-link)
    ((struct "wl_listener") surface_destroy wlr-subsurface-surface-destroy)
    ((struct "wl_listener") parent_destroy wlr-subsurface-parent-destroy)
    ((struct "wl_signal") events.destroy wlr-subsurface-events-destroy))

  (define wlr-surface-for-each-surface
    (foreign-safe-lambda* void ((wlr-surface* surface) (scheme-object callback))
      "void *root = CHICKEN_new_gc_root();"
      "CHICKEN_gc_root_set(root, callback);"
      "wlr_surface_for_each_surface(surface, scheme_wlr_surface_iterator, root);"
      "CHICKEN_delete_gc_root(root);"))

  (define wlr-surface-get-effective-damage
    (foreign-lambda void "wlr_surface_get_effective_damage" wlr-surface* pixman-region32*))

  (define wlr-subsurface-has-cache? wlr-subsurface-has-cache)
  (define wlr-subsurface-synchronized? wlr-subsurface-synchronized)
  (define wlr-subsurface-reordered? wlr-subsurface-reordered)
  (define wlr-surface-has-buffer? wlr-surface-has-buffer)
  (define wlr-surface-point-accepts-input? wlr-surface-point-accepts-input))
