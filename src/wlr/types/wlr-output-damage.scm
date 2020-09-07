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

(foreign-declare "#include <wlr/types/wlr_output_damage.h>")
(include "wlroots-types.scm")

(module (wlr types wlr-output-damage)
        (wlr-output-damage-previous-len
         wlr-output-damage-output
         wlr-output-damage-max-rects
         wlr-output-damage-current
         wlr-output-damage-previous
         wlr-output-damage-previous-idx
         wlr-output-damage-events-frame
         wlr-output-damage-events-destroy
         wlr-output-damage-output-destroy
         wlr-output-damage-output-mode
         wlr-output-damage-output-transform
         wlr-output-damage-output-scale
         wlr-output-damage-output-needs-frame
         wlr-output-damage-output-damage
         wlr-output-damage-output-frame
         wlr-output-damage-output-commit

         wlr-output-damage-create
         wlr-output-damage-destroy
         wlr-output-damage-attach-render
         wlr-output-damage-add
         wlr-output-damage-add-whole
         wlr-output-damage-add-box)
  (import (scheme)
          (chicken base)
          (chicken gc) 
          (chicken memory))
  (include "ffi-helpers.scm")

  (bind-rename wlr_output_damage_attach_render %wlr-output-damage-attach-render)
  (bind-file "include/bind/wlr/types/wlr_output_damage.h")

  (define-foreign-record-type (wlr-output-damage* "struct wlr_output_damage")
    ((struct "pixman_region32") current wlr-output-damage-current)
    ((c-pointer (struct "pixman_region32")) previous %wlr-output-damage-previous)
    ((struct "wl_signal") events.frame wlr-output-damage-events-frame)
    ((struct "wl_signal") events.destroy wlr-output-damage-events-destroy)
    ((struct "wl_listener") output_destroy wlr-output-damage-output-destroy)
    ((struct "wl_listener") output_mode wlr-output-damage-output-mode)
    ((struct "wl_listener") output_transform wlr-output-damage-output-transform)
    ((struct "wl_listener") output_scale wlr-output-damage-output-scale)
    ((struct "wl_listener") output_needs_frame wlr-output-damage-output-needs-frame)
    ((struct "wl_listener") output_damage wlr-output-damage-output-damage)
    ((struct "wl_listener") output_frame wlr-output-damage-output-frame)
    ((struct "wl_listener") output_commit wlr-output-damage-output-commit))

  (define (wlr-output-damage-previous damage #!optional idx)
    (if idx
      ((foreign-lambda* (c-pointer (struct "pixman_region32"))
                        (((c-pointer (struct "pixman_region32")) damage) (int idx))
         "C_return(damage+idx);")
        (%wlr-output-damage-previous damage) idx)
      (%wlr-output-damage-previous damage)))
  
  (define (wlr-output-damage-attach-render output-damage)
    (let ((buffer-damage (allocate (foreign-type-size pixman-region32))))
      (set-finalizer! buffer-damage free)
      (let-location ((needs-frame bool))
        (let ((rval (%wlr-output-damage-attach-render output-damage needs-frame (location buffer-damage))))
          (values rval needs-frame buffer-damage))))))
