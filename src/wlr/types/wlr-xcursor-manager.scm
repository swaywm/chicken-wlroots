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

(foreign-declare "#include <wlr/types/wlr_xcursor_manager.h>")
(include "wlroots-types.scm")

(module (wlr types wlr-xcursor-manager)
        (wlr-xcursor-manager-theme-scale
         wlr-xcursor-manager-theme-theme
         wlr-xcursor-manager-theme-link

         wlr-xcursor-manager-name
         wlr-xcursor-manager-size
         wlr-xcursor-manager-scaled-themes

         wlr-xcursor-manager-create
         wlr-xcursor-manager-destroy
         wlr-xcursor-manager-load
         wlr-xcursor-manager-get-xcursor
         wlr-xcursor-manager-set-cursor-image)
  (import (scheme)
          (chicken base))
  (include "ffi-helpers.scm")

  (bind-file "include/bind/wlr/types/wlr_xcursor_manager.h")

  (define-foreign-record-type (wlr-xcursor-manager-theme* "struct wlr_xcursor_manager_theme")
    ((struct "wl_list") link wlr-xcursor-manager-theme-link))

  (define-foreign-record-type (wlr-xcursor-manager* "struct wlr_xcursor_manager")
    ((struct "wl_list") scaled_themes wlr-xcursor-manager-scaled-themes))

  (define wlr-xcursor-manager-create
    (foreign-lambda wlr-xcursor-manager* "wlr_xcursor_manager_create"
      (const c-string) unsigned-int32))

  (define wlr-xcursor-manager-destroy
    (foreign-lambda void "wlr_xcursor_manager_destroy" wlr-xcursor-manager*))

  (define wlr-xcursor-manager-load
    (foreign-lambda int "wlr_xcursor_manager_load" wlr-xcursor-manager* float))

  (define wlr-xcursor-manager-get-xcursor
    (foreign-lambda wlr-xcursor* "wlr_xcursor_manager_get_xcursor"
      wlr-xcursor-manager* (const c-string) float))

  (define wlr-xcursor-manager-set-cursor-image
    (foreign-lambda void "wlr_xcursor_manager_set_cursor_image"
      wlr-xcursor-manager* (const c-string) wlr-cursor*)))
