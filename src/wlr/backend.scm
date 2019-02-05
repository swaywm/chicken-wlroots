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

(foreign-declare "#include <wlr/backend.h>")
(include "wlroots-types.scm")

(module (wlr backend)
        (wlr-backend-events-destroy
         wlr-backend-events-new-input
         wlr-backend-events-new-output
         wlr-backend-autocreate
         wlr-backend-start 
         wlr-backend-destroy
         wlr-backend-get-renderer
         wlr-backend-get-session
         wlr-backend-get-presentation-clock)
  (import (scheme)
          (chicken base)
          (chicken foreign)
          (foreigners)
          (bind))

  (include "bind-options.scm")
  (bind-file "include/bind/wlr/backend.h")

  (define-foreign-record-type (wlr-backend* "struct wlr_backend")
    ((struct "wl_signal") events.destroy wlr-backend-events-destroy)
    ((struct "wl_signal") events.new_input wlr-backend-events-new-input)
    ((struct "wl_signal") events.new_output wlr-backend-events-new-output)))
