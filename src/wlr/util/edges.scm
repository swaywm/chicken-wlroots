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

(foreign-declare "#include <wlr/util/edges.h>")
(include "wlroots-types.scm")

(module (wlr util edges)
        (wlr-edge/none
         wlr-edge/top
         wlr-edge/bottom
         wlr-edge/left
         wlr-edge/right)
  (import (scheme)
          (chicken foreign)
          (ffi-helpers))
  (define-foreign-values wlr-edges
    (wlr-edge/none   "WLR_EDGE_NONE")
    (wlr-edge/top    "WLR_EDGE_TOP")
    (wlr-edge/bottom "WLR_EDGE_BOTTOM")
    (wlr-edge/left   "WLR_EDGE_LEFT")
    (wlr-edge/right  "WLR_EDGE_RIGHT")))
