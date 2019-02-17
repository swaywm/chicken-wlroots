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

(foreign-declare "#include <wlr/types/wlr_matrix.h>")
(include "wlroots-types.scm")

(module (wlr types wlr-matrix)
        (make-wlr-matrix
         wlr-matrix-ref
         wlr-matrix-set
         wlr-matrix-identity
         wlr-matrix-transpose
         wlr-matrix-translate
         wlr-matrix-scale
         wlr-matrix-rotate
         wlr-matrix-transform
         wlr-matrix-projection
         wlr-matrix-project-box)
  (import (scheme)
          (chicken base)
          (chicken gc)
          (chicken memory))
  (include "ffi-helpers.scm")

  (bind-file "include/bind/wlr/types/wlr_matrix.h")

  (define-foreign-type wlr-matrix (c-pointer float))

  (define (make-wlr-matrix)
    ; allocate a float[9]
    ; XXX: assumes 32-bit float
    (let ((mem (allocate (* 9 4))))
      (set-finalizer! mem free)
      mem))

  (define wlr-matrix-ref
    (foreign-lambda* float ((wlr-matrix matrix) (int i))
      "C_return(matrix[i]);"))

  (define wlr-matrix-set
    (foreign-lambda* void ((wlr-matrix matrix) (int i) (float v))
      "matrix[i] = v;")))
