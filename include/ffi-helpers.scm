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

(import (chicken foreign)
        (foreigners)
        (bind))

; Define a series of foreign values of the same type.
(define-syntax define-foreign-values
  (syntax-rules ()
    ((define-foreign-values type)
      (begin))
    ((define-foreign-values type (scm-name c-name) . rest)
      (begin
        (define scm-name (foreign-value c-name type))
        (define-foreign-values type . rest)))))

; XXX: prevent bind from converting arguments to SRFI-4 vector types
(bind-type "wlr_matrix" (c-pointer float))
(bind-type "wlr_rgba" (c-pointer float))
(bind-type "uint32_ptr" (c-pointer unsigned-int32))

(bind-type "int16_t" short)
(bind-type "uint16_t" unsigned-short)

; FIXME: use configure script to determine correct size
(bind-type "pid_t" int32)
(bind-type "uid_t" unsigned-int32)
(bind-type "gid_t" unsigned-int32)
(bind-type "time_t" unsigned-integer64)

(bind-type "EGLint" int32)
(bind-type "EGLint_ptr" (c-pointer int32))
(bind-type "EGLenum" unsigned-int)

(bind-type "xkb_mod_mask_t" unsigned-int32)
(bind-type "xkb_led_index_t" unsigned-int32)
(bind-type "xkb_mod_index_t" unsigned-int32)

(bind-options export-constants: #t
              mutable-fields: #t
              default-renaming: "")
