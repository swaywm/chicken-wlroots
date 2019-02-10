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

(foreign-declare "#include <time.h>")

(module (wlr time)
        (make-timespec
         clock-gettime)
  (import (scheme)
          (chicken base)
          (chicken gc)
          (chicken memory)
          (chicken foreign))

  (define (make-timespec)
    (let ((ts (allocate (foreign-type-size (struct "timespec")))))
      (set-finalizer! ts free)
      ts))

  (define (clock-gettime #!optional (clock 'monotonic))
    (let ((clock (case clock
                   ((realtime) (foreign-value "CLOCK_REALTIME" int))
                   ((monotonic) (foreign-value "CLOCK_MONOTONIC" int))))
          (ts (make-timespec)))
      ((foreign-lambda int "clock_gettime" int (c-pointer (struct "timespec"))) clock ts)
      ts)))
