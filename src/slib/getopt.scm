;;; "getopt.scm" POSIX command argument processing
;Copyright (C) 1993, 1994, 2002 Aubrey Jaffer
;
;Permission to copy this software, to modify it, to redistribute it,
;to distribute modified versions, and to use it for any purpose is
;granted, subject to the following restrictions and understandings.
;
;1.  Any copy made of this software must include this copyright notice
;in full.
;
;2.  I have made no warranty or representation that the operation of
;this software will be error-free, and I am under no obligation to
;provide any services, by way of maintenance, update, or otherwise.
;
;3.  In conjunction with products arising from the use of this
;material, there shall be no use of my name in any advertising,
;promotional, or sales literature without prior written consent in
;each case.

;; Packaged for R7RS Scheme by Peter Lane, 2017
;;
;; Parameters exported for option-index/option-arg/option-name

(module (slib getopt)
        (getopt
         getopt--
         option-index
         option-arg
         option-name)
  (import (scheme) 
          (srfi 13)
          (chicken base)
          (chicken process-context))

  (define getopt:scan #f)
  (define getopt:char #\-)
  (define args (argv))

  (define option-index (make-parameter 1)) ;; *optind* now a parameter
  (define option-arg (make-parameter 0))   ;; *optarg* now a parameter
  (define option-name (make-parameter #f)) ;; getopt:opt now a parameter

  ;@
  (define (getopt optstring)
    (define (argref) (list-ref args (option-index)))
    (if (cond ((and getopt:scan 
                    (not (string=? "" getopt:scan)))
               #t)
              ((>= (option-index) (length args)) 
               #f)
              (else
                (let ((arg (argref)))
                  (cond ((or (<= (string-length arg) 1)
                             (not (char=? (string-ref arg 0) getopt:char)))
                         #f)
                        ((and (= (string-length arg) 2)
                              (char=? (string-ref arg 1) getopt:char))
                         (option-index (+ (option-index) 1))
                         #f)
                        (else
                          (set! getopt:scan (string-copy arg 1))
                          #t)))))
      (begin
        (option-name (string-ref getopt:scan 0))
        (set! getopt:scan (string-copy getopt:scan 1))
        (when (string=? "" getopt:scan) 
          (option-index (+ (option-index) 1)))
        (let* ((opts (string->list optstring))
               (place (member (option-name) opts)))
          (cond ((not place) 
                 #\?)
                ((or (null? (cdr place)) 
                     (not (char=? #\: (cadr place))))
                 (option-name))
                ((not (string=? "" getopt:scan))
                 (option-arg getopt:scan)
                 (option-index (+ (option-index) 1))
                 (set! getopt:scan #f)
                 (option-name))
                ((< (option-index) (length args))
                 (option-arg (argref))
                 (option-index (+ (option-index) 1))
                 (option-name))
                ((and (not (null? opts)) 
                      (char=? #\: (car opts))) 
                 #\:)
                (else 
                  #\?))))
      #f))

  ;@
  (define (getopt-- optstring)
    (let* ((opt (getopt (string-append optstring "-:")))
           (optarg (option-arg)))
      (cond ((eqv? #\- opt)		;long option
             (do ((l (string-length (option-arg)))
                  (i 0 (+ 1 i)))
               ((or (>= i l) (char=? #\= (string-ref optarg i)))
                (cond ((>= i l) (option-arg #f) optarg)
                      (else (option-arg (string-copy optarg (+ 1 i) l))
                            (string-copy optarg 0 i))))))
            (else opt)))))
