#! /usr/bin/env -S csi -script

(import (srfi 13)
        (chicken file)
        (chicken io)
        (chicken process)
        (chicken process-context)
        (chicken string))

(define (read-words-from-command cmdline)
  (call-with-input-pipe cmdline
    (lambda (p)
      (string-split (read-string #f p)))))

(define (read-command cmdline)
  (call-with-input-pipe cmdline
    (lambda (p)
      (read-string #f p))))

(define (arg-prefix prefix args)
  (let loop ((args args) (result '()))
    (if (null? args)
      (reverse result)
      (loop (cdr args) (cons (car args) (cons prefix result))))))

(define cflags  (arg-prefix "-C" (read-words-from-command "pkg-config --cflags wlroots")))
(define ldflags (arg-prefix "-L" (read-words-from-command "pkg-config --libs wlroots")))

; Create the file "config.scm" from <wlr/config.h> using chicken-bind.
(unless (file-exists? "config.scm")
  (process-execute "chicken-bind"
    (list "-o" "config.scm"
          (string-append (string-trim-right
                           (read-command "pkg-config --variable=includedir wlroots"))
                         "/wlr/config.h"))))
(load "config.scm")

; Create feature flags for the values we just loaded from config.scm.
(define feature-flags (append (if (= WLR_HAS_LIBCAP 0)      '() '("-D" "libcap"))
                              (if (= WLR_HAS_SYSTEMD 0)     '() '("-D" "systemd"))
                              (if (= WLR_HAS_ELOGIND 0)     '() '("-D" "elogind"))
                              (if (= WLR_HAS_X11_BACKEND 0) '() '("-D" "x11-backend"))
                              (if (= WLR_HAS_XWAYLAND 0)    '() '("-D" "xwayland"))
                              (if (= WLR_HAS_XCB_ERRORS 0)  '() '("-D" "xcb-errors"))
                              (if (= WLR_HAS_XCB_ICCCM 0)   '() '("-D" "xcb-icccm"))))

(process-execute (get-environment-variable "CHICKEN_CSC")
                 (append cflags ldflags feature-flags (command-line-arguments)))
