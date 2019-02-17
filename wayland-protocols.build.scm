#! /usr/bin/env -S csi -script

(import (srfi 13)
        (chicken io)
        (chicken process)
        (chicken process-context)
        (chicken string))

(define (command->string cmdline)
  (call-with-input-pipe cmdline
    (lambda (p)
      (string-trim-right (read-string #f p)))))

(define wayland-protocols (command->string "pkg-config --variable=pkgdatadir wayland-protocols"))
(define wayland-scanner (command->string "pkg-config --variable=wayland_scanner wayland-scanner"))
(define cc (get-environment-variable "CHICKEN_CC"))

(define (h-path name) (string-append "include/" name ".h"))
(define (c-path name) (string-append "src/" name ".h"))
(define (o-path name) (string-append name ".o"))

(define (generate-protocol name xml-path)
  (process-execute wayland-scanner (list "server-header" xml-path (h-path name)))
  (process-execute wayland-scanner (list "private-code"  xml-path (c-path name)))
  (process-execute cc (list "-c" "-fPIC" "-o" (o-path name) (c-path name))))

(generate-protocol "xdg-shell-protocol"
  (string-append wayland-protocols "/stable/xdg-shell/xdg-shell.xml"))
