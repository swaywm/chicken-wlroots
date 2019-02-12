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

(foreign-declare "#include <wlr/util/log.h>")
(include "wlroots-types.scm")

; XXX: If we allow custom logging functions to be written in Scheme, then pretty
;      much every wlroots function needs to be ___safe, because any calls to
;      wlr_log will invoke a Scheme callback. Not worth it.
;
;      If a custom logging function is required, the user will have to write it
;      in C.
;#>
;static void *scheme_log_callback = NULL;
;
;static void scheme_log_func(enum wlr_log_importance importance, const char *fmt, va_list args)
;{
;    // print into buffer
;    char buf[256];
;    int len = vsnprintf(buf, 256, fmt, args);
;
;    // call user-supplied callback
;    C_word *ptr = C_alloc(sizeof(C_word));
;    C_save(C_string(&ptr, len, buf));
;    C_save(C_fix(importance));
;    C_callback(CHICKEN_gc_root_ref(scheme_log_callback), 2);
;}
;
;static void scheme_wlr_log_init(enum wlr_log_importance verbosity, C_word callback)
;{
;    if (!scheme_log_callback) {
;        scheme_log_callback = CHICKEN_new_gc_root();
;    }
;    CHICKEN_gc_root_set(scheme_log_callback, callback);
;    // use default logger if user passed #f
;    wlr_log_init(verbosity, C_truep(callback) ? scheme_log_func : NULL);
;}
;<#

(module (wlr util log)
        (%wlr-log-init
         wlr-log-init
         wlr-log-get-verbosity
         (wlr-log %wlr-log))
  (import (scheme)
          (srfi 13)
          (srfi 28)
          (chicken base)
          (chicken foreign))

  (define (int->wlr-log-importance n)
    (case n
      ((0) 'silent)
      ((1) 'error)
      ((2) 'info)
      ((3) 'debug)
      ; this should never happen
      (else 'silent)))

  (define (wlr-log-importance->int sym)
    (case sym
      ((silent) 0)
      ((error)  1)
      ((info)   2)
      ((debug)  3)
      ; FIXME: error
      (else     0)))

  (define-foreign-type wlr-log-importance
                       (enum "wlr_log_importance")
                       wlr-log-importance->int
                       int->wlr-log-importance)

  (define %wlr-log-init
    (foreign-lambda void "wlr_log_init" wlr-log-importance c-pointer))

  (define (wlr-log-init verb)
    (%wlr-log-init verb #f))

  (define %wlr-log-init
    (foreign-lambda void "wlr_log_init"
      wlr-log-importance c-pointer))

  (define (wlr-log-init verbosity #!optional c-callback)
    (%wlr-log-init verbosity c-callback))

  (define wlr-log-get-verbosity
    (foreign-lambda wlr-log-importance "wlr_log_get_verbosity"))

  ;; Sanitize input that will be interpreted as a C format string,
  ;; i.e. replace '%' with '%%'.
  (define (sanitize-format str)
    (apply string
      (reverse
        (string-fold
          (lambda (c acc)
            (if (char=? c #\%)
              (cons #\% (cons #\% acc))
              (cons c acc)))
          '()
          str))))

  ;; This is defined as a macro so that we can use get-line-number.
  (define-syntax wlr-log
    (er-macro-transformer
      (lambda (expr r compare)
        (let ((verb (cadr expr))
              (fmt  (caddr expr))
              (args (cdddr expr))
              (line (get-line-number expr)))
          `(,(r '%wlr-log) ,verb ,line ,fmt ,@args)))))

  (define (%wlr-log verb line fmt . args)
    ((foreign-safe-lambda* void ((wlr-log-importance verb) (c-string str))
       "_wlr_log(verb, str);")
      verb
      (sanitize-format (string-append (if line (format "[~a] " line) "[?] ")
                                      (apply format fmt args))))))
