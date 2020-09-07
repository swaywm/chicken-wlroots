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

(import (scheme base)
        (srfi 1)
        (srfi 4)
        (srfi 28)
        (chicken bitwise)
        (wayland-server)
        (wlr backend)
        (wlr render wlr-renderer)
        (wlr time)
        (wlr types wlr-cursor)
        (wlr types wlr-input-device)
        (wlr types wlr-keyboard)
        (wlr types wlr-output)
        (wlr types wlr-output-layout)
        (wlr types wlr-pointer)
        (wlr types wlr-tablet-tool)
        (wlr types wlr-xcursor-manager)
        (wlr util log)
        (xkbcommon))

(include "xkbcommon-keysyms.scm")

(define-record-type state
  (make-state display xcursor-manager cursor default-color clear-color layout
              devices last-frame touch-points)
  state?
  (display         state-display         state-display-set!)
  (xcursor-manager state-xcursor-manager state-xcursor-manager-set!)
  (cursor          state-cursor          state-cursor-set!)
  (default-color   state-default-color   state-default-color-set!)
  (clear-color     state-clear-color     state-clear-color-set!)
  (layout          state-layout          state-layout-set!)
  (devices         state-devices         state-devices-set!)
  (last-frame      state-last-frame      state-last-frame-set!)
  (touch-points    state-touch-points    state-touch-points-set!))

(define-record-type touch-point
  (make-touch-point id x y)
  touch-point?
  (id touch-point-id)
  (x  touch-point-x)
  (y  touch-point-y))

(define-record-type output
  (make-sample-output state output)
  output?
  (state   output-state   output-state-set!)
  (output  output-output  output-output-set!)
  (frame   output-frame   output-frame-set!)
  (destroy output-destroy output-destroy-set!))

(define-record-type keyboard
  (make-keyboard state device key destroy)
  keyboard?
  (state   keyboard-state   keyboard-state-set!)
  (device  keyboard-device  keyboard-device-set!)
  (key     keyboard-key     keyboard-key-set!)
  (destroy keyboard-destroy keyboard-destroy-set!))

(define (exit/error message)
  (wlr-log 'error message)
  (exit 1))

(define (warp-to-touch state dev)
  (unless (null? (state-touch-points state))
    (let ((x 0) (y 0) (n 0))
      (for-each (lambda (point)
                  (set! x (+ x (touch-point-x point)))
                  (set! y (+ y (touch-point-y point)))
                  (set! n (+ n 1)))
                (state-touch-points state))
      (set! x (/ x n))
      (set! y (/ y n))
      (wlr-cursor-warp-absolute (state-cursor state) dev x y))))

(define (output-frame-notify sample-output)
  (let* ((state (output-state sample-output))
         (wlr-output (output-output sample-output))
         (renderer (wlr-backend-get-renderer (wlr-output-backend wlr-output))))
    (wlr-output-attach-render wlr-output)
    (wlr-renderer-begin renderer (wlr-output-width wlr-output) (wlr-output-height wlr-output))
    (wlr-renderer-clear renderer (state-clear-color state))
    (wlr-output-render-software-cursors wlr-output #f)
    (wlr-renderer-end renderer)
    (wlr-output-commit wlr-output)))

(define (handle-cursor-motion state event)
  (wlr-cursor-move (state-cursor state)
                   (wlr-event-pointer-motion-device event)
                   (wlr-event-pointer-motion-delta-x event)
                   (wlr-event-pointer-motion-delta-y event)))

(define (handle-cursor-motion-absolute state event)
  (wlr-cursor-warp-absolute (state-cursor state)
                            (wlr-event-pointer-motion-absolute-device event)
                            (wlr-event-pointer-motion-absolute-x event)
                            (wlr-event-pointer-motion-absolute-y event)))

(define (handle-cursor-button state event)
  (if (= (wlr-event-pointer-button-state event) wlr-button/released)
    (state-clear-color-set! state (state-default-color state))
    (let ((color (f32vector 0.25 0.25 0.25 1)))
      (f32vector-set! color (modulo (wlr-event-pointer-button-button event) 3) 1.0)
      (state-clear-color-set! state color))))

(define (handle-cursor-axis state event)
  (let loop ((i 0))
    (when (< i 3)
      (let ((color (state-default-color state)))
        (f32vector-set! color i
          (max 0.0 (min 1.0 (+ (f32vector-ref color i)
                               (if (> (wlr-event-pointer-axis-delta event) 0)
                                 -0.05
                                 +0.05))))))
      (loop (+ i 1))))
  (state-clear-color-set! state (state-default-color state)))

(define (handle-touch-up state event)
  (state-touch-points-set! state
    (delete! (wlr-event-touch-up-touch-id event)
             (state-touch-points state)
             (lambda (id point)
               (= id (touch-point-id point)))))
  (warp-to-touch state (wlr-event-touch-up-device event)))

(define (handle-touch-down state event)
  (state-touch-points-set! state
    (cons (make-touch-point (wlr-event-touch-down-touch-id event)
                            (wlr-event-touch-down-x event)
                            (wlr-event-touch-down-y event))
          (state-touch-points state)))
  (warp-to-touch state (wlr-event-touch-down-device event)))

(define (handle-touch-motion state event)
  (for-each (lambda (point)
              (when (= (touch-point-id point)
                       (wlr-event-touch-motion-touch-id event))
                (touch-point-x-set! point (wlr-event-touch-motion-x event))
                (touch-point-y-set! point (wlr-event-touch-motion-y event))))
            (state-touch-points state))
  (warp-to-touch state (wlr-event-touch-motion-device event)))

(define (handle-touch-cancel)
  (void))

(define (handle-tablet-tool-axis state event)
  ; XXX: shouldn't this be OR?
  (when (and (bit->boolean (wlr-event-tablet-tool-axis-updated-axes event) wlr-tablet-tool-axis/x)
             (bit->boolean (wlr-event-tablet-tool-axis-updated-axes event) wlr-tablet-tool-axis/y))
    (wlr-cursor-warp-absolute (state-cursor state)
                              (wlr-event-tablet-tool-axis-device event)
                              (wlr-event-tablet-tool-axis-x event)
                              (wlr-event-tablet-tool-axis-y event))))

(define (keyboard-key-notify keyboard event)
  (let ((keycode (+ 8 (wlr-event-keyboard-key-keycode event)))
        (wlr-keyboard (wlr-input-device-keyboard (keyboard-device keyboard))))
    (for-each (lambda (sym)
                (when (= sym XKB_KEY_Escape)
                  (wl-display-terminate (state-display (keyboard-state keyboard)))))
              (xkb-state-key-get-syms (wlr-keyboard-xkb-state wlr-keyboard) keycode))))

(define (output-remove-notify sample-output)
  (let* ((state (output-state sample-output)))
    (wlr-output-layout-remove (state-layout state) (output-output sample-output))
    (remove-wl-listener (output-frame sample-output))
    (remove-wl-listener (output-destroy sample-output))
    (free-wl-listener (output-frame sample-output))
    (free-wl-listener (output-destroy sample-output))))

(define (new-output-notify state output)
  (let* ((sample-output (make-sample-output state output))
         (output-modes (wlr-output-modes output)))
    (unless (null? output-modes)
      (wlr-output-set-mode output (last output-modes)))
    (output-frame-set! sample-output
      (make-wl-listener (lambda (_) (output-frame-notify sample-output))))
    (wl-signal-add (wlr-output-events-frame output) (output-frame sample-output))
    (output-destroy-set! sample-output
      (make-wl-listener (lambda (_) (output-remove-notify sample-output))))
    (wl-signal-add (wlr-output-events-destroy output) (output-destroy sample-output))
    (wlr-output-layout-add-auto (state-layout state) output)

    (wlr-xcursor-manager-load (state-xcursor-manager state) (wlr-output-scale output))
    (wlr-xcursor-manager-set-cursor-image (state-xcursor-manager state)
                                          "left_ptr"
                                          (state-cursor state))
    (wlr-output-commit output)))

(define (keyboard-destroy-notify keyboard)
  (remove-wl-listener (keyboard-key keyboard))
  (remove-wl-listener (keyboard-destroy keyboard))
  (free-wl-listener (keyboard-key keyboard))
  (free-wl-listener (keyboard-destroy keyboard)))

(define (new-input-notify state device)
  (cond
    ((memq (wlr-input-device-type device)
           (list wlr-input-device/pointer
                 wlr-input-device/touch
                 wlr-input-device/tablet-tool))
      (wlr-cursor-attach-input-device (state-cursor state) device))
    ((= (wlr-input-device-type device) wlr-input-device/keyboard)
      (let ((keyboard (make-keyboard state device #f #f)))
        (keyboard-key-set! keyboard
          (make-wl-listener (lambda (event) (keyboard-key-notify keyboard event))))
        (wl-signal-add (wlr-keyboard-events-key (wlr-input-device-keyboard device))
                       (keyboard-key keyboard))
        (keyboard-destroy-set! keyboard
          (make-wl-listener (lambda (_) (keyboard-destroy-notify keyboard))))
        (wl-signal-add (wlr-input-device-events-destroy device) (keyboard-destroy keyboard))
        (let* ((context (or (make-xkb-context)
                            (exit/error "Failed to create XKB context~%")))
               (keymap  (or (make-xkb-keymap context)
                            (exit/error "Failed to create XKB keymap~%"))))
          (wlr-keyboard-set-keymap (wlr-input-device-keyboard device) keymap)
          (xkb-keymap-unref keymap)
          (xkb-context-unref context))))))

(wlr-log-init 'debug)
(let* ((display (wl-display-create))
       (backend (or (wlr-backend-autocreate display #f)
                    (exit/error "Failed to create backend~%")))
       (cursor (wlr-cursor-create))
       (layout (wlr-output-layout-create))
       (cursor-manager (or (wlr-xcursor-manager-create "default" 24)
                           (exit/error "Failed to load left_ptr cursor")))
       (state (make-state display cursor-manager cursor
                          (f32vector 0.25 0.25 0.25 1.0)
                          (f32vector 0.25 0.25 0.25 1.0)
                          layout
                          '()
                          #f
                          '())))

  (wlr-cursor-attach-output-layout cursor layout)

  ; pointer events
  (wl-signal-add (wlr-cursor-events-motion cursor)
    (make-wl-listener (lambda (event) (handle-cursor-motion state event))))
  (wl-signal-add (wlr-cursor-events-motion-absolute cursor)
    (make-wl-listener (lambda (event) (handle-cursor-motion-absolute state event))))
  (wl-signal-add (wlr-cursor-events-button cursor)
    (make-wl-listener (lambda (event) (handle-cursor-button state event))))
  (wl-signal-add (wlr-cursor-events-axis cursor)
    (make-wl-listener (lambda (event) (handle-cursor-axis state event))))

  ; touch events
  (wl-signal-add (wlr-cursor-events-touch-up cursor)
    (make-wl-listener (lambda (event) (handle-touch-up state event))))
  (wl-signal-add (wlr-cursor-events-touch-down cursor)
    (make-wl-listener (lambda (event) (handle-touch-down state event))))
  (wl-signal-add (wlr-cursor-events-touch-motion cursor)
    (make-wl-listener (lambda (event) (handle-touch-motion state event))))
  (wl-signal-add (wlr-cursor-events-touch-cancel cursor)
    (make-wl-listener (lambda (event) (handle-touch-motion state event))))

  ; tool events
  (wl-signal-add (wlr-cursor-events-tablet-tool-axis cursor)
    (make-wl-listener (lambda (event) (handle-tablet-tool-axis state event))))

  ; backend events
  (wl-signal-add (wlr-backend-events-new-input backend)
    (make-wl-listener (lambda (device) (new-input-notify state device))))
  (wl-signal-add (wlr-backend-events-new-output backend)
    (make-wl-listener (lambda (output) (new-output-notify state output))))

  (wlr-xcursor-manager-set-cursor-image cursor-manager "left_ptr" cursor)

  (state-last-frame-set! state (clock-gettime))
  (unless (wlr-backend-start backend)
    (wlr-backend-destroy backend)
    (exit/error "Failed to start backend~%"))

  (wl-display-run display)
  (wl-display-destroy display)

  (wlr-xcursor-manager-destroy (state-xcursor-manager state))
  (wlr-cursor-destroy (state-cursor state))
  (wlr-output-layout-destroy (state-layout state)))
