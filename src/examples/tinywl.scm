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

;;
;; TinyWL implementation in Scheme.
;; Note that this is a more or less direct translation from C.
;;

(import (scheme base)
        (scheme process-context)
        (srfi 1)
        (srfi 4)
        (srfi 17)
        (srfi 28) ; format
        (chicken bitwise)
        (only (chicken process) process-run)
        (only (chicken process-context)
              set-environment-variable!
              program-name)
        (slib getopt)
        (wayland-server)
        (wlr backend)
        (wlr render wlr-renderer)
        (wlr time) ; clock-gettime
        (wlr types wlr-box)
        (wlr types wlr-compositor)
        (wlr types wlr-cursor)
        (wlr types wlr-data-device)
        (wlr types wlr-input-device)
        (wlr types wlr-keyboard)
        (wlr types wlr-matrix)
        (wlr types wlr-output)
        (wlr types wlr-output-layout)
        (wlr types wlr-pointer)
        (wlr types wlr-seat)
        (wlr types wlr-surface)
        (wlr types wlr-xcursor-manager)
        (wlr types wlr-xdg-shell)
        (wlr util edges)
        (wlr util log)
        (xkbcommon))

(include "xkbcommon-keysyms.scm")

(define-record-type tinywl-server
  (%make-server display backend renderer
                xdg-shell views
                cursor cursor-manager cursor-mode
                seat keyboards grabbed-view grab-x grab-y grab-width grab-height resize-edges
                output-layout outputs)
  server?
  (display        server:display        server:display-set!)
  (backend        server:backend        server:backend-set!)
  (renderer       server:renderer       server:renderer-set!)

  (xdg-shell      server:xdg-shell      server:xdg-shell-set!)
  (views          server:views          server:views-set!)

  (cursor         server:cursor         server:cursor-set!)
  (cursor-manager server:cursor-manager server:cursor-manager-set!)
  (cursor-mode    server:cursor-mode    server:cursor-mode-set!)

  (seat           server:seat           server:seat-set!)
  (keyboards      server:keyboards      server:keyboards-set!)
  (grabbed-view   server:grabbed-view   server:grabbed-view-set!)
  (grab-x         server:grab-x         server:grab-x-set!)
  (grab-y         server:grab-y         server:grab-y-set!)
  (grab-width     server:grab-width     server:grab-width-set!)
  (grab-height    server:grab-height    server:grab-height-set!)
  (resize-edges   server:resize-edges   server:resize-edges-set!)

  (output-layout  server:output-layout  server:output-layout-set!)
  (outputs        server:outputs        server:outputs-set!))

(define (make-server display backend renderer xdg-shell cursor cursor-manager seat olayout)
  (%make-server display backend renderer
                xdg-shell '()
                cursor cursor-manager 'passthrough
                seat '() #f 0 0 0 0 0
                olayout '()))

(define-record-type tinywl-output
  (make-tinywl-output server output)
  tinywl-output?
  (server tinywl-output:server tinywl-output:server-set!)
  (output tinywl-output:output tinywl-output:output-set!))

(define-record-type tinywl-view
  (make-tinywl-view server xdg-surface mapped x y)
  tinywl-view?
  (server      tinywl-view:server      tinywl-view:server-set!)
  (xdg-surface tinywl-view:xdg-surface tinywl-view:xdg-surface-set!)
  (mapped      tinywl-view:mapped?     tinywl-view:mapped?-set!)
  (x           tinywl-view:x           tinywl-view:x-set!)
  (y           tinywl-view:y           tinywl-view:y-set!))

(define-record-type tinywl-keyboard
  (make-tinywl-keyboard server device)
  tinywl-keyboard?
  (server    tinywl-keyboard:server    tinywl-keyboard:server-set!)
  (device    tinywl-keyboard:device    tinywl-keyboard:device-set!))

(define (focus-view view wlr-surface)
  ; Note: this function only deals with keyboard focus.
  (when view
    (let* ((server (tinywl-view:server view))
           (seat (server:seat server))
           (prev-surface (wlr-seat-keyboard-state-focused-surface
                           (wlr-seat-keyboard-state seat))))
      (unless (equal? wlr-surface prev-surface)
        (when prev-surface
          ; Deactivate the previously focused surface. This lets the client know
          ; it no longer has focus and the client will repaint accordingly, e.g.
          ; stop displaying a caret.
          (wlr-xdg-toplevel-set-activated (wlr-xdg-surface-from-wlr-surface prev-surface) #f))
        ; Move the view to the front
        (server:views-set! server
          (append (delete! view (server:views server))
                  (list view)))
        ; Activate the new surface
        (wlr-xdg-toplevel-set-activated (tinywl-view:xdg-surface view) #t)
        ; Tell the seat to have the keyboard enter this surface. wlroots will keep
        ; track of this and automatically send key events to the appropriate
        ; clients without additional work on your part.
        (let ((keyboard (wlr-seat-get-keyboard seat)))
          (wlr-seat-keyboard-notify-enter seat
            (wlr-xdg-surface-surface (tinywl-view:xdg-surface view))
            (wlr-keyboard-keycodes keyboard)
            (wlr-keyboard-num-keycodes keyboard)
            (wlr-keyboard-modifiers keyboard)))))))

; This event is raised when a modifier key, such as shift or alt, is
; pressed. We simply comminucate this to the client.
(define (keyboard-handle-modifiers keyboard)
  (let ((seat (server:seat (tinywl-keyboard:server keyboard)))
        (wlr-keyboard (wlr-input-device-keyboard (tinywl-keyboard:device keyboard))))
    ; A seat can only have one keyboard, but this is a limitation of the
    ; wayland protocol - not wlroots. We assign all connected keyboards to the
    ; same seat. You can swap out the underlying wlr-keyboard like this and
    ; wlr-seat handles this transparently
    (wlr-seat-set-keyboard seat (tinywl-keyboard:device keyboard))
    ; Send modifiers to the client.
    (wlr-seat-keyboard-notify-modifiers seat (wlr-keyboard-modifiers wlr-keyboard))))

; Here we handle compositor keybindings. This is when the compositor is
; processing keys, rather than passing them to the client for its own
; processing.
;
; This function assumes Alt is held down.
(define (handle-keybinding server sym)
  (cond
    ((or (= sym xkb-key-escape)
         (= sym xkb-key-f2))
      (wl-display-terminate (server:display server))
      #t)
    ((= sym xkb-key-f1)
      ; cycle to the next view
      (let* ((views (server:views server))
             (nr-views (length views)))
        (when (> nr-views 1)
          (let* ((next-view (car (server:views server)))
                 (surface (wlr-xdg-surface-surface (tinywl-view:xdg-surface next-view))))
            (focus-view next-view surface))))
      #t)
    (else #f)))

(define (keyboard-handle-key keyboard event)
  (let* ((server (tinywl-keyboard:server keyboard))
         (seat (server:seat server))
         (keycode (+ 8 (wlr-event-keyboard-key-keycode event)))
         (wlr-keyboard (wlr-input-device-keyboard (tinywl-keyboard:device keyboard)))
         (syms (xkb-state-key-get-syms (wlr-keyboard-xkb-state wlr-keyboard) keycode))
         (modifiers (wlr-keyboard-get-modifiers wlr-keyboard))
         (handled #f))
    (when (and (positive? (bitwise-and modifiers wlr-modifier/alt))
               (= (wlr-event-keyboard-key-state event) wlr-key/pressed))
      ; If alt is held down and this button was _pressed_, we attempt to
      ; process it as a compositor keybinding.
      (for-each (lambda (sym) (set! handled (handle-keybinding server sym)))
                syms))
    
    (unless handled
      ; Otherwise, we pass it along to the client.
      (wlr-seat-set-keyboard seat (tinywl-keyboard:device keyboard))
      (wlr-seat-keyboard-notify-key seat
                                    (wlr-event-keyboard-key-time-msec event)
                                    (wlr-event-keyboard-key-keycode event)
                                    (wlr-event-keyboard-key-state event)))))

(define (server-new-keyboard server device)
  ; We need to prepare an XKB keymap and assign it to the keybaord. This
  ; assumes the defaults (e.g. layout = "us").
  (let* ((keyboard (make-tinywl-keyboard server device))
         (wlr-keyboard (wlr-input-device-keyboard device))
         (context (make-xkb-context)) 
         (keymap (make-xkb-keymap context)))
    (wlr-keyboard-set-keymap (wlr-input-device-keyboard device) keymap)
    (xkb-keymap-unref keymap)
    (xkb-context-unref context)
    (wlr-keyboard-set-repeat-info wlr-keyboard 25 600)

    ; Here we set up listeners for keyboard events.
    (wl-signal-add (wlr-keyboard-events-modifiers wlr-keyboard)
      (make-wl-listener (lambda (_) (keyboard-handle-modifiers keyboard))))
    (wl-signal-add (wlr-keyboard-events-key wlr-keyboard)
      (make-wl-listener (lambda (event) (keyboard-handle-key keyboard event))))

    (wlr-seat-set-keyboard (server:seat server) device)

    ; Add the keyboard to our list of keyboards
    (server:keyboards-set! server (cons keyboard (server:keyboards server)))))

(define (server-new-pointer server device)
  ; We don't do anything special with pointers. All of out pointer handling
  ; is proxied through wlr-cursor. On another compositor, you might take this
  ; opportunity to do libinput configuration on the device to set
  ; acceleration, etc.
  (wlr-cursor-attach-input-device (server:cursor server) device))

; This event is raised by the backend when a new input device becomes
; available.
(define (server-new-input server device)
  (cond
    ; FIXME: should be a CASE form, but enum values not immediate
    ((= (wlr-input-device-type device) wlr-input-device/pointer)
      (server-new-pointer server device))
    ((= (wlr-input-device-type device) wlr-input-device/keyboard)
      (server-new-keyboard server device)))

  ; We need to let the wlr-seat know what our capabilities are, which is
  ; communicated to the client. In TinyWL we always have a cursor, even if
  ; there are no pointer devices, so we always include that capability.
  (let ((caps wl-seat-capability/pointer))
    (unless (null? (server:keyboards server))
      (set! caps (bitwise-ior caps wl-seat-capability/keyboard)))
    (wlr-seat-set-capabilities (server:seat server) caps)))

; This event is raised by the seat when a client provides a cursor image.
(define (seat-request-cursor server event)
  ; This can be sent by any client, so we check to make sure this one
  ; actually has pointer focus first.
  (if (equal? (wlr-seat-pointer-state-focused-client
                (wlr-seat-pointer-state (server:seat server)))
              (wlr-seat-pointer-request-set-cursor-event-seat-client event))
    ; Once we've vetted the client, we can tell the cursor to use the
    ; provided surface as the cursor image. It will set the hardware cursor
    ; on the output that it's currently on and continue to do so as the
    ; cursor moves between outputs.
    (wlr-cursor-set-surface (server:cursor server)
                            (wlr-seat-pointer-request-set-cursor-event-surface event)
                            (wlr-seat-pointer-request-set-cursor-event-hotspot-x event)
                            (wlr-seat-pointer-request-set-cursor-event-hotspot-y event))))

(define (view-at view lx ly)
  ; XDG toplevels may have nested surfaces, such as popup windows for context
  ; menus or tooltips. This function tests if any of those are underneath the
  ; coordinates lx and ly (in output Layout Coordinates). If so, it returns
  ; that wlr-surface and the coordinates relative to that surface's top-left
  ; corner.
  (wlr-xdg-surface-surface-at (tinywl-view:xdg-surface view)
                              (- lx (tinywl-view:x view))
                              (- ly (tinywl-view:y view))))

(define (desktop-view-at server lx ly)
  ; This iterates over all of our surfaces and attempts to find one under the
  ; cursor. This relies on server:views being ordered from top-to-bottom.
  (let loop ((views (reverse (server:views server))))
    (if (null? views)
      (values #f #f #f #f)
      (let-values (((surface sx sy) (view-at (car views) lx ly)))
        (if surface
          (values (car views) surface sx sy)
          (loop (cdr views)))))))

; Move the grabbed view to the new position.
(define (process-cursor-move server time)
  (tinywl-view:x-set! (server:grabbed-view server)
                      (- (wlr-cursor-x (server:cursor server))
                         (server:grab-x server)))
  (tinywl-view:y-set! (server:grabbed-view server)
                      (- (wlr-cursor-y (server:cursor server))
                         (server:grab-y server))))

; Resizing the grabbed view can be a little bit complicated, because we
; could be resizing from any corner or edge. This not only resizes the view
; on one or two axes, but can also move the view if you resize from the top
; or left edges (or top-left corner).
;
; Note that I took some shortcuts here. In a more fleshed-out compositor,
; you'd wait for the client to prepare a buffer at the new size, then
; commit any movement that was prepared.
(define (process-cursor-resize server time)
  (let* ((view (server:grabbed-view server))
         (dx (- (wlr-cursor-x (server:cursor server))
                (server:grab-x server)))
         (dy (- (wlr-cursor-y (server:cursor server))
                (server:grab-y server)))
         (x (tinywl-view:x view))
         (y (tinywl-view:y view))
         (width (server:grab-width server))
         (height (server:grab-height server))
         (edges (server:resize-edges server)))
    (cond
      ((positive? (bitwise-and edges wlr-edge/top))
        (set! y (+ (server:grab-y server) dy))
        (set! height (- height dy))
        (when (< height 1)
          (set! y (+ y height))))
      ((positive? (bitwise-and edges wlr-edge/bottom))
        (set! height (+ height dy))))
    (cond
      ((positive? (bitwise-and edges wlr-edge/left))
        (set! x (+ (server:grab-x server) dx))
        (set! width (- width dx))
        (when (< width 1)
          (set! x (+ x width))))
      ((positive? (bitwise-and edges wlr-edge/right))
        (set! width (+ width dx))))
    (tinywl-view:x-set! view x)
    (tinywl-view:y-set! view y)
    (wlr-xdg-toplevel-set-size (tinywl-view:xdg-surface view)
                               (max 0 (exact (truncate width)))
                               (max 0 (exact (truncate height))))))

(define (process-cursor-motion server time)
  ; If the mode is non-passthrough, delegate to those functions.
  (case (server:cursor-mode server)
    ((move) (process-cursor-move server time))
    ((resize) (process-cursor-resize server time))
    (else
      ; Otherwise, find the view under the pointer and send the event along
      (let*-values (((cursor) (server:cursor server))
                    ((seat) (server:seat server))
                    ((view surface sx sy) (desktop-view-at server
                                                           (wlr-cursor-x cursor)
                                                           (wlr-cursor-y cursor))))
        (unless view
          ; If there's no view under the cursor, set the cursor image to a
          ; default. This is what makes the cursor image appear when you move it
          ; around the screen, not over any views
          (wlr-xcursor-manager-set-cursor-image (server:cursor-manager server)
                                                "left_ptr"
                                                cursor))
        (if surface
          (let ((focus-changed (not (equal? (wlr-seat-pointer-state-focused-surface
                                              (wlr-seat-pointer-state seat))
                                            surface))))
            ; "Enter" the surface if necessary. This lets the client know that the
            ; cursor has entered one of its surfaces.
            ;
            ; Note that this gives the surface "pointer focus", which is distinct
            ; from keyboard focus. You get pointer focus by moving the pointer over
            ; a window.
            (wlr-seat-pointer-notify-enter seat surface sy sy)
            (when (not focus-changed)
              ; The enter event contains coordinates, so we only need to notify
              ; on motion if the focus did not change.
              (wlr-seat-pointer-notify-motion seat time sx sy)))
          ; Clear pointer focus so future button events and such are not sent to
          ; the last client to have the cursor over it.
          (wlr-seat-pointer-clear-focus seat))))))

; This event is forwarded by the cursor when a pointer emits a _relative_
; pointer motion event (i.e. a delta).
(define (server:cursor-motion server event)
  ; The cursor doesn't move unless we tell it to. The cursor automatically
  ; handles constraining the motion to the output layout, as well as any
  ; special configuration applid for the specific input device which
  ; generated the event. You can pass #f for the device if you want to move
  ; the cursor around without any input.
  (wlr-cursor-move (server:cursor server)
                   (wlr-event-pointer-motion-device event)
                   (wlr-event-pointer-motion-delta-x event)
                   (wlr-event-pointer-motion-delta-y event))
  (process-cursor-motion server (wlr-event-pointer-motion-time-msec event)))

; This event is forwarded by the cursor when a pointer emits an _absolute_
; motion event, from 0..1 on each axis. This happens, for example, when
; wlroots is running under a Wayland window rather than KMS+DRM, and you
; move the mouse over the window. You could enter the window from any edge,
; so we have to warp the mouse there. There is also some hardware which
; emits these events.
(define (server:cursor-motion-absolute server event)
  (wlr-cursor-warp-absolute (server:cursor server)
                            (wlr-event-pointer-motion-absolute-device event)
                            (wlr-event-pointer-motion-absolute-x event)
                            (wlr-event-pointer-motion-absolute-y event))
  (process-cursor-motion server (wlr-event-pointer-motion-absolute-time-msec event)))

; This event is forwarded by the cursor when a pointer emits a button event.
(define (server:cursor-button server event)
  ; Notify the client with a pointer focus that a button press has occurred.
  (wlr-seat-pointer-notify-button (server:seat server)
                                  (wlr-event-pointer-button-time-msec event)
                                  (wlr-event-pointer-button-button event)
                                  (wlr-event-pointer-button-state event))
  (let*-values (((cursor) (server:cursor server))
                ((view surface sx sy) (desktop-view-at server
                                                       (wlr-cursor-x cursor)
                                                       (wlr-cursor-y cursor))))
    (if (= (wlr-event-pointer-button-state event)
           wlr-button/released)
      ; If you released any buttons, we exit interactive move/resize mode.
      (server:cursor-mode-set! server 'passthrough)
      ; Focus the client if the button was _pressed_.
      (focus-view view surface))))

; This event is forwarded by the cursor when a pointer emits an axis event,
; for example when you move the scroll wheel.
(define (server:cursor-axis server event)
  ; Notify the client with a pointer focus of the axis event.
  (wlr-seat-pointer-notify-axis (server:seat server)
                                (wlr-event-pointer-axis-time-msec event)
                                (wlr-event-pointer-axis-orientation event)
                                (wlr-event-pointer-axis-delta event)
                                (wlr-event-pointer-axis-delta-discrete event)
                                (wlr-event-pointer-axis-source event)))

; This function is called for every surface that needs to be rendered.
(define (render-surface surface sx sy output view renderer time)
  ; We first obtain a wlr-texture, which is a GPU resource. wlroots
  ; automatically handles negotiating these with the client. The underlying
  ; resource could be an opaque handle passed from the client, or the client
  ; could have sent a pixel buffer which we copied to the GPU, or a few other
  ; means. You don't have to worry about this, wlroots takes care of it.
  (let ((texture (wlr-surface-get-texture surface)))
    (when texture
      ; The view has a position in layout coordinates. If you have two displays,
      ; one next to the other, both 1080p, a view on the rightmost display might
      ; have layout coordinates of 2000,100. We need to translate that to
      ; output-local coordinates, or (2000 - 1920).
      (let*-values (((x y) (wlr-output-layout-output-coords
                             (server:output-layout (tinywl-view:server view))
                             output 0 0))
                    ((w)   (wlr-surface-state-width (wlr-surface-current surface)))
                    ((h)   (wlr-surface-state-height (wlr-surface-current surface)))
                    ; We also have to apply the scale factor for HiDPI outputs. This is only
                    ; part of the puzzle. TinyWL does not fully support HiDPI.
                    ((box) (make-wlr-box (exact (truncate (* (+ x  sx (tinywl-view:x view))
                                                             (wlr-output-scale output))))
                                         (exact (truncate (* (+ y  sy (tinywl-view:y view))
                                            (wlr-output-scale output))))
                                         (exact (truncate (* w (wlr-output-scale output))))
                                         (exact (truncate (* h (wlr-output-scale output))))))
                    ((mat) (make-wlr-matrix)))
        ; Those familiar with OpenGL are also familiar with the role of matrices
        ; in graphics programming. We need to prepare a matrix to render the view
        ; with. wlr-matrix-project-box is a helper which takes a box with a desired
        ; x, y coordinates, width and height, and an output geometry, then
        ; prepares an orthographic projection and multiplies the necessary
        ; transforms to produce a model-view-projection matrix.
        ;
        ; Naturally you can do this any way you like, for example to make a 3D
        ; compositor.
        (let ((transform (wlr-output-transform-invert
                           (wlr-surface-state-transform (wlr-surface-current surface)))))
          (wlr-matrix-project-box mat box transform 0 (wlr-output-transform-matrix output)))

        ; This takes our matrix, the texture, and an alpha, and performs the actual
        ; rendering on the GPU.
        (wlr-render-texture-with-matrix renderer texture mat 1)

        ; This lets the client know that we've displayed that frame and it can
        ; prepare another one now if it likes.
        (wlr-surface-send-frame-done surface time)))))

(define clear-color (f32vector 0.3 0.3 0.3 1.0))

; This function is called every time an output is ready to display a frame,
; generally at the output's refresh rate (e.g. 60Hz).
(define (output-frame server wm-output output)
  (let-values (((renderer) (server:renderer server))
               ((now) (clock-gettime))
               ; wlr-output-make-current makes the OpenGL context current.
               ((success buffer-age) (wlr-output-make-current output)))
    (when success
      ; The "effective" resolution can change if you rotate your inputs
      (let-values (((width height) (wlr-output-effective-resolution output)))
        ; Begin the renderer (calls glViewport and some other GL sanity checks)
        (wlr-renderer-begin renderer width height))
      (wlr-renderer-clear renderer clear-color)

      ; Each subsequent window we render is rendered on top of the last.
      (for-each (lambda (view)
                  ; An unmapped view should not be rendered.
                  (when (tinywl-view:mapped? view)
                    ; This calls our render-surface function for each surface among the
                    ; xdg-surface's toplevel and popups.
                    (wlr-xdg-surface-for-each-surface (tinywl-view:xdg-surface view)
                      (lambda (surface sx sy)
                        (render-surface surface sx sy
                                        (tinywl-output:output wm-output)
                                        view
                                        renderer
                                        now)))))
                (server:views server))

      ; Hardware cursors are rendered by the GPU on a separate plane, and can be
      ; moved around without re-rendering what's beneath them - which is more
      ; efficient. However, not all hardware supports hardware cursors. For this
      ; reason, wlroots provides a software fallback, which we ask it to render
      ; here. wlr-cursor handles configuring hardware vs software cursors for you,
      ; and this function is a no-op when hardware cursors are in use.
      (wlr-output-render-software-cursors output #f)
      ; conclude rendering and swap the buffers, showing the final frame on-screen.
      (wlr-renderer-end renderer)
      (wlr-output-swap-buffers output #f #f))))

; This event is raised by the backend when a new output (aka a display or
; monitor) becomes available. 
(define (server-new-output server output)
  ; some backends don't have modes. DRM+KMS does, and we need to set a mode
  ; before we can use the output. The mode is a tuple of (width, height,
  ; refresh rate), and each monitor supports only a specific set of modes. We
  ; just pick the first, a more sophisticated compositor would let the user
  ; configure it or pick the mode the display advertises as preferred.
  (unless (wl-list-empty? (wlr-output-%modes output))
    (wlr-output-set-mode output (last (wlr-output-modes output))))

  ; Allocates and configures out state for this output
  (let ((wm-output (make-tinywl-output server output)))
    (wl-signal-add (wlr-output-events-frame output)
                   (make-wl-listener (lambda (_) (output-frame server wm-output output))))
    (server:outputs-set! server (cons wm-output (server:outputs server))))

  ; Adds this to the output layout. The ADD-AUTO function arranges outputs
  ; from left-to-right in the order they appear.  A more sophisticated
  ; compositor would let the user configure the arrangement of the outputs in the
  ; layout.
  (wlr-output-layout-add-auto (server:output-layout server) output)

  ; Creating the global adds a wl_output global to the display, which Wayland
  ; clients can see to find out information about the output (such as DPI,
  ; scale factor, manufacturer, etc).
  (wlr-output-create-global output))

; Called when the surface is mapped, or ready to display on-screen.
(define (xdg-surface-map view)
  (tinywl-view:mapped?-set! view #t)
  (focus-view view (wlr-xdg-surface-surface (tinywl-view:xdg-surface view))))

; Called when the surface is unmapped, and should no longer be shown.
(define (xdg-surface-unmap view)
  (tinywl-view:mapped?-set! view #f))

; Called when the surface is destroyed and should never be shown again.
(define (xdg-surface-destroy server view)
  (server:views-set! server (delete! view (server:views server))))

; This function sets up an interactive move or resize operation, where the
; compositor stops propegating pointer events to clients and instead
; consumes them itself, to move or resize windows.
(define (begin-interactive view mode edges)
  (let* ((server (tinywl-view:server view))
         (cursor (server:cursor server)))
    ; Deny move/resize requests from unfocused clients.
    (when (equal? (wlr-xdg-surface-surface (tinywl-view:xdg-surface view))
                  (wlr-seat-pointer-state-focused-surface
                    (wlr-seat-pointer-state (server:seat server))))
      (server:grabbed-view-set! server view)
      (server:cursor-mode-set! server mode)
      (let ((geo-box (wlr-xdg-surface-get-geometry (tinywl-view:xdg-surface view))))
        (if (symbol=? mode 'move)
          (begin
            (server:grab-x-set! server (- (wlr-cursor-x cursor) (tinywl-view:x view)))
            (server:grab-y-set! server (- (wlr-cursor-y cursor) (tinywl-view:y view))))
          (begin
            (server:grab-x-set! server (+ (wlr-cursor-x cursor) (wlr-box-x geo-box)))
            (server:grab-y-set! server (+ (wlr-cursor-y cursor) (wlr-box-y geo-box)))))
        (server:grab-width-set! server (wlr-box-width geo-box))
        (server:grab-height-set! server (wlr-box-height geo-box))
        (server:resize-edges-set! server edges)))))

; This event is raised when a client would like to begin an interactive
; move, typically because the user clicked on their client-side
; decorations. Note that a more sophisticated compositor should check the
; provided serial against a list of button press serials sent to this
; client, to prevent the client from requesting this whenever they want.
(define (xdg-toplevel-request-move view)
  (begin-interactive view 'move 0))

; This event is raised when a client would like to begin an interactive
; resize, typically because the user clicked on their client-side
; decorations. Note that a more sophisticated compositor should check the
; provided serial against a list of button press serials sent to this
; client, to prevent the client from requesting this whenever they want.
(define (xdg-toplevel-request-resize view event)
  (begin-interactive view
                     'resize
                     (wlr-xdg-toplevel-resize-event-edges event)))

;; This event is raised when wlr-xdg-shell receives a new xdg surface from a
;; client, either a toplevel (application window) or popup. 
(define (server-new-xdg-surface server xdg-surface)
  (when (= (wlr-xdg-surface-role xdg-surface)
           wlr-xdg-surface-role/toplevel)
    ; Allocate a view for this surface.
    (let ((view (make-tinywl-view server xdg-surface #f 0 0)))
      ; Listen to the various events it can emit.
      (wl-signal-add (wlr-xdg-surface-events-map xdg-surface)
                     (make-wl-listener (lambda (_) (xdg-surface-map view))))
      (wl-signal-add (wlr-xdg-surface-events-unmap xdg-surface)
                     (make-wl-listener (lambda (_) (xdg-surface-unmap view))))
      (wl-signal-add (wlr-xdg-surface-events-destroy xdg-surface)
                     (make-wl-listener (lambda (_) (xdg-surface-destroy server view))))
      (wl-signal-add (wlr-xdg-toplevel-events-request-move (wlr-xdg-surface-toplevel xdg-surface))
                     (make-wl-listener (lambda (_) (xdg-toplevel-request-move view))))
      (wl-signal-add (wlr-xdg-toplevel-events-request-resize (wlr-xdg-surface-toplevel xdg-surface))
                     (make-wl-listener (lambda (event) (xdg-toplevel-request-resize view event))))
      ; Add it to the list of views.
      (server:views-set! server (append (server:views server) (list view))))))

(define (usage)
  (format #t "Usage: ~a [-s startup-command]~%" (program-name))
  (exit))

(define startup-command #f)
(let loop ()
  (case (getopt ":s:")
    ((#\s) (set! startup-command (option-arg)))
    ((#\:) (format #t "Missing argument to -~a option~%" (option-name))
           (usage))
    ((#f)  (if (< (option-index) (length (command-line)))
             (format #t "argv[~a]=~s" (option-index) (list-ref (command-line) (option-index))))
           (option-index (+ 1 (option-index))))
    (else (format #t "Unrecognized option: -~a~%" (option-name))
          (usage)))
  (if (< (option-index) (length (command-line)))
    (loop)))

(wlr-log-init 'debug)

       ; The wayland display is managed by libwayland. It handles accepting
       ; clients from the Unix socket, managing Wayland globals, and so on.
(let* ((display (wl-display-create))
       ; The backend is a wlroots feature which abstracts the underlying input and
       ; output hardware. The autocreate option will choose the most suitable
       ; backend based on the current environment, such as opening an X11 window
       ; if an X11 server is running. The #f argument here optionally allows you
       ; to pass in a custom renderer if wlr-renderer doesn't meet your needs. The
       ; backend uses the renderer, for example, to fall back to software cursors
       ; if the backend does not support hardware cursors (some older GPUs don't).
       (backend (wlr-backend-autocreate display #f))
       ; If we don't provide a renderer, autocreate makes a GLES2 renderer for us.
       (renderer (wlr-backend-get-renderer backend))
       ; Creates an output layout, which is a wlroots utility for working with an
       ; arrangement of screens in a physical layout.
       (olayout (wlr-output-layout-create))
       ; The xdg-shell is a Wayland protocol which is used for application windows.
       (xdg-shell (wlr-xdg-shell-create display))
       ; Creates a cursor, which is a wlroots utility for tracking the cursor
       ; image shown on screen.
       (cursor (wlr-cursor-create))
       ; Creates an xcursor manager, another wlroots utility which loads up
       ; xcursor themes to source cursor images from and makes sure that cursor
       ; images are available at all scale factors on the screen (necessary for
       ; HiDPI support).
       (cursor-manager (wlr-xcursor-manager-create #f 24))
       ; Allocate a seat
       (seat (wlr-seat-create display "seat0"))
       ; Allocate a server object
       (server (make-server display backend renderer xdg-shell cursor cursor-manager seat olayout)))
  ; The renderer is responsible for defining the various pixel formats it
  ; supports for shared memory. This configures that for clients.
  (wlr-renderer-init-wl-display renderer display)
  ; This creates some hands-off wlroots interfaces. The compositor is
  ; necessary for clients to allocate surfaces and the data device manager
  ; handles the clipboard. Each of these wlroots interfaces has room for you
  ; to dig your fingers in and play with their beharior if you want.
  (wlr-compositor-create display renderer)
  (wlr-data-device-manager-create display)
  ; Configure a listener to be notified when new outputs are available on the backend.
  (wl-signal-add (wlr-backend-events-new-output backend)
    (make-wl-listener (lambda (output) (server-new-output server output))))
  ; Configure a listener to be notified when we recieve a new surface.
  (wl-signal-add (wlr-xdg-shell-events-new-surface xdg-shell)
    (make-wl-listener (lambda (xdg-surface) (server-new-xdg-surface server xdg-surface))))

  ; Attach out output layout to the wlr-cursor.
  (wlr-cursor-attach-output-layout cursor olayout)

  ; Add a cursor theme at scale factor 1 to begin with.
  (wlr-xcursor-manager-load cursor-manager 1)

  ; wlr-cursor *only* displays an image on screen. It does not move around
  ; when the pointer moves. However, we can attach input devices to it, and
  ; it will generate aggregate events for all of them. In these events, we
  ; can choose how we want to process them, forwarding them to clients and
  ; moving the cursor around.
  (wl-signal-add (wlr-cursor-events-motion cursor)
    (make-wl-listener (lambda (event) (server:cursor-motion server event))))
  (wl-signal-add (wlr-cursor-events-motion-absolute cursor)
    (make-wl-listener (lambda (event) (server:cursor-motion-absolute server event))))
  (wl-signal-add (wlr-cursor-events-button cursor)
    (make-wl-listener (lambda (event) (server:cursor-button server event))))
  (wl-signal-add (wlr-cursor-events-axis cursor)
    (make-wl-listener (lambda (event) (server:cursor-axis server event))))

  ; Configures a seat, which is a single "seat" at which the user sits and
  ; operates the computer. This conceptually includes up to one keyboard,
  ; pointer, touch, and drawing tablet device. We also rig up a listener to
  ; let us know when new input devices are available on the backend.
  (wl-signal-add (wlr-backend-events-new-input backend)
    (make-wl-listener (lambda (input-device) (server-new-input server input-device))))
  (wl-signal-add (wlr-seat-events-request-set-cursor seat)
    (make-wl-listener (lambda (event) (seat-request-cursor server event))))

  ; Add a Unix socket to the Wayland display.
  (let ((socket (wl-display-add-socket-auto display)))
    (unless socket
      (wlr-backend-destroy backend)
      (exit 1))

    ; Start the backend. This will enumerate outputs and inputs, become the DRM
    ; master, etc.
    (unless (wlr-backend-start backend)
      (wlr-backend-destroy backend)
      (wl-display-destroy display))

    ; Set the WAYLAND_DISPLAY environment variable to our socket and run the
    ; startup command if requested.
    (set-environment-variable! "WAYLAND_DISPLAY" socket) 
    (when startup-command
      (process-run "/bin/sh" `("-c" ,startup-command)))

    ; Run the Wayland event loop. This does not return until you exit the
    ; compositor. Starting the backend rigged up all of the necessary event
    ; loop configuration to listen to libinput events, DRM events, generate
    ; frame events at the refresh rate, and so on.
    ;(format #t "Running Wayland compositor on WAYLAND_DISPLAY=~a~%" socket)
    (wl-display-run display)
    (wl-display-destroy-clients display)
    (wl-display-destroy display)))
