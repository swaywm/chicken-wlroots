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

(define-foreign-type scheme-wl-listener* (c-pointer (struct "scheme_wl_listener")))

(define-foreign-type wl-argument (union "wl_argument"))
(define-foreign-type wl-argument* (c-pointer wl-argument))

(define-foreign-type wl-array (struct "wl_array"))
(define-foreign-type wl-array* (c-pointer wl-array))

(define-foreign-type wl-client (struct "wl_client"))
(define-foreign-type wl-client* (c-pointer wl-client))

(define-foreign-type wl-display (struct "wl_display"))
(define-foreign-type wl-display* (c-pointer wl-display))

(define-foreign-type wl-event-loop (struct "wl_event_loop"))
(define-foreign-type wl-event-loop* (c-pointer wl-event-loop))

(define-foreign-type wl-event-source (struct "wl_event_source"))
(define-foreign-type wl-event-source* (c-pointer wl-event-source))

(define-foreign-type wl-global (struct "wl_global"))
(define-foreign-type wl-global* (c-pointer wl-global))

(define-foreign-type wl-interface (struct "wl_interface"))
(define-foreign-type wl-interface* (c-pointer wl-interface))

(define-foreign-type wl-list (struct "wl_list"))
(define-foreign-type wl-list* (c-pointer wl-list))

(define-foreign-type wl-listener (struct "wl_listener"))
(define-foreign-type wl-listener* (c-pointer wl-listener))

(define-foreign-type wl-message (struct "wl_message"))
(define-foreign-type wl-message* (c-pointer wl-message))

(define-foreign-type wl-protocol-logger (struct "wl_protocol_logger"))
(define-foreign-type wl-protocol-logger* (c-pointer wl-protocol-logger))

(define-foreign-type wl-protocol-logger-message (struct "wl_protocol_logger_message"))
(define-foreign-type wl-protocol-logger-message* (c-pointer wl-protocol-logger-message))

(define-foreign-type wl-resource (struct "wl_resource"))
(define-foreign-type wl-resource* (c-pointer wl-resource))

(define-foreign-type wl-shm-buffer (struct "wl_shm_buffer"))
(define-foreign-type wl-shm-buffer* (c-pointer wl-shm-buffer))

(define-foreign-type wl-shm-pool (struct "wl_shm_pool"))
(define-foreign-type wl-shm-pool* (c-pointer wl-shm-pool))

(define-foreign-type wl-signal (struct "wl_signal"))
(define-foreign-type wl-signal* (c-pointer wl-signal))

(define-foreign-type wl-data-device-manager-dnd-action (enum "wl_data_device_manager_dnd_action"))
(define-foreign-type wl-iterator-result (enum "wl_iterator_result"))
(define-foreign-type wl-output-transform (enum "wl_output_transform"))
(define-foreign-type wl-protocol-logger-type (enum "wl_protocol_logger_type"))
(define-foreign-type wl-seat-capability (enum "wl_seat_capability"))
(define-foreign-type wl-shm-format (enum "wl_shm_format"))
