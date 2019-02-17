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

(foreign-declare "#include <wlr/types/wlr_keyboard.h>")
(include "wlroots-types.scm")

(module (wlr types wlr-keyboard)
        (wlr-led/num-lock
         wlr-led/caps-lock
         wlr-led/scroll-lock

         wlr-modifier/shift
         wlr-modifier/caps
         wlr-modifier/ctrl
         wlr-modifier/alt
         wlr-modifier/mod2
         wlr-modifier/mod3
         wlr-modifier/logo
         wlr-modifier/mod5

         wlr-led-count
         wlr-modifier-count
         wlr-keyboard-keys-cap

         wlr-keyboard-modifiers-depressed
         wlr-keyboard-modifiers-latched
         wlr-keyboard-modifiers-locked
         wlr-keyboard-modifiers-group

         wlr-keyboard-impl
         wlr-keyboard-keymap-string
         wlr-keyboard-keymap-size
         wlr-keyboard-keymap
         wlr-keyboard-xkb-state
         wlr-keyboard-led-indexes
         wlr-keyboard-mod-indexes
         wlr-keyboard-keycodes
         wlr-keyboard-num-keycodes
         wlr-keyboard-modifiers
         wlr-keyboard-repeat-info-rate
         wlr-keyboard-repeat-info-delay
         wlr-keyboard-events-key
         wlr-keyboard-events-modifiers
         wlr-keyboard-events-keymap
         wlr-keyboard-events-repeat-info
         wlr-keyboard-data

         wlr-key/released
         wlr-key/pressed

         wlr-event-keyboard-key-time-msec
         wlr-event-keyboard-key-keycode
         wlr-event-keyboard-key-update-state
         wlr-event-keyboard-key-state

         wlr-keyboard-set-keymap
         wlr-keyboard-set-repeat-info
         wlr-keyboard-led-update
         wlr-keyboard-get-modifiers)
  (import (scheme)
          (chicken base))
  (include "ffi-helpers.scm")

  (bind-file "include/bind/wlr/types/wlr_keyboard.h")

  (define-foreign-type xkb-mod-mask unsigned-int32)
  (define-foreign-type xkb-led-index unsigned-int32)
  (define-foreign-type xkb-mod-index unsigned-int32)

  (define-foreign-values wlr-keyboard-led
    (wlr-led/num-lock    "WLR_LED_NUM_LOCK")
    (wlr-led/caps-lock   "WLR_LED_CAPS_LOCK")
    (wlr-led/scroll-lock "WLR_LED_SCROLL_LOCK"))

  (define-foreign-values wlr-keyboard-modifier
    (wlr-modifier/shift "WLR_MODIFIER_SHIFT")
    (wlr-modifier/caps  "WLR_MODIFIER_CAPS")
    (wlr-modifier/ctrl  "WLR_MODIFIER_CTRL")
    (wlr-modifier/alt   "WLR_MODIFIER_ALT")
    (wlr-modifier/mod2  "WLR_MODIFIER_MOD2")
    (wlr-modifier/mod3  "WLR_MODIFIER_MOD3")
    (wlr-modifier/logo  "WLR_MODIFIER_LOGO")
    (wlr-modifier/mod5  "WLR_MODIFIER_MOD5"))

  (define-foreign-values int
    (wlr-led-count         "WLR_LED_COUNT")
    (wlr-modifier-count    "WLR_MODIFIER_COUNT")
    (wlr-keyboard-keys-cap "WLR_KEYBOARD_KEYS_CAP"))

  (define-foreign-record-type (wlr-keyboard* "struct wlr_keyboard")
    ; XXX- see below
    ((c-pointer xkb-led-index) led_indexes %wlr-keyboard-led-indexes)
    ((c-pointer xkb-mod-index) mod_indexes %wlr-keyboard-mod-indexes)
    ((c-pointer unsigned-int32) keycodes %wlr-keyboard-keycodes)
    ((struct "wlr_modifiers") modifiers wlr-keyboard-modifiers)
    (int32 repeat_info.rate wlr-keyboard-repeat-info-rate)
    (int32 repeat_info.delay wlr-keyboard-repeat-info-delay)
    ((struct "wl_signal") events.key wlr-keyboard-events-key)
    ((struct "wl_signal") events.modifiers wlr-keyboard-events-modifiers)
    ((struct "wl_signal") events.keymap wlr-keyboard-events-keymap)
    ((struct "wl_signal") events.repeat_info wlr-keyboard-events-repeat-info))

  (define wlr-keyboard-led-indexes
    (case-lambda
      ((keyboard) (%wlr-keyboard-led-indexes keyboard))
      ((keyboard i)
        ((foreign-lambda* xkb-led-index ((wlr-keyboard* keyboard) (int i))
           "C_return(keyboard->led_indexes[i]);")
         keyboard i))))

  (define wlr-keyboard-mod-indexes
    (case-lambda
      ((keyboard) (%wlr-keyboard-mod-indexes keyboard))
      ((keyboard i)
        ((foreign-lambda* xkb-mod-index ((wlr-keyboard* keyboard) (int i))
           "C_return(keyboard->mod_indexes[i]);")
         keyboard i))))

  (define wlr-keyboard-keycodes
    (case-lambda
      ((keyboard) (%wlr-keyboard-keycodes keyboard))
      ((keyboard i)
        ((foreign-lambda* unsigned-int32 ((wlr-keyboard* keyboard) (int i))
           "C_return(keyboard->keycodes[i]);")
         keyboard i))))

  (define-foreign-values wlr-key-state
    (wlr-key/released "WLR_KEY_RELEASED")
    (wlr-key/pressed  "WLR_KEY_PRESSED")))
