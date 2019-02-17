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

(foreign-declare "#include <wlr/types/wlr_input_device.h>")
(include "wlroots-types.scm")

(module (wlr types wlr-input-device)
        (wlr-button/released
         wlr-button/pressed

         wlr-input-device/keyboard
         wlr-input-device/pointer
         wlr-input-device/touch
         wlr-input-device/tablet-tool
         wlr-input-device/tablet-pad
         wlr-input-device/switch

         wlr-input-device-impl
         wlr-input-device-type
         wlr-input-device-vendor
         wlr-input-device-product
         wlr-input-device-name
         wlr-input-device-width-mm
         wlr-input-device-height-mm
         wlr-input-device-output-name
         wlr-input-device-_device
         wlr-input-device-keyboard
         wlr-input-device-pointer
         wlr-input-device-lid-switch
         wlr-input-device-touch
         wlr-input-device-tablet
         wlr-input-device-tablet-pad
         wlr-input-device-events-destroy
         wlr-input-device-data
         wlr-input-device-link)
  (import (scheme)
          (chicken base))
  (include "ffi-helpers.scm")

  (bind-file "include/bind/wlr/types/wlr_input_device.h")

  (define-foreign-values wlr-button-state
    (wlr-button/released "WLR_BUTTON_RELEASED")
    (wlr-button/pressed  "WLR_BUTTON_PRESSED"))

  (define-foreign-values wlr-input-device-type
    (wlr-input-device/keyboard    "WLR_INPUT_DEVICE_KEYBOARD")
    (wlr-input-device/pointer     "WLR_INPUT_DEVICE_POINTER")
    (wlr-input-device/touch       "WLR_INPUT_DEVICE_TOUCH")
    (wlr-input-device/tablet-tool "WLR_INPUT_DEVICE_TABLET_TOOL")
    (wlr-input-device/tablet-pad  "WLR_INPUT_DEVICE_TABLET_PAD")
    (wlr-input-device/switch      "WLR_INPUT_DEVICE_SWITCH"))

  (define-foreign-record-type (wlr-input-device* "struct wlr_input_device")
    (c-pointer _device wlr-input-device-_device)
    ((c-pointer (struct "wlr_keyboard")) keyboard wlr-input-device-keyboard)
    ((c-pointer (struct "wlr_pointer")) pointer wlr-input-device-pointer)
    ((c-pointer (struct "wlr_switch")) lid_switch wlr-input-device-lid-switch)
    ((c-pointer (struct "wlr_touch")) touch wlr-input-device-touch)
    ((c-pointer (struct "wlr_tablet")) tablet wlr-input-device-tablet)
    ((c-pointer (struct "wlr_tablet_pad")) tablet_pad wlr-input-device-tablet-pad)
    ((struct "wl_signal") events.destroy wlr-input-device-events-destroy)
    ((struct "wl_list") link wlr-input-device-link)))
