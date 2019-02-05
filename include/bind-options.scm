; XXX: prevent bind from converting arguments to SRFI-4 vector types
(bind-type "wlr_matrix" (c-pointer float))
(bind-type "wlr_rgba" (c-pointer float))
(bind-type "uint32_ptr" (c-pointer unsigned-int32))

; FIXME: use configure script to determine correct size
(bind-type "pid_t" int32)
(bind-type "uid_t" unsigned-int32)
(bind-type "gid_t" unsigned-int32)

(bind-type "EGLint" int32)
(bind-type "EGLint_ptr" (c-pointer int32))
(bind-type "EGLenum" unsigned-int)

(bind-type "xkb_mod_mask_t" unsigned-int32)
(bind-type "xkb_led_index_t" unsigned-int32)
(bind-type "xkb_mod_index_t" unsigned-int32)

(bind-options export-constants: #t
              mutable-fields: #t
              default-renaming: "")
