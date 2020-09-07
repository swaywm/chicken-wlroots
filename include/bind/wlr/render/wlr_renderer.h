//enum wlr_renderer_read_pixels_flags {
//	WLR_RENDERER_READ_PIXELS_Y_INVERT = 1,
//};

struct wlr_renderer_impl;
struct wlr_drm_format_set;

struct wlr_renderer {
	const struct wlr_renderer_impl *impl;

	bool rendering;

//	struct {
//		struct wl_signal destroy;
//	} events;
};

struct wlr_renderer *wlr_renderer_autocreate(struct wlr_egl *egl, EGLenum platform,
	void *remote_display, EGLint_ptr config_attribs, EGLint visual_id);

void wlr_renderer_begin(struct wlr_renderer *r, int width, int height);
void wlr_renderer_end(struct wlr_renderer *r);
void wlr_renderer_clear(struct wlr_renderer *r, const float color[static 4]);
/**
 * Defines a scissor box. Only pixels that lie within the scissor box can be
 * modified by drawing functions. Providing a NULL `box` disables the scissor
 * box.
 */
void wlr_renderer_scissor(struct wlr_renderer *r, struct wlr_box *box);
/**
 * Renders the requested texture.
 */
bool wlr_render_texture(struct wlr_renderer *r, struct wlr_texture *texture,
	const wlr_matrix projection, int x, int y, float alpha);
/**
 * Renders the requested texture using the provided matrix.
 */
bool wlr_render_texture_with_matrix(struct wlr_renderer *r,
	struct wlr_texture *texture, const wlr_matrix matrix, float alpha);
/**
 * Renders the requested texture using the provided matrix, after cropping it
 * to the provided rectangle.
 */
bool wlr_render_subtexture_with_matrix(struct wlr_renderer *r,
	struct wlr_texture *texture, const struct wlr_fbox *box,
	const wlr_matrix matrix, float alpha);
/**
 * Renders a solid rectangle in the specified color.
 */
void wlr_render_rect(struct wlr_renderer *r, const struct wlr_box *box,
	const float color[static 4], const wlr_matrix projection);
/**
 * Renders a solid quadrangle in the specified color with the specified matrix.
 */
void wlr_render_quad_with_matrix(struct wlr_renderer *r,
	const float color[static 4], const wlr_matrix matrix);
/**
 * Renders a solid ellipse in the specified color.
 */
void wlr_render_ellipse(struct wlr_renderer *r, const struct wlr_box *box,
	const float color[static 4], const wlr_matrix projection);
/**
 * Renders a solid ellipse in the specified color with the specified matrix.
 */
void wlr_render_ellipse_with_matrix(struct wlr_renderer *r,
	const float color[static 4], const wlr_matrix matrix);
/**
 * Returns a list of pixel formats supported by this renderer.
 */
const enum wl_shm_format *wlr_renderer_get_formats(struct wlr_renderer *r,
	size_t *len);
/**
 * Returns true if this wl_buffer is a wl_drm buffer.
 */
bool wlr_renderer_resource_is_wl_drm_buffer(struct wlr_renderer *renderer,
	struct wl_resource *buffer);
/**
 * Gets the width and height of a wl_drm buffer.
 */
void wlr_renderer_wl_drm_buffer_get_size(struct wlr_renderer *renderer,
	struct wl_resource *buffer, int *width, int *height);
/**
 * Get the available DMA-BUF formats
 */
const struct wlr_drm_format_set *wlr_renderer_get_dmabuf_formats(
	struct wlr_renderer *renderer);
/**
 * Reads out of pixels of the currently bound surface into data. `stride` is in
 * bytes.
 *
 * If `flags` is not NULl, the caller indicates that it accepts frame flags
 * defined in `enum wlr_renderer_read_pixels_flags`.
 */
bool wlr_renderer_read_pixels(struct wlr_renderer *r, enum wl_shm_format fmt,
	uint32_t *flags, uint32_t stride, uint32_t width, uint32_t height,
	uint32_t src_x, uint32_t src_y, uint32_t dst_x, uint32_t dst_y, void *data);

/**
 * Blits the dmabuf in src onto the one in dst.
 */
bool wlr_renderer_blit_dmabuf(struct wlr_renderer *r,
	struct wlr_dmabuf_attributes *dst, struct wlr_dmabuf_attributes *src);
/**
 * Checks if a format is supported.
 */
bool wlr_renderer_format_supported(struct wlr_renderer *r,
	enum wl_shm_format fmt);
/**
 * Creates necessary shm and invokes the initialization of the implementation.
 *
 * Returns false on failure.
 */
bool wlr_renderer_init_wl_display(struct wlr_renderer *r,
	struct wl_display *wl_display);
/**
 * Destroys this wlr_renderer. Textures must be destroyed separately.
 */
void wlr_renderer_destroy(struct wlr_renderer *renderer);
