/**
 * Damage tracking requires to keep track of previous frames' damage. To allow
 * damage tracking to work with triple buffering, a history of two frames is
 * required.
 */
#define WLR_OUTPUT_DAMAGE_PREVIOUS_LEN 2

/**
 * Tracks damage for an output.
 *
 * When a `frame` event is emitted, `wlr_output_damage_make_current` should be
 * called. If necessary, the output should be repainted and
 * `wlr_output_damage_swap_buffers` should be called. No rendering should happen
 * outside a `frame` event handler.
 */
struct wlr_output_damage {
	struct wlr_output *output;
	int max_rects; // max number of damaged rectangles

//	pixman_region32_t current; // in output-local coordinates

	// circular queue for previous damage
//	pixman_region32_t previous[WLR_OUTPUT_DAMAGE_PREVIOUS_LEN];
	size_t previous_idx;

//	struct {
//		struct wl_signal frame;
//		struct wl_signal destroy;
//	} events;

//	struct wl_listener output_destroy;
//	struct wl_listener output_mode;
//	struct wl_listener output_transform;
//	struct wl_listener output_scale;
//	struct wl_listener output_needs_frame;
//	struct wl_listener output_damage;
//	struct wl_listener output_frame;
//	struct wl_listener output_commit;
};

struct wlr_output_damage *wlr_output_damage_create(struct wlr_output *output);
void wlr_output_damage_destroy(struct wlr_output_damage *output_damage);
/**
 * Attach the renderer's buffer to the output. Compositors must call this
 * function before rendering. After they are done rendering, they should call
 * `wlr_output_set_damage` and `wlr_output_commit` to submit the new frame.
 *
 * `needs_frame` will be set to true if a frame should be submitted. `damage`
 * will be set to the region of the output that needs to be repainted, in
 * output-buffer-local coordinates.
 */
bool wlr_output_damage_attach_render(struct wlr_output_damage *output_damage,
	bool *needs_frame, pixman_region32_t *buffer_damage);
/**
 * Accumulates damage and schedules a `frame` event.
 */
void wlr_output_damage_add(struct wlr_output_damage *output_damage,
	pixman_region32_t *damage);
/**
 * Damages the whole output and schedules a `frame` event.
 */
void wlr_output_damage_add_whole(struct wlr_output_damage *output_damage);
/**
 * Accumulates damage from a box and schedules a `frame` event.
 */
void wlr_output_damage_add_box(struct wlr_output_damage *output_damage,
	struct wlr_box *box);
