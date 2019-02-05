struct wlr_box {
	int x, y;
	int width, height;
};

void wlr_box_closest_point(const struct wlr_box *box, double x, double y,
	___out double *dest_x, ___out double *dest_y);

bool wlr_box_intersection(struct wlr_box *dest, const struct wlr_box *box_a,
	const struct wlr_box *box_b);

bool wlr_box_contains_point(const struct wlr_box *box, double x, double y);

bool wlr_box_empty(const struct wlr_box *box);

/**
 * Transforms a box inside a `width` x `height` box.
 */
void wlr_box_transform(struct wlr_box *dest, const struct wlr_box *box,
	enum wl_output_transform transform, int width, int height);

/**
 * Creates the smallest box that contains the box rotated about its center.
 */
void wlr_box_rotated_bounds(struct wlr_box *dest, const struct wlr_box *box, float rotation);

// FIXME struct as argument
//void wlr_box_from_pixman_box32(struct wlr_box *dest, const pixman_box32_t box);
