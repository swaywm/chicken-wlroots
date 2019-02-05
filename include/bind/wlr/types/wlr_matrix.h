/** Writes the identity matrix into mat */
void wlr_matrix_identity(wlr_matrix mat);

/** mat ← a × b */
void wlr_matrix_multiply(wlr_matrix mat, const wlr_matrix a,
	const wlr_matrix b);

void wlr_matrix_transpose(wlr_matrix mat, const wlr_matrix a);

/** Writes a 2D translation matrix to mat of magnitude (x, y) */
void wlr_matrix_translate(wlr_matrix mat, float x, float y);

/** Writes a 2D scale matrix to mat of magnitude (x, y) */
void wlr_matrix_scale(wlr_matrix mat, float x, float y);

/** Writes a 2D rotation matrix to mat at an angle of rad radians */
void wlr_matrix_rotate(wlr_matrix mat, float rad);

/** Writes a transformation matrix which applies the specified
 *  wl_output_transform to mat */
void wlr_matrix_transform(wlr_matrix mat,
	enum wl_output_transform transform);

/** Writes a 2D orthographic projection matrix to mat of (width, height) with a
 *  specified wl_output_transform*/
void wlr_matrix_projection(wlr_matrix mat, int width, int height,
	enum wl_output_transform transform);

/** Shortcut for the various matrix operations involved in projecting the
 *  specified wlr_box onto a given orthographic projection with a given
 *  rotation. The result is written to mat, which can be applied to each
 *  coordinate of the box to get a new coordinate from [-1,1]. */
void wlr_matrix_project_box(wlr_matrix mat, const struct wlr_box *box,
	enum wl_output_transform transform, float rotation,
	const wlr_matrix projection);
