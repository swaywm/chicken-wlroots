struct wlr_surface;

struct wlr_subcompositor {
	struct wl_global *global;
};

struct wlr_compositor {
	struct wl_global *global;
	struct wlr_renderer *renderer;

//	struct wlr_subcompositor subcompositor;

//	struct wl_listener display_destroy;

//	struct {
//		struct wl_signal new_surface;
//		struct wl_signal destroy;
//	} events;
};

struct wlr_compositor *wlr_compositor_create(struct wl_display *display,
	struct wlr_renderer *renderer);

bool wlr_surface_is_subsurface(struct wlr_surface *surface);

/**
 * Get a subsurface from a surface. Can return NULL if the subsurface has been
 * destroyed.
 */
struct wlr_subsurface *wlr_subsurface_from_wlr_surface(
	struct wlr_surface *surface);
