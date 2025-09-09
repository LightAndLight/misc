# GitLab Issues UI

Cloning GitLab's "New Issue" UI in GTK4.

## Notes

### Registering icons

* GResources in `gtk-rd`

  <https://gtk-rs.org/gtk4-rs/stable/latest/book/composite_templates.html#resources>

* Themed icons overview

  <https://developer.gnome.org/documentation/tutorials/themed-icons.html>

  * `-symbolic` icons work with CSS `color`.

* Why does the `gresource` prefix use the `actions` namespace?

  <https://specifications.freedesktop.org/icon-naming-spec/latest/>

* A Reddit user trying to get scalable icons working

  <https://old.reddit.com/r/GTK/comments/148rh0m/gresources_custom_scalable_svg_icons/>

* `librsvg` is required

  <https://discourse.gnome.org/t/svg-support-in-gtk4/9238/7>
