use gtk::prelude::*;
use gtk::{gdk, gio};
use gtk4 as gtk;

fn main() -> gio::glib::ExitCode {
    gio::resources_register_include!("gitlab_issue.gresource")
        .expect("Failed to register resources");

    let app = gtk::Application::new(
        Some("io.ielliott.gitlab_issue"),
        gio::ApplicationFlags::default(),
    );

    app.connect_startup(|_this| {
        let provider = gtk::CssProvider::new();
        provider.load_from_path("main.css");

        let display = gdk::Display::default().unwrap();
        gtk::style_context_add_provider_for_display(
            &display,
            &provider,
            gtk::STYLE_PROVIDER_PRIORITY_APPLICATION,
        );

        let icon_theme = gtk::IconTheme::for_display(&display);
        icon_theme.add_resource_path("/gitlab_issue/icons");
    });

    app.connect_activate(|this| {
        let window = gtk::ApplicationWindow::new(this);

        window.set_title(Some("GitLab Issues"));

        let display = gdk::Display::default().unwrap();
        let settings = gtk::Settings::for_display(&display);
        if let Some(font_name) = settings.gtk_font_name() {
            println!("{:?}", settings.gtk_font_name());
            window
                .pango_context()
                .set_font_description(Some(&gtk::pango::FontDescription::from_string(&font_name)));
        }

        let before_points_to_pixels = (window.pango_context().font_description().unwrap().size()
            as f32)
            / (gtk::pango::SCALE as f32);
        println!("font size in pt: {before_points_to_pixels}");

        let dpi = (settings.gtk_xft_dpi() as f32) / (gtk::pango::SCALE as f32);
        println!("dpi: {dpi}");
        let px_size = before_points_to_pixels /* pt */ * 1.0/72.0 /* in/pt */ * dpi /* px/in */;
        println!("font size in px: {px_size}");

        let rem = (96.0 / 72.0) * before_points_to_pixels;
        println!("1rem = {rem}px");

        let main = gtk::Box::new(gtk::Orientation::Vertical, 0);
        main.add_css_class("main");

        let content_wrapper = adw::Clamp::new();
        main.append(&content_wrapper);
        content_wrapper.add_css_class("content-wrapper");
        content_wrapper.set_maximum_size((70.0 * rem) as i32);
        content_wrapper.set_tightening_threshold((70.0 * rem) as i32);
        content_wrapper.set_unit(adw::LengthUnit::Px);

        let content = gtk::Box::new(gtk::Orientation::Vertical, 0);
        content_wrapper.set_child(Some(&content));
        content.add_css_class("content");

        let title = gtk::Label::new(Some("New Issue"));
        content.append(&title);
        title.add_css_class("title");
        title.set_halign(gtk::Align::Start);

        /*
        let type_field_wrapper = adw::Clamp::new();
        content.append(&type_field_wrapper);
        type_field_wrapper.set_maximum_size((13.0 * rem) as i32);
        type_field_wrapper.set_tightening_threshold((13.0 * rem) as i32);
        type_field_wrapper.set_unit(adw::LengthUnit::Px);
        */

        let type_field = gtk::Box::new(gtk::Orientation::Vertical, 0);
        content.append(&type_field);
        type_field.add_css_class("form-field");

        let type_label = gtk::Label::new(Some("Type"));
        type_field.append(&type_label);
        type_label.add_css_class("form-label");
        type_label.set_halign(gtk::Align::Start);

        let type_dropdown = gtk::DropDown::new(
            Some(gtk::StringList::new(&["Incident", "Issue", "Task"])),
            None::<&gtk::Expression>,
        );
        type_field.append(&type_dropdown);
        type_dropdown.set_selected(1);

        window.set_child(Some(&main));
        window.present();
    });

    app.run()
}
