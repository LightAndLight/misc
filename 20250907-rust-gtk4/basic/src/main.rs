use gtk4 as gtk;
use gtk4::gio;
use gtk4::prelude::*;

fn main() -> gtk4::glib::ExitCode {
    let app = gtk::Application::new(Some("io.ielliott.basic"), gio::ApplicationFlags::default());
    app.connect_activate(|app| {
        let window = gtk::ApplicationWindow::builder()
            .application(app)
            .default_width(200)
            .default_height(200)
            .title("Window")
            .build();

        window.present();
    });

    app.run()
}
