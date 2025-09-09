use gtk4 as gtk;
use gtk4::gio;
use gtk4::glib::clone;
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

        let r#box = gtk::Box::new(gtk::Orientation::Vertical, 0);
        r#box.set_halign(gtk::Align::Center);
        r#box.set_valign(gtk::Align::Center);

        window.set_child(Some(&r#box));

        let button = gtk::Button::with_label("Hello World");

        button.connect_clicked(|_self| {
            print!("Hello, world!");
        });
        button.connect_clicked(clone!(
            #[strong]
            window,
            move |_self| {
                window.destroy();
            }
        ));

        r#box.append(&button);

        window.present();
    });

    app.run()
}
