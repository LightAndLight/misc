fn main() {
    glib_build_tools::compile_resources(
        &["resources"],
        "resources/gresources.xml",
        "gitlab_issue.gresource",
    );
}
