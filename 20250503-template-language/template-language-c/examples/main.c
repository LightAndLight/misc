#include <unistd.h>
#include <tpl.h>

void write_stdout(void* buffer, size_t len) {
  write(STDOUT_FILENO, buffer, len);
}

int main() {
  Template* template = tpl_template_read("simple_html.html.tpl");
  Arg args[2] = {
    {
      .name = "title",
      .value = "Simple HTML"
    },
    {
      .name = "body",
      .value = "Hello, world!"
    }
  };
  tpl_template_run(template, write_stdout, args);
  tpl_template_free(template);
}
