#ifndef tpl_h
#define tpl_h

typedef struct { const char* name; const char* value; } Arg;

typedef struct Template_impl Template;

typedef void(*)(void*, size_t) WriteFn;

Template* tpl_template_compile(void* buffer, size_t len);
Template* tpl_template_read(const char* path);
void tpl_template_free(Template* template);
void tpl_template_run(Template* template, WriteFn write, *const Arg args, size_t args_count);

#endif
