pub mod writers;

use std::collections::HashMap;

use crate::{free_vars, supply::Supply, Def, Expr, Exprs, Ident, Module};

use self::writers::{CBlockWriter, CExpr, CModuleWriter};

pub fn compile_runtime(module_writer: &mut CModuleWriter) {
    module_writer.include("<stdlib.h>");
    module_writer.include("<assert.h>");
    module_writer.newline();

    module_writer.empty_typedef_struct("Value");
    module_writer.newline();

    module_writer.typedef_struct("Closure", |typedef_struct| {
        typedef_struct.field("Value*", "env");
        typedef_struct.field("void*", "code");
    });
    module_writer.newline();

    module_writer.typedef_enum("Tag", |v| {
        v.variant("CLOSURE");
        v.variant("INTEGER");
    });
    module_writer.newline();

    module_writer.typedef_struct("Value", |typedef_struct| {
        typedef_struct.field("Tag", "tag");
        typedef_struct.field("union { Closure closure; size_t integer; }", "value");
    });
}

pub fn compile_module(module_writer: &mut CModuleWriter, module: &Module) {
    let mut supply = Supply::new();

    let body = CBlockWriter::with(|block_writer| {
        module
            .defs
            .iter()
            .for_each(|def| compile_def(&mut supply, module_writer, block_writer, module, def));

        block_writer.r#return(CExpr::usize(0));
    });

    module_writer.func("int", "main", |_| {}, body);
}

pub fn compile_def(
    supply: &mut Supply,
    module_writer: &mut CModuleWriter,
    block_writer: &mut CBlockWriter,
    module: &Module,
    def: &Def,
) {
    match def {
        Def::Comment(_) => {}
        Def::Def {
            comment,
            name,
            value,
        } => {
            let value = compile_expr(
                supply,
                module_writer,
                block_writer,
                module,
                &HashMap::new(),
                module.expr(*value),
            );

            block_writer.define(
                "Value",
                &format!("var_{}", module.ident(*name).value()),
                value,
            );
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn compile_apply(
    supply: &mut Supply,
    module_writer: &mut CModuleWriter,
    block_writer: &mut CBlockWriter,
    module: &Module,
    context: &HashMap<Ident, String>,
    f: Expr,
    xs: Exprs,
) -> CExpr {
    let result_name = {
        let name = supply.fresh();
        format!("result_{}", name)
    };

    let f_name = {
        let suffix = supply.fresh();
        format!("f_{}", suffix)
    };
    {
        let value = compile_expr(supply, module_writer, block_writer, module, context, f);
        block_writer.define("Value", &f_name, value);
    }

    {
        let value = CExpr::call(CExpr::ident("assert"), |a| {
            a.arg(CExpr::eq(
                CExpr::field(CExpr::ident(&f_name), "tag"),
                CExpr::ident("CLOSURE"),
            ));
        });

        block_writer.eval(value);
    }

    let f_typed_name = {
        let suffix = supply.fresh();
        format!("f_{}", suffix)
    };
    let f_arg_types = {
        let mut string = String::new();

        // env argument
        string.push_str("Value*");

        for _ in xs {
            string.push_str(", Value");
        }
        string
    };
    {
        let value = CExpr::cast(
            &format!("Value (*)({})", f_arg_types),
            CExpr::field(CExpr::ident(&f_name), "value.closure.code"),
        );
        block_writer.define(
            "Value",
            &format!("(*{})({})", f_typed_name, f_arg_types),
            value,
        );
    }

    let args = {
        let mut args = Vec::new();

        for x in xs {
            let arg = compile_expr(supply, module_writer, block_writer, module, context, *x);
            args.push(arg);
        }

        args
    };

    {
        let value = CExpr::call(CExpr::ident(&f_typed_name), |a| {
            a.arg(CExpr::field(CExpr::ident(&f_name), "value.closure.env"));
            for arg in args {
                a.arg(arg);
            }
        });
        block_writer.define("Value", &result_name, value);
    }

    CExpr::ident(&result_name)
}

pub fn compile_expr(
    supply: &mut Supply,
    module_writer: &mut CModuleWriter,
    block_writer: &mut CBlockWriter,
    module: &Module,
    context: &HashMap<Ident, String>,
    expr: Expr,
) -> CExpr {
    match expr {
        Expr::Var(v) => {
            let ident = module.ident(v);
            match context.get(&ident) {
                Some(v) => CExpr::ident(&format!("var_{}", v)),
                None => CExpr::ident(&format!("var_{}", ident.value())),
            }
        }
        Expr::Lam(args, body) => {
            let args = module.idents(args);
            let vars = free_vars(module, args.iter().copied().collect(), module.expr(body));

            let code_name = {
                let suffix = supply.fresh();
                format!("code_{}", suffix)
            };

            {
                let body = CBlockWriter::with(|block_writer| {
                    let context: HashMap<Ident, String> = vars
                        .iter()
                        .enumerate()
                        .map(|(index, var)| (*var, format!("env[{}]", index)))
                        .collect();
                    let value = compile_expr(
                        supply,
                        module_writer,
                        block_writer,
                        module,
                        &context,
                        module.expr(body),
                    );
                    block_writer.r#return(value);
                });
                module_writer.func(
                    "Value",
                    &code_name,
                    |a| {
                        a.arg("Value*", "var_env");
                        args.iter()
                            .for_each(|name| a.arg("Value", &format!("var_{}", name.value())));
                    },
                    body,
                );
            }

            let value_name = {
                let name = supply.fresh();
                format!("value_{}", name)
            };

            if vars.is_empty() {
                let value = CExpr::r#struct(|f| {
                    f.field("tag", CExpr::ident("CLOSURE"));
                    f.field(
                        "value",
                        CExpr::r#struct(|f| {
                            f.field(
                                "closure",
                                CExpr::r#struct(|f| {
                                    f.field("env", CExpr::ident("NULL"));
                                    f.field("code", CExpr::ident(&code_name));
                                }),
                            );
                        }),
                    );
                });
                block_writer.define("Value", &value_name, value);
            } else {
                let env = format!("env_{}", supply.fresh());

                {
                    let value = CExpr::call(CExpr::ident("malloc"), |a| {
                        a.arg(CExpr::mul(
                            CExpr::usize(vars.len()),
                            CExpr::call(CExpr::ident("sizeof"), |a| a.arg(CExpr::ident("Value"))),
                        ));
                    });

                    block_writer.define("Value*", &env, value);

                    for (ix, var) in vars.iter().enumerate() {
                        block_writer.assign(
                            CExpr::index(CExpr::ident(&env), CExpr::usize(ix)),
                            CExpr::ident(&format!("var_{}", var.value())),
                        );
                    }
                }

                {
                    let value = CExpr::r#struct(|f| {
                        f.field("tag", CExpr::ident("CLOSURE"));
                        f.field(
                            "value",
                            CExpr::r#struct(|f| {
                                f.field(
                                    "closure",
                                    CExpr::r#struct(|f| {
                                        f.field("env", CExpr::ident(&env));
                                        f.field("code", CExpr::ident(&code_name));
                                    }),
                                );
                            }),
                        );
                    });

                    block_writer.define("Value", &value_name, value);
                }
            };

            CExpr::ident(&value_name)
        }
        Expr::App(f, xs) => compile_apply(
            supply,
            module_writer,
            block_writer,
            module,
            context,
            module.expr(f),
            module.exprs(xs),
        ),
        Expr::Int(n) => wrap_int(supply, block_writer, CExpr::usize(n)),
        Expr::Add(l, r) => {
            let a = compile_expr(
                supply,
                module_writer,
                block_writer,
                module,
                context,
                module.expr(l),
            );

            let b = compile_expr(
                supply,
                module_writer,
                block_writer,
                module,
                context,
                module.expr(r),
            );

            let a = unwrap_int(supply, block_writer, a);
            let b = unwrap_int(supply, block_writer, b);
            wrap_int(supply, block_writer, CExpr::add(a, b))
        }
    }
}

fn wrap_int(supply: &mut Supply, block_writer: &mut CBlockWriter, n: CExpr) -> CExpr {
    let value_name = format!("value_{}", supply.fresh());
    let value = CExpr::r#struct(|f| {
        f.field("tag", CExpr::ident("INTEGER"));
        f.field(
            "value",
            CExpr::r#struct(|f| {
                f.field("integer", n);
            }),
        );
    });
    block_writer.define("Value", &value_name, value);
    CExpr::ident(&value_name)
}

fn unwrap_int(supply: &mut Supply, block_writer: &mut CBlockWriter, value: CExpr) -> CExpr {
    let value_name = format!("int_{}", supply.fresh());
    block_writer.define("Value", &value_name, value);

    let value = CExpr::call(CExpr::ident("assert"), |a| {
        a.arg(CExpr::eq(
            CExpr::field(CExpr::ident(&value_name), "tag"),
            CExpr::ident("INTEGER"),
        ));
    });

    block_writer.eval(value);

    CExpr::field(CExpr::ident(&value_name), "value.integer")
}
