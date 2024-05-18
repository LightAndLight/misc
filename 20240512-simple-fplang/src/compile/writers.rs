use std::{fmt::Display, io};

use crate::{internal_error, supply::Supply};

pub struct CModuleWriter<'a> {
    decls: &'a mut dyn io::Write,
}

impl<'a> CModuleWriter<'a> {
    pub fn new(decls: &'a mut dyn io::Write) -> Self {
        Self { decls }
    }

    pub fn newline(&mut self) {
        writeln!(self.decls).unwrap_or_else(internal_error!());
    }

    pub fn include(&mut self, value: &str) {
        writeln!(self.decls, "#include {value}").unwrap_or_else(internal_error!());
    }

    pub fn empty_typedef_struct(&mut self, name: &str) {
        writeln!(self.decls, "typedef struct _{name} {name};").unwrap_or_else(internal_error!());
    }

    pub fn typedef_struct(
        &mut self,
        name: &str,
        mut f: impl FnMut(&mut CTypedefStructFieldsWriter),
    ) {
        fn inner(
            this: &mut CModuleWriter,
            name: &str,
            f: &mut dyn FnMut(&mut CTypedefStructFieldsWriter),
        ) {
            writeln!(this.decls, "typedef struct _{name} {{").unwrap_or_else(internal_error!());
            f(&mut CTypedefStructFieldsWriter { decls: this.decls });
            writeln!(this.decls, "}} {name};").unwrap_or_else(internal_error!());
        }

        inner(self, name, &mut f)
    }

    pub fn typedef_enum(
        &mut self,
        name: &str,
        mut variants: impl FnMut(&mut CTypedefEnumVariantsWriter),
    ) {
        fn inner(
            this: &mut CModuleWriter,
            name: &str,
            variants: &mut dyn FnMut(&mut CTypedefEnumVariantsWriter),
        ) {
            write!(this.decls, "typedef enum {{ ").unwrap_or_else(internal_error!());
            {
                let mut variants_writer = CTypedefEnumVariantsWriter {
                    first: true,
                    module_writer: this,
                };
                variants(&mut variants_writer);
            }
            writeln!(this.decls, " }} {name};").unwrap_or_else(internal_error!());
        }

        inner(self, name, &mut variants)
    }

    pub fn func(
        &mut self,
        return_type: &str,
        name: &str,
        mut args: impl FnMut(&mut CFuncArgsWriter),
        body: CBlock,
    ) {
        fn inner(
            this: &mut CModuleWriter,
            return_type: &str,
            name: &str,
            args: &mut dyn FnMut(&mut CFuncArgsWriter),
            body: CBlock,
        ) {
            write!(this.decls, "{return_type} {name}(").unwrap_or_else(internal_error!());
            args(&mut CFuncArgsWriter {
                first: true,
                decls: this.decls,
            });
            writeln!(this.decls, ") {{").unwrap_or_else(internal_error!());
            write!(this.decls, "{}", body).unwrap_or_else(internal_error!());
            writeln!(this.decls, "}}").unwrap_or_else(internal_error!());
        }

        inner(self, return_type, name, &mut args, body)
    }
}

pub struct CFuncArgsWriter<'a> {
    first: bool,
    decls: &'a mut dyn io::Write,
}

impl<'a> CFuncArgsWriter<'a> {
    pub fn arg(&mut self, r#type: &str, name: &str) {
        if self.first {
            self.first = false;
        } else {
            write!(self.decls, ", ").unwrap_or_else(internal_error!());
        }
        write!(self.decls, "{type} {name}").unwrap_or_else(internal_error!());
    }
}

pub struct CTypedefStructFieldsWriter<'a> {
    decls: &'a mut dyn io::Write,
}

impl<'a> CTypedefStructFieldsWriter<'a> {
    pub fn field(&mut self, r#type: &str, name: &str) {
        writeln!(self.decls, "  {type} {name};").unwrap_or_else(internal_error!());
    }
}

pub struct CTypedefEnumVariantsWriter<'a, 'b> {
    first: bool,
    module_writer: &'b mut CModuleWriter<'a>,
}

impl<'a, 'b> CTypedefEnumVariantsWriter<'a, 'b> {
    pub fn variant(&mut self, name: &str) {
        if self.first {
            self.first = false;
        } else {
            write!(self.module_writer.decls, ", ").unwrap_or_else(internal_error!());
        }
        write!(self.module_writer.decls, "{}", name).unwrap_or_else(internal_error!());
    }
}

pub struct CExpr(String);

impl Display for CExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.str().fmt(f)
    }
}

impl CExpr {
    pub fn str(&self) -> &str {
        &self.0
    }

    pub fn ident(name: &str) -> CExpr {
        let mut value = String::new();
        value.push_str(name);
        CExpr(value)
    }

    pub fn usize(n: usize) -> CExpr {
        let mut value = String::new();
        value.push_str(&n.to_string());
        CExpr(value)
    }

    pub fn cast(r#type: &str, expr: CExpr) -> CExpr {
        let mut value = String::new();
        value.push('(');
        value.push_str(r#type);
        value.push_str(") ");
        value.push_str(expr.str());
        CExpr(value)
    }

    pub fn field(expr: CExpr, field: &str) -> CExpr {
        let mut value = expr.0;
        value.push('.');
        value.push_str(field);
        CExpr(value)
    }

    pub fn index(expr: CExpr, index: CExpr) -> CExpr {
        let mut value = expr.0;
        value.push('[');
        value.push_str(index.str());
        value.push(']');
        CExpr(value)
    }

    pub fn call(func: CExpr, args: impl FnOnce(&mut CExprArgsWriter)) -> CExpr {
        let mut value = func.0;
        value.push('(');
        let mut cexpr_args_writer = CExprArgsWriter { first: true, value };
        args(&mut cexpr_args_writer);
        let mut value = cexpr_args_writer.value;
        value.push(')');
        CExpr(value)
    }

    // How to avoid templating/monomorphism for FnOnce?
    pub fn r#struct(fields: impl FnOnce(&mut CExprStructFieldsWriter)) -> CExpr {
        let mut value = String::new();
        value.push_str("{ ");

        let mut cexpr_struct_fields_writer = CExprStructFieldsWriter { first: true, value };
        fields(&mut cexpr_struct_fields_writer);

        let mut value = cexpr_struct_fields_writer.value;
        value.push_str(" }");

        CExpr(value)
    }

    #[allow(clippy::should_implement_trait)]
    pub fn mul(a: CExpr, b: CExpr) -> CExpr {
        let mut value = a.0;
        value.push_str(" * ");
        value.push_str(b.str());
        CExpr(value)
    }

    #[allow(clippy::should_implement_trait)]
    pub fn add(a: CExpr, b: CExpr) -> CExpr {
        let mut value = a.0;
        value.push_str(" + ");
        value.push_str(b.str());
        CExpr(value)
    }

    pub fn eq(a: CExpr, b: CExpr) -> CExpr {
        let mut value = a.0;
        value.push_str(" == ");
        value.push_str(b.str());
        CExpr(value)
    }
}

pub struct CExprArgsWriter {
    first: bool,
    value: String,
}

impl CExprArgsWriter {
    pub fn arg(&mut self, expr: CExpr) {
        if self.first {
            self.first = false;
        } else {
            self.value.push_str(", ");
        }
        self.value.push_str(expr.str());
    }
}

pub struct CExprStructFieldsWriter {
    first: bool,
    value: String,
}

impl CExprStructFieldsWriter {
    pub fn field(&mut self, name: &str, expr: CExpr) {
        if self.first {
            self.first = false;
        } else {
            self.value.push_str(", ");
        }
        self.value.push('.');
        self.value.push_str(name);
        self.value.push_str(" = ");
        self.value.push_str(expr.str());
    }
}

pub struct CBlock(String);

impl Display for CBlock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.str().fmt(f)
    }
}

impl CBlock {
    pub fn str(&self) -> &str {
        &self.0
    }
}

pub struct CBlockWriter {
    indent_level: usize,
    statements: String,
}

impl CBlockWriter {
    pub fn new() -> Self {
        Self {
            indent_level: 2,
            statements: String::new(),
        }
    }

    pub fn finish(self) -> CBlock {
        CBlock(self.statements)
    }

    pub fn with<F: FnOnce(&mut Self)>(f: F) -> CBlock {
        let mut block_writer = Self::new();
        f(&mut block_writer);
        block_writer.finish()
    }

    pub fn r#return(&mut self, value: CExpr) {
        self.statements.extend((0..self.indent_level).map(|_| ' '));
        self.statements.push_str("return ");
        self.statements.push_str(value.str());
        self.statements.push_str(";\n");
    }

    pub fn define(&mut self, r#type: &str, name: &str, value: CExpr) {
        self.statements.extend((0..self.indent_level).map(|_| ' '));
        self.statements.push_str(r#type);
        self.statements.push(' ');
        self.statements.push_str(name);
        self.statements.push_str(" = ");
        self.statements.push_str(value.str());
        self.statements.push_str(";\n");
    }

    pub fn assign(&mut self, name: CExpr, value: CExpr) {
        self.statements.extend((0..self.indent_level).map(|_| ' '));
        self.statements.push_str(name.str());
        self.statements.push_str(" = ");
        self.statements.push_str(value.str());
        self.statements.push_str(";\n");
    }

    pub fn eval(&mut self, value: CExpr) {
        self.statements.extend((0..self.indent_level).map(|_| ' '));
        self.statements.push_str(value.str());
        self.statements.push_str(";\n");
    }
}

impl Default for CBlockWriter {
    fn default() -> Self {
        Self::new()
    }
}
