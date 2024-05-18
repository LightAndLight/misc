use crate::{Def, Expr, IdentRef, Module};

#[derive(Debug)]
pub enum Error {
    ValueIsReducible { name: IdentRef },
}

pub fn check_module(module: &Module) -> Result<(), Error> {
    module
        .defs
        .iter()
        .try_for_each(|def| check_def(module, def))
}

pub fn check_def(module: &Module, def: &Def) -> Result<(), Error> {
    match def {
        Def::Comment(_) => Ok(()),
        Def::Def {
            comment,
            name,
            value,
        } => {
            if is_reducible(module.expr(*value)) {
                Err(Error::ValueIsReducible { name: *name })
            } else {
                Ok(())
            }
        }
    }
}

fn is_reducible(expr: Expr) -> bool {
    match expr {
        Expr::Var(_) | Expr::App(_, _) | Expr::Add(_, _) => true,
        Expr::Lam(_, _) | Expr::Int(_) => false,
    }
}
