pub mod check;
pub mod compile;
pub mod error;
pub mod parse;
pub mod supply;

use std::collections::HashSet;

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct Ident<'src>(&'src str);

impl<'src> Ident<'src> {
    pub fn value(&self) -> &'src str {
        self.0
    }
}

#[derive(Debug)]
pub struct Module<'src> {
    idents: Vec<Ident<'src>>,
    exprs: Vec<Expr>,
    defs: Vec<Def<'src>>,
}

impl<'src> Module<'src> {
    pub fn new() -> Self {
        Self {
            idents: Vec::new(),
            exprs: Vec::new(),
            defs: Vec::new(),
        }
    }

    pub fn ident(&self, ident_ref: IdentRef) -> Ident<'src> {
        self.idents[ident_ref.0]
    }

    pub fn idents(&self, idents_ref: IdentsRef) -> &[Ident<'src>] {
        &self.idents[idents_ref.start..idents_ref.start + idents_ref.len]
    }

    pub fn expr(&self, expr_ref: ExprRef) -> Expr {
        self.exprs[expr_ref.0]
    }

    pub fn exprs(&self, exprs_ref: ExprsRef) -> Exprs {
        Exprs(&self.exprs[exprs_ref.start..exprs_ref.start + exprs_ref.len])
    }

    pub fn alloc_ident(&mut self, ident: Ident<'src>) -> IdentRef {
        let index = self.idents.len();
        self.idents.push(ident);
        IdentRef(index)
    }

    pub fn alloc_idents(&mut self, idents: Vec<Ident<'src>>) -> IdentsRef {
        let start = self.idents.len();
        let len = idents.len();
        self.idents.extend(idents);
        IdentsRef { start, len }
    }

    pub fn alloc_expr(&mut self, expr: Expr) -> ExprRef {
        let index = self.exprs.len();
        self.exprs.push(expr);
        ExprRef(index)
    }

    pub fn alloc_exprs(&mut self, exprs: Vec<Expr>) -> ExprsRef {
        let start = self.exprs.len();
        let len = exprs.len();
        self.exprs.extend(exprs);
        ExprsRef { start, len }
    }
}

impl<'src> Default for Module<'src> {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ExprRef(usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ExprsRef {
    start: usize,
    len: usize,
}

#[derive(Debug, Clone, Copy)]
pub struct IdentRef(usize);

#[derive(Debug, Clone, Copy)]
pub struct IdentsRef {
    start: usize,
    len: usize,
}

#[derive(Debug, Clone, Copy)]
pub enum Expr {
    Var(IdentRef),
    Lam(IdentsRef, ExprRef),
    App(ExprRef, ExprsRef),
    Int(usize),
    Add(ExprRef, ExprRef),
}

#[derive(Debug)]
pub struct ExprInModule<'a, 'src> {
    expr: Expr,
    module: &'a Module<'src>,
}

#[derive(Debug, Clone, Copy)]
pub struct Exprs<'a>(&'a [Expr]);

impl<'a> IntoIterator for Exprs<'a> {
    type Item = <&'a [Expr] as IntoIterator>::Item;

    type IntoIter = <&'a [Expr] as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

impl<'a, 'src> ExprInModule<'a, 'src> {
    pub fn expr(&'a self, expr_ref: ExprRef) -> ExprInModule<'a, 'src> {
        ExprInModule {
            expr: self.module.expr(expr_ref),
            module: self.module,
        }
    }

    pub fn exprs(&'a self, exprs_ref: ExprsRef) -> ExprsInModule<'a, 'src> {
        ExprsInModule {
            exprs: self.module.exprs(exprs_ref),
            module: self.module,
        }
    }
}

impl<'a, 'src> Eq for ExprInModule<'a, 'src> {}

impl<'a, 'src> PartialEq for ExprInModule<'a, 'src> {
    fn eq(&self, other: &Self) -> bool {
        match self.expr {
            Expr::Var(v_a) => match other.expr {
                Expr::Var(v_b) => self.module.ident(v_a) == other.module.ident(v_b),
                _ => false,
            },
            Expr::Lam(ident_a, body_a) => match other.expr {
                Expr::Lam(ident_b, body_b) => {
                    self.module.idents(ident_a) == other.module.idents(ident_b)
                        && self.expr(body_a) == other.expr(body_b)
                }
                _ => false,
            },
            Expr::App(f_a, args_a) => match other.expr {
                Expr::App(f_b, args_b) => {
                    self.expr(f_a) == other.expr(f_b) && self.exprs(args_a) == other.exprs(args_b)
                }
                _ => false,
            },
            Expr::Int(n_a) => match other.expr {
                Expr::Int(n_b) => n_a == n_b,
                _ => false,
            },
            Expr::Add(l_a, r_a) => match other.expr {
                Expr::Add(l_b, r_b) => {
                    self.expr(l_a) == other.expr(l_b) && self.expr(r_a) == other.expr(r_b)
                }
                _ => false,
            },
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ExprsInModule<'a, 'src> {
    exprs: Exprs<'a>,
    module: &'a Module<'src>,
}

impl<'a, 'src> Eq for ExprsInModule<'a, 'src> {}

impl<'a, 'src> PartialEq for ExprsInModule<'a, 'src> {
    fn eq(&self, other: &Self) -> bool {
        self.exprs.0.len() == other.exprs.0.len()
            && self.exprs.0.iter().zip(other.exprs.0.iter()).all(|(a, b)| {
                ExprInModule {
                    expr: *a,
                    module: self.module,
                } == ExprInModule {
                    expr: *b,
                    module: other.module,
                }
            })
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Def<'src> {
    Comment(&'src str),
    Def {
        comment: Option<&'src str>,
        name: IdentRef,
        value: ExprRef,
    },
}

#[derive(Debug)]
pub struct DefInModule<'a, 'src> {
    def: Def<'src>,
    module: &'a Module<'src>,
}

impl<'a, 'src> Eq for DefInModule<'a, 'src> {}

impl<'a, 'src> PartialEq for DefInModule<'a, 'src> {
    fn eq(&self, other: &Self) -> bool {
        match self.def {
            Def::Comment(str_a) => match other.def {
                Def::Comment(str_b) => str_a == str_b,
                _ => false,
            },
            Def::Def {
                comment: comment_a,
                name: name_a,
                value: value_a,
            } => match other.def {
                Def::Def {
                    comment: comment_b,
                    name: name_b,
                    value: value_b,
                } => {
                    self.module.ident(name_a) == other.module.ident(name_b)
                        && ExprInModule {
                            expr: self.module.expr(value_a),
                            module: self.module,
                        } == ExprInModule {
                            expr: other.module.expr(value_b),
                            module: other.module,
                        }
                }
                _ => false,
            },
        }
    }
}

fn free_vars<'src>(
    module: &Module<'src>,
    exclude: HashSet<Ident<'src>>,
    expr: Expr,
) -> HashSet<Ident<'src>> {
    fn inner<'src>(
        module: &Module<'src>,
        exclude: &mut Vec<HashSet<Ident<'src>>>,
        result: &mut HashSet<Ident<'src>>,
        expr: Expr,
    ) {
        match expr {
            Expr::Var(v) => {
                let v = module.ident(v);
                if !exclude.iter().any(|vars| vars.contains(&v)) {
                    result.insert(v);
                }
            }
            Expr::Lam(args, body) => {
                let args = module.idents(args);
                exclude.push(args.iter().copied().collect());
                inner(module, exclude, result, module.expr(body));
                exclude.pop();
            }
            Expr::App(f, xs) => {
                inner(module, exclude, result, module.expr(f));
                let xs = module.exprs(xs);
                for x in xs {
                    inner(module, exclude, result, *x);
                }
            }
            Expr::Int(_) => {}
            Expr::Add(l, r) => {
                inner(module, exclude, result, module.expr(l));
                inner(module, exclude, result, module.expr(r));
            }
        }
    }

    let mut exclude = vec![exclude];
    let mut result = HashSet::new();
    inner(module, &mut exclude, &mut result, expr);
    result
}

#[cfg(test)]
mod test {
    use std::collections::HashSet;

    use crate::{free_vars, Expr, Ident, Module};

    #[test]
    fn free_vars_1() {
        let mut module = Module::new();
        let expr = Expr::Lam(module.alloc_idents(vec![Ident("x")]), {
            let x = module.alloc_ident(Ident("x"));
            module.alloc_expr(Expr::Var(x))
        });
        assert_eq!(free_vars(&module, HashSet::new(), expr), HashSet::new());
    }

    #[test]
    fn free_vars_2() {
        let mut module = Module::new();
        let expr = Expr::Lam(module.alloc_idents(vec![Ident("x")]), {
            let x = module.alloc_ident(Ident("x"));
            let y = module.alloc_ident(Ident("y"));
            let var_x = module.alloc_expr(Expr::Var(x));
            let var_y = module.alloc_exprs(vec![Expr::Var(y)]);
            module.alloc_expr(Expr::App(var_x, var_y))
        });
        assert_eq!(
            free_vars(&module, HashSet::new(), expr),
            HashSet::from([Ident("y")])
        );
    }
}
