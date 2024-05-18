use crate::{Def, Expr, ExprRef, Ident, IdentRef, Module};

pub struct Parser<'src> {
    src: &'src str,
    offset: usize,
    char: Option<char>,
    src_iter: std::iter::Peekable<std::str::CharIndices<'src>>,
}

impl<'src> Parser<'src> {
    pub fn new(src: &'src str) -> Self {
        let mut src_iter = src.char_indices().peekable();
        let (offset, char) = match src_iter.peek() {
            None => (0, None),
            Some((offset, c)) => (*offset, Some(*c)),
        };
        Self {
            src,
            offset,
            char,
            src_iter,
        }
    }

    pub fn peek(&mut self) -> Option<char> {
        self.char
    }

    pub fn consume(&mut self) -> Option<char> {
        match self.char {
            Some(c) => {
                self.src_iter.next();
                if let Some((new_offset, new_c)) = self.src_iter.peek() {
                    self.offset = *new_offset;
                    self.char = Some(*new_c);
                } else {
                    self.offset += c.len_utf8();
                    self.char = None;
                }
                Some(c)
            }
            None => None,
        }
    }

    pub fn consume_if<F: FnOnce(char) -> bool>(&mut self, f: F) -> Option<char> {
        match self.char {
            Some(c) if f(c) => {
                self.src_iter.next();
                if let Some((new_offset, new_c)) = self.src_iter.peek() {
                    self.offset = *new_offset;
                    self.char = Some(*new_c);
                } else {
                    self.offset += c.len_utf8();
                    self.char = None;
                }
                Some(c)
            }
            _ => None,
        }
    }

    fn consume_spaces(&mut self) {
        while self.consume_if(|c| c.is_whitespace()).is_some() {}
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum ParseError {
    Eof { offset: usize },
    Unexpected { offset: usize, expecting: Expecting },
}

#[derive(Debug, PartialEq, Eq)]
pub enum Expecting {
    Char(char),
    Label(&'static str),
    Eof,
}

pub fn parse_symbol(parser: &mut Parser, symbol: &str) -> Result<(), ParseError> {
    fn parse_char(parser: &mut Parser, expected_char: char) -> Result<(), ParseError> {
        match parser.src_iter.peek() {
            None => Err(ParseError::Unexpected {
                offset: parser.offset,
                expecting: Expecting::Char(expected_char),
            }),
            Some((_, actual_char)) => {
                if expected_char == *actual_char {
                    parser.consume();
                    Ok(())
                } else {
                    Err(ParseError::Unexpected {
                        offset: parser.offset,
                        expecting: Expecting::Char(expected_char),
                    })
                }
            }
        }
    }

    symbol.chars().try_for_each(|c| parse_char(parser, c))?;
    parser.consume_spaces();
    Ok(())
}

pub fn parse_module<'src>(parser: &mut Parser<'src>) -> Result<Module<'src>, ParseError> {
    let mut module = Module::new();

    loop {
        match parser.peek() {
            Some(c) if is_def_start(c) => match parse_def(&mut module, parser) {
                Ok(def) => {
                    module.defs.push(def);
                    continue;
                }
                Err(err) => {
                    return Err(err);
                }
            },
            _ => {
                break;
            }
        }
    }

    if parser.peek().is_some() {
        Err(ParseError::Unexpected {
            offset: parser.offset,
            expecting: Expecting::Eof,
        })
    } else {
        Ok(module)
    }
}

fn is_def_start(c: char) -> bool {
    // val
    c == 'v' ||
    // (*
    c == '('
}

pub fn parse_def_val<'src>(
    module: &mut Module<'src>,
    parser: &mut Parser<'src>,
) -> Result<(IdentRef, ExprRef), ParseError> {
    debug_assert_eq!(parser.peek(), Some('v'));
    parse_symbol(parser, "val")?;

    let name = parse_ident(parser)?;

    parse_symbol(parser, "=")?;

    let value = parse_expr(module, parser)?;

    parse_symbol(parser, ";")?;

    let name = module.alloc_ident(name);
    let value = module.alloc_expr(value);
    Ok((name, value))
}

pub fn parse_def<'src>(
    module: &mut Module<'src>,
    parser: &mut Parser<'src>,
) -> Result<Def<'src>, ParseError> {
    match parser.peek() {
        Some(c) => match c {
            'v' => {
                let (name, value) = parse_def_val(module, parser)?;
                Ok(Def::Def {
                    comment: None,
                    name,
                    value,
                })
            }
            '(' => {
                let comment = parse_comment(parser)?;

                if let Some('v') = parser.peek() {
                    let (name, value) = parse_def_val(module, parser)?;
                    Ok(Def::Def {
                        comment: Some(comment),
                        name,
                        value,
                    })
                } else {
                    Ok(Def::Comment(comment))
                }
            }
            _ => Err(ParseError::Unexpected {
                offset: parser.offset,
                expecting: Expecting::Label("definition"),
            }),
        },
        None => Err(ParseError::Unexpected {
            offset: parser.offset,
            expecting: Expecting::Label("definition"),
        }),
    }
}

pub fn parse_comment<'src>(parser: &mut Parser<'src>) -> Result<&'src str, ParseError> {
    {
        let offset = parser.offset;

        match parser.consume() {
            Some('(') => {}
            _ => {
                return Err(ParseError::Unexpected {
                    offset,
                    expecting: Expecting::Label("comment"),
                });
            }
        };

        match parser.consume() {
            Some('*') => {}
            _ => {
                return Err(ParseError::Unexpected {
                    offset,
                    expecting: Expecting::Label("comment"),
                });
            }
        };
    }

    let start = parser.offset;
    let mut end = None;
    while let Some(c) = parser.peek() {
        match c {
            '*' => {
                let potential_end = parser.offset;

                parser.consume();
                if let Some(')') = parser.consume() {
                    end = Some(potential_end);
                    break;
                }
            }
            '\\' => {
                let offset = parser.offset;
                parser.consume();
                match parser.consume() {
                    Some(c) if c == '\\' || c == '*' => {}
                    _ => {
                        return Err(ParseError::Unexpected {
                            offset,
                            expecting: Expecting::Label("\\\\ or \\*"),
                        })
                    }
                }
            }
            _ => {
                parser.consume();
            }
        }
    }

    match end {
        None => Err(ParseError::Unexpected {
            offset: parser.offset,
            expecting: Expecting::Label("*)"),
        }),
        Some(end) => {
            parser.consume_spaces();
            Ok(&parser.src[start..end])
        }
    }
}

pub fn parse_expr<'src>(
    module: &mut Module<'src>,
    parser: &mut Parser<'src>,
) -> Result<Expr, ParseError> {
    match parser.src_iter.peek() {
        Some((_, c)) if *c == '\\' => parse_expr_lam(module, parser),
        Some(_) => parse_expr_op(module, parser),
        None => Err(ParseError::Eof {
            offset: parser.offset,
        }),
    }
}

pub fn parse_expr_lam<'src>(
    module: &mut Module<'src>,
    parser: &mut Parser<'src>,
) -> Result<Expr, ParseError> {
    let c = parser.consume();
    parser.consume_spaces();
    debug_assert_eq!(c, Some('\\'), "parse_expr_lam must start with \\");

    let mut args = Vec::new();
    while parser.peek() != Some('.') {
        match parse_ident(parser) {
            Err(err) => {
                return Err(err);
            }
            Ok(arg) => {
                args.push(arg);

                let result = parser.consume_if(|c| c == ',');
                parser.consume_spaces();
                if result.is_some() {
                    continue;
                } else {
                    break;
                }
            }
        }
    }
    parse_symbol(parser, ".")?;
    let body = parse_expr(module, parser)?;

    let arg = module.alloc_idents(args);
    let body = module.alloc_expr(body);
    Ok(Expr::Lam(arg, body))
}

pub fn parse_expr_op<'src>(
    module: &mut Module<'src>,
    parser: &mut Parser<'src>,
) -> Result<Expr, ParseError> {
    let mut a = parse_expr_app(module, parser)?;

    while let Some('+') = parser.peek() {
        parser.consume();
        parser.consume_spaces();

        match parse_expr_app(module, parser) {
            Err(err) => {
                return Err(err);
            }
            Ok(b) => {
                a = Expr::Add(module.alloc_expr(a), module.alloc_expr(b));
            }
        }
    }

    Ok(a)
}

pub fn parse_expr_app<'src>(
    module: &mut Module<'src>,
    parser: &mut Parser<'src>,
) -> Result<Expr, ParseError> {
    let f = parse_expr_simple(module, parser)?;

    let args = parse_expr_app_args(module, parser)?;
    match args {
        None => Ok(f),
        Some(args) => {
            let f = module.alloc_expr(f);
            let args = module.alloc_exprs(args);
            Ok(Expr::App(f, args))
        }
    }
}

pub fn parse_expr_app_args<'src>(
    module: &mut Module<'src>,
    parser: &mut Parser<'src>,
) -> Result<Option<Vec<Expr>>, ParseError> {
    if parser.consume_if(|c| c == '(').is_some() {
        parser.consume_spaces();

        let mut args: Vec<Expr> = Vec::new();
        while parser.peek() != Some(')') {
            let arg = parse_expr(module, parser)?;
            args.push(arg);

            let result = parser.consume_if(|c| c == ',');
            parser.consume_spaces();
            if result.is_some() {
                continue;
            } else {
                break;
            }
        }

        parse_symbol(parser, ")")?;

        Ok(Some(args))
    } else {
        Ok(None)
    }
}

pub fn parse_expr_simple<'src>(
    module: &mut Module<'src>,
    parser: &mut Parser<'src>,
) -> Result<Expr, ParseError> {
    if let Some(c) = parser.peek() {
        if c.is_ascii_digit() {
            let n = parse_int(parser)?;
            Ok(Expr::Int(n))
        } else {
            let ident = parse_ident(parser)?;
            Ok(Expr::Var(module.alloc_ident(ident)))
        }
    } else {
        Err(ParseError::Unexpected {
            offset: parser.offset,
            expecting: Expecting::Label("expression"),
        })
    }
}

pub fn parse_int(parser: &mut Parser) -> Result<usize, ParseError> {
    debug_assert!(parser.peek().map_or(false, |c| c.is_ascii_digit()));

    let mut n = parser.peek().unwrap().to_digit(10).unwrap() as usize;
    parser.consume();

    while let Some(c) = parser.peek() {
        match c.to_digit(10) {
            Some(new_n) => {
                n *= 10;
                n += new_n as usize;
            }
            None => {
                break;
            }
        }
    }
    parser.consume_spaces();

    Ok(n)
}

fn is_ident_start(c: char) -> bool {
    c.is_ascii_alphabetic()
}

fn is_ident_continue(c: char) -> bool {
    c.is_ascii_alphabetic()
}

fn parse_ident<'src>(parser: &mut Parser<'src>) -> Result<Ident<'src>, ParseError> {
    let start_offset: usize = parser.offset;
    let ident = match parser.src_iter.peek() {
        Some((_, c)) => {
            if is_ident_start(*c) {
                parser.consume();
                while let Some((_, c)) = parser.src_iter.peek() {
                    if is_ident_continue(*c) {
                        parser.consume();
                        continue;
                    } else {
                        break;
                    }
                }
                let ident = match parser.src_iter.peek() {
                    None => unsafe { Ident(parser.src.get_unchecked(start_offset..)) },
                    Some((end_offset, _)) => unsafe {
                        Ident(parser.src.get_unchecked(start_offset..*end_offset))
                    },
                };
                Ok(ident)
            } else {
                Err(ParseError::Unexpected {
                    offset: parser.offset,
                    expecting: Expecting::Label("identifier"),
                })
            }
        }
        None => Err(ParseError::Eof {
            offset: parser.offset,
        }),
    }?;
    parser.consume_spaces();

    Ok(ident)
}

#[cfg(test)]
mod test {
    use crate::{
        parse::parse_comment, Def, DefInModule, Expr, ExprInModule, Exprs, ExprsInModule, Module,
    };

    use super::{
        parse_def, parse_expr, parse_expr_app_args, parse_ident, Ident, ParseError, Parser,
    };

    #[test]
    fn parse_ident_1() {
        let input = "";
        let mut parser = Parser::new(input);
        assert_eq!(parse_ident(&mut parser), Err(ParseError::Eof { offset: 0 }))
    }

    #[test]
    fn parse_ident_2() {
        let input = "asdf";
        let mut parser = Parser::new(input);
        assert_eq!(parse_ident(&mut parser), Ok(Ident("asdf")))
    }

    #[test]
    fn parse_ident_3() {
        let input = "asdf~asdf";
        let mut parser = Parser::new(input);
        assert_eq!(parse_ident(&mut parser), Ok(Ident("asdf")))
    }

    #[test]
    fn parse_expr_app_args_1() {
        let input = "()";

        let mut module_a = Module::new();
        let module_b = Module::new();
        assert_eq!(
            {
                let mut parser = Parser::new(input);
                parse_expr_app_args(&mut module_a, &mut parser)
                    .as_ref()
                    .map(|m_exprs| {
                        m_exprs.as_ref().map(|exprs| ExprsInModule {
                            exprs: Exprs(exprs),
                            module: &module_a,
                        })
                    })
            },
            Ok(Some(vec![])).as_ref().map(|m_exprs| {
                m_exprs.as_ref().map(|exprs| ExprsInModule {
                    exprs: Exprs(exprs),
                    module: &module_b,
                })
            })
        )
    }

    #[test]
    fn parse_expr_app_args_2() {
        let input = "(x)";

        let mut module_a = Module::new();
        let mut module_b = Module::new();
        assert_eq!(
            {
                let mut parser = Parser::new(input);
                parse_expr_app_args(&mut module_a, &mut parser)
                    .as_ref()
                    .map(|m_exprs| {
                        m_exprs.as_ref().map(|exprs| ExprsInModule {
                            exprs: Exprs(exprs),
                            module: &module_a,
                        })
                    })
            },
            Ok(Some(vec![Expr::Var(module_b.alloc_ident(Ident("x")))]))
                .as_ref()
                .map(|m_exprs| {
                    m_exprs.as_ref().map(|exprs| ExprsInModule {
                        exprs: Exprs(exprs),
                        module: &module_b,
                    })
                })
        )
    }

    #[test]
    fn parse_expr_1() {
        let input = "x";

        let mut module_a = Module::new();
        let mut module_b = Module::new();

        let mut parser = Parser::new(input);
        assert_eq!(
            parse_expr(&mut module_a, &mut parser).map(|expr| ExprInModule {
                expr,
                module: &module_a
            }),
            Ok(Expr::Var(module_b.alloc_ident(Ident("x")))).map(|expr| ExprInModule {
                expr,
                module: &module_b
            })
        )
    }

    #[test]
    fn parse_expr_2() {
        let input = "f(x)";

        let mut module_a = Module::new();
        let mut module_b = Module::new();
        assert_eq!(
            {
                let mut parser = Parser::new(input);
                parse_expr(&mut module_a, &mut parser).map(|expr| ExprInModule {
                    expr,
                    module: &module_a,
                })
            },
            Ok(Expr::App(
                {
                    let f = module_b.alloc_ident(Ident("f"));
                    module_b.alloc_expr(Expr::Var(f))
                },
                {
                    let x = module_b.alloc_ident(Ident("x"));
                    module_b.alloc_exprs(vec![Expr::Var(x)])
                }
            ))
            .map(|expr| ExprInModule {
                expr,
                module: &module_b
            })
        )
    }

    #[test]
    fn parse_expr_3() {
        let input = "f(x, y)";

        let mut module_a = Module::new();
        let mut module_b = Module::new();
        assert_eq!(
            {
                let mut parser = Parser::new(input);
                parse_expr(&mut module_a, &mut parser).map(|expr| ExprInModule {
                    expr,
                    module: &module_a,
                })
            },
            Ok(Expr::App(
                {
                    let f = module_b.alloc_ident(Ident("f"));
                    module_b.alloc_expr(Expr::Var(f))
                },
                {
                    let x = Expr::Var(module_b.alloc_ident(Ident("x")));
                    let y = Expr::Var(module_b.alloc_ident(Ident("y")));
                    module_b.alloc_exprs(vec![x, y])
                }
            ))
            .map(|expr| ExprInModule {
                expr,
                module: &module_b
            })
        )
    }

    #[test]
    fn parse_def_1() {
        let input = "val id = \\x. x;";

        let mut module_a = Module::new();
        let mut module_b = Module::new();
        assert_eq!(
            {
                let mut parser = Parser::new(input);
                parse_def(&mut module_a, &mut parser).map(|def| DefInModule {
                    def,
                    module: &module_a,
                })
            },
            Ok(Def::Def {
                comment: None,
                name: module_b.alloc_ident(Ident("id")),
                value: {
                    let x = module_b.alloc_ident(Ident("x"));
                    let arg_x = module_b.alloc_idents(vec![Ident("x")]);
                    let value = module_b.alloc_expr(Expr::Var(x));
                    module_b.alloc_expr(Expr::Lam(arg_x, value))
                }
            })
            .map(|def| DefInModule {
                def,
                module: &module_b
            })
        )
    }

    #[test]
    fn parse_def_2() {
        let input = "(* A comment *)\nval id = \\x. x;";

        let mut module_a = Module::new();
        let mut module_b = Module::new();

        let expected = {
            Ok(Def::Def {
                comment: Some(" A comment "),
                name: module_a.alloc_ident(Ident("id")),
                value: {
                    let x = module_a.alloc_ident(Ident("x"));
                    let arg_x = module_a.alloc_idents(vec![Ident("x")]);
                    let value = module_a.alloc_expr(Expr::Var(x));
                    module_a.alloc_expr(Expr::Lam(arg_x, value))
                },
            })
            .map(|def| DefInModule {
                def,
                module: &module_a,
            })
        };

        let actual = {
            let mut parser = Parser::new(input);
            parse_def(&mut module_b, &mut parser).map(|def| DefInModule {
                def,
                module: &module_b,
            })
        };

        assert_eq!(expected, actual)
    }

    #[test]
    fn parse_comment_1() {
        let input = "(* hello world *)";

        let expected = Ok(" hello world ");
        let actual = parse_comment(&mut Parser::new(input));

        assert_eq!(expected, actual)
    }

    #[test]
    fn parse_comment_2() {
        let input = "(* hello world";

        let expected = Err(ParseError::Unexpected {
            offset: input.len(),
            expecting: crate::parse::Expecting::Label("*)"),
        });
        let actual = parse_comment(&mut Parser::new(input));

        assert_eq!(expected, actual)
    }
}
