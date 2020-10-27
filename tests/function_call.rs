use gluaparse::ast::AstNode;
use gluaparse::parse;

#[test]
fn func_call_no_args() {
    assert_eq!(
        parse("test()").unwrap(),
        AstNode::Block(vec![AstNode::Call(
            Box::new(AstNode::Ident("test".to_string())),
            Vec::new(),
        )])
    )
}

#[test]
fn func_call_args() {
    assert_eq!(
        parse("test(a, b, c)").unwrap(),
        AstNode::Block(vec![AstNode::Call(
            Box::new(AstNode::Ident("test".to_string())),
            vec![
                AstNode::Ident("a".to_string()),
                AstNode::Ident("b".to_string()),
                AstNode::Ident("c".to_string()),
            ],
        )])
    )
}

#[test]
fn func_call_indexed() {
    assert_eq!(
        parse("t1.t2.test(arg1)").unwrap(),
        AstNode::Block(vec![AstNode::Call(
            Box::new(AstNode::Index(
                Box::new(AstNode::Index(
                    Box::new(AstNode::Ident("t1".to_string())),
                    Box::new(AstNode::Ident("t2".to_string())),
                )),
                Box::new(AstNode::Ident("test".to_string())),
            )),
            vec![AstNode::Ident("arg1".to_string()),],
        )]),
    )
}

#[test]
fn func_call_self() {
    assert_eq!(
        parse("t1:test()").unwrap(),
        AstNode::Block(vec![AstNode::Call(
            Box::new(AstNode::Index(
                Box::new(AstNode::Ident("t1".to_string())),
                Box::new(AstNode::Ident("test".to_string())),
            )),
            vec![AstNode::Ident("self".to_string()),],
        )])
    )
}

#[test]
fn func_call_self_args() {
    assert_eq!(
        parse("t1:test(a, b, c)").unwrap(),
        AstNode::Block(vec![AstNode::Call(
            Box::new(AstNode::Index(
                Box::new(AstNode::Ident("t1".to_string())),
                Box::new(AstNode::Ident("test".to_string())),
            )),
            vec![
                AstNode::Ident("self".to_string()),
                AstNode::Ident("a".to_string()),
                AstNode::Ident("b".to_string()),
                AstNode::Ident("c".to_string()),
            ],
        )])
    )
}

#[test]
fn func_call_vararg() {
    assert_eq!(
        parse("test(...)").unwrap(),
        AstNode::Block(vec![AstNode::Call(
            Box::new(AstNode::Ident("test".to_string())),
            vec![AstNode::Vararg],
        )]),
    )
}

#[test]
fn func_call_vararg_args() {
    assert_eq!(
        parse("test(a, b, c, ...)").unwrap(),
        AstNode::Block(vec![AstNode::Call(
            Box::new(AstNode::Ident("test".to_string())),
            vec![
                AstNode::Ident("a".to_string()),
                AstNode::Ident("b".to_string()),
                AstNode::Ident("c".to_string()),
                AstNode::Vararg,
            ],
        )]),
    )
}

#[test]
fn func_call_str_shorthand() {
    assert_eq!(
        parse("test\"hello\"").unwrap(),
        AstNode::Block(vec![AstNode::Call(
            Box::new(AstNode::Ident("test".to_string())),
            vec![AstNode::Str("hello".to_string()),],
        )]),
    )
}

#[test]
fn func_call_str_multiline_shorthand() {
    assert_eq!(
        parse("test[[hello]]").unwrap(),
        AstNode::Block(vec![AstNode::Call(
            Box::new(AstNode::Ident("test".to_string())),
            vec![AstNode::Str("hello".to_string()),],
        )]),
    )
}

#[test]
fn func_call_table_shorthand() {
    assert_eq!(
        parse("test{a, b, c}").unwrap(),
        AstNode::Block(vec![AstNode::Call(
            Box::new(AstNode::Ident("test".to_string())),
            vec![AstNode::Table(vec![
                AstNode::Ident("a".to_string()),
                AstNode::Ident("b".to_string()),
                AstNode::Ident("c".to_string()),
            ])],
        )]),
    )
}

#[test]
fn func_call_self_table_shorthand() {
    assert_eq!(
        parse("t:test{a}").unwrap(),
        AstNode::Block(vec![AstNode::Call(
            Box::new(AstNode::Index(
                Box::new(AstNode::Ident("t".to_string())),
                Box::new(AstNode::Ident("test".to_string())),
            )),
            vec![
                AstNode::Ident("self".to_string()),
                AstNode::Table(vec![AstNode::Ident("a".to_string()),])
            ],
        )]),
    )
}

#[test]
fn func_call_store_ret_local() {
    assert_eq!(
        parse("local ret = func()").unwrap(),
        AstNode::Block(vec![AstNode::Declaration(
            Box::new(AstNode::Ident("ret".to_string())),
            Box::new(AstNode::Call(
                Box::new(AstNode::Ident("func".to_string())),
                Vec::new(),
            )),
        )]),
    )
}

#[test]
fn func_call_store_ret() {
    assert_eq!(
        parse("ret = func()").unwrap(),
        AstNode::Block(vec![AstNode::Assignment(
            Box::new(AstNode::Ident("ret".to_string())),
            Box::new(AstNode::Call(
                Box::new(AstNode::Ident("func".to_string())),
                Vec::new(),
            )),
        )]),
    )
}

#[test]
fn func_call_stacked() {
    assert_eq!(
        parse("test()()").unwrap(),
        AstNode::Block(vec![AstNode::Call(
            Box::new(AstNode::Call(
                Box::new(AstNode::Ident("test".to_string())),
                Vec::new(),
            )),
            Vec::new(),
        )]),
    )
}
