use gluaparse::ast::AstNode;
use gluaparse::parse;

#[test]
fn func_named() {
    assert_eq!(
        parse("function test() end").unwrap(),
        AstNode::Block(vec![AstNode::Assignment(
            Box::new(AstNode::Ident("test".to_string())),
            Box::new(AstNode::Function(
                Vec::new(),
                Box::new(AstNode::Block(Vec::new()))
            )),
        )])
    );
}

#[test]
fn func_anon() {
    assert_eq!(
        parse("local test = function() end").unwrap(),
        AstNode::Block(vec![AstNode::Declaration(
            Box::new(AstNode::Ident("test".to_string())),
            Box::new(AstNode::Function(
                Vec::new(),
                Box::new(AstNode::Block(Vec::new()))
            )),
        )])
    );
}

#[test]
fn func_vararg() {
    assert_eq!(
        parse("local function test(...) end").unwrap(),
        AstNode::Block(vec![AstNode::Declaration(
            Box::new(AstNode::Ident("test".to_string())),
            Box::new(AstNode::Function(
                vec![AstNode::Vararg],
                Box::new(AstNode::Block(Vec::new()))
            )),
        )])
    );
}

#[test]
fn func_vararg_with_arg() {
    assert_eq!(
        parse("local function test(a, b, ...) end").unwrap(),
        AstNode::Block(vec![AstNode::Declaration(
            Box::new(AstNode::Ident("test".to_string())),
            Box::new(AstNode::Function(
                vec![
                    AstNode::Ident("a".to_string()),
                    AstNode::Ident("b".to_string()),
                    AstNode::Vararg,
                ],
                Box::new(AstNode::Block(Vec::new()))
            )),
        )])
    );
}

#[test]
fn func_arg_after_vararg() {
    assert!(gluaparse::parse("local function test(a, b, ..., c) end").is_err());
}

#[test]
fn func_self() {
    assert_eq!(
        parse("function tab:test() end").unwrap(),
        AstNode::Block(vec![AstNode::Assignment(
            Box::new(AstNode::Index(
                Box::new(AstNode::Ident("tab".to_string())),
                Box::new(AstNode::Ident("test".to_string())),
            )),
            Box::new(AstNode::Function(
                vec![AstNode::Ident("self".to_string())],
                Box::new(AstNode::Block(Vec::new())),
            ))
        )])
    )
}
