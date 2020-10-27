use gluaparse;
use gluaparse::ast::AstNode;

#[test]
fn named_func() {
    let ast = gluaparse::parse("function test() end").unwrap();

    assert_eq!(
        ast,
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
fn anon_func() {
    let ast = gluaparse::parse("local test = function() end").unwrap();

    assert_eq!(
        ast,
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
fn vararg_func() {
    let ast = gluaparse::parse("local function test(...) end").unwrap();

    assert_eq!(
        ast,
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
fn self_func() {
    let ast = gluaparse::parse("function tab:test() end").unwrap();

    assert_eq!(
        ast,
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
