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
        AstNode::Block(vec![AstNode::Assignment(
            Box::new(AstNode::Ident("test".to_string())),
            Box::new(AstNode::Function(
                Vec::new(),
                Box::new(AstNode::Block(Vec::new()))
            )),
        )])
    );
}
