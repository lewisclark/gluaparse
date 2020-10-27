use gluaparse::ast::AstNode;
use gluaparse::parse;

#[test]
fn single_expr() {
    assert_eq!(
        parse("abc = true and false").unwrap(),
        AstNode::Block(vec![AstNode::Assignment(
            Box::new(AstNode::Ident("abc".to_string())),
            Box::new(AstNode::And(
                Box::new(AstNode::Bool(true)),
                Box::new(AstNode::Bool(false)),
            )),
        )])
    )
}

#[test]
fn single_expr_paren() {
    assert_eq!(
        parse("abc = (true and false)").unwrap(),
        AstNode::Block(vec![AstNode::Assignment(
            Box::new(AstNode::Ident("abc".to_string())),
            Box::new(AstNode::And(
                Box::new(AstNode::Bool(true)),
                Box::new(AstNode::Bool(false)),
            )),
        )])
    )
}

#[test]
fn single_expr_paren_excessive() {
    assert_eq!(
        parse("abc = ((true and ((false))))").unwrap(),
        AstNode::Block(vec![AstNode::Assignment(
            Box::new(AstNode::Ident("abc".to_string())),
            Box::new(AstNode::And(
                Box::new(AstNode::Bool(true)),
                Box::new(AstNode::Bool(false)),
            )),
        )])
    )
}
