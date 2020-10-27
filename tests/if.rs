use gluaparse::ast::AstNode;
use gluaparse::parse;

#[test]
fn if_true() {
    assert_eq!(
        parse("if true then end").unwrap(),
        AstNode::Block(vec![AstNode::If(vec![AstNode::IfStub(
            Some(Box::new(AstNode::Bool(true))),
            Box::new(AstNode::Block(Vec::new()))
        ),])])
    )
}

#[test]
fn if_true_else() {
    assert_eq!(
        parse("if true then else end").unwrap(),
        AstNode::Block(vec![AstNode::If(vec![
            AstNode::IfStub(
                Some(Box::new(AstNode::Bool(true))),
                Box::new(AstNode::Block(Vec::new()))
            ),
            AstNode::IfStub(None, Box::new(AstNode::Block(Vec::new()))),
        ])])
    )
}

#[test]
fn if_true_elseif_false() {
    assert_eq!(
        parse("if true then elseif false then end").unwrap(),
        AstNode::Block(vec![AstNode::If(vec![
            AstNode::IfStub(
                Some(Box::new(AstNode::Bool(true))),
                Box::new(AstNode::Block(Vec::new()))
            ),
            AstNode::IfStub(
                Some(Box::new(AstNode::Bool(false))),
                Box::new(AstNode::Block(Vec::new()))
            ),
        ])])
    )
}

#[test]
fn if_true_elseif_false_else() {
    assert_eq!(
        parse("if true then elseif false then else end").unwrap(),
        AstNode::Block(vec![AstNode::If(vec![
            AstNode::IfStub(
                Some(Box::new(AstNode::Bool(true))),
                Box::new(AstNode::Block(Vec::new()))
            ),
            AstNode::IfStub(
                Some(Box::new(AstNode::Bool(false))),
                Box::new(AstNode::Block(Vec::new()))
            ),
            AstNode::IfStub(None, Box::new(AstNode::Block(Vec::new()))),
        ])])
    )
}

#[test]
fn if_true_elseif_many() {
    assert_eq!(
        parse("if true then elseif false then elseif false then elseif false then end").unwrap(),
        AstNode::Block(vec![AstNode::If(vec![
            AstNode::IfStub(
                Some(Box::new(AstNode::Bool(true))),
                Box::new(AstNode::Block(Vec::new()))
            ),
            AstNode::IfStub(
                Some(Box::new(AstNode::Bool(false))),
                Box::new(AstNode::Block(Vec::new()))
            ),
            AstNode::IfStub(
                Some(Box::new(AstNode::Bool(false))),
                Box::new(AstNode::Block(Vec::new()))
            ),
            AstNode::IfStub(
                Some(Box::new(AstNode::Bool(false))),
                Box::new(AstNode::Block(Vec::new()))
            ),
        ])])
    )
}
