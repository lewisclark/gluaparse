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
