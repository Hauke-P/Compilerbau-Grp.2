module Test.AbstractTranslations.NegUnary where

import ScannerParser.AbstrakteSyntax

negUnaryAbstractSyntax = 
    Class(
        [Public], 
        "NegUnary", 
        [],
        [Method (
            [], 
            "void", 
            "foo", 
            [
                ("boolean", "x")
            ], 
            Block [
                If (
                    Unary (
                        Negation , 
                        LocalOrFieldVar "x"), 
                    Block [], 
                    Nothing )
            ]
        )]
    )

-- negUnaryAbstractTypedSyntax

-- negUnaryAbstractByteCode