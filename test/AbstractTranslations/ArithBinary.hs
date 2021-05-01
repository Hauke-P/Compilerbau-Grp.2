module Test.AbstractTranslations.ArithBinary where

import ScannerParser.AbstrakteSyntax2

arithBinaryAbstractSyntax = 
    Class(
        [Public], 
        "ArithBinary", 
        [],
        [Method(
            [], 
            "int", 
            "foo", 
            [
                ("int","x"),
                ("int", "y")
            ], 
            Block [
                Return(
                    Just (
                        Binary(
                            Plus, 
                            LocalOrFieldVar "x",
                            LocalOrFieldVar "y"
                        )
                    )
                )
            ]
        )]
    )

-- arithBinaryAbstractTypedSyntax

-- arithBinaryAbstractByteCode