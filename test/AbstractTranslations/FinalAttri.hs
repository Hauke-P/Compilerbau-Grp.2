module Test.AbstractTranslations.FinalAttri where

import ScannerParser.AbstrakteSyntax

finalAttriAbstractSyntax = 
    Class(
        [Public], 
        "FinalAttri", 
        [
            FieldDecl ([
                Final],
                 "int", 
                 "x"
            )
        ],
        [],
        [Method (
            [Public ], 
            "", 
            "FinalAttri", 
            [],
            Block [
                StmtExprStmt (
                    Assign (
                        LocalOrFieldVar "x",
                        Integer 1
                    )
                )
            ]
        )]
    )

-- finalAttriAbstractTypedSyntax

-- finalAttriAbstractByteCode