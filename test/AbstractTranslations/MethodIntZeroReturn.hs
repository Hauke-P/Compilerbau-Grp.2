module Test.AbstractTranslations.MethodIntZeroReturn where

import ScannerParser.AbstrakteSyntax

methodIntZeroReturnAbstractSyntax = 
    Class(
        [Public], 
        "MethodIntZeroReturn", 
        [],
        [Method (
            [], 
            "int", 
            "foo", 
            [], 
            Block [
                Return (
                    Just (
                        Integer 1
                    ) 
                )
            ]
        )]
    )

-- methodIntZeroReturnAbstractTypedSyntax

-- methodIntZeroReturnAbstractByteCode