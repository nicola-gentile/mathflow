module Parser
where

    import Lexer 

    data BinaryOperation = Sum | Subtraction | Multiplication | Division | Pow
    data Expression = Assignment [Char] Expression | BinOp BinaryOperation Expression Expression | Value | LiteralNumber | DoNothing

    isLexicalError :: Token -> Bool
    isLexicalError (LexicalError _) = True
    isLexicalError _ = False 

    parseExpression = 0