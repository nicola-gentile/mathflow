import Lexer 

main :: IO ()
main = print . readAllTokens $ "a =! ((12. + 5)* x^2)/(23.7-1)"
