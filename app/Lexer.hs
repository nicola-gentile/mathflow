
module Lexer (readToken, readAllTokens, Token(..))
where

import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.Int (Int64)

data State = SStart | SWord [Char] | SNumber [Char] | SNumberDot [Char] | SDecimalNumber [Char] [Char] | SPlus | SMinus | SStar | SSlash | SPow | SEqual | SOpenPar | SClosePar | SSeparator | SInvalidCharacter Char deriving (Show)

isSeparator :: Char -> Bool
isSeparator = (`elem` " \r\t")

nextState :: State -> Char -> Maybe State
nextState SStart c
    | isAlpha c || c == '_' = Just (SWord [c]) 
    | isDigit c = Just (SNumber [c])
    | otherwise = case c of
        '+' -> Just SPlus
        '-' -> Just SMinus
        '*' -> Just SStar
        '/' -> Just SSlash
        '^' -> Just SPow
        '=' -> Just SEqual
        '(' -> Just SOpenPar
        ')' -> Just SClosePar
        _ -> Just (SInvalidCharacter c) 
nextState (SWord word) c
    | isAlphaNum c || c == '_' = Just (SWord (c:word))
    | otherwise  = Nothing
nextState (SNumber num) c
    | isDigit c = Just (SNumber (c:num))
    | c == '.' = Just (SNumberDot num) 
    | otherwise = Nothing
nextState (SNumberDot ipart) c
    | isDigit c = Just (SDecimalNumber ipart [c])
    | otherwise = Nothing
nextState (SDecimalNumber ipart fpart) c
    | isDigit c = Just (SDecimalNumber ipart (c:fpart))
    | otherwise = Nothing
nextState _ _ = Nothing
 
data Token = Word [Char] | Number Int64 Double | Plus | Minus | Star | Slash | Pow | Equal | OpenPar | ClosePar | LexicalError [Char] | EndLine | Ignore deriving (Show) 

nextToken :: [Char] -> State -> ([Char], State)
nextToken "" state = ("", state)
nextToken line@(c:cs) state = case nextState state c of 
    Just next_state -> nextToken cs next_state
    Nothing -> (line, state) 

readToken :: [Char] -> (Token, [Char])
readToken "" = (EndLine, "")
readToken (c:cs) | isSeparator c = readToken cs
readToken line = 
    let (line_tail, state) = nextToken line SStart
        token = case state of 
            SInvalidCharacter c -> LexicalError ("Invalid character " ++ [c])
            SPlus -> Plus
            SMinus -> Minus
            SStar -> Star
            SSlash -> Slash
            SPow -> Pow
            SEqual -> Equal
            SOpenPar -> OpenPar
            SClosePar -> ClosePar
            SWord word -> Word (reverse word)
            SNumber num -> Number (parseInt64 num :: Int64) 0.0
            SDecimalNumber ipart fpart -> Number (parseInt64 ipart :: Int64) (parseDouble ("0." ++ fpart) :: Double)
            SNumberDot _ -> LexicalError "Incomplete decimal number"
            _ -> LexicalError "This occurred because of a bug in the parser"
    in (token, line_tail)
    where
        parseInt64 = read . reverse
        parseDouble = read . reverse

readAllTokens :: [Char] -> [Token]
readAllTokens line = 
    let (next_tok, line_tail) = readToken line
    in case next_tok of
        EndLine -> []
        Ignore -> readAllTokens line_tail
        _ -> next_tok:readAllTokens line_tail
