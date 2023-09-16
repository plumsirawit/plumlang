module Lexer (plumLex, prettyPrint) where
import Data.Int (Int64)
import Data.Char (isDigit, digitToInt, isAlpha, isLetter, isSpace, isAscii)
import Base (Token(..))

conditionToken :: Token -> String -> String -> Bool
conditionToken tok tmpl st = length st >= length tmpl && take (length tmpl) st == tmpl

unwrapLexRecur :: (String -> Maybe [Token]) -> Int -> String -> Token -> Maybe [Token]
unwrapLexRecur f num st now = case f (drop num st) of
  Just res -> Just (now : res)
  Nothing -> Nothing

escape :: Char -> Maybe Char
escape ch = case ch of
  't'  -> Just '\t'
  'n'  -> Just '\n'
  '\\' -> Just '\\'
  'b'  -> Just '\b'
  'r'  -> Just '\r'
  'f'  -> Just '\f'
  _    -> Nothing

lexUntilEndString :: (String -> Maybe [Token]) -> String -> String -> Maybe [Token]
lexUntilEndString f st buf
  | null st = Nothing
  | head st == '"' = case f (tail st) of
                      Just res -> Just (String (reverse buf) : res)
                      Nothing  -> Nothing
  | length st >= 2 && head st == '\\' = case escape (head (tail st)) of
                                          Just escres -> lexUntilEndString f (tail (tail st)) (escres: buf)
                                          Nothing     -> Nothing
  | otherwise = lexUntilEndString f (tail st) (head st:buf)

lexUntilEndChar :: (String -> Maybe [Token]) -> String -> Maybe [Token]
lexUntilEndChar f st =  if length st >= 2 && head (tail st) == '\'' then
                          case f (drop 2 st) of
                            Just res -> Just (Char (head st) : res)
                            Nothing  -> Nothing
                        else Nothing

lexUntilEndNumber :: (String -> Maybe [Token]) -> String -> Int64 -> Maybe [Token]
lexUntilEndNumber f st buf
  | null st = Just []
  | isDigit (head st) = lexUntilEndNumber f (tail st) (buf * 10 + fromIntegral (digitToInt (head st)))
  | otherwise = case f st of
                  Just res -> Just (Int64 buf : res)
                  Nothing  -> Nothing

lexUntilEndIdent :: (String -> Maybe [Token]) -> String -> String -> Maybe [Token]
lexUntilEndIdent f st buf
  | null st = Just [Identifier (reverse buf)]
  | isAscii (head st) && (isLetter (head st) || isDigit (head st)) = lexUntilEndIdent f (tail st) (head st : buf)
  | otherwise = case f st of
                  Just res -> Just (Identifier (reverse buf) : res)
                  Nothing  -> Nothing

lexLiteral :: (String -> Maybe [Token]) -> String -> Maybe [Token]
lexLiteral f st
  | null st = Just []
  | head st == '"' = lexUntilEndString f (tail st) ""
  | head st == '\'' = lexUntilEndChar f (tail st)
  | isDigit (head st) = lexUntilEndNumber f (tail st) 0
  | isAscii (head st) && isLetter (head st) = lexUntilEndIdent f (tail st) [head st]
  | head st `elem` "+-*/%{}[]()," = case f (tail st) of
                                    Just res -> Just (Symbol (head st) : res)
                                    Nothing  -> Nothing
  | isSpace (head st) = f (tail st)
  | otherwise = Nothing

matchLexCases :: (String -> Maybe [Token]) -> [(Token, String)] -> String -> Maybe [Token]
matchLexCases f [] st = lexLiteral f st
matchLexCases f (toktmpl:tts) st = let (tok, tmpl) = toktmpl in
  if conditionToken tok tmpl st then unwrapLexRecur f (length tmpl) st tok else matchLexCases f tts st

plumLex :: String -> Maybe [Token]
plumLex st
  | null st = Just []
  | otherwise = matchLexCases plumLex [
    (PrintALineOf,   "Print a line of"),
    (Print,          "Print"),
    (FollowedBy,     "followed by"),
    (ExceptWhen,     "except when"),
    (Read,           "Read the"),
    (Read,           "Read an"),
    (Read,           "Read a"),
    (Read,           "Read "),
    (DefineFunction, "Define function"),
    (Let,            "Let"),
    (Set,            "Set"),
    (To,             "to"),
    (Be,             "be"),
    (Dot,            "."),
    (WhichTakes,     "which takes the"),
    (WhichTakes,     "which takes an"),
    (WhichTakes,     "which takes a"),
    (CopyOf,         "copy of"),
    (AndStoreThemIn, "and store them in"),
    (AndReturns,     "and returns")
  ] st

tokenToStr :: Token -> String
tokenToStr (Identifier st) = "Identifier [" ++ st ++ "]"
tokenToStr (String st) = "String [\"" ++ st ++ "\"]"
tokenToStr (Char ch) = "Char ['" ++ [ch] ++ "']"
tokenToStr (Int64 n) = "Int64 [" ++ show n ++ "]"
tokenToStr (Symbol sym) = "Symbol [" ++ [sym] ++ "]"
tokenToStr Print = "Print"
tokenToStr PrintALineOf = "PrintALineOf"
tokenToStr FollowedBy = "FollowedBy"
tokenToStr ExceptWhen = "ExceptWhen"
tokenToStr Dot = "Dot"
tokenToStr Read = "Read"
tokenToStr AndStoreThemIn = "AndStoreThemIn"
tokenToStr DefineFunction = "DefineFunction"
tokenToStr WhichTakes = "WhichTakes"
tokenToStr CopyOf = "CopyOf"
tokenToStr AndReturns = "AndReturns"
tokenToStr Let = "Let"
tokenToStr Be = "Be"
tokenToStr Set = "Set"
tokenToStr To = "To"

prettyPrint :: Maybe [Token] -> String
prettyPrint (Just xs) = foldr (\a b -> tokenToStr a ++ "\n" ++ b) "" xs
prettyPrint Nothing = "Nothing"