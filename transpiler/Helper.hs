module Debugger where
import Base (Token(..), Type (..))

tokenToStr :: Token -> String
tokenToStr (Identifier st) = "Identifier [" ++ st ++ "]"
tokenToStr (String st) = "String [\"" ++ st ++ "\"]"
tokenToStr (Char ch) = "Char ['" ++ [ch] ++ "']"
tokenToStr (Int64 n) = "Int64 [" ++ show n ++ "]"
tokenToStr (Symbol sym) = "Symbol [" ++ [sym] ++ "]"
tokenToStr (Type TInt32) = "Type Int32"
tokenToStr (Type TInt64) = "Type Int64"
tokenToStr (Type TChar) = "Type Char"
tokenToStr (Type TString) = "Type String"
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