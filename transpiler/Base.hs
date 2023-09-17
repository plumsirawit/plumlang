module Base (Token(..), Type(..)) where
import Data.Int (Int64)

data Type = TInt32 | TInt64 | TChar | TString

data Token =
    Identifier String
  | String String
  | Char Char
  | Int64 Int64
  | Symbol Char
  | Type Type
  | Print
  | PrintALineOf
  | FollowedBy
  | ExceptWhen
  | Dot
  | Read
  | AndStoreThemIn
  | DefineFunction
  | WhichTakes
  | CopyOf
  | AndReturns
  | Let
  | Be
  | Set
  | To