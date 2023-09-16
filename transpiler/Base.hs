module Base (Token(..)) where
import Data.Int (Int64)
data Token =
    Identifier String
  | String String
  | Char Char
  | Int64 Int64
  | Symbol Char
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