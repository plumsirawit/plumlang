module Parser where
import Base (Token(..), Type(..))
import Data.Int (Int64)

type Ident = String
data Const = String String | Char Char | Int64 Int64
data Operation = Plus | Minus | Multiply | Divide | Modulo | MatrixMultiply | Concatenate
data Expr = IdExpr Ident | ConstExpr Const | Op Operation [Expr]

data PrintOpt = FollowedBy Expr | ExceptWhen Expr
data PrintFn = PrintALineOf Expr [PrintOpt] | Print Expr [PrintOpt]
data ReadFn = ReadMultiple Expr Type Ident | ReadOne Type Ident
data FnDecl = DefineFunction Ident Bool Type Type
data VarDecl = Let Ident Const
data VarAssign = Set Ident Const
data Stmt = PrintFn PrintFn | ReadFn ReadFn | FnDecl FnDecl | VarDecl VarDecl | VarAssign VarAssign
type Prog = [Stmt]

plumParse :: [Token] -> Maybe Prog
plumParse tokens = undefined