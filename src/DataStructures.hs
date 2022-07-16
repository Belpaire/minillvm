module DataStructures (CArgs(..),VariableTypeAndName(..),TypeStruct(..),CExpr(..),CStatement(..),CDeclaration(..), MyProgram(..)) where


data VariableTypeAndName = MkVarInfo String TypeStruct deriving Show
data TypeStruct = TyInt |TyBool deriving (Show)
newtype CArgs = MkListArgs [CExpr] deriving Show
data CExpr = MkInt Int | MkPlus CExpr CExpr |  MkVarE String | MkApplyFunc String CArgs | MkFalse | MkTrue  deriving (Show)
data CStatement = ExprStatement CExpr | Compoundstatement [Either CDeclaration CStatement]   | ReturnStatement CExpr deriving (Show)
data CDeclaration = MkFunction VariableTypeAndName [VariableTypeAndName] CStatement | MkExternFunc VariableTypeAndName   [VariableTypeAndName] | MkVarD VariableTypeAndName CExpr deriving (Show)
newtype MyProgram = MkProgram [CDeclaration] deriving (Show)

