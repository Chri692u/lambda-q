module Language.Types where
    
-- | Types in lambda q
data Type = Base String
          | TArr Type Type
          | TQuote Type
          deriving (Eq)

instance Show Type where
    show (Base s) = s
    show (TArr t1 t2) = "(" ++ show t1 ++ " -> " ++ show t2 ++ ")"
    show (TQuote t) = "`" ++ show t ++ "`"

stringType :: Type
stringType = Base "string"

unitType :: Type
unitType = Base "unit"