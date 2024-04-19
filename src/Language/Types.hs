module Language.Types where
    
-- | Types in lambda q
data Type = Base String
          | TArr Type Type
          | TQuote Type
          deriving (Show, Eq)

stringType :: Type
stringType = Base "String"

unitType :: Type
unitType = Base "()"
