{-# LANGUAGE InstanceSigs #-}
module Language.Syntax where

import Language.Types

type Id = String

-- | Binary operators
data BOps = Concat | Seq

instance Show BOps where
    show Concat = "++"
    show Seq = ";"

-- | Expressions in lambda q
data Exp = Var Id
         | Con Con
         | App Exp Exp
         | Abs Id Type Exp
         | Quote Exp
         | Unquote Exp
         | Bin BOps Exp Exp

instance Show Exp where
    show :: Exp -> String
    show (Var id) = id
    show (Con con) = show con
    show (App exp1 exp2) = "(" ++ show exp1 ++ " " ++ show exp2 ++ ")"
    show (Abs id _ exp) = "(\\" ++ id ++ "." ++ show exp ++ ")"
    show (Quote exp) = "`" ++ show exp ++ "`"
    show (Unquote exp) = "(u" ++ show exp ++ "u)"
    show (Bin bops exp1 exp2) = "(" ++ show exp1 ++ " " ++ show bops ++ " " ++ show exp2 ++ ")"

-- | Constants in lambda q
data Con = CString String
         | CCWD
         | CCD Exp
         | CTOUCH Exp
         | CMKDIR Exp
         | CRM Exp

instance Show Con where
    show (CString str) = str
    show CCWD = "cwd"
    show (CCD exp) = "cd " ++ show exp
    show (CTOUCH exp) = "touch " ++ show exp
    show (CMKDIR exp) = "mkdir " ++ show exp
    show (CRM exp) = "rm " ++ show exp