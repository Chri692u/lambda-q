{-# LANGUAGE FlexibleContexts #-}
module Language.Check where

import Language.Syntax
import Language.Types
import Runtime.Env

import Control.Monad.Except
import Control.Monad.Reader

typecheck :: Gamma -> Exp -> Either String Type
typecheck env exp = run env (check exp)

run :: Gamma -> Check a -> Either String a
run env = flip runReader env . runExceptT

judgement :: Id -> Check Type
judgement x = do
    env <- ask
    case find x env of
        Just t -> return t
        Nothing -> case find x builtins of
            Just t -> return t
            Nothing -> throwError $ "Variable not in scope: " ++ x

check :: Exp -> Check Type
check (Con con) = case con of
    CCWD -> return stringType
    CCD exp -> do
        t <- check exp
        case t of
            Base "string" -> return unitType
            _ -> throwError "type mismatch"
    CTOUCH exp -> do
        t <- check exp
        case t of
            Base "string" -> return unitType
            _ -> throwError "type mismatch"
    CMKDIR exp -> do
        t <- check exp
        case t of
            Base "string" -> return unitType
            _ -> throwError "type mismatch"
    CRM exp -> do
        t <- check exp
        case t of
            Base "string" -> return unitType
            _ -> throwError "type mismatch"
    CString _ -> return stringType
check (Var id) = do
    env <- ask
    judgement id
check (Abs id t exp) = do
    rhs <- extendM (id, t) (check exp)
    return (TArr t rhs)
check (App exp1 exp2) = do
    t1 <- check exp1
    t2 <- check exp2
    case t1 of
        (TArr a b) | a == t2 -> return b
                   | otherwise -> throwError "type mismatch"
        ty -> throwError "Not a function type"
check (Quote exp) = undefined
check (Unquote exp) = undefined
check (Bin op exp1 exp2) = case op of
    Concat -> do
        undefined
    Seq -> do
        undefined