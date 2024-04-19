module Language.Eval where

import Language.Syntax
import qualified Runtime.Env as E
import qualified Runtime.Filesystem as FS

import System.Directory
import System.FilePath
import Data.Foldable (find)
import Data.Maybe
import System.IO

-- | Value in lambda q
data Val = VUnit
           | VString String
           | VQuote Exp
           | VClosure Id Exp EnvV

instance Show Val where
  show VUnit = "VUnit"
  show (VString str) = "VString " ++ show str
  show (VQuote q) = "V`" ++ show q ++ "`"
  show (VClosure x t _) = "<" ++ x ++ ":" ++ show t ++ ">"

-- | Environment for values
type EnvV = E.Env Val

data State a = State
             { env :: E.Env a
             , path :: FS.Path
             , fst :: FS.FST
             }

type Interpreter a = IO (a, State a)

eval :: Exp -> State Val -> Interpreter Val
eval expr s@(State env p fst) =
    case expr of
        Con c -> do
            case c of
                CString str -> return (VString str, s)
                _ -> do
                    (res, s') <- evalOS c s
                    return (res, s')
        Var x -> do
            let v = E.find x env
            case v of
                Just val -> return (val, s)
                Nothing -> error "Variable not in scope."
        App (Quote e1) e2 -> do
            return (VQuote (App e1 e2), s)
        App e1 (Quote e2) -> do
            return (VQuote (App e1 e2), s)
        App e1 e2 -> do
            (v, s') <- eval e2 s
            (closure, s'') <- eval e1 s'
            case closure of
                VClosure x e env' -> do
                    let env'' = E.extend env' (x, v)
                    eval e (State env'' p fst)
                _ -> error "Runtime error, not a function."
        Abs x t e -> do
            return (VClosure x e env, s)
        Quote e -> do
            return (VQuote e, s)
        Unquote e -> do
            (v, s') <- eval e s
            case v of
                VQuote q -> eval q s'
                _ -> error "Runtime error, expected a quote."
        Bin op e1 e2 -> do
            undefined

evalOS :: Con -> State Val -> Interpreter Val
evalOS c s@(State env p fst) =
    case c of
        CCWD -> do
            return (VString p, s)
        CCD e -> do
            (dir, s') <- eval e s
            case dir of
                VString d -> do
                    let p' = p </> d
                    fval <- FS.fs p'
                    case fval of
                        FS.FST fst -> return (VUnit, State env p' fst)
                        _ -> error "Cursor is not under a directory."
                _ -> error "Runtime error, expected a string."
        CTOUCH file -> do
            (dir, s') <- eval file s
            case dir of
                VString d -> do
                    handle <- openFile (p </> d) WriteMode
                    hClose handle
                    fval <- FS.fs p
                    case fval of
                        FS.FST fst -> return (VUnit, State env p fst)
                        _ -> error "Cursor is not under a directory."
                _ -> error "Runtime error, expected a string."
        CMKDIR dir -> do
            (dir, s') <- eval dir s
            case dir of
                VString d -> do
                    createDirectory $ p </> d
                    fval <- FS.fs p
                    case fval of
                        FS.FST fst -> return (VUnit, State env p fst)
                        _ -> error "Cursor is not under a directory."
                _ -> error "Runtime error, expected a string."
        CRM file -> do
            (file, s') <- eval file s
            case file of
                VString d -> do
                    removeFile $ p </> d
                    fval <- FS.fs p
                    case fval of
                        FS.FST fst -> return (VUnit, State env p fst)
                        _ -> error "Cursor is not under a directory."
                _ -> error "Runtime error, expected a string."
