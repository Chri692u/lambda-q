module Runtime.Env where

import Language.Syntax
import Language.Types

import Data.Monoid
import Data.Foldable hiding (toList)
import qualified Data.Map as M

-- | Generic type for environments
-- ^ The environment for values: Env Value
-- ^ The environment for types: Env Type
-- ^ The environment for filesystem: Env FST
newtype Env a = Env { values :: M.Map Id a } deriving (Eq)

-- | Empty environment
empty :: Env a
empty = Env M.empty

-- | Extend the environment
extend :: Env a -> (Id, a) -> Env a
extend env (x, s) = env { values = M.insert x s (values env) }

-- | Remove a binding
remove :: Env a -> Id -> Env a
remove (Env env) var = Env (M.delete var env)

-- | Lookup a binding
find :: Id -> Env a -> Maybe a
find key (Env env) = M.lookup key env

-- | Get all the keys in the environment
keys :: Env a -> [Id]
keys (Env env) = M.keys env

-- | Merge two environments
merge :: Env a -> Env a -> Env a
merge (Env a) (Env b) = Env (M.union a b)

-- to and from List to Env
fromList :: [(Id, a)] -> Env a
fromList xs = Env (M.fromList xs)

toList :: Env a -> [(Id, a)]
toList (Env env) = M.toList env