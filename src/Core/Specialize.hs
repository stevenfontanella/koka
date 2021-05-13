module Core.Specialize ( specializeAll ) where

import Data.Functor
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid (Sum(..))

import Common.NameMap (NameMap)
import Core.Core
import Core.Pretty

import Type.Type

import Debug.Trace
import Control.Monad.Reader
import Control.Monad.State

import Data.IntSet (IntSet)
import qualified Data.IntSet as IS

-- the defs also need to be mutated for sure. Should this be Expr or Def?

-- also, we need some way to replace a function call, not just a name
-- e.g. map(+1, xs) ~~> map_plusone(xs)
type Inlines = NameMap Expr

data SpecializeState = SpecializeState
  { _knownVars :: NameMap Expr
  , _specializable :: NameMap [Either Int Name]
  } deriving (Show)

knownVars :: SpecializeM (NameMap Expr)
knownVars = asks _knownVars

specializable :: SpecializeM (NameMap [Either Int Name])
specializable = asks _specializable

-- type SpecializeM = StateT Inlines (Reader (NameMap Def))
type SpecializeM = Reader SpecializeState

-- update inlines when we decide to inline something
-- specializeAll :: DefGroups -> DefGroups
-- specializeAll groups = 
--     flip runReader (nameMappings groups) 
--   $ flip evalStateT M.empty
--   $ checkGroups groups

specializeAll :: DefGroups -> DefGroups
specializeAll groups = groups -- runReader runSpecialize (SpecializeState known specializable)
  where
    specializable = getSpecializable groups
    known = knownRHS groups
    runSpecialize = error "runSpecialize"

specializeOne :: Def -> Name -> SpecializeM Def
specializeOne def toSpecialize = pure $ def { defExpr = Let [DefNonRec $ makeDef undefined undefined] $ mapExpr f $ defExpr def }
  where
    f var@(Var (TName name typ) info) | name == toSpecialize = var { varName = TName (newName "newName") typ }
    f x = x

-- specializeDef :: Def -> (Def, Def)
-- specializeDef

-- getSpecializable :: DefGroups -> NameMap [Either Int Name]
getSpecializable = M.fromList . fmap (\def -> (defName def, inlineable def)) . allDefs

knownRHS :: DefGroups -> NameMap Expr
knownRHS = M.fromList . fmap (\def -> (defName def, defExpr def)) . allDefs

checkGroups :: DefGroups -> SpecializeM DefGroups
checkGroups = mapM checkOneGroup

nameMappings :: DefGroups -> NameMap Def
nameMappings = M.fromList . map (\def -> (defName def, def)) . allDefs

allDefs :: DefGroups -> [Def]
allDefs = concatMap handleGroup
  where
    handleGroup (DefNonRec def) = [def]
    handleGroup (DefRec defs) = defs

checkOneGroup :: DefGroup -> SpecializeM DefGroup
checkOneGroup (DefRec [rec_]) = DefRec . (:[]) <$> checkOneRec rec_
checkOneGroup def = pure def

checkOneRec :: Def -> SpecializeM Def
checkOneRec def = case defExpr def of
  e@(Lam params effect body) -> traceShowM (countOccurrences e) $> def
  _ -> pure def

-- TODO monoid map type?
-- how many times is each var called?
countOccurrences :: Expr -> NameMap Int
countOccurrences e = M.fromListWith (+) $ foldMapExpr countVar e
  where
    countVar (App (Var (TName name _) info) _) = [(name, 1)]
    --   countVar (Var name info) = [(name, 1)]
    countVar _ = []
-- countOccurrences = id

inlineable :: Def -> [Either Int Name]
inlineable def 
  | Lam params effect body <- defExpr def = 
     let
      usedFunctions = foldMapExpr f body
      -- a def can't be inlineable in its own body
      f (App (Var (TName name _) info) _) | name /= defName def  = [name]
      f _ = []
     in map Right usedFunctions
  | otherwise = []
-- inlineable _ = []

showCon :: Expr -> String
showCon e = case e of
    Lam {} -> "Lam"
    Var {} -> "Var"
    App {} -> "App"
    TypeLam {} -> "TypeLam"
    TypeApp {} -> "TypeApp"
    Con {} -> "Con"
    Lit lit -> "Lit " <> show lit
    Let {} -> "Let"
    Case {} -> "Case"
