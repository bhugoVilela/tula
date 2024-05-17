{-# LANGUAGE TupleSections #-}
import Control.Monad.Trans.State (StateT (runStateT), get, put)
import TulaLexer (TulaAtom (..), TulaCase (..), TulaIdentifier (..), TulaLiteral (TLiteral), runLexer, lexAtom, parseCase)
import Control.Monad.Trans.Except (ExceptT)
import qualified Data.Map as Map
import Data.List (partition)
import Runner (atomMatch)
import Control.Monad.Trans.Class (lift)
import Control.Monad (unless)

data TypeVariable = Const TulaAtom | Var String

data TypeCheckerState = TCState {
  tmRead         :: TulaAtom,
  tmState        :: TulaAtom,
  types          :: Map.Map String [TulaAtom],
  typeBindings   :: [(TulaAtom, String)], -- (a, b) in Pairs where Pairs is already expanded
  typeVariables  :: Map.Map String (String, String), -- varname : (varinstancename, type)
  typeInstances  :: Map.Map String String, -- (c, Pairs)
  boundVariables :: Map.Map String TulaAtom,
  tcase          :: TulaCase
} deriving (Show)

type Error = String

type TypeChecker = StateT TypeCheckerState (Either Error)

-- computes typeVariables and typeInstances
createTypeInstances :: TypeChecker ()
createTypeInstances = do
  state@(TCState { types, typeBindings, typeInstances }) <- get
  let (typeVariables, typeInstances, _) = foldl createTypeInstance ([], [], "a") typeBindings
  put $ state{
    typeVariables = Map.fromList typeVariables,
    typeInstances = Map.fromList typeInstances
    }
  return ()
  where
    nextString str
      | last str == 'z' = map (const 'a') str ++ "a"
      | otherwise = init str ++ [succ $ last str]
    createTypeInstance (ls, ls2, name) (atom, ttype)
      | null vars = (ls, ls2, name')
      | otherwise = (
        vars ++ ls,
        (name', fst (head vars)):ls2,
        name')
      where
        name' = nextString name
        vars = map (,(name', ttype)) (getVariablesFromAtom atom)

-- computes bound variables
patternMatchBound :: TypeChecker ()
patternMatchBound = do
  state@(TCState { tmState, tmRead, typeVariables, tcase }) <- get
  let tstate = tcaseState tcase
  let tread = tcaseRead tcase
  let stateVariables = Map.fromList $ filter (isInTyVars typeVariables) (patternMatch tstate tmState)
  let readVariables = Map.fromList $ filter (isInTyVars typeVariables) (patternMatch tread tmRead)
  let intersections = Map.intersectionWith atomMatch stateVariables readVariables
  let badMatches = map fst $ filter (not . snd) $ Map.toList intersections
  -- at this point we know that all bound variables are consistent
  --ie. all refs to a var have the same value
  --does this short circuit the do block?
  unless (null badMatches) $ lift $ Left $ errorMsg badMatches

  put $ state {
    boundVariables = stateVariables `mappend` readVariables
  }

  return ()
  where
    errorMsg badMatches = "Variables [ " ++ unwords badMatches ++ " ] failed to match, each instance was found to have multiple possible matches"
    isInTyVars :: Map.Map String (String, String) -> (String, TulaAtom) -> Bool
    isInTyVars tyVars match = fst match `Map.member` tyVars

-- TODO
-- now we have 
-- values: a = 1 b = 0
-- bindings: (a, b): C : Pairs
-- let's replace (a, b) with (1, b)
-- and patternMatch it against each value in C, keeping those that match
-- so if C = { (0, 1) (1, 2) } 
-- we keep (1, 2) that matched with b == 1
reduceTypeVariable :: TypeChecker ()
reduceTypeVariable = do
  (TCState { typeVariables, boundVariables })  <- get
  let freeVariables = typeVariables `Map.difference` boundVariables
  -- TODO continue
  return ()

varIsReferrenced :: TulaAtom -> String -> Bool
varIsReferrenced (TAIdentifier (TIdentifier name _ )) var = name == var
varIsReferrenced (TALiteral _) var = False
varIsReferrenced (TASExpr list) var = any (`varIsReferrenced` var) list

varIsBound :: TulaCase -> String -> Bool
varIsBound (TulaCase { tcaseState, tcaseRead }) var = varIsReferrenced tcaseState var || varIsReferrenced tcaseRead var

getVariablesFromAtom :: TulaAtom -> [String]
getVariablesFromAtom (TAIdentifier (TIdentifier name _)) = [name]
getVariablesFromAtom (TALiteral _) = []
getVariablesFromAtom (TASExpr list) = concatMap getVariablesFromAtom list

fst3 :: (a, b, c) -> a
fst3 (a, b, c) = a

snd3 :: (a, b, c) -> b
snd3 (a, b, c) = b

trd3 :: (a, b, c) -> c
trd3 (a, b, c) = c

patternMatch :: TulaAtom -> TulaAtom -> [(String, TulaAtom)]
patternMatch (TAIdentifier (TIdentifier name _)) b = [(name, b)]
patternMatch (TALiteral (TLiteral name _)) b      = [(name, b)]
patternMatch pattern@(TASExpr listA) (TASExpr listB)
  | length listA == length listB = concatMap (uncurry patternMatch) $ zip listA listB
  | otherwise = []
patternMatch _ _ = []


makeTest = do
  state <- fst <$> runLexer lexAtom "(S 0 1)"
  read <- fst <$> runLexer lexAtom "(R 1)"

  typeValues <- sequence $ map (fst <$>) [
    runLexer lexAtom "(0 1)",
    runLexer lexAtom "(1 2)"
    ]

  let typeN = Map.singleton "N" typeValues

  typeBVar <- fst <$> runLexer lexAtom "(a b)"
  let typeBindings = [(typeBVar, "N")]

  tcase <- fst <$> runLexer parseCase "case (S a b) (R b) 0 -> T"

  return TCState {
    tmState = state,
    tmRead = read,
    types = typeN,
    typeBindings = typeBindings,
    typeVariables = Map.empty,
    typeInstances = Map.empty,
    boundVariables = Map.empty,
    tcase = tcase
    }

getTest :: TypeChecker ()
getTest = do
  createTypeInstances
  patternMatchBound
  return ()

runTest = do
  test <- makeTest
  return $ runStateT getTest test

