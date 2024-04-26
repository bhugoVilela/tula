module Runner (runProgram, scanProgram, scanLive, TulaProgram, fromTulaProgram, TulaState(..) ) where

import qualified Data.Map as Map
import TulaLexer (
  TulaStatement (TStmtCase, TStmtSet, TStmtFor),
  TulaProgram (TulaProgram, getProgramStatements),
  TulaCase (TulaCase, tcaseRead, tcaseWrite, tcaseState, tcaseNewState),
  TulaDirection (..),
  TulaFor (TulaFor, body),
  TulaSet (TulaSet),
  TulaTape (TulaTape),
  TulaAtom (TAIdentifier, TALiteral, TASExpr), TulaIdentifier (idName, TIdentifier), TulaLiteral (literalName, TLiteral)
  )
import Control.Monad.Trans.State (StateT (runStateT), put, State)
import Control.Monad.Trans.State.Lazy (get)
import Control.Monad (guard, when, unless, liftM, replicateM)
import Data.Maybe (fromJust, fromMaybe, mapMaybe)
import Control.Exception (try)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE, catchE)
import Control.Monad.Trans.Class (lift)
import Data.Functor.Identity (Identity(..))
import GHC.Base (Alternative)
import Control.Applicative (Alternative(empty), asum, (<|>))
import Data.List (intercalate, intersperse)
import Debug.Trace (trace, traceShow, traceShowId)
import Control.Monad (join)
import Data.Foldable (Foldable(toList))

-- Error Messages
errFailedToMatch = "Failed to match a statement"

tryE :: Monad m => ExceptT e m a -> ExceptT e m (Either e a)
tryE m = catchE (liftM Right m) (return . Left)

type BindingTable = Map.Map String [TulaAtom]

data TulaState = TulaState {
  state      :: TulaAtom,
  tape       :: [TulaAtom],
  tapeIdx    :: Int,
  bindings   :: BindingTable,
  statements :: [TulaStatement]
}

fromTulaProgram :: TulaProgram -> TulaState
fromTulaProgram prog@(TulaProgram tape statements) = TulaState {
  state = getInitialState prog,
  tape = tape',
  tapeIdx = 0,
  bindings = Map.empty,
  statements = statements
}
  where
    getInitialState :: TulaProgram -> TulaAtom
    getInitialState = tcaseState . head . mapMaybe getCase . getProgramStatements
      where
        getCase (TStmtCase c) = Just c
        getCase _ = Nothing
    tape' = case tape of
      Just (TulaTape r) -> r
      _                 -> []


instance Show TulaState where
  show (TulaState {state, tape, tapeIdx, bindings}) =
    stateStr ++ ": " ++ unwords (map show tape) ++ "   " ++ bindingStr ++ "\n"
    ++ prefix ++ "^"
    where
      stateStr = show state
      bindingStr = " | " ++ commaList (map showBinding (Map.toList bindings))
      showBinding (name, values) = name ++ "={" ++ unwords (map show values) ++ "}"
      prefix = replicate prefixLen ' '
      prefixLen = length stateStr + 2 + addPrevs + (length (show (tape !! tapeIdx)) `div` 2) + tapeIdx
      addPrevs = sum . map (length . show) . take tapeIdx $ tape
      commaList = intercalate ", "

currentCell :: TulaState -> TulaAtom
currentCell s = tape s !! tapeIdx s

type TulaStateM = ExceptT String (State TulaState)

atomId :: TulaAtom -> String
atomId (TAIdentifier identifier) = idName identifier
atomId (TALiteral literal) = literalName literal
atomId (TASExpr list) = concatMap atomId list

atomMatch :: TulaAtom -> TulaAtom -> Bool
atomMatch a b = atomId a == atomId b

updateCell :: TulaAtom -> TulaStateM ()
updateCell newValue = do
  state@(TulaState { tape, tapeIdx }) <- lift get
  lift . put $ state { tape = getNewTape tape tapeIdx newValue }
  where
    getNewTape tape idx value = let (pre, aft) = splitAt idx tape in case aft of
      [] -> pre ++ [newValue]
      (x:xs) -> pre ++ [newValue] ++ xs

updateState :: TulaAtom -> TulaStateM ()
updateState newState = do
  state@(TulaState { tape, tapeIdx }) <- lift get
  lift . put $ state { state = newState }


writeCell :: a -> Int -> [a] -> [a]
writeCell newValue idx original =
  let (pre, aft) = splitAt idx original
   in case aft of
        [] -> pre ++ [newValue]
        (x : xs) -> pre ++ [newValue] ++ xs

advanceTape :: TulaDirection -> TulaStateM ()
advanceTape dir = do
  state@TulaState { tape, tapeIdx } <- lift get
  let newIdx = if dir == TRight then tapeIdx + 1 else tapeIdx - 1
  guard $ newIdx >= 0 && newIdx < length tape
  lift . put $ state { tapeIdx = newIdx }

evalCase :: TulaCase -> TulaStateM ()
evalCase t@(TulaCase expectedState read write dir newState) = do
  currState@(TulaState { state }) <- lift get
  let currCell = currentCell currState
  -- trace ("tryCase " ++ show t) $ return ()
  guard (atomMatch currCell read && atomMatch state expectedState)
  -- trace ("match") $ return ()
  updateCell write
  updateState newState
  advanceTape dir

evalFor :: TulaFor -> TulaStateM ()
evalFor (TulaFor fBindings source body) = do
  s@TulaState { bindings } <- lift get
  -- all possible values from source
  let values = concatMap (lookupBindings bindings) source
  -- trace ("evalFor " ++ show values) (return ())
  -- all possible combinations of (binding, value)
  let combinations = generateCombinations fBindings values
  let patternMatchedCombinations = map (concatMap (uncurry patternMatch)) combinations
  asum $ evalWithCombination <$> patternMatchedCombinations
  where
    evalWithCombination :: [ (String, TulaAtom) ] -> TulaStateM ()
    evalWithCombination combination = do
      s@TulaState { bindings } <- lift get
      let newBindings = addToMap combination bindings
      let newStatements = map (reduceStatement newBindings) body
      -- trace ("newBindings " ++ show newBindings) $ pure ()
      -- trace ("newStatements " ++ (unlines $ map show newStatements)) $ pure ()
      lift . put $ s { bindings = addToMap combination bindings }
      asum $ evalStatement <$> newStatements
    addToMap :: [(String, TulaAtom)] -> BindingTable -> BindingTable
    addToMap entries map = foldl (\m (k,v) -> Map.insert k [v] m) map entries

reduceStatement :: BindingTable -> TulaStatement -> TulaStatement
reduceStatement bindingTable s@(TStmtFor f@(TulaFor{ body })) = s --TStmtFor $ f{ body = map (reduceStatement bindingTable) body }
reduceStatement bindingTable s@(TStmtSet {}) = trace "REDUCE SET NOT IMPLEMENTED" s
reduceStatement bindingTable s@(TStmtCase t@(TulaCase state read write _ newState)) =
  let new = TStmtCase (t {
    tcaseState    = replace bindingTable state,
    tcaseRead     = replace bindingTable read,
    tcaseWrite    = replace bindingTable write,
    tcaseNewState = replace bindingTable newState
    }) in {- trace ("NEW: " ++ show new ++ "\nOLD: " ++ show t) -} new
  where
    replace :: BindingTable -> TulaAtom -> TulaAtom
    replace table (TASExpr list) = TASExpr $ map (replace table) list
    replace table atom = if atomIsReplaceable atom then fromMaybe atom $ do
        (x:xs) <- Map.lookup (atomId atom) table
        return $ matchReplace (atomId atom) x atom
        else atom
    atomIsReplaceable :: TulaAtom -> Bool
    atomIsReplaceable (TALiteral _) = False
    atomIsReplaceable _             = True
    --Replace instances of pattern in original with replacement
    matchReplace :: String -> TulaAtom -> TulaAtom -> TulaAtom
    matchReplace pattern original@(TAIdentifier _) replacement = if atomId original == pattern then replacement else original
    matchReplace pattern original@(TALiteral _) _ = original
    matchReplace pattern original@(TASExpr list) replacement = TASExpr $ map (\x -> matchReplace pattern x replacement) list

evalStatement :: TulaStatement -> TulaStateM ()
evalStatement (TStmtCase tcase) = evalCase tcase
evalStatement (TStmtSet tset)   = evalSet tset
evalStatement (TStmtFor tfor)   = evalFor tfor
evalStatement _ = throwE "eval statement other than TulaCase not implemented"

-- given a pattern and a source returns a map of all mappings
patternMatch :: TulaAtom -> TulaAtom -> [(String, TulaAtom)]
patternMatch (TAIdentifier (TIdentifier name _)) b = [(name, b)]
patternMatch (TALiteral (TLiteral name _)) b      = [(name, b)]
patternMatch pattern@(TASExpr listA) (TASExpr listB)
  | length listA == length listB = concatMap (uncurry patternMatch) $ zip listA listB
  | otherwise = []
patternMatch _ _ = []

lookupBindings :: BindingTable -> TulaIdentifier -> [TulaAtom]
lookupBindings map key = fromMaybe [TAIdentifier key] $ Map.lookup (idName key) map

generateCombinations :: [a] -> [b] -> [[(a, b)]]
generateCombinations names values =
  let valueCombinations = replicateM (length names) values
      nameValueCombinations = map (zip names) valueCombinations
   in nameValueCombinations

evalSet :: TulaSet -> TulaStateM ()
evalSet (TulaSet name symbols) = do
  state@TulaState { bindings } <- lift get
  -- TODO enable replacement within Sets to make joined sets
  -- let newSymbols = concatMap (lookupBindings bindings) symbols
  let newBindings = Map.insert name symbols bindings
  -- let newBindings = Map.insert name symbols bindings
  lift . put $ state { bindings = newBindings }
  throwE "Set Expression does not count as match"

evalOnce :: TulaStateM ()
evalOnce = do
  state@(TulaState{ statements }) <- lift get
  let allStatements = evalStatement <$>  statements
  let runUntilSuccess = asum allStatements
  runUntilSuccess <|> do throwE errFailedToMatch

runProgram :: TulaStateM String
runProgram = do
  state@TulaState{ statements } <- lift get

  res <- tryE evalOnce
  case res of
    Right _                        -> runProgram
    Left e | e == errFailedToMatch -> return "DONE"
    Left str                       -> throwE str

evalOnceRepeatedly :: ExceptT String TulaStateM ()
evalOnceRepeatedly = do
  lift evalOnce
  evalOnceRepeatedly

scanLive initialState = do
  print initialState

  let (e, state) = runEvalOnce initialState

  case e of
    Right _ -> getLine >> scanLive state
    Left _  -> putStrLn "DONE"

scanProgram initialState = do
  print initialState

  let (e, state) = runEvalOnce initialState

  case e of
    Right _ -> scanProgram state
    Left _  -> putStrLn "DONE"

runEvalOnce = runIdentity . runStateT (runExceptT evalOnce)
