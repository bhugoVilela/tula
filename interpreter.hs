import Control.Applicative (Alternative (empty, many))
import Control.Exception (IOException)
import Control.Monad (replicateM, unless, when)
import Control.Monad.Trans.State (evalStateT)
import Control.Monad.Trans.State.Lazy (runStateT)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe, isJust)
import GHC.Base (Alternative (..))
import GHC.Data.Maybe (mapMaybe)
import GHC.Exception (throw)
import GHC.IO (catch)
import GHC.IO.Exception (ExitCode (ExitFailure))
import System.Environment (getArgs)
import System.Exit (exitWith)
import TulaLexer (LexState (lexRest), TulaCase (..), TulaDirection (..), TulaProgram (..), TulaStatement (..), TulaTape (..), lexToken, parseCase, parseProgram, TulaFor (TulaFor))
import TulaLexer qualified as TL
import Data.Map qualified as Map

type Cell = String

data TulaState = TulaState
  { cells :: [Cell],
    currentCell :: Int,
    state :: String
  }

instance Show TulaState where
  show (TulaState cells idx state) = state ++ ": " ++ unwords cells ++ "\n" ++ prefix ++ "^"
    where
      prefix = replicate prefixLen ' '
      prefixLen = length state + 2 + (idx * 2)

type SubstitutionTable = Map.Map String String 

advanceTulaState :: TL.TulaDirection -> TulaState -> TulaState
advanceTulaState TRight (TulaState cells idx state) = TulaState cells (idx + 1) state
advanceTulaState TLeft (TulaState cells idx state) = TulaState cells (idx - 1) state

getCellValue :: TulaState -> String
getCellValue (TulaState cells idx _) = cells !! idx

writeCell :: a -> Int -> [a] -> [a]
writeCell newValue idx original =
  let (pre, aft) = splitAt idx original
   in case aft of
        [] -> pre ++ [newValue]
        (x : xs) -> pre ++ [newValue] ++ xs

-- Given a substitution table, replace all matches in a case
reduceCase :: SubstitutionTable -> TulaCase -> TulaCase
reduceCase map (TulaCase state read write dir newState lexems) = TulaCase (replace state) (replace read) (replace write) dir (replace newState) lexems
  where
    replace :: String -> String
    replace str = Map.findWithDefault str str map

reduceFor :: SubstitutionTable -> TulaFor -> TulaFor
reduceFor map (TulaFor bindings source body) = undefined

match :: TulaCase -> TulaState -> Either TulaState TulaState
match tcase tstate
  | matches = Right $ apply tcase tstate
  | otherwise = Left tstate
  where
    matches = getCaseState tcase == state tstate && getCaseReadSymbol tcase == getCellValue tstate

apply tcase =
  advanceTulaState (getCaseDirection tcase)
    . setNewState (getCaseNewState tcase)
    . writeNewValue (getCaseWriteSymbol tcase)

setNewState newState (TulaState cells idx state) = TulaState cells idx newState

writeNewValue newValue (TulaState cells idx state) = TulaState (writeCell newValue idx cells) idx state

init = TulaState ["&", "(", "(", ")", ")", "&", "0", "0", "0", "0", "0", "0", "0", "0", "&"] 0 "Start"

makeCase s = fromMaybe undefined (evalStateT parseCase (TL.emptyState s))

runByHand = do
  a <- match (makeCase "case Start & & -> Pick") Main.init
  return a

runOnce :: [TulaCase] -> TulaState -> Either TulaState (TulaState, [TulaCase])
runOnce [] state = Left state
runOnce (c : cs) state = case match c state of
  Right state -> Right (state, cs)
  Left _ -> runOnce cs state

runToCompletion :: [TulaCase] -> TulaState -> TulaState
runToCompletion cases state = case runOnce cases state of
  Left state -> state
  Right (state', left) -> runToCompletion cases state'

scanToCompletion :: [TulaCase] -> TulaState -> [TulaState]
scanToCompletion cases state = reverse $ scanToCompletion' cases state [state]

scanToCompletion' cases state acc = case runOnce cases state of
  Left state -> acc
  Right (state', left) -> scanToCompletion' cases state' (state' : acc)

readCell :: FilePath -> IO Cell
readCell = fmap read . readFile

main = do
  (srcPath, tapePath) <- getArgs >>= parseArgs
  putStrLn $ "src: " ++ srcPath ++ "\n" ++ "tapePath: " ++ tapePath

  -- parse tula
  src <- readFile srcPath
  let Just (program, state) = runStateT parseProgram (TL.emptyState src)
  unless (null (lexRest state)) $ do
    putStrLn "An Error occured parsing the program"
    exitWith $ ExitFailure 1

  Just (TulaTape tape) <- do
    let pTape = getProgramTape program
    tape <- if isJust pTape
            then putStrLn "using program tape" >> return pTape
            else putStrLn "using tape file" >> readTape tapePath
    unless (isJust tape) $ do
      putStrLn "Missing Tape"
      exitWith $ ExitFailure 1
    return tape

  let initialState = TulaState tape 0 (getInitialState program)

  let steps = scanToCompletion (getProgramCases program) initialState
  let str = unlines $ show <$> steps

  putStrLn ""
  putStrLn str
  where
    parseArgs [] = do
      putStrLn "Missing Args"
      printUsage
      exitWith $ ExitFailure 1
    parseArgs [x] = return (x ++ ".tula", x ++ ".tape")
    parseArgs [x, y] = return (x, y)
    printUsage = do
      putStrLn "USAGE:"
      putStrLn "<program> <path_to_source> <path_to_tape>"
    readTape :: String -> IO (Maybe TulaTape)
    readTape path = do
      Just str <- safeReadFile path
      return . Just . TulaTape . words $ str
    safeReadFile path = (Just <$> readFile path) `catch` ((\e -> pure Nothing) :: IOException -> IO (Maybe String))

getInitialState :: TulaProgram -> String
getInitialState = getCaseState . head . mapMaybe getCase . getProgramStatements
  where
    getCase (TStmtCase c) = Just c
    getCase _ = Nothing

getProgramCases :: TulaProgram -> [TulaCase]
getProgramCases = mapMaybe getCase . getProgramStatements
  where
    getCase (TStmtCase c) = Just c
    getCase _ = Nothing

getEither :: Either a a -> a
getEither (Right a) = a
getEither (Left a) = a

debugTape = words "0 0 0 0"

debugProgram =
  [ TulaCase "Inc" "1" "0" TRight "Inc" [],
    TulaCase "Inc" "0" "1" TRight "Halt" []
  ]

initState = TulaState debugTape 0 "Inc"

debugParseProgram :: FilePath -> Int -> IO ()
debugParseProgram path n = do
  src <- readFile path
  let prg = runStateT (nParser n) (TL.emptyState src)
  print prg
  where
    nParser = flip replicateM parseCase

generateCombinations :: [a] -> [b] -> [[(a, b)]]
generateCombinations names values =
  let valueCombinations = replicateM (length names) values
      nameValueCombinations = map (zip names) valueCombinations
   in nameValueCombinations
