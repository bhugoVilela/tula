module Main where
import TulaLexer (TulaTape(TulaTape), runParser, LexState (lexRest), getProgramTape, TulaProgram (..), TulaStatement(TStmtCase), TulaCase (..), runLexer, parseTapeFile, TulaAtom(..), TulaIdentifier (..), TulaLiteral (TLiteral), ShowSource (showSource))
import Control.Exception (IOException, throw, throwIO)
import GHC.IO.Exception (ExitCode (ExitFailure))
import System.Environment (getArgs)
import Control.Monad (unless, when)
import System.Exit (exitWith)
import Data.Maybe (isJust, mapMaybe, fromMaybe, isNothing)
import Runner (fromTulaProgram, TulaState (..), scanLive, runProgram, scanProgram, runEvalOnce, expandProgram)
import GHC.IO (catch)
import Debug.Trace (trace)
import Data.Functor ((<&>))
import Control.Applicative ((<|>))
import Data.Map qualified as Map
import qualified Data.Map.Lazy as Map
import Control.Monad.Trans.State (StateT(runStateT))
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.State.Lazy (runState)


main = do
  parsedArgs <- parseArgs <$> getArgs
  Args { mode, tulaFilePath, tapeFilePath } <- case parsedArgs of
    Left str -> putStrLn str >> exitWith (ExitFailure 1)
    Right args -> return args

  putStrLn $ "src: " ++ tulaFilePath ++ "\n" ++ "tapePath: " ++ tapeFilePath

  src <- readFile tulaFilePath
  let Just (program, lexState) = runParser src

  unless (null (lexRest lexState)) $ do
    putStrLn "An Error occured parsing the program"
    print lexState
    exitWith $ ExitFailure 1

  Just (TulaTape tape) <- do
    let pTape = getProgramTape program
    tape <- if isJust pTape
            then putStrLn "using program tape" >> return pTape
            else putStrLn "using tape file" >> readTape tapeFilePath
    unless (isJust tape) $ do
      putStrLn "Missing Tape"
      exitWith $ ExitFailure 1
    return tape

  let initialState = (fromTulaProgram program){ tape = tape }

  case mode of
    Interactive -> scanLive initialState
    Run         -> run initialState
    Trace       -> scanProgram initialState
    Expand      -> putStrLn . unlines $ (showSource <$> expand initialState)

  where
    readTape :: String -> IO (Maybe TulaTape)
    readTape path = do
      Just str <- safeReadFile path
      return $ runLexer parseTapeFile str <&> fst
    safeReadFile path = (Just <$> readFile path) `catch` ((\e -> pure Nothing) :: IOException -> IO (Maybe String))
    run :: TulaState -> IO ()
    run initialState = do
      let (res, newState) = runEvalOnce initialState
      runInterpreterFlags newState
      case res of
        Right _ -> run newState
        Left _ -> return ()

runInterpreterFlags :: TulaState -> IO ()
runInterpreterFlags state = do
  let Just (tcase, tstate) = matchingState state
  runMatchingCaseFlags tcase tstate state

runMatchingCaseFlags :: TulaCase -> TulaState -> TulaState -> IO()
runMatchingCaseFlags tcase prevState newState = do
  let flags = tcaseFlags tcase
  let Just runFlags = (sequence $ map (runInterpreterFlag prevState newState tcase) flags) >>= \x -> return (sequence x)
  runFlags
  return ()

runInterpreterFlag :: TulaState -> TulaState -> TulaCase -> TulaAtom -> Maybe (IO String)
runInterpreterFlag prevState newState tcase (TASExpr [])     = Nothing
runInterpreterFlag prevState newState tcase (TASExpr (op:xs)) = case op of
    TAIdentifier (TIdentifier "print" _) -> Just $ do
      let a = map (runInterpreterFlag prevState newState tcase) xs
      let (Just getStr) = sequence a >>= \x -> return (sequence x)
      str <- unwords <$> getStr
      putStrLn str
      return str
runInterpreterFlag prevState newState tcase (TAIdentifier t@(TIdentifier name _)) = return <$> value
    where
      maybeBinding = show <$> Map.lookup name (bindings prevState)
      value = case name of
        "$tape" -> Just $ unwords . map show $ tape prevState
        "$new_tape" -> Just $ unwords . map show $ tape newState
        "$cell" -> Just $ show $ tape prevState !! tapeIdx prevState
        "$new_cell" -> Just $ show $ tape newState !! tapeIdx newState
        "$case" -> Just $ show tcase
        _      ->  maybeBinding <|> Just (show t)
runInterpreterFlag prevState newState tcase (TALiteral t@(TLiteral name _)) = return . return $ name


getInitialState :: TulaProgram -> TulaAtom
getInitialState = tcaseState . head . mapMaybe getCase . getProgramStatements
  where
    getCase (TStmtCase c) = Just c
    getCase _ = Nothing

fst3 (a, b, c) = a

-- ARGS

data Mode = Interactive | Run | Trace | Expand
data Args = Args {
  mode         :: Mode,
  tulaFilePath :: String,
  tapeFilePath :: String
}

parseArgs :: [String] -> Either String Args
parseArgs [] = Left $ "Missing args\n" ++ usage
parseArgs args = if length args < 2 then Left $ "Missing args\n" ++ usage else do
    mode <- parseMode $ head args
    let file1 = args !! 1
    let file2 = if length args == 3 then Just $ args !! 2 else Nothing
    let file2' = fromMaybe "" $ file2 <|> Just (file1 ++ ".tape")
    let file1' = if isNothing file2 then file1 ++ ".tula" else file1
    return $ Args mode file1' file2'
  where
    parseMode :: String -> Either String Mode
    parseMode "run"   = return Run
    parseMode "trace" = return Trace
    parseMode "debug" = return Interactive
    parseMode "expand" = return Expand
    parseMode _       = Left "first argument must be mode"
usage = "USAGE:\n" ++ "<program> { run | trace | debug } <path_to_source> [path_to_tape]"

expand :: TulaState -> [TulaStatement]
expand state = case res of
  Right stmts -> stmts
  Left e -> trace e []
  where
    res = fst $ runState (runExceptT expandProgram) state

