module Main where
import TulaLexer (TulaTape(TulaTape), runParser, LexState (lexRest), getProgramTape, TulaProgram (..), TulaStatement(TStmtCase), TulaCase (..), runLexer, parseTapeFile, TulaAtom)
import Control.Exception (IOException, throw, throwIO)
import GHC.IO.Exception (ExitCode (ExitFailure))
import System.Environment (getArgs)
import Control.Monad (unless, when)
import System.Exit (exitWith)
import Data.Maybe (isJust, mapMaybe, fromMaybe, isNothing)
import Runner (fromTulaProgram, TulaState (tape), scanLive, runProgram, scanProgram)
import GHC.IO (catch)
import Debug.Trace (trace)
import Data.Functor ((<&>))
import Control.Applicative ((<|>))


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
    Run         -> putStrLn "Run Not implemented yet" >> exitWith (ExitFailure 1)
    Trace       -> scanProgram initialState

  where
    readTape :: String -> IO (Maybe TulaTape)
    readTape path = do
      Just str <- safeReadFile path
      return $ runLexer parseTapeFile str <&> fst
    safeReadFile path = (Just <$> readFile path) `catch` ((\e -> pure Nothing) :: IOException -> IO (Maybe String))

getInitialState :: TulaProgram -> TulaAtom
getInitialState = tcaseState . head . mapMaybe getCase . getProgramStatements
  where
    getCase (TStmtCase c) = Just c
    getCase _ = Nothing

fst3 (a, b, c) = a

-- ARGS

data Mode = Interactive | Run | Trace
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
    parseMode _       = Left "first argument must be mode"
usage = "USAGE:\n" ++ "<program> { run | trace | debug } <path_to_source> [path_to_tape]"
