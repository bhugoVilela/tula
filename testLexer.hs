import TulaLexer (runLexer, lexTulaIdentifier, Lexer, LexState (LexState, lexRest), TulaIdentifier (TIdentifier))
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE, catchE)
import Control.Monad (guard, when)

type Result a =  Either String a

testIdentifier = do
  (TIdentifier name _) <- matchesAll lexTulaIdentifier "abc"
  when (name /= "abc") $ do 
    Left $ "Expected " ++ "'abc'" ++ " but got " ++ name


testAll :: IO()
testAll = do
  test "TulaIdentifier" testIdentifier
  where
    test str test = do
      putStrLn $ "TEST: " ++ str
      putStr " "
      case test of
        Right _ -> putStrLn "OK"
        Left err -> putStrLn $ "ERR: " ++ err

--- UTILS

matches :: Lexer a -> String -> Result (a, LexState)
matches lexer input = expectToMatch (runLexer lexer input)

matchesAll :: Lexer a -> String -> Either String a
matchesAll lexer input = do
  (item, LexState {lexRest}) <- matches lexer input
  if null lexRest
    then return item
    else Left "Lexer didn't match everthing"

expectToMatch :: Maybe a -> Result a
expectToMatch (Just a) = Right a
expectToMatch (Just a) = Left "Failed to Match"
