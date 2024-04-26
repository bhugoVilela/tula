module TulaLexer (TulaTape(..), parseProgram, TulaProgram(..), TulaStatement(..), TulaCase(..), lexToken, emptyState, TulaDirection(TRight, TLeft), LexState(..), parseCase, TulaFor(..), TulaSet(..), statementIsExpression, runParser, runLexer, TulaAtom(..), lexTulaIdentifier, lexTulaLiteral, lexAtom, Lexer, TulaIdentifier(..), TulaLiteral(..), parseTapeDeclaration, parseTapeFile) where

import Data.Char (isSpace)
import Control.Monad.Trans.State (StateT (runStateT), modify, state, get, execState, execStateT, put)
import Control.Applicative ( Alternative(many, empty), (<|>), optional )
import Control.Monad ( void, when, guard )
import GHC.Base (Alternative(some))
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode (ExitFailure))
import System.IO (openFile, IOMode (ReadMode))
-- State read write -> State

specialChars = [ '{', '}', '(', ')' ]
specialKeywords = [ "case", "in", "let", "for" ]

type Row = Int
type Col = Int
type Symbol = String
type ReadSymbol = TulaAtom
type WriteSymbol = TulaAtom
type TulaState = Symbol
data TulaDirection = TLeft | TRight
  deriving (Show, Eq)

data TulaAtom = TAIdentifier TulaIdentifier
              | TALiteral TulaLiteral
              | TASExpr [TulaAtom]

data TulaIdentifier = TIdentifier { idName :: String, idLexem :: Lexem }
data TulaLiteral = TLiteral { literalName :: String, literalLexem :: Lexem }

instance Show TulaIdentifier where
  show a = if null $ idName a then "-" else idName a
instance Show TulaLiteral where
  show = literalName

instance Show TulaAtom where
  show (TAIdentifier t) = show t
  show (TALiteral t)    = show t
  show (TASExpr list)   = "(" ++ unwords (map show list) ++ ")"

data TulaCase = TulaCase {
  tcaseState    :: TulaAtom,
  tcaseRead     :: TulaAtom,
  tcaseWrite    :: TulaAtom,
  tcaseDir      :: TulaDirection,
  tcaseNewState :: TulaAtom
}

instance Show TulaCase where
  show (TulaCase state read write dir newState) = unwords [show state, show read, show write, show dir, show newState]

data TulaSet = TulaSet {
  getName :: String,
  getSymbols :: [TulaAtom]
} deriving (Show)

data SExpr = SAtom String
           | SList [SExpr]
  deriving (Show)

data TulaFor = TulaFor {
  bindings :: [TulaIdentifier],
  source   :: [TulaIdentifier],
  body     :: [TulaStatement]
} deriving Show

newtype TulaTape = TulaTape [TulaAtom]
  deriving Show

-- state monad, result is Lexem, state is LexData

data LexState = LexState {
  lexRest :: String,
  lexRow :: Int,
  lexCol :: Int
} deriving (Show)

type Lexem = (String, Row, Col)
type Lexer = StateT LexState Maybe

data TulaStatement = TStmtCase TulaCase
                    | TStmtSet TulaSet
                    | TStmtFor TulaFor
  deriving (Show)

statementIsExpression :: TulaStatement -> Bool
statementIsExpression (TStmtCase _) = True
statementIsExpression (TStmtFor _) = True
statementIsExpression (TStmtSet _) = False

data TulaProgram = TulaProgram {
    getProgramTape       :: Maybe TulaTape,
    getProgramStatements :: [TulaStatement]
  }
  deriving (Show)

advance '\n' = ((+1),(*0))
advance _ = (id,(+1))

emptyState str = LexState str 0 0

-- Lex while char condition is true
lexIfC :: (Char -> Bool) -> Lexer Char
lexIfC pred = do
  LexState str@(c:cs) row col <- get
  if null str then
    fail ""
  else if pred c then do
    let (advanceRow, advanceCol) = advance c
    put $ LexState cs (advanceRow row) (advanceCol col)
    return c
  else fail ""

inlineWs :: Lexer ()
inlineWs = void $ many $ lexIfC (\x -> isSpace x && x /= '\n')

comment :: Lexer ()
comment = do
  lexChar '-'
  lexChar '-'
  many $ lexIfC (/= '\n')
  return ()

ws :: Lexer ()
ws = void $ many $ (void (lexIfC isSpace) <|> comment)

ws1 :: Lexer ()
ws1 = void $ some $ lexIfC isSpace

lexEOF :: Lexer ()
lexEOF = do
  LexState rest _ _ <- get
  guard (null rest)
  return ()

lexToken :: Lexer Lexem
lexToken = do
  ws
  token <- some $ lexIfC (not . isSpace)
  LexState rest row col <- get
  ws
  return (token, row, col - length token)

lexAtom :: Lexer TulaAtom
lexAtom = do
  atom <- (TAIdentifier <$> lexTulaIdentifier) <|> (TALiteral <$> lexTulaLiteral) <|> lexAtomSexpr
  ws
  return atom

lexTulaIdentifier :: Lexer TulaIdentifier
lexTulaIdentifier = do
  l@(name,_,_) <- lexIdentifier
  guard $ head name /= '\''
  return $ TIdentifier name l

lexTulaLiteral :: Lexer TulaLiteral
lexTulaLiteral = do
  l@(name,_,_) <- lexIdentifier
  guard $ head name == '\''
  return $ TLiteral name l

lexAtomSexpr :: Lexer TulaAtom
lexAtomSexpr = do
  ws
  lexChar '('
  atoms <- many lexAtom
  lexChar ')'
  return $ TASExpr atoms

lexIdentifier :: Lexer Lexem
lexIdentifier = do
  ws
  LexState (x:xs) row col <- get
  guard (x `notElem` specialChars)
  token <- if x == '\'' then do
    let (match, rest) = span (/= '\'') xs
    let rest' = if null rest then rest else drop 1 rest
    put $ LexState rest' row (col + length match + 1)
    return (match, row, col)
    else do
      let (match, rest) = break isBreak xs
      put $ LexState rest row col
      return (x:match, row, col)
  guard $ (fst3 token) `notElem` specialKeywords
  ws
  return token
  where
    isBreak c = isSpace c || c `elem` specialChars

runLexer lexer str = runStateT lexer (LexState str 0 0)

lexChar :: Char -> Lexer Char
lexChar c = lexIfC (== c)

lexKeyword :: String -> Lexer Lexem
lexKeyword str = do
  ws
  token <- mapM lexChar str
  s <- get
  ws1 <|> lexEOF
  return (token, lexRow s, lexCol s - length token)

parseCase :: Lexer TulaCase
parseCase = do
  caseKeyword <- lexKeyword "case"
  state       <- lexAtom
  read        <- lexAtom
  write       <- lexAtom
  direction   <- lexKeyword "->" <|> lexKeyword "<-"
  newState    <- lexAtom
  let (Just dir) = directionFromStr $ fst3 direction
  return $ TulaCase state read write dir newState
  where
    directionFromStr str = case str of
      "->" -> Just TRight
      "<-" -> Just TLeft
      _    -> Nothing

fst3 (a, b, c) = a

parseTulaSet = do
  lexKeyword "let"
  name <- lexToken
  lexKeyword "{"
  items <- many lexAtom
  lexKeyword "}"
  return $ TulaSet (fst3 name) items

lexTokenNotBracket = do
  token <- lexToken
  guard $ fst3 token /= "}"
  return token

lexCondToken cond = do
  token <- lexToken
  guard $ cond (fst3 token)
  return token

parseTapeDeclaration :: Lexer TulaTape
parseTapeDeclaration = do
  lexKeyword "tape"
  lexKeyword "{"
  items <- many lexAtom
  lexKeyword "}"
  return $ TulaTape items

parseTapeBody :: Lexer [TulaAtom]
parseTapeBody = many lexAtom

parseTapeFile :: Lexer TulaTape
parseTapeFile = TulaTape <$> parseTapeBody

parseFor :: Lexer TulaFor
parseFor = do
  lexKeyword "for"
  bindings <- many lexTulaIdentifier
  lexKeyword "in"
  source <- many lexTulaIdentifier
  body <- parseBody <|> ((:[]) <$> parseExpression)
  return TulaFor {
    bindings=bindings,
    source=source,
    body=body}
  where
    parseBody = do
      lexKeyword "{"
      statements <- many parseExpression
      lexKeyword "}"
      return statements

parseExpression :: Lexer TulaStatement
parseExpression = (TStmtCase <$> parseCase) <|> (TStmtFor <$> parseFor)

parseStatement :: Lexer TulaStatement
parseStatement = (TStmtSet <$> parseTulaSet) <|> parseExpression

parseProgram :: Lexer TulaProgram
parseProgram = do
  tape <- optional parseTapeDeclaration
  statements <- many parseStatement
  ws
  return $ TulaProgram tape statements

runParser str = runStateT parseProgram (emptyState str)

