import Control.Monad
import qualified Control.Applicative as CA

data Parser a = Parser { parse :: String -> [(a, String)] }

-- common functor and applicative defination based on monad
instance Functor Parser where
  fmap f p = do
    v <- p
    return $ f v

instance Applicative Parser where
  pure = return
  pf <*> pv = do
    f <- pf
    v <- pv
    return $ f v

instance Monad Parser where
  return v = Parser (\s -> [(v, s)])
  p >>= f = Parser (\s -> (parse p s) >>= \(v, s') -> parse (f v) s')

-- for MonadPlus
instance CA.Alternative Parser where
  empty = mzero
  (<|>) = mplus

instance MonadPlus Parser where
  mzero = Parser (\_ -> [])
  mplus p q = Parser (\s -> parse p s ++ parse q s)

data MindTree = MindNode String [MindTree] deriving Show

(+++) :: Parser a -> Parser a -> Parser a
(+++) p q = Parser (\s -> headWithFail $ parse parserSum s)
  where
    parserSum = p `mplus` q
    headWithFail [] = []
    headWithFail (h:_) = [h]

item :: Parser Char
item = Parser getHead
  where
    getHead "" = []
    getHead (c:cs) = [(c, cs)]

sat :: (Char -> Bool) -> Parser Char
sat p = do
  c <- item
  if p c
  then return c
  else mzero

char c = sat (==c)

many :: Parser a -> Parser [a]
many p = do {x <- p; xs <- many p; return (x:xs)} +++ return []

isChar '\n' = False
isChar ')' = False
isChar '(' = False
isChar _ = True

normalChar :: Parser Char
normalChar = sat isChar

firstChar = sat nospace
  where
    nospace ' ' = False
    nospace x = isChar x

term :: Parser String
term = do {
  c <- normalChar;
  cs <- term;
  return $ c:cs
} +++ do {
  c <- normalChar;
  return $ [c]
}

termIndent = do
  many $ char ' '
  c <- firstChar
  cs <- term
  return $ c:cs

list :: Parser [MindTree]
list = do {
  n <- expression;
  char '\n';
  ns <- list;
  return $ n:ns
} +++ do {
  n <- expression;
  return $ [n]
}

expression :: Parser MindTree
expression = do {
  s <- termIndent;
  return $ MindNode s []
} +++ do {
  many $ char ' ';
  char '(';
  s <- termIndent;
  char '\n';
  es <- list;
  char ')';
  return $ MindNode s es
}

reduceTree :: MindTree -> [String]
reduceTree (MindNode s []) = []
reduceTree (MindNode s nodes) = childs ++ lines
  where
    childs = concat $ map reduceTree nodes
    draw (MindNode t _) = "\"" ++ s ++ "\" -> \"" ++ t ++ "\""
    lines = map draw nodes

main = do
  s <- getContents
  let [(tree, _)] = (parse expression) s
  let dot = foldl (\s -> \line -> s ++ "\n  " ++ line) "" (reduceTree tree)
  putStr $ "digraph {" ++ dot ++ "\n}"
