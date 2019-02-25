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

main = do
  return ()
