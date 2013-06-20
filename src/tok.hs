{-# LANGUAGE   NoMonomorphismRestriction
             , FlexibleContexts
             , FlexibleInstances
             , UndecidableInstances
             , MultiParamTypeClasses
             , FunctionalDependencies           #-}

import Control.Monad.State       (MonadState(..), StateT(..))
import Control.Applicative       (Applicative(..), Alternative(..))
import Control.Monad.Trans       (MonadTrans(..))
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Traversable          (Traversable(..))

-- Replacement MonadError that doesn't require Error constraint for instances

class Monad m => MonadError e m | m -> e where
  throwError :: e -> m a
  catchError :: m a -> (e -> m a) -> m a

instance MonadError e (Either e) where
  throwError               =  Left
  catchError  (Right x) _  =  Right x
  catchError  (Left e)  f  =  f e
  
instance MonadError e m => MonadError e (StateT s m) where
  throwError      =  lift . throwError
  catchError m f  =  StateT g
    where
      g s = catchError (runStateT m s) 
                       (\e -> runStateT (f e) s)

instance MonadError e m => MonadError e (MaybeT m) where
  throwError      =  lift . throwError
  catchError m f  =  MaybeT $ catchError (runMaybeT m) (runMaybeT . f)


class Switch f where
  switch :: f a -> f ()

instance Switch Maybe where
  switch (Just _) = Nothing
  switch Nothing  = Just ()

instance (Functor m, Switch m) => Switch (StateT s m) where
  switch (StateT f) = StateT (\s -> fmap (const ((), s)) . switch $ f s)

instance Functor m => Switch (MaybeT m) where
  switch (MaybeT m) = MaybeT (fmap switch m)


-- the prelude:  type classes, data types, instances, and general combinators

type Pos = (Int, Int)
type Err = (String, Pos)

-- ErrorT e m a :: m (Either e a)
-- MaybeT m a :: m (Maybe a)
type Parser t a = StateT [t] (StateT Pos (MaybeT (Either Err))) a 

runParser :: Parser t a -> [t] -> Either Err (Maybe ((a, [t]), Pos))
runParser p ts = runMaybeT $ runStateT (runStateT p ts) (1, 1)


many0 = many
many1 = some


item :: (MonadState [t] m, Alternative m) => m t
item =
  get >>= \xs -> case xs of
                        (t:ts) -> put ts *> pure t;
                        []     -> empty;

getState = lift get
putState = lift . put
updateState f = getState >>= (putState . f)

bumpLine (ln, c) = (ln + 1, 1)
bumpCol (ln, c) = (ln, c + 1)

count :: Parser Char Char
count = item >>= \x -> updateState (f x) >> pure x
  where f '\n' = bumpLine
        f _    = bumpCol

check :: (Monad m, Alternative m) => (a -> Bool) -> m a -> m a
check f p =
    p >>= \x ->
    if (f x) then return x else empty

satisfy :: (Char -> Bool) -> Parser Char Char
satisfy = flip check count

literal :: Char -> Parser Char Char
literal c = satisfy ((==) c)

string :: String -> Parser Char String
string = sequenceA . map literal

not1 :: Parser Char a -> Parser Char Char
not1 p = switch p *> count

not0 :: Parser t a -> Parser t ()
not0 = switch

end :: Parser Char ()
end = switch count

commit :: (MonadError e m, Alternative m) => e -> m a -> m a
commit err p = p <|> throwError err

optionalM :: Alternative f => f a -> f (Maybe a)
optionalM p = fmap Just p <|> pure Nothing

--------------------------------
-- error message example

data Silly 
  = Paren [Silly]
  | Square [Silly]
  | Curly [Silly]
  | SSymbol String
  | SNumber String
  deriving (Show, Eq)

ws :: Parser Char String
ws = many1 (satisfy (flip elem " \t\n\r\f\f"))

munch :: Parser Char a -> Parser Char a
munch p = optionalM ws *> p

op = string "("
cp = string ")"
oc = string "{"
cc = string "}"
os = string "["
cs = string "]"
_digit = satisfy (flip elem "0123456789")
num = many1 _digit
sym = fmap (:) first <*> many0 rest
  where first = satisfy (flip elem ('_' : ['a' .. 'z'] ++ ['A' .. 'Z']))
        rest = first <|> _digit

token :: Parser Char (Pos, String)
token = munch (getState >>= \p -> fmap (\s -> (p, s)) toks)
  where toks = foldr (<|>) empty [op, 
                                  cp, 
                                  oc, 
                                  cc, 
                                  os, 
                                  cs, 
                                  num, 
                                  sym, 
                                  fmap (:[]) item]

tok :: String -> Parser Char (Pos, String)
tok s = check (\(_, ts) -> s == ts) token

between :: Parser Char a -> Parser Char b -> Parser Char c -> String -> Parser Char [c]
between open close body message = 
    open                                  *>
    many0 (body <|> checkError)          <*
    (getState                            >>= \p ->
     commit ("missing close", p) close)
  where
    checkError = not0 close   *> 
                 token       >>= \(p, _) -> 
                 throwError (message, p)

paren :: Parser Char [Silly]
paren = between (tok "(") (tok ")") silly "in application: invalid content"

curly :: Parser Char [Silly]
curly = between (tok "{") (tok "}") silly "in curlies: invalid content"

square :: Parser Char [Silly]
square = between (tok "[") (tok "]") silly "in squares: invalid content"

-- I need to be able to check for token type to be able to re-implement this using `token`
number :: Parser Char String
number = munch $ many1 (satisfy (flip elem "0123456789"))

symbol :: Parser Char String
symbol = munch (fmap (:) first <*> rest)
  where first = satisfy (flip elem ('_' : ['a' .. 'z'] ++ ['A' .. 'Z']))
        rest = many0 (first <|> satisfy (flip elem "0123456789"))

silly :: Parser Char Silly
silly = fmap Paren   paren    <|> 
        fmap SNumber number   <|>
        fmap Square  square   <|>
        fmap Curly   curly    <|>
        fmap SSymbol symbol

