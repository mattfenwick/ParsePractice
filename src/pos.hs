{-# LANGUAGE   NoMonomorphismRestriction
             , FlexibleContexts
             , FlexibleInstances
             , UndecidableInstances
             , MultiParamTypeClasses
             , FunctionalDependencies           #-}

import Control.Monad.State       (MonadState(..), StateT(..))
import Control.Applicative       (Applicative(..), Alternative(..))
import Data.List                 (nub)
import Control.Monad.Trans       (MonadTrans(..))
import Control.Monad.Trans.Maybe (MaybeT(..))

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

sillyMunch :: Parser Char a -> Parser Char a
sillyMunch p = optionalM ws *> p

op = sillyMunch $ literal '('
cp = sillyMunch $ literal ')'
oc = sillyMunch $ literal '{'
cc = sillyMunch $ literal '}'
os = sillyMunch $ literal '['
cs = sillyMunch $ literal ']'

between :: Parser Char a -> Parser Char b -> Parser Char c -> String -> Parser Char [c]
between open close body message = 
    open                                  *>
    many0 (body <|> checkError)          <*
    (getState                            >>= \p ->
     commit ("missing close", p) close)
  where
    checkError = getState    >>= \p -> 
                 not0 close   *> 
                 throwError (message, p)

sillyParen :: Parser Char [Silly]
sillyParen = between op cp silly "in application: invalid content"

sillyCurly :: Parser Char [Silly]
sillyCurly = between oc cc silly "in curlies: invalid content"

sillySquare :: Parser Char [Silly]
sillySquare = between os cs silly "in squares: invalid content"

sillyNumber :: Parser Char String
sillyNumber = sillyMunch $ many1 (satisfy (flip elem "0123456789"))

sillySymbol :: Parser Char String
sillySymbol = sillyMunch (fmap (:) first <*> rest)
  where first = satisfy (flip elem ('_' : ['a' .. 'z'] ++ ['A' .. 'Z']))
        rest = many0 (first <|> satisfy (flip elem "0123456789"))

silly :: Parser Char Silly
silly = fmap Paren sillyParen      <|> 
        fmap SNumber sillyNumber   <|>
        fmap Square sillySquare    <|>
        fmap Curly sillyCurly      <|>
        fmap SSymbol sillySymbol


--


data AST
    = ASymbol String
    | ALambda [String] [AST]
    | ADefine String AST
    | AApp    AST  [AST]
  deriving (Show, Eq)


example = "{define \n\
\  f \n\
\  {lambda {x y}\n\
\    (plus x y)}}\n\
\\n\
\(a b (c d e))\n\
\\n\
\; here's a nice comment !!\n\
\\n\
\"

-- the actual parser

whitespace = many1 $ satisfy (flip elem " \n\t\r\f")
comment = pure (:) <*> literal ';' <*> many0 (not1 $ literal '\n')

munch p = many0 (whitespace <|> comment) *> p


opencurly  = munch $ literal '{'
closecurly = munch $ literal '}'
openparen  = munch $ literal '('
closeparen = munch $ literal ')'
symbol = munch $ many1 char
  where char = satisfy (flip elem (['a' .. 'z'] ++ ['A' .. 'Z']))

application =
    openparen            *>
    getState            >>= \op ->
    pure AApp           <*>
    commit ("application: missing operator", op) form      <*>
    many0 form          <*
    (getState            >>= \p ->
     commit ("application: missing close paren", p) closeparen)

define =
    opencurly                    *>
    check (== "define") symbol   *>
    pure ADefine                <*>
    symbol                      <*>
    form                        <*
    closecurly

lambda = 
    opencurly                       *>
    check (== "lambda") symbol      *>
    opencurly                       *>
    pure ALambda                   <*>
    check distinct (many0 symbol)  <*>
    (closecurly                     *>
     many1 form                    <*
     closecurly)
  where
    distinct names = length names == length (nub names)

special = define <|> lambda

form = fmap ASymbol symbol <|> application <|> special

woof = many1 form <* munch end

{-
test = runParser woof example == r
  where r = Just ([ADefine "f" (ALambda ["x","y"] [AApp (ASymbol "plus") [ASymbol "x",ASymbol "y"]]),
                   AApp (ASymbol "a") [ASymbol "b",AApp (ASymbol "c") [ASymbol "d",ASymbol "e"]]],
                  "")
-}
