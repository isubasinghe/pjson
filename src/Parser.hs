module Parser (char, string, float, integer, Parser (..)) where

import Control.Applicative (Alternative (..), liftA2)
import Data.Bifunctor (Bifunctor (first))
import Data.Foldable (asum)
import Data.Map.Lazy (Map, fromList)

newtype Parser a = Parser {runParser :: String -> Either String (a, String)}

instance Functor Parser where
  fmap f (Parser p) = Parser (fmap (first f) . p)

instance Applicative Parser where
  pure a = Parser (\input -> Right (a, input))
  (<*>) (Parser p) (Parser p') = Parser $ \input -> do
    (a, s) <- p input
    (a', s') <- p' s
    Right (a a', s')

instance Monad Parser where
  (>>=) (Parser p) f = Parser $ \input -> case p input of
    Right (a, s) -> runParser (f a) s
    Left l -> Left l

instance Alternative Parser where
  empty = Parser (\_ -> Left "Empty")
  (<|>) (Parser p) (Parser p') = Parser $ \input -> p input <> p' input

char :: Char -> Parser Char
char c = do
  Parser f
  where
    f [] = Left ("Wanted " ++ [c] ++ " but got EOF")
    f (c' : cs)
      | c' == c = Right (c, cs)
      | otherwise = Left ("Expected " ++ [c] ++ " but got " ++ [c'])
{-# INLINE char #-}

string :: String -> Parser String
string = traverse char

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep el = ((:) <$> el) <*> many (sep *> el)
{-# SPECIALIZE INLINE sepBy :: Parser Char -> Parser String -> Parser [String] #-}

integer :: Parser JsonObject
integer = (\s -> JsonInt (read s :: Integer)) <$> integer'

integer' :: Parser [Char]
integer' = some (asum $ char <$> ['0' .. '9'])
{-# INLINE integer' #-}

float :: Parser JsonObject
float = do
  int <- integer'
  dot <- char '.'
  int' <- integer'
  return (JsonDouble (read (int ++ [dot] ++ int') :: Double))

-- type Ident = String

data JsonObject
  = JsonInt Integer
  | JsonDouble Double
  | JsonString String
  | JsonArray [JsonObject]
  | JsonNull
  | JsonObject (Map String JsonObject)
  deriving (Show, Eq, Ord)

space :: Parser Char
space = char ' ' <|> char '\t' <|> char '\n'

takeTill :: (Char -> Bool) -> Parser [Char]
takeTill f = do Parser (Right . f')
  where
    f' [] = ("", "")
    f' (c : cs) =
      if f c
        then
          let (p, cs') = f' cs
           in (c : p, cs')
        else ("", c : cs)

string' :: Parser JsonObject
string' = do
  char '"'
  insideString <- takeTill (/= '"')
  char '"'
  return (JsonString insideString)

string'' :: Parser String
string'' = do
  char '"'
  insideString <- takeTill (/= '"')
  char '"'
  return insideString

null' :: Parser JsonObject
null' = do
  string "null"
  return JsonNull

all' :: Parser JsonObject
all' = object <|> array <|> string' <|> null' <|> float <|> integer

array :: Parser JsonObject
array = do
  contents <- ((ws *> char '[' <* ws) *> (sepBy (ws *> char ',' <* ws) all' <|> pure []) <* ws <* char ']')
  return (JsonArray contents)

ws :: Parser [Char]
ws = many space
{-# INLINE ws #-}

object :: Parser JsonObject
object = do
  ws *> char '{' <* ws
  contents <- (sepBy (ws *> char ',' <* ws) (liftA2 (,) (string'' <* ws <* char ':' <* ws) all')) <|> pure []
  ws *> char '}'
  return (JsonObject $ fromList contents)