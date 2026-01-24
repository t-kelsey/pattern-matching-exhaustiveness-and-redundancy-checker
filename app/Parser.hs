-- Code adapted from the lecture Functional Programming at the University of Freiburg
-- https://proglang.github.io/teaching/24ws/fp.html

newtype Parser t r = Parser { runParser :: [t] -> [(r, [t])] }

-- recognizes the empty language
pempty :: Parser t r
pempty = Parser $ \ts -> []

-- recognizes the language with just the empty word
succeed :: r -> Parser t r
succeed r = Parser $ \ts -> [(r, ts)]

-- `satisfy p` recognizes the language { a | p a }
satisfy :: (t -> Bool) -> Parser t t
satisfy p = Parser $ \ts -> case ts of
  (t : ts') | p t -> [(t, ts')]
  _               -> []

-- variation of satisfy
msatisfy :: (t -> Maybe r) -> Parser t r
msatisfy m = Parser $ \ts -> case ts of
  (t: ts) | Just r <- m t -> [(r, ts)]
  _                       -> []

-- `lit t` recognizes { t }
lit :: Eq t => t -> Parser t t
lit t = satisfy (== t)

-- alternative of parsers: recognizes the union of the languages of p1 and p2
palt :: Parser t r -> Parser t r -> Parser t r
palt (Parser p1) (Parser p2) = Parser $ \ts -> p1 ts ++ p2 ts

-- sequence of parsers: recognizes the concatenation of the languages of p1 and p2
pseq :: Parser t (a -> b) -> Parser t a -> Parser t b
pseq (Parser p1) (Parser p2) = Parser $ \ts ->
  [ (f a, ts2) | (f, ts1) <- p1 ts, (a, ts2) <- p2 ts1]

pmap :: (s -> r) -> (Parser t s -> Parser t r)
pmap f (Parser p) = Parser $ \ts -> [ (f s, ts') | (s, ts') <- p ts ]

-- Solution

runParserEnd :: Parser t a -> [t] -> [a]
runParserEnd p ts = [ x | (x, ts') <- runParser p ts, null ts' ]

instance Functor (Parser t) where
  fmap = pmap

instance Applicative (Parser t) where
  pure = succeed
  (<*>) = pseq

instance Alternative (Parser t) where
  empty = pempty
  (<|>) = palt

instance Monad (Parser t) where
  (Parser px) >>= f =
    Parser $ \ts -> px ts >>= \(x, ts') -> runParser (f x) ts'
  
many0 :: Parser t x -> Parser t [x]
many0 p = pure [] <|> many1 p 

many1 :: Parser t x -> Parser t [x]
many1 p = pure (:) <*> p <*> many0 p

poptional :: Parser t r -> Parser t (Maybe r)
poptional p = Just <$> p <|> pure Nothing

sepBy1 :: Parser t a -> Parser t b -> Parser t [a]
sepBy1 p sep = (:[]) <$> p 
           <|> (:) <$> p <* sep <*> sepBy0 p sep

sepBy0 :: Parser t a -> Parser t b -> Parser t [a]
sepBy0 p sep = pure [] <|> sepBy1 p sep

-- Note: `many0`, `many1`, `poptional` are actually automatically implemented
-- as `some`, `many`, and `optional` as this construction works for arbitrary
-- Applicatives with an Alternative instance.
-- See documentation of Alternative: 
--   https://hackage.haskell.org/package/base-4.20.0.1/docs/Control-Applicative.html#t:Alternative

lits :: (Eq t) => [t] -> Parser t [t]
lits []     = pure []
lits (t:ts) = pure (:) <*> lit t <*> lits ts

type Type = String
data Pattern = PVar String | PType Type | PCons [Pattern] | POr Pattern Pattern deriving (Eq, Show)
type DTypes = [(String, [Type])]
type PMat   = [[Pattern]]
type TypeToMatch = (Type, [Type])

data Match = MDTypes DTypes | MPMat PMat | MTypeToMatch TypeToMatch deriving (Eq, Show)

string :: Parser Char String
string = lit '"' *> many0 (satisfy (/= '"')) <* lit '"'

dtypes :: Parser Char DTypes
dtypes = many1 dtype where  -- this still needs to be made into one list
    dtype = toTuple <$ lits "data" *> ws *> string <* lits "where" *> ws *> dTypeDefs *> ws
        where toTuple name defs = (name, defs) -- this whole thing probably doesn't work

match :: Parser Char Match
match = MDTypes <$> dtypes
    <|> MPMat   <$> pmat
    <|> MTypeToMatch <$> typetomatch

match' :: Parser Char Match
match' = ws *> match <* ws

data Expr1 = ENat Int | EAdd Expr1 Expr1 deriving (Eq, Show)

pDigit :: Parser Char Int
pDigit = msatisfy (\c -> readMaybe [c])

digitsToInt :: [Int] -> Int
digitsToInt ds = sum $ zipWith (*) ds $ map (10^) $ reverse [0..length ds-1]

nat :: Parser Char Int
nat = fmap digitsToInt (many1 pDigit)

expr1 :: Parser Char Expr1
expr1 = enat <|> eadd where
  enat = do
    n <- nat
    return $ ENat n
  eadd = do
    lit '('
    e1 <- expr1
    lit '+'
    e2 <- expr1
    lit ')'
    return $ EAdd e1 e2

expr1' :: Parser Char Expr1
expr1' = ENat <$> nat
     <|> EAdd <$ lit '(' <*> expr1 <* lit '+' <*> expr1 <* lit ')'

example = runParser expr1 "(2 + (3 + 5))"

ws :: Parser Char [Char]
ws = many0 (lit ' ' <|> lit '\n' <|> lit '\t' <|> lit '\r')

int :: Parser Char Int
int = toInt <$> poptional (lit '-') <*> nat where
  toInt Nothing  n = n
  toInt (Just _) n = -n

bool :: Parser Char Bool
bool = True  <$ lits "true" 
   <|> False <$ lits "false"

data JSON = JInt Int
          | JBool Bool
          | JNull
          | JString String
          | JList [JSON] 
          | JObject [(String, JSON)]
          deriving (Show, Eq)

string :: Parser Char String
string = lit '"' *> many0 (satisfy (/= '"')) <* lit '"'

commaSep0 :: Parser Char a -> Parser Char [a]
commaSep0 p = p `sepBy0` (lit ',' <* ws)

json :: Parser Char JSON
json = JInt    <$> int
   <|> JBool   <$> bool
   <|> JNull   <$  lits "null"
   <|> JString <$> string
   <|> JList   <$  lit '[' <* ws <*> commaSep0 (json <* ws) <* lit ']'
   <|> JObject <$  lit '{' <* ws <*> commaSep0 ((,) <$> string <* ws <* lit ':' <* ws <*> json) <* ws <* lit '}'

jsonM :: Parser Char JSON
jsonM = jInt <|> jBool <|> jNull <|> jString <|> jList <|> jObject where
  jInt = do
    i <- int
    return $ JInt i
  jBool = do
    b <- bool
    return $ JBool b
  jNull = do
    lits "null"
    return JNull
  jString = do
    s <- string
    return $ JString s
  jList = do
    lit '[' 
    ws 
    xs <- commaSep0 jsonM
    ws 
    lit ')'
    return $ JList xs
  jObject = do
    lit '{' 
    ws
    items <- commaSep0 $ do
      key <- string
      ws
      lit ':'
      ws
      val <- jsonM
      return (key, val)
    ws 
    lit '}'
    return $ JObject items

json' :: Parser Char JSON
json' = ws *> json <* ws

prettyJson :: JSON -> String
prettyJson (JInt i) = show i
prettyJson (JBool True) = "true"
prettyJson (JBool False) = "false"
prettyJson (JString s) = "\"" ++ s ++ "\""
prettyJson JNull = "null"
prettyJson (JList xs) = "[" ++ intercalate ", " (map prettyJson xs) ++ "]"
prettyJson (JObject items) = "{" ++ intercalate ", " (map prettyItem items) ++ "}" where
  prettyItem (k, v) = "\"" ++ k ++ "\"" ++ ": " ++ prettyJson v

exJson :: String
exJson = unlines
  [ "{"
  , "  \"foo\": 42,"
  , "  \"bar\": [1, 2, false, null],"
  , "  \"baz\": \"boo\""
  , "}"
  ]

exParseJson :: [JSON]
exParseJson = runParserEnd json' exJson