-- Code adapted from the lecture Functional Programming at the University of Freiburg
-- https://proglang.github.io/teaching/24ws/fp.html

module Parser where
import Control.Applicative
import Text.Read (readMaybe)
import Data.Char (isAlpha)
import GHC.IO.Handle (hFlush)
import GHC.IO.Handle.FD (stdout)
import Data.List (intercalate)
import Data.Foldable (forM_)
import Data.List (delete)
import Data.Char (isUpper, isAlphaNum)

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

--string :: Parser Char String
--string = lit '"' *> many0 (satisfy (/= '"')) <* lit '"'

string :: Parser Char String
string = many1 (satisfy isAlphaNum)

-- A structure for types, e.g.: "OneOfThose Nat"
data Type
  = TCon String
  | TApp Type Type
  deriving (Eq, Show)

-- A type for our data type definitions, e.g.: 
-- data Unit where
--    tt : Unit
type DType = (String, [(String, [Type])])
type DTypes = [DType]

-- A structure for each pattern in the matrix, e.g.: "(nat x zero y)"
data Pattern = PCon String
            | PVar String
            | PApp Pattern [Pattern]
            | POr Pattern Pattern
            deriving (Eq, Show)

-- A type for our pattern matrices
type PMat  = [[Pattern]]

-- A type for the entire structure that should be parsed
type Match = (DTypes, PMat, Type)


-- MATCH PARSER

-- Parses a type, along with any applications already applied, e.g.: "OneOfThose Nat", into a structure
ttypeatom :: Parser Char Type
ttypeatom = TCon <$> string

ttype :: Parser Char Type
ttype = foldl1 TApp <$> many1 (ttypeatom <* ws)

-- Parses "a -> b -> c ..." into a list of types
typeSignature :: Parser Char [Type] 
typeSignature = sepBy1 ttype (ws *> lits "->" <* ws)

-- Parses any amount of "f : a -> b -> c ..." type signatures into a list 
constrDeclarations :: Parser Char [(String, [Type])]
constrDeclarations = many1 constrDeclaration where
  constrDeclaration = (,) <$> string <* ws <* lit ':' <* ws <*> typeSignature

-- Parses any amount of data types and their constructor declarations into a structure
dtypes :: Parser Char DTypes
dtypes = many1 dtype where
  dtype = (,) <$> (lits "data" *> ws *> string) <* ws <* lits "where" <* ws <*> constrDeclarations <* ws

-- Parses a matrix of patterns [[Pattern]]. We don't need to check if it's of size n*m, as rules that are not the right size won't match
-- Here the design decision is that single-length alphastrings are parsed as vars ("x"), while longer strings are cons ("zero")
pmat :: Parser Char PMat
pmat = many1 (pMatchRule <* ws) where
  pMatchRule = many1 (p <* ws')

-- Manual recursion as string is not greedy enough somehow for the pattern
pString :: Parser Char String
pString = do
  c  <- satisfy isAlphaNum
  cs <- (pString <|> pure "")
  pure (c:cs)

pAtom :: Parser Char Pattern
pAtom = pName <|> pApp
  where
    pApp = PApp <$ lit '(' <* ws' <*> pName <* ws' <*> many0 (p <* ws') <* lit ')'
    pName = do
      name <- pString
      pure $ case name of
        (x:_) | isUpper x -> PCon name
        _                 -> PVar name


-- Match each individual pattern in the pattern matrix
p :: Parser Char Pattern
p = pAtom >>= \left ->
        (ws' *> lit '|' *> ws' *> pAtom >>= \right -> 
            pure (POr left right))
        <|> pure left

match :: Parser Char Match
match =  
  (,,)
    <$> (ws *> lits "=== data types ===" *> ws *> dtypes)
    <*> (ws *> lits "=== pattern matrix ===" *> ws *> pmat)
    <*> (ws *> lits "=== type ===" *> ws *> ttype)

match' :: Parser Char Match
match' = ws *> match <* ws

-- pretty[datatype] is for displaying the parsed data type (e.g. debugging)

prettyMatch :: Match -> String
prettyMatch (x, y, z) = "\n=== data types ===\n" ++ prettyDTypes x ++ "\n\n=== pattern matrix ===\n" ++ prettyPMat y ++ "\n\n=== type ===\n" ++ prettyType z

prettyDTypes :: DTypes -> String
prettyDTypes xs = intercalate "\n\n" $ prettyDType <$> xs

prettyDType :: DType -> String
prettyDType xs = first xs ++ "\n  " ++ intercalate "\n  " (map (\x -> first x ++ " : " ++ intercalate " -> " (map prettyType (second x))) (second xs))
  where first (x, _) = x
        second (_, x) = x

data Expr1 = ENat Int | EAdd Expr1 Expr1 deriving (Eq, Show)

prettyPMat :: PMat -> String
prettyPMat xs = concat $ (fmap $ intercalate "") $ ((fmap . fmap) prettyP xs)

prettyP :: Pattern -> String
prettyP (PVar x) = x
prettyP (PCon x) = x
prettyP (POr x y) = prettyP x ++ " | " ++ prettyP y
prettyP (PApp x xs) = "(" ++ intercalate " " (prettyP x : (prettyP <$> xs)) ++ ")"

prettyType :: Type -> String
prettyType (TCon x) = x
prettyType (TApp x y) = prettyType x ++ prettyType y

pDigit :: Parser Char Int
pDigit = msatisfy (\c -> readMaybe [c])

digitsToInt :: [Int] -> Int
digitsToInt ds = sum $ zipWith (*) ds $ map (10^) $ reverse [0..length ds-1]

nat :: Parser Char Int
nat = fmap digitsToInt (many1 pDigit)

expr1' :: Parser Char Expr1
expr1' = ENat <$> nat
     <|> EAdd <$ lit '(' <*> expr1' <* lit '+' <*> expr1' <* lit ')'

example = runParser expr1' "(2 + (3 + 5))"

ws :: Parser Char [Char]
ws = many0 (lit ' ' <|> lit '\n' <|> lit '\t' <|> lit '\r')

ws' :: Parser Char [Char]
ws' = many0 (lit ' ')

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