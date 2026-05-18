-- Code adapted from the lecture Functional Programming at the University of Freiburg
-- https://proglang.github.io/teaching/24ws/fp.html

module Parser where
import Control.Applicative
import Data.List (intercalate)
import Data.Char (isLower, isAlphaNum)

newtype Parser t r = Parser { runParser :: [t] -> [(r, [t])] }

-- recognizes the empty language
pempty :: Parser t r
pempty = Parser $ \_ -> []

-- recognizes the language with just the empty word
succeed :: r -> Parser t r
succeed r = Parser $ \ts -> [(r, ts)]

-- `satisfy p` recognizes the language { a | p a }
satisfy :: (t -> Bool) -> Parser t t
satisfy par = Parser $ \ts -> case ts of
  (t : ts') | par t -> [(t, ts')]
  _               -> []

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
pmap f (Parser par) = Parser $ \ts -> [ (f s, ts') | (s, ts') <- par ts ]

runParserEnd :: Parser t a -> [t] -> [a]
runParserEnd par ts = [ x | (x, ts') <- runParser par ts, null ts' ]

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
many0 par = pure [] <|> many1 par

many1 :: Parser t x -> Parser t [x]
many1 par = pure (:) <*> par <*> many0 par

poptional :: Parser t r -> Parser t (Maybe r)
poptional par = Just <$> par <|> pure Nothing

sepBy1 :: Parser t a -> Parser t b -> Parser t [a]
sepBy1 par sep = (:[]) <$> par 
           <|> (:) <$> par <* sep <*> sepBy0 par sep

sepBy0 :: Parser t a -> Parser t b -> Parser t [a]
sepBy0 par sep = pure [] <|> sepBy1 par sep

-- This function should take a Parser that functions as a separator, and a Parser that will always parse the given input.
-- The function runs the given parser until the separator parser parses, then stops and return everything parsed before the sep.
runUntilSep :: Parser t a -> Parser t b -> Parser t [a]
runUntilSep par sep = ([] <$ sep) 
                  <|> (:) <$> par <*> runUntilSep par sep

lits :: (Eq t) => [t] -> Parser t [t]
lits []     = pure []
lits (t:ts) = pure (:) <*> lit t <*> lits ts

string :: Parser Char String
string = many1 (satisfy isAlphaNum)

-- zero or more whitespace chars
ws :: Parser Char [Char]
ws = many0 (lit ' ' <|> lit '\n' <|> lit '\t' <|> lit '\r')

-- one or more whitespace chars
ws1 :: Parser Char [Char]
ws1 = many1 (lit ' ' <|> lit '\n' <|> lit '\t' <|> lit '\r')

-- zero or more non-new-line whitespace chars
ws' :: Parser Char [Char]
ws' = many0 (lit ' ' <|> lit '\t')


-- CODE FOR MATCH PARSER


-- A type for the entire structure that is be parsed
type Match = (DTypes, PMat, VVec)


-- A type for our data type definitions, e.g.: 
-- data Unit where
--    tt : Unit
type Type = String
type Constructor = String

type DType = (Type, [(Constructor, [Type])])
type DTypes = [DType]

-- A type for our pattern matrices
type PVec = [Pattern]
type PMat  = [PVec]

-- A structure for each pattern in the matrix, e.g.: "(nat x zero y)"
data Pattern = PVar String
            | PCon Constructor [Pattern]
            | POr Pattern Pattern
            deriving (Eq, Show)

-- A structure for the value vectors, e.g.: "OneOfThose Nat"
type VVec = [Type]

-- Parses any amount of data types and their constructor declarations into a structure
dtypes :: Parser Char DTypes
dtypes = many1 dtype

dtype :: Parser Char DType
dtype = 
    (,) <$> (lits "data" *> ws *> ttype) <* ws <* lits "where" <* ws 
        <*> constrDeclarations <* ws

-- Parses any amount of "f : a -> b -> c ..." type signatures into a list 
constrDeclarations :: Parser Char [(String, [Type])]
constrDeclarations = sepBy1 constrDeclaration (ws' *> lit '\n' *> ws') where
  constrDeclaration = (,) <$> constructor <* ws <* lit ':' <* ws <*> typeSignature

-- Parses "a -> b -> c ..." into a list of types
typeSignature :: Parser Char [Type] 
typeSignature = sepBy1 ttype (ws *> lits "->" <* ws)

-- Parses a matrix of patterns [[Pattern]]. We don't need to check if it's of size n*m, as rules that are not the right size won't match
-- Here the design decision is that single-length lowercase alphastrings are parsed as vars ("x"), while other strings are cons ("zero")
pmat :: Parser Char PMat
pmat = sepBy1 (many1 (p <* ws')) (many1 (lit '\n' <* ws))

pvec :: Parser Char PVec
pvec = many1 (p <* ws')

-- Manual recursion as string is not greedy enough somehow for the pattern
pString :: Parser Char String
pString = do
  c  <- satisfy isAlphaNum
  cs <- (pString <|> pure "")
  pure (c:cs)

pAtom :: Parser Char Pattern
pAtom = pOr <|> pName <|> pCon
  where
    pOr = lit '(' *> ws' *> p <* ws' <* lit ')'  -- case or-pattern with explicit parentheses
    pCon = PCon <$ lit '(' <* ws' <*> pString <* ws' <*> many0 (p <* ws') <* lit ')' -- case constructed pattern with one or more arity
    pName = do
      name <- pString
      pure $ case name of
        (x:[]) | isLower x -> PVar name  -- case constructed pattern with zero arity
        _                  -> PCon name []    -- case wildcard


-- Match each individual pattern in the pattern matrix
p :: Parser Char Pattern
p = pAtom >>= \left ->
        (ws' *> lit '|' *> ws' *> pAtom >>= \right -> 
            pure (POr left right)) -- case or-pattern with implicit parentheses
        <|> pure left

-- Parses a type
ttype :: Parser Char Type
ttype = string

constructor :: Parser Char Constructor
constructor = string

-- Parses a value vector, e.g.: "OneOfThose Nat", into a structure
vvec :: Parser Char VVec
vvec = sepBy1 ttype ws1

-- Parse the entire match structure
match :: Parser Char Match
match =  
  (,,)
    <$> (ws *> lits "=== data types ===" *> ws *> dtypes)
    <*> (ws *> lits "=== pattern matrix ===" *> ws *> pmat)
    <*> (ws *> lits "=== type ===" *> ws *> vvec)

match' :: Parser Char Match
match' = ws *> match <* ws


checkSectionHeaders :: String -> Either String ()
checkSectionHeaders s = 
  case runParserEnd sectionHeaders s of

    [] -> (Left $ "\n\nParse failure: Sections could not be detected. Are they properly separated with '=== section name ==='?\n\n")

    _:_  -> (Right ())


checkDTypes :: String -> Either String ()
checkDTypes s = 
  case runParserEnd sectionHeaders s of

    (dts, _, _):_ -> let dts' = splitOnData dts 
                     in findDTypesErrs dts'

    _ -> (Left "")

splitOnData :: String -> [String]
splitOnData s = tail $ sD [] s
  where 
    sD :: String -> String -> [String]
    sD saved ('d':'a':'t':'a':xs) = saved : (sD [] xs)
    sD saved [] = [saved]
    sD saved (x:xs) = sD (saved ++ [x]) xs


findDTypesErrs :: [String] -> Either String ()
findDTypesErrs (x:xs) = 
  case runParserEnd (ws *> dtype <* ws) ("data " ++ x) of
    [] -> (Left $ "\n\nParse error in:\n\ndata" ++ x ++ "\n\n")
    _ -> findDTypesErrs xs
findDTypesErrs [] = (Right ())


checkPMat :: String -> Either String ()
checkPMat s =
  case runParserEnd sectionHeaders s of

    (_, p, _):_ -> case runParserEnd (satisfy isAlphaNum) p of

      [] -> (Left $ "\n\nParse failure, non-allowed characters detected in:\n" ++ p ++ "\n\n")
      _ -> case runParserEnd pmat p of

        [] -> (Left $ "\n\nParse failure in:\n" ++ p ++ "\n\n")
        _ -> (Right ())
    
    _ -> (Left "")

checkType :: String -> Either String ()
checkType s =
  case runParserEnd sectionHeaders s of

    (_, _, t):_ -> case runParserEnd ttype t of
      [] -> (Left $ "\n\nParse failure in:\n" ++ t ++ "\n\n")
      _ -> (Right ())
    
    _ -> (Left "")
                          

sectionHeaders :: Parser Char (String, String, String)
sectionHeaders =
   (,,)
    <$> (ws *> lits "=== data types ===" *> ws *> anything <* ws)
    <*> (ws *> lits "=== pattern matrix ===" *> ws *> anything <* ws)
    <*> (ws *> lits "=== type ===" *> ws *> anything <* ws)

anything :: Parser Char String
anything = (many1 $ (satisfy (\x->True) <* ws))

-- Displaying and testing functions

-- pretty[...] is for displaying the parsed data type for debugging and testing

prettyMatch :: Match -> String
prettyMatch (x, y, z) = "\n=== data types ===\n" ++ prettyDTypes x ++ "\n\n=== pattern matrix ===\n" ++ prettyPMat y ++ "\n\n=== type ===\n" ++ prettyVVec z

prettyDTypes :: DTypes -> String
prettyDTypes xs = intercalate "\n\n" $ prettyDType <$> xs

prettyDType :: DType -> String
prettyDType xs = first xs ++ "\n  " ++ intercalate "\n  " (map prettyConstrDec (second xs))
  where first (x, _) = x
        second (_, x) = x

prettyConstrDec :: (Constructor, [Type]) -> String
prettyConstrDec xs = first xs ++ " : " ++ intercalate " -> " (map prettyType (second xs))
  where first (x, _) = x
        second (_, x) = x

prettyPVec :: PVec -> String
prettyPVec xs = intercalate " " (fmap prettyP xs)

prettyPMat :: PMat -> String
prettyPMat xs = (intercalate "\n") $ (intercalate " ") <$> ((fmap . fmap) prettyP xs)

prettyP :: Pattern -> String
prettyP (PVar x) = x
prettyP (POr x y) = prettyP x ++ " | " ++ prettyP y
prettyP (PCon x []) = x
prettyP (PCon x xs) = "(" ++ intercalate " " (x : (prettyP <$> xs)) ++ ")"

prettyType :: Type -> String
prettyType = id

prettyVVec :: VVec -> String
prettyVVec xs = intercalate " " $ prettyType <$> xs

debugPMat :: PMat -> String
debugPMat xs = "\n" ++ intercalate "\n" (fmap debugPVec xs) ++ "\n"

debugPVec :: PVec -> String
debugPVec xs = "[" ++ intercalate ", " (fmap debugP xs) ++ "]"

debugP :: Pattern -> String
debugP (PVar x) = "(PVar " ++ x ++ ")"
debugP (POr x y) = "(POr " ++ debugP x ++ " | " ++ debugP y ++ ")"
debugP (PCon x []) = "(PCon " ++ x ++ " [])"
debugP (PCon x xs) = "(PCon " ++ x ++ " [" ++ intercalate ", " (debugP <$> xs) ++ "])"
