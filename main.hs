{-
    Author: Ary Pablo Batista <arypbatista@gmail.com>

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}
import Data.Char (ord)
import System.IO (readFile)


data Memory = Memory [(VarName, Int)]
    
    
instance Show Memory where
    show (Memory ss) = show ss

find p []     = Nothing
find p (x:xs) = if p x then
                    Just x
                 else
                    find p xs

repl :: ((VarName, Int) -> Bool) -> Int -> [(VarName, Int)] -> [(VarName, Int)]
repl p y []       = error "Not found"
repl p y ((v,x):xs) = if p (v,x) then
                    (v, y) : xs
                 else
                    (v, x) : repl p y xs


memoryRead  (Memory ss) v   = case (find (\(v', x) -> v' == v) ss) of
                                Nothing    -> Nothing
                                Just (v,x) -> Just x

memoryCreate (Memory ss) v x = Memory ((v,x):ss)

memoryReplace (Memory ss) v x = Memory (repl (\(v', x) -> v' == v) x ss)

memoryWrite :: Memory -> VarName -> Int -> Memory
memoryWrite mem v x = case memoryRead mem v of
                        Nothing -> memoryCreate mem v x
                        Just _  -> memoryReplace mem v x
memoryVariables (Memory ss) = map fst ss
memoryNew = (Memory [])
memoryDump (Memory ss) = ss

data Program = Program Block
type Block = [Comando]
data Comando = Skip
             | Assign VarName NExp
             | If BExp Block Block
             | While BExp Block
             
data BExp = BCte Bool
          | And BExp BExp
          | Cmp ROp NExp NExp
          | Not BExp
          | Or BExp BExp
    
data ROp = Equal
         | Greater
         | GreaterEqual
         | NotEqual
         | Lower
         | LowerEqual
         
type VarName = String

data NExp = Variable VarName
          | NCte Int
          | Add NExp NExp
          | Sub NExp NExp
          | Mul NExp NExp
          | Div NExp NExp
          | Mod NExp NExp
          

join :: String -> [String] -> String
join sep [] = ""
join sep (s:ss) = s ++ sep ++ join sep ss 

wrap s = "(" ++ s ++ ")"

wNJ = wrap . (join " ")
          
instance Show ROp where
    show Equal        = "Equal"
    show NotEqual     = "NotEqual"
    show Greater      = "Greater"
    show GreaterEqual = "GreaterEqual"
    show Lower        = "Lower"
    show LowerEqual   = "LowerEqual"
          
          
instance Show BExp where
    show (BCte b)          = wNJ ["BCte", show b]
    show (And b1 b2)       = wNJ ["And",  show b1,  show b2]
    show (Or b1 b2)        = wNJ ["Or",   show b1,  show b2]
    show (Cmp rop e1 e2)   = wNJ ["Cmp",  show rop, show e1, show e2]
    show (Not b)           = wNJ ["Not",  show b]
    
          
instance Show NExp where
    show (Variable name) = wNJ ["Variable", name]
    show (NCte n)        = wNJ ["NCte", show n]
    show (Add e1 e2)     = wNJ ["Add",  show e1, show e2]
    show (Sub e1 e2)     = wNJ ["Sub",  show e1, show e2]
    show (Mul e1 e2)     = wNJ ["Mul",  show e1, show e2]
    show (Div e1 e2)     = wNJ ["Div",  show e1, show e2]
    show (Mod e1 e2)     = wNJ ["Mod",  show e1, show e2]
    
          
instance Show Comando where
    show Skip = "Skip"
    show (Assign v ne) = wNJ ["Assign ", show v, show ne]
    show (If b tb fb)  = wNJ ["If ", show b, show tb, show fb]
    show (While b block) = wNJ ["While ", show b, show block]

instance Show Program where
    show (Program cs) = "Program \n" ++ show cs

------------------------
-- PARSER
------------------------

newtype Parser a = Parser (String -> [(a, String)])

class Monad m => MonadZeroOr m where
    zero  ::  m a
    (<|>)  ::  m a -> m a -> m a
    



{-
instance Monad Parser where
    return v         =  Parser (\inp -> [(v, inp)])
    Parser p >>= f   =  Parser (\inp -> concat [ parse (f v) out | (v, out) <- p inp])
-}

instance MonadZeroOr Parser where
    zero    =  Parser(\inp -> [])
    (Parser p) <|> (Parser q)  =  Parser(\inp -> (p inp ++ q inp))
    

if' p tf ff = if p then tf else ff


item = Parser (\inp -> case inp of 
                           []     -> []
                           (x:xs) -> [(x, xs)])


satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do
              c <- item
              if p c then return c else zero

symbol :: Char -> Parser Char
symbol x = satisfy (==x)
    
token :: String -> Parser String
token "" = do return ""
token (c:cs) = do
                 symbol c
                 token cs
                 return (c:cs)

skip = token "Skip" <@ const Skip
--command = skip

many p = (do
           x <- p
           xs <- many p
           return (x:xs)) <|> return []
           
--commands = many command

pack open p close = do
                      open
                      x <- p
                      close
                      return x
                                
block = pack (symbol '{') commands (symbol '}')


option p = p <|> return []

listOf1 p sep = many (do
                        _ <- sep
                        x <- p
                        return x)
                
listOf p sep = do
                 x  <- p
                 xs <- listOf1 p sep
                 return (x:xs)

{-
listOfOpt p sep = many ((do
                          x <- p
                          _ <- sep
                          return x) <|> p)
-}

{-
listOfOpt p sep = many (do
                               x <- p
                               _ <- (option sep)
                               return x)

-}



commands = do
             cs <- listOf command (option (token ";"))
             option (token ";")
             return cs

assign = do
           v <- varname
           _ <- token ":="
           e <- nExpr
           return (Assign v e)

-- Varname

between low high = (\x -> low <= x && x <= high)

lower = satisfy (between 'a' 'z')
upper = satisfy (between 'A' 'Z')
letter = lower <|> upper

digit = satisfy (between '0' '9')

alphanum = letter <|> digit

word = word' <|> return ""
       where
        word' = do
                  x  <- letter
                  xs <- word
                  return (x:xs)

wordPrefixed p = do
                    x  <- lower
                    xs <- word
                    return (x:xs)

alphanumWord = alphanumWord' <|> return ""
               where
                alphanumWord' = do
                          x  <- alphanum
                          xs <- alphanumWord
                          return (x:xs)
                          
anWordPrefixed pref = do
                       x  <- pref
                       xs <- alphanumWord
                       return (x:xs)


lowerId = anWordPrefixed lower

varname = lowerId

-- nExpr

many1 p = do
            x  <- p
            xs <- many p
            return (x:xs)

natural = many1 digit

optionDef p def = p <|> return def

sign = symbol '+' <|> symbol '-'

integer = do
            s <- optionDef sign '+'
            n <- natural
            return (NCte (toInt s n))
          where
            applySign :: Char -> Int -> Int
            applySign s = case s of 
                          '+' -> (*1)
                          '-' -> (* -1)
            toInt :: Char -> String -> Int
            toInt s n = applySign s (foldl (\a x -> a*10 + digitToInt x) 0 n)
            digitToInt x = ord x - ord '0'

parenthesized p = pack (symbol '(') p (symbol ')')

p <@ f = do { x <- p; return (f x); }

nExpr = term `chainl1` addop
term  = factor `chainr1` mulop
factor = integer <|> variable <|> parenthesized nExpr

variable = varname <@ Variable


op s f = token s <@ const f

addop = op "+" Add <|> op "-" Sub
mulop = op "*" Mul <|> op "/" Div <|> op "%" Mod

p `chainl1` op = do { x <- p;  rest x }
                 where rest x = (do
                                    f <- op
                                    y <- p
                                    (rest (f x y))) <|> return x
                                 
p `chainr1` op = do 
                    x <- p
                    (do 
                        f <- op
                        y <- p `chainr1` op
                        return (f x y) ) <|> return x
                                
inn :: Eq a => a -> [a] -> Bool
x `inn` xs = foldr (\y r -> r || (y == x)) False xs

removeWhites = dropWhile (\c -> c `inn` ['\n', '\t', ' '])

removeComments ('-':'-':inp) = dropWhile (\x -> x /= '\n') inp
removeComments inp = inp
                               
junkOut [] = []
junkOut inp = let inp' = (removeWhites . removeComments) inp
              in if inp' /= inp then
                    junkOut inp'
                 else
                    inp'
                

instance Monad Parser where
    return v         =  Parser (\inp -> [(v, junkOut inp)])
    Parser p >>= f   =  Parser (\inp -> concat [ parse (f v) (junkOut out) | (v, out) <- p ( junkOut inp)])
                
                
-- Introduce iff

bExpr = bTerm `chainl1` logop
bTerm = bValue <|> nCmp
nCmp  = do
          n1 <- nExpr
          r  <- relop
          n2 <- nExpr
          return (Cmp r n1 n2)
          
relop  =  op ">"  Greater      
      <|> op ">=" GreaterEqual 
      <|> op "<"  Lower
      <|> op "<=" LowerEqual
      <|> op "==" Equal 
      <|> op "/=" NotEqual
          
bTrue  = token "True"  <@ const (BCte True )
bFalse = token "False" <@ const (BCte False)
bValue = bTrue <|> bFalse

logop = op "&&" And <|> op "||" Or


iff = do
        token "if"
        bexp    <- parenthesized bExpr
        bt      <- block
        bf      <- option iffelse
        return (If bexp bt bf)
        
iffelse = do
            token "else"
            bf <- block
            return bf
        
while = do
            token "while"
            bexp <- parenthesized bExpr
            blk  <- block
            return (While bexp blk)
        
        

command  =  skip
        <|> assign
        <|> iff
        <|> while




lisParser = do 
              token "program"
              blk <- block
              return (Program blk)




{- NO
whitespace = satisfy (\x -> x `inn` [' ', '\n', '\t'])
junk :: Parser a
junk = many whitespace
-}



-- PARSE & EVAL


parse (Parser x) = x

bestParse p = (filter (\(v, inp') -> inp' == "")) . parse p

bestFirst p = head . bestParse p

parseLIS = bestParse lisParser

detParse = \inp -> case (parseLIS inp) of
                     []     -> error "Parsing error"
                     (x:xs) -> x


execute = evalProgram . fst . detParse


evalB (BCte b) mem = b
evalB (Cmp rop e1 e2) mem = evalROp rop (evalN e1 mem) (evalN e2 mem)
evalB (And e1 e2) mem = evalB e1 mem && evalB e2 mem

evalROp Equal        = (==)
evalROp NotEqual     = (/=)
evalROp Greater      = (>)
evalROp GreaterEqual = (>=)
evalROp Lower        = (<)
evalROp LowerEqual   = (<=)

evalProgram (Program block) = evalBlock block
evalBlock [] = \mem -> mem
evalBlock (c:cs) =
    \mem -> let mem' = evalCom c mem
            in evalProgram (Program cs) mem'
            
evalCom Skip = \mem -> mem
evalCom (Assign x ne) = \mem -> memoryWrite mem x (evalN ne mem)
evalCom (If be bt bf)
    = \mem -> if (evalB be mem) 
              then (evalBlock bt mem)
              else (evalBlock bf mem)
              
evalCom (While be p)
    = evalCom (If be (p ++ [While be p]) [Skip])

evalAndApply f e1 e2 mem = f (evalN e1 mem) (evalN e2 mem)

evalN (Variable x) = \mem ->
    case (memoryRead mem x) of
        Nothing -> error ("Variable '" ++ x ++ "' indefinida")
        Just v  -> v
evalN (NCte n) = \mem -> n
evalN (Add e1 e2) = evalAndApply (+) e1 e2
evalN (Sub e1 e2) = evalAndApply (-) e1 e2
evalN (Mul e1 e2) = evalAndApply (*) e1 e2
evalN (Div e1 e2) = evalAndApply div e1 e2
evalN (Mod e1 e2) = evalAndApply mod e1 e2

----





fromFile filename f = do  
        contents <- readFile filename
        f contents
        

executeFile = (flip fromFile) (print . (flip execute) memoryNew)
parseFile = (flip fromFile) (print . (parse lisParser))
