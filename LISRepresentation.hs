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

module LISRepresentation (
    Program (Program),
    
    Block,
    
    Command (
        Skip,
        Assign,
        If,
        While,
    ),
    
    BExp (
        BCte,
        And,
        Cmp,
        Not,
        Or,
    ),
    
    ROp (
        Equal,
        NotEqual,
        Greater,
        GreaterEqual,
        Lower,
        LowerEqual,
    ),
    
    VarName,
    
    NExp (
        Variable,
        NCte,
        Add,
        Sub,
        Mul,
        Div,
        Mod,
    ),
) where


-- LIS Representation

data Program = Program Block

type Block   = [Command]

data Command = Skip
             | Assign VarName NExp
             | If BExp Block Block
             | While BExp Block
             
data BExp    = BCte Bool
             | And BExp BExp
             | Cmp ROp NExp NExp
             | Not BExp
             | Or BExp BExp
    
data ROp     = Equal
             | Greater
             | GreaterEqual
             | NotEqual
             | Lower
             | LowerEqual
         
type VarName = String

data NExp    = Variable VarName
             | NCte Int
             | Add NExp NExp
             | Sub NExp NExp
             | Mul NExp NExp
             | Div NExp NExp
             | Mod NExp NExp
          

-- LIS Show
          
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
    
          
instance Show Command where
    show Skip = "Skip"
    show (Assign v ne) = wNJ ["Assign ", show v, show ne]
    show (If b tb fb)  = wNJ ["If ", show b, show tb, show fb]
    show (While b block) = wNJ ["While ", show b, show block]

instance Show Program where
    show (Program cs) = "Program \n" ++ show cs
    
    
-- Auxiliary functions
    
join :: String -> [String] -> String
join sep [] = ""
join sep (s:ss) = s ++ sep ++ join sep ss 

wrap s = "(" ++ s ++ ")"

wNJ = wrap . (join " ")