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

module LISEval (
    evalB,
    evalROp,
    evalProgram,
    evalBlock,
    evalCom,
    evalN,
) where

import Memory
import LISRepresentation

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