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

module Memory (
    Memory,
    memoryNew,
    memoryWrite,
    memoryRead,
    memoryVariables,    
) where

type VarName = String
type VarValueAssoc = (VarName, Int)

data Memory = Memory [VarValueAssoc]
    
memoryNew :: Memory
memoryNew = (Memory [])

memoryWrite :: Memory -> VarName -> Int -> Memory
memoryWrite mem v x = case memoryRead mem v of
                        Nothing -> memoryAdd mem v x
                        Just _  -> memoryReplace mem v x

memoryRead :: Memory -> VarName -> Maybe Int
memoryRead (Memory ss) v = case (find (\(v', x) -> v' == v) ss) of
                             Nothing    -> Nothing
                             Just (v,x) -> Just x

memoryVariables :: Memory -> [VarName]
memoryVariables (Memory ss) = map fst ss


-- Internal functions

memoryReplace :: Memory -> VarName -> Int -> Memory
memoryReplace (Memory ss) v x = Memory (repl (\(v', x) -> v' == v) x ss)

memoryAdd :: Memory -> VarName -> Int -> Memory
memoryAdd (Memory ss) v x = Memory ((v,x):ss)

memoryDump :: Memory -> [VarValueAssoc]
memoryDump (Memory ss) = ss

find :: (VarValueAssoc -> Bool) -> [VarValueAssoc] -> Maybe VarValueAssoc
find p []     = Nothing
find p (x:xs) = if p x then
                  Just x
                else
                  find p xs

repl :: (VarValueAssoc -> Bool) -> Int -> [VarValueAssoc] -> [VarValueAssoc]
repl p y []         = error "Not found"
repl p y ((v,x):xs) = if p (v,x) then
                        (v, y) : xs
                      else
                        (v, x) : repl p y xs



instance Show Memory where
    show = show . memoryDump