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

import System.IO (readFile)
import ParserLib
import Memory
import LISRepresentation
import LISEval
import LISParser


parseLIS = (bestFirst lisParser) . lisLexer

execute = evalProgram . fst . parseLIS

fromFile filename f = do  
        contents <- readFile filename
        f contents
    

executeFile = (flip fromFile) (print . (flip execute) memoryNew)
parseFile = (flip fromFile) (print . (parse lisParser) . lisLexer)
