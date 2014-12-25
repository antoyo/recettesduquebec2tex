-- Copyright (C) 2014  Boucher, Antoni <bouanto@gmail.com>
-- 
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
-- 
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

module Utils (comm3, machineName) where

import Data.Char (toLower, toUpper)
import Data.Maybe (fromJust, isJust)
import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Syntax

data Capitalization = ToUpper | ToLower

comm3 :: LaTeXC l => String -> l -> l -> l -> l
comm3 str = liftL3 $ \l1 l2 l3 -> TeXComm str [FixArg l1, FixArg l2, FixArg l3]

letters :: [(Char, Char)]
letters = [ ('á', 'a')
          , ('à', 'a')
          , ('â', 'a')
          , ('ä', 'a')
          , ('é', 'e')
          , ('è', 'e')
          , ('ê', 'e')
          , ('ë', 'e')
          , ('í', 'i')
          , ('ì', 'i')
          , ('î', 'i')
          , ('ï', 'i')
          , ('ó', 'o')
          , ('ò', 'o')
          , ('ô', 'o')
          , ('ö', 'o')
          , ('ú', 'u')
          , ('ù', 'u')
          , ('û', 'u')
          , ('ü', 'u')
          ]

machineName :: String -> String
machineName = machineName' ToLower

machineName' :: Capitalization -> String -> String
machineName' _ "" = ""
machineName' _ ('-':name) = machineName' ToUpper name
machineName' _ (' ':name) = machineName' ToUpper name
machineName' capitalization (l:name)
    | isJust found = machineName' capitalization $ fromJust found:name
        where found = lookup (toLower l) letters
machineName' ToUpper (n:name) = toUpper n : machineName' ToLower name
machineName' ToLower (n:name) = toLower n : machineName' ToLower name
