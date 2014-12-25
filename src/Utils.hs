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

{-# OPTIONS_GHC -Wall #-}

module Utils (machineName) where

import Data.Char (toLower, toUpper)

data Capitalization = ToUpper | ToLower | Same

machineName :: String -> String
machineName = machineName' ToLower

machineName' :: Capitalization -> String -> String
machineName' _ "" = ""
machineName' capitalization ('á':name) = machineName' capitalization ('a':name)
machineName' capitalization ('à':name) = machineName' capitalization ('a':name)
machineName' capitalization ('â':name) = machineName' capitalization ('a':name)
machineName' capitalization ('ä':name) = machineName' capitalization ('a':name)
machineName' capitalization ('Á':name) = machineName' capitalization ('A':name)
machineName' capitalization ('À':name) = machineName' capitalization ('A':name)
machineName' capitalization ('Â':name) = machineName' capitalization ('A':name)
machineName' capitalization ('Ä':name) = machineName' capitalization ('A':name)
machineName' capitalization ('é':name) = machineName' capitalization ('e':name)
machineName' capitalization ('è':name) = machineName' capitalization ('e':name)
machineName' capitalization ('ê':name) = machineName' capitalization ('e':name)
machineName' capitalization ('ë':name) = machineName' capitalization ('e':name)
machineName' capitalization ('É':name) = machineName' capitalization ('E':name)
machineName' capitalization ('È':name) = machineName' capitalization ('E':name)
machineName' capitalization ('Ê':name) = machineName' capitalization ('E':name)
machineName' capitalization ('Ë':name) = machineName' capitalization ('E':name)
machineName' capitalization ('í':name) = machineName' capitalization ('i':name)
machineName' capitalization ('ì':name) = machineName' capitalization ('i':name)
machineName' capitalization ('î':name) = machineName' capitalization ('i':name)
machineName' capitalization ('ï':name) = machineName' capitalization ('i':name)
machineName' capitalization ('Í':name) = machineName' capitalization ('I':name)
machineName' capitalization ('Ì':name) = machineName' capitalization ('I':name)
machineName' capitalization ('Î':name) = machineName' capitalization ('I':name)
machineName' capitalization ('Ï':name) = machineName' capitalization ('I':name)
machineName' capitalization ('ó':name) = machineName' capitalization ('o':name)
machineName' capitalization ('ò':name) = machineName' capitalization ('o':name)
machineName' capitalization ('ô':name) = machineName' capitalization ('o':name)
machineName' capitalization ('ö':name) = machineName' capitalization ('o':name)
machineName' capitalization ('Ó':name) = machineName' capitalization ('O':name)
machineName' capitalization ('Ò':name) = machineName' capitalization ('O':name)
machineName' capitalization ('Ô':name) = machineName' capitalization ('O':name)
machineName' capitalization ('Ö':name) = machineName' capitalization ('O':name)
machineName' capitalization ('ú':name) = machineName' capitalization ('u':name)
machineName' capitalization ('ù':name) = machineName' capitalization ('u':name)
machineName' capitalization ('û':name) = machineName' capitalization ('u':name)
machineName' capitalization ('ü':name) = machineName' capitalization ('u':name)
machineName' capitalization ('Ú':name) = machineName' capitalization ('U':name)
machineName' capitalization ('Ù':name) = machineName' capitalization ('U':name)
machineName' capitalization ('Û':name) = machineName' capitalization ('U':name)
machineName' capitalization ('Ü':name) = machineName' capitalization ('U':name)
machineName' _ ('-':name) = machineName' ToUpper name
machineName' _ (' ':name) = machineName' ToUpper name
machineName' ToUpper (n:name) = toUpper n : machineName' Same name
machineName' ToLower (n:name) = toLower n : machineName' Same name
machineName' Same (n:name) = n : machineName' Same name
