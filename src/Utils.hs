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

{-|
Module      : Utils
Description : Util functions.
Copyright   : © Antoni Boucher, 2014
License     : GPL-3
Maintener   : bouanto@zoho.com
Stability   : experimental
Portability : POSIX

This module provides util functions.
-}

module Utils (comm3, machineName, trim) where

import Data.Char (isSpace, toLower, toUpper)
import Data.Maybe (fromMaybe)
import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Syntax

data Capitalization = ToUpper | ToLower

-- |A three parameter command generator using the name of the command.
comm3 :: LaTeXC l => String -> l -> l -> l -> l
comm3 str = liftL3 $ \l1 l2 l3 -> TeXComm str [FixArg l1, FixArg l2, FixArg l3]

commonWords :: [String]
commonWords = [ "a"
              , "au"
              , "aux"
              , "de"
              , "du"
              , "et"
              , "l"
              , "la"
              ]

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

-- |Generate a machine name of a string.
--
-- Every special character/word will be replaced or removed.
--
-- > machineName "l'autre nom de la machine" = "autreNomMachine"
machineName :: String -> String
machineName = machineName' ToLower . removeCommonWords . unaccentuate

machineName' :: Capitalization -> String -> String
machineName' _ "" = ""
machineName' _ (c:name)
    | c `elem` specialCharacters = machineName' ToUpper name
machineName' ToUpper (n:name) = toUpper n : machineName' ToLower name
machineName' ToLower (n:name) = toLower n : machineName' ToLower name

removeCommonWords :: String -> String
removeCommonWords string = unwords $ filter (`notElem` commonWords) $ splitWords string

specialCharacters :: String
specialCharacters = "- '()"

splitWords :: String -> [String]
splitWords "" = []
splitWords string = let (word, rest) = break (`elem` delimiters) string
                    in trim word : splitWords (tailSafe rest)
    where delimiters = "' "

tailSafe :: [a] -> [a]
tailSafe [] = []
tailSafe (_:xs) = xs

-- |Remove the whitespaces at each end of a string.
trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

unaccentuate :: String -> String
unaccentuate = map possiblyUnaccentuate
    where possiblyUnaccentuate l = fromMaybe l $ unaccentuateLetter l

unaccentuateLetter :: Char -> Maybe Char
unaccentuateLetter letter = lookup (toLower letter) letters
