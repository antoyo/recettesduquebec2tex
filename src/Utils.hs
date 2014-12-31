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

module Utils (capitalize, dropFirstWord, machineName, maybeRead, tailSafe, trim) where

import Data.Char (isSpace, toLower, toUpper)
import Data.Maybe (fromMaybe, listToMaybe)

data Capitalization = ToUpper | ToLower

-- |Return the same string with the first letter in uppercase.
capitalize :: String -> String
capitalize "" = ""
capitalize (s:ss) = toUpper s : ss

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

-- |Drop the first word (and the space after it) of the string.
dropFirstWord :: String -> String
dropFirstWord = tail . dropWhile (/= ' ')

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

-- |Try to read a string and return Nothing in case of failure.
maybeRead :: (Read a) => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

removeCommonWords :: String -> String
removeCommonWords string = unwords $ filter (`notElem` commonWords) $ splitWords string

specialCharacters :: String
specialCharacters = "- '()"

splitWords :: String -> [String]
splitWords "" = []
splitWords string = let (word, rest) = break (`elem` delimiters) string
                    in trim word : splitWords (tailSafe rest)
    where delimiters = "' "

-- |Extract the elements after the head of a list or return the empty list if the argument is the empty list.
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
