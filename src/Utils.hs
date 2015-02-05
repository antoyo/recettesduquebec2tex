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

{-# LANGUAGE OverloadedStrings #-}

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

module Utils (capitalize, dropFirstWord, getNumber, getNumbers, machineName, tailSafe) where

import Data.Char (isAlphaNum, isDigit, toLower)
import Data.Maybe (fromMaybe)
import Data.Either.Combinators (fromRight', rightToMaybe)
import Data.Text (Text)
import qualified Data.Text as Text (append, concat, drop, dropWhile, filter, map, null, span, split, tail, take, toLower, toTitle, toUpper, unwords, words)
import Data.Text.Read (decimal)

-- |Return the same string with the first letter in uppercase.
capitalize :: Text -> Text
capitalize = changeFirstLetter Text.toUpper

changeFirstLetter :: (Text -> Text) -> Text -> Text
changeFirstLetter _ "" = ""
changeFirstLetter f text = f (Text.take 1 text) `Text.append` Text.drop 1 text

commonWords :: [Text]
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
dropFirstWord :: Text -> Text
dropFirstWord = Text.tail . Text.dropWhile (/= ' ')

-- |Get a number from a text.
getNumber :: Text -> Maybe Int
getNumber text = do
    number <- rightToMaybe $ decimal text
    return $ fst number

-- |Get all the numbers in a string. "1h 10 min" returns [1, 10].
getNumbers :: Text -> [Int]
getNumbers string
    | Text.null string || Text.null start = []
    | otherwise = fst (fromRight' (decimal number)) : getNumbers rest
    where start = Text.dropWhile (not . isDigit) string
          (number, rest) = Text.span isDigit start

isAlphaNumSpace :: Char -> Bool
isAlphaNumSpace character = isAlphaNum character || character == ' '

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

lowerFirstLetter :: Text -> Text
lowerFirstLetter = changeFirstLetter Text.toLower

-- |Generate a machine name of a string.
--
-- Every special character/word will be replaced or removed.
--
-- > machineName "l'autre nom de la machine" = "autreNomMachine"
machineName :: Text -> Text
machineName = lowerFirstLetter . Text.concat . Text.words . Text.toTitle . removeSpecialCharacters . removeCommonWords . unaccentuate

removeCommonWords :: Text -> Text
removeCommonWords string = Text.unwords $ filter (`notElem` commonWords) $ splitWords string

removeSpecialCharacters :: Text -> Text
removeSpecialCharacters = Text.filter isAlphaNumSpace

splitWords :: Text -> [Text]
splitWords = Text.split (`elem` "' ")

-- |Extract the elements after the head of a list or return the empty list if the argument is the empty list.
tailSafe :: [a] -> [a]
tailSafe [] = []
tailSafe (_:xs) = xs

unaccentuate :: Text -> Text
unaccentuate = Text.map possiblyUnaccentuate
    where possiblyUnaccentuate l = fromMaybe l $ unaccentuateLetter l

unaccentuateLetter :: Char -> Maybe Char
unaccentuateLetter letter = lookup (toLower letter) letters
