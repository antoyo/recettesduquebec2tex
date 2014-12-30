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
Module      : Utils.DOM
Description : Provides various util functions to fetch values in a DOM element.
Copyright   : © Antoni Boucher, 2014
License     : GPL-3
Maintener   : bouanto@zoho.com
Stability   : experimental
Portability : POSIX

This module fetches the recipe from program argument URL and output LaTeX code in one file.
-}

module Utils.DOM (getAlt, getAttribute, getMaybeIntValue, getSrc, getValues) where

import qualified Data.Map as Map (lookup)
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import qualified Data.Text as Text (unpack)
import qualified Data.Text.Lazy as LazyText (unpack)
import Text.XML (Element (elementAttributes), Node (NodeElement))
import Text.XML.Cursor (Cursor, node)
import Text.XML.Scraping (innerText)
import Text.XML.Selector.TH (queryT)
import Text.XML.Selector.Types (JQSelector)

-- |Get the alt attribute of the element.
getAlt :: [Cursor] -> String
getAlt element = fromMaybe "" $ getAttribute element "alt"

-- |Get an element attribute by name.
getAttribute :: [Cursor] -> String -> Maybe String
getAttribute [] _ = Nothing
getAttribute (c:_) attributeName = let (NodeElement nodeElement) = node c in
                                   case Map.lookup (fromString attributeName) (elementAttributes nodeElement) of
                                      Just value -> Just $ Text.unpack value
                                      Nothing -> Nothing

getMaybeIntValue :: [JQSelector] -> Cursor -> Maybe Int
getMaybeIntValue selector element = case queryT selector element of
    [] -> Nothing
    (c:_) -> Just $ read $ head (words $ LazyText.unpack $ innerText c)

-- |Get the src attribute of the element.
getSrc :: [Cursor] -> Maybe String
getSrc element = getAttribute element "src"

getValues :: [JQSelector] -> Cursor -> [String]
getValues selector element = map (LazyText.unpack . innerText) matches
    where matches = queryT selector element
