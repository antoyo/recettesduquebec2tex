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
Module      : Utils.DOM
Description : Provides various util functions to fetch values in a DOM element.
Copyright   : © Antoni Boucher, 2014
License     : GPL-3
Maintener   : bouanto@zoho.com
Stability   : experimental
Portability : POSIX

This module fetches the recipe from program argument URL and output LaTeX code in one file.
-}

module Utils.DOM (getAlt, getAttribute, getClasses, getSrc, getText, getTexts) where

import qualified Data.Map as Map (lookup)
import Data.Maybe (listToMaybe)
import Data.Text as Text (Text)
import Data.Text.Lazy (toStrict)
import Text.XML (Element (elementAttributes), Name (Name), Node (NodeElement))
import Text.XML.Cursor (Cursor, node)
import Text.XML.Scraping (eclass, innerText)
import Text.XML.Selector.TH (queryT)
import Text.XML.Selector.Types (JQSelector)

-- |Get the alt attribute of the element.
getAlt :: [Cursor] -> Maybe Text
getAlt = getAttribute "alt"

-- |Get an element attribute by name.
getAttribute :: Text -> [Cursor] -> Maybe Text
getAttribute _ [] = Nothing
getAttribute attributeName (c:_) = let (NodeElement nodeElement) = node c in
                                   Map.lookup (Name attributeName Nothing Nothing) (elementAttributes nodeElement)

-- |Return the style classes of the element.
getClasses :: Cursor -> [Text]
getClasses = eclass . node

-- |Get the src attribute of the element.
getSrc :: [Cursor] -> Maybe Text
getSrc = getAttribute "src"

-- |Get the text of the first element of the selection.
getText :: [JQSelector] -> Cursor -> Maybe Text
getText selector element = listToMaybe $ getTexts selector element

-- |Get a list of texts from a selection of elements.
getTexts :: [JQSelector] -> Cursor -> [Text]
getTexts selector element = map (toStrict . innerText) matches
    where matches = queryT selector element
