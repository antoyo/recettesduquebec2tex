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

{-# LANGUAGE FlexibleInstances, NamedFieldPuns, TypeSynonymInstances #-}

{-|
Module      : Utils.LaTeX
Description : Provides util functions to use with HaTeX values.
Copyright   : © Antoni Boucher, 2014
License     : GPL-3
Maintener   : bouanto@zoho.com
Stability   : experimental
Portability : POSIX

This module provides util functions to use with HaTeX values.
-}

module Utils.LaTeX (comm3, maybeLaTeX, nodeToString) where

import Data.Foldable (forM_)
import Data.String (fromString)
import qualified Data.Text.Lazy as LazyText (unpack)
import Text.LaTeX (LaTeXT_)
import Text.LaTeX.Base.Class (LaTeXC, liftL3)
import Text.LaTeX.Base.Syntax (LaTeX (TeXComm), TeXArg (FixArg))
import Text.XML.Cursor (Cursor)
import Text.XML.Scraping (innerText)

-- |A three parameter command generator using the name of the command.
comm3 :: LaTeXC l => String -> l -> l -> l -> l
comm3 str = liftL3 $ \l1 l2 l3 -> TeXComm str [FixArg l1, FixArg l2, FixArg l3]

-- |Convert a possibly null integer to LaTeX using the LaTeX command of the first argument.
maybeLaTeX :: Monad m => (LaTeXT_ m -> LaTeXT_ m) -> Maybe Int -> LaTeXT_ m
maybeLaTeX f maybeInt = forM_ maybeInt (f . fromString . show)

-- |Get the inner text of a node element as a string.
nodeToString :: Cursor -> String
nodeToString = LazyText.unpack . innerText
