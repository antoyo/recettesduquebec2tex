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

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeSynonymInstances #-}

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

module Utils.LaTeX (comm1Opt1, comm3, maybeLaTeX, nodeToText, showLaTeX, textToLaTeX) where

import Data.Foldable (forM_)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text.Lazy as LazyText (toStrict)
import Text.LaTeX (LaTeXT_)
import Text.LaTeX.Base.Class (LaTeXC, comm1, liftL2, liftL3)
import Text.LaTeX.Base.Commands (raw)
import Text.LaTeX.Base.Syntax (LaTeX (TeXComm), TeXArg (FixArg, OptArg), protectText)
import Text.XML.Cursor (Cursor)
import Text.XML.Scraping (innerText)

-- |A command generator with two parameters, the first one being optional, using the name of the command.
comm1Opt1 :: LaTeXC l => String -> Maybe l -> l -> l
comm1Opt1 str Nothing = comm1 str
comm1Opt1 str (Just value1) = (liftL2 $ \l1 l2 -> TeXComm str [OptArg l1, FixArg l2]) value1

-- |A three parameter command generator using the name of the command.
comm3 :: LaTeXC l => String -> l -> l -> l -> l
comm3 str = liftL3 $ \l1 l2 l3 -> TeXComm str [FixArg l1, FixArg l2, FixArg l3]

-- |Convert a possibly null showable value to LaTeX using the LaTeX command of the first argument.
maybeLaTeX :: (Monad m, Show a) => (LaTeXT_ m -> LaTeXT_ m) -> Maybe a -> LaTeXT_ m
maybeLaTeX f value = forM_ value (f . showLaTeX)

-- |Get the inner text of a node element as a string.
nodeToText :: Cursor -> Text
nodeToText = LazyText.toStrict . innerText

-- |Convert a showable value to a LaTeX value.
showLaTeX :: (LaTeXC l, Show s) => s -> l
showLaTeX = fromString . show

-- |Escape reserved characters from the text and convert it to a LaTeX value.
textToLaTeX :: LaTeXC l => Text -> l
textToLaTeX = raw . protectText
