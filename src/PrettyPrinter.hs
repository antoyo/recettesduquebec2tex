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
Module      : PrettyPrinter
Description : Pretty printer for HaTeX documents.
Copyright   : © Antoni Boucher, 2014
License     : GPL-3
Maintener   : bouanto@zoho.com
Stability   : experimental
Portability : POSIX

This module provides a pretty printer for HaTeX documents which outputs a human-readable LaTeX document.
-}

module PrettyPrinter (prettyPrint) where

import Data.Monoid (mconcat)
import Text.LaTeX.Base.Syntax (LaTeX (TeXComm, TeXCommS, TeXComment, TeXEmpty, TeXEnv, TeXLineBreak, TeXRaw, TeXSeq), TeXArg (FixArg, OptArg))
import Text.PrettyPrint.Free (Pretty (pretty), (<>), Doc, backslash, braces, brackets, empty, indent, line, space, text)

-- |Pretty print a LaTeX document in a human-readable format.
prettyPrint :: LaTeX -> String
prettyPrint latex = show (latex2Doc latex False)

latex2Doc :: LaTeX -> Bool -> Doc ()
latex2Doc (TeXComm n []) _ = backslash <> text n <> text "{}" <> line <> line
latex2Doc (TeXComm "cookingpart" args) _ = backslash <> text "cookingpart" <> mconcat (map latexArg2Doc args) <> line
latex2Doc (TeXComm "item" args@[_]) _ = backslash <> text "item" <> mconcat (map latexArg2Doc args) <> space
latex2Doc (TeXComm n args) newLine = backslash <> text n <> mconcat (map latexArg2Doc args) <> line <> newLineOrEmpty newLine
latex2Doc (TeXCommS n) newLine = backslash <> text n <> newLineOrEmpty newLine
latex2Doc (TeXComment comment) _ = text "% " <> pretty comment <> line <> line
latex2Doc (TeXEnv environment _ latex) _ =
    backslash <> text "begin" <> braces (text environment) <> line
        <> indent 4 (latex2Doc latex False) <> line
        <> backslash <> text "end" <> braces (text environment) <> line
        <> newLineOrEmpty (environment /= "steps")
latex2Doc TeXEmpty _ = empty
latex2Doc (TeXLineBreak _ _) _ = text "\\\\" <> line
latex2Doc (TeXRaw rawText) newLine = pretty rawText <> newLineOrEmpty newLine
latex2Doc (TeXSeq latex1@(TeXCommS "item ") latex2@(TeXSeq _ _)) _ = latex2Doc latex1 False <> latex2Doc latex2 True
latex2Doc (TeXSeq latex1@(TeXCommS "item ") latex2) _ = latex2Doc latex1 False <> latex2Doc latex2 False
latex2Doc (TeXSeq latex1 latex2) newLine = latex2Doc latex1 newLine <> latex2Doc latex2 True

latexArg2Doc :: TeXArg -> Doc ()
latexArg2Doc (FixArg arg) = braces $ latex2Doc arg False
latexArg2Doc (OptArg arg) = brackets $ latex2Doc arg False

newLineOrEmpty :: Bool -> Doc ()
newLineOrEmpty newLine = if newLine then line else empty
