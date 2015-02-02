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
Module      : Cookbook
Description : LaTeX Cookbook document class for HaTeX.
Copyright   : © Antoni Boucher, 2014
License     : GPL-3
Maintener   : bouanto@zoho.com
Stability   : experimental
Portability : POSIX

This module provides functions for the commands and environments of the cookbook LaTeX document class.
-}

module Cookbook (cookingPart, cookingTime, ingredients, marinateTime, portions, preparationTime, recipe, steps, totalTime) where

import Text.LaTeX.Base.Class (LaTeXC, comm0, comm1, liftL)
import Text.LaTeX.Base.Syntax (LaTeX (TeXEnv))

import Utils.LaTeX (comm1Opt1, comm3)

-- |Generate a 'cookingpart' command.
cookingPart :: LaTeXC l => l -> l
cookingPart = comm1 "cookingpart"

-- |Generate a 'cookingtime' command.
cookingTime :: LaTeXC l => Maybe l -> l -> l
cookingTime = comm1Opt1 "cookingtime"

-- |Generate an 'ingredients' environment.
ingredients :: LaTeXC l => l -> l
ingredients = liftL $ TeXEnv "ingredients" []

-- |Generate a 'marinatetime' command.
marinateTime :: LaTeXC l => Maybe l -> l -> l
marinateTime = comm1Opt1 "macerationtime"

-- |Generate a 'portions' command.
portions :: LaTeXC l => l -> l
portions = comm1 "portions"

-- |Generate a 'preparationtime' command.
preparationTime :: LaTeXC l => Maybe l -> l -> l
preparationTime = comm1Opt1 "preparationtime"

-- |Generate a 'recipe' command.
recipe :: LaTeXC l => l -> l -> l -> l
recipe = comm3 "recipe"

-- |Generate a 'steps' environment.
steps :: LaTeXC l => l -> l
steps = liftL $ TeXEnv "steps" []

-- |Generate a 'totaltime' command.
totalTime :: LaTeXC l => l
totalTime = comm0 "totaltime"
