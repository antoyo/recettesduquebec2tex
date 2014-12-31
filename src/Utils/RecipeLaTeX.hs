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

{-# LANGUAGE NamedFieldPuns #-}

{-|
Module      : Utils.RecipeLaTeX
Description : Util functions to convert recipes into LaTeX commands.
Copyright   : © Antoni Boucher, 2014
License     : GPL-3
Maintener   : bouanto@zoho.com
Stability   : experimental
Portability : POSIX

This module provides util functions to convert recipes into LaTeX commands.
-}

module Utils.RecipeLaTeX (recipeIndexToLaTeX, recipeToLaTeX) where

import Data.String (fromString)
import Text.LaTeX (LaTeXT_, comment)

import Cookbook (cookingTime, ingredients, marinateTime, portions, preparationTime, recipe, steps, totalTime)
import Utils.LaTeX (maybeLaTeX, stringsToLaTeXList)
import Utils.Recipe (Recipe (Recipe, recipeCookingTime, recipeIngredients, recipeMarinateTime, recipeName, recipePortions, recipePreparationTime, recipeSteps, recipeType, recipeURL))

-- |Convert the recipe to a '\recipe{}' LaTeX command.
recipeIndexToLaTeX :: Monad m => String -> Recipe -> LaTeXT_ m
recipeIndexToLaTeX machineRecipeName (Recipe {recipeName, recipeType}) = recipe (fromString $ show recipeType) (fromString machineRecipeName) (fromString recipeName)

-- |Convert a recipe into LaTeX.
recipeToLaTeX :: Monad m => Recipe -> LaTeXT_ m
recipeToLaTeX (Recipe {recipeURL, recipeCookingTime, recipeIngredients, recipeMarinateTime, recipePortions, recipePreparationTime, recipeSteps}) = do
    comment $ fromString $ "Source: " ++ recipeURL
    maybeLaTeX preparationTime recipePreparationTime
    maybeLaTeX marinateTime recipeMarinateTime
    maybeLaTeX cookingTime recipeCookingTime
    totalTime
    maybeLaTeX portions recipePortions
    ingredients $ stringsToLaTeXList recipeIngredients
    steps $ stringsToLaTeXList recipeSteps