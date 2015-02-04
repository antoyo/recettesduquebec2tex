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

import Control.Monad (when)
import Data.Monoid (mempty)
import Data.String (fromString)
import Data.Text (Text)
import Text.LaTeX (LaTeXT_, comment, item, lnbk)
import Text.LaTeX.Base.Commands (raw)

import Cookbook (cookingPart, cookingTime, ingredients, marinateTime, portions, preparationTime, recipe, steps, totalTime)
import Utils.LaTeX (maybeLaTeX, showLaTeX)
import Utils.Recipe (ListItem (Category, Item), Recipe (Recipe, recipeCookingTime, recipeIngredients, recipeMarinateTime, recipeName, recipePortions, recipePreparationTime, recipeSteps, recipeType, recipeURL), RecipeTime (RecipeTime, recipeTimeHours, recipeTimeMinutes))

-- |Convert a ListItem to a LaTeX value.
listItemToLaTeX :: (Monad m) => ListItem -> Bool -> LaTeXT_ m
listItemToLaTeX (Category category) addLineBreak = do
    when addLineBreak
        lnbk
    item $ Just mempty
    cookingPart $ raw category
listItemToLaTeX (Item itemList) _ = do
    item Nothing
    raw itemList

-- |Convert a list of items to a LaTeX list of items.
listItemToLaTeXList :: Monad m => [ListItem] -> LaTeXT_ m
listItemToLaTeXList = listItemToLaTeXList' False
    where listItemToLaTeXList' _ [] = mempty
          listItemToLaTeXList' addLineBreak (i:is) = do
              listItemToLaTeX i addLineBreak
              listItemToLaTeXList' True is

-- |Convert the recipe to a '\recipe{}' LaTeX command.
recipeIndexToLaTeX :: Monad m => Text -> Recipe -> LaTeXT_ m
recipeIndexToLaTeX machineRecipeName (Recipe {recipeName, recipeType}) = recipe (showLaTeX recipeType) (raw machineRecipeName) (raw recipeName)

recipeTimeToLaTeX :: Monad m => (Maybe (LaTeXT_ m) -> LaTeXT_ m -> LaTeXT_ m) -> Maybe RecipeTime -> LaTeXT_ m
recipeTimeToLaTeX _ Nothing = mempty
recipeTimeToLaTeX command (Just (RecipeTime { recipeTimeHours, recipeTimeMinutes }))
    | recipeTimeHours == 0 = command Nothing (showLaTeX recipeTimeMinutes)
    | otherwise = command (Just $ showLaTeX recipeTimeHours) (showLaTeX recipeTimeMinutes)

-- |Convert a recipe into LaTeX.
recipeToLaTeX :: Monad m => Recipe -> LaTeXT_ m
recipeToLaTeX (Recipe {recipeURL, recipeCookingTime, recipeIngredients, recipeMarinateTime, recipePortions, recipePreparationTime, recipeSteps}) = do
    comment $ fromString $ "Source: " ++ recipeURL
    recipeTimeToLaTeX preparationTime recipePreparationTime
    recipeTimeToLaTeX marinateTime recipeMarinateTime
    recipeTimeToLaTeX cookingTime recipeCookingTime
    totalTime
    maybeLaTeX portions recipePortions
    ingredients $ listItemToLaTeXList recipeIngredients
    steps $ listItemToLaTeXList recipeSteps
