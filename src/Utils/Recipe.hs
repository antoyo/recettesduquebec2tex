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
Module      : Utils.Recipe
Description : Provides util functions to use with recipes.
Copyright   : © Antoni Boucher, 2014
License     : GPL-3
Maintener   : bouanto@zoho.com
Stability   : experimental
Portability : POSIX

This module provides util functions to use with recipes.
-}

module Utils.Recipe (ListItem (Category, Item), Recipe (Recipe, recipeCookingTime, recipeImageURL, recipeIngredients, recipeMarinateTime, recipeName, recipePortions, recipePreparationTime, recipeSteps, recipeType, recipeURL), RecipeType (Breakfasts, Desserts, MainDishes), readRecipeType) where

-- |Either category list item or a normal list item.
data ListItem = Category String
              | Item String

-- |A recipe.
data Recipe = Recipe { recipeName :: String
                     , recipeURL :: String
                     , recipeCookingTime :: Maybe Int
                     , recipeImageURL :: Maybe String
                     , recipeIngredients :: [ListItem]
                     , recipeMarinateTime :: Maybe Int
                     , recipePortions :: Maybe Int
                     , recipePreparationTime :: Maybe Int
                     , recipeSteps :: [ListItem]
                     , recipeType :: RecipeType
                     }

-- |A recipe type.
data RecipeType = Breakfasts | Desserts | MainDishes

instance Show RecipeType where
    show Breakfasts = "breakfasts"
    show Desserts = "desserts"
    show MainDishes = "mainDishes"

-- |Convert a string into a recipe type or Nothing if the string is invalid.
readRecipeType :: String -> Maybe RecipeType
readRecipeType ('0':_) = Just Desserts
readRecipeType ('1':_) = Just MainDishes
readRecipeType ('2':_) = Just Breakfasts
readRecipeType "" = Just Desserts
readRecipeType _ = Nothing
