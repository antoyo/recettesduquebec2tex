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
Module      : Utils.Recipe
Description : Provides util functions to use with recipes.
Copyright   : © Antoni Boucher, 2014
License     : GPL-3
Maintener   : bouanto@zoho.com
Stability   : experimental
Portability : POSIX

This module provides util functions to use with recipes.
-}

module Utils.Recipe (ListItem (Category, Item), Recipe (Recipe, recipeCookingTime, recipeImageURL, recipeIngredients, recipeMarinateTime, recipeName, recipePortions, recipePreparationTime, recipeSteps, recipeType, recipeURL), RecipeTime (RecipeTime, recipeTimeHours, recipeTimeMinutes), RecipeType (Breakfasts, Desserts, MainDishes), readRecipeType) where

import Data.Text (Text)

-- |Either category list item or a normal list item.
data ListItem = Category Text
              | Item Text

-- |A recipe.
data Recipe = Recipe { recipeName :: Text
                     , recipeURL :: String
                     , recipeCookingTime :: Maybe RecipeTime
                     , recipeImageURL :: Maybe Text
                     , recipeIngredients :: [ListItem]
                     , recipeMarinateTime :: Maybe RecipeTime
                     , recipePortions :: Maybe Int
                     , recipePreparationTime :: Maybe RecipeTime
                     , recipeSteps :: [ListItem]
                     , recipeType :: RecipeType
                     }

-- |A recipe time type (hours + minutes).
data RecipeTime = RecipeTime
    { recipeTimeHours :: Int
    , recipeTimeMinutes :: Int
    }

-- |A recipe type.
data RecipeType = Breakfasts | Desserts | MainDishes

instance Show RecipeType where
    show Breakfasts = "breakfasts"
    show Desserts = "desserts"
    show MainDishes = "mainDishes"

-- |Convert a string into a recipe type or Nothing if the string is invalid.
readRecipeType :: Text -> Maybe RecipeType
readRecipeType "dessert" = Just Desserts
readRecipeType "boisson" = Just Desserts
readRecipeType "plat-principal" = Just MainDishes
readRecipeType "souper" = Just MainDishes
readRecipeType "dejeuner-et-brunch" = Just Breakfasts
readRecipeType _ = Nothing
