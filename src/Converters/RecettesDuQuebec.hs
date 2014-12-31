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

{-# LANGUAGE NamedFieldPuns, QuasiQuotes #-}

{-|
Module      : Converters.RecettesDuQuebec
Description : Convert recipes from recettes.qc.ca in LaTeX.
Copyright   : © Antoni Boucher, 2014
License     : GPL-3
Maintener   : bouanto@zoho.com
Stability   : experimental
Portability : POSIX

This module converts the recipe from a DOM element and returns a LaTeX document.
-}

module Converters.RecettesDuQuebec (parseRecipe) where

import Control.Applicative ((<$>))
import Data.Maybe (maybeToList)
import Text.XML.Cursor (Cursor)
import Text.XML.Selector.TH (jq, queryT)

import Utils (dropFirstWord)
import Utils.DOM (getAlt, getFirstWordAsInt, getSrc, getText, getTexts)
import Utils.Recipe (Recipe (Recipe, recipeCookingTime, recipeImageURL, recipeIngredients, recipeMarinateTime, recipeName, recipePortions, recipePreparationTime, recipeSteps, recipeType, recipeURL), RecipeType)

getImageURL :: Cursor -> Maybe String
getImageURL element = do
    let image = queryT [jq| [itemprop="image"] |] element
    alt <- getAlt image
    if alt /= "Default Image"
        then getSrc image
        else Nothing

getCookingTime :: Cursor -> Maybe Int
getCookingTime = getFirstWordAsInt [jq| [itemprop="cookTime"] |]

getIngredients :: Cursor -> [String]
getIngredients = getTexts [jq| [itemprop="ingredients"] |]

getMarinateTime :: Cursor -> Maybe Int
getMarinateTime = getFirstWordAsInt [jq| dd.marinate-time |]

getPortions :: Cursor -> Maybe Int
getPortions = getFirstWordAsInt [jq| [itemprop="recipeYield"] |]

getPreparationTime :: Cursor -> Maybe Int
getPreparationTime = getFirstWordAsInt [jq| [itemprop="prepTime"] |]

getRecipeName :: Cursor -> String
getRecipeName element = concat $ maybeToList $ dropFirstWord <$> name
    where name = getText [jq| [itemprop="name"] |] element

getSteps :: Cursor -> [String]
getSteps = getTexts [jq| div.step-detail p |]

-- |Parse the recipe from the DOM element.
parseRecipe :: Cursor -> String -> RecipeType -> Recipe
parseRecipe element url recipeType =
    Recipe { recipeName = getRecipeName element
           , recipeURL = url
           , recipeCookingTime = getCookingTime element
           , recipeImageURL = getImageURL element
           , recipeIngredients = getIngredients element
           , recipeMarinateTime = getMarinateTime element
           , recipePortions = getPortions element
           , recipePreparationTime = getPreparationTime element
           , recipeSteps = getSteps element
           , recipeType
           }
