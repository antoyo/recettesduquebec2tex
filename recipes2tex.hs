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

{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns, OverloadedStrings, QuasiQuotes #-}
{-# OPTIONS_GHC -Wall #-}

module Main (main) where

import Control.Applicative ((<$>))
import Data.Foldable (forM_)
import qualified Data.Text.Lazy as Text
import Network.HTTP.Conduit
import Text.HTML.DOM (parseLBS)
import Text.LaTeX
import Text.LaTeX.Base.Pretty (prettyLaTeX)
import Text.XML.Cursor (Cursor, fromDocument)
import Text.XML.Scraping (innerText)
import Text.XML.Selector.TH
import Text.XML.Selector.Types (JQSelector)

import Cookbook
import Utils (machineName)

data Recipe = Recipe { recipeName :: String
                     , recipeURL :: String
                     , recipeCookingTime :: Maybe Int
                     , recipeIngredients :: [String]
                     , recipeMarinateTime :: Maybe Int
                     , recipePortions :: Maybe Int
                     , recipePreparationTime :: Maybe Int
                     , recipeSteps :: [String]
                     }

getCookingTime :: Cursor -> Maybe Int
getCookingTime = getMaybeIntValue [jq| [itemprop="cookTime"] |]

getIngredients :: Cursor -> [String]
getIngredients = getValues [jq| [itemprop="ingredients"] |]

getMarinateTime :: Cursor -> Maybe Int
getMarinateTime = getMaybeIntValue [jq| dd.marinate-time |]

getMaybeIntValue :: [JQSelector] -> Cursor -> Maybe Int
getMaybeIntValue selector element = case queryT selector element of
    [] -> Nothing
    (c:_) -> Just $ read $ head (words $ Text.unpack $ innerText c)

getPortions :: Cursor -> Maybe Int
getPortions = getMaybeIntValue [jq| [itemprop="recipeYield"] |]

getPreparationTime :: Cursor -> Maybe Int
getPreparationTime = getMaybeIntValue [jq| [itemprop="prepTime"] |]

getRecipeName :: Cursor -> String
getRecipeName element = unwords $ drop 1 $ words name
    where name = head $ getValues [jq| [itemprop="name"] |] element

getSteps :: Cursor -> [String]
getSteps = getValues [jq| div.step-detail p |]

getValues :: [JQSelector] -> Cursor -> [String]
getValues selector element = map (Text.unpack . innerText) matches
    where matches = queryT selector element

main :: IO ()
main = do
    let url = "http://www.recettes.qc.ca/recette/biscuits-sables-vanille-165747"
    root <- (fromDocument . parseLBS) <$> simpleHttp url
    putStrLn $ parseRecipe root url

maybeLaTeX :: Monad m => (LaTeXT_ m -> LaTeXT_ m) -> Maybe Int -> LaTeXT_ m
maybeLaTeX f maybeInt = forM_ maybeInt (f . fromString . show)

parseRecipe :: Cursor -> String -> String
parseRecipe element url = do
    let onlineRecipe = Recipe { recipeName = getRecipeName element
                              , recipeURL = url
                              , recipeCookingTime = getCookingTime element
                              , recipeIngredients = getIngredients element
                              , recipeMarinateTime = getMarinateTime element
                              , recipePortions = getPortions element
                              , recipePreparationTime = getPreparationTime element
                              , recipeSteps = getSteps element
                              }
    execLaTeXT (recipeToLaTeX onlineRecipe) >>= prettyLaTeX

recipeToLaTeX :: Monad m => Recipe -> LaTeXT_ m
recipeToLaTeX (Recipe {recipeName, recipeURL, recipeCookingTime, recipeIngredients, recipeMarinateTime, recipePortions, recipePreparationTime, recipeSteps}) = do
    let machineRecipeName = machineName recipeName
    comment "In recipes.tex:"
    recipe "desserts" (fromString machineRecipeName) (fromString recipeName)
    comment $ fromString $ "In " ++ machineRecipeName ++ ".tex:"
    comment $ fromString $ "Source: " ++ recipeURL
    maybeLaTeX preparationTime recipePreparationTime
    maybeLaTeX marinateTime recipeMarinateTime
    maybeLaTeX cookingTime recipeCookingTime
    totalTime
    maybeLaTeX portions recipePortions
    ingredients $ stringsToLaTeX recipeIngredients
    steps $ stringsToLaTeX recipeSteps

stringsToLaTeX :: Monad m => [String] -> LaTeXT_ m
stringsToLaTeX [] = mempty
stringsToLaTeX (s:strings) = do
    item Nothing
    fromString s
    stringsToLaTeX strings
