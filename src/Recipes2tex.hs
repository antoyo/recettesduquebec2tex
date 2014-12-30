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

-- TODO: check if the image has the right extension and is shown in the generated PDF.
-- TODO: create a module for the downloader and another module for the converter.
-- TODO: divide the downloader module so that we can support other recipe websites later.
-- TODO: the ingredients and the steps can be in different categories.
-- TODO: try to convert impure functions to pure functions.
-- TODO: add unit test.
-- TODO: find the recipe type in the HTML. The only place where it seems to be is in the Google Analytics JS code, for instance:
-- _gaq.push(['b._setCustomVar', 3, 'Cat3', "dessert", 3]);
-- TODO: fix common spelling mistakes (oeuf -> œuf).

{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns, OverloadedStrings, QuasiQuotes #-}

{-|
Module      : Recipes2tex
Description : Convert recipes from recettes.qc.ca in LaTeX.
Copyright   : © Antoni Boucher, 2014
License     : GPL-3
Maintener   : bouanto@zoho.com
Stability   : experimental
Portability : POSIX

This module fetches the recipe from program argument URL and output LaTeX code in two files.
-}

module Main (main) where

import Control.Applicative ((<$>))
import Control.Monad (unless)
import qualified Data.ByteString.Lazy as ByteString
import Data.Foldable (forM_)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import Network.HTTP.Conduit
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.FilePath ((</>), (<.>))
import Text.HTML.DOM (parseLBS)
import Text.LaTeX
import Text.XML (Element (elementAttributes), Node (NodeElement))
import Text.XML.Cursor (Cursor, fromDocument, node)
import Text.XML.Scraping (innerText)
import Text.XML.Selector.TH
import Text.XML.Selector.Types (JQSelector)

import Cookbook
import PrettyPrinter
import Utils (machineName)

data RecipeFiles = RecipeFiles { recipeFile :: String
                               , recipeIndex :: String
                               , recipeMachineName :: String
                               }

data Recipe = Recipe { recipeName :: String
                     , recipeURL :: String
                     , recipeCookingTime :: Maybe Int
                     , recipeIngredients :: [String]
                     , recipeMarinateTime :: Maybe Int
                     , recipePortions :: Maybe Int
                     , recipePreparationTime :: Maybe Int
                     , recipeSteps :: [String]
                     , recipeType :: RecipeType
                     }

data RecipeType = Breakfasts | Desserts | MainDishes

instance Show RecipeType where
    show Breakfasts = "breakfasts"
    show Desserts = "desserts"
    show MainDishes = "mainDishes"

askRecipeType :: IO RecipeType
askRecipeType = do
    putStrLn "0: Desserts"
    putStrLn "1: Main Dishes"
    putStrLn "2: Breakfasts"
    putStrLn "Enter the recipe type [0]:"
    line <- getLine
    case readRecipeType line of
        Just recipeType -> return recipeType
        Nothing -> do
            putStrLn "Please enter a number between 0 and 2."
            askRecipeType

doIO :: RecipeType -> RecipeFiles -> IO ()
doIO recipeType recipeFiles = do
    let RecipeFiles { recipeFile, recipeIndex, recipeMachineName } = recipeFiles
        directoryName = show recipeType
        recipeFileName = directoryName </> recipeMachineName <.> "tex"
    putStrLn "Add this line in recipes.tex:"
    putStrLn recipeIndex
    createDirectoryIfMissing False directoryName
    writeFile recipeFileName recipeFile
    putStrLn $ "Recipe written to " ++ recipeFileName ++ "."

downloadImage :: Cursor -> RecipeType -> RecipeFiles -> IO ()
downloadImage element recipeType (RecipeFiles { recipeMachineName }) =
    let image = queryT [jq| [itemprop="image"] |] element
        alt = getAlt image
    in
    unless (alt == "Default Image") $
        case getSrc image of
            Nothing -> return ()
            Just imageURL ->
                simpleHttp imageURL >>=
                    ByteString.writeFile (show recipeType </> recipeMachineName <.> "png")

getAlt :: [Cursor] -> String
getAlt element = fromMaybe "" $ getAttribute element "alt"

getAttribute :: [Cursor] -> String-> Maybe String
getAttribute [] _ = Nothing
getAttribute (c:_) attributeName = let (NodeElement nodeElement) = node c in
                                   case Map.lookup (fromString attributeName) (elementAttributes nodeElement) of
                                      Just value -> Just $ Text.unpack value
                                      Nothing -> Nothing

getCookingTime :: Cursor -> Maybe Int
getCookingTime = getMaybeIntValue [jq| [itemprop="cookTime"] |]

getIngredients :: Cursor -> [String]
getIngredients = getValues [jq| [itemprop="ingredients"] |]

getMarinateTime :: Cursor -> Maybe Int
getMarinateTime = getMaybeIntValue [jq| dd.marinate-time |]

getMaybeIntValue :: [JQSelector] -> Cursor -> Maybe Int
getMaybeIntValue selector element = case queryT selector element of
    [] -> Nothing
    (c:_) -> Just $ read $ head (words $ LazyText.unpack $ innerText c)

getPortions :: Cursor -> Maybe Int
getPortions = getMaybeIntValue [jq| [itemprop="recipeYield"] |]

getPreparationTime :: Cursor -> Maybe Int
getPreparationTime = getMaybeIntValue [jq| [itemprop="prepTime"] |]

getRecipeName :: Cursor -> String
getRecipeName element = unwords $ drop 1 $ words name
    where name = head $ getValues [jq| [itemprop="name"] |] element

getSrc :: [Cursor] -> Maybe String
getSrc element = getAttribute element "src"

getSteps :: Cursor -> [String]
getSteps = getValues [jq| div.step-detail p |]

getValues :: [JQSelector] -> Cursor -> [String]
getValues selector element = map (LazyText.unpack . innerText) matches
    where matches = queryT selector element

-- |Convert the recipe from the program argument URL and output it as LaTeX code.
main :: IO ()
main = do
    args <- getArgs
    if length args < 1 then
        putStrLn "Please provide at least on URL parameter."
    else
        forM_ args $ \url -> do
            root <- (fromDocument . parseLBS) <$> simpleHttp url
            recipeType <- askRecipeType
            let recipeFiles = parseRecipe root url recipeType
            doIO recipeType recipeFiles
            downloadImage root recipeType recipeFiles

maybeLaTeX :: Monad m => (LaTeXT_ m -> LaTeXT_ m) -> Maybe Int -> LaTeXT_ m
maybeLaTeX f maybeInt = forM_ maybeInt (f . fromString . show)

parseRecipe :: Cursor -> String -> RecipeType -> RecipeFiles
parseRecipe element url recipeType = do
    let onlineRecipe = Recipe { recipeName = getRecipeName element
                              , recipeURL = url
                              , recipeCookingTime = getCookingTime element
                              , recipeIngredients = getIngredients element
                              , recipeMarinateTime = getMarinateTime element
                              , recipePortions = getPortions element
                              , recipePreparationTime = getPreparationTime element
                              , recipeSteps = getSteps element
                              , recipeType
                              }
        recipeMachineName = machineName $ recipeName onlineRecipe
        recipeIndex = execLaTeXT (recipeIndexToLaTeX recipeMachineName onlineRecipe) >>= prettyPrint
        recipeFile = execLaTeXT (recipeToLaTeX onlineRecipe) >>= prettyPrint
    RecipeFiles { recipeFile, recipeIndex, recipeMachineName }

readRecipeType :: String -> Maybe RecipeType
readRecipeType ('0':_) = Just Desserts
readRecipeType ('1':_) = Just MainDishes
readRecipeType ('2':_) = Just Breakfasts
readRecipeType _ = Nothing

recipeIndexToLaTeX :: Monad m => String -> Recipe -> LaTeXT_ m
recipeIndexToLaTeX machineRecipeName (Recipe {recipeName, recipeType}) = recipe (fromString $ show recipeType) (fromString machineRecipeName) (fromString recipeName)

recipeToLaTeX :: Monad m => Recipe -> LaTeXT_ m
recipeToLaTeX (Recipe {recipeURL, recipeCookingTime, recipeIngredients, recipeMarinateTime, recipePortions, recipePreparationTime, recipeSteps}) = do
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
