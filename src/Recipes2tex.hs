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

-- TODO: the ingredients and the steps can be in different categories.
-- TODO: find the recipe type in the HTML. The only place where it seems to be is in the Google Analytics JS code, for instance:
-- _gaq.push(['b._setCustomVar', 3, 'Cat3', "dessert", 3]);
-- TODO: try to convert impure functions to pure functions.
-- TODO: add unit test.
-- TODO: fix common spelling mistakes (oeuf -> œuf).

{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns, OverloadedStrings #-}

{-|
Module      : Recipes2tex
Description : Convert recipes from recettes.qc.ca in LaTeX.
Copyright   : © Antoni Boucher, 2014
License     : GPL-3
Maintener   : bouanto@zoho.com
Stability   : experimental
Portability : POSIX

This module fetches the recipe from program argument URL and output LaTeX code in one file.
-}

module Main (main) where

import Control.Applicative ((<$>))
import Control.Monad (forM_)
import qualified Data.ByteString.Lazy as ByteString (writeFile)
import Data.Maybe (fromMaybe)
import Network.HTTP.Conduit (simpleHttp)
import Network.URI (parseURI, uriAuthority, uriRegName)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.FilePath ((</>), (<.>))
import Text.HTML.DOM (parseLBS)
import Text.LaTeX (execLaTeXT)
import Text.XML.Cursor (Cursor, fromDocument)

import PrettyPrinter (prettyPrint)
import Converters.RecettesDuQuebec as RecettesDuQuebec (parseRecipe)
import Utils (machineName)
import Utils.Recipe (Recipe (Recipe, recipeImageURL, recipeName), RecipeType, readRecipeType)
import Utils.RecipeLaTeX (recipeIndexToLaTeX, recipeToLaTeX)

data RecipeFiles = RecipeFiles { recipeFile :: String
                               , recipeIndex :: String
                               , recipeMachineName :: String
                               }

urlToConverter :: [(String, Cursor -> String -> RecipeType -> Recipe)]
urlToConverter = [ ("www.recettes.qc.ca", RecettesDuQuebec.parseRecipe)
                 ]

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

convertRecipe :: String -> IO ()
convertRecipe url = do
    root <- (fromDocument . parseLBS) <$> simpleHttp url
    recipeType <- askRecipeType
    let host = getUrlHost url
        maybeParser = lookup host urlToConverter
        parser = fromMaybe (snd $ head urlToConverter) maybeParser
        recipe = parser root url recipeType
        recipeFiles = getRecipeFiles recipe
    doIO recipeType recipeFiles
    downloadImage (recipeImageURL recipe) recipeType recipeFiles

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

downloadImage :: Maybe String -> RecipeType -> RecipeFiles -> IO ()
downloadImage Nothing _ _ = return ()
downloadImage (Just imageURL) recipeType (RecipeFiles { recipeMachineName }) =
    simpleHttp imageURL >>=
        ByteString.writeFile (show recipeType </> recipeMachineName <.> "jpg")

getRecipeFiles :: Recipe -> RecipeFiles
getRecipeFiles recipe@(Recipe {recipeName}) =
    let recipeMachineName = machineName recipeName
        recipeIndex = execLaTeXT (recipeIndexToLaTeX recipeMachineName recipe) >>= prettyPrint
        recipeFile = execLaTeXT (recipeToLaTeX recipe) >>= prettyPrint
    in
    RecipeFiles { recipeFile, recipeIndex, recipeMachineName }

getUrlHost :: String -> String
getUrlHost url = fromMaybe "" $ do
    uri <- parseURI url
    authority <- uriAuthority uri
    return $ uriRegName authority

-- |Convert the recipe from the program argument URL and output it as LaTeX code.
main :: IO ()
main = do
    args <- getArgs
    if length args < 1 then
        putStrLn "Please provide at least on URL parameter."
    else
        forM_ args convertRecipe
