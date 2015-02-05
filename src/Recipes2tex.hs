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

-- TODO: add unit test.
-- TODO: parse time with days.
-- TODO: use Travis for Continuous Integration.
-- TODO: fix common spelling mistakes (oeuf -> œuf).
-- TODO: delete unused functions.
-- TODO: import explicitly every imported functions by writing their name in parentheses.

{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

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
import Data.Text (Text)
import qualified Data.Text as Text (unpack)
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
import Utils.Recipe (Recipe (Recipe, recipeImageURL, recipeName, recipeType), RecipeType)
import Utils.RecipeLaTeX (recipeIndexToLaTeX, recipeToLaTeX)

data RecipeFiles = RecipeFiles { recipeFile :: String
                               , recipeIndex :: String
                               , recipeMachineName :: Text
                               }

urlToConverter :: [(String, Cursor -> String -> Recipe)]
urlToConverter = [ ("www.recettes.qc.ca", RecettesDuQuebec.parseRecipe)
                 ]

convertRecipe :: String -> Cursor -> (Recipe, RecipeFiles)
convertRecipe url root = (recipe, recipeFiles)
        where host = getUrlHost url
              maybeParser = lookup host urlToConverter
              parser = fromMaybe (snd $ head urlToConverter) maybeParser
              recipe = parser root url
              recipeFiles = createRecipeFiles recipe

createRecipeFiles :: Recipe -> RecipeFiles
createRecipeFiles recipe@(Recipe {recipeName}) =
    let recipeMachineName = machineName recipeName
        recipeIndex = execLaTeXT (recipeIndexToLaTeX recipeMachineName recipe) >>= prettyPrint
        recipeFile = execLaTeXT (recipeToLaTeX recipe) >>= prettyPrint
    in
    RecipeFiles { recipeFile, recipeIndex, recipeMachineName }

downloadAndConvertRecipe :: String -> IO ()
downloadAndConvertRecipe url = do
    root <- (fromDocument . parseLBS) <$> simpleHttp url
    let (recipe, recipeFiles) = convertRecipe url root
    writeToStdoutAndFile (recipeType recipe) recipeFiles
    downloadImage (recipeImageURL recipe) (recipeType recipe) recipeFiles

downloadImage :: Maybe Text -> RecipeType -> RecipeFiles -> IO ()
downloadImage Nothing _ _ = return ()
downloadImage (Just imageURL) recipeType (RecipeFiles { recipeMachineName }) =
    simpleHttp (Text.unpack imageURL) >>=
        ByteString.writeFile (show recipeType </> Text.unpack recipeMachineName <.> "jpg")

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
        forM_ args downloadAndConvertRecipe

writeToStdoutAndFile :: RecipeType -> RecipeFiles -> IO ()
writeToStdoutAndFile recipeType recipeFiles = do
    let RecipeFiles { recipeFile, recipeIndex, recipeMachineName } = recipeFiles
        directoryName = show recipeType
        recipeFileName = directoryName </> Text.unpack recipeMachineName <.> "tex"
    putStrLn "Add this line in recipes.tex:"
    putStrLn recipeIndex
    createDirectoryIfMissing False directoryName
    writeFile recipeFileName recipeFile
    putStrLn $ "Recipe written to " ++ recipeFileName ++ "."
