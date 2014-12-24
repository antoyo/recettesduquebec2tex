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

module Cookbook (cookingTime, ingredients, marinateTime, portions, preparationTime, recipe, steps, totalTime) where

import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Syntax

cookingTime :: LaTeXC l => l -> l
cookingTime = comm1 "cookingtime"

ingredients :: LaTeXC l => l -> l
ingredients = liftL $ TeXEnv "ingredients" []

marinateTime :: LaTeXC l => l -> l
marinateTime = comm1 "macerationtime"

portions :: LaTeXC l => l -> l
portions = comm1 "portions"

preparationTime :: LaTeXC l => l -> l
preparationTime = comm1 "preparationtime"

recipe :: LaTeXC l => l -> l -> l -> l
recipe = liftL3 $ \category machineName name -> TeXComm "recipe" [FixArg category, FixArg machineName, FixArg name]

steps :: LaTeXC l => l -> l
steps = liftL $ TeXEnv "steps" []

totalTime :: LaTeXC l => l
totalTime = comm0 "totaltime"
