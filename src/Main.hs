{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.ICU as Regex

getBlockComments :: T.Text -> [T.Text]
getBlockComments text = do
  let re = Regex.regex [] "\\{\\-([\\s\\S]*?)\\-\\}"
  catMaybes $ map (Regex.group 1) $ Regex.findAll re text

isPragma :: T.Text -> Bool
isPragma t = not (T.null t) && T.head t == '#'

main = do
  (path:_) <- getArgs

  file <- TIO.readFile path
  mapM_ TIO.putStrLn $ filter (\c -> not (T.null c) && not (isPragma c)) $ getBlockComments file


