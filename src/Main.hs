{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.ICU as Regex
import qualified System.Directory as Dir
import qualified System.FilePath as Path
import System.Environment

getBlockComments :: T.Text -> [T.Text]
getBlockComments text = do
  let re = Regex.regex [] "\\{\\-([\\s\\S]*?)\\-\\}"
  catMaybes $ map (Regex.group 1) $ Regex.findAll re text

isPragma :: T.Text -> Bool
isPragma t = not (T.null t) && T.head t == '#'

foldOnDir :: FilePath -> (FilePath -> T.Text -> IO ()) -> IO ()
foldOnDir base process = do
  paths <- Dir.listDirectory base

  forM_ paths $ \filename -> do
    let path = Path.joinPath [base, filename]

    isFile <- Dir.doesFileExist path
    if isFile then process path =<< TIO.readFile path
    else foldOnDir path process

main = do
  (path:_) <- getArgs
  current <- Dir.getCurrentDirectory

  let outDir = "docs"

  foldOnDir path $ \filename content -> do
    let path = Path.joinPath [outDir, filename]
    Dir.createDirectoryIfMissing True (Path.dropFileName path)

    TIO.writeFile path $ T.unlines $ filter (\c -> not (T.null c) && not (isPragma c)) $ getBlockComments content

