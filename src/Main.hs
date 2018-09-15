{-# LANGUAGE OverloadedStrings #-}
module Main where

import RIO
import qualified RIO.FilePath as Path
import qualified RIO.Text as T
import qualified RIO.Directory as Dir
import qualified Data.Text.ICU as Regex
import System.Environment

getBlockComments :: T.Text -> [T.Text]
getBlockComments text = do
  let re = Regex.regex [] "\\{\\-([\\s\\S]*?)\\-\\}"
  catMaybes $ map (Regex.group 1) $ Regex.findAll re text

isPragma :: T.Text -> Bool
isPragma t = not (T.null t) && T.take 1 t == "#"

createIndex :: FilePath -> [FilePath] -> IO ()
createIndex base paths = do
  let path = Path.joinPath [base, "README.md"]
  Dir.createDirectoryIfMissing True base
  writeFileUtf8 path $ T.unlines $
    ("# " `T.append` T.pack base) : "" : map (("- " `T.append`) . T.pack) paths

foldOnDir :: FilePath -> (FilePath -> T.Text -> IO ()) -> (FilePath -> [FilePath] -> IO ()) -> IO ()
foldOnDir base onFile onDir = do
  paths <- Dir.listDirectory base
  onDir base paths

  forM_ paths $ \filename -> do
    let path = Path.joinPath [base, filename]

    isFile <- Dir.doesFileExist path
    if isFile then do
      when (Path.takeExtension path == ".hs") $ do
        onFile path =<< readFileUtf8 path
    else foldOnDir path onFile onDir

main = runSimpleApp $ liftIO $ do
  (path:_) <- getArgs
  current <- Dir.getCurrentDirectory

  let outDir = "docs"

  foldOnDir path
    (\filename content -> do
        let path = Path.replaceExtensions (Path.joinPath [outDir, filename]) ".md"
        Dir.createDirectoryIfMissing True (Path.dropFileName path)

        writeFileUtf8 path $ T.unlines $ filter (\c -> not (T.null c) && not (isPragma c)) $ getBlockComments content)
    (\base paths -> do
        createIndex (Path.joinPath [outDir, base]) paths)

