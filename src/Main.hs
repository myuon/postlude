{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude (print)
import RIO
import qualified RIO.FilePath as Path
import qualified RIO.Text as T
import qualified RIO.Text.Partial as T
import qualified RIO.Text.Lazy as TL
import qualified RIO.Directory as Dir
import qualified Data.Text.ICU as Regex
import qualified Data.Text.Format as Format
import System.Environment

data Doc = Section T.Text | Note T.Text | Pragma T.Text | Paragraph T.Text deriving (Show)

parseDoc :: T.Text -> Doc
parseDoc text = (\(Just x) -> x) $ parseSection <|> parseNote <|> parsePragma <|> Just (Paragraph text) where
  genParser regex con = do
    let re = Regex.regex [] regex
    match <- Regex.find re text
    fmap con $ Regex.group 1 match

  parseSection = genParser "\\*{3}[\\s\\*]+([^\\*]*)[\\s\\*]+\\*{3}" Section
  parseNote = genParser "Note \\[(.*)\\]" Note
  parsePragma = genParser "#\\s([\\S]+)\\s#" Pragma

renderDoc :: Doc -> Maybe T.Text
renderDoc doc = case doc of
  Section t -> Just $ TL.toStrict $ Format.format "# {}" [t]
  Note t -> Just $ TL.toStrict $ Format.format "### Note: {}" [t]
  Paragraph t -> Just t
  Pragma t -> Nothing

getBlockComments :: T.Text -> [T.Text]
getBlockComments text = do
  let re = Regex.regex [] "\\{\\-([\\s\\S]*?)\\-\\}"
  catMaybes $ map (Regex.group 1) $ Regex.findAll re text

isPragma :: T.Text -> Bool
isPragma t = not (T.null t) && T.take 1 t == "#"

data FileType = File | Directory deriving (Show)

isFile :: FileType -> Bool
isFile File = True
isFile _ = False

createIndex :: FilePath -> FilePath -> [(FileType, FilePath)] -> IO ()
createIndex cur base paths = do
  Dir.createDirectoryIfMissing True (Path.joinPath [cur, base])
  writeFileUtf8 (Path.joinPath [cur, base, "README.md"]) $ T.unlines
    $ ("# " `T.append` T.pack base)
    : ""
    : map (\(file, filename) -> TL.toStrict $ Format.format "- [{}]({})" [filename, rewritePath file filename]) paths

  where
    rewritePath file filename = case file of
      File -> Path.joinPath [base, Path.replaceExtension filename ""]
      Directory -> Path.joinPath [base, filename] ++ "/"

getFileType :: FilePath -> FilePath -> IO (Maybe FileType)
getFileType base path = do
  isFile <- Dir.doesFileExist $ Path.joinPath [base, path]
  isDirectory <- Dir.doesDirectoryExist $ Path.joinPath [base, path]
  return $
    if not isFile && not isDirectory then Nothing
    else if isFile then Just File else Just Directory

foldOnDir :: FilePath -> (FilePath -> T.Text -> IO ()) -> (FilePath -> [(FileType, FilePath)] -> IO ()) -> IO ()
foldOnDir base onFile onDir = do
  listDir <- Dir.listDirectory base
  paths <- fmap catMaybes $ forM listDir $ \path -> do
    result <- getFileType base path
    return $ maybe Nothing (\ft -> Just (ft, path)) result
  onDir base paths

  forM_ paths $ \(file, path') -> do
    let path = Path.joinPath [base, path']

    case file of
      File -> do
        when (Path.takeExtension path == ".hs") $ readFileUtf8 path >>= onFile path
      Directory -> do
        foldOnDir path onFile onDir

main = runSimpleApp $ liftIO $ do
  (path:_) <- getArgs
  current <- Dir.getCurrentDirectory

  let outDir = "docs"
  foldOnDir path
    (\filename content -> do
        let path = Path.replaceExtensions (Path.joinPath [outDir, filename]) ".md"
        Dir.createDirectoryIfMissing True (Path.dropFileName path)

        writeFileUtf8 path
          $ T.intercalate "\n\n"
          $ catMaybes $ map (renderDoc . parseDoc) $ concat
          $ map (T.splitOn "\n\n")
          $ getBlockComments content)
    (\base paths -> do
        createIndex outDir base paths)

  writeFileUtf8 (Path.joinPath [outDir, "README.md"]) $ T.unlines
    [ "# ghc-docs-book"
    , ""
    , "- [ghc/compiler](ghc/compiler/)"
    ]

