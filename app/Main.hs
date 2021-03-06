{-  -*- coding:utf-8 -*-  -}
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Lib
import Dfm
import Checks.DfmNonAsciiInSql
import System.IO (stdout, stderr, hPutStrLn)
import System.Console.CmdArgs
import System.Environment (getProgName)
import Control.Monad (when, forM_)
import Text.Regex.TDFA
import Data.Either (isLeft, isRight, rights)
import System.Directory (doesFileExist, doesDirectoryExist)

import Paths_delphi_lint (version)
import Data.Version (showVersion)

programVersion :: String
programVersion =
  showVersion version

-- | Флаги коммандной строки
data Flags = Flags
           {
             source_path_1 :: FilePath,
             sources_paths :: [FilePath],
             ignore_path_pattern :: [String],
             ignore_binary_dfm :: Bool
           } deriving (Data, Typeable)

opts' :: IO Flags
opts' = getProgName >>= \programName -> return $
        Flags {
          source_path_1 =
            def
            &= argPos 0
            &= typ "DIR_OR_FILE_1",
          sources_paths =
            def
            &= args
            &= typ "DIR_OR_FILE_2 [DIR_OR_FILE_3 [...]]",

          ignore_path_pattern =
            def
            &= typ "REGEX"
            &= help "Regex pattern to ignore. It is applied to each file name with full path",
               
          ignore_binary_dfm =
            def
            &= help "DFM's in binary format are not checked by this utility but a warning is displayed for every DFM in binary format. This option suppress these warnings"
        }
        &= program programName
        &= summary ("Lint for Delphi version " ++ programVersion)
        &= details
             [
              "Checks Delphi sources for common errors."
             ,"Usage examples:"
             ,programName ++ " --ignore-path-pattern=\"[\\]enu[\\]\" --ignore-path-pattern=\"[\\]dc[\\]\" .\\Source"
             ,programName ++ " Unit1.dfm Unit2.dfm"
             ]


fileNameCompOpts = defaultCompOpt {
                     -- Case insensitive because Delphi is for windows and there file
                     -- names are case insensitive.
                     caseSensitive = False
                   }
fileNameExecOpts = defaultExecOpt
-- | Compile regex for matching file name
makeFileNameRegexM = makeRegexOptsM fileNameCompOpts fileNameExecOpts

checkOptions :: Flags -> IO ()
checkOptions opts = do

  let badRegexps = filter (isLeft . snd) $
                   map (\x -> (x,(makeFileNameRegexM x))) $
                       ignore_path_pattern opts
  when (not . null $ badRegexps) $
       let firstElem = head badRegexps
           errMsg = case snd firstElem of
                      Left x -> x
       in error $ "Bad regex \"" ++ (fst firstElem) ++ "\" in --ignore-path-pattern: " ++ errMsg

  forM_ (source_path_1 opts : sources_paths opts) $ \path -> do
    b <- (||) <$> doesFileExist path <*> doesDirectoryExist path
    when (not b) $ hPutStrLn stderr $ "File or directory \"" ++ path ++ "\" does not exists"

  return ()


main :: IO ()
main = do
  opts'' <- cmdArgs =<< opts'
  checkOptions opts''
  let
    ignorePath :: FilePath -> Bool
    ignorePath path =
      or $ map (`matchTest` path) compiledRegexps
        where compiledRegexps = rights $ map makeFileNameRegexM $ ignore_path_pattern opts''

  files <- fmap concat $ mapM (findFiles filterDfmFiles) $ source_path_1 opts'' : sources_paths opts''
  mapM (\f -> ((,) f) <$> checkDfmFile opts'' f) (filter (not . ignorePath) files) >>= printResults
  return ()


-- | Проверяет указанный dfm-файл и возвращает текст со описанием ошибок, которые
-- необходимо поправить. Если при обработке файла произошла ошибка, то её текст будет
checkDfmFile :: Flags -> FilePath -> IO (Either String (Maybe String))
checkDfmFile opts file = do
  parsedDfm <- parseDfmFile file (ParseDfmOpts{ignoreBinaryDfm = ignore_binary_dfm opts})
  --print parsedDfm
  return $ case parsedDfm of
             Left e -> Left $ show e
             Right (Just o) -> Right $ checkDfmForNonAsciiSymbolsInSql o
             _  -> Right Nothing


printResults :: [(FilePath, Either String (Maybe String))] -> IO ()
printResults results =
  mapM_ printOneResult results
  where
    printOneResult ((fileName, Left msg)) = hPutStrLn stderr $ "Error while parsing file \"" ++ fileName ++ "\": " ++ msg
    printOneResult ((fileName, Right (Just msg))) = hPutStrLn stdout $ "\n\nDFM-file \"" ++ fileName ++ "\" has issues:\n" ++ msg
    printOneResult _ = return ()
  
