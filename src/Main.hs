{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Data.Maybe (fromMaybe)
import Control.Monad (forM_, forM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Except (throwE, ExceptT(..), runExceptT)
import Text.PrettyPrint.ANSI.Leijen hiding ((</>), (<>), (<$>))
import Data.Monoid ((<>))
import System.IO (hFlush, stdout)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath.Posix ((</>))
import Data.Text.Template
import Data.Text (Text)
import qualified System.Directory as D
import qualified System.FilePath.Posix as D
import qualified Data.Text.IO as T
import qualified Data.Text as T

usage :: IO a
usage = do
  putStrLn $ unlines ["usage: skeletons <skeleton>"
                     ,""
                     ,"  <skeleton> is a folder in $HOME/.config/closet which contain templates"
                     ]
  exitFailure

type Skel a = ExceptT Doc IO a

newtype SkelM a = SkelM { runSkel :: Skel a }
  deriving (Monad, Functor, Applicative)

instance MonadIO SkelM where
  liftIO = SkelM . lift

data SkelPrompt a = SkelVar !Text

liftWut :: IO a -> (a -> IO Text) -> IO Text
liftWut io fnIO = io >>= fnIO

specialVars :: FilePath -> Text -> IO (Maybe Text)
specialVars _  "$basename" = Just . T.pack . D.takeBaseName <$> D.getCurrentDirectory
specialVars fp "$filename" = return . Just . T.pack $ fp
specialVars _ _ = return Nothing

processVar :: FilePath -> Text -> IO Text
processVar file var = do
  matchedVar <- specialVars file var
  case matchedVar of
    Just val -> return val
    Nothing -> do
      putStr $ T.unpack var <> ": "
      hFlush stdout
      fmap T.pack getLine

process :: FilePath -> Text -> IO Text
process file contents = applyTemplate (processVar file) (parseTemplate contents)

info :: String -> Doc
info s = white (text s)

err :: Doc
err = red (text "error")

defaultSkelDir :: FilePath -> FilePath
defaultSkelDir homeDir = homeDir </> ".config/closet"

skeleton :: Maybe FilePath -> FilePath -> SkelM [(FilePath, Text)]
skeleton mcloset skel = do
  homeDir <- liftIO D.getHomeDirectory

  let closet = fromMaybe (defaultSkelDir homeDir) mcloset
      path :: String
      path = closet </> skel

  dirExists <- liftIO (D.doesDirectoryExist path)
  if not dirExists then
    SkelM $ throwE $ err <+> squotes (text path) <+> text "does not exist"
  else do
    files <- liftIO (D.getDirectoryContents path)
    let files' = filter (not . (`elem` [".", ".."])) files
    liftIO $ forM files' $ \file -> do
      contents <- liftIO (T.readFile $ path </> file)
      processed <- process file contents
      return (file, processed)

note :: Doc -> IO ()
note doc = putStrLn (displayS (renderPretty 1.0 80 doc) "")

runSkeleton :: FilePath -> IO (Either Doc [(FilePath, Text)])
runSkeleton x = runExceptT $ runSkel $ skeleton Nothing x

main :: IO ()
main = do
  args <- getArgs
  case args of
    -- TODO: configurable closet path
    (x:_) -> do
      result <- runSkeleton x
      case result of
        Left doc  -> note doc
        Right res -> forM_ res $ \(file, contents) -> do
          note (info "writing" <+> text file)
          T.writeFile file contents
    [] -> usage
