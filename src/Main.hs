{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Data.Maybe (fromMaybe)
import Data.IORef
import Control.Applicative
import Control.Monad (forM_, forM)
import Control.Monad.Trans.Class (lift, MonadTrans(..))
import Control.Monad.Trans.Cont
import Control.Monad.IO.Class (MonadIO(..))
import Control.Arrow (second)
import Control.Monad.Trans.Except (throwE, ExceptT(..), runExceptT)
import Text.PrettyPrint.ANSI.Leijen hiding ((</>), (<>), (<$>))
import Data.Monoid ((<>))
import Data.Map (insert, Map(..))
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
import qualified Data.Map as M

usage :: IO a
usage = do
  putStrLn $ unlines ["usage: skeletons <skeleton>"
                     ,""
                     ,"  <skeleton> is a folder in $HOME/.config/closet which contain templates"
                     ]
  exitFailure

type Skel a = ExceptT Doc IO a

newtype Default = Default { unDefault :: Text }
                deriving (Show)

newtype SkelM a = SkelM { runSkel :: Skel a }
  deriving (Monad, Functor, Applicative)

instance MonadIO SkelM where
  liftIO = SkelM . lift

lastSplit []     = Nothing
lastSplit [x]    = Nothing
lastSplit [x,y]  = Just (x, y)
lastSplit (x:xs) = lastSplit xs

parseDefault :: Text -> Maybe (Text, Default)
parseDefault = fmap (second Default) . lastSplit . map T.strip . T.splitOn "?"

specialVars :: FilePath -> Text -> IO (Maybe Text)
specialVars _  "$basename" = Just . T.pack . D.takeBaseName <$> D.getCurrentDirectory
specialVars fp "$filename" = return . Just . T.pack $ fp
specialVars _ _ = return Nothing

promptWithDefault :: Default -> Text -> IO Text
promptWithDefault (unDefault -> def) var = do
  putStr $ T.unpack var <> " (" <> T.unpack def <> "): "
  hFlush stdout
  input <- getLine
  return $ if null input
             then def
             else T.pack input

prompt :: Text -> IO Text
prompt var = do
  putStr $ T.unpack var <> ": "
  hFlush stdout
  fmap T.pack getLine

processVar :: (Monad (t IO), MonadTrans t) => FilePath -> Text -> t IO Text
processVar file var = do
  let mioDef = do (defVar, def) <- parseDefault var
                  return (defVar, def, fmap Default <$> specialVars file (unDefault def))
  matchedVar <- lift $ specialVars file var
  case (matchedVar, mioDef) of
    -- we got a matched return it
    (Just val, _) -> lift $ return val

    -- we got a default variable, check to see if it's a special var
    (_, Just (defVar, def, ioMatchDef)) -> do
      matchDef <- lift ioMatchDef
      case matchDef of
        -- we got a default variable that was matched with a special var
        Just matched -> lift $ promptWithDefault matched defVar

        -- we got a default variable that was NOT matched with a special var
        Nothing -> lift $ promptWithDefault def defVar

    -- We just got a regular var, prompt!
    (_, Nothing) -> lift $ prompt var

process :: IORef (Map Text Text) -> FilePath -> Text -> IO Text
process ref file contents = do
  let procs var = evalContT (mapContT (mapper var) (processVar file var))
  applyTemplate procs (parseTemplate contents)
  where
    mapper var mr = do
      vals <- readIORef ref
      case M.lookup var vals of
        Just cached -> do
          -- putStrLn $ T.unpack $ "cache hit on " <> var <> " val " <> cached
          return cached
        Nothing -> do r <- mr
                      -- cache the result
                      modifyIORef ref (M.insert var r)
                      -- putStrLn $ T.unpack $ "caching " <> T.pack (show var) <> " val " <> r
                      return r


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
    ref <- liftIO $ newIORef M.empty
    liftIO $ forM files' $ \file -> do
      contents <- liftIO (T.readFile $ path </> file)
      processed <- process ref file contents
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
