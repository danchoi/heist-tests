{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import             Blaze.ByteString.Builder
import             Data.ByteString.Char8 (ByteString)
import qualified   Data.ByteString.Char8 as B
import             Data.Text (Text)
import qualified   Data.Text as T
import qualified   Text.XmlHtml as X
import             Heist
import             Heist.Interpreted
import System.Environment
import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans (lift)
import Data.Maybe (fromMaybe)

import Database.HDBC
import Database.HDBC.PostgreSQL
import Control.Monad.Trans.Either (runEitherT, EitherT)

connPg = "dbname=netflix"

data Title = Title {
    tTitle :: Text
  , tSynopsis :: Text
  } deriving (Show)


-- deprecated
renderTitle :: Monad m => Title -> Splice m
renderTitle title = do
  runChildrenWithText [("tTitle", tTitle title), ("tSynopsis", tSynopsis title)]

titleSplice :: Monad m => Title -> Splice m
titleSplice title = do
    -- templates/titleSplice.tpl
    mTemplate <- callTemplateWithText (B.pack ("titleSplice")) [("tTitle", tTitle title), ("tSynopsis", tSynopsis title)]
    return mTemplate

renderTitles :: Monad m => [Title] -> Splice m
-- renderTitles = mapSplices renderTitle
renderTitles = mapSplices titleSplice

getRecentTitles :: IO [Title]
getRecentTitles  = do
      pg <- connectPostgreSQL connPg
      rows <- quickQuery' pg "select netflix_id, title, synopsis, year from titles_instant limit 20" [] 
      let titles = map convRowTitle rows
      mapM_ (putStrLn . show) titles 
      return titles
  where
      convRowTitle [_, title, synopsis, _]  = Title (fromSql title) (fromSql synopsis)

recentTitlesSplice :: Splice IO
recentTitlesSplice = liftIO getRecentTitles >>= renderTitles

mySplices = [ ("recentTitles", recentTitlesSplice) ]



-- load :: MonadIO n => FilePath -> [(Text, Splice n)] -> IO (HeistState n)
load baseDir splices = do
    tmap <- runEitherT  $ do
        templates <- loadTemplates baseDir
        let hc = HeistConfig mySplices  [] [] [] templates
        initHeist hc
    either (error . concat) return tmap

main = do
      ts <- load "templates" $ bindSplices mySplices 
      renderWithArgs [("test", T.pack "hello world")]  ts "index" >>= 
        B.putStr . maybe "Page not found" (toByteString . fst) 
     
      

