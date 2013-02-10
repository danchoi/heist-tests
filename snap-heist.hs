-- | This example illustrates usage of the Heist as well as the Snap library
{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
import Control.Applicative ((<$>), (<*>))

import Control.Exception (SomeException, try)
import Data.ByteString (ByteString)
import Control.Lens.TH
import Data.Text (Text)
import qualified Heist.Interpreted as I
import Snap.Http.Server (defaultConfig, httpServe)
import Snap.Snaplet
import Snap.Snaplet.Heist
import System.IO (hPutStrLn, stderr)
import Text.Digestive
import Text.Digestive.Heist
import Text.Digestive.Snap
import qualified Data.Text as T

--------------------------------------------------------------------------------
-- Application state                                                          --
--------------------------------------------------------------------------------

data App = App
    { _heist :: Snaplet (Heist App)
    }

makeLenses ''App

instance HasHeist App where
    heistLens = subSnaplet heist

type AppHandler = Handler App App

--------------------------------------------------------------------------------
-- Forms                                                                      --
--------------------------------------------------------------------------------

data Date = Date
    { dateDay   :: Int
    , dateMonth :: Int
    , dateYear  :: Int
    } deriving (Show)

dateForm :: Monad m => Form Text m Date
dateForm = check "Not a valid date" validDate $ Date
    <$> "day"   .: stringRead "Not a number" (Just 16)
    <*> "month" .: stringRead "Not a number" (Just 6)
    <*> "year"  .: stringRead "Not a number" (Just 1990)
  where
    validDate (Date day month _) =
        day   >= 1 && day   <= 31 &&
        month >= 1 && month <= 12

data Sex = Female | Male
    deriving (Eq, Show)

data User = User
    { userName      :: Text
    , userPassword  :: Text
    , userSex       :: Sex
    , userBirthdate :: Date
    } deriving (Show)

userForm :: Monad m => Form Text m User
userForm = check "name required" reqName $ User
    <$> "name"      .: text (Just "daniel")
    <*> "password"  .: text (Just "password")
    <*> "sex"       .: choice [(Female, "Female"), (Male, "Male")] Nothing
    <*> "birthdate" .: dateForm
  where reqName (User n p s b) = not . T.null $ n


--------------------------------------------------------------------------------
-- Form handler                                                               --
--------------------------------------------------------------------------------

form :: Handler App App ()
form = do
    (view, result) <- runForm "form" userForm
    case result of
        Just x  -> heistLocal (bindUser x) $ render "user"
        Nothing -> heistLocal (bindDigestiveSplices view) $ render "user-form"
  where
    bindUser user =
        I.bindSplices [("user", I.textSplice (T.pack $ show user))]

--------------------------------------------------------------------------------
-- Main code: glue everything together                                        --
--------------------------------------------------------------------------------

routes :: [(ByteString, Handler App App ())]
routes = [("/", form)]

app :: SnapletInit App App
app = makeSnaplet "app" "digestive-functors example application" Nothing $ do
    h <- nestSnaplet "heist" heist $ heistInit "templates"
    addRoutes routes
    return $ App h

main :: IO ()
main = do
    (msgs, site, cleanup) <- runSnaplet Nothing app
    hPutStrLn stderr $ T.unpack msgs
    _ <- try $ httpServe defaultConfig site :: IO (Either SomeException ())
    cleanup
