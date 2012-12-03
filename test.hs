{-# LANGUAGE OverloadedStrings #-}
module Main where
import             Blaze.ByteString.Builder
import             Data.ByteString.Char8 (ByteString)
import qualified   Data.ByteString.Char8 as B
import             Data.Text (Text)
import qualified   Data.Text as T
import qualified   Text.XmlHtml as X
import             Text.Templating.Heist
import System.Environment

link :: Text -> Text -> X.Node
link target text = X.Element "a" [("href", target)] [X.TextNode text]

loginLink :: X.Node
loginLink = link "/login" "Login"

logoutLink :: Text -> X.Node
logoutLink user = link "/logout" (T.append "Logout " user)

loginLogoutSplice :: Splice IO
loginLogoutSplice = do
   -- let user = Just (name :: T.Text)
   -- let user = Nothing
   -- return [maybe loginLink logoutLink user]
   -- return [maybe loginLink logoutLink user]
   runChildrenWithText [("test", "testText")]

mySplices = [ ("loginLogout", loginLogoutSplice) ]

main = do
   [name] <- getArgs
   Right ts <- loadTemplates "templates" $
          bindSplices mySplices defaultHeistState
   -- let ts = either error id ets
   t <- renderWithArgs [("test2", T.pack name)]  ts "index" 
   B.putStr $ maybe "Page not found" (toByteString . fst) t
