{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}

import Wuss

import GHC.Generics
import Control.Concurrent (forkIO)
import Control.Monad (forever, unless, void)
import qualified Data.Text as T (Text, pack, words, unwords)
-- import Data.Aeson (decode)
import Network.WebSockets (ClientApp, receiveData, sendClose)

data Message = Message {
      nick :: T.Text
    , dta :: T.Text
    } deriving (Generic, Show) 

main :: IO ()
main = runSecureClient "chat.strims.gg" 443 "/ws" ws

ws :: ClientApp ()
ws connection = do
  putStrLn "Connected!"

  void . forkIO . forever $ do
      message <- receiveData connection
      -- decode (Message . Data.Text.unwords $ tail $ Data.Text.words (message :: Data.Text.Text))
      print (message :: T.Text)

  let loop = do
          line <- getLine
          unless (null line) loop

  loop

  sendClose connection (T.pack "Closed")
  putStrLn "Disconnected"
