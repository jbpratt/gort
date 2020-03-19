{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import           Wuss

import           GHC.Generics
import           Control.Concurrent             ( forkIO )
import           Control.Monad                  ( forever
                                                , unless
                                                , void
                                                )
import           Data.Int                       ( Int64 )
import           Data.List                      ( (!!) )
import qualified Data.Text                     as T
                                                ( Text
                                                , pack
                                                , unpack
                                                , words
                                                , unwords
                                                , head
                                                )
import qualified Data.Text.Lazy                as L
                                                ( fromStrict )
import qualified Data.Text.Lazy.Encoding       as E
                                                ( encodeUtf8 )
import qualified Data.ByteString.Lazy.Char8    as C
                                                ( pack
                                                , unpack
                                                , ByteString
                                                )
import           Data.Aeson                     ( decode
                                                , FromJSON
                                                , parseJSON
                                                , withObject
                                                , (.:)
                                                )
import           Network.WebSockets             ( ClientApp
                                                , receiveData
                                                , sendClose
                                                )

data MessageType = MSG | PRIVMSG | JOIN | QUIT | VIEWERSTATE deriving (Show, Enum, Read)

data ChatMessage = ChatMessage {
      nick :: T.Text
    , content :: T.Text
    , features :: [T.Text]
    , timestamp :: Data.Int.Int64
    } deriving (Generic, Show, Eq)


instance FromJSON ChatMessage where
  parseJSON = withObject "ChatMessage" $ \v -> do
    nick      <- v .: "nick"
    features  <- v .: "features"
    content   <- v .: "data"
    timestamp <- v .: "timestamp"
    return
      (ChatMessage { nick      = nick
                   , features  = features
                   , content   = content
                   , timestamp = timestamp
                   }
      )


main :: IO ()
main = runSecureClient "chat.strims.gg" 443 "/ws" ws

ws :: ClientApp ()
ws connection = do
  putStrLn "Connected!"

  void . forkIO . forever $ do
    message <- receiveData connection
    let mt =
          read $ T.unpack $ head $ T.words (message :: T.Text) :: MessageType
    print $ parseMessage (message :: T.Text)

  let loop = do
        line <- getLine
        unless (null line) loop

  loop

  sendClose connection (T.pack "Closed")
  putStrLn "Disconnected"


parseMessage :: T.Text -> Maybe ChatMessage
parseMessage msg =
  decode $ E.encodeUtf8 $ L.fromStrict (parseMessageContent msg) :: Maybe
      ChatMessage

parseMessageContent :: T.Text -> T.Text
parseMessageContent = T.unwords . tail . T.words
