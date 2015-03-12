module Auto.Chatbot.Backend.IRC where

import Auto.Chatbot.Core
import Control.Applicative
import Control.Arrow
import Control.Auto.Run
import Control.Concurrent
import Control.Exception
import Control.Monad            (void, forever, when)
import Data.Foldable            (forM_)
import Data.Text hiding         (length)
import Data.Text.Encoding
import Data.Text.Encoding.Error
import Data.Time
import Network.SimpleIRC
import qualified Data.Map       as M

withIrcConf :: IrcConfig -> Bool -> Int -> ChatBot IO -> IO ()
withIrcConf ircconf debug chronRate chatbot = do

    inputChan <- newChan :: IO (Chan (Either ChronEvent InMessage))
    startClock <- newEmptyMVar :: IO (MVar ())

    let events   = cEvents ircconf ++ [ Privmsg (onMessage inputChan)
                                      , Join (clockStart startClock) ]
        ircconf' = ircconf { cEvents = events }
    connectResult <- connect ircconf' True debug

    case connectResult of
      Left e       ->
        throw e

      Right server -> do
        _ <- forkIO . void $
          runOnChanM id (processOutput server) inputChan chatbot
        _ <- forkIO $ onClock startClock inputChan
        exitVar <- newEmptyMVar :: IO (MVar ())
        void . forkIO $ do
          forever (threadDelay 1000000000)
          putMVar exitVar ()
        takeMVar exitVar    -- TODO: Find some way to exit?

  where
    processOutput :: MIrc -> OutMessages -> IO Bool
    processOutput server (OutMessages outs) = do
      _ <- flip M.traverseWithKey outs $ \channel messages -> do
        let channel' = encodeUtf8 . pack $ channel
        forM_ messages $ \message -> do
          let message' = encodeUtf8 . pack $ message
          sendMsg server channel' message'
          when debug $
            putStrLn $ "To " ++ channel ++ ": " ++ message
      return True

    onMessage :: Chan (Either ChronEvent InMessage) -> EventFunc
    onMessage inputChan _ message = do
      let nickmsgsrc :: Either String (Nick, Channel, Message)
          nickmsgsrc = do
            nick  <- toEither "Message with no nick"   $ mNick message
            let msg = mMsg message
            src   <- toEither "Message with no source" $ mOrigin message

            nick' <- left (unicodeException "nick")    $ decodeUtf8' nick
            msg'  <- left (unicodeException "body")    $ decodeUtf8' msg
            src'  <- left (unicodeException "nick")    $ decodeUtf8' src

            return (unpack nick', unpack msg', unpack src')
      case nickmsgsrc of
        Left e -> putStrLn $ "Bad incoming message: " ++ e
                          ++ "; message skipped."
        Right (nick, msg, src) -> do
          time <- getCurrentTime
          writeChan inputChan . Right $ InMessage nick msg src time

    clockStart :: MVar () -> EventFunc
    clockStart startClock server _ = do
      goAhead <- isEmptyMVar startClock
      when goAhead $ do
        joinCoint <- length <$> getChannels server
        when (joinCoint >= length (cChannels ircconf)) $ do
          when debug (print "All channels joined!")
          void $ tryPutMVar startClock ()

    onClock :: MVar () -> Chan (Either ChronEvent InMessage) -> IO ()
    onClock startClock inputChan = forever $ do
      readMVar startClock
      threadDelay chronRate
      t <- getCurrentTime
      writeChan inputChan (Left t)

    toEither :: e -> Maybe a -> Either e a
    toEither e = maybe (Left e) Right
    unicodeException :: String -> UnicodeException -> String
    unicodeException d e = "Error on decoding message " ++ d
                        ++ ": " ++ show e

withIrc :: String -> Nick -> [Channel] -> Bool -> Int
        -> ChatBot IO -> IO ()
withIrc server nick chans = withIrcConf $
    (mkDefaultConfig server nick) { cChannels = chans }
