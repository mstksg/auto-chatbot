module Auto.Chatbot.Backend.Stdin (
    stdinLoop
  , stdinLoopChron
  ) where

import Auto.Chatbot.Core
import Control.Auto
import Control.Auto.Run
import Control.Concurrent
import Control.Exception
import Control.Monad      (void, forever)
import Data.Foldable      (mapM_)
import Data.Maybe
import Data.Time
import Prelude hiding     ((.), id, mapM_)
import System.IO

stdinLoop :: Nick -> Channel -> ChatBot IO -> IO ()
stdinLoop nick chan = void . run getFirstInp processOutp
  where
    getFirstInp :: IO (Either ChronEvent InMessage)
    getFirstInp = fromJust <$> getInput nick chan

    processOutp :: OutMessages -> IO (Maybe (Either ChronEvent InMessage))
    processOutp (OutMessages out) = do
      mapM_ (mapM_ putStrLn) out
      getInput nick chan

stdinLoopChron :: Nick -> Channel -> Int -> ChatBot IO -> IO ()
stdinLoopChron nick channel chronRate chatbot = do
    inputChan <- newChan      :: IO (Chan (Either ChronEvent InMessage))
    exitVar   <- newEmptyMVar :: IO (MVar ())
    void . forkIO $ runOnChanM id processOutput inputChan chatbot
    void . forkIO $ fromStdIn inputChan exitVar
    void . forkIO $ fromClock inputChan
    takeMVar exitVar
  where
    processOutput :: OutMessages -> IO Bool
    processOutput (OutMessages out) = True <$ mapM_ (mapM_ putStrLn) out
    fromStdIn inputChan exitVar = do
        input <- getInput nick channel
        case input of
          Just inp -> do
            writeChan inputChan inp
            fromStdIn inputChan exitVar
          Nothing ->
            putMVar exitVar ()
    fromClock :: Chan (Either ChronEvent InMessage) -> IO ()
    fromClock inputChan = forever $ do
        threadDelay chronRate
        t <- getCurrentTime
        writeChan inputChan (Left t)

getInput :: Nick -> Channel -> IO (Maybe (Either ChronEvent InMessage))
getInput nick chan = do
    putStr "> "
    hFlush stdout
    inp <- try getLine :: IO (Either SomeException String)
    case inp of
      Right inp' -> do
        t <- getCurrentTime
        return . Just . Right $ InMessage nick inp' chan t
      Left _ -> return Nothing
