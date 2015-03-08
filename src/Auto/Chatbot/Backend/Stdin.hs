module Auto.Chatbot.Backend.Stdin where

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
stdinLoopChron nick channel chronRate chatbot0 = do
    inputVar <- newEmptyMVar :: IO (MVar (Either ChronEvent InMessage))
    exitVar  <- newEmptyMVar :: IO (MVar ())
    void . forkIO $ processInput inputVar chatbot0
    void . forkIO $ fromStdIn inputVar exitVar
    void . forkIO $ fromClock inputVar
    takeMVar exitVar
  where
    processInput :: MVar (Either ChronEvent InMessage) -> ChatBot IO
                 -> IO ()
    processInput inputVar chatbot1 = do
      input <- takeMVar inputVar
      (OutMessages out, chatbot2) <- stepAuto chatbot1 input
      mapM_ (mapM_ putStrLn) out
      processInput inputVar chatbot2
    fromStdIn :: MVar (Either ChronEvent InMessage) -> MVar ()
              -> IO ()
    fromStdIn inputVar exitVar = do
        input <- getInput nick channel
        case input of
          Just inp -> do
            putMVar inputVar inp
            fromStdIn inputVar exitVar
          Nothing ->
            putMVar exitVar ()
    fromClock :: MVar (Either ChronEvent InMessage) -> IO ()
    fromClock inputVar = forever $ do
        threadDelay chronRate
        t <- getCurrentTime
        putMVar inputVar (Left t)

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
