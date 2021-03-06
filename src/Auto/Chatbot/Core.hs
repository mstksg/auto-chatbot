{-# LANGUAGE Arrows #-}

module Auto.Chatbot.Core (
    InMessage(..)
  , OutMessages(..)
  , Message
  , Nick
  , Channel
  , ChronEvent
  , ChatBot
  , ChatBotMany
  , ChatBotRoom
  , ChatBotChron
  , ChatBotChronRoom
  , fromRoomToMany
  , fromRoom
  , fromMany
  , fromChron
  , fromChronRoom
  ) where

import Control.Auto
import Prelude hiding ((.), id)
import Data.Map.Strict (Map, unionsWith, unionWith, singleton)
import Data.Time

data InMessage = InMessage { inMessageNick   :: Nick
                           , inMessageBody   :: Message
                           , inMessageSource :: Channel
                           , inMessageTime   :: UTCTime
                           } deriving Show

type Message    = String
type Nick       = String
type Channel    = String
type ChronEvent = UTCTime

newtype OutMessages = OutMessages { outMessageMap :: Map Channel [Message]
                                  } deriving Show

instance Monoid OutMessages where
    mempty = OutMessages mempty
    mappend (OutMessages a) (OutMessages b) = OutMessages (unionWith (<>) a b)

type ChatBot m          = Auto m (Either ChronEvent InMessage) OutMessages

type ChatBotMany m      = Auto m InMessage                     OutMessages
type ChatBotRoom m      = Auto m InMessage                     (Blip [Message])
type ChatBotChron m     = Auto m UTCTime                       OutMessages
type ChatBotChronRoom m = Auto m (Either ChronEvent InMessage) (Blip [Message])

fromRoomToMany :: Monad m => ChatBotRoom m -> ChatBotMany m
fromRoomToMany cbr = proc im -> do
    outMsgs <- fromBlips [] . cbr -< im
    id -< OutMessages $ singleton (inMessageSource im) outMsgs

fromRoom :: Monad m => ChatBotRoom m -> ChatBot m
fromRoom = fromMany . fromRoomToMany

fromMany :: Monad m => ChatBotMany m -> ChatBot m
fromMany = fmap (either (const mempty) id) . right

fromChron :: Monad m => ChatBotChron m -> ChatBot m
fromChron = fmap (either id (const mempty)) . left

fromChronRoom :: Monad m => ChatBotChronRoom m -> ChatBot m
fromChronRoom cbcr = proc inp -> do
    outMsgs <- fromBlips [] . cbcr -< inp
    id -< case inp of
            Right im -> OutMessages $ singleton (inMessageSource im) outMsgs
            Left _   -> mempty
