module Main where

import           Calamity
import           Calamity.Cache.InMemory
import           Calamity.Commands
import           Calamity.Commands.Context (useFullContext)
import           Calamity.Metrics.Noop
import           Control.Monad
import           Data.Default
import           Data.Maybe
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Di
import           DiPolysemy
import           Optics
import qualified Polysemy                  as P
import System.IO (readFile)

readToken :: FilePath -> IO T.Text
readToken path = do
    text <- readFile path  -- This reads the file and returns IO String
    return (T.pack text)   -- Convert String to Text and return it as IO Text



main :: IO ()
main = do
  -- Fetch my token
  tokenText <- readToken "application.secrets"
  
  -- set variable to token
  let botToken = BotToken tokenText
  
  --run Di
  Di.new $ \di ->
    void
    . P.runFinal
    . P.embedToFinal @IO
    . runDiToIO di
    . runCacheInMemory
    . runMetricsNoop
    . useFullContext
    . useConstantPrefix "!"
    . runBotIO (botToken) defaultIntents
    $ do
      info @Text "Setting up commands and handlers..."
      
      --judges you for editing messages
      react @'MessageUpdateEvt $ \(_oldMsg, newMsg, _user, _member) -> do
        void . tell @Text newMsg $ "Oh dear, I did see that! \r\n*smug giggles*"
        void . invoke $ CreateReaction _oldMsg _oldMsg (UnicodeEmoji "🍇")

      --set up some old fashioned commands here, the same way Beelzebot handles them
      --react @'MessageCreateEvt $ \(msg,  _usr, _member) -> do
      -- when ("!help" `T.isInfixOf` (msg ^. #content)) $
        --  void . tell @Text $ "Thank you for choosing Hedonismbot\r\nHedonism OS currently has no commands but soon there will be some!"

      --a silent command that adds grapes when grapes is said
      react @'MessageCreateEvt $ \(msg, _usr, _member) -> do
        when ("grapes" `T.isInfixOf` (msg ^. #content)) $
          void . invoke $ CreateReaction msg msg (UnicodeEmoji "🍇")