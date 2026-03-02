{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

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
import           Polysemy.IO
import System.IO (readFile)
import           Network.HTTP.Simple
import Data.Aeson (object, (.=), Value (Object))
import qualified System.IO as IO

--this took me forever, but it works!
readToken :: FilePath -> IO T.Text
readToken path = do
    text <- readFile path
    return (T.pack text)


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
    . runBotIO botToken defaultIntents
    $ do
      info @Text "Setting up commands and handlers..."

      --judges you for editing messages
      react @'MessageUpdateEvt $ \(_oldMsg, newMsg, _user, _member) -> do
        void . tell @Text newMsg $ "Oh dear, I did see that! \r\n*smug giggles*"
        void . invoke $ CreateReaction _oldMsg _oldMsg (UnicodeEmoji "🍇")


      --a silent command that adds grapes when grapes is said
      react @'MessageCreateEvt $ \(msg, _usr, _member) -> do
          let _messageContent = msg ^. #content
          when ("grapes" `T.isInfixOf` _messageContent) $
              void . invoke $ CreateReaction msg msg (UnicodeEmoji "🍇")

      -- Respond to messages where the bot is tagged
      react @'MessageCreateEvt $ \(msg, _usr, _member) -> do
          let userMessage = msg ^. #content
          let botMention = "<@" <> T.pack (show $ msg ^. #author) <> ">"
          when (botMention `T.isInfixOf` userMessage) $ do
            let prompt = T.strip $ T.replace botMention "" userMessage
            P.embed $ putStrLn $ "Received mention with prompt: " ++ T.unpack prompt
            --ollama response
            response <- P.embed $ generateResponse prompt
            case response of
              Just reply -> do
                P.embed $ putStrLn $ "Sending reply: " ++ T.unpack reply
                void . tell @Text msg $ reply
              Nothing -> do
                P.embed $ putStrLn "No reply generated."
                --void . tell @Text msg $ "I'm sorry, I couldn't generate a response."

-- Function to generate a response using Ollama
generateResponse :: Text -> IO (Maybe Text)
generateResponse input = do
  putStrLn $ "Sending prompt to LLM: " ++ T.unpack input
  let request = setRequestBodyJSON (object ["prompt" .= input, "model" .= ("gemma3" :: Text), "stream" .= False])
                $ setRequestMethod "POST"
                $ setRequestPath "/api/generate"
                $ setRequestHost "localhost" -- Corrected host
                $ setRequestPort 11434
                $ setRequestSecure False -- Switched to HTTP
                $ defaultRequest
  response <- httpJSONEither request :: IO (Response (Either JSONException Value))
  case getResponseBody response of
    Right (Object body) -> do
      let output = T.pack $ show body
      putStrLn $ "Received response from LLM: " ++ T.unpack output
      return $ Just output
    Left err -> do
      putStrLn $ "Error from LLM: " ++ show err
      return Nothing
    _ -> do
      putStrLn "LLM returned an unexpected response format."
      return Nothing



