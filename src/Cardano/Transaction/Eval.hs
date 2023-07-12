{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Transaction.Eval (evalEither, evalTxEither, evalRaw) where

import           Cardano.Transaction
import           Data.Text (Text)
import qualified Data.Text as T
import           Control.Exception
import           System.Exit
import qualified Data.ByteString.Lazy.Char8 as BSLC
import           System.FilePath.Posix
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Managed
import           System.IO.Temp
import           System.Process.Typed



-- | Safe version of eval, returns Either
evalEither :: EvalConfig -> Tx () -> IO (Either Text String)
evalEither cfg tx = tryText $ eval cfg tx

-- | Safe version of evalTx, returns Either
evalTxEither :: EvalConfig -> Tx () -> IO (Either Text String)
evalTxEither cfg tx = tryText $ evalTx cfg tx

evalTx :: EvalConfig -> Tx () -> IO String
evalTx EvalConfig {..} (Tx m) =
  let
    runCardanoCli args = do
      print args
      (exitCode, outStr) <- readProcessInterleaved . setSocketPath ecSocketPath . proc cardanoCliPath $ args
      case exitCode of
        ExitSuccess -> pure $ BSLC.unpack outStr
        ExitFailure _ -> liftIO . throwIO . EvalException "cardano-cli" args . BSLC.unpack $ outStr

  in flip with pure $ do
    tempDir <- maybe (managed (withSystemTempDirectory "tx-builder")) pure ecOutputDir
    txBuilder <- liftIO . execStateT (runReaderT m (ChainInfo ecTestnet ecSocketPath)) $ mempty
    bodyFlags <- transactionBuilderToBuildFlags tempDir ecTestnet ecProtocolParams ecUseRequiredSigners txBuilder

    liftIO $ do
      void $ runCardanoCli bodyFlags
      readFile $ tempDir </> "body.txt"

tryText :: forall a. IO a -> IO (Either Text a)
tryText action = do
  r <- Control.Exception.try action
  pure $ case (r :: Either SomeException a) of
    Left err -> Left $ T.pack . show $ err
    Right a -> Right a

eval :: EvalConfig -> Tx () -> IO String
eval EvalConfig {..} (Tx m) =
  let
    runCardanoCli args = do
      print args
      (exitCode, outStr) <- readProcessInterleaved . setSocketPath ecSocketPath . proc cardanoCliPath $ args
      case exitCode of
        ExitSuccess -> pure $ BSLC.unpack outStr
        ExitFailure _ -> liftIO . throwIO . EvalException "cardano-cli" args . BSLC.unpack $ outStr

  in flip with pure $ do
    tempDir <- maybe (managed (withSystemTempDirectory "tx-builder")) pure ecOutputDir
    txBuilder <- liftIO . execStateT (runReaderT m (ChainInfo ecTestnet ecSocketPath)) $ mempty
    bodyFlags <- transactionBuilderToBuildFlags tempDir ecTestnet ecProtocolParams ecUseRequiredSigners txBuilder

    liftIO $ do
      void $ runCardanoCli bodyFlags
      let
        bodyFile = toSigningBodyFlags tempDir
      -- get the txid
      txId <- fmap init $ runCardanoCli $ ["transaction", "txid"] <> bodyFile

      void . runCardanoCli . transactionBuilderToSignFlags tempDir ecTestnet $ txBuilder

      void . runCardanoCli . mconcat $
        [ [ "transaction", "submit" ]
        , toTestnetFlags ecTestnet
        , ["--tx-file", tempDir </> "signed-body.txt"]
        ]

      pure $ txId

evalRaw :: EvalConfig -> Integer -> Tx () -> IO String
evalRaw EvalConfig {..} fee (Tx m) =
  let
    runCardanoCli args = do
      print args
      (exitCode, outStr) <- readProcessInterleaved . setSocketPath ecSocketPath . proc cardanoCliPath $ args
      case exitCode of
        ExitSuccess -> pure $ BSLC.unpack outStr
        ExitFailure _ -> liftIO . throwIO . EvalException "cardano-cli" args . BSLC.unpack $ outStr

  in flip with pure $ do
    tempDir <- maybe (managed (withSystemTempDirectory "tx-builder")) pure ecOutputDir
    txBuilder <- liftIO . execStateT (runReaderT m (ChainInfo ecTestnet ecSocketPath)) $ mempty
    bodyFlags <- transactionBuilderToRawFlags tempDir ecProtocolParams ecUseRequiredSigners txBuilder fee

    liftIO $ do
      void $ runCardanoCli bodyFlags
      let
        bodyFile = toSigningBodyFlags tempDir
      -- get the txid
      txId <- fmap init $ runCardanoCli $ ["transaction", "txid"] <> bodyFile

      void . runCardanoCli . transactionBuilderToSignFlags tempDir ecTestnet $ txBuilder

      void . runCardanoCli . mconcat $
        [ [ "transaction", "submit" ]
        , toTestnetFlags ecTestnet
        , ["--tx-file", tempDir </> "signed-body.txt"]
        ]

      pure $ txId
