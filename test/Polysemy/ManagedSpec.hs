{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Polysemy.ManagedSpec (spec) where

import Control.Concurrent.MVar (MVar)
import Control.Concurrent.MVar qualified as IO
import Control.Monad (forM_, void)
import Control.Monad.Catch qualified as MC
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource qualified as MTR
import HaskellWorks.Hspec.Hedgehog qualified as H
import Hedgehog ((===), PropertyT)
import Polysemy (Final, Sem)
import Polysemy qualified as PY
import Polysemy.Embed (Embed)
import Polysemy.Managed qualified as PY
import Polysemy.Resource (Resource)
import Polysemy.Resource qualified as PY
import Test.Hspec (describe, it, Spec)

{- HLINT ignore "Redundant do" -}

data Error = Error deriving (Eq, Show, MC.Exception)

runMainEffects :: forall a. ()
  => Sem '[Embed IO , Resource , Final IO] a
  -> IO a
runMainEffects =
    PY.runFinal
  . PY.resourceToIOFinal
  . PY.embedToFinal

run :: MVar [PropertyT IO ()] -> PropertyT IO () -> IO ()
run mv p = do
  ps <- IO.takeMVar mv
  IO.putMVar mv (p:ps)

spec :: Spec
spec = describe "Polysemy.ManagedSpec" do
  describe "De-allocated allocated resource on return" do
    it "Embed" $ H.requireTest do
      mva <- liftIO IO.newEmptyMVar
      result1 <- liftIO $ runMainEffects $
        PY.runManaged do
          void $ MTR.allocate (return ()) $ const (IO.putMVar mva ())
          liftIO $ IO.tryTakeMVar mva

      result2 <- liftIO $ IO.tryTakeMVar mva

      result1 === Nothing
      result2 === Just ()

    it "Final" $ H.requireTest do
      mva <- liftIO IO.newEmptyMVar
      result1 <- liftIO $ runMainEffects $
        PY.runManagedFinal do
          void $ MTR.allocate (return ()) $ const (IO.putMVar mva ())
          liftIO $ IO.tryTakeMVar mva

      result2 <- liftIO $ IO.tryTakeMVar mva

      result1 === Nothing
      result2 === Just ()

  describe "De-allocated allocated resource on exception" do
    it "Embed" $ H.requireTest do
      mAssertions <- liftIO $ IO.newMVar []
      mva <- liftIO IO.newEmptyMVar
      liftIO $ MC.catch
        do runMainEffects do
            PY.runManaged do
              liftIO $ IO.tryReadMVar mva >>= \result -> run mAssertions $ result === Nothing
              void $ MTR.allocate (return ()) $ const (IO.putMVar mva ())
              liftIO $ IO.tryReadMVar mva >>= \result -> run mAssertions $ result === Nothing
              void $ liftIO $ MC.throwM Error
        do \Error -> do
            liftIO $ IO.tryReadMVar mva >>= \result -> run mAssertions $ result === Just ()
            return ()

      liftIO $ IO.tryReadMVar mva >>= \result -> run mAssertions $ result === Just ()

      assertions <- fmap reverse . liftIO $ IO.readMVar mAssertions

      forM_ assertions id

    it "Final" $ H.requireTest do
      mAssertions <- liftIO $ IO.newMVar []
      mva <- liftIO IO.newEmptyMVar
      liftIO $ MC.catch
        do runMainEffects do
            PY.runManagedFinal do
              liftIO $ IO.tryReadMVar mva >>= \result -> run mAssertions $ result === Nothing
              void $ MTR.allocate (return ()) $ const (IO.putMVar mva ())
              liftIO $ IO.tryReadMVar mva >>= \result -> run mAssertions $ result === Nothing
        do \Error -> do
            liftIO $ IO.tryReadMVar mva >>= \result -> run mAssertions $ result === Just ()
            return ()

      assertions <- fmap reverse . liftIO $ IO.readMVar mAssertions

      forM_ assertions id

  it "Has local capability (embed)" $ H.requireTest do
    mAssertions <- liftIO $ IO.newMVar []
    mva <- liftIO IO.newEmptyMVar
    mvb <- liftIO IO.newEmptyMVar
    liftIO $ runMainEffects do
      PY.runManaged do
        void $ MTR.allocate (return ()) $ const (IO.putMVar mva ())
        PY.managedLocal do
          liftIO $ IO.tryReadMVar mva >>= \result -> run mAssertions $ result === Nothing
          liftIO $ IO.tryReadMVar mvb >>= \result -> run mAssertions $ result === Nothing
          void $ MTR.allocate (return ()) $ const $ IO.putMVar mvb ()
          liftIO $ IO.tryReadMVar mva >>= \result -> run mAssertions $ result === Nothing
          liftIO $ IO.tryReadMVar mvb >>= \result -> run mAssertions $ result === Nothing
        liftIO $ IO.tryReadMVar mva >>= \result -> run mAssertions $ result === Nothing
        liftIO $ IO.tryReadMVar mvb >>= \result -> run mAssertions $ result === Just ()
      liftIO $ IO.tryReadMVar mva >>= \result -> run mAssertions $ result === Just ()
      liftIO $ IO.tryReadMVar mvb >>= \result -> run mAssertions $ result === Just ()

    liftIO $ IO.tryReadMVar mva >>= \result -> run mAssertions $ result === Just ()
    liftIO $ IO.tryReadMVar mvb >>= \result -> run mAssertions $ result === Just ()

    assertions <- fmap reverse . liftIO $ IO.readMVar mAssertions

    forM_ assertions id

  it "Has local capability (final)" $ H.requireTest do
    mAssertions <- liftIO $ IO.newMVar []
    mva <- liftIO IO.newEmptyMVar
    mvb <- liftIO IO.newEmptyMVar
    liftIO $ runMainEffects do
      PY.runManagedFinal do
        void $ MTR.allocate (return ()) $ const (IO.putMVar mva ())
        PY.managedLocal do
          liftIO $ IO.tryReadMVar mva >>= \result -> run mAssertions $ result === Nothing
          liftIO $ IO.tryReadMVar mvb >>= \result -> run mAssertions $ result === Nothing
          void $ MTR.allocate (return ()) $ const $ IO.putMVar mvb ()
          liftIO $ IO.tryReadMVar mva >>= \result -> run mAssertions $ result === Nothing
          liftIO $ IO.tryReadMVar mvb >>= \result -> run mAssertions $ result === Nothing
        liftIO $ IO.tryReadMVar mva >>= \result -> run mAssertions $ result === Nothing
        liftIO $ IO.tryReadMVar mvb >>= \result -> run mAssertions $ result === Just ()
      liftIO $ IO.tryReadMVar mva >>= \result -> run mAssertions $ result === Just ()
      liftIO $ IO.tryReadMVar mvb >>= \result -> run mAssertions $ result === Just ()

    liftIO $ IO.tryReadMVar mva >>= \result -> run mAssertions $ result === Just ()
    liftIO $ IO.tryReadMVar mvb >>= \result -> run mAssertions $ result === Just ()

    assertions <- fmap reverse . liftIO $ IO.readMVar mAssertions

    forM_ assertions id
