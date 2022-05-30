{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE BlockArguments       #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Polysemy.Managed
  ( Managed(..)

  , runManagedResource
  , runManaged
  , runManagedFinal

  , managedAsk
  , managedLocal
  ) where

import Control.Exception qualified as E
import Control.Monad.Trans.Resource (createInternalState, MonadResource, InternalState)
import Control.Monad.Trans.Resource qualified as MTR
import Control.Monad.Trans.Resource.Internal qualified as RI
import Polysemy (Embed, Final, Sem, Member)
import Polysemy qualified as P
import Polysemy.Final qualified as PF
import Polysemy.Reader (Reader, runReader)

data Managed m a where
  ManagedAsk :: Managed m InternalState
  ManagedLocal :: m a -> Managed m a

P.makeSem ''Managed

runManaged :: forall a r. ()
  => Member (Embed IO) r
  => Sem (Managed ': r) a
  -> Sem r a
runManaged f = do
  state <- createInternalState
  managedBracketImpl state $ runManagedImpl state f
{-# INLINE runManaged #-}

runManagedFinal :: forall a r. ()
  => Member (Final IO) r
  => Sem (Managed ': r) a
  -> Sem r a
runManagedFinal f = do
  state <- P.embedFinal @IO createInternalState
  managedBracketFinalImpl state $ runManagedFinalImpl state f
{-# INLINE runManagedFinal #-}

runManagedResource :: ()
  => Member (Embed IO) r
  => Sem (Reader MTR.InternalState ': r) a
  -> Sem r a
runManagedResource f = do
  istate <- createInternalState
  runReader istate f

--------------------------------------------------------------------------------
-- Internal

runManagedImpl :: forall a r. ()
  => Member (Embed IO) r
  => InternalState
  -> Sem (Managed ': r) a
  -> Sem r a
runManagedImpl state = P.interpretH $ \case
  ManagedAsk -> P.pureT state
  ManagedLocal m -> do
    mm <- P.runT m
    newState <- createInternalState
    managedBracketImpl newState $ P.raise $ runManagedImpl newState mm
{-# INLINE runManagedImpl #-}

runManagedFinalImpl :: forall a r. ()
  => Member (Final IO) r
  => InternalState
  -> Sem (Managed ': r) a
  -> Sem r a
runManagedFinalImpl state = P.interpretH $ \case
  ManagedAsk -> P.pureT state
  ManagedLocal m -> do
    mm <- P.runT m
    newState <- P.embedFinal @IO createInternalState
    managedBracketFinalImpl newState $ P.raise $ runManagedFinalImpl newState mm
{-# INLINE runManagedFinalImpl #-}

managedBracketImpl :: forall a r. ()
  => Member (Embed IO) r
  => MTR.InternalState
  -> Sem r a
  -> Sem r a
managedBracketImpl istate f = do
  P.withLowerToIO $ \lower finish -> do
    E.mask $ \restore -> do
      res <- restore (lower f) `E.catch` \e -> do
        RI.stateCleanupChecked (Just e) istate
        E.throwIO e
      RI.stateCleanupChecked Nothing istate
      finish
      return res

managedBracketFinalImpl :: forall a r. ()
  => Member (Final IO) r
  => MTR.InternalState
  -> Sem r a
  -> Sem r a
managedBracketFinalImpl istate f = PF.withStrategicToFinal @IO $ do
  f' <- PF.runS f
  pure $ E.mask $ \restore -> do
    res <- restore f' `E.catch` \e -> do
      RI.stateCleanupChecked  (Just e) istate
      E.throwIO e
    RI.stateCleanupChecked Nothing istate
    pure res

instance
  ( Member (Embed IO) r
  , Member Managed r
  ) => MonadResource (Sem r) where
  liftResourceT (RI.ResourceT r) = managedAsk >>= P.embed . r
