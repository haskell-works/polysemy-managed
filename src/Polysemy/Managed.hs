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
  ( runManagedResource
  , runManaged
  , managedAsk
  , managedLocal
  ) where

import Control.Exception qualified as E
import Control.Monad.Trans.Resource (createInternalState, MonadResource, InternalState)
import Control.Monad.Trans.Resource qualified as MTR
import Control.Monad.Trans.Resource.Internal qualified as RI
import Polysemy (withLowerToIO, Embed, Sem, Member, embed, makeSem, interpretH, runT, raise, pureT)
import Polysemy.Reader (Reader, runReader)

data Managed m a where
  ManagedAsk :: Managed m InternalState
  ManagedLocal :: m a -> Managed m a

makeSem ''Managed

runManagedImpl :: forall a r. ()
  => Member (Embed IO) r
  => InternalState
  -> Sem (Managed ': r) a
  -> Sem r a
runManagedImpl state = interpretH $ \case
  ManagedAsk -> pureT state
  ManagedLocal m -> do
    mm <- runT m
    newState <- createInternalState
    resourceBracket2 newState $ raise $ runManagedImpl newState mm
{-# INLINE runManagedImpl #-}

runManaged :: forall a r. ()
  => Member (Embed IO) r
  => Sem (Managed ': r) a
  -> Sem r a
runManaged f = do
  state <- createInternalState
  resourceBracket2 state $ runManagedImpl state f
{-# INLINE runManaged #-}

resourceBracket2 :: forall a r. ()
  => Member (Embed IO) r
  => MTR.InternalState
  -> Sem r a
  -> Sem r a
resourceBracket2 istate f = do
  withLowerToIO $ \lower finish -> do
    E.mask $ \restore -> do
      res <- restore (lower f) `E.catch` \e -> do
        RI.stateCleanupChecked (Just e) istate
        E.throwIO e
      RI.stateCleanupChecked Nothing istate
      finish
      return res

runManagedResource :: ()
  => Member (Embed IO) r
  => Sem (Reader MTR.InternalState ': r) a
  -> Sem r a
runManagedResource f = do
  istate <- createInternalState
  runReader istate f

instance
  ( Member (Embed IO) r
  , Member Managed r
  ) => MonadResource (Sem r) where
  liftResourceT (RI.ResourceT r) = managedAsk >>= embed . r
