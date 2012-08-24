{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction, GADTs #-}

---------------------------
-- | some utility functions 
---------------------------

module Util where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans
-- 
import Control.Monad.Coroutine
import Control.Monad.Coroutine.Event
import Control.Monad.Coroutine.Object
import Control.Monad.Coroutine.Queue 
-- 
import Type.SubOp
import Type.WorldState 

-- | 
fireEvent :: (MonadState (WorldAttrib m1) m, Eventable e) => e -> m ()
fireEvent ev = modify (worldState.bufQueue %~ enqueue wrappedev)
  where wrappedev = Right (eventWrap ev)

-- | 
fireAction :: (MonadState (WorldAttrib m1) m) => ((Event -> IO ()) -> IO ()) 
           -> m ()
fireAction action = modify (worldState.bufQueue %~ enqueue wrappedev)
  where wrappedev = Left (ActionOrder action)


-- | 
initServer :: (MethodInput op ->  ServerT op m ()) -> ServerObj op m ()
initServer = ReaderT 

-- | 
ignore = do Input GiveEventSub r <- lift (request Ignore) 
            return r 

-- | 
nextev = do Input GiveEventSub r <- lift (request (Output GiveEventSub ()))
            return r 