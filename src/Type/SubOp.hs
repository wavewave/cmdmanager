{-# LANGUAGE GADTs #-}

module Type.SubOp where

-- from coroutine-object
import Control.Monad.Coroutine
import Control.Monad.Coroutine.Event
import Control.Monad.Coroutine.Object 
-- 
import Type.CmdExecEvent

-- | 
data SubOp i o where 
  GiveEventSub :: SubOp CmdExecEvent ()


-- |
giveEventSub :: (Monad m) => CmdExecEvent -> ClientObj SubOp m () 
giveEventSub ev = request (Input GiveEventSub ev) >> return ()

