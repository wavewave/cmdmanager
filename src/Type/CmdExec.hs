{-# LANGUAGE GADTs #-}

module Type.CmdExec where

import Control.Monad.Coroutine
import Control.Monad.Coroutine.Object
--
import Type.CmdSet 
import Type.WorldState 


-- | 
data CmdOp i o where 
  CmdReady :: CmdOp CmdSet () 
  CmdInit  :: CmdOp () ()

-- |
cmdReady :: (Monad m) => CmdSet -> ClientObj CmdOp m () 
cmdReady cmd = request (Input CmdReady cmd) >> return ()


-- | 
cmdInit :: (Monad m) => ClientObj CmdOp m ()
cmdInit = request (Input CmdInit ()) >> return ()
