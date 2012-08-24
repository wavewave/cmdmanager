{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, GADTs #-}

module CmdClient where

import Control.Monad
import Control.Monad.Error
import Control.Monad.State
--
import Control.Monad.Coroutine
import Control.Monad.Coroutine.Event
import Control.Monad.Coroutine.Object 
-- 
import CmdExec 
import Type.CmdExec
import Type.CmdExecEvent 
import Type.CmdSet
import Type.SubOp
import Type.WorldState
import Util

-- | command client actor 
cmdclient :: forall m m1. (MonadState (WorldAttrib m1) m) => 
             Int -> ServerObj SubOp m ()
cmdclient i = initServer go 
  where 
    --- server :: (MonadState (WorldAttrib m1) m) => ServerObj CmdOp (ServerT SubOp m) ()
    server = cmdexec i 
    -- client :: (MonadState (WorldAttrib m1) m) => 
    --           MethodInput SubOp 
    --        -> ClientObj CmdOp (ServerT SubOp m) ()
    client = cmdclientMain i
    go :: MethodInput SubOp -> ServerT SubOp m ()
    go req = do 
      Right serv 
        <- runErrorT (liftM fst (server <==> client req))
      return ()

-- | Main coroutine for command client 
cmdclientMain 
  :: forall m m1. (Monad m, MonadState (WorldAttrib m1) m) => 
     Int
  -> MethodInput SubOp  
  -> ClientObj CmdOp (ServerT SubOp m) ()
cmdclientMain i (Input GiveEventSub ev) = main ev 
  where main (Start i' (cmd1,cmd2)) 
          | i == i' = do cmdReady cmd1 
                         cmdInit 
                         nextev >>= waitSecond cmd2 
          | otherwise = ignore >>= main 
        main _ = ignore >>= main  
        waitSecond cmd2 (Finish i') 
          | i == i' = do cmdReady cmd2
                         cmdInit 
                         nextev >>= waitFinish 
          | otherwise = ignore >>= waitSecond cmd2 
        waitSecond cmd2 _ = ignore >>= waitSecond cmd2 
        waitFinish (Finish i) = return () 
        waitFinish _ = ignore >>= waitFinish
 



{-              cmdReady cmdset2
              cmdInit 
            r <- lift (request (Output GiveEventSub ())
          waitFinish r  -}

{-    let myserver = cmdexec idnum 


    --  let wlst' = myserver i : wlst 
    modify (worldState.nextID %~ (+1) )
    Right myserv1 <- runErrorT $ 
                       liftM fst (myserver <==> giveCmdSub (Ready cmdset1))
    req <- lift (request (Output GiveEvent ())
    Right myserv2 <- runErrorT $ 
                       liftM fst (myserver <==> giveEventSub (Ready cmdset2))
    
      modify (worldState.jobMap.at i .~ Just jst')

          Render -> do 
            modify (worldState.bufLog %~ 
                       (. (<> show i <> "th job status = " <> show jst <> "\n")))
            return (True,jst)
 
-}