{-# LANGUAGE ScopedTypeVariables, GADTs #-}

module World where

import Control.Applicative 
import Control.Lens 
import Control.Monad.Error 
import Control.Monad.State 
import Control.Monad.Trans
-- 
import Control.Monad.Coroutine
import Control.Monad.Coroutine.Logger
import Control.Monad.Coroutine.Object
import Control.Monad.Coroutine.Queue
import Control.Monad.Coroutine.World 
-- 
import Type.CmdExecEvent
import Type.SubOp
import Type.WorldState
import CmdClient 
import CmdExec 
import Util 

-- | 
world :: forall m. (MonadIO m) => ServerObj (WorldOp m) m () 
world = initServer staction 
  where 
    staction req = do runStateT (go req) initWorld
                      return ()
    go :: (MonadIO m) => MethodInput (WorldOp m) -> StateT (WorldAttrib (ServerT (WorldOp m) m)) (ServerT (WorldOp m) m) () 
    go (Input GiveEvent ev) = do
      case getCmdExecEvent ev of  
        Nothing -> return () 
        Just e -> do wlst <- (^. worldActor.workers ) <$> get 
                     case e of 
                       Init -> do 
                         i <- (^. worldState.nextID) <$> get 
                         let wlst' = cmdclient i : wlst 
                         modify (worldActor.workers .~ wlst')
                         modify (worldState.nextID %~ (+1) )
                         -- fireEvent (Init i)
                       _ -> do  
                         -- Right wlst' <- 
                         r <- runErrorT $ mapM (\x -> liftM fst (x <==> giveEventSub e)) wlst
                         case r of 
                           Left err -> liftIO $ print err >> error "hello"
                           Right wlst' -> modify (worldActor.workers .~ wlst')
      req <- lift (request (Output GiveEvent ()))
      go req 
    go (Input FlushLog (logobj :: LogServer m ())) = do
      logf <- get >>= return . (^. worldState.bufLog )
      let msg = logf "" 
      if ((not . null) msg) 
        then do 
          let l1 = runErrorT (logobj <==> writeLog ("[World] " ++ (logf ""))) 
          Right (logobj',_) <- lift . lift $ l1
          modify (worldState.bufLog .~ id)
          req <- lift (request (Output FlushLog logobj'))
          go req  
        else do 
          req <- lift (request Ignore) 
          go req 
    go (Input FlushQueue ()) = do
      q <- ( ^. worldState.bufQueue ) <$> get
      let lst = fqueue q ++ reverse (bqueue q)
      modify ( worldState.bufQueue .~ emptyQueue )
      req <- lift (request (Output FlushQueue lst))
      go req 

