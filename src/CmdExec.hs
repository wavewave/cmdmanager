{-# LANGUAGE GADTs, NoMonomorphismRestriction, ScopedTypeVariables, 
             KindSignatures, RecordWildCards #-}

----------------------------
-- | This module provides main object of cmd executor
----------------------------

module CmdExec where 

import Control.Applicative hiding (empty)
import Control.Category
import Control.Concurrent
import Control.Lens
import Control.Monad.Error 
import Control.Monad.Reader
import Control.Monad.State
import Data.Map.Lens 
import Data.Monoid
import System.Directory 
import System.Process 
-- from coroutine-object
import Control.Monad.Coroutine
import Control.Monad.Coroutine.Event 
import Control.Monad.Coroutine.Logger 
import Control.Monad.Coroutine.Object
import Control.Monad.Coroutine.Queue
import Control.Monad.Coroutine.World
-- from this package 
import Type
-- 
import Prelude hiding ((.),id)

-- | 
doCmdAction :: Int -> CmdSet -> (Event -> IO ()) -> IO ()
doCmdAction i cmdset handler = do 
    forkIO $ do setCurrentDirectory (workdir cmdset)
                system (program cmdset)
                handler (eventWrap (Finish i))
    return ()


-- | command executor actor
cmdexec :: forall m. (Monad m) => 
           CmdSet     -- ^ command set 
        -> Int        -- ^ id 
        -> ServerObj SubOp (StateT (WorldAttrib m) m) ()
cmdexec cmdset idnum = ReaderT (workerW idnum None)  
  where 
    workerW :: Int 
            -> JobStatus 
            -> MethodInput SubOp 
            -> ServerT SubOp (StateT (WorldAttrib m) m) ()
    workerW i jst (Input GiveEventSub ev) = do 
      (r,jst') <- case ev of 
          Init i' -> do 
            if i == i' 
              then do 
                let action = Left . ActionOrder $ doCmdAction i cmdset 
                modify (worldState.bufQueue %~ enqueue action)
                return (True,Started)
              else return (False,jst)
          Finish i' -> do
            if i == i' then return (True,Ended) 
                       else return (False,jst)
          Render -> do 
            modify (worldState.bufLog %~ 
                       (. (<> show i <> "th job status = " <> show jst <> "\n")))
            return (True,jst)
          _ -> return (False,jst)

      modify (worldState.jobMap.at i .~ Just jst')
      req <- if r then request (Output GiveEventSub ())
                  else request Ignore 
      workerW i jst' req 

          
-- | 
world :: forall m. (MonadIO m) => ServerObj (WorldOp m) m () 
world = ReaderT staction 
  where 
    staction req = do runStateT (go req) initWorld
                      return ()
    go :: (MonadIO m) => MethodInput (WorldOp m) -> StateT (WorldAttrib (ServerT (WorldOp m) m)) (ServerT (WorldOp m) m) () 
    go (Input GiveEvent ev) = do
      case getCmdExecEvent ev of  
        Nothing -> return () 
        Just e -> do wlst <- (^. worldActor.workers ) <$> get 
                     case e of 
                       Start cmdset -> do 
                         i <- (^. worldState.nextID) <$> get 
                         let wlst' = cmdexec cmdset i : wlst 
                         modify (worldActor.workers .~ wlst')
                         modify (worldState.nextID %~ (+1) )
                       _ -> do  
                         Right wlst' <- 
                           runErrorT $ mapM (\x -> liftM fst (x <==> giveEventSub e)) wlst
                         modify (worldActor.workers .~ wlst')
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
