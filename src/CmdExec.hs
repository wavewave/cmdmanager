{-# LANGUAGE GADTs, NoMonomorphismRestriction, ScopedTypeVariables, 
             KindSignatures, RecordWildCards, FlexibleContexts #-}

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
import System.FilePath 
import System.Directory 
import System.IO
import System.Process 
-- from coroutine-object
import Control.Monad.Coroutine
import Control.Monad.Coroutine.Event 
import Control.Monad.Coroutine.Logger 
import Control.Monad.Coroutine.Object
import Control.Monad.Coroutine.Queue
import Control.Monad.Coroutine.World
-- from this package 
import Type.CmdExec
import Type.CmdExecEvent
import Type.CmdSet
import Type.WorldState
import Util
-- 
import Prelude hiding ((.),id)

-- | 
doCmdAction :: Int -> CmdSet -> (Event -> IO ()) -> IO ()
doCmdAction i cmdset@CmdSet {..}  handler = do 
    forkIO $ do setCurrentDirectory workdir
                (_mhin,Just hout,_mherr,h) <- createProcess (shell program) { std_out = CreatePipe }  
                str <- hGetContents hout 
                writeFile (workdir </> stdoutfile ) str 
                handler (eventWrap (Finish i))
    return ()



-- | command executor actor
cmdexec :: forall m m1. (Monad m, MonadState (WorldAttrib m1) m) => 
           Int -> ServerObj CmdOp m ()
cmdexec i = initServer (go i None)  
  where 
    go :: Int -> JobStatus CmdSet -> MethodInput CmdOp -> ServerT CmdOp m ()
    go i _jst (Input CmdReady cset) = do 
        req <- request (Output CmdReady ())
        go i (Assigned cset) req 
    go i (Assigned cset) (Input CmdInit ())  = do 
        jst' <- fireAction (doCmdAction i cset)
        req <- request (Output CmdInit ())
        go i (Started cset) req 

{-
case ev of 
          CmdReady -> do 
            if i == i' 
              else return (False,jst)
          Finish i' -> do
            if i == i' then return (True,Ended) 
                       else return (False,jst)
          _ -> return (False,jst)
      req <- if r then request (Output GiveEventSub ())
                  else request Ignore 
      workerW i jst' req 
-}
          
