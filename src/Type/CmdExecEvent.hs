{-# LANGUAGE ScopedTypeVariables #-}

module Type.CmdExecEvent where

import Control.Applicative
import Data.SafeCopy 
import Data.Serialize.Get
import Data.Serialize.Put
import Data.UUID
-- 
import Control.Monad.Coroutine
import Control.Monad.Coroutine.Event
import Control.Monad.Coroutine.Object
--
import Type.CmdSet 

-- | 
data CmdExecEvent = Init
                  | Start Int (CmdSet,CmdSet)
                  | Finish Int
                    deriving (Show,Eq)


-- | 
instance SafeCopy CmdExecEvent where
  putCopy Init  = contain $ safePut (0 :: Int)  
  putCopy (Start n execs) = contain $ safePut (1 :: Int) 
                                      >> safePut n
                                      >> safePut execs
  putCopy (Finish n) = contain $ safePut (2 :: Int) >> safePut n 
  getCopy = contain $ do (x :: Int) <- safeGet 
                         case x of 
                           0 -> pure Init  
                           1 -> Start <$> safeGet <*> safeGet   
                           2 -> Finish <$> safeGet
                           _ -> error "failed in getCopy of CmdExecEvent"


-- | global UUID for CmdExecEvent 
evuuid :: UUID
evuuid = case (fromString "858066b5-e6e7-431d-9ff3-facc5eb0befc") of 
           Nothing -> error "evuuid in CmdExec.hs" 
           Just i -> i 

evwrap :: CmdExecEvent -> Event 
evwrap ev = Event (evuuid,runPut (safePut ev))


-- | 
instance Eventable CmdExecEvent where
  eventClassID _ = evuuid 
  eventWrap = evwrap 

getCmdExecEvent :: Event -> Maybe CmdExecEvent 
getCmdExecEvent (Event (i,bstr))  
    | i == evuuid = either (const Nothing) Just (runGet safeGet bstr)
    | otherwise = Nothing 

