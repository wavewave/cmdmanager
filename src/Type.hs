{-# LANGUAGE GADTs, NoMonomorphismRestriction, ScopedTypeVariables,
             RecordWildCards #-}

------------------------------------------------
-- | Some types for cmd executor 
-- 
-- 
-------------------------------------------------

module Type where 

import Control.Applicative hiding (empty)
import Control.Category
import Control.Lens
import Control.Monad.Error 
import Control.Monad.State
import Data.Map hiding (null)
import Data.SafeCopy
import Data.Serialize.Get 
import Data.Serialize.Put
import Data.UUID hiding (null)
-- from coroutine-object
import Control.Monad.Coroutine
import Control.Monad.Coroutine.Event 
import Control.Monad.Coroutine.Object
import Control.Monad.Coroutine.Queue
-- 
import Prelude hiding (id,(.))


-- | 
data SubOp i o where 
  GiveEventSub :: SubOp CmdExecEvent ()

-- | info about command 
data CmdSet = CmdSet { program :: String 
                     , workdir :: FilePath } 
            deriving (Show,Eq)

-- | 
data CmdExecEvent = Start CmdSet
                  | Init Int 
                  | Finish Int
                  | Render 
                    deriving (Show,Eq)

-- | 
instance SafeCopy CmdSet where
  putCopy CmdSet {..} = contain $ safePut program >> safePut workdir 
  getCopy = contain $ CmdSet <$> safeGet <*> safeGet                        


-- | 
instance SafeCopy CmdExecEvent where
  putCopy (Start exec) = contain $ safePut (1 :: Int) >> safePut exec
  putCopy (Init n) = contain $ safePut (2 :: Int) >> safePut n 
  putCopy (Finish n) = contain $ safePut (3 :: Int) >> safePut n 
  putCopy Render = contain $ safePut (4 :: Int)
  getCopy = contain $ do (x :: Int) <- safeGet 
                         case x of 
                           1 -> Start <$> safeGet  
                           2 -> Init <$> safeGet
                           3 -> Finish <$> safeGet
                           4 -> pure Render 
                           _ -> error "failed in getCopy of CmdExecEvent"


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

-- |
giveEventSub :: (Monad m) => CmdExecEvent -> ClientObj SubOp m () 
giveEventSub ev = request (Input GiveEventSub ev) >> return ()



-- | 
data JobStatus = None | Started | Ended 
               deriving (Show,Eq)



-- | full state of world 
data WorldState = WorldState { _nextID :: Int 
                             , _jobMap :: Map Int JobStatus
                             , _bufLog :: String -> String 
                             , _bufQueue :: Queue (Either ActionOrder Event) }

-- | lens 
nextID :: Simple Lens WorldState Int 
nextID = lens _nextID (\a b -> a { _nextID = b })


-- | lens
jobMap :: Simple Lens WorldState (Map Int JobStatus)
jobMap = lens _jobMap (\a b -> a { _jobMap = b })


-- | 
bufLog :: Simple Lens WorldState (String -> String) 
bufLog = lens _bufLog (\a b -> a { _bufLog = b } )


-- | 
bufQueue :: Simple Lens WorldState (Queue (Either ActionOrder Event))
bufQueue = lens _bufQueue (\a b -> a { _bufQueue = b} )


-- | 
emptyWorldState :: WorldState 
emptyWorldState = WorldState { _nextID = 0 
                             , _jobMap = empty 
                             , _bufLog = id 
                             , _bufQueue = emptyQueue } 


-- | type synonym for actor object in world 
type WorldObject m r = ServerObj SubOp (StateT (WorldAttrib m) m) r 



-- | full collection of actors in world 
data WorldActor m 
    = WorldActor { _workers :: [WorldObject m ()] } 


-- | objWorker lens
workers :: Simple Lens (WorldActor m) [WorldObject m ()]
workers = lens _workers (\a b -> a { _workers = b })


-- | 
initWorldActor :: (Monad m) => WorldActor m 
initWorldActor = WorldActor { _workers = [] }

-- | 
data WorldAttrib m =
       WorldAttrib { _worldState :: WorldState
                   , _worldActor :: WorldActor m } 


-- | lens
worldState :: Simple Lens (WorldAttrib m) WorldState
worldState = lens _worldState (\a b -> a {_worldState = b})


-- | lens 
worldActor :: Simple Lens (WorldAttrib m) (WorldActor m)
worldActor = lens _worldActor (\a b -> a {_worldActor = b})


-- | initialization   
initWorld :: (Monad m) => WorldAttrib m 
initWorld = WorldAttrib emptyWorldState initWorldActor
