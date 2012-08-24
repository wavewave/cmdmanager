{-# LANGUAGE GADTs, NoMonomorphismRestriction, ScopedTypeVariables,
             RecordWildCards #-}

------------------------------------------------
-- | Some types for cmd executor 
-- 
-- 
-------------------------------------------------

module Type.WorldState where 

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
import Type.CmdSet 
import Type.SubOp 
-- 
import Prelude hiding (id,(.))


-- | 
data JobStatus a = None | Assigned a | Started a | Ended a  
               deriving (Show,Eq)



-- | full state of world 
data WorldState = WorldState { _nextID :: Int 
                             , _jobMap :: Map Int (JobStatus CmdSet)
                             , _bufLog :: String -> String 
                             , _bufQueue :: Queue (Either ActionOrder Event) }

-- | lens 
nextID :: Simple Lens WorldState Int 
nextID = lens _nextID (\a b -> a { _nextID = b })


-- | lens
jobMap :: Simple Lens WorldState (Map Int (JobStatus CmdSet))
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


