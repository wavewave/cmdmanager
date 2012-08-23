{-# LANGUAGE ScopedTypeVariables, StandaloneDeriving, GADTs, Rank2Types, 
             RecordWildCards #-}

module Main where

import Control.Concurrent 
import System.FilePath
-- from hep-platform package 
import Control.Monad.Coroutine.Driver 
import Control.Monad.Coroutine.Event 
import Control.Monad.Coroutine.EventHandler 
import HEP.Physics.MSSM.Model.MSUGRA
-- from this package 
import WebLog
import CmdExec
import Type 

-- |
sampleMSUGRA :: MSUGRAInput 
sampleMSUGRA = MSUGRAInput { mSUGRA_m0 = 250
                           , mSUGRA_m12 = 100 
                           , mSUGRA_a0 = 0
                           , mSUGRA_tanb = 10 
                           , mSUGRA_sgnmu = sgnplus } 

softsusydir :: FilePath 
softsusydir = "/home/wavewave/repo/ext/softsusy-3.3.3" 

tempworkdir :: FilePath 
tempworkdir = "/home/wavewave/repo/workspace/haskellstudy/slow"

argsForMSUGRA :: MSUGRAInput -> String 
argsForMSUGRA MSUGRAInput {..} = "sugra " 
                                 ++ show mSUGRA_m0 
                                 ++ " " ++ show mSUGRA_m12 
                                 ++ " " ++ show mSUGRA_a0
                                 ++ " " ++ show mSUGRA_tanb
                                 ++ " unified " 
                                 ++ " " ++ show (toInt mSUGRA_sgnmu)

softpointCmd :: MSUGRAInput -> CmdSet 
softpointCmd msugra 
    = CmdSet (softsusydir </> "softpoint.x" ++ " " ++ argsForMSUGRA msugra) 
             "/home/wavewave/repo/workspace/haskellstudy/slow"
             "teststdout"




test_tickingevent :: IO () 
test_tickingevent = do 
    dref <- newEmptyMVar :: IO (MVar (Driver IO ()))
    let logger = weblogger "http://127.0.0.1:7800"
    putMVar dref (driver logger world (eventHandler dref))
    putStrLn "starting ticking" 
    ticking dref 0    


second :: Int 
second = 1000000

{- -- | 
mycmd :: CmdSet
mycmd = CmdSet "/home/wavewave/repo/workspace/haskellstudy/slow/slow 20"
               "/home/wavewave/repo/workspace/haskellstudy/slow"
               "teststdout"
-}

-- | 

ticking :: MVar (Driver IO ()) -> Int -> IO () 
ticking mvar n = do 
    putStrLn "--------------------------"
    putStrLn ("ticking : " ++ show n)
    print (softpointCmd sampleMSUGRA)
    {- if n `mod` 10 == 0 
      then eventHandler mvar Open  
      else if n `mod` 10 == 5                 
        then eventHandler mvar Close 
        else if n `mod` 10 == 3
          then eventHandler mvar Render
          else eventHandler mvar (Message ("test : " ++ show n)) -}
    {- if n == 5 
      then eventHandler mvar Start 
      else if n `mod` 3 == 0 
           then eventHandler mvar Render
           else eventHandler mvar (Message ("test : " ++ show n)) -}
    let action 
            | n `mod` 10 == 5 = eventHandler mvar 
                                  (eventWrap (Start (softpointCmd sampleMSUGRA)))
            --  | n `mod` 10 == 9 = eventHandler mvar (eventWrap (Init (n `div` 10)))
            | otherwise = eventHandler mvar (eventWrap Render)
    action 

   

    putStrLn "_-_-_-_-_-_-_-_-_-_-_-_-_-"
    threadDelay (1*second)
    ticking mvar (n+1)

------------------------------- 
-- test 
-------------------------------

main :: IO ()
main = test_tickingevent 
