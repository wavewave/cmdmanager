{-# LANGUAGE RecordWildCards #-}

module Type.CmdSet where

import Control.Applicative
import Data.SafeCopy 
-- 

-- | info about command 
data CmdSet = CmdSet { program :: String 
                     , workdir :: FilePath 
                     , stdoutfile :: FilePath 
                     } 
            deriving (Show,Eq)

-- | 
instance SafeCopy CmdSet where
  putCopy CmdSet {..} = contain $ do safePut program 
                                     safePut workdir 
                                     safePut stdoutfile 
  getCopy = contain $ CmdSet <$> safeGet <*> safeGet <*> safeGet                

