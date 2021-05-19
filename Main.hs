module Main (main) where

import Data.Word (Word8)
import System.IO (openFile, IOMode(..))
import StreamK (IsStream, MonadAsync)
-- import Operations (unfoldrM, drain, postscan)
import qualified Operations
import qualified StreamK
import qualified Fold
import qualified Handle

{-
{-# INLINE sourceUnfoldrM #-}
sourceUnfoldrM :: MonadAsync m => StreamK.Stream m Int
sourceUnfoldrM = unfoldrM step 0
    where
    step cnt =
        if cnt > 100000
        then return Nothing
        else return (Just (cnt, cnt + 1))
-}

main :: IO ()
-- main = drain sourceUnfoldrM
-- main = drain $ postscan Fold.sum sourceUnfoldrM
main = do
    devNull <- openFile "/dev/null" WriteMode
    let source = Operations.after_
                    (return ())
                    (Operations.replicate 10000000 (123 :: Word8))
     in Operations.fold (Handle.write devNull) source
