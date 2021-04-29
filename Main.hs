module Main (main) where

import StreamK (IsStream, MonadAsync)
import Operations (unfoldrM, drain)
import qualified StreamK

{-# INLINE sourceUnfoldrM #-}
sourceUnfoldrM :: MonadAsync m => StreamK.Stream m Int
sourceUnfoldrM = unfoldrM step 0
    where
    step cnt =
        if cnt > 100000
        then return Nothing
        else return (Just (cnt, cnt + 1))

main :: IO ()
main = drain sourceUnfoldrM
