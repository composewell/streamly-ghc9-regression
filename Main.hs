module Main (main) where

import qualified Rules
import qualified StreamK
import StreamK (IsStream, MonadAsync)

{-# INLINE sourceUnfoldrM #-}
sourceUnfoldrM :: MonadAsync m => StreamK.Stream m Int
sourceUnfoldrM = Rules.unfoldrM step 0
    where
    step cnt =
        if cnt > 100000
        then return Nothing
        else return (Just (cnt, cnt + 1))

main :: IO ()
main = Rules.drain sourceUnfoldrM
