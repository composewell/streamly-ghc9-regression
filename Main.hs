module Main (main) where

import Data.Functor.Identity (Identity)
import qualified Operations
import qualified StreamK

{-# INLINE sourceUnfoldr #-}
sourceUnfoldr :: Monad m => Int -> Int -> StreamK.Stream m Int
sourceUnfoldr count start = Operations.unfoldr step start
    where
    step cnt =
        if cnt > start + count
        then Nothing
        else Just (cnt, cnt + 1)

{-# INLINE foldableMin #-}
foldableMin :: Int -> Int -> Int
foldableMin value n =
    Prelude.minimum (sourceUnfoldr value n :: StreamK.Stream Identity Int)

main :: IO ()
main = do
    let r = foldableMin 100000 1
    print r
