module Main (main) where

import Data.Char (ord)
import Data.Word (Word8)
import System.IO (openFile, IOMode(..), Handle)
import StreamK (IsStream, MonadAsync)
import qualified Operations
import qualified StreamK
import qualified Fold
import qualified Handle
import qualified Array
import Array (Array)

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

toarr :: String -> Array Word8
toarr = Array.fromList . map (fromIntegral . ord)

-- | Split on a word8 sequence.
splitOnSeq :: String -> Handle -> IO ()
splitOnSeq str inh =
    Operations.drain $ Operations.splitOnSeq (toarr str) Fold.drain
        $ Operations.unfold Handle.read inh

main :: IO ()
-- main = drain sourceUnfoldrM
-- main = drain $ postscan Fold.sum sourceUnfoldrM
{-
main = do
    devNull <- openFile "/dev/null" WriteMode
    let source = Operations.after_
                    (return ())
                    (Operations.replicate 10000000 (123 :: Word8))
     in Operations.fold (Handle.write devNull) source
-}

main = do
    inh <- openFile "input.txt" ReadMode
    splitOnSeq "abcdefghi" inh
