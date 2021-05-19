{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
module Handle
    (
     write
    )

where

import Control.Monad.IO.Class (MonadIO(..))
import Data.Word (Word8)
import Foreign.Storable (Storable(..))
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (plusPtr, minusPtr, castPtr, nullPtr)
import GHC.ForeignPtr (ForeignPtr(..), newForeignPtr_)
import GHC.Ptr (Ptr(..))
import System.IO (Handle, hGetBufSome, hPutBuf, stdin, stdout)
import Fold (Fold(..))
import qualified MArray as MA
import qualified Fold as FL
import Prelude hiding (length)

data Array a =
    Array
    { aStart :: {-# UNPACK #-} !(ForeignPtr a) -- first address
    , aEnd   :: {-# UNPACK #-} !(Ptr a)        -- first unused addres
    }

{-# INLINE unsafeFreeze #-}
unsafeFreeze :: MA.Array a -> Array a
unsafeFreeze (MA.Array as ae _) = Array as ae

{-# INLINE [1] writeNUnsafe #-}
writeNUnsafe :: forall m a. (MonadIO m, Storable a)
    => Int -> Fold m a (Array a)
writeNUnsafe n = unsafeFreeze <$> MA.writeNUnsafe n

{-# INLINE unsafeThaw #-}
unsafeThaw :: Array a -> MA.Array a
unsafeThaw (Array as ae) = MA.Array as ae ae

{-# INLINE length #-}
length :: forall a. Storable a => Array a -> Int
length arr =  MA.length (unsafeThaw arr)

{-# INLINABLE writeArray #-}
writeArray :: Storable a => Handle -> Array a -> IO ()
writeArray _ arr | length arr == 0 = return ()
writeArray h Array{..} = withForeignPtr aStart $ \p -> hPutBuf h p aLen
    where
    aLen =
        let p = unsafeForeignPtrToPtr aStart
        in aEnd `minusPtr` p

{-# INLINE writeChunks #-}
writeChunks :: (MonadIO m, Storable a) => Handle -> Fold m (Array a) ()
writeChunks h = FL.drainBy (liftIO . writeArray h)

{-# INLINE writeWithBufferOf #-}
writeWithBufferOf :: MonadIO m => Int -> Handle -> Fold m Word8 ()
writeWithBufferOf n h = FL.chunksOf n (writeNUnsafe n) (writeChunks h)

allocOverhead :: Int
allocOverhead = 2 * sizeOf (undefined :: Int)

mkChunkSize :: Int -> Int
mkChunkSize n = let size = n - allocOverhead in max size 0

mkChunkSizeKB :: Int -> Int
mkChunkSizeKB n = mkChunkSize (n * k)
   where k = 1024

defaultChunkSize :: Int
defaultChunkSize = mkChunkSizeKB 32

{-# INLINE write #-}
write :: MonadIO m => Handle -> Fold m Word8 ()
write = writeWithBufferOf defaultChunkSize
