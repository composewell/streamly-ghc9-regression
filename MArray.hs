{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module MArray
    (
      Array (..)
    , writeNUnsafe
    , length
    )

where

import Control.Exception (assert)
import Fold (Fold(..))
import Control.Monad.IO.Class (MonadIO(..))
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Ptr (plusPtr, minusPtr, castPtr, nullPtr)
import Foreign.Storable (Storable(..))
import GHC.ForeignPtr (ForeignPtr(..))
import GHC.IO (IO(IO), unsafePerformIO)
import GHC.Ptr (Ptr(..))
import qualified GHC.ForeignPtr as GHC
import qualified Fold as FL
import Prelude hiding (length)

data Array a =
    Array
    { aStart :: {-# UNPACK #-} !(ForeignPtr a) -- ^ first address
    , aEnd   :: {-# UNPACK #-} !(Ptr a)        -- ^ first unused address
    , aBound :: {-# UNPACK #-} !(Ptr a)        -- ^ first address beyond allocated memory
    }

data ArrayUnsafe a = ArrayUnsafe
    {-# UNPACK #-} !(ForeignPtr a) -- first address
    {-# UNPACK #-} !(Ptr a)        -- first unused address

-- | allocate a new array using the provided allocator function.
{-# INLINE newArrayAlignedAllocWith #-}
newArrayAlignedAllocWith :: forall a. Storable a
    => (Int -> Int -> IO (ForeignPtr a)) -> Int -> Int -> IO (Array a)
newArrayAlignedAllocWith alloc alignSize count = do
    let size = count * sizeOf (undefined :: a)
    fptr <- alloc size alignSize
    let p = unsafeForeignPtrToPtr fptr
    return $ Array
        { aStart = fptr
        , aEnd   = p
        , aBound = p `plusPtr` size
        }

{-# INLINE mallocForeignPtrAlignedBytes #-}
mallocForeignPtrAlignedBytes :: Int -> Int -> IO (GHC.ForeignPtr a)
mallocForeignPtrAlignedBytes =
    GHC.mallocPlainForeignPtrAlignedBytes

{-# INLINE newArrayAligned #-}
newArrayAligned :: forall a. Storable a => Int -> Int -> IO (Array a)
newArrayAligned = newArrayAlignedAllocWith mallocForeignPtrAlignedBytes

{-# INLINE newArray #-}
newArray :: forall a. Storable a => Int -> IO (Array a)
newArray = newArrayAligned (alignment (undefined :: a))

-- | Like 'writeN' but does not check the array bounds when writing. The fold
-- driver must not call the step function more than 'n' times otherwise it will
-- corrupt the memory and crash. This function exists mainly because any
-- conditional in the step function blocks fusion causing 10x performance
-- slowdown.
--
-- @since 0.7.0
{-# INLINE [1] writeNUnsafe #-}
writeNUnsafe :: forall m a. (MonadIO m, Storable a)
    => Int -> Fold m a (Array a)
writeNUnsafe n = Fold step initial extract

    where

    initial = do
        (Array start end _) <- liftIO $ newArray (max n 0)
        return $ FL.Partial $ ArrayUnsafe start end

    step (ArrayUnsafe start end) x = do
        liftIO $ poke end x
        return
          $ FL.Partial
          $ ArrayUnsafe start (end `plusPtr` sizeOf (undefined :: a))

    extract (ArrayUnsafe start end) = return $ Array start end end -- liftIO . shrinkToFit

{-# INLINE byteLength #-}
byteLength :: Array a -> Int
byteLength Array{..} =
    let p = unsafeForeignPtrToPtr aStart
        len = aEnd `minusPtr` p
    in assert (len >= 0) len

-- | /O(1)/ Get the length of the array i.e. the number of elements in the
-- array.
--
-- @since 0.7.0
{-# INLINE length #-}
length :: forall a. Storable a => Array a -> Int
length arr = byteLength arr `div` sizeOf (undefined :: a)
