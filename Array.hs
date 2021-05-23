{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
module Array
    (
      Array(..)
    , fromList
    , read
    , length
    , writeNUnsafe
    , MA.unsafeInlineIO
    , MA.memcmp
    , unsafeFreezeWithShrink
    , foldl'
    , unsafeIndexIO
    )

where

import Control.Monad.IO.Class (MonadIO(..))
import Data.Word (Word8)
import Foreign.Storable (Storable(..))
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (plusPtr, minusPtr, castPtr, nullPtr)
import GHC.ForeignPtr (ForeignPtr(..), newForeignPtr_)
import GHC.ForeignPtr (mallocPlainForeignPtrBytes)
import GHC.IO (IO(IO), unsafePerformIO)
import GHC.Ptr (Ptr(..))
import System.IO (Handle, hGetBufSome, hPutBuf, stdin, stdout)
import Unfold (Unfold(..))
import Fold (Fold(..))
import qualified MArray as MA
import qualified Fold as FL
import qualified Unfold as UF
import qualified StreamD as D
import Prelude hiding (length, read)

data Array a =
    Array
    { aStart :: {-# UNPACK #-} !(ForeignPtr a) -- first address
    , aEnd   :: {-# UNPACK #-} !(Ptr a)        -- first unused addres
    }

{-# INLINE unsafeFreeze #-}
unsafeFreeze :: MA.Array a -> Array a
unsafeFreeze (MA.Array as ae _) = Array as ae

{-# INLINABLE fromList #-}
fromList :: Storable a => [a] -> Array a
fromList xs = unsafeFreeze $ MA.fromList xs

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

{-# INLINE [1] read #-}
read :: forall m a. (Monad m, Storable a) => Unfold m (Array a) a
read = UF.lmap unsafeThaw MA.read

{-# INLINE unsafeFreezeWithShrink #-}
unsafeFreezeWithShrink :: Storable a => MA.Array a -> Array a
unsafeFreezeWithShrink arr = unsafePerformIO $ do
  MA.Array as ae _ <- MA.shrinkToFit arr
  return $ Array as ae

{-# INLINE [1] foldl' #-}
foldl' :: forall a b. Storable a => (b -> a -> b) -> b -> Array a -> b
foldl' f z arr = MA.foldl' f z (unsafeThaw arr)

{-# INLINE [1] unsafeIndexIO #-}
unsafeIndexIO :: forall a. Storable a => Array a -> Int -> IO a
unsafeIndexIO arr = MA.unsafeIndexIO (unsafeThaw arr)

