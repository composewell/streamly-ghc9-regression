module Rules (unfoldrM, drain) where

import StreamK (IsStream, MonadAsync)
import qualified StreamK as K
import qualified StreamD as D
import qualified Serial

{-# INLINE [2] unfoldrM #-}
unfoldrM :: (IsStream t, MonadAsync m) => (b -> m (Maybe (a, b))) -> b -> t m a
unfoldrM = K.unfoldrM

{-# RULES "unfoldrM serial" unfoldrM = unfoldrMSerial #-}
{-# INLINE [2] unfoldrMSerial #-}
unfoldrMSerial :: MonadAsync m => (b -> m (Maybe (a, b))) -> b -> K.Stream m a
unfoldrMSerial = Serial.unfoldrM

{-# INLINE [2] drain #-}
drain :: (IsStream t, Monad m) => t m a -> m ()
drain m = D.drain $ D.fromStreamK (K.toStream m)
{-# RULES "drain fallback to CPS" [1]
    forall a. D.drain (D.fromStreamK a) = K.drain a #-}
