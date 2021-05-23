{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Operations (unfoldrM, drain, postscan, after_, replicate, fold, unfold,
        splitOnSeq) where

import Control.Monad.IO.Class (MonadIO(..))
import Data.Bits (shiftR, shiftL, (.|.), (.&.))
import Data.Word (Word, Word32)
import Fold (Fold(..))
import Foreign.Storable (Storable(..))
import GHC.Types (SPEC(..))
import Array (Array)
import StreamK (IsStream, MonadAsync)
import Step (Step(..))
import Unfold (Unfold)
import Fusion.Plugin.Types (Fuse(..))
import StreamK (adaptState)
import qualified StreamK as K
import qualified StreamD as D
import qualified Serial
import qualified Fold as FL
import qualified Array as A
import qualified Ring as RB
import Prelude hiding (replicate)

{-# INLINE [2] unfoldrM #-}
unfoldrM :: (IsStream t, MonadAsync m) => (b -> m (Maybe (a, b))) -> b -> t m a
unfoldrM = K.unfoldrM

{-# RULES "unfoldrM serial" unfoldrM = unfoldrMSerial #-}
{-# INLINE [2] unfoldrMSerial #-}
unfoldrMSerial :: MonadAsync m => (b -> m (Maybe (a, b))) -> b -> K.Stream m a
unfoldrMSerial = Serial.unfoldrM

{-# INLINE [2] drain #-}
drain :: (Monad m) => K.Stream m a -> m ()
drain m = D.drain $ D.fromStreamK (K.toStream m)
{-# RULES "drain fallback to CPS" [1]
    forall a. D.drain (D.fromStreamK a) = K.drain a #-}

{-# INLINE [1] postscan #-}
postscan :: (IsStream t, Monad m)
    => Fold m a b -> t m a -> t m b
postscan fld m =
    D.fromStreamD $ D.postscanOnce fld $ D.toStreamD m

{-# INLINE [1] replicate #-}
replicate :: (IsStream t, Monad m) => Int -> a -> t m a
replicate n = D.fromStreamD . D.replicate n

{-# INLINE fold_ #-}
fold_ :: Monad m => Fold m a b -> K.Stream m a -> m (b, K.Stream m a)
fold_ fl strm = do
    (b, str) <- D.fold_ fl $ D.toStreamD strm
    return $! (b, D.fromStreamD str)

{-# INLINE fold #-}
fold :: Monad m => Fold m a b -> K.Stream m a -> m b
fold fl strm = do
    (b, _) <- fold_ fl strm
    return $! b

{-# INLINE after_ #-}
after_ :: (IsStream t, Monad m) => m b -> t m a -> t m a
after_ action xs = D.fromStreamD $ D.after_ action $ D.toStreamD xs

{-# INLINE unfold #-}
unfold :: (IsStream t, Monad m) => Unfold m a b -> a -> t m b
unfold unf x = D.fromStreamD $ D.unfold unf x

{-# ANN type SplitOnSeqState Fuse #-}
data SplitOnSeqState rb rh ck w fs s b x =
      SplitOnSeqInit
    | SplitOnSeqYield b (SplitOnSeqState rb rh ck w fs s b x)
    | SplitOnSeqDone

    | SplitOnSeqEmpty !fs s

    | SplitOnSeqSingle !fs s x

    | SplitOnSeqWordInit !fs s
    | SplitOnSeqWordLoop !w s !fs
    | SplitOnSeqWordDone Int !fs !w

    | SplitOnSeqKRInit Int !fs s rb !rh
    | SplitOnSeqKRLoop fs s rb !rh !ck
    | SplitOnSeqKRCheck fs s rb !rh
    | SplitOnSeqKRDone Int !fs rb !rh

    | SplitOnSeqReinit (fs -> SplitOnSeqState rb rh ck w fs s b x)

{-# INLINE [1] splitOnSeqD #-}
splitOnSeqD
    :: forall m a b. (MonadIO m, Storable a, Enum a, Eq a)
    => Array a
    -> Fold m a b
    -> D.Stream m a
    -> D.Stream m b
splitOnSeqD patArr (Fold fstep initial done) (D.Stream step state) =
    D.Stream stepOuter SplitOnSeqInit

    where

    patLen = A.length patArr
    maxIndex = patLen - 1
    elemBits = sizeOf (undefined :: a) * 8

    -- For word pattern case
    wordMask :: Word
    wordMask = (1 `shiftL` (elemBits * patLen)) - 1

    elemMask :: Word
    elemMask = (1 `shiftL` elemBits) - 1

    wordPat :: Word
    wordPat = wordMask .&. A.foldl' addToWord 0 patArr

    addToWord wd a = (wd `shiftL` elemBits) .|. fromIntegral (fromEnum a)

    -- For Rabin-Karp search
    k = 2891336453 :: Word32
    coeff = k ^ patLen

    addCksum cksum a = cksum * k + fromIntegral (fromEnum a)

    deltaCksum cksum old new =
        addCksum cksum new - coeff * fromIntegral (fromEnum old)

    -- XXX shall we use a random starting hash or 1 instead of 0?
    patHash = A.foldl' addCksum 0 patArr

    skip = return . Skip

    nextAfterInit nextGen stepRes =
        case stepRes of
            FL.Partial s -> nextGen s
            FL.Done b -> SplitOnSeqYield b (SplitOnSeqReinit nextGen)

    {-# INLINE yieldProceed #-}
    yieldProceed nextGen fs =
        initial >>= skip . SplitOnSeqYield fs . nextAfterInit nextGen

    {-# INLINE [0] stepOuter #-}
    stepOuter _ SplitOnSeqInit = do
        res <- initial
        case res of
            FL.Partial acc ->
                if patLen == 0
                then return $ Skip $ SplitOnSeqEmpty acc state
                else if patLen == 1
                     then do
                         pat <- liftIO $ A.unsafeIndexIO patArr 0
                         return $ Skip $ SplitOnSeqSingle acc state pat
                     else if sizeOf (undefined :: a) * patLen
                               <= sizeOf (undefined :: Word)
                          then return $ Skip $ SplitOnSeqWordInit acc state
                          else do
                              (rb, rhead) <- liftIO $ RB.new patLen
                              skip $ SplitOnSeqKRInit 0 acc state rb rhead
            FL.Done b -> skip $ SplitOnSeqYield b SplitOnSeqInit

    stepOuter _ (SplitOnSeqYield x next) = return $ Yield x next

    ---------------------------
    -- Checkpoint
    ---------------------------

    stepOuter _ (SplitOnSeqReinit nextGen) =
        initial >>= skip . nextAfterInit nextGen

    ---------------------------
    -- Empty pattern
    ---------------------------

    stepOuter gst (SplitOnSeqEmpty acc st) = do
        res <- step (adaptState gst) st
        case res of
            Yield x s -> do
                r <- fstep acc x
                b1 <-
                    case r of
                        FL.Partial acc1 -> done acc1
                        FL.Done b -> return b
                let jump c = SplitOnSeqEmpty c s
                 in yieldProceed jump b1
            Skip s -> skip (SplitOnSeqEmpty acc s)
            Stop -> return Stop

    -----------------
    -- Done
    -----------------

    stepOuter _ SplitOnSeqDone = return Stop

    -----------------
    -- Single Pattern
    -----------------

    stepOuter gst (SplitOnSeqSingle fs st pat) = do
        res <- step (adaptState gst) st
        case res of
            Yield x s -> do
                let jump c = SplitOnSeqSingle c s pat
                if pat == x
                then done fs >>= yieldProceed jump
                else do
                    r <- fstep fs x
                    case r of
                        FL.Partial fs1 -> skip $ jump fs1
                        FL.Done b -> yieldProceed jump b
            Skip s -> return $ Skip $ SplitOnSeqSingle fs s pat
            Stop -> do
                r <- done fs
                return $ Skip $ SplitOnSeqYield r SplitOnSeqDone

    ---------------------------
    -- Short Pattern - Shift Or
    ---------------------------

    stepOuter _ (SplitOnSeqWordDone 0 fs _) = do
        r <- done fs
        skip $ SplitOnSeqYield r SplitOnSeqDone
    stepOuter _ (SplitOnSeqWordDone n fs wrd) = do
        let old = elemMask .&. (wrd `shiftR` (elemBits * (n - 1)))
        r <- fstep fs (toEnum $ fromIntegral old)
        case r of
            FL.Partial fs1 -> skip $ SplitOnSeqWordDone (n - 1) fs1 wrd
            FL.Done b -> do
                 let jump c = SplitOnSeqWordDone (n - 1) c wrd
                 yieldProceed jump b

    stepOuter gst (SplitOnSeqWordInit fs st0) =
        go SPEC 0 0 st0

        where

        {-# INLINE go #-}
        go !_ !idx !wrd !st = do
            res <- step (adaptState gst) st
            case res of
                Yield x s -> do
                    let wrd1 = addToWord wrd x
                    if idx == maxIndex
                    then do
                        if wrd1 .&. wordMask == wordPat
                        then do
                            let jump c = SplitOnSeqWordInit c s
                            done fs >>= yieldProceed jump
                        else skip $ SplitOnSeqWordLoop wrd1 s fs
                    else go SPEC (idx + 1) wrd1 s
                Skip s -> go SPEC idx wrd s
                Stop -> do
                    if idx /= 0
                    then skip $ SplitOnSeqWordDone idx fs wrd
                    else do
                        r <- done fs
                        skip $ SplitOnSeqYield r SplitOnSeqDone

    stepOuter gst (SplitOnSeqWordLoop wrd0 st0 fs0) =
        go SPEC wrd0 st0 fs0

        where

        {-# INLINE go #-}
        go !_ !wrd !st !fs = do
            res <- step (adaptState gst) st
            case res of
                Yield x s -> do
                    let jump c = SplitOnSeqWordInit c s
                        wrd1 = addToWord wrd x
                        old = (wordMask .&. wrd)
                                `shiftR` (elemBits * (patLen - 1))
                    r <- fstep fs (toEnum $ fromIntegral old)
                    case r of
                        FL.Partial fs1 -> do
                            if wrd1 .&. wordMask == wordPat
                            then done fs1 >>= yieldProceed jump
                            else go SPEC wrd1 s fs1
                        FL.Done b -> yieldProceed jump b
                Skip s -> go SPEC wrd s fs
                Stop -> skip $ SplitOnSeqWordDone patLen fs wrd

    -------------------------------
    -- General Pattern - Karp Rabin
    -------------------------------

    stepOuter gst (SplitOnSeqKRInit idx fs st rb rh) = do
        res <- step (adaptState gst) st
        case res of
            Yield x s -> do
                rh1 <- liftIO $ RB.unsafeInsert rb rh x
                if idx == maxIndex
                then do
                    let fld = RB.unsafeFoldRing (RB.ringBound rb)
                    let !ringHash = fld addCksum 0 rb
                    if ringHash == patHash
                    then skip $ SplitOnSeqKRCheck fs s rb rh1
                    else skip $ SplitOnSeqKRLoop fs s rb rh1 ringHash
                else skip $ SplitOnSeqKRInit (idx + 1) fs s rb rh1
            Skip s -> skip $ SplitOnSeqKRInit idx fs s rb rh
            Stop -> do
                skip $ SplitOnSeqKRDone idx fs rb (RB.startOf rb)

    -- XXX The recursive "go" is more efficient than the state based recursion
    -- code commented out below. Perhaps its more efficient because of
    -- factoring out "rb" outside the loop.
    --
    stepOuter gst (SplitOnSeqKRLoop fs0 st0 rb rh0 cksum0) =
        go SPEC fs0 st0 rh0 cksum0

        where

        go !_ !fs !st !rh !cksum = do
            res <- step (adaptState gst) st
            case res of
                Yield x s -> do
                    old <- liftIO $ peek rh
                    let cksum1 = deltaCksum cksum old x
                    r <- fstep fs old
                    case r of
                        FL.Partial fs1 -> do
                            rh1 <- liftIO (RB.unsafeInsert rb rh x)
                            if cksum1 == patHash
                            then skip $ SplitOnSeqKRCheck fs1 s rb rh1
                            else go SPEC fs1 s rh1 cksum1
                        FL.Done b -> do
                            let rst = RB.startOf rb
                                jump c = SplitOnSeqKRInit 0 c s rb rst
                            yieldProceed jump b
                Skip s -> go SPEC fs s rh cksum
                Stop -> skip $ SplitOnSeqKRDone patLen fs rb rh

    -- XXX The following code is 5 times slower compared to the recursive loop
    -- based code above. Need to investigate why. One possibility is that the
    -- go loop above does not thread around the ring buffer (rb). This code may
    -- be causing the state to bloat and getting allocated on each iteration.
    -- We can check the cmm/asm code to confirm.  If so a good GHC solution to
    -- such problem is needed. One way to avoid this could be to use unboxed
    -- mutable state?
    {-
    stepOuter gst (SplitOnSeqKRLoop fs st rb rh cksum) = do
            res <- step (adaptState gst) st
            case res of
                Yield x s -> do
                    old <- liftIO $ peek rh
                    let cksum1 = deltaCksum cksum old x
                    fs1 <- fstep fs old
                    if (cksum1 == patHash)
                    then do
                        r <- done fs1
                        skip $ SplitOnSeqYield r $ SplitOnSeqKRInit 0 s rb rh
                    else do
                        rh1 <- liftIO (RB.unsafeInsert rb rh x)
                        skip $ SplitOnSeqKRLoop fs1 s rb rh1 cksum1
                Skip s -> skip $ SplitOnSeqKRLoop fs s rb rh cksum
                Stop -> skip $ SplitOnSeqKRDone patLen fs rb rh
    -}

    stepOuter _ (SplitOnSeqKRCheck fs st rb rh) = do
        if RB.unsafeEqArray rb rh patArr
        then do
            r <- done fs
            let rst = RB.startOf rb
                jump c = SplitOnSeqKRInit 0 c st rb rst
            yieldProceed jump r
        else skip $ SplitOnSeqKRLoop fs st rb rh patHash

    stepOuter _ (SplitOnSeqKRDone 0 fs _ _) = do
        r <- done fs
        skip $ SplitOnSeqYield r SplitOnSeqDone
    stepOuter _ (SplitOnSeqKRDone n fs rb rh) = do
        old <- liftIO $ peek rh
        let rh1 = RB.advance rb rh
        r <- fstep fs old
        case r of
            FL.Partial fs1 -> skip $ SplitOnSeqKRDone (n - 1) fs1 rb rh1
            FL.Done b -> do
                 let jump c = SplitOnSeqKRDone (n - 1) c rb rh1
                 yieldProceed jump b

{-# INLINE splitOnSeq #-}
splitOnSeq
    :: (IsStream t, MonadIO m, Storable a, Enum a, Eq a)
    => Array a -> Fold m a b -> t m a -> t m b
splitOnSeq patt f m = D.fromStreamD $ splitOnSeqD patt f (D.toStreamD m)
