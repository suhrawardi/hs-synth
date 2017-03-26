{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{- |
Type class for several signal storage types
that allows alter element types.
There is some overlap between the two @Transform@ classes.
This was done in order to save us
from ubiquitous @Transform sig y y@ constraints.
-}
module Signal2 where

import Synthesizer.Generic.Signal (Read, viewL, sum, )
import qualified Synthesizer.Generic.Signal as SigG

import qualified Algebra.Module   as Module
-- import qualified Algebra.Additive as Additive

import qualified Synthesizer.State.Signal as SigS
import qualified Synthesizer.Plain.Signal as Sig
-- import qualified Synthesizer.Storable.Signal as SigSt
import qualified Data.StorableVector.Lazy as Vector

import qualified Synthesizer.Plain.Modifier as Modifier

import Foreign.Storable (Storable)

import Control.Monad.Trans.State (runState, )

import qualified Data.List as List

import Data.Tuple.HT (fst3, snd3, thd3, )
import Prelude
   (Bool, Int, Maybe(Just), maybe, fst, snd,
    flip,
    return, )


class (SigG.Transform sig y0, SigG.Transform sig y1) =>
          Transform sig y0 y1 where
   map :: (y0 -> y1) -> (sig y0 -> sig y1)
   scanL :: (y1 -> y0 -> y1) -> y1 -> sig y0 -> sig y1
   crochetL :: (y0 -> s -> Maybe (y1, s)) -> s -> sig y0 -> sig y1


instance (Storable y0, Storable y1) => Transform Vector.Vector y0 y1 where
   {-# INLINE map #-}
   map = Vector.map
   {-# INLINE scanL #-}
   scanL = Vector.scanl
   {-# INLINE crochetL #-}
   crochetL = Vector.crochetL


instance Transform [] y0 y1 where
   {-# INLINE map #-}
   map = List.map
   {-# INLINE scanL #-}
   scanL = List.scanl
   {-# INLINE crochetL #-}
   crochetL = Sig.crochetL


instance Transform SigS.T y0 y1 where
   {-# INLINE map #-}
   map = SigS.map
   {-# INLINE scanL #-}
   scanL = SigS.scanL
   {-# INLINE crochetL #-}
   crochetL = SigS.crochetL



{-# INLINE zipWith #-}
zipWith :: (Read sig a, Transform sig b c) =>
   (a -> b -> c) -> (sig a -> sig b -> sig c)
zipWith h a =
   crochetL
      (\x0 a0 ->
          do (y0,a1) <- viewL a0
             Just (h y0 x0, a1))
      a

{-# INLINE mapAdjacent #-}
mapAdjacent :: (Read sig a, Transform sig a b) =>
   (a -> a -> b) -> sig a -> sig b
mapAdjacent f xs0 =
   let xs1 = maybe xs0 snd (viewL xs0)
   in  zipWith f xs0 xs1


{-# INLINE zip #-}
zip :: (Read sig a, Transform sig b (a,b)) =>
   sig a -> sig b -> sig (a,b)
zip = zipWith (,)


{-# INLINE unzip #-}
unzip :: (Transform sig (a,b) a, Transform sig (a,b) b) =>
   sig (a,b) -> (sig a, sig b)
unzip xs =
   (map fst xs, map snd xs)

{-# INLINE unzip3 #-}
unzip3 :: (Transform sig (a,b,c) a, Transform sig (a,b,c) b, Transform sig (a,b,c) c) =>
   sig (a,b,c) -> (sig a, sig b, sig c)
unzip3 xs =
   (map fst3 xs, map snd3 xs, map thd3 xs)



{-# INLINE modifyStatic #-}
modifyStatic :: (Transform sig a b) =>
   Modifier.Simple s ctrl a b -> ctrl -> sig a -> sig b
modifyStatic (Modifier.Simple state proc) control =
   crochetL (\a acc -> Just (runState (proc control a) acc)) state

{-| Here the control may vary over the time. -}
{-# INLINE modifyModulated #-}
modifyModulated :: (Transform sig a b, Read sig ctrl) =>
   Modifier.Simple s ctrl a b -> sig ctrl -> sig a -> sig b
modifyModulated (Modifier.Simple state proc) control =
   crochetL
      (\x (acc0,cs0) ->
         do (c,cs1) <- viewL cs0
            let (y,acc1) = runState (proc c x) acc0
            return (y,(acc1,cs1)))
      (state,control)

linearComb ::
   (Module.C t y, Read sig t, Transform sig y y) =>
   sig t -> sig y -> y
linearComb ts ys =
   sum (zipWith (Module.*>) ts ys)

mapTails :: (Transform sig a b) =>
   (sig a -> b) -> sig a -> sig b
mapTails f x =
   crochetL (\_ xs0 ->
      do (_,xs1) <- viewL xs0
         Just (f xs0, xs1))
      x x

{-# INLINE zipWithTails #-}
zipWithTails :: (Read sig b, Transform sig a c) =>
   (a -> sig b -> c) -> sig a -> sig b -> sig c
zipWithTails f =
   flip (crochetL (\x ys0 ->
      do (_,ys) <- viewL ys0
         Just (f x ys0, ys)))

{-# INLINE zipWith2Tails #-}
zipWith2Tails :: (Read sig b, Read sig c, Transform sig a d) =>
   (a -> sig b -> sig c -> d) -> sig a -> sig b -> sig c -> sig d
zipWith2Tails f as bs cs =
   crochetL (\x (ys0,zs0) ->
      do (_,ys1) <- viewL ys0
         (_,zs1) <- viewL zs0
         Just (f x ys0 zs0, (ys1,zs1)))
      (bs,cs) as

zipWithState :: (Transform sig b c) =>
   (a -> b -> c) -> SigS.T a -> sig b -> sig c
zipWithState f =
   crochetL (\b as0 ->
      do (a,as1) <- SigS.viewL as0
         Just (f a b, as1))


