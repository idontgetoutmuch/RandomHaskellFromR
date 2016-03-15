{-# OPTIONS_GHC -Wall                 #-}

{-# LANGUAGE ForeignFunctionInterface #-}

module TestMwcViaR where

import Foreign
import Foreign.C.Types

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable as VS

import System.Random ( next )
import System.Random ( RandomGen )

import qualified System.Random.MWC as MWC

import Control.Monad.Primitive
import System.IO.Unsafe


data MWCRNG = MWCRNG { mWCRNG :: MWC.Gen (PrimState IO) }

instance RandomGen MWCRNG where
  next g@(MWCRNG gen) = unsafeDupablePerformIO $
   do v <- MWC.uniform gen
      return (v, g)

foreign export ccall createHsMwcS :: Ptr Word32 -> Ptr CInt -> IO ()

createHsMwc :: IO (VS.Vector Word32)
createHsMwc = do
  c <- MWC.create
  s <- MWC.save c
  return $ V.convert $ MWC.fromSeed s

createHsMwcS :: Ptr Word32 -> Ptr CInt -> IO ()
createHsMwcS v l = do
  s <- createHsMwc
  pokeArray v (VS.toList s)
  poke l (fromIntegral $ VS.length s)
  return ()

foreign export ccall randHsMwcS :: Ptr Word32 -> Ptr CInt ->
                                   Ptr CInt -> Ptr Word32 -> Ptr CInt ->
                                   IO ()

randHsMwc :: VS.Vector Word32 -> IO (Int, VS.Vector Word32)
randHsMwc vs1 = do
  let us1 :: VU.Vector Word32
      us1 = V.convert vs1
  g1 <- MWC.restore $ MWC.toSeed us1
  let (r, g2) = next (MWCRNG g1)
  us2 <- MWC.save $ mWCRNG g2
  let vs2 = V.convert $ MWC.fromSeed us2
  return (r, vs2)

randHsMwcS :: Ptr Word32 -> Ptr CInt ->
              Ptr CInt -> Ptr Word32 -> Ptr CInt ->
              IO ()
randHsMwcS v1 lPtr1 rPtr v2 lPtr2 = do
  l1 <- peek lPtr1
  vs1 <- peekArray (fromIntegral l1) v1
  (r, g) <- randHsMwc $ VS.fromList vs1
  poke rPtr (fromIntegral r)
  pokeArray v2 (VS.toList g)
  poke lPtr2 (fromIntegral $ VS.length g)
  return ()
