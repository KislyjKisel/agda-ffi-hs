{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Word where

postulate
    Word   : Set
    Word8  : Set
    Word16 : Set
    Word32 : Set
    Word64 : Set

    byteSwap16 : Word16 → Word16
    byteSwap32 : Word32 → Word32
    byteSwap64 : Word64 → Word64

    bitReverse8  : Word8 → Word8
    bitReverse16 : Word16 → Word16
    bitReverse32 : Word32 → Word32
    bitReverse64 : Word64 → Word64


{-# FOREIGN GHC import qualified Data.Word #-}

{-# COMPILE GHC Word   = type Data.Word.Word   #-}
{-# COMPILE GHC Word8  = type Data.Word.Word8  #-}
{-# COMPILE GHC Word16 = type Data.Word.Word16 #-}
{-# COMPILE GHC Word32 = type Data.Word.Word32 #-}
{-# COMPILE GHC Word64 = type Data.Word.Word64 #-}

{-# COMPILE GHC byteSwap16 = Data.Word.byteSwap16 #-}
{-# COMPILE GHC byteSwap32 = Data.Word.byteSwap32 #-}
{-# COMPILE GHC byteSwap64 = Data.Word.byteSwap64 #-}

{-# COMPILE GHC bitReverse8  = Data.Word.bitReverse8  #-}
{-# COMPILE GHC bitReverse16 = Data.Word.bitReverse16 #-}
{-# COMPILE GHC bitReverse32 = Data.Word.bitReverse32 #-}
{-# COMPILE GHC bitReverse64 = Data.Word.bitReverse64 #-}
