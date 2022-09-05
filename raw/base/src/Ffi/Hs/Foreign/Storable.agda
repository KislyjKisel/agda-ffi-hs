{-# OPTIONS --without-K #-}

module Ffi.Hs.Foreign.Storable where

open import Agda.Primitive
open import Ffi.Hs.-base.Class using ()
open import Ffi.Hs.-base.Unit  using (⊤)
open import Ffi.Hs.Data.Int    using (Int)
open import Ffi.Hs.Foreign.Ptr using (Ptr)
open import Ffi.Hs.System.IO   using (IO)

open Ffi.Hs.-base.Class public
    using (Storable)

private
    variable
        aℓ bℓ : Level
        A : Set aℓ
        B : Set bℓ

postulate
    sizeOf      : A → Int
    alignment   : A → Int
    peekElemOff : Ptr A → Int → IO A
    pokeElemOff : Ptr A → Int → A → IO (⊤ {lzero})
    peekByteOff : Ptr B → Int → IO A
    pokeByteOff : Ptr B → Int → A → IO (⊤ {lzero})
    peek        : Ptr A → IO A
    poke        : Ptr A → A → IO (⊤ {lzero})

{-# FOREIGN GHC import qualified Foreign.Storable #-}
{-# COMPILE GHC sizeOf      = \ aℓ a      -> Foreign.Storable.sizeOf      #-}
{-# COMPILE GHC alignment   = \ aℓ a      -> Foreign.Storable.alignment   #-}
{-# COMPILE GHC peekElemOff = \ aℓ a      -> Foreign.Storable.peekElemOff #-}
{-# COMPILE GHC pokeElemOff = \ aℓ a      -> Foreign.Storable.pokeElemOff #-}
{-# COMPILE GHC peekByteOff = \ bℓ b aℓ a -> Foreign.Storable.peekByteOff #-}
{-# COMPILE GHC pokeByteOff = \ bℓ b aℓ a -> Foreign.Storable.pokeByteOff #-}
{-# COMPILE GHC peek        = \ aℓ a      -> Foreign.Storable.peek        #-}
{-# COMPILE GHC poke        = \ aℓ a      -> Foreign.Storable.poke        #-}
