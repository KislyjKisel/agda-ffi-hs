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
    sizeOf      : ⦃ Storable A ⦄ → A → Int
    alignment   : ⦃ Storable A ⦄ → A → Int
    peekElemOff : ⦃ Storable A ⦄ → Ptr A → Int → IO A
    pokeElemOff : ⦃ Storable A ⦄ → Ptr A → Int → A → IO (⊤ {lzero})
    peekByteOff : ⦃ Storable A ⦄ → Ptr B → Int → IO A
    pokeByteOff : ⦃ Storable A ⦄ → Ptr B → Int → A → IO (⊤ {lzero})
    peek        : ⦃ Storable A ⦄ → Ptr A → IO A
    poke        : ⦃ Storable A ⦄ → Ptr A → A → IO (⊤ {lzero})

{-# FOREIGN GHC
import qualified Foreign.Storable
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Class (AgdaStorable)
#-}
{-# COMPILE GHC sizeOf      = \ aℓ a      AgdaStorable -> Foreign.Storable.sizeOf      #-}
{-# COMPILE GHC alignment   = \ aℓ a      AgdaStorable -> Foreign.Storable.alignment   #-}
{-# COMPILE GHC peekElemOff = \ aℓ a      AgdaStorable -> Foreign.Storable.peekElemOff #-}
{-# COMPILE GHC pokeElemOff = \ aℓ a      AgdaStorable -> Foreign.Storable.pokeElemOff #-}
{-# COMPILE GHC peekByteOff = \ bℓ b aℓ a AgdaStorable -> Foreign.Storable.peekByteOff #-}
{-# COMPILE GHC pokeByteOff = \ bℓ b aℓ a AgdaStorable -> Foreign.Storable.pokeByteOff #-}
{-# COMPILE GHC peek        = \ aℓ a      AgdaStorable -> Foreign.Storable.peek        #-}
{-# COMPILE GHC poke        = \ aℓ a      AgdaStorable -> Foreign.Storable.poke        #-}
