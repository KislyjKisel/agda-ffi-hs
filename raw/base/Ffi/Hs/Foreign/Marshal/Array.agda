{-# OPTIONS --without-K #-}

module Ffi.Hs.Foreign.Marshal.Array where

open import Agda.Builtin.IO    using (IO)
open import Agda.Builtin.List  using (List)
open import Agda.Primitive
open import Ffi.Hs.-base.Class using (Storable; Eq)
open import Ffi.Hs.-base.Unit  using (⊤; ⊤′)
open import Ffi.Hs.Data.Int    using (Int)
open import Ffi.Hs.Foreign.Ptr using (Ptr)

{-# FOREIGN GHC
import qualified Foreign.Marshal.Array
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ : Level
        A B : Set aℓ

postulate
    mallocArray   : ⦃ Storable A ⦄ → Int → IO (Ptr A)
    mallocArray0  : ⦃ Storable A ⦄ → Int → IO (Ptr A)
    allocaArray   : ⦃ Storable A ⦄ → Int → (Ptr A → IO B) → IO B
    allocaArray0  : ⦃ Storable A ⦄ → Int → (Ptr A → IO B) → IO B
    reallocArray  : ⦃ Storable A ⦄ → Ptr A → Int → IO (Ptr A)
    reallocArray0 : ⦃ Storable A ⦄ → Ptr A → Int → IO (Ptr A)
    callocArray   : ⦃ Storable A ⦄ → Int → IO (Ptr A)
    callocArray0  : ⦃ Storable A ⦄ → Int → IO (Ptr A)
    
    peekArray  : ⦃ Storable A ⦄ → Int → Ptr A → IO (List A)
    peekArray0 : ⦃ Storable A ⦄ → ⦃ Eq A ⦄ → A → Ptr A → IO (List A)
    pokeArray  : ⦃ Storable A ⦄ → Ptr A → List A → IO ⊤
    pokeArray0 : ⦃ Storable A ⦄ → A → Ptr A → List A → IO ⊤

    newArray      : ⦃ Storable A ⦄ → List A → IO (Ptr A)
    newArray0     : ⦃ Storable A ⦄ → A → List A → IO (Ptr A)
    withArray     : ⦃ Storable A ⦄ → List A → (Ptr A → IO B) → IO B
    withArray0    : ⦃ Storable A ⦄ → A → List A → (Ptr A → IO B) → IO B
    withArrayLen  : ⦃ Storable A ⦄ → List A → (Int → Ptr A → IO B) → IO B
    withArrayLen0 : ⦃ Storable A ⦄ → A → List A → (Int → Ptr A → IO B) → IO B

    copyArray    : ⦃ Storable A ⦄ → Ptr A → Ptr A → Int → IO ⊤
    moveArray    : ⦃ Storable A ⦄ → Ptr A → Ptr A → Int → IO ⊤
    lengthArray0 : ⦃ Storable A ⦄ → ⦃ Eq A ⦄ → A → Ptr A → IO Int
    advancePtr   : ⦃ Storable A ⦄ → Ptr A → Int → Ptr A

{-# COMPILE GHC mallocArray   = \ aℓ a      AgdaStorable -> Foreign.Marshal.Array.mallocArray   #-}
{-# COMPILE GHC mallocArray0  = \ aℓ a      AgdaStorable -> Foreign.Marshal.Array.mallocArray   #-}
{-# COMPILE GHC allocaArray   = \ aℓ a bℓ b AgdaStorable -> Foreign.Marshal.Array.allocaArray   #-}
{-# COMPILE GHC allocaArray0  = \ aℓ a bℓ b AgdaStorable -> Foreign.Marshal.Array.allocaArray0  #-}
{-# COMPILE GHC reallocArray  = \ aℓ a      AgdaStorable -> Foreign.Marshal.Array.reallocArray  #-}
{-# COMPILE GHC reallocArray0 = \ aℓ a      AgdaStorable -> Foreign.Marshal.Array.reallocArray0 #-}
{-# COMPILE GHC callocArray   = \ aℓ a      AgdaStorable -> Foreign.Marshal.Array.callocArray   #-}
{-# COMPILE GHC callocArray0  = \ aℓ a      AgdaStorable -> Foreign.Marshal.Array.callocArray0  #-}

{-# COMPILE GHC peekArray   = \ aℓ a AgdaStorable        -> Foreign.Marshal.Array.peekArray   #-}
{-# COMPILE GHC peekArray0  = \ aℓ a AgdaStorable AgdaEq -> Foreign.Marshal.Array.peekArray0  #-}
{-# COMPILE GHC pokeArray   = \ aℓ a AgdaStorable        -> Foreign.Marshal.Array.pokeArray   #-}
{-# COMPILE GHC pokeArray0  = \ aℓ a AgdaStorable        -> Foreign.Marshal.Array.pokeArray0  #-}

{-# COMPILE GHC newArray      = \ aℓ a      AgdaStorable -> Foreign.Marshal.Array.newArray      #-}
{-# COMPILE GHC newArray0     = \ aℓ a      AgdaStorable -> Foreign.Marshal.Array.newArray0     #-}
{-# COMPILE GHC withArray     = \ aℓ a bℓ b AgdaStorable -> Foreign.Marshal.Array.withArray     #-}
{-# COMPILE GHC withArray0    = \ aℓ a bℓ b AgdaStorable -> Foreign.Marshal.Array.withArray0    #-}
{-# COMPILE GHC withArrayLen  = \ aℓ a bℓ b AgdaStorable -> Foreign.Marshal.Array.withArrayLen  #-}
{-# COMPILE GHC withArrayLen0 = \ aℓ a bℓ b AgdaStorable -> Foreign.Marshal.Array.withArrayLen0 #-}

{-# COMPILE GHC copyArray    = \ aℓ a AgdaStorable        -> Foreign.Marshal.Array.copyArray    #-}
{-# COMPILE GHC moveArray    = \ aℓ a AgdaStorable        -> Foreign.Marshal.Array.moveArray    #-}
{-# COMPILE GHC lengthArray0 = \ aℓ a AgdaStorable AgdaEq -> Foreign.Marshal.Array.lengthArray0 #-}
{-# COMPILE GHC advancePtr   = \ aℓ a AgdaStorable        -> Foreign.Marshal.Array.advancePtr   #-}
