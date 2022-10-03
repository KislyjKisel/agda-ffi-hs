{-# OPTIONS --without-K #-}

module Ffi.Hs.GHC.TypeNats where

open import Ffi.Hs.GHC.Exts using (Proxy#)
open import Agda.Primitive

open import Ffi.Hs.Numeric.Natural public
    using (Natural)

{-# FOREIGN GHC {-# LANGUAGE DataKinds #-} #-}
{-# FOREIGN GHC
import qualified GHC.TypeNats
#-}

private
    variable
        pℓ : Level
        N : Set


Nat = Natural

postulate
    `Nat : Set → Set

{-# FOREIGN GHC data Agda'98'Nat (a :: GHC.TypeNats.Nat) = Agda'98'Nat #-}
{-# COMPILE GHC `Nat = type(0) Agda'98'Nat #-}

postulate
    KnownNat : (N : Set) → ⦃ `Nat N ⦄ → Set₁

    natVal  : ∀{pℓ} {Proxy : Set → Set pℓ} → ⦃ _ : `Nat N ⦄ → ⦃ KnownNat N ⦄ → Proxy N → Natural
    natVal' : ⦃ _ : `Nat N ⦄ → ⦃ KnownNat N ⦄ → Proxy# N → Natural

-- todo: SomeNat, someNatVal

{-# FOREIGN GHC data AgdaKnownNat n _ = GHC.TypeNats.KnownNat n => AgdaKnownNat #-}
{-# COMPILE GHC KnownNat = type(0) AgdaKnownNat #-}

{-# COMPILE GHC natVal  = \ n pℓ proxy Agda'98'Nat AgdaKnownNat -> GHC.TypeNats.natVal  #-}
{-# COMPILE GHC natVal' = \ n Agda'98'Nat AgdaKnownNat          -> GHC.TypeNats.natVal' #-}

postulate
    0# 1# 2# 3# 4# 5# : Set
    `Nat[0#] : `Nat 0#
    `Nat[1#] : `Nat 1#
    `Nat[2#] : `Nat 2#
    `Nat[3#] : `Nat 3#
    `Nat[4#] : `Nat 4#
    `Nat[5#] : `Nat 5#

{-# COMPILE GHC 0# = type 0 #-}
{-# COMPILE GHC 0# = type 1 #-}
{-# COMPILE GHC 0# = type 2 #-}
{-# COMPILE GHC 0# = type 3 #-}
{-# COMPILE GHC 0# = type 4 #-}
{-# COMPILE GHC 0# = type 5 #-}

{-# COMPILE GHC `Nat[0#] = Agda'98'Nat #-}
{-# COMPILE GHC `Nat[1#] = Agda'98'Nat #-}
{-# COMPILE GHC `Nat[2#] = Agda'98'Nat #-}
{-# COMPILE GHC `Nat[3#] = Agda'98'Nat #-}
{-# COMPILE GHC `Nat[4#] = Agda'98'Nat #-}
{-# COMPILE GHC `Nat[5#] = Agda'98'Nat #-}
