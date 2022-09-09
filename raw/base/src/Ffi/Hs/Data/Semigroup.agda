{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Semigroup where

open import Agda.Primitive
open import Ffi.Hs.-base.Class         using (Integral)
open import Ffi.Hs.Data.List.NonEmpty  using (NonEmpty)
open import Ffi.Hs.Data.Monoid         using (Monoid; Endo)

open Ffi.Hs.-base.Class public
    using (Semigroup)

{-# FOREIGN GHC
import qualified Data.Semigroup
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Class
    ( AgdaSemigroup, AgdaIntegral, AgdaMonoid
    )
#-}

private
    variable
        aℓ bℓ : Level
        A : Set aℓ
        B : Set bℓ

infixr 6 _<>_

postulate
    _<>_      : ⦃ Semigroup A ⦄ → A → A → A
    sconcat   : ⦃ Semigroup A ⦄ → NonEmpty A → A
    stimes    : ⦃ Semigroup A ⦄ → ⦃ Integral B ⦄ → B → A → A

    stimesMonoid           : ⦃ Integral B ⦄ → ⦃ Monoid A ⦄ → B → A → A
    stimesIdempotent       : ⦃ Integral B ⦄ → B → A → A
    stimesIdempotentMonoid : ⦃ Integral B ⦄ → ⦃ Monoid A ⦄ → B → A → A
    mtimesDefault          : ⦃ Integral B ⦄ → ⦃ Monoid A ⦄ → B → A → A

    Min : Set aℓ → Set aℓ
    mkMin  : A → Min A
    getMin : Min A → A

    Max : Set aℓ → Set aℓ
    mkMax  : A → Max A
    getMax : Max A → A

    First : Set aℓ → Set aℓ
    mkFirst  : A → First A
    getFirst : First A → A

    Last : Set aℓ → Set aℓ
    mkLast  : A → Last A
    getLast : Last A → A

    diff   : ⦃ Semigroup A ⦄ → A → Endo A
    cycle1 : ⦃ Semigroup A ⦄ → A → A

{-# COMPILE GHC _<>_    = \ aℓ a AgdaSemigroup                   -> Data.Semigroup._<>_    #-}
{-# COMPILE GHC sconcat = \ aℓ a AgdaSemigroup                   -> Data.Semigroup.sconcat #-}
{-# COMPILE GHC stimes  = \ aℓ a bℓ b AgdaSemigroup AgdaIntegral -> Data.Semigroup.stimes  #-}

{-# COMPILE GHC stimesMonoid           = \ bℓ b aℓ a AgdaIntegral AgdaMonoid -> Data.Semigroup.stimesMonoid           #-}
{-# COMPILE GHC stimesIdempotent       = \ bℓ b aℓ a AgdaIntegral            -> Data.Semigroup.stimesIdempotent       #-}
{-# COMPILE GHC stimesIdempotentMonoid = \ bℓ b aℓ a AgdaIntegral AgdaMonoid -> Data.Semigroup.stimesIdempotentMonoid #-}
{-# COMPILE GHC mtimesDefault          = \ bℓ b aℓ a AgdaIntegral AgdaMonoid -> Data.Semigroup.mtimesDefault          #-}

{-# FOREIGN GHC type AgdaMin aℓ = Data.Semigroup.Min #-}
{-# COMPILE GHC Min = type(1) AgdaMin #-}
{-# COMPILE GHC mkMin  = \ aℓ a -> Data.Semigroup.mkMin  #-}
{-# COMPILE GHC getMin = \ aℓ a -> Data.Semigroup.getMin #-}

{-# FOREIGN GHC type AgdaMax aℓ = Data.Semigroup.Max #-}
{-# COMPILE GHC Max = type(1) AgdaMax #-}
{-# COMPILE GHC mkMax  = \ aℓ a -> Data.Semigroup.mkMax  #-}
{-# COMPILE GHC getMax = \ aℓ a -> Data.Semigroup.getMax #-}

{-# FOREIGN GHC type AgdaFirst aℓ = Data.Semigroup.First #-}
{-# COMPILE GHC First = type(1) AgdaFirst #-}
{-# COMPILE GHC mkFirst  = \ aℓ a -> Data.Semigroup.mkFirst  #-}
{-# COMPILE GHC getFirst = \ aℓ a -> Data.Semigroup.getFirst #-}

{-# FOREIGN GHC type AgdaLast aℓ = Data.Semigroup.Last #-}
{-# COMPILE GHC Last = type(1) AgdaLast #-}
{-# COMPILE GHC mkLast  = \ aℓ a -> Data.Semigroup.mkLast  #-}
{-# COMPILE GHC getLast = \ aℓ a -> Data.Semigroup.getLast #-}

{-# COMPILE GHC diff   = \ aℓ a AgdaSemigroup -> Data.Semigroup.diff   #-}
{-# COMPILE GHC cycle1 = \ aℓ a AgdaSemigroup -> Data.Semigroup.cycle1 #-}
