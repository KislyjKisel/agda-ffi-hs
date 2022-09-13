{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Semigroup where

open import Agda.Primitive
open import Ffi.Hs.-base.Class
open import Ffi.Hs.Data.List.NonEmpty  using (NonEmpty)
open import Ffi.Hs.Data.Monoid         using (Monoid; Endo)

open Ffi.Hs.-base.Class public
    using (Semigroup)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Data.Semigroup
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
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

{-# COMPILE GHC _<>_    = \ aℓ a AgdaSemigroup                   -> (Data.Semigroup.<>)    #-}
{-# COMPILE GHC sconcat = \ aℓ a AgdaSemigroup                   -> Data.Semigroup.sconcat #-}
{-# COMPILE GHC stimes  = \ aℓ a bℓ b AgdaSemigroup AgdaIntegral -> Data.Semigroup.stimes  #-}

{-# COMPILE GHC stimesMonoid           = \ bℓ b aℓ a AgdaIntegral AgdaMonoid -> Data.Semigroup.stimesMonoid           #-}
{-# COMPILE GHC stimesIdempotent       = \ bℓ b aℓ a AgdaIntegral            -> Data.Semigroup.stimesIdempotent       #-}
{-# COMPILE GHC stimesIdempotentMonoid = \ bℓ b aℓ a AgdaIntegral AgdaMonoid -> Data.Semigroup.stimesIdempotentMonoid #-}
{-# COMPILE GHC mtimesDefault          = \ bℓ b aℓ a AgdaIntegral AgdaMonoid -> Data.Semigroup.mtimesDefault          #-}

-- todo: Data.Semigroup non-opaque newtypes

{-# FOREIGN GHC type AgdaMin aℓ = Data.Semigroup.Min #-}
{-# COMPILE GHC Min = type(1) AgdaMin #-}
{-# COMPILE GHC mkMin  = \ aℓ a -> Data.Semigroup.Min  #-}
{-# COMPILE GHC getMin = \ aℓ a -> Data.Semigroup.getMin #-}

{-# FOREIGN GHC type AgdaMax aℓ = Data.Semigroup.Max #-}
{-# COMPILE GHC Max = type(1) AgdaMax #-}
{-# COMPILE GHC mkMax  = \ aℓ a -> Data.Semigroup.Max  #-}
{-# COMPILE GHC getMax = \ aℓ a -> Data.Semigroup.getMax #-}

{-# FOREIGN GHC type AgdaFirst aℓ = Data.Semigroup.First #-}
{-# COMPILE GHC First = type(1) AgdaFirst #-}
{-# COMPILE GHC mkFirst  = \ aℓ a -> Data.Semigroup.First  #-}
{-# COMPILE GHC getFirst = \ aℓ a -> Data.Semigroup.getFirst #-}

{-# FOREIGN GHC type AgdaLast aℓ = Data.Semigroup.Last #-}
{-# COMPILE GHC Last = type(1) AgdaLast #-}
{-# COMPILE GHC mkLast  = \ aℓ a -> Data.Semigroup.Last  #-}
{-# COMPILE GHC getLast = \ aℓ a -> Data.Semigroup.getLast #-}

{-# COMPILE GHC diff   = \ aℓ a AgdaSemigroup -> Data.Semigroup.diff   #-}
{-# COMPILE GHC cycle1 = \ aℓ a AgdaSemigroup -> Data.Semigroup.cycle1 #-}

record Arg (A : Set aℓ) (B : Set bℓ) : Set (aℓ ⊔ bℓ) where
    constructor mkArg
    field
        a : A
        b : B

{-# FOREIGN GHC type AgdaArg aℓ bℓ = Data.Semigroup.Arg #-}
{-# COMPILE GHC Arg = data(2) AgdaArg (Data.Semigroup.Arg) #-}

postulate
    Bifoldable[Arg]     : Bifoldable {aℓ} {bℓ} Arg
    Bifunctor[Arg]      : Bifunctor {aℓ} {bℓ} Arg
    Bitraversable[Arg]  : Bitraversable {aℓ} {bℓ} Arg
    Foldable[Arg[A]]    : Foldable {bℓ} (Arg A)
    Traversable[Arg[A]] : Traversable {bℓ} (Arg A)
    Functor[Arg[A]]     : Functor (Arg {aℓ} {aℓ} A)
    Data[Arg[A,B]]      : ⦃ Data A ⦄ → ⦃ Data B ⦄ → Data (Arg A B)
    Read[Arg[A,B]]      : ⦃ Read A ⦄ → ⦃ Read B ⦄ → Read (Arg A B)
    Show[Arg[A,B]]      : ⦃ Show A ⦄ → ⦃ Show B ⦄ → Show (Arg A B)
    Eq[Arg[A,B]]        : ⦃ Eq A ⦄ → Eq (Arg A B)
    Ord[Arg[A,B]]       : ⦃ Ord A ⦄ → Ord (Arg A B)

{-# COMPILE GHC Bifoldable[Arg]     = \ aℓ bℓ                       -> AgdaBifoldable    #-}
{-# COMPILE GHC Bifunctor[Arg]      = \ aℓ bℓ                       -> AgdaBifunctor     #-}
{-# COMPILE GHC Bitraversable[Arg]  = \ aℓ bℓ                       -> AgdaBitraversable #-}
{-# COMPILE GHC Foldable[Arg[A]]    = \ bℓ aℓ a                     -> AgdaFoldable      #-}
{-# COMPILE GHC Traversable[Arg[A]] = \ bℓ aℓ a                     -> AgdaTraversable   #-}
{-# COMPILE GHC Functor[Arg[A]]     = \ aℓ a                        -> AgdaFunctor       #-}
{-# COMPILE GHC Data[Arg[A,B]]      = \ aℓ a bℓ b AgdaData AgdaData -> AgdaData          #-}
{-# COMPILE GHC Read[Arg[A,B]]      = \ aℓ a bℓ b AgdaRead AgdaRead -> AgdaRead          #-}
{-# COMPILE GHC Show[Arg[A,B]]      = \ aℓ a bℓ b AgdaShow AgdaShow -> AgdaShow          #-}
{-# COMPILE GHC Eq[Arg[A,B]]        = \ aℓ a bℓ b AgdaEq            -> AgdaEq            #-}
{-# COMPILE GHC Ord[Arg[A,B]]       = \ aℓ a bℓ b AgdaOrd           -> AgdaOrd           #-}

ArgMin : Set aℓ → Set bℓ → Set (aℓ ⊔ bℓ)
ArgMin A B = Min (Arg A B)

ArgMax : Set aℓ → Set bℓ → Set (aℓ ⊔ bℓ)
ArgMax A B = Max (Arg A B)
