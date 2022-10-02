{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Functor.Const where

open import Agda.Primitive
open import Ffi.Hs.-base.Class

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Data.Functor.Const
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ : Level
        A B : Set aℓ

record Const (A : Set aℓ) (B : Set aℓ) : Set aℓ where
    constructor mkConst
    field
        getConst : A

{-# FOREIGN GHC type AgdaConst aℓ = Data.Functor.Const.Const #-}
{-# COMPILE GHC Const = data(1) AgdaConst (Data.Functor.Const.Const) #-}

postulate
    Bifoldable[Const]       : Bifoldable (Const {aℓ})
    Bifunctor[Const]        : Bifunctor (Const {aℓ})
    Bitraversable[Const]    : Bitraversable (Const {aℓ})
    Eq2[Const]              : Eq2 (Const {aℓ})
    Ord2[Const]             : Ord2 (Const {aℓ})
    Read2[Const]            : Read2 (Const {aℓ})
    Show2[Const]            : Show2 (Const {aℓ})
    Foldable[Const[A]]      : Foldable (Const A)
    Eq1[Const[A]]           : ⦃ Eq A ⦄ → Eq1 (Const A)
    Ord1[Const[A]]          : ⦃ Ord A ⦄ → Ord1 (Const A)
    Read1[Const[A]]         : ⦃ Read A ⦄ → Read1 (Const A)
    Show1[Const[A]]         : ⦃ Show A ⦄ → Show1 (Const A)
    Contravariant[Const[A]] : Contravariant (Const A)
    Traversable[Const[A]]   : Traversable (Const A)
    Applicative[Const[A]]   : ⦃ Monoid A ⦄ → Applicative (Const A)
    Functor[Const[A]]       : Functor (Const A)
    -- todo: Data instance - Typeable kind
    -- Data[Const[A,B]]        : ⦃ Data A ⦄ → ⦃ Typeable B ⦄ → Data (Const A B)
    IsString[Const[A,B]]    : ⦃ IsString A ⦄   → IsString (Const A B)
    Storable[Const[A,B]]    : ⦃ Storable A ⦄   → Storable (Const A B)
    Monoid[Const[A,B]]      : ⦃ Monoid A ⦄     → Monoid (Const A B)
    Semigroup[Const[A,B]]   : ⦃ Semigroup A ⦄  → Semigroup (Const A B)
    Bits[Const[A,B]]        : ⦃ Bits A ⦄       → Bits (Const A B)
    FiniteBits[Const[A,B]]  : ⦃ FiniteBits A ⦄ → FiniteBits (Const A B)
    Bounded[Const[A,B]]     : ⦃ Bounded A ⦄    → Bounded (Const A B)
    Enum[Const[A,B]]        : ⦃ Enum A ⦄       → Enum (Const A B)
    Floating[Const[A,B]]    : ⦃ Floating A ⦄   → Floating (Const A B)
    RealFloat[Const[A,B]]   : ⦃ RealFloat A ⦄  → RealFloat (Const A B)
    Ix[Const[A,B]]          : ⦃ Ix A ⦄         → Ix (Const A B)
    Num[Const[A,B]]         : ⦃ Num A ⦄        → Num (Const A B)
    Read[Const[A,B]]        : ⦃ Read A ⦄       → Read (Const A B)
    Fractional[Const[A,B]]  : ⦃ Fractional A ⦄ → Fractional (Const A B)
    Integral[Const[A,B]]    : ⦃ Integral A ⦄   → Integral (Const A B)
    Real[Const[A,B]]        : ⦃ Real A ⦄       → Real (Const A B)
    RealFrac[Const[A,B]]    : ⦃ RealFrac A ⦄   → RealFrac (Const A B)
    Show[Const[A,B]]        : ⦃ Show A ⦄       → Show (Const A B)
    Eq[Const[A,B]]          : ⦃ Eq A ⦄         → Eq (Const A B)
    Ord[Const[A,B]]         : ⦃ Ord A ⦄        → Ord (Const A B)

{-# COMPILE GHC Bifoldable[Const]       = \ aℓ                           -> AgdaBifoldable    #-}
{-# COMPILE GHC Bifunctor[Const]        = \ aℓ                           -> AgdaBifunctor     #-}
{-# COMPILE GHC Bitraversable[Const]    = \ aℓ                           -> AgdaBitraversable #-}
{-# COMPILE GHC Eq2[Const]              = \ aℓ                           -> AgdaEq2           #-}
{-# COMPILE GHC Ord2[Const]             = \ aℓ                           -> AgdaOrd2          #-}
{-# COMPILE GHC Read2[Const]            = \ aℓ                           -> AgdaRead2         #-}
{-# COMPILE GHC Show2[Const]            = \ aℓ                           -> AgdaShow2         #-}
{-# COMPILE GHC Foldable[Const[A]]      = \ aℓ a                         -> AgdaFoldable      #-}
{-# COMPILE GHC Eq1[Const[A]]           = \ aℓ a AgdaEq                  -> AgdaEq1           #-}
{-# COMPILE GHC Ord1[Const[A]]          = \ aℓ a AgdaOrd                 -> AgdaOrd1          #-}
{-# COMPILE GHC Read1[Const[A]]         = \ aℓ a AgdaRead                -> AgdaRead1         #-}
{-# COMPILE GHC Show1[Const[A]]         = \ aℓ a AgdaShow                -> AgdaShow1         #-}
{-# COMPILE GHC Contravariant[Const[A]] = \ aℓ a                         -> AgdaContravariant #-}
{-# COMPILE GHC Traversable[Const[A]]   = \ aℓ a                         -> AgdaTraversable   #-}
{-# COMPILE GHC Applicative[Const[A]]   = \ aℓ a AgdaMonoid              -> AgdaApplicative   #-}
{-# COMPILE GHC Functor[Const[A]]       = \ aℓ a                         -> AgdaFunctor       #-}
{-# COMPILE GHC IsString[Const[A,B]]    = \ aℓ a b AgdaIsString          -> AgdaIsString      #-}
{-# COMPILE GHC Storable[Const[A,B]]    = \ aℓ a b AgdaStorable          -> AgdaStorable      #-}
{-# COMPILE GHC Monoid[Const[A,B]]      = \ aℓ a b AgdaMonoid            -> AgdaMonoid        #-}
{-# COMPILE GHC Semigroup[Const[A,B]]   = \ aℓ a b AgdaSemigroup         -> AgdaSemigroup     #-}
{-# COMPILE GHC Bits[Const[A,B]]        = \ aℓ a b AgdaBits              -> AgdaBits          #-}
{-# COMPILE GHC FiniteBits[Const[A,B]]  = \ aℓ a b AgdaFiniteBits        -> AgdaFiniteBits    #-}
{-# COMPILE GHC Bounded[Const[A,B]]     = \ aℓ a b AgdaBounded           -> AgdaBounded       #-}
{-# COMPILE GHC Enum[Const[A,B]]        = \ aℓ a b AgdaEnum              -> AgdaEnum          #-}
{-# COMPILE GHC Floating[Const[A,B]]    = \ aℓ a b AgdaFloating          -> AgdaFloating      #-}
{-# COMPILE GHC RealFloat[Const[A,B]]   = \ aℓ a b AgdaRealFloat         -> AgdaRealFloat     #-}
{-# COMPILE GHC Ix[Const[A,B]]          = \ aℓ a b AgdaIx                -> AgdaIx            #-}
{-# COMPILE GHC Num[Const[A,B]]         = \ aℓ a b AgdaNum               -> AgdaNum           #-}
{-# COMPILE GHC Read[Const[A,B]]        = \ aℓ a b AgdaRead              -> AgdaRead          #-}
{-# COMPILE GHC Fractional[Const[A,B]]  = \ aℓ a b AgdaFractional        -> AgdaFractional    #-}
{-# COMPILE GHC Integral[Const[A,B]]    = \ aℓ a b AgdaIntegral          -> AgdaIntegral      #-}
{-# COMPILE GHC Real[Const[A,B]]        = \ aℓ a b AgdaReal              -> AgdaReal          #-}
{-# COMPILE GHC RealFrac[Const[A,B]]    = \ aℓ a b AgdaRealFrac          -> AgdaRealFrac      #-}
{-# COMPILE GHC Show[Const[A,B]]        = \ aℓ a b AgdaShow              -> AgdaShow          #-}
{-# COMPILE GHC Eq[Const[A,B]]          = \ aℓ a b AgdaEq                -> AgdaEq            #-}
{-# COMPILE GHC Ord[Const[A,B]]         = \ aℓ a b AgdaOrd               -> AgdaOrd           #-}
