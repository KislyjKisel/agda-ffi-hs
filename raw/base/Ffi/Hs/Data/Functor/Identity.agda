{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Functor.Identity where

open import Agda.Primitive
open import Ffi.Hs.-base.Class

{-# FOREIGN GHC
import qualified Data.Functor.Identity
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ : Level
        A : Set aℓ

record Identity (A : Set aℓ) : Set aℓ where
    constructor mkIdentity
    field
        runIdentity : A

open Identity public

{-# FOREIGN GHC type AgdaIdentity aℓ = Data.Functor.Identity.Identity #-}
{-# COMPILE GHC Identity = data(1) AgdaIdentity (Data.Functor.Identity.Identity) #-}

postulate
    MonadFix[Identity]      : MonadFix {aℓ} Identity
    MonadZip[Identity]      : MonadZip {aℓ} Identity
    Foldable[Identity]      : Foldable {aℓ} Identity
    Traversable[Identity]   : Traversable {aℓ} Identity
    Applicative[Identity]   : Applicative {aℓ} Identity
    Functor[Identity]       : Functor {aℓ} Identity
    Monad[Identity]         : Monad {aℓ} Identity
    Data[Identity[A]]       : ⦃ Data A ⦄ → Data (Identity A)
    IsString[Identity[A]]   : ⦃ IsString A ⦄ → IsString (Identity A)
    Storable[Identity[A]]   : ⦃ Storable A ⦄ → Storable (Identity A)
    Monoid[Identity[A]]     : ⦃ Monoid A ⦄ → Monoid (Identity A)
    Semigroup[Identity[A]]  : ⦃ Semigroup A ⦄ → Semigroup (Identity A)
    Bits[Identity[A]]       : ⦃ Bits A ⦄ → Bits (Identity A)
    FiniteBits[Identity[A]] : ⦃ FiniteBits A ⦄ → FiniteBits (Identity A)
    Bounded[Identity[A]]    : ⦃ Bounded A ⦄ → Bounded (Identity A)
    Enum[Identity[A]]       : ⦃ Enum A ⦄ → Enum (Identity A)
    Floating[Identity[A]]   : ⦃ Floating A ⦄ → Floating (Identity A)
    RealFloat[Identity[A]]  : ⦃ RealFloat A ⦄ → RealFloat (Identity A)
    Ix[Identity[A]]         : ⦃ Ix A ⦄ → Ix (Identity A)
    Num[Identity[A]]        : ⦃ Num A ⦄ → Num (Identity A)
    Read[Identity[A]]       : ⦃ Read A ⦄ → Read (Identity A)
    Fractional[Identity[A]] : ⦃ Fractional A ⦄ → Fractional (Identity A)
    Integral[Identity[A]]   : ⦃ Integral A ⦄ → Integral (Identity A)
    Real[Identity[A]]       : ⦃ Real A ⦄ → Real (Identity A)
    RealFrac[Identity[A]]   : ⦃ RealFrac A ⦄ → RealFrac (Identity A)
    Show[Identity[A]]       : ⦃ Show A ⦄ → Show (Identity A)
    Eq[Identity[A]]         : ⦃ Eq A ⦄ → Eq (Identity A)
    Ord[Identity[A]]        : ⦃ Ord A ⦄ → Ord (Identity A)

{-# COMPILE GHC MonadFix[Identity]      = \ aℓ                  -> AgdaMonadFix    #-}
{-# COMPILE GHC MonadZip[Identity]      = \ aℓ                  -> AgdaMonadZip    #-}
{-# COMPILE GHC Foldable[Identity]      = \ aℓ                  -> AgdaFoldable    #-}
{-# COMPILE GHC Traversable[Identity]   = \ aℓ                  -> AgdaTraversable #-}
{-# COMPILE GHC Applicative[Identity]   = \ aℓ                  -> AgdaApplicative #-}
{-# COMPILE GHC Functor[Identity]       = \ aℓ                  -> AgdaFunctor     #-}
{-# COMPILE GHC Monad[Identity]         = \ aℓ                  -> AgdaMonad       #-}
{-# COMPILE GHC Data[Identity[A]]       = \ aℓ a AgdaData       -> AgdaData        #-}
{-# COMPILE GHC IsString[Identity[A]]   = \ aℓ a AgdaIsString   -> AgdaIsString    #-}
{-# COMPILE GHC Storable[Identity[A]]   = \ aℓ a AgdaStorable   -> AgdaStorable    #-}
{-# COMPILE GHC Monoid[Identity[A]]     = \ aℓ a AgdaMonoid     -> AgdaMonoid      #-}
{-# COMPILE GHC Semigroup[Identity[A]]  = \ aℓ a AgdaSemigroup  -> AgdaSemigroup   #-}
{-# COMPILE GHC Bits[Identity[A]]       = \ aℓ a AgdaBits       -> AgdaBits        #-}
{-# COMPILE GHC FiniteBits[Identity[A]] = \ aℓ a AgdaFiniteBits -> AgdaFiniteBits  #-}
{-# COMPILE GHC Bounded[Identity[A]]    = \ aℓ a AgdaBounded    -> AgdaBounded     #-}
{-# COMPILE GHC Enum[Identity[A]]       = \ aℓ a AgdaEnum       -> AgdaEnum        #-}
{-# COMPILE GHC Floating[Identity[A]]   = \ aℓ a AgdaFloating   -> AgdaFloating    #-}
{-# COMPILE GHC RealFloat[Identity[A]]  = \ aℓ a AgdaRealFloat  -> AgdaRealFloat   #-}
{-# COMPILE GHC Ix[Identity[A]]         = \ aℓ a AgdaIx         -> AgdaIx          #-}
{-# COMPILE GHC Num[Identity[A]]        = \ aℓ a AgdaNum        -> AgdaNum         #-}
{-# COMPILE GHC Read[Identity[A]]       = \ aℓ a AgdaRead       -> AgdaRead        #-}
{-# COMPILE GHC Fractional[Identity[A]] = \ aℓ a AgdaFractional -> AgdaFractional  #-}
{-# COMPILE GHC Integral[Identity[A]]   = \ aℓ a AgdaIntegral   -> AgdaIntegral    #-}
{-# COMPILE GHC Real[Identity[A]]       = \ aℓ a AgdaReal       -> AgdaReal        #-}
{-# COMPILE GHC RealFrac[Identity[A]]   = \ aℓ a AgdaRealFrac   -> AgdaRealFrac    #-}
{-# COMPILE GHC Show[Identity[A]]       = \ aℓ a AgdaShow       -> AgdaShow        #-}
{-# COMPILE GHC Eq[Identity[A]]         = \ aℓ a AgdaEq         -> AgdaEq          #-}
{-# COMPILE GHC Ord[Identity[A]]        = \ aℓ a AgdaOrd        -> AgdaOrd         #-}
