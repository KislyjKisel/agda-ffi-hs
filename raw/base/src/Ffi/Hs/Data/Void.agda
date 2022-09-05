{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Void where

open import Agda.Primitive
open import Ffi.Hs.-base.Class

{-# FOREIGN GHC
import qualified Data.Void
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Class
    ( AgdaSemigroup, AgdaException, AgdaIx, AgdaRead
    , AgdaShow, AgdaEq, AgdaOrd, AgdaFunctor
    )
#-}

private
    variable
        aℓ fℓ : Level
        A : Set aℓ
        F : Set fℓ → Set fℓ

data Void : Set where

{-# COMPILE GHC Void = data Data.Void.Void () #-}

absurd : Void → A
absurd ()

postulate
    vacuous : ⦃ Functor F ⦄ → F Void → F A

{-# COMPILE GHC vacuous = \ fℓ f a AgdaFunctor -> Data.Void.vacuous #-}

postulate
    Semigroup[Void] : Semigroup Void
    Exception[Void] : Exception Void
    Ix[Void]        : Ix Void
    Read[Void]      : Read Void
    Show[Void]      : Show Void
    Eq[Void]        : Eq Void
    Ord[Void]       : Ord Void
    -- todo: Data, Generic, Rep

{-# COMPILE GHC Semigroup[Void] = AgdaSemigroup #-}
{-# COMPILE GHC Exception[Void] = AgdaException #-}
{-# COMPILE GHC Ix[Void]        = AgdaIx        #-}
{-# COMPILE GHC Read[Void]      = AgdaRead      #-}
{-# COMPILE GHC Show[Void]      = AgdaShow      #-}
{-# COMPILE GHC Eq[Void]        = AgdaEq        #-}
{-# COMPILE GHC Ord[Void]       = AgdaOrd       #-}
