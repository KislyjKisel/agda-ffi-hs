{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Complex where

open import Agda.Primitive
open import Ffi.Hs.-base.Class
open import Ffi.Hs.Data.Tuple  using (Tuple2)

{-# FOREIGN GHC
import qualified Data.Complex
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ : Level
        A : Set aℓ

infix 6 _:+_

data Complex (A : Set aℓ) : Set aℓ where
    _:+_ : A → A → Complex A

{-# FOREIGN GHC type AgdaComplex aℓ = Data.Complex.Complex #-}
{-# COMPILE GHC Complex = data(1) AgdaComplex ((Data.Complex.:+)) #-}

postulate
    MonadFix[Complex]    : MonadFix {aℓ} Complex
    MonadZip[Complex]    : MonadZip {aℓ} Complex
    Foldable[Complex]    : Foldable {aℓ} Complex
    Traversable[Complex] : Traversable {aℓ} Complex
    Applicative[Complex] : Applicative {aℓ} Complex
    Functor[Complex]     : Functor {aℓ} Complex
    Monad[Complex]       : Monad {aℓ} Complex

    Data[Complex[A]]       : ⦃ Data A ⦄ → Data (Complex A)
    Storable[Complex[A]]   : ⦃ Storable A ⦄ → Storable (Complex A)
    Floating[Complex[A]]   : ⦃ RealFloat A ⦄ → Floating (Complex A)
    Num[Complex[A]]        : ⦃ RealFloat A ⦄ → Num (Complex A)
    Read[Complex[A]]       : ⦃ Read A ⦄ → Read (Complex A)
    Fractional[Complex[A]] : ⦃ RealFloat A ⦄ → Fractional (Complex A)
    Show[Complex[A]]       : ⦃ Show A ⦄ → Show (Complex A)
    Eq[Complex[A]]         : ⦃ Eq A ⦄ → Eq (Complex A)

{-# COMPILE GHC MonadFix[Complex]    = \ aℓ -> AgdaMonadFix    #-}
{-# COMPILE GHC MonadZip[Complex]    = \ aℓ -> AgdaMonadZip    #-}
{-# COMPILE GHC Foldable[Complex]    = \ aℓ -> AgdaFoldable    #-}
{-# COMPILE GHC Traversable[Complex] = \ aℓ -> AgdaTraversable #-}
{-# COMPILE GHC Applicative[Complex] = \ aℓ -> AgdaApplicative #-}
{-# COMPILE GHC Functor[Complex]     = \ aℓ -> AgdaFunctor     #-}
{-# COMPILE GHC Monad[Complex]       = \ aℓ -> AgdaMonad       #-}

{-# COMPILE GHC Data[Complex[A]]       = \ aℓ a AgdaData      -> AgdaData       #-}
{-# COMPILE GHC Storable[Complex[A]]   = \ aℓ a AgdaStorable  -> AgdaStorable   #-}
{-# COMPILE GHC Floating[Complex[A]]   = \ aℓ a AgdaRealFloat -> AgdaFloating   #-}
{-# COMPILE GHC Num[Complex[A]]        = \ aℓ a AgdaRealFloat -> AgdaNum        #-}
{-# COMPILE GHC Read[Complex[A]]       = \ aℓ a AgdaRead      -> AgdaRead       #-}
{-# COMPILE GHC Fractional[Complex[A]] = \ aℓ a AgdaRealFloat -> AgdaFractional #-}
{-# COMPILE GHC Show[Complex[A]]       = \ aℓ a AgdaShow      -> AgdaShow       #-}
{-# COMPILE GHC Eq[Complex[A]]         = \ aℓ a AgdaEq        -> AgdaEq         #-}

realPart : Complex A → A
realPart (x :+ _) = x

imagPart : Complex A → A
imagPart (_ :+ x) = x

postulate
    mkPolar   : ⦃ Floating A ⦄ → A → A → Complex A
    cis       : ⦃ Floating A ⦄ → A → Complex A
    polar     : ⦃ RealFloat A ⦄ → Complex A → Tuple2 A A
    magnitude : ⦃ RealFloat A ⦄ → Complex A → A
    phase     : ⦃ RealFloat A ⦄ → Complex A → A
    conjugate : ⦃ Num A ⦄ → Complex A → Complex A

{-# COMPILE GHC mkPolar   = \ aℓ a AgdaFloating  -> Data.Complex.mkPolar   #-}
{-# COMPILE GHC cis       = \ aℓ a AgdaFloating  -> Data.Complex.cis       #-}
{-# COMPILE GHC polar     = \ aℓ a AgdaRealFloat -> Data.Complex.polar     #-}
{-# COMPILE GHC magnitude = \ aℓ a AgdaRealFloat -> Data.Complex.magnitude #-}
{-# COMPILE GHC phase     = \ aℓ a AgdaRealFloat -> Data.Complex.phase     #-}
{-# COMPILE GHC conjugate = \ aℓ a AgdaNum       -> Data.Complex.conjugate #-}
