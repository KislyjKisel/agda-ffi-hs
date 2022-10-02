{-# OPTIONS --without-K #-}

module Ffi.Hs.-base.Class where

open import Agda.Primitive
open import Ffi.Hs.-base.Kind using (IsKind)

{-# FOREIGN GHC
import qualified GHC.Num
import qualified GHC.Real
import qualified GHC.Enum
import qualified GHC.Float
import qualified Data.Ix
import qualified Data.Eq
import qualified Data.Ord
import qualified Data.Bits
import qualified Text.Read
import qualified Text.Show
import qualified Data.Functor
import qualified Data.Functor.Contravariant
import qualified Data.Functor.Classes
import qualified Control.Applicative
import qualified Control.Monad
import qualified Control.Monad.Fix
import qualified Control.Monad.Zip
import qualified Control.Monad.IO.Class
import qualified Control.Category
import qualified Control.Arrow
import qualified Data.Bifunctor
import qualified Data.Bifoldable
import qualified Data.Bitraversable
import qualified Foreign.Storable
import qualified Control.Exception.Base
import qualified Data.Data
import qualified Type.Reflection
import qualified Data.Type.Equality
import qualified Data.Type.Coercion
import qualified Data.Coerce
import qualified Data.String
import qualified Data.Semigroup
import qualified Data.Monoid
import qualified Data.Foldable
import qualified Data.Traversable
#-}

private
    variable
        aℓ bℓ cℓ : Level

postulate
    Num        : Set aℓ → Set aℓ
    Integral   : Set aℓ → Set aℓ
    Real       : Set aℓ → Set aℓ
    Fractional : Set aℓ → Set aℓ
    RealFrac   : Set aℓ → Set aℓ
    Floating   : Set aℓ → Set aℓ
    RealFloat  : Set aℓ → Set aℓ

    Bits       : Set aℓ → Set aℓ
    FiniteBits : Set aℓ → Set aℓ

    Read : Set aℓ → Set aℓ
    Show : Set aℓ → Set aℓ

    Bounded : Set aℓ → Set aℓ
    Enum    : Set aℓ → Set aℓ

    Eq  : Set aℓ → Set aℓ
    Ord : Set aℓ → Set aℓ
    Ix  : Set aℓ → Set aℓ

    Semigroup : Set aℓ → Set aℓ
    Monoid    : Set aℓ → Set aℓ

    Foldable    : (Set aℓ → Set bℓ) → Set (aℓ ⊔ bℓ)
    Traversable : (Set aℓ → Set aℓ) → Set aℓ

    Functor       : (Set aℓ → Set aℓ) → Set aℓ
    Contravariant : (Set aℓ → Set aℓ) → Set aℓ
    Applicative   : (Set aℓ → Set aℓ) → Set aℓ
    Alternative   : (Set aℓ → Set aℓ) → Set aℓ
    Monad         : (Set aℓ → Set aℓ) → Set aℓ
    MonadPlus     : (Set aℓ → Set aℓ) → Set aℓ
    MonadFail     : (Set aℓ → Set aℓ) → Set aℓ
    MonadFix      : (Set aℓ → Set aℓ) → Set aℓ
    MonadZip      : (Set aℓ → Set aℓ) → Set aℓ
    MonadIO       : (Set aℓ → Set aℓ) → Set aℓ

    Category    : (Set aℓ → Set bℓ → Set cℓ) → Set (aℓ ⊔ bℓ ⊔ cℓ)
    Arrow       : (Set aℓ → Set bℓ → Set cℓ) → Set (aℓ ⊔ bℓ ⊔ cℓ)
    ArrowZero   : (Set aℓ → Set bℓ → Set cℓ) → Set (aℓ ⊔ bℓ ⊔ cℓ)
    ArrowChoice : (Set aℓ → Set bℓ → Set cℓ) → Set (aℓ ⊔ bℓ ⊔ cℓ)
    ArrowApply  : (Set aℓ → Set bℓ → Set cℓ) → Set (aℓ ⊔ bℓ ⊔ cℓ)
    ArrowPlus   : (Set aℓ → Set bℓ → Set cℓ) → Set (aℓ ⊔ bℓ ⊔ cℓ)
    ArrowLoop   : (Set aℓ → Set bℓ → Set cℓ) → Set (aℓ ⊔ bℓ ⊔ cℓ)

    Bifunctor     : (Set aℓ → Set bℓ → Set cℓ) → Set (aℓ ⊔ bℓ ⊔ cℓ)
    Bifoldable    : (Set aℓ → Set bℓ → Set cℓ) → Set (aℓ ⊔ bℓ ⊔ cℓ)
    Bitraversable : (Set aℓ → Set aℓ → Set aℓ) → Set aℓ

    Storable  : Set aℓ → Set aℓ
    Exception : Set aℓ → Set aℓ

    Data         : {K : Set (lsuc aℓ)} → K → Set aℓ
    Typeable     : {K : Set (lsuc aℓ)} → K → Set aℓ
    TestEquality : (Set aℓ → Set bℓ) → Set (aℓ ⊔ bℓ)
    TestCoercion : {K₁ : Set (lsuc aℓ)} {K₂ : Set (lsuc bℓ)} → (K₁ → K₂) → Set (aℓ ⊔ bℓ)
    Coercible    : Set aℓ → Set bℓ → Set (aℓ ⊔ bℓ)

    IsString : Set aℓ → Set aℓ

    Eq1   : (Set aℓ → Set bℓ) → Set (aℓ ⊔ bℓ)
    Ord1  : (Set aℓ → Set bℓ) → Set (aℓ ⊔ bℓ)
    Read1 : (Set aℓ → Set bℓ) → Set (aℓ ⊔ bℓ)
    Show1 : (Set aℓ → Set bℓ) → Set (aℓ ⊔ bℓ)
    Eq2   : (Set aℓ → Set bℓ → Set cℓ) → Set (aℓ ⊔ bℓ ⊔ cℓ)
    Ord2  : (Set aℓ → Set bℓ → Set cℓ) → Set (aℓ ⊔ bℓ ⊔ cℓ)
    Read2 : (Set aℓ → Set bℓ → Set cℓ) → Set (aℓ ⊔ bℓ ⊔ cℓ)
    Show2 : (Set aℓ → Set bℓ → Set cℓ) → Set (aℓ ⊔ bℓ ⊔ cℓ)

{-# FOREIGN GHC
data AgdaNum aℓ a        = GHC.Num.Num a         => AgdaNum
data AgdaIntegral aℓ a   = GHC.Real.Integral a   => AgdaIntegral
data AgdaReal aℓ a       = GHC.Real.Real a       => AgdaReal
data AgdaFractional aℓ a = GHC.Real.Fractional a => AgdaFractional
data AgdaRealFrac aℓ a   = GHC.Real.RealFrac a   => AgdaRealFrac
data AgdaFloating aℓ a   = GHC.Float.Floating a  => AgdaFloating
data AgdaRealFloat aℓ a  = GHC.Float.RealFloat a => AgdaRealFloat
#-}
{-# COMPILE GHC Num        = type(0) AgdaNum        #-}
{-# COMPILE GHC Integral   = type(0) AgdaIntegral   #-}
{-# COMPILE GHC Real       = type(0) AgdaReal       #-}
{-# COMPILE GHC Fractional = type(0) AgdaFractional #-}
{-# COMPILE GHC RealFrac   = type(0) AgdaRealFrac   #-}
{-# COMPILE GHC Floating   = type(0) AgdaFloating   #-}
{-# COMPILE GHC RealFloat  = type(0) AgdaRealFloat  #-}

{-# FOREIGN GHC
data AgdaBits aℓ a       = Data.Bits.Bits a       => AgdaBits
data AgdaFiniteBits aℓ a = Data.Bits.FiniteBits a => AgdaFiniteBits
#-}
{-# COMPILE GHC Bits       = type(0) AgdaBits       #-}
{-# COMPILE GHC FiniteBits = type(0) AgdaFiniteBits #-}

{-# FOREIGN GHC
data AgdaRead aℓ a = Text.Read.Read a => AgdaRead
data AgdaShow aℓ a = Text.Show.Show a => AgdaShow
#-}
{-# COMPILE GHC Read = type(0) AgdaRead #-}
{-# COMPILE GHC Show = type(0) AgdaShow #-}

{-# FOREIGN GHC
data AgdaBounded aℓ a = GHC.Enum.Bounded a => AgdaBounded
data AgdaEnum aℓ a    = GHC.Enum.Enum a    => AgdaEnum
data AgdaIx aℓ a      = Data.Ix.Ix a       => AgdaIx
data AgdaEq aℓ a      = Data.Eq.Eq a       => AgdaEq
data AgdaOrd aℓ a     = Data.Ord.Ord a     => AgdaOrd
#-}
{-# COMPILE GHC Bounded = type(0) AgdaBounded #-}
{-# COMPILE GHC Enum    = type(0) AgdaEnum    #-}
{-# COMPILE GHC Ix      = type(0) AgdaIx      #-}
{-# COMPILE GHC Eq      = type(0) AgdaEq      #-}
{-# COMPILE GHC Ord     = type(0) AgdaOrd     #-}

{-# FOREIGN GHC
data AgdaSemigroup aℓ a = Data.Semigroup.Semigroup a => AgdaSemigroup
data AgdaMonoid aℓ a    = Data.Monoid.Monoid a       => AgdaMonoid
#-}
{-# COMPILE GHC Semigroup = type(0) AgdaSemigroup #-}
{-# COMPILE GHC Monoid    = type(0) AgdaMonoid    #-}

{-# FOREIGN GHC
data AgdaFoldable aℓ bℓ f = Data.Foldable.Foldable f       => AgdaFoldable
data AgdaTraversable aℓ f = Data.Traversable.Traversable f => AgdaTraversable
#-}
{-# COMPILE GHC Foldable    = type(0) AgdaFoldable    #-}
{-# COMPILE GHC Traversable = type(0) AgdaTraversable #-}

{-# FOREIGN GHC
data AgdaFunctor fℓ f       = Data.Functor.Functor f                     => AgdaFunctor
data AgdaContravariant fℓ f = Data.Functor.Contravariant.Contravariant f => AgdaContravariant
data AgdaApplicative fℓ f   = Control.Applicative.Applicative f          => AgdaApplicative
data AgdaAlternative fℓ f   = Control.Applicative.Alternative f          => AgdaAlternative
data AgdaMonad fℓ f         = Control.Monad.Monad f                      => AgdaMonad
data AgdaMonadPlus fℓ f     = Control.Monad.MonadPlus f                  => AgdaMonadPlus
data AgdaMonadFail fℓ f     = Control.Monad.MonadFail f                  => AgdaMonadFail
data AgdaMonadFix fℓ f      = Control.Monad.Fix.MonadFix f               => AgdaMonadFix
data AgdaMonadZip fℓ f      = Control.Monad.Zip.MonadZip f               => AgdaMonadZip
data AgdaMonadIO fℓ f       = Control.Monad.IO.Class.MonadIO f           => AgdaMonadIO
#-}
{-# COMPILE GHC Functor       = type(0) AgdaFunctor       #-}
{-# COMPILE GHC Contravariant = type(0) AgdaContravariant #-}
{-# COMPILE GHC Applicative   = type(0) AgdaApplicative   #-}
{-# COMPILE GHC Alternative   = type(0) AgdaAlternative   #-}
{-# COMPILE GHC Monad         = type(0) AgdaMonad         #-}
{-# COMPILE GHC MonadPlus     = type(0) AgdaMonadPlus     #-}
{-# COMPILE GHC MonadFail     = type(0) AgdaMonadFail     #-}
{-# COMPILE GHC MonadFix      = type(0) AgdaMonadFix      #-}
{-# COMPILE GHC MonadZip      = type(0) AgdaMonadZip      #-}
{-# COMPILE GHC MonadIO       = type(0) AgdaMonadIO       #-}

{-# FOREIGN GHC
data AgdaCategory aℓ bℓ cℓ a    = Control.Category.Category a => AgdaCategory
data AgdaArrow aℓ bℓ cℓ a       = Control.Arrow.Arrow a       => AgdaArrow
data AgdaArrowZero aℓ bℓ cℓ a   = Control.Arrow.ArrowZero a   => AgdaArrowZero
data AgdaArrowChoice aℓ bℓ cℓ a = Control.Arrow.ArrowChoice a => AgdaArrowChoice
data AgdaArrowApply aℓ bℓ cℓ a  = Control.Arrow.ArrowApply  a => AgdaArrowApply
data AgdaArrowPlus aℓ bℓ cℓ a   = Control.Arrow.ArrowPlus a   => AgdaArrowPlus
data AgdaArrowLoop aℓ bℓ cℓ a   = Control.Arrow.ArrowLoop a   => AgdaArrowLoop
#-}
{-# COMPILE GHC Category    = type(0) AgdaCategory    #-}
{-# COMPILE GHC Arrow       = type(0) AgdaArrow       #-}
{-# COMPILE GHC ArrowZero   = type(0) AgdaArrowZero   #-}
{-# COMPILE GHC ArrowChoice = type(0) AgdaArrowChoice #-}
{-# COMPILE GHC ArrowApply  = type(0) AgdaArrowApply  #-}
{-# COMPILE GHC ArrowPlus   = type(0) AgdaArrowPlus   #-}
{-# COMPILE GHC ArrowLoop   = type(0) AgdaArrowLoop   #-}

{-# FOREIGN GHC
data AgdaBifunctor aℓ bℓ cℓ f  = Data.Bifunctor.Bifunctor f         => AgdaBifunctor
data AgdaBifoldable aℓ bℓ cℓ f = Data.Bifoldable.Bifoldable f       => AgdaBifoldable
data AgdaBitraversable aℓ f    = Data.Bitraversable.Bitraversable f => AgdaBitraversable
#-}
{-# COMPILE GHC Bifunctor     = type(0) AgdaBifunctor     #-}
{-# COMPILE GHC Bifoldable    = type(0) AgdaBifoldable    #-}
{-# COMPILE GHC Bitraversable = type(0) AgdaBitraversable #-}

{-# FOREIGN GHC
data AgdaStorable  aℓ a = Foreign.Storable.Storable a        => AgdaStorable
data AgdaException aℓ a = Control.Exception.Base.Exception a => AgdaException
#-}
{-# COMPILE GHC Storable  = type(0) AgdaStorable  #-}
{-# COMPILE GHC Exception = type(0) AgdaException #-}

{-# FOREIGN GHC
data AgdaData aℓ k a                = Data.Data.Data a                  => AgdaData
data AgdaTypeable aℓ k a            = Type.Reflection.Typeable a        => AgdaTypeable
data AgdaTestEquality aℓ bℓ f       = Data.Type.Equality.TestEquality f => AgdaTestEquality
data AgdaTestCoercion aℓ bℓ k1 k2 f = Data.Type.Coercion.TestCoercion f => AgdaTestCoercion
data AgdaCoercible aℓ bℓ a b        = Data.Coerce.Coercible a b         => AgdaCoercible
#-}
{-# COMPILE GHC Data         = type(0) AgdaData         #-}
{-# COMPILE GHC Typeable     = type(0) AgdaTypeable     #-}
{-# COMPILE GHC TestEquality = type(0) AgdaTestEquality #-}
{-# COMPILE GHC TestCoercion = type(0) AgdaTestCoercion #-}
{-# COMPILE GHC Coercible    = type(0) AgdaCoercible    #-}

{-# FOREIGN GHC
data AgdaIsString aℓ a = Data.String.IsString a => AgdaIsString
#-}
{-# COMPILE GHC IsString = type(0) AgdaIsString #-}

{-# FOREIGN GHC
data AgdaEq1 aℓ bℓ f      = Data.Functor.Classes.Eq1 f   => AgdaEq1
data AgdaOrd1 aℓ bℓ f     = Data.Functor.Classes.Ord1 f  => AgdaOrd1
data AgdaRead1 aℓ bℓ f    = Data.Functor.Classes.Read1 f => AgdaRead1
data AgdaShow1 aℓ bℓ f    = Data.Functor.Classes.Show1 f => AgdaShow1
data AgdaEq2 aℓ bℓ cℓ f   = Data.Functor.Classes.Eq2 f   => AgdaEq2
data AgdaOrd2 aℓ bℓ cℓ f  = Data.Functor.Classes.Ord2 f  => AgdaOrd2
data AgdaRead2 aℓ bℓ cℓ f = Data.Functor.Classes.Read2 f => AgdaRead2
data AgdaShow2 aℓ bℓ cℓ f = Data.Functor.Classes.Show2 f => AgdaShow2
#-}
{-# COMPILE GHC Eq1   = type(0) AgdaEq1   #-}
{-# COMPILE GHC Ord1  = type(0) AgdaOrd1  #-}
{-# COMPILE GHC Read1 = type(0) AgdaRead1 #-}
{-# COMPILE GHC Show1 = type(0) AgdaShow1 #-}
{-# COMPILE GHC Eq2   = type(0) AgdaEq2   #-}
{-# COMPILE GHC Ord2  = type(0) AgdaOrd2  #-}
{-# COMPILE GHC Read2 = type(0) AgdaRead2 #-}
{-# COMPILE GHC Show2 = type(0) AgdaShow2 #-}
