{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Type.Equality where

open import Agda.Builtin.Maybe using (Maybe)
open import Agda.Primitive
open import Ffi.Hs.-base.Class
open import Ffi.Hs.Data.Bool   using (`Bool)

open Ffi.Hs.-base.Class public
    using (TestEquality)

{-# FOREIGN GHC {-# LANGUAGE GADTs #-} #-}

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Data.Type.Equality
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ bℓ : Level
        A B C : Set aℓ
        F G : Set aℓ → Set bℓ

infix 4 _:~:_ _:~~:_ _~_ _~~_

data _:~:_ {K : Set (lsuc aℓ)} (A : K) : K → Set (lsuc aℓ) where
    Refl : A :~: A

-- todo: (a :: k)
{-# FOREIGN GHC
type AgdaTypePropEq aℓ k = (Data.Type.Equality.:~:)
#-}
{-# COMPILE GHC _:~:_ = data(2) AgdaTypePropEq (Data.Type.Equality.Refl) #-}

-- same level kinds, otherwise _:~~:_ is in Setω (sort can't depend on indices)
data _:~~:_
        {K₁ : Set (lsuc aℓ)} (A : K₁) :
        {K₂ : Set (lsuc aℓ)} (B : K₂) → Set (lsuc (lsuc aℓ))
        where
    HRefl : _:~~:_ A {K₂ = K₁} A

-- todo: how to specify that (b :: k2) ?
{-# FOREIGN GHC
type AgdaTypePropHEq aℓ k1 (a :: k1) k2 = (Data.Type.Equality.:~~:) a
#-}
{-# COMPILE GHC _:~~:_ = data(4) AgdaTypePropHEq (Data.Type.Equality.HRefl) #-}

postulate
    sym : A :~: B → B :~: A 
    trans : A :~: B → B :~: C → A :~: C
    castWith : A :~: B → A → B
    -- -- gcastWith : A :~: B → 
    apply : F :~: G → A :~: B → F A :~: G B
    inner : F A :~: G B → A :~: B
    outer : F A :~: G B → F :~: G

    testEquality : ⦃ TestEquality F ⦄ → F A → F B → Maybe (A :~: B)

    _==_ : Set aℓ → Set bℓ → `Bool

    _~_  : {K : Set (lsuc aℓ)} (A : K) → K → Set (lsuc aℓ)
    _~~_ : {K₁ : Set (lsuc aℓ)} (A : K₁) {K₂ : Set (lsuc aℓ)} (B : K₂) → Set (lsuc (lsuc aℓ))

{-# COMPILE GHC sym      = \ aℓ a b        -> Data.Type.Equality.sym      #-}
{-# COMPILE GHC trans    = \ aℓ a b c      -> Data.Type.Equality.trans    #-}
{-# COMPILE GHC castWith = \ aℓ a b        -> Data.Type.Equality.castWith #-}
{-# COMPILE GHC apply    = \ aℓ bℓ f g a b -> Data.Type.Equality.apply    #-}
{-# COMPILE GHC inner    = \ aℓ bℓ f g a b -> Data.Type.Equality.inner    #-}
{-# COMPILE GHC outer    = \ aℓ bℓ f g a b -> Data.Type.Equality.outer    #-}

{-# COMPILE GHC testEquality = \ aℓ bℓ f a b AgdaTestEquality -> Data.Type.Equality.testEquality #-}

{-# FOREIGN GHC
type AgdaBoolTypeEq aℓ bℓ a b = (Data.Type.Equality.==) a b
#-}
{-# COMPILE GHC _==_ = type(4) AgdaBoolTypeEq #-}

{-# FOREIGN GHC
data AgdaTypeEq aℓ k (a :: k) (b :: k) = (a Data.Type.Equality.~ b) => AgdaTypeEq
#-}
{-# COMPILE GHC _~_ = type(2) AgdaTypeEq #-}

-- todo: Heterogeneous type equality can't be used in instances without first arg
-- Kind must(?) be applied in synonym, K₂ is index, 1st type is parameter, K₂ goes after 1st type
-- move some params to indices?
{-# FOREIGN GHC
data AgdaTypeHEq aℓ k1 (a :: k1) k2 (b :: k2) = (a Data.Type.Equality.~~ b) => AgdaTypeHEq
#-}
{-# COMPILE GHC _~~_ = type(4) AgdaTypeHEq #-}

postulate
    TestEquality[A:~:] : TestEquality (A :~:_)
    Bounded[A:~:B]     : ⦃ A ~ B ⦄ → Bounded (A :~: B)
    Category[:~:]      : Category {aℓ} _:~:_
    -- Data[A:~:B] : ⦃ A ~ B ⦄ → ⦃ Data A ⦄ → Data (A :~: B)
    Enum[A:~:B] : ⦃ A ~ B ⦄ → Enum (A :~: B)
    Read[A:~:B] : ⦃ A ~ B ⦄ → Read (A :~: B)
    Show[A:~:B] : Show (A :~: B)
    Eq[A:~:B]   : Eq (A :~: B)
    Ord[A:~:B]  : Ord (A :~: B)

{-# COMPILE GHC TestEquality[A:~:]  = \ aℓ a              -> AgdaTestEquality #-}
{-# COMPILE GHC Category[:~:]       = \ aℓ                -> AgdaCategory     #-}
{-# COMPILE GHC Bounded[A:~:B]      = \ aℓ a b AgdaTypeEq -> AgdaBounded      #-}
{-# COMPILE GHC Enum[A:~:B]         = \ aℓ a b AgdaTypeEq -> AgdaEnum         #-}
{-# COMPILE GHC Read[A:~:B]         = \ aℓ a b AgdaTypeEq -> AgdaRead         #-}
{-# COMPILE GHC Show[A:~:B]         = \ aℓ a b            -> AgdaShow         #-}
{-# COMPILE GHC Eq[A:~:B]           = \ aℓ a b            -> AgdaEq           #-}
{-# COMPILE GHC Ord[A:~:B]          = \ aℓ a b            -> AgdaOrd          #-}
-- {-# COMPILE GHC Data[A:~:B]         = \ aℓ a b AgdaTypeEq AgdaData -> AgdaData #-}

postulate
    TestEquality[A:~~:] : TestEquality (A :~~:_)
    Bounded[A:~~:B]     : ⦃ A ~~ B ⦄ → Bounded (A :~~: B)
    -- todo: invalid lambda term in type Category[:~~:]      : Category (λ a → _:~~:_ {K₁ = Set (lsuc aℓ)} a)
    Enum[A:~~:B] : ⦃ A ~~ B ⦄ → Enum (A :~~: B)
    Read[A:~~:B] : ⦃ A ~~ B ⦄ → Read (A :~~: B)
    Show[A:~~:B] : Show (A :~~: B)
    Eq[A:~~:B]   : Eq (A :~~: B)
    Ord[A:~~:B]  : Ord (A :~~: B)
    Data[A:~~:B] : {K₁ K₂ : Set (lsuc aℓ)} {A : K₁} {B : K₂} →
                 ⦃ Typeable K₁ ⦄ → ⦃ Typeable K₂ ⦄ → ⦃ Typeable A ⦄ → ⦃ Typeable B ⦄ →
                 ⦃ A ~~ B ⦄ → Data (A :~~: B)

{-# COMPILE GHC TestEquality[A:~~:] = \ aℓ a -> AgdaTestEquality #-}

-- {-# COMPILE GHC Category[:~~:]      = \ aℓ                 -> AgdaCategory     #-}
{-# COMPILE GHC Bounded[A:~~:B]     = \ aℓ a b AgdaTypeHEq -> AgdaBounded      #-}
{-# COMPILE GHC Enum[A:~~:B]        = \ aℓ a b AgdaTypeHEq -> AgdaEnum         #-}
{-# COMPILE GHC Read[A:~~:B]        = \ aℓ a b AgdaTypeHEq -> AgdaRead         #-}
{-# COMPILE GHC Show[A:~~:B]        = \ aℓ a b             -> AgdaShow         #-}
{-# COMPILE GHC Eq[A:~~:B]          = \ aℓ a b             -> AgdaEq           #-}
{-# COMPILE GHC Ord[A:~~:B]         = \ aℓ a b             -> AgdaOrd          #-}
{-# COMPILE GHC Data[A:~~:B] =
    \ aℓ k1 k2 a b AgdaTypeable AgdaTypeable AgdaTypeable AgdaTypeable AgdaTypeHEq -> AgdaData
#-}
