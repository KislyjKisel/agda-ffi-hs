{-# OPTIONS --without-K #-}

module Ffi.Hs.Linear.Vector where

open import Agda.Builtin.List            using (List)
open import Agda.Builtin.Maybe           using (Maybe)
open import Agda.Primitive
open import Ffi.Hs.-base.Class           using (Functor; Num; Fractional; Foldable; Traversable)
open import Ffi.Hs.Control.Applicative   using (ZipList)
open import Ffi.Hs.Data.Complex          using (Complex)
open import Ffi.Hs.Data.Functor.Compose  using (Compose)
open import Ffi.Hs.Data.Functor.Identity using (Identity)
open import Ffi.Hs.Data.Functor.Product  using (Product)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Linear.Vector
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ : Level
        A B C : Set aℓ
        F G V : Set aℓ → Set aℓ

infixl 6 _^+^_ _^-^_

postulate
    Additive : (Set aℓ → Set aℓ) → Set aℓ
    Additive[F]⇒Functor[F] : ⦃ Additive F ⦄ → Functor F

    zero   : ⦃ Additive F ⦄ → ⦃ Num A ⦄ → F A
    _^+^_  : ⦃ Additive F ⦄ → ⦃ Num A ⦄ → F A → F A → F A
    _^-^_  : ⦃ Additive F ⦄ → ⦃ Num A ⦄ → F A → F A → F A
    lerp   : ⦃ Additive F ⦄ → ⦃ Num A ⦄ → A → F A → F A → F A
    liftU2 : ⦃ Additive F ⦄ → (A → A → A) → F A → F A → F A
    liftI2 : ⦃ Additive F ⦄ → (A → B → C) → F A → F B → F C

    Additive[List]     : Additive {aℓ} List
    Additive[Maybe]    : Additive {aℓ} Maybe
    Additive[Complex]  : Additive {aℓ} Complex
    Additive[Identity] : Additive {aℓ} Identity
    Additive[ZipList]  : Additive {aℓ} ZipList
    Additive[Product]  : ⦃ Additive F ⦄ → ⦃ Additive G ⦄ → Additive (Product F G)
    Additive[Compose]  : ⦃ Additive F ⦄ → ⦃ Additive G ⦄ → Additive (Compose F G)
    -- todo: Additive instances for Map, HashMap, IntMap, boxed Vector

{-# FOREIGN GHC data AgdaAdditive fℓ f = Linear.Vector.Additive f => AgdaAdditive #-}
{-# COMPILE GHC Additive = type(0) AgdaAdditive #-}

{-# COMPILE GHC Additive[F]⇒Functor[F] = \ fℓ f AgdaAdditive -> AgdaFunctor #-}

{-# COMPILE GHC zero   = \ fℓ f a AgdaAdditive AgdaNum -> Linear.Vector.zero   #-}
{-# COMPILE GHC _^+^_  = \ fℓ f a AgdaAdditive AgdaNum -> (Linear.Vector.^+^)  #-}
{-# COMPILE GHC _^-^_  = \ fℓ f a AgdaAdditive AgdaNum -> (Linear.Vector.^-^)  #-}
{-# COMPILE GHC lerp   = \ fℓ f a AgdaAdditive AgdaNum -> Linear.Vector.lerp   #-}
{-# COMPILE GHC liftU2 = \ fℓ f a AgdaAdditive         -> Linear.Vector.liftU2 #-}
{-# COMPILE GHC liftI2 = \ fℓ f a b c AgdaAdditive     -> Linear.Vector.liftI2 #-}

{-# COMPILE GHC Additive[List]     = \ aℓ     -> AgdaAdditive #-}
{-# COMPILE GHC Additive[Maybe]    = \ aℓ     -> AgdaAdditive #-}
{-# COMPILE GHC Additive[Complex]  = \ aℓ     -> AgdaAdditive #-}
{-# COMPILE GHC Additive[Identity] = \ aℓ     -> AgdaAdditive #-}
{-# COMPILE GHC Additive[ZipList]  = \ aℓ     -> AgdaAdditive #-}
{-# COMPILE GHC Additive[Product]  = \ aℓ f g -> AgdaAdditive #-}
{-# COMPILE GHC Additive[Compose]  = \ aℓ f g -> AgdaAdditive #-}


record E (T : Set aℓ → Set aℓ) : Set (lsuc aℓ) where
    constructor mkE
    field
        {X} : Set aℓ
        el : {F : Set aℓ → Set aℓ} → ⦃ Functor F ⦄ → (X → F X) → T X → F (T X)

{-# FOREIGN GHC type AgdaE aℓ = Linear.Vector.E #-}
{-# COMPILE GHC E = data(1) AgdaE (Linear.Vector.E) #-}
-- todo: indexed-traversable instances for E


infixl 7 _^*_ _*^_ _^/_

postulate
    negated  : ⦃ Functor F ⦄ → ⦃ Num A ⦄ → F A → F A
    _^*_     : ⦃ Functor F ⦄ → ⦃ Num A ⦄ → F A → A → F A
    _*^_     : ⦃ Functor F ⦄ → ⦃ Num A ⦄ → A → F A → F A
    _^/_     : ⦃ Functor F ⦄ → ⦃ Fractional A ⦄ → F A → A → F A
    sumV     : ⦃ Foldable F ⦄ → ⦃ Additive V ⦄ → ⦃ Num A ⦄ → F (V A) → V A
    basis    : ⦃ Additive F ⦄ → ⦃ Traversable F ⦄ → ⦃ Num A ⦄ → List (F A)
    basisFor : ⦃ Traversable F ⦄ → ⦃ Num A ⦄ → F B → List (F A)
    scaled   : ⦃ Traversable F ⦄ → ⦃ Num A ⦄ → F A → F (F A)
    outer    : ⦃ Functor F ⦄ → ⦃ Functor G ⦄ → ⦃ Num A ⦄ → F A → G A → F (G A)
    unit     : ⦃ Additive F ⦄ → ⦃ Num A ⦄ → ((A → Identity A) → F A → Identity (F A)) → F A

{-# COMPILE GHC negated  = \ fℓ f a AgdaFunctor AgdaNum                    -> Linear.Vector.negated  #-}
{-# COMPILE GHC _^*_     = \ fℓ f a AgdaFunctor AgdaNum                    -> (Linear.Vector.^*)     #-}
{-# COMPILE GHC _*^_     = \ fℓ f a AgdaFunctor AgdaNum                    -> (Linear.Vector.*^)     #-}
{-# COMPILE GHC _^/_     = \ fℓ f a AgdaFunctor AgdaFractional             -> (Linear.Vector.^/)     #-}
{-# COMPILE GHC sumV     = \ aℓ bℓ f v a AgdaFoldable AgdaAdditive AgdaNum -> Linear.Vector.sumV     #-}
{-# COMPILE GHC basis    = \ fℓ f a AgdaAdditive AgdaTraversable AgdaNum   -> Linear.Vector.basis    #-}
{-# COMPILE GHC basisFor = \ fℓ f a b AgdaTraversable AgdaNum              -> Linear.Vector.basisFor #-}
{-# COMPILE GHC scaled   = \ fℓ f a AgdaTraversable AgdaNum                -> Linear.Vector.scaled   #-}
{-# COMPILE GHC outer    = \ aℓ f g a AgdaFunctor AgdaFunctor AgdaNum      -> Linear.Vector.outer    #-}
{-# COMPILE GHC unit     = \ fℓ f a AgdaAdditive AgdaNum                   -> Linear.Vector.unit     #-}
