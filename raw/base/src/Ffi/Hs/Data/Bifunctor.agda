{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Bifunctor where

open import Agda.Primitive

private
    variable
        aℓ bℓ cℓ dℓ eℓ f1ℓ f2ℓ : Level
        A : Set aℓ
        B : Set bℓ
        C : Set cℓ
        D : Set dℓ
        E : Set eℓ
        F : Set f1ℓ → Set f2ℓ → Set (f1ℓ ⊔ f2ℓ)

postulate
    bimap  : ⦃ Bifunctor F ⦄ → (A → B) → (C → D) → F A C → F B D
    first  : ⦃ Bifunctor F ⦄ → (A → B) → F A C → F B C
    second : ⦃ Bifunctor F ⦄ → (B → C) → F A B → F A C

{-# FOREIGN GHC import qualified Data.Bifunctor #-}
{-# COMPILE GHC bimap  = \ f1ℓ f2ℓ a b c d f AgdaBifunctor -> Data.Bifunctor.bimap  #-}
{-# COMPILE GHC first  = \ f1ℓ f2ℓ a b c   f AgdaBifunctor -> Data.Bifunctor.first  #-}
{-# COMPILE GHC second = \ f1ℓ f2ℓ a b c   f AgdaBifunctor -> Data.Bifunctor.second #-}


open import Ffi.Hs.Data.Tuple  using (Tuple2; Tuple3; Tuple4; Tuple5)
open import Ffi.Hs.Data.Either using (Either)

postulate
    Bifunctor[Tuple2]        : Bifunctor {aℓ} {bℓ} Tuple2
    Bifunctor[Tuple3[A]]     : Bifunctor {bℓ} {cℓ} (Tuple3 A)
    Bifunctor[Tuple4[A,B]]   : Bifunctor {cℓ} {dℓ} (Tuple4 A B)
    Bifunctor[Tuple5[A,B,C]] : Bifunctor {dℓ} {eℓ} (Tuple5 A B C)

    Bifunctor[Either]        : Bifunctor {aℓ} {bℓ} Either

{-# COMPILE GHC Bifunctor[Tuple2]        = \ aℓ bℓ -> AgdaBifunctor                #-}
{-# COMPILE GHC Bifunctor[Tuple3[A]]     = \ aℓ a bℓ cℓ -> AgdaBifunctor           #-}
{-# COMPILE GHC Bifunctor[Tuple4[A,B]]   = \ aℓ bℓ a b cℓ dℓ -> AgdaBifunctor      #-}
{-# COMPILE GHC Bifunctor[Tuple5[A,B,C]] = \ aℓ bℓ cℓ a b c dℓ eℓ -> AgdaBifunctor #-}

{-# COMPILE GHC Bifunctor[Either]        = \ aℓ bℓ -> AgdaBifunctor                #-}
