{-# OPTIONS --without-K #-}

module Ffi.Hs.Control.Monad where

open import Agda.Builtin.Bool using (Bool)
open import Agda.Primitive
open import Ffi.Hs.Data.String using (String)
open import Ffi.Hs.-base.Class using (Applicative)

open Ffi.Hs.-base.Class public
    using (Monad; MonadPlus; MonadFail)

open import Ffi.Hs.Control.Monad.Fail public
    using (fail)

private
    variable
        aℓ bℓ cℓ dℓ eℓ fℓ mℓ : Level
        A : Set aℓ
        B : Set bℓ
        C : Set cℓ
        D : Set dℓ
        E : Set eℓ
        F : Set fℓ
        M : Set mℓ → Set mℓ

infixl 4 _<$!>_
infixl 1 _>>=_ _>>_
infixr 1 _=<<_ _>=>_ _<=<_

postulate
    Monad  : (Set aℓ → Set aℓ) → Set aℓ
    return : ⦃ Monad M ⦄ → A → M A
    _>>=_  : ⦃ Monad M ⦄ → M A → (A → M B) → M B
    _>>_   : ⦃ Monad M ⦄ → M A → M B → M B

    mzero : ⦃ MonadPlus M ⦄ → M A
    mplus : ⦃ MonadPlus M ⦄ → M A → M A → M A
    
    _=<<_ : ⦃ Monad M ⦄ → (A → M B) → M A → M B 
    _>=>_ : ⦃ Monad M ⦄ → (A → M B) → (B → M C) → A → M C
    _<=<_ : ⦃ Monad M ⦄ → (B → M C) → (A → M B) → A → M C
    
    forever : ⦃ Applicative M ⦄ → M A → M B

    join : ⦃ Monad M ⦄ → M (M A) → M A
    mfilter : ⦃ MonadPlus M ⦄ → (A → Bool) → M A → M A

    liftM  : ⦃ Monad M ⦄ → (A → B) → M A → M B
    liftM2 : ⦃ Monad M ⦄ → (A → B → C) → M A → M B → M C
    liftM3 : ⦃ Monad M ⦄ → (A → B → C → D) → M A → M B → M C → M D
    liftM4 : ⦃ Monad M ⦄ → (A → B → C → D → E) → M A → M B → M C → M D → M E
    liftM5 : ⦃ Monad M ⦄ → (A → B → C → D → E → F) → M A → M B → M C → M D → M E → M F
    ap     : ⦃ Monad M ⦄ → M (A → B) → M A → M B 

    _<$!>_ : ⦃ Monad M ⦄ → (A → B) → M A → M B
