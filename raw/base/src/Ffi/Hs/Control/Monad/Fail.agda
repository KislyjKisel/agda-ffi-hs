{-# OPTIONS --without-K #-}

module Ffi.Hs.Control.Monad.Fail where

open import Agda.Builtin.Char using (Char)
open import Agda.Builtin.List using (List)
open import Agda.Primitive

private
    variable
        aℓ fℓ : Level
        A : Set aℓ
        F : Set fℓ → Set fℓ

postulate
    MonadFail : (Set fℓ → Set fℓ) → Set fℓ
    fail : ⦃ MonadFail F ⦄ → List Char → F A

{-# FOREIGN GHC import qualified Control.Monad.Fail #-}
{-# FOREIGN GHC data AgdaMonadFail fℓ f = Control.Monad.Fail.MonadFail f => AgdaMonadFail #-}
{-# COMPILE GHC MonadFail = type(0) AgdaMonadFail #-}
{-# COMPILE GHC fail = \ fℓ a f AgdaMonadFail -> Control.Monad.Fail.fail #-}
