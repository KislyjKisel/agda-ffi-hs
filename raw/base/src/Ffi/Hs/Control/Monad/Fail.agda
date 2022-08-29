{-# OPTIONS --without-K #-}

module Ffi.Hs.Control.Monad.Fail where

open import Agda.Builtin.Char using (Char)
open import Agda.Builtin.List using (List)
open import Agda.Primitive

open import Ffi.Hs.-base.Class public
    using (MonadFail)

private
    variable
        aℓ fℓ : Level
        A : Set aℓ
        F : Set fℓ → Set fℓ

postulate
    fail : ⦃ MonadFail F ⦄ → List Char → F A

{-# FOREIGN GHC import qualified Control.Monad.Fail #-}
{-# COMPILE GHC fail = \ fℓ a f AgdaMonadFail -> Control.Monad.Fail.fail #-}
