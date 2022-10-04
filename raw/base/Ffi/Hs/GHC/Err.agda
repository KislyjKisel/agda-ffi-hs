{-# OPTIONS --without-K #-}

module Ffi.Hs.GHC.Err where

open import Agda.Builtin.Char using (Char)
open import Agda.Builtin.List using (List)
open import Agda.Primitive
open import Ffi.Hs.GHC.Stack  using (HasCallStack)

{-# FOREIGN GHC
import qualified GHC.Err
import MAlonzo.Code.Ffi.Hs.GHC.Stack (AgdaHasCallStack(AgdaHasCallStack))
#-}

private
    variable
        aℓ : Level
        A : Set aℓ

-- todo: different runtime reps

postulate
    absentErr              : A
    error                  : ⦃ HasCallStack ⦄ → List Char → A
    errorWithoutStackTrace : List Char → A
    undefined              : ⦃ HasCallStack ⦄ → A

{-# COMPILE GHC absentErr              = \ aℓ a                  -> GHC.Err.absentErr              #-}
{-# COMPILE GHC error                  = \ aℓ a AgdaHasCallStack -> GHC.Err.error                  #-}
{-# COMPILE GHC errorWithoutStackTrace = \ aℓ a                  -> GHC.Err.errorWithoutStackTrace #-}
{-# COMPILE GHC undefined              = \ aℓ a AgdaHasCallStack -> GHC.Err.undefined              #-}
