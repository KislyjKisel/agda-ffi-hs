{-# OPTIONS --without-K #-}

module Ffi.Hs.Control.Exception where

open import Agda.Builtin.IO    using (IO)
open import Agda.Builtin.List  using (List)
open import Agda.Primitive
open import Ffi.Hs.-base.Class using (Exception; Functor)
open import Ffi.Hs.-base.Unit  using (⊤; ⊤′)

open import Ffi.Hs.Control.Exception.Base public

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Control.Exception
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ eℓ : Level
        A : Set aℓ

data Handler {eℓ} {aℓ} (A : Set aℓ) : Set (lsuc eℓ ⊔ aℓ) where
    mkHandler : {E : Set eℓ} → ⦃ Exception E ⦄ → (E → IO A) → Handler A

{-# FOREIGN GHC type AgdaHandler eℓ aℓ = Control.Exception.Handler #-}
{-# COMPILE GHC Handler = data(1) AgdaHandler (Control.Exception.Handler) #-}

postulate
    Functor[Handler] : Functor (Handler {eℓ} {lsuc eℓ})
    catches        : IO A → List (Handler {eℓ} A) → IO A
    interruptible  : IO A → IO A
    allowInterrupt : IO ⊤

{-# COMPILE GHC Functor[Handler] = \ eℓ -> AgdaFunctor #-}
{-# COMPILE GHC catches        = \ aℓ a eℓ -> Control.Exception.catches        #-}
{-# COMPILE GHC interruptible  = \ aℓ a    -> Control.Exception.interruptible  #-}
{-# COMPILE GHC allowInterrupt =              Control.Exception.allowInterrupt #-}
