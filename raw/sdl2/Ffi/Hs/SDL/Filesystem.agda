{-# OPTIONS --without-K #-}

module Ffi.Hs.SDL.Filesystem where

open import Agda.Builtin.String using () renaming (String to Text)
open import Agda.Primitive
open import Ffi.Hs.-base.Class  using (MonadIO)
open import Ffi.Hs.-base.Level  using (Liftℓ)

{-# FOREIGN GHC
import qualified SDL.Filesystem
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Class (AgdaMonadIO(AgdaMonadIO))
#-}

private
    variable
        mℓ : Level
        M : Set mℓ → Set mℓ

postulate
    getBasePath : ⦃ MonadIO M ⦄ → M (Liftℓ _ Text)
    getPrefPath : ⦃ MonadIO M ⦄ → Text → Text → M (Liftℓ _ Text)

{-# COMPILE GHC getBasePath = \ mℓ m AgdaMonadIO -> SDL.Filesystem.getBasePath #-}
{-# COMPILE GHC getPrefPath = \ mℓ m AgdaMonadIO -> SDL.Filesystem.getPrefPath #-}
