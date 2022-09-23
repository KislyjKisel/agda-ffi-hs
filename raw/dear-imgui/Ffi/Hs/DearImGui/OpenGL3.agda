{-# OPTIONS --without-K #-}

module Ffi.Hs.DearImGui.OpenGL3 where

open import Agda.Builtin.Bool  using (Bool)
open import Agda.Primitive
open import Ffi.Hs.-base.Class using (MonadIO)
open import Ffi.Hs.-base.Level using (Liftℓ)
open import Ffi.Hs.-base.Unit  using (⊤′)
open import Ffi.Hs.DearImGui{- todo: .Raw -} using (DrawData)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified DearImGui.OpenGL3
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        mℓ : Level
        M : Set mℓ → Set mℓ

postulate
    openGL3Init           : ⦃ MonadIO M ⦄ → M (Liftℓ _ Bool)
    openGL3Shutdown       : ⦃ MonadIO M ⦄ → M ⊤′
    openGL3NewFrame       : ⦃ MonadIO M ⦄ → M ⊤′
    openGL3RenderDrawData : ⦃ MonadIO M ⦄ → DrawData → M ⊤′

{-# COMPILE GHC openGL3Init           = \ mℓ m AgdaMonadIO -> DearImGui.OpenGL3.openGL3Init           #-}
{-# COMPILE GHC openGL3Shutdown       = \ mℓ m AgdaMonadIO -> DearImGui.OpenGL3.openGL3Shutdown       #-}
{-# COMPILE GHC openGL3NewFrame       = \ mℓ m AgdaMonadIO -> DearImGui.OpenGL3.openGL3NewFrame       #-}
{-# COMPILE GHC openGL3RenderDrawData = \ mℓ m AgdaMonadIO -> DearImGui.OpenGL3.openGL3RenderDrawData #-}
