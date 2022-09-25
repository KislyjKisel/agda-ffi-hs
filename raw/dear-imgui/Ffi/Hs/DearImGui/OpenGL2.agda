{-# OPTIONS --without-K #-}

module Ffi.Hs.DearImGui.OpenGL2 where

open import Agda.Primitive
open import Ffi.Hs.-base.Class using (MonadIO)
open import Ffi.Hs.-base.Unit  using (⊤′)
open import Ffi.Hs.DearImGui{- todo: .Raw -} using (DrawData)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified DearImGui.OpenGL2
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        mℓ : Level
        M : Set mℓ → Set mℓ

postulate
    openGL2Init           : ⦃ MonadIO M ⦄ → M ⊤′
    openGL2Shutdown       : ⦃ MonadIO M ⦄ → M ⊤′
    openGL2NewFrame       : ⦃ MonadIO M ⦄ → M ⊤′
    openGL2RenderDrawData : ⦃ MonadIO M ⦄ → DrawData → M ⊤′

{-# COMPILE GHC openGL2Init           = \ mℓ m AgdaMonadIO -> DearImGui.OpenGL2.openGL2Init           #-}
{-# COMPILE GHC openGL2Shutdown       = \ mℓ m AgdaMonadIO -> DearImGui.OpenGL2.openGL2Shutdown       #-}
{-# COMPILE GHC openGL2NewFrame       = \ mℓ m AgdaMonadIO -> DearImGui.OpenGL2.openGL2NewFrame       #-}
{-# COMPILE GHC openGL2RenderDrawData = \ mℓ m AgdaMonadIO -> DearImGui.OpenGL2.openGL2RenderDrawData #-}
