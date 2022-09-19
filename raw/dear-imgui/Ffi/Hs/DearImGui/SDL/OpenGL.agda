{-# OPTIONS --without-K #-}

module Ffi.Hs.DearImGui.SDL.OpenGL where

open import Agda.Builtin.Bool         using (Bool)
open import Ffi.Hs.-base.Class        using (MonadIO)
open import Ffi.Hs.-base.Level        using (Liftℓ)
open import Ffi.Hs.SDL.Internal.Types using (Window)
open import Ffi.Hs.SDL.Video.OpenGL   using (GLContext)

{-# FOREIGN GHC
import qualified DearImGui.SDL.OpenGL
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Class (AgdaMonadIO(AgdaMonadIO))
#-}

postulate
    sdl2InitForOpenGL : ∀{mℓ} {M : Set mℓ → Set mℓ} → ⦃ MonadIO M ⦄ → Window → GLContext → M (Liftℓ _ Bool)

{-# COMPILE GHC sdl2InitForOpenGL = \ mℓ m AgdaMonadIO -> DearImGui.SDL.OpenGL.sdl2InitForOpenGL #-}
