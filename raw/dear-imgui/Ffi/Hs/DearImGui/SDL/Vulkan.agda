{-# OPTIONS --without-K #-}

module Ffi.Hs.DearImGui.SDL.Vulkan where

open import Agda.Builtin.Bool         using (Bool)
open import Ffi.Hs.-base.Class        using (MonadIO)
open import Ffi.Hs.-base.Level        using (Liftℓ)
open import Ffi.Hs.SDL.Internal.Types using (Window)

{-# FOREIGN GHC
import qualified DearImGui.SDL.Vulkan
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Class (AgdaMonadIO(AgdaMonadIO))
#-}

postulate
    sdl2InitForVulkan : ∀{mℓ} {M : Set mℓ → Set mℓ} → ⦃ MonadIO M ⦄ → Window → M (Liftℓ _ Bool)

{-# COMPILE GHC sdl2InitForVulkan = \ mℓ m AgdaMonadIO -> DearImGui.SDL.Vulkan.sdl2InitForVulkan #-}
