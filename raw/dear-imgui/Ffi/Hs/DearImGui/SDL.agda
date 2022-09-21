{-# OPTIONS --without-K #-}

module Ffi.Hs.DearImGui.SDL where

open import Agda.Builtin.List  using (List)
open import Agda.Builtin.Maybe using (Maybe)
open import Agda.Primitive
open import Ffi.Hs.-base.Class using (MonadIO)
open import Ffi.Hs.-base.Level using (Liftℓ)
open import Ffi.Hs.-base.Unit  using (⊤; ⊤′)
open import Ffi.Hs.SDL.Event   using (Event)

{-# FOREIGN GHC
import qualified DearImGui.SDL
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Class (AgdaMonadIO(AgdaMonadIO))
#-}

private
    variable
        mℓ : Level
        M : Set mℓ → Set mℓ

postulate
    sdl2NewFrame        : ⦃ MonadIO M ⦄ → M ⊤′
    sdl2Shutdown        : ⦃ MonadIO M ⦄ → M ⊤′
    pollEventWithImGui  : ⦃ MonadIO M ⦄ → M (Liftℓ _ (Maybe Event))
    pollEventsWithImGui : ⦃ MonadIO M ⦄ → M (Liftℓ _ (List Event))

{-# COMPILE GHC sdl2NewFrame        = \ mℓ m AgdaMonadIO -> DearImGui.SDL.sdl2NewFrame        #-}
{-# COMPILE GHC sdl2Shutdown        = \ mℓ m AgdaMonadIO -> DearImGui.SDL.sdl2Shutdown        #-}
{-# COMPILE GHC pollEventWithImGui  = \ mℓ m AgdaMonadIO -> DearImGui.SDL.pollEventWithImGui  #-}
{-# COMPILE GHC pollEventsWithImGui = \ mℓ m AgdaMonadIO -> DearImGui.SDL.pollEventsWithImGui #-}
