{-# OPTIONS --without-K #-}

module Ffi.Hs.DearImGui.Raw.ListClipper where

open import Agda.Builtin.Bool        using (Bool)
open import Agda.Primitive           using (Level)
open import Ffi.Hs.-base.Class       using (MonadIO)
open import Ffi.Hs.-base.Level       using (Liftℓ)
open import Ffi.Hs.-base.Unit        using (⊤′)
open import Ffi.Hs.-dear-imgui.Types using (ImGuiListClipper)
open import Ffi.Hs.Foreign.C.Types   using (CInt; CFloat)
open import Ffi.Hs.Foreign.Ptr       using (Ptr)

{-# FOREIGN GHC
import qualified DearImGui.Raw.ListClipper
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Class (AgdaMonadIO(AgdaMonadIO))
#-}

private
    variable
        mℓ : Level
        M : Set mℓ → Set mℓ


ListClipper : Set
ListClipper = Ptr ImGuiListClipper

postulate
    new          : ⦃ MonadIO M ⦄ → M (Liftℓ _ ListClipper)
    delete       : ⦃ MonadIO M ⦄ → ListClipper → M ⊤′
    begin        : ⦃ MonadIO M ⦄ → ListClipper → CInt → CFloat → M ⊤′
    displayStart : ListClipper → CInt
    displayEnd   : ListClipper → CInt
    step         : ⦃ MonadIO M ⦄ → ListClipper → M (Liftℓ _ Bool)

{-# COMPILE GHC new          = \ mℓ m AgdaMonadIO -> DearImGui.Raw.ListClipper.new          #-}
{-# COMPILE GHC delete       = \ mℓ m AgdaMonadIO -> DearImGui.Raw.ListClipper.delete       #-}
{-# COMPILE GHC begin        = \ mℓ m AgdaMonadIO -> DearImGui.Raw.ListClipper.begin        #-}
{-# COMPILE GHC displayStart =                       DearImGui.Raw.ListClipper.displayStart #-}
{-# COMPILE GHC displayEnd   =                       DearImGui.Raw.ListClipper.displayEnd   #-}
{-# COMPILE GHC step         = \ mℓ m AgdaMonadIO -> DearImGui.Raw.ListClipper.step         #-}
