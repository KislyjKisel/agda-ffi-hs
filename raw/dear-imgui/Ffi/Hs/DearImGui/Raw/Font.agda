{-# OPTIONS --without-K #-}

module Ffi.Hs.DearImGui.Raw.Font where

open import Agda.Primitive                   using (Level)
open import Ffi.Hs.-base.Class               using (MonadIO)
open import Ffi.Hs.-base.Level               using (Liftℓ)
open import Ffi.Hs.-base.Unit                using (⊤′)
open import Ffi.Hs.-dear-imgui.Types         using (ImFont)
open import Ffi.Hs.DearImGui.Raw.Font.Config using (FontConfig)
open import Ffi.Hs.Foreign.C.String          using (CString; CStringLen)
open import Ffi.Hs.Foreign.C.Types           using (CFloat)
open import Ffi.Hs.Foreign.Ptr               using (Ptr)

open import Ffi.Hs.DearImGui.Raw.Font.GlyphRanges public
    using (GlyphRanges)

{-# FOREIGN GHC
import qualified DearImGui.Raw.Font
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Class (AgdaMonadIO(AgdaMonadIO))
#-}

private
    variable
        mℓ : Level
        M : Set mℓ → Set mℓ


data Font : Set where
    mkFont : Ptr ImFont → Font

{-# COMPILE GHC Font = data DearImGui.Raw.Font.Font (DearImGui.Raw.Font.Font) #-}

postulate
    addFontDefault       : ⦃ MonadIO M ⦄ → M (Liftℓ _ Font)
    addFontFromFileTTF   : ⦃ MonadIO M ⦄ → CString → CFloat → FontConfig → GlyphRanges → M (Liftℓ _ Font)
    addFontFromMemoryTTF : ⦃ MonadIO M ⦄ → CStringLen → CFloat → FontConfig → GlyphRanges → M (Liftℓ _ Font)

    pushFont : ⦃ MonadIO M ⦄ → Font → M ⊤′
    popFont  : ⦃ MonadIO M ⦄ → Font → M ⊤′

    clearFontAtlas : ⦃ MonadIO M ⦄ → M ⊤′
    buildFontAtlas : ⦃ MonadIO M ⦄ → M ⊤′

{-# COMPILE GHC addFontDefault       = \ mℓ m AgdaMonadIO -> DearImGui.Raw.Font.addFontDefault       #-}
{-# COMPILE GHC addFontFromFileTTF   = \ mℓ m AgdaMonadIO -> DearImGui.Raw.Font.addFontFromFileTTF   #-}
{-# COMPILE GHC addFontFromMemoryTTF = \ mℓ m AgdaMonadIO -> DearImGui.Raw.Font.addFontFromMemoryTTF #-}

{-# COMPILE GHC pushFont = \ mℓ m AgdaMonadIO -> DearImGui.Raw.Font.pushFont #-}
{-# COMPILE GHC popFont  = \ mℓ m AgdaMonadIO -> DearImGui.Raw.Font.popFont  #-}

{-# COMPILE GHC clearFontAtlas = \ mℓ m AgdaMonadIO -> DearImGui.Raw.Font.clearFontAtlas #-}
{-# COMPILE GHC buildFontAtlas = \ mℓ m AgdaMonadIO -> DearImGui.Raw.Font.buildFontAtlas #-}
