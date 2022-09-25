{-# OPTIONS --without-K #-}

module Ffi.Hs.DearImGui.Raw.Font.Config where

open import Agda.Primitive                        using (Level)
open import Ffi.Hs.-base.Class                    using (MonadIO)
open import Ffi.Hs.-base.Level                    using (Liftℓ)
open import Ffi.Hs.-base.Unit                     using (⊤′)
open import Ffi.Hs.-dear-imgui.Types              using (ImFontConfig; ImWchar; ImVec2)
open import Ffi.Hs.DearImGui.Raw.Font.GlyphRanges using (GlyphRanges)
open import Ffi.Hs.Foreign.C.Types                using (CInt; CUInt; CBool; CFloat)
open import Ffi.Hs.Foreign.Ptr                    using (Ptr)

{-# FOREIGN GHC
import qualified DearImGui.Raw.Font.Config
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Class (AgdaMonadIO(AgdaMonadIO))
#-}

private
    variable
        mℓ : Level
        M : Set mℓ → Set mℓ


data FontConfig : Set where
    mkFontConfig : Ptr ImFontConfig → FontConfig

{-# COMPILE GHC FontConfig = data DearImGui.Raw.Font.Config.FontConfig (DearImGui.Raw.Font.Config.FontConfig) #-}

postulate
    new     : ⦃ MonadIO M ⦄ → M (Liftℓ _ FontConfig)
    destroy : ⦃ MonadIO M ⦄ → FontConfig → M ⊤′

{-# COMPILE GHC new     = \ mℓ m AgdaMonadIO -> DearImGui.Raw.Font.Config.new     #-}
{-# COMPILE GHC destroy = \ mℓ m AgdaMonadIO -> DearImGui.Raw.Font.Config.destroy #-}

-- Changing settings
postulate
    setFontDataOwnedByAtlas : ⦃ MonadIO M ⦄ → FontConfig → CBool → M ⊤′
    setFontNo               : ⦃ MonadIO M ⦄ → FontConfig → CInt → M ⊤′
    setSizePixels           : ⦃ MonadIO M ⦄ → FontConfig → CFloat → M ⊤′
    setOversampleH          : ⦃ MonadIO M ⦄ → FontConfig → CInt → M ⊤′
    setOversampleV          : ⦃ MonadIO M ⦄ → FontConfig → CInt → M ⊤′
    setPixelSnapH           : ⦃ MonadIO M ⦄ → FontConfig → CBool → M ⊤′
    setGlyphExtraSpacing    : ⦃ MonadIO M ⦄ → FontConfig → Ptr ImVec2 → M ⊤′
    setGlyphOffset          : ⦃ MonadIO M ⦄ → FontConfig → Ptr ImVec2 → M ⊤′
    setGlyphRanges          : ⦃ MonadIO M ⦄ → FontConfig → GlyphRanges → M ⊤′
    setGlyphMinAdvanceX     : ⦃ MonadIO M ⦄ → FontConfig → CFloat → M ⊤′
    setGlyphMaxAdvanceX     : ⦃ MonadIO M ⦄ → FontConfig → CFloat → M ⊤′
    setMergeMode            : ⦃ MonadIO M ⦄ → FontConfig → CBool → M ⊤′
    setFontBuilderFlags     : ⦃ MonadIO M ⦄ → FontConfig → CUInt → M ⊤′
    setRasterizerMultiply   : ⦃ MonadIO M ⦄ → FontConfig → CFloat → M ⊤′
    setEllipsisChar         : ⦃ MonadIO M ⦄ → FontConfig → ImWchar → M ⊤′

{-# COMPILE GHC setFontDataOwnedByAtlas = \ mℓ m AgdaMonadIO -> DearImGui.Raw.Font.Config.setFontDataOwnedByAtlas #-}
{-# COMPILE GHC setFontNo               = \ mℓ m AgdaMonadIO -> DearImGui.Raw.Font.Config.setFontNo               #-}
{-# COMPILE GHC setSizePixels           = \ mℓ m AgdaMonadIO -> DearImGui.Raw.Font.Config.setSizePixels           #-}
{-# COMPILE GHC setOversampleH          = \ mℓ m AgdaMonadIO -> DearImGui.Raw.Font.Config.setOversampleH          #-}
{-# COMPILE GHC setOversampleV          = \ mℓ m AgdaMonadIO -> DearImGui.Raw.Font.Config.setOversampleV          #-}
{-# COMPILE GHC setPixelSnapH           = \ mℓ m AgdaMonadIO -> DearImGui.Raw.Font.Config.setPixelSnapH           #-}
{-# COMPILE GHC setGlyphExtraSpacing    = \ mℓ m AgdaMonadIO -> DearImGui.Raw.Font.Config.setGlyphExtraSpacing    #-}
{-# COMPILE GHC setGlyphOffset          = \ mℓ m AgdaMonadIO -> DearImGui.Raw.Font.Config.setGlyphOffset          #-}
{-# COMPILE GHC setGlyphRanges          = \ mℓ m AgdaMonadIO -> DearImGui.Raw.Font.Config.setGlyphRanges          #-}
{-# COMPILE GHC setGlyphMinAdvanceX     = \ mℓ m AgdaMonadIO -> DearImGui.Raw.Font.Config.setGlyphMinAdvanceX     #-}
{-# COMPILE GHC setGlyphMaxAdvanceX     = \ mℓ m AgdaMonadIO -> DearImGui.Raw.Font.Config.setGlyphMaxAdvanceX     #-}
{-# COMPILE GHC setMergeMode            = \ mℓ m AgdaMonadIO -> DearImGui.Raw.Font.Config.setMergeMode            #-}
{-# COMPILE GHC setFontBuilderFlags     = \ mℓ m AgdaMonadIO -> DearImGui.Raw.Font.Config.setFontBuilderFlags     #-}
{-# COMPILE GHC setRasterizerMultiply   = \ mℓ m AgdaMonadIO -> DearImGui.Raw.Font.Config.setRasterizerMultiply   #-}
{-# COMPILE GHC setEllipsisChar         = \ mℓ m AgdaMonadIO -> DearImGui.Raw.Font.Config.setEllipsisChar         #-}
