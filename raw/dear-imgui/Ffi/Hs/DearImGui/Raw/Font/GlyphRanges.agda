{-# OPTIONS --without-K #-}

module Ffi.Hs.DearImGui.Raw.Font.GlyphRanges where

open import Agda.Builtin.Maybe       using (Maybe)
open import Agda.Primitive           using (Level)
open import Ffi.Hs.-base.Class
open import Ffi.Hs.-base.Level       using (Liftℓ)
open import Ffi.Hs.-base.Unit        using (⊤; ⊤′)
open import Ffi.Hs.-dear-imgui.Types using (ImWchar; ImFontGlyphRangesBuilder)
open import Ffi.Hs.Foreign.C.String  using (CString)
open import Ffi.Hs.Foreign.Ptr       using (Ptr)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified DearImGui.Raw.Font.GlyphRanges
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        mℓ : Level
        M : Set mℓ → Set mℓ


data GlyphRanges : Set where
    mkGlyphRanges : Ptr ImWchar → GlyphRanges

{-# COMPILE GHC GlyphRanges = data DearImGui.Raw.Font.GlyphRanges.GlyphRanges (DearImGui.Raw.Font.GlyphRanges.GlyphRanges) #-}


data Builtin : Set where
    Latin                   : Builtin
    Korean                  : Builtin
    Japanese                : Builtin
    ChineseFull             : Builtin
    ChineseSimplifiedCommon : Builtin
    Cyrillic                : Builtin
    Thai                    : Builtin
    Vietnamese              : Builtin

{-# COMPILE GHC Builtin = data DearImGui.Raw.Font.GlyphRanges.Builtin (DearImGui.Raw.Font.GlyphRanges.Builtin) #-}

postulate
    Bounded[Builtin] : Bounded Builtin
    Enum[Builtin]    : Enum Builtin
    Show[Builtin]    : Show Builtin
    Eq[Builtin]      : Eq Builtin
    Ord[Builtin]     : Ord Builtin

{-# COMPILE GHC Bounded[Builtin] = AgdaBounded #-}
{-# COMPILE GHC Enum[Builtin]    = AgdaEnum    #-}
{-# COMPILE GHC Show[Builtin]    = AgdaShow    #-}
{-# COMPILE GHC Eq[Builtin]      = AgdaEq      #-}
{-# COMPILE GHC Ord[Builtin]     = AgdaOrd     #-}

postulate
    getBuiltin   : Builtin → GlyphRanges
    builtinSetup : Builtin → Maybe GlyphRanges

{-# COMPILE GHC getBuiltin   = DearImGui.Raw.Font.GlyphRanges.getBuiltin   #-}
{-# COMPILE GHC builtinSetup = DearImGui.Raw.Font.GlyphRanges.builtinSetup #-}


data GlyphRangesBuilder : Set where
    mkGlyphRangesBuilder : Ptr ImFontGlyphRangesBuilder → GlyphRangesBuilder

{-# COMPILE GHC GlyphRangesBuilder = data DearImGui.Raw.Font.GlyphRanges.GlyphRangesBuilder (DearImGui.Raw.Font.GlyphRanges.GlyphRangesBuilder) #-}

postulate
    new       : ⦃ MonadIO M ⦄ → M (Liftℓ _ GlyphRangesBuilder)
    destroy   : ⦃ MonadIO M ⦄ → GlyphRangesBuilder → M ⊤′
    addChar   : ⦃ MonadIO M ⦄ → GlyphRangesBuilder → ImWchar → M ⊤′
    addText   : ⦃ MonadIO M ⦄ → GlyphRangesBuilder → CString → M ⊤′
    addRanges : ⦃ MonadIO M ⦄ → GlyphRangesBuilder → GlyphRanges → M ⊤′

{-# COMPILE GHC new       = \ mℓ m AgdaMonadIO -> DearImGui.Raw.Font.GlyphRanges.new       #-}
{-# COMPILE GHC destroy   = \ mℓ m AgdaMonadIO -> DearImGui.Raw.Font.GlyphRanges.destroy   #-}
{-# COMPILE GHC addChar   = \ mℓ m AgdaMonadIO -> DearImGui.Raw.Font.GlyphRanges.addChar   #-}
{-# COMPILE GHC addText   = \ mℓ m AgdaMonadIO -> DearImGui.Raw.Font.GlyphRanges.addText   #-}
{-# COMPILE GHC addRanges = \ mℓ m AgdaMonadIO -> DearImGui.Raw.Font.GlyphRanges.addRanges #-}


data GlyphRangesVector : Set where
    mkGlyphRangesVector : Ptr ⊤ → GlyphRangesVector

{-# COMPILE GHC GlyphRangesVector = data DearImGui.Raw.Font.GlyphRanges.GlyphRangesVector (DearImGui.Raw.Font.GlyphRanges.GlyphRangesVector) #-}

postulate
    buildRangesVector   : ⦃ MonadIO M ⦄ → GlyphRangesVector → M (Liftℓ _ GlyphRangesVector)
    fromRangesVector    : GlyphRangesVector → GlyphRanges
    destroyRangesVector : ⦃ MonadIO M ⦄ → GlyphRangesVector → M ⊤′

{-# COMPILE GHC buildRangesVector   = \ mℓ m AgdaMonadIO -> DearImGui.Raw.Font.GlyphRanges.buildRangesVector   #-}
{-# COMPILE GHC fromRangesVector    =                       DearImGui.Raw.Font.GlyphRanges.fromRangesVector    #-}
{-# COMPILE GHC destroyRangesVector = \ mℓ m AgdaMonadIO -> DearImGui.Raw.Font.GlyphRanges.destroyRangesVector #-}
