{-# OPTIONS --without-K #-}

module Ffi.Hs.DearImGui.Raw where

-- todo: FiniteEnum (req KnownNat)

open import Agda.Builtin.Bool             using (Bool)
open import Agda.Builtin.Maybe            using (Maybe)
open import Agda.Primitive                using (Level)
open import Ffi.Hs.-base.Class
open import Ffi.Hs.-base.Level            using (Liftℓ)
open import Ffi.Hs.-base.Unit             using (⊤; ⊤′)
open import Ffi.Hs.DearImGui.Raw.DrawList using (DrawList)
open import Ffi.Hs.Foreign.C.String       using (CString; CStringLen)
open import Ffi.Hs.Foreign.C.Types        using (CInt; CBool; CFloat; CUChar)
open import Ffi.Hs.Foreign.Ptr            using (Ptr)

open import Ffi.Hs.-dear-imgui.Types public

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified DearImGui.Raw
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ mℓ : Level
        A : Set aℓ
        M : Set mℓ → Set mℓ


record ImGuiTableColumnSortSpecs : Set where
    constructor mkImGuiTableColumnSortSpecs
    field
        columnUserID  : ImGuiID
        columnIndex   : ImS16
        sortOrder     : ImS16
        sortDirection : ImGuiSortDirection

{-# COMPILE GHC ImGuiTableColumnSortSpecs = data DearImGui.ImGuiTableColumnSortSpecs (DearImGui.ImGuiTableColumnSortSpecs) #-}

postulate
    Storable[ImGuiTableColumnSortSpecs] : Storable ImGuiTableColumnSortSpecs
    Show[ImGuiTableColumnSortSpecs]     : Show ImGuiTableColumnSortSpecs
    Eq[ImGuiTableColumnSortSpecs]       : Eq ImGuiTableColumnSortSpecs

{-# COMPILE GHC Storable[ImGuiTableColumnSortSpecs] = AgdaStorable #-}
{-# COMPILE GHC Show[ImGuiTableColumnSortSpecs]     = AgdaShow     #-}
{-# COMPILE GHC Eq[ImGuiTableColumnSortSpecs]       = AgdaEq       #-}


record ImGuiTableSortSpecs : Set where
    constructor mkImGuiTableSortSpecs
    field
        specs      : Ptr ImGuiTableColumnSortSpecs
        specsCount : CInt
        specsDirty : CBool

{-# COMPILE GHC ImGuiTableSortSpecs = data DearImGui.ImGuiTableSortSpecs (DearImGui.ImGuiTableSortSpecs) #-}

postulate
    Storable[ImGuiTableSortSpecs] : Storable ImGuiTableSortSpecs
    Show[ImGuiTableSortSpecs]     : Show ImGuiTableSortSpecs
    Eq[ImGuiTableSortSpecs]       : Eq ImGuiTableSortSpecs

{-# COMPILE GHC Storable[ImGuiTableSortSpecs] = AgdaStorable #-}
{-# COMPILE GHC Show[ImGuiTableSortSpecs]     = AgdaShow     #-}
{-# COMPILE GHC Eq[ImGuiTableSortSpecs]       = AgdaEq       #-}


-- Context Creation and Access

data Context : Set where
    mkContext : Ptr ImGuiContext → Context

{-# COMPILE GHC Context = data DearImGui.Context (DearImGui.Context) #-}

postulate
    createContext     : ⦃ MonadIO M ⦄ → M (Liftℓ _ Context)
    destroyContext    : ⦃ MonadIO M ⦄ → Context → M ⊤′
    getCurrentContext : ⦃ MonadIO M ⦄ → M (Liftℓ _ Context)
    setCurrentContext : ⦃ MonadIO M ⦄ → Context → M ⊤′

{-# COMPILE GHC createContext     = \ mℓ m AgdaMonadIO -> DearImGui.Raw.createContext     #-}
{-# COMPILE GHC destroyContext    = \ mℓ m AgdaMonadIO -> DearImGui.Raw.destroyContext    #-}
{-# COMPILE GHC getCurrentContext = \ mℓ m AgdaMonadIO -> DearImGui.Raw.getCurrentContext #-}
{-# COMPILE GHC setCurrentContext = \ mℓ m AgdaMonadIO -> DearImGui.Raw.setCurrentContext #-}


-- Main

data DrawData : Set where
    mkDrawData : Ptr ⊤ → DrawData

{-# COMPILE GHC DrawData = data DearImGui.DrawData (DearImGui.DrawData) #-}

postulate
    newFrame     : ⦃ MonadIO M ⦄ → M ⊤′
    endFrame     : ⦃ MonadIO M ⦄ → M ⊤′
    render       : ⦃ MonadIO M ⦄ → M ⊤′
    getDrawData  : ⦃ MonadIO M ⦄ → M (Liftℓ _ DrawData)
    checkVersion : ⦃ MonadIO M ⦄ → M ⊤′

{-# COMPILE GHC newFrame     = \ mℓ m AgdaMonadIO -> DearImGui.Raw.newFrame     #-}
{-# COMPILE GHC endFrame     = \ mℓ m AgdaMonadIO -> DearImGui.Raw.endFrame     #-}
{-# COMPILE GHC render       = \ mℓ m AgdaMonadIO -> DearImGui.Raw.render       #-}
{-# COMPILE GHC getDrawData  = \ mℓ m AgdaMonadIO -> DearImGui.Raw.getDrawData  #-}
{-# COMPILE GHC checkVersion = \ mℓ m AgdaMonadIO -> DearImGui.Raw.checkVersion #-}


-- Demo, Debug, Information

postulate
    showDemoWindow    : ⦃ MonadIO M ⦄ → M ⊤′
    showMetricsWindow : ⦃ MonadIO M ⦄ → M ⊤′
    showAboutWindow   : ⦃ MonadIO M ⦄ → M ⊤′
    showUserGuide     : ⦃ MonadIO M ⦄ → M ⊤′
    getVersion        : ⦃ MonadIO M ⦄ → M (Liftℓ _ CString)

{-# COMPILE GHC showDemoWindow    = \ mℓ m AgdaMonadIO -> DearImGui.Raw.showDemoWindow    #-}
{-# COMPILE GHC showMetricsWindow = \ mℓ m AgdaMonadIO -> DearImGui.Raw.showMetricsWindow #-}
{-# COMPILE GHC showAboutWindow   = \ mℓ m AgdaMonadIO -> DearImGui.Raw.showAboutWindow   #-}
{-# COMPILE GHC showUserGuide     = \ mℓ m AgdaMonadIO -> DearImGui.Raw.showUserGuide     #-}
{-# COMPILE GHC getVersion        = \ mℓ m AgdaMonadIO -> DearImGui.Raw.getVersion        #-}


-- Styles

postulate
    styleColorsDark    : ⦃ MonadIO M ⦄ → M ⊤′
    styleColorsLight   : ⦃ MonadIO M ⦄ → M ⊤′
    styleColorsClassic : ⦃ MonadIO M ⦄ → M ⊤′

{-# COMPILE GHC styleColorsDark    = \ mℓ m AgdaMonadIO -> DearImGui.Raw.styleColorsDark    #-}
{-# COMPILE GHC styleColorsLight   = \ mℓ m AgdaMonadIO -> DearImGui.Raw.styleColorsLight   #-}
{-# COMPILE GHC styleColorsClassic = \ mℓ m AgdaMonadIO -> DearImGui.Raw.styleColorsClassic #-}


-- Windows

postulate
    begin : ⦃ MonadIO M ⦄ → CString → Maybe (Ptr Bool) → Maybe ImGuiWindowFlags → M (Liftℓ _ Bool)
    end   : ⦃ MonadIO M ⦄ → M ⊤′

{-# COMPILE GHC begin = \ mℓ m AgdaMonadIO -> DearImGui.Raw.begin #-}
{-# COMPILE GHC end   = \ mℓ m AgdaMonadIO -> DearImGui.Raw.end   #-}


-- Utilities

postulate
    getWindowDrawList : ⦃ MonadIO M ⦄ → M (Liftℓ _ DrawList)
    getWindowPos      : ⦃ MonadIO M ⦄ → M (Liftℓ _ ImVec2)
    getWindowSize     : ⦃ MonadIO M ⦄ → M (Liftℓ _ ImVec2)
    getWindowWidth    : ⦃ MonadIO M ⦄ → M (Liftℓ _ CFloat)
    getWindowHeight   : ⦃ MonadIO M ⦄ → M (Liftℓ _ CFloat)

{-# COMPILE GHC getWindowDrawList = \ mℓ m AgdaMonadIO -> DearImGui.Raw.getWindowDrawList #-}
{-# COMPILE GHC getWindowPos      = \ mℓ m AgdaMonadIO -> DearImGui.Raw.getWindowPos      #-}
{-# COMPILE GHC getWindowSize     = \ mℓ m AgdaMonadIO -> DearImGui.Raw.getWindowSize     #-}
{-# COMPILE GHC getWindowWidth    = \ mℓ m AgdaMonadIO -> DearImGui.Raw.getWindowWidth    #-}
{-# COMPILE GHC getWindowHeight   = \ mℓ m AgdaMonadIO -> DearImGui.Raw.getWindowHeight   #-}


-- Manipulation

postulate
    setNextWindowPos             : ⦃ MonadIO M ⦄ → Ptr ImVec2 → ImGuiCond → Maybe (Ptr ImVec2) → M ⊤′
    setNextWindowSize            : ⦃ MonadIO M ⦄ → Ptr ImVec2 → ImGuiCond → M ⊤′
    setNextWindowFullscreen      : ⦃ MonadIO M ⦄ → M ⊤′
    setNextWindowContentSize     : ⦃ MonadIO M ⦄ → Ptr ImVec2 → M ⊤′
    setNextWindowSizeConstraints : ⦃ MonadIO M ⦄ → Ptr ImVec2 → Ptr ImVec2 → M ⊤′
    setNextWindowCollapsed       : ⦃ MonadIO M ⦄ → CBool → ImGuiCond → M ⊤′
    setNextWindowBgAlpha         : ⦃ MonadIO M ⦄ → CFloat → M ⊤′

{-# COMPILE GHC setNextWindowPos             = \ mℓ m AgdaMonadIO -> DearImGui.setNextWindowPos             #-}
{-# COMPILE GHC setNextWindowSize            = \ mℓ m AgdaMonadIO -> DearImGui.setNextWindowSize            #-}
{-# COMPILE GHC setNextWindowFullscreen      = \ mℓ m AgdaMonadIO -> DearImGui.setNextWindowFullscreen      #-}
{-# COMPILE GHC setNextWindowContentSize     = \ mℓ m AgdaMonadIO -> DearImGui.setNextWindowContentSize     #-}
{-# COMPILE GHC setNextWindowSizeConstraints = \ mℓ m AgdaMonadIO -> DearImGui.setNextWindowSizeConstraints #-}
{-# COMPILE GHC setNextWindowCollapsed       = \ mℓ m AgdaMonadIO -> DearImGui.setNextWindowCollapsed       #-}
{-# COMPILE GHC setNextWindowBgAlpha         = \ mℓ m AgdaMonadIO -> DearImGui.setNextWindowBgAlpha         #-}


-- Child Windows

postulate
    beginChild        : ⦃ MonadIO M ⦄ → CString → Ptr ImVec2 → CBool → ImGuiWindowFlags → M (Liftℓ _ Bool)
    beginChildContext : ⦃ MonadIO M ⦄ → CString → M (Liftℓ _ Bool)
    endChild          : ⦃ MonadIO M ⦄ → M ⊤′

{-# COMPILE GHC beginChild        = \ mℓ m AgdaMonadIO -> DearImGui.Raw.beginChild        #-}
{-# COMPILE GHC beginChildContext = \ mℓ m AgdaMonadIO -> DearImGui.Raw.beginChildContext #-}
{-# COMPILE GHC endChild          = \ mℓ m AgdaMonadIO -> DearImGui.Raw.endChild          #-}


-- Parameter stacks

postulate
    pushStyleColor : ⦃ MonadIO M ⦄ → ImGuiCol → Ptr ImVec4 → M ⊤′
    popStyleColor  : ⦃ MonadIO M ⦄ → CInt → M ⊤′
    pushStyleVar   : ⦃ MonadIO M ⦄ → ImGuiStyleVar → Ptr ImVec2 → M ⊤′
    popStyleVar    : ⦃ MonadIO M ⦄ → CInt → M ⊤′

{-# COMPILE GHC pushStyleColor = \ mℓ m AgdaMonadIO -> DearImGui.Raw.pushStyleColor #-}
{-# COMPILE GHC popStyleColor  = \ mℓ m AgdaMonadIO -> DearImGui.Raw.popStyleColor  #-}
{-# COMPILE GHC pushStyleVar   = \ mℓ m AgdaMonadIO -> DearImGui.Raw.pushStyleVar   #-}
{-# COMPILE GHC popStyleVar    = \ mℓ m AgdaMonadIO -> DearImGui.Raw.popStyleVar    #-}


-- Cursor/Layout

postulate
    separator               : ⦃ MonadIO M ⦄ → M ⊤′
    sameLine                : ⦃ MonadIO M ⦄ → M ⊤′
    newLine                 : ⦃ MonadIO M ⦄ → M ⊤′
    spacing                 : ⦃ MonadIO M ⦄ → M ⊤′
    dummy                   : ⦃ MonadIO M ⦄ → Ptr ImVec2 → M ⊤′
    indent                  : ⦃ MonadIO M ⦄ → CFloat → M ⊤′
    unindent                : ⦃ MonadIO M ⦄ → CFloat → M ⊤′
    setNextItemWidth        : ⦃ MonadIO M ⦄ → CFloat → M ⊤′
    pushItemWidth           : ⦃ MonadIO M ⦄ → CFloat → M ⊤′
    popItemWidth            : ⦃ MonadIO M ⦄ → M ⊤′
    beginGroup              : ⦃ MonadIO M ⦄ → M ⊤′
    endGroup                : ⦃ MonadIO M ⦄ → M ⊤′
    setCursorPos            : ⦃ MonadIO M ⦄ → Ptr ImVec2 → M ⊤′
    alignTextToFramePadding : ⦃ MonadIO M ⦄ → M ⊤′

{-# COMPILE GHC separator               = \ mℓ m AgdaMonadIO -> DearImGui.Raw.separator               #-}
{-# COMPILE GHC sameLine                = \ mℓ m AgdaMonadIO -> DearImGui.Raw.sameLine                #-}
{-# COMPILE GHC newLine                 = \ mℓ m AgdaMonadIO -> DearImGui.Raw.newLine                 #-}
{-# COMPILE GHC spacing                 = \ mℓ m AgdaMonadIO -> DearImGui.Raw.spacing                 #-}
{-# COMPILE GHC dummy                   = \ mℓ m AgdaMonadIO -> DearImGui.Raw.dummy                   #-}
{-# COMPILE GHC indent                  = \ mℓ m AgdaMonadIO -> DearImGui.Raw.indent                  #-}
{-# COMPILE GHC unindent                = \ mℓ m AgdaMonadIO -> DearImGui.Raw.unindent                #-}
{-# COMPILE GHC setNextItemWidth        = \ mℓ m AgdaMonadIO -> DearImGui.Raw.setNextItemWidth        #-}
{-# COMPILE GHC pushItemWidth           = \ mℓ m AgdaMonadIO -> DearImGui.Raw.pushItemWidth           #-}
{-# COMPILE GHC popItemWidth            = \ mℓ m AgdaMonadIO -> DearImGui.Raw.popItemWidth            #-}
{-# COMPILE GHC beginGroup              = \ mℓ m AgdaMonadIO -> DearImGui.Raw.beginGroup              #-}
{-# COMPILE GHC endGroup                = \ mℓ m AgdaMonadIO -> DearImGui.Raw.endGroup                #-}
{-# COMPILE GHC setCursorPos            = \ mℓ m AgdaMonadIO -> DearImGui.Raw.setCursorPos            #-}
{-# COMPILE GHC alignTextToFramePadding = \ mℓ m AgdaMonadIO -> DearImGui.Raw.alignTextToFramePadding #-}


-- Widgets
postulate
    -- Text
    textUnformatted : ⦃ MonadIO M ⦄ → CString → Maybe CString → M ⊤′
    textColored     : ⦃ MonadIO M ⦄ → Ptr ImVec4 → CString → M ⊤′
    textDisabled    : ⦃ MonadIO M ⦄ → CString → M ⊤′
    textWrapped     : ⦃ MonadIO M ⦄ → CString → M ⊤′
    labelText       : ⦃ MonadIO M ⦄ → CString → CString → M ⊤′
    bulletText      : ⦃ MonadIO M ⦄ → CString → M ⊤′

    -- Main
    button          : ⦃ MonadIO M ⦄ → CString → M (Liftℓ _ Bool)
    smallButton     : ⦃ MonadIO M ⦄ → CString → M (Liftℓ _ Bool)
    invisibleButton : ⦃ MonadIO M ⦄ → CString → Ptr ImVec2 → ImGuiButtonFlags → M (Liftℓ _ Bool)
    arrowButton     : ⦃ MonadIO M ⦄ → CString → ImGuiDir → M (Liftℓ _ Bool)
    image           : ⦃ MonadIO M ⦄ → Ptr ⊤ → Ptr ImVec2 → Ptr ImVec2 → Ptr ImVec2 → Ptr ImVec4 → Ptr ImVec4 → M ⊤′
    imageButton     : ⦃ MonadIO M ⦄ → Ptr ⊤ → Ptr ImVec2 → Ptr ImVec2 → Ptr ImVec2 → CInt → Ptr ImVec4 → Ptr ImVec4 → M (Liftℓ _ Bool)
    checkbox        : ⦃ MonadIO M ⦄ → CString → Ptr CBool → M (Liftℓ _ Bool)
    progressBar     : ⦃ MonadIO M ⦄ → CFloat → CString → M ⊤′
    bullet          : ⦃ MonadIO M ⦄ → M ⊤′

    -- Combo Box
    beginCombo : ⦃ MonadIO M ⦄ → CString → CString → M (Liftℓ _ Bool)
    endCombo   : ⦃ MonadIO M ⦄ → M ⊤′
    combo      : ⦃ MonadIO M ⦄ → CString → Ptr CInt → Ptr CString → CInt → M (Liftℓ _ Bool)

    -- Drag Sliders
    dragFloat       : ⦃ MonadIO M ⦄ → CString → Ptr CFloat → CFloat → CFloat → CFloat → M (Liftℓ _ Bool)
    dragFloat2      : ⦃ MonadIO M ⦄ → CString → Ptr CFloat → CFloat → CFloat → CFloat → M (Liftℓ _ Bool)
    dragFloat3      : ⦃ MonadIO M ⦄ → CString → Ptr CFloat → CFloat → CFloat → CFloat → M (Liftℓ _ Bool)
    dragFloat4      : ⦃ MonadIO M ⦄ → CString → Ptr CFloat → CFloat → CFloat → CFloat → M (Liftℓ _ Bool)
    dragFloatRange2 : ⦃ MonadIO M ⦄ → CString → Ptr CFloat → Ptr CFloat → CFloat → CFloat → CFloat → CString → CString → ImGuiSliderFlags → M (Liftℓ _ Bool)
    dragInt         : ⦃ MonadIO M ⦄ → CString → Ptr CInt → CFloat → CInt → CInt → CString → ImGuiSliderFlags → M (Liftℓ _ Bool)
    dragInt2        : ⦃ MonadIO M ⦄ → CString → Ptr CInt → CFloat → CInt → CInt → CString → ImGuiSliderFlags → M (Liftℓ _ Bool)
    dragInt3        : ⦃ MonadIO M ⦄ → CString → Ptr CInt → CFloat → CInt → CInt → CString → ImGuiSliderFlags → M (Liftℓ _ Bool)
    dragInt4        : ⦃ MonadIO M ⦄ → CString → Ptr CInt → CFloat → CInt → CInt → CString → ImGuiSliderFlags → M (Liftℓ _ Bool)
    dragIntRange2   : ⦃ MonadIO M ⦄ → CString → Ptr CInt → Ptr CInt → CFloat → CInt → CInt → CString → CString → ImGuiSliderFlags → M (Liftℓ _ Bool)
    dragScalar      : ⦃ MonadIO M ⦄ → CString → ImGuiDataType → Ptr A → CFloat → Ptr A → Ptr A → CString → ImGuiSliderFlags → M (Liftℓ _ Bool)
    dragScalarN     : ⦃ MonadIO M ⦄ → CString → ImGuiDataType → Ptr A → CInt → CFloat → Ptr A → Ptr A → CString → ImGuiSliderFlags → M (Liftℓ _ Bool)

    -- Slider
    sliderFloat   : ⦃ MonadIO M ⦄ → CString → Ptr CFloat → CFloat → CFloat → M (Liftℓ _ Bool)
    sliderFloat2  : ⦃ MonadIO M ⦄ → CString → Ptr CFloat → CFloat → CFloat → M (Liftℓ _ Bool)
    sliderFloat3  : ⦃ MonadIO M ⦄ → CString → Ptr CFloat → CFloat → CFloat → M (Liftℓ _ Bool)
    sliderFloat4  : ⦃ MonadIO M ⦄ → CString → Ptr CFloat → CFloat → CFloat → M (Liftℓ _ Bool)
    sliderAngle   : ⦃ MonadIO M ⦄ → CString → Ptr CFloat → CFloat → CFloat → CString → ImGuiSliderFlags → M (Liftℓ _ Bool)
    sliderInt     : ⦃ MonadIO M ⦄ → CString → Ptr CInt → CInt → CInt → CString → ImGuiSliderFlags → M (Liftℓ _ Bool)
    sliderInt2    : ⦃ MonadIO M ⦄ → CString → Ptr CInt → CInt → CInt → CString → ImGuiSliderFlags → M (Liftℓ _ Bool)
    sliderInt3    : ⦃ MonadIO M ⦄ → CString → Ptr CInt → CInt → CInt → CString → ImGuiSliderFlags → M (Liftℓ _ Bool)
    sliderInt4    : ⦃ MonadIO M ⦄ → CString → Ptr CInt → CInt → CInt → CString → ImGuiSliderFlags → M (Liftℓ _ Bool)
    sliderScalar  : ⦃ MonadIO M ⦄ → CString → ImGuiDataType → Ptr A → Ptr A → Ptr A → CString → ImGuiSliderFlags → M (Liftℓ _ Bool)
    sliderScalarN : ⦃ MonadIO M ⦄ → CString → ImGuiDataType → Ptr A → CInt → Ptr A → Ptr A → CString → ImGuiSliderFlags → M (Liftℓ _ Bool)
    vSliderFloat  : ⦃ MonadIO M ⦄ → CString → Ptr ImVec2 → Ptr CFloat → CFloat → CFloat → CString → ImGuiSliderFlags → M (Liftℓ _ Bool)
    vSliderInt    : ⦃ MonadIO M ⦄ → CString → Ptr ImVec2 → Ptr CInt → CInt → CInt → CString → ImGuiSliderFlags → M (Liftℓ _ Bool)
    vSliderScalar : ⦃ MonadIO M ⦄ → CString → Ptr ImVec2 → ImGuiDataType → Ptr A → Ptr A → Ptr A → CString → ImGuiSliderFlags → M (Liftℓ _ Bool)

    -- Text Input
    inputText          : ⦃ MonadIO M ⦄ → CString → CStringLen → ImGuiInputTextFlags → M (Liftℓ _ Bool)
    inputTextMultiline : ⦃ MonadIO M ⦄ → CString → CStringLen → Ptr ImVec2 → ImGuiInputTextFlags → M (Liftℓ _ Bool)
    inputTextWithHint  : ⦃ MonadIO M ⦄ → CString → CString → CStringLen → ImGuiInputTextFlags → M (Liftℓ _ Bool)

    -- Color Editor/Picker
    colorPicker3 : ⦃ MonadIO M ⦄ → CString → Ptr CFloat → M (Liftℓ _ Bool)
    colorButton  : ⦃ MonadIO M ⦄ → CString → Ptr ImVec4 → M (Liftℓ _ Bool)

    -- Tables
    beginTable               : ⦃ MonadIO M ⦄ → CString → CInt → ImGuiTableFlags → Ptr ImVec2 → CFloat → M (Liftℓ _ Bool)
    endTable                 : ⦃ MonadIO M ⦄ → M ⊤′
    tableNextRow             : ⦃ MonadIO M ⦄ → ImGuiTableRowFlags → CFloat → M ⊤′
    tableNextColumn          : ⦃ MonadIO M ⦄ → M (Liftℓ _ Bool)
    tableSetColumnIndex      : ⦃ MonadIO M ⦄ → CInt → M (Liftℓ _ Bool)
    tableSetupColumn         : ⦃ MonadIO M ⦄ → CString → ImGuiTableColumnFlags → CFloat → ImGuiID → M ⊤′
    tableSetupScrollFreeze   : ⦃ MonadIO M ⦄ → CInt → CInt → M ⊤′
    tableHeadersRow          : ⦃ MonadIO M ⦄ → M ⊤′
    tableHeader              : ⦃ MonadIO M ⦄ → CString → M ⊤′
    tableGetSortSpecs        : ⦃ MonadIO M ⦄ → M (Liftℓ _ (Maybe (Ptr ImGuiTableSortSpecs)))
    tableClearSortSpecsDirty : ⦃ MonadIO M ⦄ → Ptr ImGuiTableSortSpecs → M ⊤′
    tableGetColumnCount      : ⦃ MonadIO M ⦄ → M (Liftℓ _ CInt)
    tableGetColumnIndex      : ⦃ MonadIO M ⦄ → M (Liftℓ _ CInt)
    tableGetRowIndex         : ⦃ MonadIO M ⦄ → M (Liftℓ _ CInt)
    tableGetColumnName       : ⦃ MonadIO M ⦄ → Maybe CInt → M (Liftℓ _ CString)
    tableGetColumnFlags      : ⦃ MonadIO M ⦄ → Maybe CInt → M (Liftℓ _ ImGuiTableColumnFlags)
    tableSetColumnEnabled    : ⦃ MonadIO M ⦄ → CInt → CBool → M ⊤′
    tableSetBgColor          : ⦃ MonadIO M ⦄ → ImGuiTableBgTarget → ImU32 → Maybe CInt → M ⊤′

    -- Trees
    treeNode : ⦃ MonadIO M ⦄ → CString → M (Liftℓ _ Bool)
    treePush : ⦃ MonadIO M ⦄ → CString → M ⊤′
    treePop  : ⦃ MonadIO M ⦄ → M ⊤′

    -- Selectables
    selectable : ⦃ MonadIO M ⦄ → CString → CBool → ImGuiSelectableFlags → Ptr ImVec2 → M (Liftℓ _ Bool)

    -- List Boxes
    listBox : ⦃ MonadIO M ⦄ → CString → Ptr CInt → Ptr CString → CInt → M (Liftℓ _ Bool)

    -- Data Plotting
    plotHistogram : ⦃ MonadIO M ⦄ → CString → Ptr CFloat → CInt → M ⊤′

    -- Menus
    beginMenuBar     : ⦃ MonadIO M ⦄ → M (Liftℓ _ Bool)
    endMenuBar       : ⦃ MonadIO M ⦄ → M ⊤′
    beginMainMenuBar : ⦃ MonadIO M ⦄ → M (Liftℓ _ Bool)
    endMainMenuBar   : ⦃ MonadIO M ⦄ → M ⊤′
    beginMenu        : ⦃ MonadIO M ⦄ → CString → M (Liftℓ _ Bool)
    endMenu          : ⦃ MonadIO M ⦄ → M ⊤′
    menuItem         : ⦃ MonadIO M ⦄ → CString → M (Liftℓ _ Bool)

    -- Tabs, tab bar
    beginTabBar      : ⦃ MonadIO M ⦄ → CString → ImGuiTabBarFlags → M (Liftℓ _ Bool)
    endTabBar        : ⦃ MonadIO M ⦄ → M ⊤′
    beginTabItem     : ⦃ MonadIO M ⦄ → CString → Ptr CBool → ImGuiTabBarFlags → M (Liftℓ _ Bool)
    endTabItem       : ⦃ MonadIO M ⦄ → M ⊤′
    tabItemButton    : ⦃ MonadIO M ⦄ → CString → ImGuiTabItemFlags → M (Liftℓ _ Bool)
    setTabItemClosed : ⦃ MonadIO M ⦄ → CString → M ⊤′

    -- Tooltips
    beginTooltip : ⦃ MonadIO M ⦄ → M ⊤′
    endTooltip   : ⦃ MonadIO M ⦄ → M ⊤′

    -- Popups/Modals
    beginPopup              : ⦃ MonadIO M ⦄ → CString → M (Liftℓ _ Bool)
    beginPopupModal         : ⦃ MonadIO M ⦄ → CString → M (Liftℓ _ Bool)
    endPopup                : ⦃ MonadIO M ⦄ → M ⊤′
    openPopup               : ⦃ MonadIO M ⦄ → CString → M ⊤′
    openPopupOnItemClick    : ⦃ MonadIO M ⦄ → CString → ImGuiPopupFlags → M ⊤′
    closeCurrentPopup       : ⦃ MonadIO M ⦄ → M ⊤′
    beginPopupContextItem   : ⦃ MonadIO M ⦄ → CString → ImGuiPopupFlags → M (Liftℓ _ Bool)
    beginPopupContextWindow : ⦃ MonadIO M ⦄ → CString → ImGuiPopupFlags → M (Liftℓ _ Bool)
    beginPopupContextVoid   : ⦃ MonadIO M ⦄ → CString → ImGuiPopupFlags → M (Liftℓ _ Bool)
    isPopupOpen             : ⦃ MonadIO M ⦄ → CString → ImGuiPopupFlags → M (Liftℓ _ Bool)

    -- ID stack/scopes
    pushIDInt    : ⦃ MonadIO M ⦄ → CInt → M ⊤′
    pushIDPtr    : ⦃ MonadIO M ⦄ → Ptr A → M ⊤′
    pushIDStr    : ⦃ MonadIO M ⦄ → CString → M ⊤′
    pushIDStrLen : ⦃ MonadIO M ⦄ → CStringLen → M ⊤′
    popID        : ⦃ MonadIO M ⦄ → M ⊤′

{-# COMPILE GHC textUnformatted = \ mℓ m AgdaMonadIO -> DearImGui.Raw.textUnformatted #-}
{-# COMPILE GHC textColored     = \ mℓ m AgdaMonadIO -> DearImGui.Raw.textColored     #-}
{-# COMPILE GHC textDisabled    = \ mℓ m AgdaMonadIO -> DearImGui.Raw.textDisabled    #-}
{-# COMPILE GHC textWrapped     = \ mℓ m AgdaMonadIO -> DearImGui.Raw.textWrapped     #-}
{-# COMPILE GHC labelText       = \ mℓ m AgdaMonadIO -> DearImGui.Raw.labelText       #-}
{-# COMPILE GHC bulletText      = \ mℓ m AgdaMonadIO -> DearImGui.Raw.bulletText      #-}

{-# COMPILE GHC button          = \ mℓ m AgdaMonadIO -> DearImGui.Raw.button          #-}
{-# COMPILE GHC smallButton     = \ mℓ m AgdaMonadIO -> DearImGui.Raw.smallButton     #-}
{-# COMPILE GHC invisibleButton = \ mℓ m AgdaMonadIO -> DearImGui.Raw.invisibleButton #-}
{-# COMPILE GHC arrowButton     = \ mℓ m AgdaMonadIO -> DearImGui.Raw.arrowButton     #-}
{-# COMPILE GHC image           = \ mℓ m AgdaMonadIO -> DearImGui.Raw.image           #-}
{-# COMPILE GHC imageButton     = \ mℓ m AgdaMonadIO -> DearImGui.Raw.imageButton     #-}
{-# COMPILE GHC checkbox        = \ mℓ m AgdaMonadIO -> DearImGui.Raw.checkbox        #-}
{-# COMPILE GHC progressBar     = \ mℓ m AgdaMonadIO -> DearImGui.Raw.progressBar     #-}
{-# COMPILE GHC bullet          = \ mℓ m AgdaMonadIO -> DearImGui.Raw.bullet          #-}

{-# COMPILE GHC beginCombo = \ mℓ m AgdaMonadIO -> DearImGui.Raw.beginCombo #-}
{-# COMPILE GHC endCombo   = \ mℓ m AgdaMonadIO -> DearImGui.Raw.endCombo   #-}
{-# COMPILE GHC combo      = \ mℓ m AgdaMonadIO -> DearImGui.Raw.combo      #-}

{-# COMPILE GHC dragFloat       = \ mℓ m AgdaMonadIO      -> DearImGui.Raw.dragFloat       #-}
{-# COMPILE GHC dragFloat2      = \ mℓ m AgdaMonadIO      -> DearImGui.Raw.dragFloat2      #-}
{-# COMPILE GHC dragFloat3      = \ mℓ m AgdaMonadIO      -> DearImGui.Raw.dragFloat3      #-}
{-# COMPILE GHC dragFloat4      = \ mℓ m AgdaMonadIO      -> DearImGui.Raw.dragFloat4      #-}
{-# COMPILE GHC dragFloatRange2 = \ mℓ m AgdaMonadIO      -> DearImGui.Raw.dragFloatRange2 #-}
{-# COMPILE GHC dragInt         = \ mℓ m AgdaMonadIO      -> DearImGui.Raw.dragInt         #-}
{-# COMPILE GHC dragInt2        = \ mℓ m AgdaMonadIO      -> DearImGui.Raw.dragInt2        #-}
{-# COMPILE GHC dragInt3        = \ mℓ m AgdaMonadIO      -> DearImGui.Raw.dragInt3        #-}
{-# COMPILE GHC dragInt4        = \ mℓ m AgdaMonadIO      -> DearImGui.Raw.dragInt4        #-}
{-# COMPILE GHC dragIntRange2   = \ mℓ m AgdaMonadIO      -> DearImGui.Raw.dragIntRange2   #-}
{-# COMPILE GHC dragScalar      = \ mℓ m aℓ a AgdaMonadIO -> DearImGui.Raw.dragScalar      #-}
{-# COMPILE GHC dragScalarN     = \ mℓ m aℓ a AgdaMonadIO -> DearImGui.Raw.dragScalarN     #-}

{-# COMPILE GHC sliderFloat   = \ mℓ m AgdaMonadIO      -> DearImGui.Raw.sliderFloat   #-}
{-# COMPILE GHC sliderFloat2  = \ mℓ m AgdaMonadIO      -> DearImGui.Raw.sliderFloat2  #-}
{-# COMPILE GHC sliderFloat3  = \ mℓ m AgdaMonadIO      -> DearImGui.Raw.sliderFloat3  #-}
{-# COMPILE GHC sliderFloat4  = \ mℓ m AgdaMonadIO      -> DearImGui.Raw.sliderFloat4  #-}
{-# COMPILE GHC sliderAngle   = \ mℓ m AgdaMonadIO      -> DearImGui.Raw.sliderAngle   #-}
{-# COMPILE GHC sliderInt     = \ mℓ m AgdaMonadIO      -> DearImGui.Raw.sliderInt     #-}
{-# COMPILE GHC sliderInt2    = \ mℓ m AgdaMonadIO      -> DearImGui.Raw.sliderInt2    #-}
{-# COMPILE GHC sliderInt3    = \ mℓ m AgdaMonadIO      -> DearImGui.Raw.sliderInt3    #-}
{-# COMPILE GHC sliderInt4    = \ mℓ m AgdaMonadIO      -> DearImGui.Raw.sliderInt4    #-}
{-# COMPILE GHC sliderScalar  = \ mℓ m aℓ a AgdaMonadIO -> DearImGui.Raw.sliderScalar  #-}
{-# COMPILE GHC sliderScalarN = \ mℓ m aℓ a AgdaMonadIO -> DearImGui.Raw.sliderScalarN #-}
{-# COMPILE GHC vSliderFloat  = \ mℓ m AgdaMonadIO      -> DearImGui.Raw.vSliderFloat  #-}
{-# COMPILE GHC vSliderInt    = \ mℓ m AgdaMonadIO      -> DearImGui.Raw.vSliderInt    #-}
{-# COMPILE GHC vSliderScalar = \ mℓ m aℓ a AgdaMonadIO -> DearImGui.Raw.vSliderScalar #-}

{-# COMPILE GHC inputText          = \ mℓ m AgdaMonadIO -> DearImGui.Raw.inputText          #-}
{-# COMPILE GHC inputTextMultiline = \ mℓ m AgdaMonadIO -> DearImGui.Raw.inputTextMultiline #-}
{-# COMPILE GHC inputTextWithHint  = \ mℓ m AgdaMonadIO -> DearImGui.Raw.inputTextWithHint  #-}

{-# COMPILE GHC colorPicker3 = \ mℓ m AgdaMonadIO -> DearImGui.Raw.colorPicker3 #-}
{-# COMPILE GHC colorButton  = \ mℓ m AgdaMonadIO -> DearImGui.Raw.colorButton  #-}

{-# COMPILE GHC beginTable               = \ mℓ m AgdaMonadIO -> DearImGui.Raw.beginTable               #-}
{-# COMPILE GHC endTable                 = \ mℓ m AgdaMonadIO -> DearImGui.Raw.endTable                 #-}
{-# COMPILE GHC tableNextRow             = \ mℓ m AgdaMonadIO -> DearImGui.Raw.tableNextRow             #-}
{-# COMPILE GHC tableNextColumn          = \ mℓ m AgdaMonadIO -> DearImGui.Raw.tableNextColumn          #-}
{-# COMPILE GHC tableSetColumnIndex      = \ mℓ m AgdaMonadIO -> DearImGui.Raw.tableSetColumnIndex      #-}
{-# COMPILE GHC tableSetupColumn         = \ mℓ m AgdaMonadIO -> DearImGui.Raw.tableSetupColumn         #-}
{-# COMPILE GHC tableSetupScrollFreeze   = \ mℓ m AgdaMonadIO -> DearImGui.Raw.tableSetupScrollFreeze   #-}
{-# COMPILE GHC tableHeadersRow          = \ mℓ m AgdaMonadIO -> DearImGui.Raw.tableHeadersRow          #-}
{-# COMPILE GHC tableHeader              = \ mℓ m AgdaMonadIO -> DearImGui.Raw.tableHeader              #-}
{-# COMPILE GHC tableGetSortSpecs        = \ mℓ m AgdaMonadIO -> DearImGui.Raw.tableGetSortSpecs        #-}
{-# COMPILE GHC tableClearSortSpecsDirty = \ mℓ m AgdaMonadIO -> DearImGui.Raw.tableClearSortSpecsDirty #-}
{-# COMPILE GHC tableGetColumnCount      = \ mℓ m AgdaMonadIO -> DearImGui.Raw.tableGetColumnCount      #-}
{-# COMPILE GHC tableGetColumnIndex      = \ mℓ m AgdaMonadIO -> DearImGui.Raw.tableGetColumnIndex      #-}
{-# COMPILE GHC tableGetRowIndex         = \ mℓ m AgdaMonadIO -> DearImGui.Raw.tableGetRowIndex         #-}
{-# COMPILE GHC tableGetColumnName       = \ mℓ m AgdaMonadIO -> DearImGui.Raw.tableGetColumnName       #-}
{-# COMPILE GHC tableGetColumnFlags      = \ mℓ m AgdaMonadIO -> DearImGui.Raw.tableGetColumnFlags      #-}
{-# COMPILE GHC tableSetColumnEnabled    = \ mℓ m AgdaMonadIO -> DearImGui.Raw.tableSetColumnEnabled    #-}
{-# COMPILE GHC tableSetBgColor          = \ mℓ m AgdaMonadIO -> DearImGui.Raw.tableSetBgColor          #-}

{-# COMPILE GHC treeNode = \ mℓ m AgdaMonadIO -> DearImGui.Raw.treeNode #-}
{-# COMPILE GHC treePush = \ mℓ m AgdaMonadIO -> DearImGui.Raw.treePush #-}
{-# COMPILE GHC treePop  = \ mℓ m AgdaMonadIO -> DearImGui.Raw.treePop #-}

{-# COMPILE GHC selectable = \ mℓ m AgdaMonadIO -> DearImGui.Raw.selectable #-}

{-# COMPILE GHC listBox = \ mℓ m AgdaMonadIO -> DearImGui.Raw.listBox #-}

{-# COMPILE GHC plotHistogram = \ mℓ m AgdaMonadIO -> DearImGui.Raw.plotHistogram #-}

{-# COMPILE GHC beginMenuBar     = \ mℓ m AgdaMonadIO -> DearImGui.Raw.beginMenuBar     #-}
{-# COMPILE GHC endMenuBar       = \ mℓ m AgdaMonadIO -> DearImGui.Raw.endMenuBar       #-}
{-# COMPILE GHC beginMainMenuBar = \ mℓ m AgdaMonadIO -> DearImGui.Raw.beginMainMenuBar #-}
{-# COMPILE GHC endMainMenuBar   = \ mℓ m AgdaMonadIO -> DearImGui.Raw.endMainMenuBar   #-}
{-# COMPILE GHC beginMenu        = \ mℓ m AgdaMonadIO -> DearImGui.Raw.beginMenu        #-}
{-# COMPILE GHC endMenu          = \ mℓ m AgdaMonadIO -> DearImGui.Raw.endMenu          #-}
{-# COMPILE GHC menuItem         = \ mℓ m AgdaMonadIO -> DearImGui.Raw.menuItem         #-}

{-# COMPILE GHC beginTabBar      = \ mℓ m AgdaMonadIO -> DearImGui.Raw.beginTabBar      #-}
{-# COMPILE GHC endTabBar        = \ mℓ m AgdaMonadIO -> DearImGui.Raw.endTabBar        #-}
{-# COMPILE GHC beginTabItem     = \ mℓ m AgdaMonadIO -> DearImGui.Raw.beginTabItem     #-}
{-# COMPILE GHC endTabItem       = \ mℓ m AgdaMonadIO -> DearImGui.Raw.endTabItem       #-}
{-# COMPILE GHC tabItemButton    = \ mℓ m AgdaMonadIO -> DearImGui.Raw.tabItemButton    #-}
{-# COMPILE GHC setTabItemClosed = \ mℓ m AgdaMonadIO -> DearImGui.Raw.setTabItemClosed #-}

{-# COMPILE GHC beginTooltip = \ mℓ m AgdaMonadIO -> DearImGui.Raw.beginTooltip #-}
{-# COMPILE GHC endTooltip   = \ mℓ m AgdaMonadIO -> DearImGui.Raw.endTooltip   #-}

{-# COMPILE GHC beginPopup              = \ mℓ m AgdaMonadIO -> DearImGui.Raw.beginPopup              #-}
{-# COMPILE GHC beginPopupModal         = \ mℓ m AgdaMonadIO -> DearImGui.Raw.beginPopupModal         #-}
{-# COMPILE GHC endPopup                = \ mℓ m AgdaMonadIO -> DearImGui.Raw.endPopup                #-}
{-# COMPILE GHC openPopup               = \ mℓ m AgdaMonadIO -> DearImGui.Raw.openPopup               #-}
{-# COMPILE GHC openPopupOnItemClick    = \ mℓ m AgdaMonadIO -> DearImGui.Raw.openPopupOnItemClick    #-}
{-# COMPILE GHC closeCurrentPopup       = \ mℓ m AgdaMonadIO -> DearImGui.Raw.closeCurrentPopup       #-}
{-# COMPILE GHC beginPopupContextItem   = \ mℓ m AgdaMonadIO -> DearImGui.Raw.beginPopupContextItem   #-}
{-# COMPILE GHC beginPopupContextWindow = \ mℓ m AgdaMonadIO -> DearImGui.Raw.beginPopupContextWindow #-}
{-# COMPILE GHC beginPopupContextVoid   = \ mℓ m AgdaMonadIO -> DearImGui.Raw.beginPopupContextVoid   #-}
{-# COMPILE GHC isPopupOpen             = \ mℓ m AgdaMonadIO -> DearImGui.Raw.isPopupOpen             #-}

{-# COMPILE GHC pushIDInt    = \ mℓ m      AgdaMonadIO -> DearImGui.Raw.pushIDInt    #-}
{-# COMPILE GHC pushIDPtr    = \ mℓ m aℓ a AgdaMonadIO -> DearImGui.Raw.pushIDPtr    #-}
{-# COMPILE GHC pushIDStr    = \ mℓ m      AgdaMonadIO -> DearImGui.Raw.pushIDStr    #-}
{-# COMPILE GHC pushIDStrLen = \ mℓ m      AgdaMonadIO -> DearImGui.Raw.pushIDStrLen #-}
{-# COMPILE GHC popID        = \ mℓ m      AgdaMonadIO -> DearImGui.Raw.popID        #-}


-- Item/Widgets Utilities

postulate
    isItemHovered       : ⦃ MonadIO M ⦄ → M (Liftℓ _ Bool)
    wantCaptureMouse    : ⦃ MonadIO M ⦄ → M (Liftℓ _ Bool)
    wantCaptureKeyboard : ⦃ MonadIO M ⦄ → M (Liftℓ _ Bool)

{-# COMPILE GHC isItemHovered       = \ mℓ m AgdaMonadIO -> DearImGui.Raw.isItemHovered       #-}
{-# COMPILE GHC wantCaptureMouse    = \ mℓ m AgdaMonadIO -> DearImGui.Raw.wantCaptureMouse    #-}
{-# COMPILE GHC wantCaptureKeyboard = \ mℓ m AgdaMonadIO -> DearImGui.Raw.wantCaptureKeyboard #-}


-- Utilities

postulate
    -- Miscellaneous
    getBackgroundDrawList : ⦃ MonadIO M ⦄ → M (Liftℓ _ DrawList)
    getForegroundDrawList : ⦃ MonadIO M ⦄ → M (Liftℓ _ DrawList)
    imCol32               : CUChar → CUChar → CUChar → CUChar → ImU32

{-# COMPILE GHC getBackgroundDrawList = \ mℓ m AgdaMonadIO -> DearImGui.Raw.getBackgroundDrawList #-}
{-# COMPILE GHC getForegroundDrawList = \ mℓ m AgdaMonadIO -> DearImGui.Raw.getForegroundDrawList #-}
{-# COMPILE GHC imCol32               =                       DearImGui.Raw.imCol32               #-}
