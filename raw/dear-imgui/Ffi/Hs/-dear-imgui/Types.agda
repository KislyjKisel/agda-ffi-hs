{-# OPTIONS --without-K #-}

module Ffi.Hs.-dear-imgui.Types where

open import Ffi.Hs.-base.Class
open import Ffi.Hs.-base.Float     using (Float)
open import Ffi.Hs.Data.Int        using (Int16)
open import Ffi.Hs.Data.Word       using (Word32)
open import Ffi.Hs.Foreign.C.Types using (CInt)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified DearImGui.Raw
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}


data ImGuiWindowFlags : Set where
    mkImGuiWindowFlags : CInt → ImGuiWindowFlags

{-# COMPILE GHC ImGuiWindowFlags = data DearImGui.Raw.ImGuiWindowFlags (DearImGui.Raw.ImGuiWindowFlags) #-}

postulate
    Storable[ImGuiWindowFlags] : Storable ImGuiWindowFlags
    Bits[ImGuiWindowFlags]     : Bits ImGuiWindowFlags
    Show[ImGuiWindowFlags]     : Show ImGuiWindowFlags
    Eq[ImGuiWindowFlags]       : Eq ImGuiWindowFlags
    Ord[ImGuiWindowFlags]      : Ord ImGuiWindowFlags

{-# COMPILE GHC Storable[ImGuiWindowFlags] = AgdaStorable #-}
{-# COMPILE GHC Bits[ImGuiWindowFlags]     = AgdaBits     #-}
{-# COMPILE GHC Show[ImGuiWindowFlags]     = AgdaShow     #-}
{-# COMPILE GHC Eq[ImGuiWindowFlags]       = AgdaEq       #-}
{-# COMPILE GHC Ord[ImGuiWindowFlags]      = AgdaOrd      #-}


data ImGuiInputTextFlags : Set where
    mkImGuiInputTextFlags : CInt → ImGuiInputTextFlags

{-# COMPILE GHC ImGuiInputTextFlags = data DearImGui.Raw.ImGuiInputTextFlags (DearImGui.Raw.ImGuiInputTextFlags) #-}

postulate
    Storable[ImGuiInputTextFlags] : Storable ImGuiInputTextFlags
    Bits[ImGuiInputTextFlags]     : Bits ImGuiInputTextFlags
    Show[ImGuiInputTextFlags]     : Show ImGuiInputTextFlags
    Eq[ImGuiInputTextFlags]       : Eq ImGuiInputTextFlags
    Ord[ImGuiInputTextFlags]      : Ord ImGuiInputTextFlags

{-# COMPILE GHC Storable[ImGuiInputTextFlags] = AgdaStorable #-}
{-# COMPILE GHC Bits[ImGuiInputTextFlags]     = AgdaBits     #-}
{-# COMPILE GHC Show[ImGuiInputTextFlags]     = AgdaShow     #-}
{-# COMPILE GHC Eq[ImGuiInputTextFlags]       = AgdaEq       #-}
{-# COMPILE GHC Ord[ImGuiInputTextFlags]      = AgdaOrd      #-}


data ImGuiTreeNodeFlags : Set where
    mkImGuiTreeNodeFlags : CInt → ImGuiTreeNodeFlags

{-# COMPILE GHC ImGuiTreeNodeFlags = data DearImGui.Raw.ImGuiTreeNodeFlags (DearImGui.Raw.ImGuiTreeNodeFlags) #-}

postulate
    Storable[ImGuiTreeNodeFlags] : Storable ImGuiTreeNodeFlags
    Bits[ImGuiTreeNodeFlags]     : Bits ImGuiTreeNodeFlags
    Show[ImGuiTreeNodeFlags]     : Show ImGuiTreeNodeFlags
    Eq[ImGuiTreeNodeFlags]       : Eq ImGuiTreeNodeFlags
    Ord[ImGuiTreeNodeFlags]      : Ord ImGuiTreeNodeFlags

{-# COMPILE GHC Storable[ImGuiTreeNodeFlags] = AgdaStorable #-}
{-# COMPILE GHC Bits[ImGuiTreeNodeFlags]     = AgdaBits     #-}
{-# COMPILE GHC Show[ImGuiTreeNodeFlags]     = AgdaShow     #-}
{-# COMPILE GHC Eq[ImGuiTreeNodeFlags]       = AgdaEq       #-}
{-# COMPILE GHC Ord[ImGuiTreeNodeFlags]      = AgdaOrd      #-}


data ImGuiPopupFlags : Set where
    mkImGuiPopupFlags : CInt → ImGuiPopupFlags

{-# COMPILE GHC ImGuiPopupFlags = data DearImGui.Raw.ImGuiPopupFlags (DearImGui.Raw.ImGuiPopupFlags) #-}

postulate
    Storable[ImGuiPopupFlags] : Storable ImGuiPopupFlags
    Bits[ImGuiPopupFlags]     : Bits ImGuiPopupFlags
    Show[ImGuiPopupFlags]     : Show ImGuiPopupFlags
    Eq[ImGuiPopupFlags]       : Eq ImGuiPopupFlags
    Ord[ImGuiPopupFlags]      : Ord ImGuiPopupFlags

{-# COMPILE GHC Storable[ImGuiPopupFlags] = AgdaStorable #-}
{-# COMPILE GHC Bits[ImGuiPopupFlags]     = AgdaBits     #-}
{-# COMPILE GHC Show[ImGuiPopupFlags]     = AgdaShow     #-}
{-# COMPILE GHC Eq[ImGuiPopupFlags]       = AgdaEq       #-}
{-# COMPILE GHC Ord[ImGuiPopupFlags]      = AgdaOrd      #-}


data ImGuiSelectableFlags : Set where
    mkImGuiSelectableFlags : CInt → ImGuiSelectableFlags

{-# COMPILE GHC ImGuiSelectableFlags = data DearImGui.Raw.ImGuiSelectableFlags (DearImGui.Raw.ImGuiSelectableFlags) #-}

postulate
    Storable[ImGuiSelectableFlags] : Storable ImGuiSelectableFlags
    Bits[ImGuiSelectableFlags]     : Bits ImGuiSelectableFlags
    Show[ImGuiSelectableFlags]     : Show ImGuiSelectableFlags
    Eq[ImGuiSelectableFlags]       : Eq ImGuiSelectableFlags
    Ord[ImGuiSelectableFlags]      : Ord ImGuiSelectableFlags

{-# COMPILE GHC Storable[ImGuiSelectableFlags] = AgdaStorable #-}
{-# COMPILE GHC Bits[ImGuiSelectableFlags]     = AgdaBits     #-}
{-# COMPILE GHC Show[ImGuiSelectableFlags]     = AgdaShow     #-}
{-# COMPILE GHC Eq[ImGuiSelectableFlags]       = AgdaEq       #-}
{-# COMPILE GHC Ord[ImGuiSelectableFlags]      = AgdaOrd      #-}


data ImGuiComboFlags : Set where
    mkImGuiComboFlags : CInt → ImGuiComboFlags

{-# COMPILE GHC ImGuiComboFlags = data DearImGui.Raw.ImGuiComboFlags (DearImGui.Raw.ImGuiComboFlags) #-}

postulate
    Storable[ImGuiComboFlags] : Storable ImGuiComboFlags
    Bits[ImGuiComboFlags]     : Bits ImGuiComboFlags
    Show[ImGuiComboFlags]     : Show ImGuiComboFlags
    Eq[ImGuiComboFlags]       : Eq ImGuiComboFlags
    Ord[ImGuiComboFlags]      : Ord ImGuiComboFlags

{-# COMPILE GHC Storable[ImGuiComboFlags] = AgdaStorable #-}
{-# COMPILE GHC Bits[ImGuiComboFlags]     = AgdaBits     #-}
{-# COMPILE GHC Show[ImGuiComboFlags]     = AgdaShow     #-}
{-# COMPILE GHC Eq[ImGuiComboFlags]       = AgdaEq       #-}
{-# COMPILE GHC Ord[ImGuiComboFlags]      = AgdaOrd      #-}


data ImGuiTabBarFlags : Set where
    mkImGuiTabBarFlags : CInt → ImGuiTabBarFlags

{-# COMPILE GHC ImGuiTabBarFlags = data DearImGui.Raw.ImGuiTabBarFlags (DearImGui.Raw.ImGuiTabBarFlags) #-}

postulate
    Storable[ImGuiTabBarFlags] : Storable ImGuiTabBarFlags
    Bits[ImGuiTabBarFlags]     : Bits ImGuiTabBarFlags
    Show[ImGuiTabBarFlags]     : Show ImGuiTabBarFlags
    Eq[ImGuiTabBarFlags]       : Eq ImGuiTabBarFlags
    Ord[ImGuiTabBarFlags]      : Ord ImGuiTabBarFlags

{-# COMPILE GHC Storable[ImGuiTabBarFlags] = AgdaStorable #-}
{-# COMPILE GHC Bits[ImGuiTabBarFlags]     = AgdaBits     #-}
{-# COMPILE GHC Show[ImGuiTabBarFlags]     = AgdaShow     #-}
{-# COMPILE GHC Eq[ImGuiTabBarFlags]       = AgdaEq       #-}
{-# COMPILE GHC Ord[ImGuiTabBarFlags]      = AgdaOrd      #-}


data ImGuiTabItemFlags : Set where
    mkImGuiTabItemFlags : CInt → ImGuiTabItemFlags

{-# COMPILE GHC ImGuiTabItemFlags = data DearImGui.Raw.ImGuiTabItemFlags (DearImGui.Raw.ImGuiTabItemFlags) #-}

postulate
    Storable[ImGuiTabItemFlags] : Storable ImGuiTabItemFlags
    Bits[ImGuiTabItemFlags]     : Bits ImGuiTabItemFlags
    Show[ImGuiTabItemFlags]     : Show ImGuiTabItemFlags
    Eq[ImGuiTabItemFlags]       : Eq ImGuiTabItemFlags
    Ord[ImGuiTabItemFlags]      : Ord ImGuiTabItemFlags

{-# COMPILE GHC Storable[ImGuiTabItemFlags] = AgdaStorable #-}
{-# COMPILE GHC Bits[ImGuiTabItemFlags]     = AgdaBits     #-}
{-# COMPILE GHC Show[ImGuiTabItemFlags]     = AgdaShow     #-}
{-# COMPILE GHC Eq[ImGuiTabItemFlags]       = AgdaEq       #-}
{-# COMPILE GHC Ord[ImGuiTabItemFlags]      = AgdaOrd      #-}


data ImGuiTableFlags : Set where
    mkImGuiTableFlags : CInt → ImGuiTableFlags

{-# COMPILE GHC ImGuiTableFlags = data DearImGui.Raw.ImGuiTableFlags (DearImGui.Raw.ImGuiTableFlags) #-}

postulate
    Storable[ImGuiTableFlags] : Storable ImGuiTableFlags
    Bits[ImGuiTableFlags]     : Bits ImGuiTableFlags
    Show[ImGuiTableFlags]     : Show ImGuiTableFlags
    Eq[ImGuiTableFlags]       : Eq ImGuiTableFlags
    Ord[ImGuiTableFlags]      : Ord ImGuiTableFlags

{-# COMPILE GHC Storable[ImGuiTableFlags] = AgdaStorable #-}
{-# COMPILE GHC Bits[ImGuiTableFlags]     = AgdaBits     #-}
{-# COMPILE GHC Show[ImGuiTableFlags]     = AgdaShow     #-}
{-# COMPILE GHC Eq[ImGuiTableFlags]       = AgdaEq       #-}
{-# COMPILE GHC Ord[ImGuiTableFlags]      = AgdaOrd      #-}


data ImGuiTableColumnFlags : Set where
    mkImGuiTableColumnFlags : CInt → ImGuiTableColumnFlags

{-# COMPILE GHC ImGuiTableColumnFlags = data DearImGui.Raw.ImGuiTableColumnFlags (DearImGui.Raw.ImGuiTableColumnFlags) #-}

postulate
    Storable[ImGuiTableColumnFlags] : Storable ImGuiTableColumnFlags
    Bits[ImGuiTableColumnFlags]     : Bits ImGuiTableColumnFlags
    Show[ImGuiTableColumnFlags]     : Show ImGuiTableColumnFlags
    Eq[ImGuiTableColumnFlags]       : Eq ImGuiTableColumnFlags
    Ord[ImGuiTableColumnFlags]      : Ord ImGuiTableColumnFlags

{-# COMPILE GHC Storable[ImGuiTableColumnFlags] = AgdaStorable #-}
{-# COMPILE GHC Bits[ImGuiTableColumnFlags]     = AgdaBits     #-}
{-# COMPILE GHC Show[ImGuiTableColumnFlags]     = AgdaShow     #-}
{-# COMPILE GHC Eq[ImGuiTableColumnFlags]       = AgdaEq       #-}
{-# COMPILE GHC Ord[ImGuiTableColumnFlags]      = AgdaOrd      #-}


data ImGuiTableRowFlags : Set where
    mkImGuiTableRowFlags : CInt → ImGuiTableRowFlags

{-# COMPILE GHC ImGuiTableRowFlags = data DearImGui.Raw.ImGuiTableRowFlags (DearImGui.Raw.ImGuiTableRowFlags) #-}

postulate
    Storable[ImGuiTableRowFlags] : Storable ImGuiTableRowFlags
    Bits[ImGuiTableRowFlags]     : Bits ImGuiTableRowFlags
    Show[ImGuiTableRowFlags]     : Show ImGuiTableRowFlags
    Eq[ImGuiTableRowFlags]       : Eq ImGuiTableRowFlags
    Ord[ImGuiTableRowFlags]      : Ord ImGuiTableRowFlags

{-# COMPILE GHC Storable[ImGuiTableRowFlags] = AgdaStorable #-}
{-# COMPILE GHC Bits[ImGuiTableRowFlags]     = AgdaBits     #-}
{-# COMPILE GHC Show[ImGuiTableRowFlags]     = AgdaShow     #-}
{-# COMPILE GHC Eq[ImGuiTableRowFlags]       = AgdaEq       #-}
{-# COMPILE GHC Ord[ImGuiTableRowFlags]      = AgdaOrd      #-}


data ImGuiTableBgTarget : Set where
    mkImGuiTableBgTarget : CInt → ImGuiTableBgTarget

{-# COMPILE GHC ImGuiTableBgTarget = data DearImGui.Raw.ImGuiTableBgTarget (DearImGui.Raw.ImGuiTableBgTarget) #-}

postulate
    Storable[ImGuiTableBgTarget] : Storable ImGuiTableBgTarget
    Bits[ImGuiTableBgTarget]     : Bits ImGuiTableBgTarget
    Show[ImGuiTableBgTarget]     : Show ImGuiTableBgTarget
    Eq[ImGuiTableBgTarget]       : Eq ImGuiTableBgTarget
    Ord[ImGuiTableBgTarget]      : Ord ImGuiTableBgTarget

{-# COMPILE GHC Storable[ImGuiTableBgTarget] = AgdaStorable #-}
{-# COMPILE GHC Show[ImGuiTableBgTarget]     = AgdaShow     #-}
{-# COMPILE GHC Eq[ImGuiTableBgTarget]       = AgdaEq       #-}
{-# COMPILE GHC Ord[ImGuiTableBgTarget]      = AgdaOrd      #-}


data ImGuiFocusedFlags : Set where
    mkImGuiFocusedFlags : CInt → ImGuiFocusedFlags

{-# COMPILE GHC ImGuiFocusedFlags = data DearImGui.Raw.ImGuiFocusedFlags (DearImGui.Raw.ImGuiFocusedFlags) #-}

postulate
    Storable[ImGuiFocusedFlags] : Storable ImGuiFocusedFlags
    Bits[ImGuiFocusedFlags]     : Bits ImGuiFocusedFlags
    Show[ImGuiFocusedFlags]     : Show ImGuiFocusedFlags
    Eq[ImGuiFocusedFlags]       : Eq ImGuiFocusedFlags
    Ord[ImGuiFocusedFlags]      : Ord ImGuiFocusedFlags

{-# COMPILE GHC Storable[ImGuiFocusedFlags] = AgdaStorable #-}
{-# COMPILE GHC Bits[ImGuiFocusedFlags]     = AgdaBits     #-}
{-# COMPILE GHC Show[ImGuiFocusedFlags]     = AgdaShow     #-}
{-# COMPILE GHC Eq[ImGuiFocusedFlags]       = AgdaEq       #-}
{-# COMPILE GHC Ord[ImGuiFocusedFlags]      = AgdaOrd      #-}


data ImGuiHoveredFlags : Set where
    mkImGuiHoveredFlags : CInt → ImGuiHoveredFlags

{-# COMPILE GHC ImGuiHoveredFlags = data DearImGui.Raw.ImGuiHoveredFlags (DearImGui.Raw.ImGuiHoveredFlags) #-}

postulate
    Storable[ImGuiHoveredFlags] : Storable ImGuiHoveredFlags
    Bits[ImGuiHoveredFlags]     : Bits ImGuiHoveredFlags
    Show[ImGuiHoveredFlags]     : Show ImGuiHoveredFlags
    Eq[ImGuiHoveredFlags]       : Eq ImGuiHoveredFlags
    Ord[ImGuiHoveredFlags]      : Ord ImGuiHoveredFlags

{-# COMPILE GHC Storable[ImGuiHoveredFlags] = AgdaStorable #-}
{-# COMPILE GHC Bits[ImGuiHoveredFlags]     = AgdaBits     #-}
{-# COMPILE GHC Show[ImGuiHoveredFlags]     = AgdaShow     #-}
{-# COMPILE GHC Eq[ImGuiHoveredFlags]       = AgdaEq       #-}
{-# COMPILE GHC Ord[ImGuiHoveredFlags]      = AgdaOrd      #-}


data ImGuiDragDropFlags : Set where
    mkImGuiDragDropFlags : CInt → ImGuiDragDropFlags

{-# COMPILE GHC ImGuiDragDropFlags = data DearImGui.Raw.ImGuiDragDropFlags (DearImGui.Raw.ImGuiDragDropFlags) #-}

postulate
    Storable[ImGuiDragDropFlags] : Storable ImGuiDragDropFlags
    Bits[ImGuiDragDropFlags]     : Bits ImGuiDragDropFlags
    Show[ImGuiDragDropFlags]     : Show ImGuiDragDropFlags
    Eq[ImGuiDragDropFlags]       : Eq ImGuiDragDropFlags
    Ord[ImGuiDragDropFlags]      : Ord ImGuiDragDropFlags

{-# COMPILE GHC Storable[ImGuiDragDropFlags] = AgdaStorable #-}
{-# COMPILE GHC Bits[ImGuiDragDropFlags]     = AgdaBits     #-}
{-# COMPILE GHC Show[ImGuiDragDropFlags]     = AgdaShow     #-}
{-# COMPILE GHC Eq[ImGuiDragDropFlags]       = AgdaEq       #-}
{-# COMPILE GHC Ord[ImGuiDragDropFlags]      = AgdaOrd      #-}


data ImGuiDataType : Set where
    mkImGuiDataType : CInt → ImGuiDataType

{-# COMPILE GHC ImGuiDataType = data DearImGui.Raw.ImGuiDataType (DearImGui.Raw.ImGuiDataType) #-}

postulate
    Storable[ImGuiDataType] : Storable ImGuiDataType
    Show[ImGuiDataType]     : Show ImGuiDataType
    -- todo: FiniteEnum instance for ImGuiDataType
    Eq[ImGuiDataType]       : Eq ImGuiDataType
    Ord[ImGuiDataType]      : Ord ImGuiDataType

{-# COMPILE GHC Storable[ImGuiDataType] = AgdaStorable #-}
{-# COMPILE GHC Show[ImGuiDataType]     = AgdaShow     #-}
{-# COMPILE GHC Eq[ImGuiDataType]       = AgdaEq       #-}
{-# COMPILE GHC Ord[ImGuiDataType]      = AgdaOrd      #-}


data ImGuiDir : Set where
    mkImGuiDir : CInt → ImGuiDir

{-# COMPILE GHC ImGuiDir = data DearImGui.Raw.ImGuiDir (DearImGui.Raw.ImGuiDir) #-}

postulate
    Storable[ImGuiDir] : Storable ImGuiDir
    -- todo: FiniteEnum instance for ImGuiDir
    Show[ImGuiDir]     : Show ImGuiDir
    Eq[ImGuiDir]       : Eq ImGuiDir
    Ord[ImGuiDir]      : Ord ImGuiDir

{-# COMPILE GHC Storable[ImGuiDir] = AgdaStorable #-}
{-# COMPILE GHC Show[ImGuiDir]     = AgdaShow     #-}
{-# COMPILE GHC Eq[ImGuiDir]       = AgdaEq       #-}
{-# COMPILE GHC Ord[ImGuiDir]      = AgdaOrd      #-}


data ImGuiSortDirection : Set where
    mkImGuiSortDirection : CInt → ImGuiSortDirection

{-# COMPILE GHC ImGuiSortDirection = data DearImGui.Raw.ImGuiSortDirection (DearImGui.Raw.ImGuiSortDirection) #-}

postulate
    Storable[ImGuiSortDirection] : Storable ImGuiSortDirection
    Show[ImGuiSortDirection]     : Show ImGuiSortDirection
    Eq[ImGuiSortDirection]       : Eq ImGuiSortDirection
    Ord[ImGuiSortDirection]      : Ord ImGuiSortDirection

{-# COMPILE GHC Storable[ImGuiSortDirection] = AgdaStorable #-}
{-# COMPILE GHC Show[ImGuiSortDirection]     = AgdaShow     #-}
{-# COMPILE GHC Eq[ImGuiSortDirection]       = AgdaEq       #-}
{-# COMPILE GHC Ord[ImGuiSortDirection]      = AgdaOrd      #-}


data ImGuiKey : Set where
    mkImGuiKey : CInt → ImGuiKey

{-# COMPILE GHC ImGuiKey = data DearImGui.Raw.ImGuiKey (DearImGui.Raw.ImGuiKey) #-}

postulate
    Storable[ImGuiKey] : Storable ImGuiKey
    -- todo: FiniteEnum instance for ImGuiKey
    Show[ImGuiKey]     : Show ImGuiKey
    Eq[ImGuiKey]       : Eq ImGuiKey
    Ord[ImGuiKey]      : Ord ImGuiKey

{-# COMPILE GHC Storable[ImGuiKey] = AgdaStorable #-}
{-# COMPILE GHC Show[ImGuiKey]     = AgdaShow     #-}
{-# COMPILE GHC Eq[ImGuiKey]       = AgdaEq       #-}
{-# COMPILE GHC Ord[ImGuiKey]      = AgdaOrd      #-}


data ImGuiModFlags : Set where
    mkImGuiModFlags : CInt → ImGuiModFlags

{-# COMPILE GHC ImGuiModFlags = data DearImGui.Raw.ImGuiModFlags (DearImGui.Raw.ImGuiModFlags) #-}

postulate
    Storable[ImGuiModFlags] : Storable ImGuiModFlags
    Bits[ImGuiModFlags]     : Bits ImGuiModFlags
    Show[ImGuiModFlags]     : Show ImGuiModFlags
    Eq[ImGuiModFlags]       : Eq ImGuiModFlags
    Ord[ImGuiModFlags]      : Ord ImGuiModFlags

{-# COMPILE GHC Storable[ImGuiModFlags] = AgdaStorable #-}
{-# COMPILE GHC Bits[ImGuiModFlags]     = AgdaBits     #-}
{-# COMPILE GHC Show[ImGuiModFlags]     = AgdaShow     #-}
{-# COMPILE GHC Eq[ImGuiModFlags]       = AgdaEq       #-}
{-# COMPILE GHC Ord[ImGuiModFlags]      = AgdaOrd      #-}


data ImGuiNavInput : Set where
    mkImGuiNavInput : CInt → ImGuiNavInput

{-# COMPILE GHC ImGuiNavInput = data DearImGui.Raw.ImGuiNavInput (DearImGui.Raw.ImGuiNavInput) #-}

postulate
    Storable[ImGuiNavInput] : Storable ImGuiNavInput
    -- todo: FiniteEnum instance for ImGuiNavInput
    Show[ImGuiNavInput]     : Show ImGuiNavInput
    Eq[ImGuiNavInput]       : Eq ImGuiNavInput
    Ord[ImGuiNavInput]      : Ord ImGuiNavInput

{-# COMPILE GHC Storable[ImGuiNavInput] = AgdaStorable #-}
{-# COMPILE GHC Show[ImGuiNavInput]     = AgdaShow     #-}
{-# COMPILE GHC Eq[ImGuiNavInput]       = AgdaEq       #-}
{-# COMPILE GHC Ord[ImGuiNavInput]      = AgdaOrd      #-}


data ImGuiConfigFlags : Set where
    mkImGuiConfigFlags : CInt → ImGuiConfigFlags

{-# COMPILE GHC ImGuiConfigFlags = data DearImGui.Raw.ImGuiConfigFlags (DearImGui.Raw.ImGuiConfigFlags) #-}

postulate
    Storable[ImGuiConfigFlags] : Storable ImGuiConfigFlags
    Bits[ImGuiConfigFlags]     : Bits ImGuiConfigFlags
    Show[ImGuiConfigFlags]     : Show ImGuiConfigFlags
    Eq[ImGuiConfigFlags]       : Eq ImGuiConfigFlags
    Ord[ImGuiConfigFlags]      : Ord ImGuiConfigFlags

{-# COMPILE GHC Storable[ImGuiConfigFlags] = AgdaStorable #-}
{-# COMPILE GHC Bits[ImGuiConfigFlags]     = AgdaBits     #-}
{-# COMPILE GHC Show[ImGuiConfigFlags]     = AgdaShow     #-}
{-# COMPILE GHC Eq[ImGuiConfigFlags]       = AgdaEq       #-}
{-# COMPILE GHC Ord[ImGuiConfigFlags]      = AgdaOrd      #-}


data ImGuiBackendFlags : Set where
    mkImGuiBackendFlags : CInt → ImGuiBackendFlags

{-# COMPILE GHC ImGuiBackendFlags = data DearImGui.Raw.ImGuiBackendFlags (DearImGui.Raw.ImGuiBackendFlags) #-}

postulate
    Storable[ImGuiBackendFlags] : Storable ImGuiBackendFlags
    Bits[ImGuiBackendFlags]     : Bits ImGuiBackendFlags
    Show[ImGuiBackendFlags]     : Show ImGuiBackendFlags
    Eq[ImGuiBackendFlags]       : Eq ImGuiBackendFlags
    Ord[ImGuiBackendFlags]      : Ord ImGuiBackendFlags

{-# COMPILE GHC Storable[ImGuiBackendFlags] = AgdaStorable #-}
{-# COMPILE GHC Bits[ImGuiBackendFlags]     = AgdaBits     #-}
{-# COMPILE GHC Show[ImGuiBackendFlags]     = AgdaShow     #-}
{-# COMPILE GHC Eq[ImGuiBackendFlags]       = AgdaEq       #-}
{-# COMPILE GHC Ord[ImGuiBackendFlags]      = AgdaOrd      #-}


data ImGuiCol : Set where
    mkImGuiCol : CInt → ImGuiCol

{-# COMPILE GHC ImGuiCol = data DearImGui.Raw.ImGuiCol (DearImGui.Raw.ImGuiCol) #-}

postulate
    Storable[ImGuiCol] : Storable ImGuiCol
    -- todo: FiniteEnum instance for ImGuiCol
    Show[ImGuiCol]     : Show ImGuiCol
    Eq[ImGuiCol]       : Eq ImGuiCol
    Ord[ImGuiCol]      : Ord ImGuiCol

{-# COMPILE GHC Storable[ImGuiCol] = AgdaStorable #-}
{-# COMPILE GHC Show[ImGuiCol]     = AgdaShow     #-}
{-# COMPILE GHC Eq[ImGuiCol]       = AgdaEq       #-}
{-# COMPILE GHC Ord[ImGuiCol]      = AgdaOrd      #-}


data ImGuiStyleVar : Set where
    mkImGuiStyleVar : CInt → ImGuiStyleVar

{-# COMPILE GHC ImGuiStyleVar = data DearImGui.Raw.ImGuiStyleVar (DearImGui.Raw.ImGuiStyleVar) #-}

postulate
    Storable[ImGuiStyleVar] : Storable ImGuiStyleVar
    -- todo: FiniteEnum instance for ImGuiStyleVar
    Show[ImGuiStyleVar]     : Show ImGuiStyleVar
    Eq[ImGuiStyleVar]       : Eq ImGuiStyleVar
    Ord[ImGuiStyleVar]      : Ord ImGuiStyleVar

{-# COMPILE GHC Storable[ImGuiStyleVar] = AgdaStorable #-}
{-# COMPILE GHC Show[ImGuiStyleVar]     = AgdaShow     #-}
{-# COMPILE GHC Eq[ImGuiStyleVar]       = AgdaEq       #-}
{-# COMPILE GHC Ord[ImGuiStyleVar]      = AgdaOrd      #-}


data ImGuiButtonFlags : Set where
    mkImGuiButtonFlags : CInt → ImGuiButtonFlags

{-# COMPILE GHC ImGuiButtonFlags = data DearImGui.Raw.ImGuiButtonFlags (DearImGui.Raw.ImGuiButtonFlags) #-}

postulate
    Storable[ImGuiButtonFlags] : Storable ImGuiButtonFlags
    Bits[ImGuiButtonFlags]     : Bits ImGuiButtonFlags
    Show[ImGuiButtonFlags]     : Show ImGuiButtonFlags
    Eq[ImGuiButtonFlags]       : Eq ImGuiButtonFlags
    Ord[ImGuiButtonFlags]      : Ord ImGuiButtonFlags

{-# COMPILE GHC Storable[ImGuiButtonFlags] = AgdaStorable #-}
{-# COMPILE GHC Bits[ImGuiButtonFlags]     = AgdaBits     #-}
{-# COMPILE GHC Show[ImGuiButtonFlags]     = AgdaShow     #-}
{-# COMPILE GHC Eq[ImGuiButtonFlags]       = AgdaEq       #-}
{-# COMPILE GHC Ord[ImGuiButtonFlags]      = AgdaOrd      #-}


data ImGuiColorEditFlags : Set where
    mkImGuiColorEditFlags : CInt → ImGuiColorEditFlags

{-# COMPILE GHC ImGuiColorEditFlags = data DearImGui.Raw.ImGuiColorEditFlags (DearImGui.Raw.ImGuiColorEditFlags) #-}

postulate
    Storable[ImGuiColorEditFlags] : Storable ImGuiColorEditFlags
    Bits[ImGuiColorEditFlags]     : Bits ImGuiColorEditFlags
    Show[ImGuiColorEditFlags]     : Show ImGuiColorEditFlags
    Eq[ImGuiColorEditFlags]       : Eq ImGuiColorEditFlags
    Ord[ImGuiColorEditFlags]      : Ord ImGuiColorEditFlags

{-# COMPILE GHC Storable[ImGuiColorEditFlags] = AgdaStorable #-}
{-# COMPILE GHC Bits[ImGuiColorEditFlags]     = AgdaBits     #-}
{-# COMPILE GHC Show[ImGuiColorEditFlags]     = AgdaShow     #-}
{-# COMPILE GHC Eq[ImGuiColorEditFlags]       = AgdaEq       #-}
{-# COMPILE GHC Ord[ImGuiColorEditFlags]      = AgdaOrd      #-}


data ImGuiSliderFlags : Set where
    mkImGuiSliderFlags : CInt → ImGuiSliderFlags

{-# COMPILE GHC ImGuiSliderFlags = data DearImGui.Raw.ImGuiSliderFlags (DearImGui.Raw.ImGuiSliderFlags) #-}

postulate
    Storable[ImGuiSliderFlags] : Storable ImGuiSliderFlags
    Bits[ImGuiSliderFlags]     : Bits ImGuiSliderFlags
    Show[ImGuiSliderFlags]     : Show ImGuiSliderFlags
    Eq[ImGuiSliderFlags]       : Eq ImGuiSliderFlags
    Ord[ImGuiSliderFlags]      : Ord ImGuiSliderFlags

{-# COMPILE GHC Storable[ImGuiSliderFlags] = AgdaStorable #-}
{-# COMPILE GHC Bits[ImGuiSliderFlags]     = AgdaBits     #-}
{-# COMPILE GHC Show[ImGuiSliderFlags]     = AgdaShow     #-}
{-# COMPILE GHC Eq[ImGuiSliderFlags]       = AgdaEq       #-}
{-# COMPILE GHC Ord[ImGuiSliderFlags]      = AgdaOrd      #-}


data ImGuiMouseButton : Set where
    mkImGuiMouseButton : CInt → ImGuiMouseButton

{-# COMPILE GHC ImGuiMouseButton = data DearImGui.Raw.ImGuiMouseButton (DearImGui.Raw.ImGuiMouseButton) #-}

postulate
    Storable[ImGuiMouseButton] : Storable ImGuiMouseButton
    -- todo: FiniteEnum instance for ImGuiMouseButton
    Show[ImGuiMouseButton]     : Show ImGuiMouseButton
    Eq[ImGuiMouseButton]       : Eq ImGuiMouseButton
    Ord[ImGuiMouseButton]      : Ord ImGuiMouseButton

{-# COMPILE GHC Storable[ImGuiMouseButton] = AgdaStorable #-}
{-# COMPILE GHC Show[ImGuiMouseButton]     = AgdaShow     #-}
{-# COMPILE GHC Eq[ImGuiMouseButton]       = AgdaEq       #-}
{-# COMPILE GHC Ord[ImGuiMouseButton]      = AgdaOrd      #-}


data ImGuiMouseCursor : Set where
    mkImGuiMouseCursor : CInt → ImGuiMouseCursor

{-# COMPILE GHC ImGuiMouseCursor = data DearImGui.Raw.ImGuiMouseCursor (DearImGui.Raw.ImGuiMouseCursor) #-}

postulate
    Storable[ImGuiMouseCursor] : Storable ImGuiMouseCursor
    -- todo: FiniteEnum instance for ImGuiMouseCursor
    Show[ImGuiMouseCursor]     : Show ImGuiMouseCursor
    Eq[ImGuiMouseCursor]       : Eq ImGuiMouseCursor
    Ord[ImGuiMouseCursor]      : Ord ImGuiMouseCursor

{-# COMPILE GHC Storable[ImGuiMouseCursor] = AgdaStorable #-}
{-# COMPILE GHC Show[ImGuiMouseCursor]     = AgdaShow     #-}
{-# COMPILE GHC Eq[ImGuiMouseCursor]       = AgdaEq       #-}
{-# COMPILE GHC Ord[ImGuiMouseCursor]      = AgdaOrd      #-}


data ImGuiCond : Set where
    mkImGuiCond : CInt → ImGuiCond

{-# COMPILE GHC ImGuiCond = data DearImGui.Raw.ImGuiCond (DearImGui.Raw.ImGuiCond) #-}

postulate
    Storable[ImGuiCond] : Storable ImGuiCond
    Show[ImGuiCond]     : Show ImGuiCond
    Eq[ImGuiCond]       : Eq ImGuiCond
    Ord[ImGuiCond]      : Ord ImGuiCond

{-# COMPILE GHC Storable[ImGuiCond] = AgdaStorable #-}
{-# COMPILE GHC Show[ImGuiCond]     = AgdaShow     #-}
{-# COMPILE GHC Eq[ImGuiCond]       = AgdaEq       #-}
{-# COMPILE GHC Ord[ImGuiCond]      = AgdaOrd      #-}


data ImDrawFlags : Set where
    mkImDrawFlags : CInt → ImDrawFlags

{-# COMPILE GHC ImDrawFlags = data DearImGui.Raw.ImDrawFlags (DearImGui.Raw.ImDrawFlags) #-}

postulate
    Storable[ImDrawFlags] : Storable ImDrawFlags
    Bits[ImDrawFlags]     : Bits ImDrawFlags
    Show[ImDrawFlags]     : Show ImDrawFlags
    Eq[ImDrawFlags]       : Eq ImDrawFlags
    Ord[ImDrawFlags]      : Ord ImDrawFlags

{-# COMPILE GHC Storable[ImDrawFlags] = AgdaStorable #-}
{-# COMPILE GHC Bits[ImDrawFlags]     = AgdaBits     #-}
{-# COMPILE GHC Show[ImDrawFlags]     = AgdaShow     #-}
{-# COMPILE GHC Eq[ImDrawFlags]       = AgdaEq       #-}
{-# COMPILE GHC Ord[ImDrawFlags]      = AgdaOrd      #-}


data ImDrawListFlags : Set where
    mkImDrawListFlags : CInt → ImDrawListFlags

{-# COMPILE GHC ImDrawListFlags = data DearImGui.Raw.ImDrawListFlags (DearImGui.Raw.ImDrawListFlags) #-}

postulate
    Storable[ImDrawListFlags] : Storable ImDrawListFlags
    Bits[ImDrawListFlags]     : Bits ImDrawListFlags
    Show[ImDrawListFlags]     : Show ImDrawListFlags
    Eq[ImDrawListFlags]       : Eq ImDrawListFlags
    Ord[ImDrawListFlags]      : Ord ImDrawListFlags

{-# COMPILE GHC Storable[ImDrawListFlags] = AgdaStorable #-}
{-# COMPILE GHC Bits[ImDrawListFlags]     = AgdaBits     #-}
{-# COMPILE GHC Show[ImDrawListFlags]     = AgdaShow     #-}
{-# COMPILE GHC Eq[ImDrawListFlags]       = AgdaEq       #-}
{-# COMPILE GHC Ord[ImDrawListFlags]      = AgdaOrd      #-}


data ImFontAtlasFlags : Set where
    mkImFontAtlasFlags : CInt → ImFontAtlasFlags

{-# COMPILE GHC ImFontAtlasFlags = data DearImGui.Raw.ImFontAtlasFlags (DearImGui.Raw.ImFontAtlasFlags) #-}

postulate
    Storable[ImFontAtlasFlags] : Storable ImFontAtlasFlags
    Bits[ImFontAtlasFlags]     : Bits ImFontAtlasFlags
    Show[ImFontAtlasFlags]     : Show ImFontAtlasFlags
    Eq[ImFontAtlasFlags]       : Eq ImFontAtlasFlags
    Ord[ImFontAtlasFlags]      : Ord ImFontAtlasFlags

{-# COMPILE GHC Storable[ImFontAtlasFlags] = AgdaStorable #-}
{-# COMPILE GHC Bits[ImFontAtlasFlags]     = AgdaBits     #-}
{-# COMPILE GHC Show[ImFontAtlasFlags]     = AgdaShow     #-}
{-# COMPILE GHC Eq[ImFontAtlasFlags]       = AgdaEq       #-}
{-# COMPILE GHC Ord[ImFontAtlasFlags]      = AgdaOrd      #-}


postulate
    ImFontAtlasFlags-NoBakedLines                  : ImFontAtlasFlags
    ImFontAtlasFlags-NoMouseCursors                : ImFontAtlasFlags
    ImFontAtlasFlags-NoPowerOfTwoHeight            : ImFontAtlasFlags
    ImFontAtlasFlags-None                          : ImFontAtlasFlags
    ImDrawListFlags-AllowVtxOffset                 : ImDrawListFlags
    ImDrawListFlags-AntiAliasedFill                : ImDrawListFlags
    ImDrawListFlags-AntiAliasedLinesUseTex         : ImDrawListFlags
    ImDrawListFlags-AntiAliasedLines               : ImDrawListFlags
    ImDrawListFlags-None                           : ImDrawListFlags
    ImDrawFlags-RoundCornersMask-                  : ImDrawFlags
    ImDrawFlags-RoundCornersDefault-               : ImDrawFlags
    ImDrawFlags-RoundCornersAll                    : ImDrawFlags
    ImDrawFlags-RoundCornersRight                  : ImDrawFlags
    ImDrawFlags-RoundCornersLeft                   : ImDrawFlags
    ImDrawFlags-RoundCornersBottom                 : ImDrawFlags
    ImDrawFlags-RoundCornersTop                    : ImDrawFlags
    ImDrawFlags-RoundCornersNone                   : ImDrawFlags
    ImDrawFlags-RoundCornersBottomRight            : ImDrawFlags
    ImDrawFlags-RoundCornersBottomLeft             : ImDrawFlags
    ImDrawFlags-RoundCornersTopRight               : ImDrawFlags
    ImDrawFlags-RoundCornersTopLeft                : ImDrawFlags
    ImDrawFlags-Closed                             : ImDrawFlags
    ImDrawFlags-None                               : ImDrawFlags
    ImGuiCond-Appearing                            : ImGuiCond
    ImGuiCond-FirstUseEver                         : ImGuiCond
    ImGuiCond-Once                                 : ImGuiCond
    ImGuiCond-Always                               : ImGuiCond
    ImGuiCond-None                                 : ImGuiCond
    ImGuiMouseCursor-NotAllowed                    : ImGuiMouseCursor
    ImGuiMouseCursor-Hand                          : ImGuiMouseCursor
    ImGuiMouseCursor-ResizeNWSE                    : ImGuiMouseCursor
    ImGuiMouseCursor-ResizeNESW                    : ImGuiMouseCursor
    ImGuiMouseCursor-ResizeEW                      : ImGuiMouseCursor
    ImGuiMouseCursor-ResizeNS                      : ImGuiMouseCursor
    ImGuiMouseCursor-ResizeAll                     : ImGuiMouseCursor
    ImGuiMouseCursor-TextInput                     : ImGuiMouseCursor
    ImGuiMouseCursor-Arrow                         : ImGuiMouseCursor
    ImGuiMouseCursor-None                          : ImGuiMouseCursor
    ImGuiMouseButton-Middle                        : ImGuiMouseButton
    ImGuiMouseButton-Right                         : ImGuiMouseButton
    ImGuiMouseButton-Left                          : ImGuiMouseButton
    ImGuiSliderFlags-InvalidMask-                  : ImGuiSliderFlags
    ImGuiSliderFlags-NoInput                       : ImGuiSliderFlags
    ImGuiSliderFlags-NoRoundToFormat               : ImGuiSliderFlags
    ImGuiSliderFlags-Logarithmic                   : ImGuiSliderFlags
    ImGuiSliderFlags-AlwaysClamp                   : ImGuiSliderFlags
    ImGuiSliderFlags-None                          : ImGuiSliderFlags
    ImGuiColorEditFlags-InputMask-                 : ImGuiColorEditFlags
    ImGuiColorEditFlags-PickerMask-                : ImGuiColorEditFlags
    ImGuiColorEditFlags-DataTypeMask-              : ImGuiColorEditFlags
    ImGuiColorEditFlags-DisplayMask-               : ImGuiColorEditFlags
    ImGuiColorEditFlags-DefaultOptions-            : ImGuiColorEditFlags
    ImGuiColorEditFlags-InputHSV                   : ImGuiColorEditFlags
    ImGuiColorEditFlags-InputRGB                   : ImGuiColorEditFlags
    ImGuiColorEditFlags-PickerHueWheel             : ImGuiColorEditFlags
    ImGuiColorEditFlags-PickerHueBar               : ImGuiColorEditFlags
    ImGuiColorEditFlags-Float                      : ImGuiColorEditFlags
    ImGuiColorEditFlags-Uint8                      : ImGuiColorEditFlags
    ImGuiColorEditFlags-DisplayHex                 : ImGuiColorEditFlags
    ImGuiColorEditFlags-DisplayHSV                 : ImGuiColorEditFlags
    ImGuiColorEditFlags-DisplayRGB                 : ImGuiColorEditFlags
    ImGuiColorEditFlags-HDR                        : ImGuiColorEditFlags
    ImGuiColorEditFlags-AlphaPreviewHalf           : ImGuiColorEditFlags
    ImGuiColorEditFlags-AlphaPreview               : ImGuiColorEditFlags
    ImGuiColorEditFlags-AlphaBar                   : ImGuiColorEditFlags
    ImGuiColorEditFlags-NoBorder                   : ImGuiColorEditFlags
    ImGuiColorEditFlags-NoDragDrop                 : ImGuiColorEditFlags
    ImGuiColorEditFlags-NoSidePreview              : ImGuiColorEditFlags
    ImGuiColorEditFlags-NoLabel                    : ImGuiColorEditFlags
    ImGuiColorEditFlags-NoTooltip                  : ImGuiColorEditFlags
    ImGuiColorEditFlags-NoInputs                   : ImGuiColorEditFlags
    ImGuiColorEditFlags-NoSmallPreview             : ImGuiColorEditFlags
    ImGuiColorEditFlags-NoOptions                  : ImGuiColorEditFlags
    ImGuiColorEditFlags-NoPicker                   : ImGuiColorEditFlags
    ImGuiColorEditFlags-NoAlpha                    : ImGuiColorEditFlags
    ImGuiColorEditFlags-None                       : ImGuiColorEditFlags
    ImGuiButtonFlags-MouseButtonDefault-           : ImGuiButtonFlags
    ImGuiButtonFlags-MouseButtonMask-              : ImGuiButtonFlags
    ImGuiButtonFlags-MouseButtonMiddle             : ImGuiButtonFlags
    ImGuiButtonFlags-MouseButtonRight              : ImGuiButtonFlags
    ImGuiButtonFlags-MouseButtonLeft               : ImGuiButtonFlags
    ImGuiButtonFlags-None                          : ImGuiButtonFlags
    ImGuiStyleVar-SelectableTextAlign              : ImGuiStyleVar
    ImGuiStyleVar-ButtonTextAlign                  : ImGuiStyleVar
    ImGuiStyleVar-TabRounding                      : ImGuiStyleVar
    ImGuiStyleVar-GrabRounding                     : ImGuiStyleVar
    ImGuiStyleVar-GrabMinSize                      : ImGuiStyleVar
    ImGuiStyleVar-ScrollbarRounding                : ImGuiStyleVar
    ImGuiStyleVar-ScrollbarSize                    : ImGuiStyleVar
    ImGuiStyleVar-CellPadding                      : ImGuiStyleVar
    ImGuiStyleVar-IndentSpacing                    : ImGuiStyleVar
    ImGuiStyleVar-ItemInnerSpacing                 : ImGuiStyleVar
    ImGuiStyleVar-ItemSpacing                      : ImGuiStyleVar
    ImGuiStyleVar-FrameBorderSize                  : ImGuiStyleVar
    ImGuiStyleVar-FrameRounding                    : ImGuiStyleVar
    ImGuiStyleVar-FramePadding                     : ImGuiStyleVar
    ImGuiStyleVar-PopupBorderSize                  : ImGuiStyleVar
    ImGuiStyleVar-PopupRounding                    : ImGuiStyleVar
    ImGuiStyleVar-ChildBorderSize                  : ImGuiStyleVar
    ImGuiStyleVar-ChildRounding                    : ImGuiStyleVar
    ImGuiStyleVar-WindowTitleAlign                 : ImGuiStyleVar
    ImGuiStyleVar-WindowMinSize                    : ImGuiStyleVar
    ImGuiStyleVar-WindowBorderSize                 : ImGuiStyleVar
    ImGuiStyleVar-WindowRounding                   : ImGuiStyleVar
    ImGuiStyleVar-WindowPadding                    : ImGuiStyleVar
    ImGuiStyleVar-DisabledAlpha                    : ImGuiStyleVar
    ImGuiStyleVar-Alpha                            : ImGuiStyleVar
    ImGuiCol-ModalWindowDimBg                      : ImGuiCol
    ImGuiCol-NavWindowingDimBg                     : ImGuiCol
    ImGuiCol-NavWindowingHighlight                 : ImGuiCol
    ImGuiCol-NavHighlight                          : ImGuiCol
    ImGuiCol-DragDropTarget                        : ImGuiCol
    ImGuiCol-TextSelectedBg                        : ImGuiCol
    ImGuiCol-TableRowBgAlt                         : ImGuiCol
    ImGuiCol-TableRowBg                            : ImGuiCol
    ImGuiCol-TableBorderLight                      : ImGuiCol
    ImGuiCol-TableBorderStrong                     : ImGuiCol
    ImGuiCol-TableHeaderBg                         : ImGuiCol
    ImGuiCol-PlotHistogramHovered                  : ImGuiCol
    ImGuiCol-PlotHistogram                         : ImGuiCol
    ImGuiCol-PlotLinesHovered                      : ImGuiCol
    ImGuiCol-PlotLines                             : ImGuiCol
    ImGuiCol-TabUnfocusedActive                    : ImGuiCol
    ImGuiCol-TabUnfocused                          : ImGuiCol
    ImGuiCol-TabActive                             : ImGuiCol
    ImGuiCol-TabHovered                            : ImGuiCol
    ImGuiCol-Tab                                   : ImGuiCol
    ImGuiCol-ResizeGripActive                      : ImGuiCol
    ImGuiCol-ResizeGripHovered                     : ImGuiCol
    ImGuiCol-ResizeGrip                            : ImGuiCol
    ImGuiCol-SeparatorActive                       : ImGuiCol
    ImGuiCol-SeparatorHovered                      : ImGuiCol
    ImGuiCol-Separator                             : ImGuiCol
    ImGuiCol-HeaderActive                          : ImGuiCol
    ImGuiCol-HeaderHovered                         : ImGuiCol
    ImGuiCol-Header                                : ImGuiCol
    ImGuiCol-ButtonActive                          : ImGuiCol
    ImGuiCol-ButtonHovered                         : ImGuiCol
    ImGuiCol-Button                                : ImGuiCol
    ImGuiCol-SliderGrabActive                      : ImGuiCol
    ImGuiCol-SliderGrab                            : ImGuiCol
    ImGuiCol-CheckMark                             : ImGuiCol
    ImGuiCol-ScrollbarGrabActive                   : ImGuiCol
    ImGuiCol-ScrollbarGrabHovered                  : ImGuiCol
    ImGuiCol-ScrollbarGrab                         : ImGuiCol
    ImGuiCol-ScrollbarBg                           : ImGuiCol
    ImGuiCol-MenuBarBg                             : ImGuiCol
    ImGuiCol-TitleBgCollapsed                      : ImGuiCol
    ImGuiCol-TitleBgActive                         : ImGuiCol
    ImGuiCol-TitleBg                               : ImGuiCol
    ImGuiCol-FrameBgActive                         : ImGuiCol
    ImGuiCol-FrameBgHovered                        : ImGuiCol
    ImGuiCol-FrameBg                               : ImGuiCol
    ImGuiCol-BorderShadow                          : ImGuiCol
    ImGuiCol-Border                                : ImGuiCol
    ImGuiCol-PopupBg                               : ImGuiCol
    ImGuiCol-ChildBg                               : ImGuiCol
    ImGuiCol-WindowBg                              : ImGuiCol
    ImGuiCol-TextDisabled                          : ImGuiCol
    ImGuiCol-Text                                  : ImGuiCol
    ImGuiBackendFlags-RendererHasVtxOffset         : ImGuiBackendFlags
    ImGuiBackendFlags-HasSetMousePos               : ImGuiBackendFlags
    ImGuiBackendFlags-HasMouseCursors              : ImGuiBackendFlags
    ImGuiBackendFlags-HasGamepad                   : ImGuiBackendFlags
    ImGuiBackendFlags-None                         : ImGuiBackendFlags
    ImGuiConfigFlags-IsTouchScreen                 : ImGuiConfigFlags
    ImGuiConfigFlags-IsSRGB                        : ImGuiConfigFlags
    ImGuiConfigFlags-NoMouseCursorChange           : ImGuiConfigFlags
    ImGuiConfigFlags-NoMouse                       : ImGuiConfigFlags
    ImGuiConfigFlags-NavNoCaptureKeyboard          : ImGuiConfigFlags
    ImGuiConfigFlags-NavEnableSetMousePos          : ImGuiConfigFlags
    ImGuiConfigFlags-NavEnableGamepad              : ImGuiConfigFlags
    ImGuiConfigFlags-NavEnableKeyboard             : ImGuiConfigFlags
    ImGuiConfigFlags-None                          : ImGuiConfigFlags
    ImGuiNavInput-KeyDown-                         : ImGuiNavInput
    ImGuiNavInput-KeyUp-                           : ImGuiNavInput
    ImGuiNavInput-KeyRight-                        : ImGuiNavInput
    ImGuiNavInput-KeyLeft-                         : ImGuiNavInput
    ImGuiNavInput-TweakFast                        : ImGuiNavInput
    ImGuiNavInput-TweakSlow                        : ImGuiNavInput
    ImGuiNavInput-FocusNext                        : ImGuiNavInput
    ImGuiNavInput-FocusPrev                        : ImGuiNavInput
    ImGuiNavInput-LStickDown                       : ImGuiNavInput
    ImGuiNavInput-LStickUp                         : ImGuiNavInput
    ImGuiNavInput-LStickRight                      : ImGuiNavInput
    ImGuiNavInput-LStickLeft                       : ImGuiNavInput
    ImGuiNavInput-DpadDown                         : ImGuiNavInput
    ImGuiNavInput-DpadUp                           : ImGuiNavInput
    ImGuiNavInput-DpadRight                        : ImGuiNavInput
    ImGuiNavInput-DpadLeft                         : ImGuiNavInput
    ImGuiNavInput-Menu                             : ImGuiNavInput
    ImGuiNavInput-Input                            : ImGuiNavInput
    ImGuiNavInput-Cancel                           : ImGuiNavInput
    ImGuiNavInput-Activate                         : ImGuiNavInput
    ImGuiModFlags-Super                            : ImGuiModFlags
    ImGuiModFlags-Alt                              : ImGuiModFlags
    ImGuiModFlags-Shift                            : ImGuiModFlags
    ImGuiModFlags-Ctrl                             : ImGuiModFlags
    ImGuiModFlags-None                             : ImGuiModFlags
    ImGuiKey-NamedKey-COUNT                        : ImGuiKey
    ImGuiKey-NamedKey-END                          : ImGuiKey
    ImGuiKey-NamedKey-BEGIN                        : ImGuiKey
    ImGuiKey-ModSuper                              : ImGuiKey
    ImGuiKey-ModAlt                                : ImGuiKey
    ImGuiKey-ModShift                              : ImGuiKey
    ImGuiKey-ModCtrl                               : ImGuiKey
    ImGuiKey-GamepadRStickRight                    : ImGuiKey
    ImGuiKey-GamepadRStickLeft                     : ImGuiKey
    ImGuiKey-GamepadRStickDown                     : ImGuiKey
    ImGuiKey-GamepadRStickUp                       : ImGuiKey
    ImGuiKey-GamepadLStickRight                    : ImGuiKey
    ImGuiKey-GamepadLStickLeft                     : ImGuiKey
    ImGuiKey-GamepadLStickDown                     : ImGuiKey
    ImGuiKey-GamepadLStickUp                       : ImGuiKey
    ImGuiKey-GamepadR3                             : ImGuiKey
    ImGuiKey-GamepadL3                             : ImGuiKey
    ImGuiKey-GamepadR2                             : ImGuiKey
    ImGuiKey-GamepadL2                             : ImGuiKey
    ImGuiKey-GamepadR1                             : ImGuiKey
    ImGuiKey-GamepadL1                             : ImGuiKey
    ImGuiKey-GamepadDpadRight                      : ImGuiKey
    ImGuiKey-GamepadDpadLeft                       : ImGuiKey
    ImGuiKey-GamepadDpadDown                       : ImGuiKey
    ImGuiKey-GamepadDpadUp                         : ImGuiKey
    ImGuiKey-GamepadFaceRight                      : ImGuiKey
    ImGuiKey-GamepadFaceLeft                       : ImGuiKey
    ImGuiKey-GamepadFaceDown                       : ImGuiKey
    ImGuiKey-GamepadFaceUp                         : ImGuiKey
    ImGuiKey-GamepadBack                           : ImGuiKey
    ImGuiKey-GamepadStart                          : ImGuiKey
    ImGuiKey-KeypadEqual                           : ImGuiKey
    ImGuiKey-KeypadEnter                           : ImGuiKey
    ImGuiKey-KeypadAdd                             : ImGuiKey
    ImGuiKey-KeypadSubtract                        : ImGuiKey
    ImGuiKey-KeypadMultiply                        : ImGuiKey
    ImGuiKey-KeypadDivide                          : ImGuiKey
    ImGuiKey-KeypadDecimal                         : ImGuiKey
    ImGuiKey-Keypad9                               : ImGuiKey
    ImGuiKey-Keypad8                               : ImGuiKey
    ImGuiKey-Keypad7                               : ImGuiKey
    ImGuiKey-Keypad6                               : ImGuiKey
    ImGuiKey-Keypad5                               : ImGuiKey
    ImGuiKey-Keypad4                               : ImGuiKey
    ImGuiKey-Keypad3                               : ImGuiKey
    ImGuiKey-Keypad2                               : ImGuiKey
    ImGuiKey-Keypad1                               : ImGuiKey
    ImGuiKey-Keypad0                               : ImGuiKey
    ImGuiKey-Pause                                 : ImGuiKey
    ImGuiKey-PrintScreen                           : ImGuiKey
    ImGuiKey-NumLock                               : ImGuiKey
    ImGuiKey-ScrollLock                            : ImGuiKey
    ImGuiKey-CapsLock                              : ImGuiKey
    ImGuiKey-GraveAccent                           : ImGuiKey
    ImGuiKey-RightBracket                          : ImGuiKey
    ImGuiKey-Backslash                             : ImGuiKey
    ImGuiKey-LeftBracket                           : ImGuiKey
    ImGuiKey-Equal                                 : ImGuiKey
    ImGuiKey-Semicolon                             : ImGuiKey
    ImGuiKey-Slash                                 : ImGuiKey
    ImGuiKey-Period                                : ImGuiKey
    ImGuiKey-Minus                                 : ImGuiKey
    ImGuiKey-Comma                                 : ImGuiKey
    ImGuiKey-Apostrophe                            : ImGuiKey
    ImGuiKey-F12                                   : ImGuiKey
    ImGuiKey-F11                                   : ImGuiKey
    ImGuiKey-F10                                   : ImGuiKey
    ImGuiKey-F9                                    : ImGuiKey
    ImGuiKey-F8                                    : ImGuiKey
    ImGuiKey-F7                                    : ImGuiKey
    ImGuiKey-F6                                    : ImGuiKey
    ImGuiKey-F5                                    : ImGuiKey
    ImGuiKey-F4                                    : ImGuiKey
    ImGuiKey-F3                                    : ImGuiKey
    ImGuiKey-F2                                    : ImGuiKey
    ImGuiKey-F1                                    : ImGuiKey
    ImGuiKey-Z                                     : ImGuiKey
    ImGuiKey-Y                                     : ImGuiKey
    ImGuiKey-X                                     : ImGuiKey
    ImGuiKey-W                                     : ImGuiKey
    ImGuiKey-V                                     : ImGuiKey
    ImGuiKey-U                                     : ImGuiKey
    ImGuiKey-T                                     : ImGuiKey
    ImGuiKey-S                                     : ImGuiKey
    ImGuiKey-R                                     : ImGuiKey
    ImGuiKey-Q                                     : ImGuiKey
    ImGuiKey-P                                     : ImGuiKey
    ImGuiKey-O                                     : ImGuiKey
    ImGuiKey-N                                     : ImGuiKey
    ImGuiKey-M                                     : ImGuiKey
    ImGuiKey-L                                     : ImGuiKey
    ImGuiKey-K                                     : ImGuiKey
    ImGuiKey-J                                     : ImGuiKey
    ImGuiKey-I                                     : ImGuiKey
    ImGuiKey-H                                     : ImGuiKey
    ImGuiKey-G                                     : ImGuiKey
    ImGuiKey-F                                     : ImGuiKey
    ImGuiKey-E                                     : ImGuiKey
    ImGuiKey-D                                     : ImGuiKey
    ImGuiKey-C                                     : ImGuiKey
    ImGuiKey-B                                     : ImGuiKey
    ImGuiKey-A                                     : ImGuiKey
    ImGuiKey-9                                     : ImGuiKey
    ImGuiKey-8                                     : ImGuiKey
    ImGuiKey-7                                     : ImGuiKey
    ImGuiKey-6                                     : ImGuiKey
    ImGuiKey-5                                     : ImGuiKey
    ImGuiKey-4                                     : ImGuiKey
    ImGuiKey-3                                     : ImGuiKey
    ImGuiKey-2                                     : ImGuiKey
    ImGuiKey-1                                     : ImGuiKey
    ImGuiKey-0                                     : ImGuiKey
    ImGuiKey-Menu                                  : ImGuiKey
    ImGuiKey-RightSuper                            : ImGuiKey
    ImGuiKey-RightAlt                              : ImGuiKey
    ImGuiKey-RightShift                            : ImGuiKey
    ImGuiKey-RightCtrl                             : ImGuiKey
    ImGuiKey-LeftSuper                             : ImGuiKey
    ImGuiKey-LeftAlt                               : ImGuiKey
    ImGuiKey-LeftShift                             : ImGuiKey
    ImGuiKey-LeftCtrl                              : ImGuiKey
    ImGuiKey-Escape                                : ImGuiKey
    ImGuiKey-Enter                                 : ImGuiKey
    ImGuiKey-Space                                 : ImGuiKey
    ImGuiKey-Backspace                             : ImGuiKey
    ImGuiKey-Delete                                : ImGuiKey
    ImGuiKey-Insert                                : ImGuiKey
    ImGuiKey-End                                   : ImGuiKey
    ImGuiKey-Home                                  : ImGuiKey
    ImGuiKey-PageDown                              : ImGuiKey
    ImGuiKey-PageUp                                : ImGuiKey
    ImGuiKey-DownArrow                             : ImGuiKey
    ImGuiKey-UpArrow                               : ImGuiKey
    ImGuiKey-RightArrow                            : ImGuiKey
    ImGuiKey-LeftArrow                             : ImGuiKey
    ImGuiKey-Tab                                   : ImGuiKey
    ImGuiKey-None                                  : ImGuiKey
    ImGuiSortDirection-Descending                  : ImGuiSortDirection
    ImGuiSortDirection-Ascending                   : ImGuiSortDirection
    ImGuiSortDirection-None                        : ImGuiSortDirection
    ImGuiDir-Down                                  : ImGuiDir
    ImGuiDir-Up                                    : ImGuiDir
    ImGuiDir-Right                                 : ImGuiDir
    ImGuiDir-Left                                  : ImGuiDir
    ImGuiDir-None                                  : ImGuiDir
    ImGuiDataType-Double                           : ImGuiDataType
    ImGuiDataType-Float                            : ImGuiDataType
    ImGuiDataType-U64                              : ImGuiDataType
    ImGuiDataType-S64                              : ImGuiDataType
    ImGuiDataType-U32                              : ImGuiDataType
    ImGuiDataType-S32                              : ImGuiDataType
    ImGuiDataType-U16                              : ImGuiDataType
    ImGuiDataType-S16                              : ImGuiDataType
    ImGuiDataType-U8                               : ImGuiDataType
    ImGuiDataType-S8                               : ImGuiDataType
    ImGuiDragDropFlags-AcceptPeekOnly              : ImGuiDragDropFlags
    ImGuiDragDropFlags-AcceptNoPreviewTooltip      : ImGuiDragDropFlags
    ImGuiDragDropFlags-AcceptNoDrawDefaultRect     : ImGuiDragDropFlags
    ImGuiDragDropFlags-AcceptBeforeDelivery        : ImGuiDragDropFlags
    ImGuiDragDropFlags-SourceAutoExpirePayload     : ImGuiDragDropFlags
    ImGuiDragDropFlags-SourceExtern                : ImGuiDragDropFlags
    ImGuiDragDropFlags-SourceAllowNullID           : ImGuiDragDropFlags
    ImGuiDragDropFlags-SourceNoHoldToOpenOthers    : ImGuiDragDropFlags
    ImGuiDragDropFlags-SourceNoDisableHover        : ImGuiDragDropFlags
    ImGuiDragDropFlags-SourceNoPreviewTooltip      : ImGuiDragDropFlags
    ImGuiDragDropFlags-None                        : ImGuiDragDropFlags
    ImGuiHoveredFlags-RootAndChildWindows          : ImGuiHoveredFlags
    ImGuiHoveredFlags-RectOnly                     : ImGuiHoveredFlags
    ImGuiHoveredFlags-NoNavOverride                : ImGuiHoveredFlags
    ImGuiHoveredFlags-AllowWhenDisabled            : ImGuiHoveredFlags
    ImGuiHoveredFlags-AllowWhenOverlapped          : ImGuiHoveredFlags
    ImGuiHoveredFlags-AllowWhenBlockedByActiveItem : ImGuiHoveredFlags
    ImGuiHoveredFlags-AllowWhenBlockedByPopup      : ImGuiHoveredFlags
    ImGuiHoveredFlags-NoPopupHierarchy             : ImGuiHoveredFlags
    ImGuiHoveredFlags-AnyWindow                    : ImGuiHoveredFlags
    ImGuiHoveredFlags-RootWindow                   : ImGuiHoveredFlags
    ImGuiHoveredFlags-ChildWindows                 : ImGuiHoveredFlags
    ImGuiHoveredFlags-None                         : ImGuiHoveredFlags
    ImGuiFocusedFlags-RootAndChildWindows          : ImGuiFocusedFlags
    ImGuiFocusedFlags-NoPopupHierarchy             : ImGuiFocusedFlags
    ImGuiFocusedFlags-AnyWindow                    : ImGuiFocusedFlags
    ImGuiFocusedFlags-RootWindow                   : ImGuiFocusedFlags
    ImGuiFocusedFlags-ChildWindows                 : ImGuiFocusedFlags
    ImGuiFocusedFlags-None                         : ImGuiFocusedFlags
    ImGuiTableBgTarget-CellBg                      : ImGuiTableBgTarget
    ImGuiTableBgTarget-RowBg1                      : ImGuiTableBgTarget
    ImGuiTableBgTarget-RowBg0                      : ImGuiTableBgTarget
    ImGuiTableBgTarget-None                        : ImGuiTableBgTarget
    ImGuiTableRowFlags-Headers                     : ImGuiTableRowFlags
    ImGuiTableRowFlags-None                        : ImGuiTableRowFlags
    ImGuiTableColumnFlags-NoDirectResize-          : ImGuiTableColumnFlags
    ImGuiTableColumnFlags-StatusMask-              : ImGuiTableColumnFlags
    ImGuiTableColumnFlags-IndentMask-              : ImGuiTableColumnFlags
    ImGuiTableColumnFlags-WidthMask-               : ImGuiTableColumnFlags
    ImGuiTableColumnFlags-IsHovered                : ImGuiTableColumnFlags
    ImGuiTableColumnFlags-IsSorted                 : ImGuiTableColumnFlags
    ImGuiTableColumnFlags-IsVisible                : ImGuiTableColumnFlags
    ImGuiTableColumnFlags-IsEnabled                : ImGuiTableColumnFlags
    ImGuiTableColumnFlags-IndentDisable            : ImGuiTableColumnFlags
    ImGuiTableColumnFlags-IndentEnable             : ImGuiTableColumnFlags
    ImGuiTableColumnFlags-PreferSortDescending     : ImGuiTableColumnFlags
    ImGuiTableColumnFlags-PreferSortAscending      : ImGuiTableColumnFlags
    ImGuiTableColumnFlags-NoHeaderWidth            : ImGuiTableColumnFlags
    ImGuiTableColumnFlags-NoHeaderLabel            : ImGuiTableColumnFlags
    ImGuiTableColumnFlags-NoSortDescending         : ImGuiTableColumnFlags
    ImGuiTableColumnFlags-NoSortAscending          : ImGuiTableColumnFlags
    ImGuiTableColumnFlags-NoSort                   : ImGuiTableColumnFlags
    ImGuiTableColumnFlags-NoClip                   : ImGuiTableColumnFlags
    ImGuiTableColumnFlags-NoHide                   : ImGuiTableColumnFlags
    ImGuiTableColumnFlags-NoReorder                : ImGuiTableColumnFlags
    ImGuiTableColumnFlags-NoResize                 : ImGuiTableColumnFlags
    ImGuiTableColumnFlags-WidthFixed               : ImGuiTableColumnFlags
    ImGuiTableColumnFlags-WidthStretch             : ImGuiTableColumnFlags
    ImGuiTableColumnFlags-DefaultSort              : ImGuiTableColumnFlags
    ImGuiTableColumnFlags-DefaultHide              : ImGuiTableColumnFlags
    ImGuiTableColumnFlags-Disabled                 : ImGuiTableColumnFlags
    ImGuiTableColumnFlags-None                     : ImGuiTableColumnFlags
    ImGuiTableFlags-SizingMask-                    : ImGuiTableFlags
    ImGuiTableFlags-SortTristate                   : ImGuiTableFlags
    ImGuiTableFlags-SortMulti                      : ImGuiTableFlags
    ImGuiTableFlags-ScrollY                        : ImGuiTableFlags
    ImGuiTableFlags-ScrollX                        : ImGuiTableFlags
    ImGuiTableFlags-NoPadInnerX                    : ImGuiTableFlags
    ImGuiTableFlags-NoPadOuterX                    : ImGuiTableFlags
    ImGuiTableFlags-PadOuterX                      : ImGuiTableFlags
    ImGuiTableFlags-NoClip                         : ImGuiTableFlags
    ImGuiTableFlags-PreciseWidths                  : ImGuiTableFlags
    ImGuiTableFlags-NoKeepColumnsVisible           : ImGuiTableFlags
    ImGuiTableFlags-NoHostExtendY                  : ImGuiTableFlags
    ImGuiTableFlags-NoHostExtendX                  : ImGuiTableFlags
    ImGuiTableFlags-SizingStretchSame              : ImGuiTableFlags
    ImGuiTableFlags-SizingStretchProp              : ImGuiTableFlags
    ImGuiTableFlags-SizingFixedSame                : ImGuiTableFlags
    ImGuiTableFlags-SizingFixedFit                 : ImGuiTableFlags
    ImGuiTableFlags-NoBordersInBodyUntilResize     : ImGuiTableFlags
    ImGuiTableFlags-NoBordersInBody                : ImGuiTableFlags
    ImGuiTableFlags-Borders                        : ImGuiTableFlags
    ImGuiTableFlags-BordersOuter                   : ImGuiTableFlags
    ImGuiTableFlags-BordersInner                   : ImGuiTableFlags
    ImGuiTableFlags-BordersV                       : ImGuiTableFlags
    ImGuiTableFlags-BordersH                       : ImGuiTableFlags
    ImGuiTableFlags-BordersOuterV                  : ImGuiTableFlags
    ImGuiTableFlags-BordersInnerV                  : ImGuiTableFlags
    ImGuiTableFlags-BordersOuterH                  : ImGuiTableFlags
    ImGuiTableFlags-BordersInnerH                  : ImGuiTableFlags
    ImGuiTableFlags-RowBg                          : ImGuiTableFlags
    ImGuiTableFlags-ContextMenuInBody              : ImGuiTableFlags
    ImGuiTableFlags-NoSavedSettings                : ImGuiTableFlags
    ImGuiTableFlags-Sortable                       : ImGuiTableFlags
    ImGuiTableFlags-Hideable                       : ImGuiTableFlags
    ImGuiTableFlags-Reorderable                    : ImGuiTableFlags
    ImGuiTableFlags-Resizable                      : ImGuiTableFlags
    ImGuiTableFlags-None                           : ImGuiTableFlags
    ImGuiTabItemFlags-Trailing                     : ImGuiTabItemFlags
    ImGuiTabItemFlags-Leading                      : ImGuiTabItemFlags
    ImGuiTabItemFlags-NoReorder                    : ImGuiTabItemFlags
    ImGuiTabItemFlags-NoTooltip                    : ImGuiTabItemFlags
    ImGuiTabItemFlags-NoPushId                     : ImGuiTabItemFlags
    ImGuiTabItemFlags-NoCloseWithMiddleMouseButton : ImGuiTabItemFlags
    ImGuiTabItemFlags-SetSelected                  : ImGuiTabItemFlags
    ImGuiTabItemFlags-UnsavedDocument              : ImGuiTabItemFlags
    ImGuiTabItemFlags-None                         : ImGuiTabItemFlags
    ImGuiTabBarFlags-FittingPolicyDefault-         : ImGuiTabBarFlags
    ImGuiTabBarFlags-FittingPolicyMask-            : ImGuiTabBarFlags
    ImGuiTabBarFlags-FittingPolicyScroll           : ImGuiTabBarFlags
    ImGuiTabBarFlags-FittingPolicyResizeDown       : ImGuiTabBarFlags
    ImGuiTabBarFlags-NoTooltip                     : ImGuiTabBarFlags
    ImGuiTabBarFlags-NoTabListScrollingButtons     : ImGuiTabBarFlags
    ImGuiTabBarFlags-NoCloseWithMiddleMouseButton  : ImGuiTabBarFlags
    ImGuiTabBarFlags-TabListPopupButton            : ImGuiTabBarFlags
    ImGuiTabBarFlags-AutoSelectNewTabs             : ImGuiTabBarFlags
    ImGuiTabBarFlags-Reorderable                   : ImGuiTabBarFlags
    ImGuiTabBarFlags-None                          : ImGuiTabBarFlags
    ImGuiComboFlags-HeightMask-                    : ImGuiComboFlags
    ImGuiComboFlags-NoPreview                      : ImGuiComboFlags
    ImGuiComboFlags-NoArrowButton                  : ImGuiComboFlags
    ImGuiComboFlags-HeightLargest                  : ImGuiComboFlags
    ImGuiComboFlags-HeightLarge                    : ImGuiComboFlags
    ImGuiComboFlags-HeightRegular                  : ImGuiComboFlags
    ImGuiComboFlags-HeightSmall                    : ImGuiComboFlags
    ImGuiComboFlags-PopupAlignLeft                 : ImGuiComboFlags
    ImGuiComboFlags-None                           : ImGuiComboFlags
    ImGuiSelectableFlags-AllowItemOverlap          : ImGuiSelectableFlags
    ImGuiSelectableFlags-Disabled                  : ImGuiSelectableFlags
    ImGuiSelectableFlags-AllowDoubleClick          : ImGuiSelectableFlags
    ImGuiSelectableFlags-SpanAllColumns            : ImGuiSelectableFlags
    ImGuiSelectableFlags-DontClosePopups           : ImGuiSelectableFlags
    ImGuiSelectableFlags-None                      : ImGuiSelectableFlags
    ImGuiPopupFlags-AnyPopup                       : ImGuiPopupFlags
    ImGuiPopupFlags-AnyPopupLevel                  : ImGuiPopupFlags
    ImGuiPopupFlags-AnyPopupId                     : ImGuiPopupFlags
    ImGuiPopupFlags-NoOpenOverItems                : ImGuiPopupFlags
    ImGuiPopupFlags-NoOpenOverExistingPopup        : ImGuiPopupFlags
    ImGuiPopupFlags-MouseButtonDefault-            : ImGuiPopupFlags
    ImGuiPopupFlags-MouseButtonMask-               : ImGuiPopupFlags
    ImGuiPopupFlags-MouseButtonMiddle              : ImGuiPopupFlags
    ImGuiPopupFlags-MouseButtonRight               : ImGuiPopupFlags
    ImGuiPopupFlags-MouseButtonLeft                : ImGuiPopupFlags
    ImGuiPopupFlags-None                           : ImGuiPopupFlags
    ImGuiTreeNodeFlags-CollapsingHeader            : ImGuiTreeNodeFlags
    ImGuiTreeNodeFlags-NavLeftJumpsBackHere        : ImGuiTreeNodeFlags
    ImGuiTreeNodeFlags-SpanFullWidth               : ImGuiTreeNodeFlags
    ImGuiTreeNodeFlags-SpanAvailWidth              : ImGuiTreeNodeFlags
    ImGuiTreeNodeFlags-FramePadding                : ImGuiTreeNodeFlags
    ImGuiTreeNodeFlags-Bullet                      : ImGuiTreeNodeFlags
    ImGuiTreeNodeFlags-Leaf                        : ImGuiTreeNodeFlags
    ImGuiTreeNodeFlags-OpenOnArrow                 : ImGuiTreeNodeFlags
    ImGuiTreeNodeFlags-OpenOnDoubleClick           : ImGuiTreeNodeFlags
    ImGuiTreeNodeFlags-DefaultOpen                 : ImGuiTreeNodeFlags
    ImGuiTreeNodeFlags-NoAutoOpenOnLog             : ImGuiTreeNodeFlags
    ImGuiTreeNodeFlags-NoTreePushOnOpen            : ImGuiTreeNodeFlags
    ImGuiTreeNodeFlags-AllowItemOverlap            : ImGuiTreeNodeFlags
    ImGuiTreeNodeFlags-Framed                      : ImGuiTreeNodeFlags
    ImGuiTreeNodeFlags-Selected                    : ImGuiTreeNodeFlags
    ImGuiTreeNodeFlags-None                        : ImGuiTreeNodeFlags
    ImGuiInputTextFlags-CallbackEdit               : ImGuiInputTextFlags
    ImGuiInputTextFlags-CallbackResize             : ImGuiInputTextFlags
    ImGuiInputTextFlags-CharsScientific            : ImGuiInputTextFlags
    ImGuiInputTextFlags-NoUndoRedo                 : ImGuiInputTextFlags
    ImGuiInputTextFlags-Password                   : ImGuiInputTextFlags
    ImGuiInputTextFlags-ReadOnly                   : ImGuiInputTextFlags
    ImGuiInputTextFlags-AlwaysOverwrite            : ImGuiInputTextFlags
    ImGuiInputTextFlags-NoHorizontalScroll         : ImGuiInputTextFlags
    ImGuiInputTextFlags-CtrlEnterForNewLine        : ImGuiInputTextFlags
    ImGuiInputTextFlags-AllowTabInput              : ImGuiInputTextFlags
    ImGuiInputTextFlags-CallbackCharFilter         : ImGuiInputTextFlags
    ImGuiInputTextFlags-CallbackAlways             : ImGuiInputTextFlags
    ImGuiInputTextFlags-CallbackHistory            : ImGuiInputTextFlags
    ImGuiInputTextFlags-CallbackCompletion         : ImGuiInputTextFlags
    ImGuiInputTextFlags-EnterReturnsTrue           : ImGuiInputTextFlags
    ImGuiInputTextFlags-AutoSelectAll              : ImGuiInputTextFlags
    ImGuiInputTextFlags-CharsNoBlank               : ImGuiInputTextFlags
    ImGuiInputTextFlags-CharsUppercase             : ImGuiInputTextFlags
    ImGuiInputTextFlags-CharsHexadecimal           : ImGuiInputTextFlags
    ImGuiInputTextFlags-CharsDecimal               : ImGuiInputTextFlags
    ImGuiInputTextFlags-None                       : ImGuiInputTextFlags
    ImGuiWindowFlags-ChildMenu                     : ImGuiWindowFlags
    ImGuiWindowFlags-Modal                         : ImGuiWindowFlags
    ImGuiWindowFlags-Popup                         : ImGuiWindowFlags
    ImGuiWindowFlags-Tooltip                       : ImGuiWindowFlags
    ImGuiWindowFlags-ChildWindow                   : ImGuiWindowFlags
    ImGuiWindowFlags-NavFlattened                  : ImGuiWindowFlags
    ImGuiWindowFlags-NoInputs                      : ImGuiWindowFlags
    ImGuiWindowFlags-NoDecoration                  : ImGuiWindowFlags
    ImGuiWindowFlags-NoNav                         : ImGuiWindowFlags
    ImGuiWindowFlags-UnsavedDocument               : ImGuiWindowFlags
    ImGuiWindowFlags-NoNavFocus                    : ImGuiWindowFlags
    ImGuiWindowFlags-NoNavInputs                   : ImGuiWindowFlags
    ImGuiWindowFlags-AlwaysUseWindowPadding        : ImGuiWindowFlags
    ImGuiWindowFlags-AlwaysHorizontalScrollbar     : ImGuiWindowFlags
    ImGuiWindowFlags-AlwaysVerticalScrollbar       : ImGuiWindowFlags
    ImGuiWindowFlags-NoBringToFrontOnFocus         : ImGuiWindowFlags
    ImGuiWindowFlags-NoFocusOnAppearing            : ImGuiWindowFlags
    ImGuiWindowFlags-HorizontalScrollbar           : ImGuiWindowFlags
    ImGuiWindowFlags-MenuBar                       : ImGuiWindowFlags
    ImGuiWindowFlags-NoMouseInputs                 : ImGuiWindowFlags
    ImGuiWindowFlags-NoSavedSettings               : ImGuiWindowFlags
    ImGuiWindowFlags-NoBackground                  : ImGuiWindowFlags
    ImGuiWindowFlags-AlwaysAutoResize              : ImGuiWindowFlags
    ImGuiWindowFlags-NoCollapse                    : ImGuiWindowFlags
    ImGuiWindowFlags-NoScrollWithMouse             : ImGuiWindowFlags
    ImGuiWindowFlags-NoScrollbar                   : ImGuiWindowFlags
    ImGuiWindowFlags-NoMove                        : ImGuiWindowFlags
    ImGuiWindowFlags-NoResize                      : ImGuiWindowFlags
    ImGuiWindowFlags-NoTitleBar                    : ImGuiWindowFlags
    ImGuiWindowFlags-None                          : ImGuiWindowFlags

{-# COMPILE GHC ImFontAtlasFlags-NoBakedLines                  = DearImGui.Raw.ImFontAtlasFlags_NoBakedLines                  #-}
{-# COMPILE GHC ImFontAtlasFlags-NoMouseCursors                = DearImGui.Raw.ImFontAtlasFlags_NoMouseCursors                #-}
{-# COMPILE GHC ImFontAtlasFlags-NoPowerOfTwoHeight            = DearImGui.Raw.ImFontAtlasFlags_NoPowerOfTwoHeight            #-}
{-# COMPILE GHC ImFontAtlasFlags-None                          = DearImGui.Raw.ImFontAtlasFlags_None                          #-}
{-# COMPILE GHC ImDrawListFlags-AllowVtxOffset                 = DearImGui.Raw.ImDrawListFlags_AllowVtxOffset                 #-}
{-# COMPILE GHC ImDrawListFlags-AntiAliasedFill                = DearImGui.Raw.ImDrawListFlags_AntiAliasedFill                #-}
{-# COMPILE GHC ImDrawListFlags-AntiAliasedLinesUseTex         = DearImGui.Raw.ImDrawListFlags_AntiAliasedLinesUseTex         #-}
{-# COMPILE GHC ImDrawListFlags-AntiAliasedLines               = DearImGui.Raw.ImDrawListFlags_AntiAliasedLines               #-}
{-# COMPILE GHC ImDrawListFlags-None                           = DearImGui.Raw.ImDrawListFlags_None                           #-}
{-# COMPILE GHC ImDrawFlags-RoundCornersMask-                  = DearImGui.Raw.ImDrawFlags_RoundCornersMask_                  #-}
{-# COMPILE GHC ImDrawFlags-RoundCornersDefault-               = DearImGui.Raw.ImDrawFlags_RoundCornersDefault_               #-}
{-# COMPILE GHC ImDrawFlags-RoundCornersAll                    = DearImGui.Raw.ImDrawFlags_RoundCornersAll                    #-}
{-# COMPILE GHC ImDrawFlags-RoundCornersRight                  = DearImGui.Raw.ImDrawFlags_RoundCornersRight                  #-}
{-# COMPILE GHC ImDrawFlags-RoundCornersLeft                   = DearImGui.Raw.ImDrawFlags_RoundCornersLeft                   #-}
{-# COMPILE GHC ImDrawFlags-RoundCornersBottom                 = DearImGui.Raw.ImDrawFlags_RoundCornersBottom                 #-}
{-# COMPILE GHC ImDrawFlags-RoundCornersTop                    = DearImGui.Raw.ImDrawFlags_RoundCornersTop                    #-}
{-# COMPILE GHC ImDrawFlags-RoundCornersNone                   = DearImGui.Raw.ImDrawFlags_RoundCornersNone                   #-}
{-# COMPILE GHC ImDrawFlags-RoundCornersBottomRight            = DearImGui.Raw.ImDrawFlags_RoundCornersBottomRight            #-}
{-# COMPILE GHC ImDrawFlags-RoundCornersBottomLeft             = DearImGui.Raw.ImDrawFlags_RoundCornersBottomLeft             #-}
{-# COMPILE GHC ImDrawFlags-RoundCornersTopRight               = DearImGui.Raw.ImDrawFlags_RoundCornersTopRight               #-}
{-# COMPILE GHC ImDrawFlags-RoundCornersTopLeft                = DearImGui.Raw.ImDrawFlags_RoundCornersTopLeft                #-}
{-# COMPILE GHC ImDrawFlags-Closed                             = DearImGui.Raw.ImDrawFlags_Closed                             #-}
{-# COMPILE GHC ImDrawFlags-None                               = DearImGui.Raw.ImDrawFlags_None                               #-}
{-# COMPILE GHC ImGuiCond-Appearing                            = DearImGui.Raw.ImGuiCond_Appearing                            #-}
{-# COMPILE GHC ImGuiCond-FirstUseEver                         = DearImGui.Raw.ImGuiCond_FirstUseEver                         #-}
{-# COMPILE GHC ImGuiCond-Once                                 = DearImGui.Raw.ImGuiCond_Once                                 #-}
{-# COMPILE GHC ImGuiCond-Always                               = DearImGui.Raw.ImGuiCond_Always                               #-}
{-# COMPILE GHC ImGuiCond-None                                 = DearImGui.Raw.ImGuiCond_None                                 #-}
{-# COMPILE GHC ImGuiMouseCursor-NotAllowed                    = DearImGui.Raw.ImGuiMouseCursor_NotAllowed                    #-}
{-# COMPILE GHC ImGuiMouseCursor-Hand                          = DearImGui.Raw.ImGuiMouseCursor_Hand                          #-}
{-# COMPILE GHC ImGuiMouseCursor-ResizeNWSE                    = DearImGui.Raw.ImGuiMouseCursor_ResizeNWSE                    #-}
{-# COMPILE GHC ImGuiMouseCursor-ResizeNESW                    = DearImGui.Raw.ImGuiMouseCursor_ResizeNESW                    #-}
{-# COMPILE GHC ImGuiMouseCursor-ResizeEW                      = DearImGui.Raw.ImGuiMouseCursor_ResizeEW                      #-}
{-# COMPILE GHC ImGuiMouseCursor-ResizeNS                      = DearImGui.Raw.ImGuiMouseCursor_ResizeNS                      #-}
{-# COMPILE GHC ImGuiMouseCursor-ResizeAll                     = DearImGui.Raw.ImGuiMouseCursor_ResizeAll                     #-}
{-# COMPILE GHC ImGuiMouseCursor-TextInput                     = DearImGui.Raw.ImGuiMouseCursor_TextInput                     #-}
{-# COMPILE GHC ImGuiMouseCursor-Arrow                         = DearImGui.Raw.ImGuiMouseCursor_Arrow                         #-}
{-# COMPILE GHC ImGuiMouseCursor-None                          = DearImGui.Raw.ImGuiMouseCursor_None                          #-}
{-# COMPILE GHC ImGuiMouseButton-Middle                        = DearImGui.Raw.ImGuiMouseButton_Middle                        #-}
{-# COMPILE GHC ImGuiMouseButton-Right                         = DearImGui.Raw.ImGuiMouseButton_Right                         #-}
{-# COMPILE GHC ImGuiMouseButton-Left                          = DearImGui.Raw.ImGuiMouseButton_Left                          #-}
{-# COMPILE GHC ImGuiSliderFlags-InvalidMask-                  = DearImGui.Raw.ImGuiSliderFlags_InvalidMask_                  #-}
{-# COMPILE GHC ImGuiSliderFlags-NoInput                       = DearImGui.Raw.ImGuiSliderFlags_NoInput                       #-}
{-# COMPILE GHC ImGuiSliderFlags-NoRoundToFormat               = DearImGui.Raw.ImGuiSliderFlags_NoRoundToFormat               #-}
{-# COMPILE GHC ImGuiSliderFlags-Logarithmic                   = DearImGui.Raw.ImGuiSliderFlags_Logarithmic                   #-}
{-# COMPILE GHC ImGuiSliderFlags-AlwaysClamp                   = DearImGui.Raw.ImGuiSliderFlags_AlwaysClamp                   #-}
{-# COMPILE GHC ImGuiSliderFlags-None                          = DearImGui.Raw.ImGuiSliderFlags_None                          #-}
{-# COMPILE GHC ImGuiColorEditFlags-InputMask-                 = DearImGui.Raw.ImGuiColorEditFlags_InputMask_                 #-}
{-# COMPILE GHC ImGuiColorEditFlags-PickerMask-                = DearImGui.Raw.ImGuiColorEditFlags_PickerMask_                #-}
{-# COMPILE GHC ImGuiColorEditFlags-DataTypeMask-              = DearImGui.Raw.ImGuiColorEditFlags_DataTypeMask_              #-}
{-# COMPILE GHC ImGuiColorEditFlags-DisplayMask-               = DearImGui.Raw.ImGuiColorEditFlags_DisplayMask_               #-}
{-# COMPILE GHC ImGuiColorEditFlags-DefaultOptions-            = DearImGui.Raw.ImGuiColorEditFlags_DefaultOptions_            #-}
{-# COMPILE GHC ImGuiColorEditFlags-InputHSV                   = DearImGui.Raw.ImGuiColorEditFlags_InputHSV                   #-}
{-# COMPILE GHC ImGuiColorEditFlags-InputRGB                   = DearImGui.Raw.ImGuiColorEditFlags_InputRGB                   #-}
{-# COMPILE GHC ImGuiColorEditFlags-PickerHueWheel             = DearImGui.Raw.ImGuiColorEditFlags_PickerHueWheel             #-}
{-# COMPILE GHC ImGuiColorEditFlags-PickerHueBar               = DearImGui.Raw.ImGuiColorEditFlags_PickerHueBar               #-}
{-# COMPILE GHC ImGuiColorEditFlags-Float                      = DearImGui.Raw.ImGuiColorEditFlags_Float                      #-}
{-# COMPILE GHC ImGuiColorEditFlags-Uint8                      = DearImGui.Raw.ImGuiColorEditFlags_Uint8                      #-}
{-# COMPILE GHC ImGuiColorEditFlags-DisplayHex                 = DearImGui.Raw.ImGuiColorEditFlags_DisplayHex                 #-}
{-# COMPILE GHC ImGuiColorEditFlags-DisplayHSV                 = DearImGui.Raw.ImGuiColorEditFlags_DisplayHSV                 #-}
{-# COMPILE GHC ImGuiColorEditFlags-DisplayRGB                 = DearImGui.Raw.ImGuiColorEditFlags_DisplayRGB                 #-}
{-# COMPILE GHC ImGuiColorEditFlags-HDR                        = DearImGui.Raw.ImGuiColorEditFlags_HDR                        #-}
{-# COMPILE GHC ImGuiColorEditFlags-AlphaPreviewHalf           = DearImGui.Raw.ImGuiColorEditFlags_AlphaPreviewHalf           #-}
{-# COMPILE GHC ImGuiColorEditFlags-AlphaPreview               = DearImGui.Raw.ImGuiColorEditFlags_AlphaPreview               #-}
{-# COMPILE GHC ImGuiColorEditFlags-AlphaBar                   = DearImGui.Raw.ImGuiColorEditFlags_AlphaBar                   #-}
{-# COMPILE GHC ImGuiColorEditFlags-NoBorder                   = DearImGui.Raw.ImGuiColorEditFlags_NoBorder                   #-}
{-# COMPILE GHC ImGuiColorEditFlags-NoDragDrop                 = DearImGui.Raw.ImGuiColorEditFlags_NoDragDrop                 #-}
{-# COMPILE GHC ImGuiColorEditFlags-NoSidePreview              = DearImGui.Raw.ImGuiColorEditFlags_NoSidePreview              #-}
{-# COMPILE GHC ImGuiColorEditFlags-NoLabel                    = DearImGui.Raw.ImGuiColorEditFlags_NoLabel                    #-}
{-# COMPILE GHC ImGuiColorEditFlags-NoTooltip                  = DearImGui.Raw.ImGuiColorEditFlags_NoTooltip                  #-}
{-# COMPILE GHC ImGuiColorEditFlags-NoInputs                   = DearImGui.Raw.ImGuiColorEditFlags_NoInputs                   #-}
{-# COMPILE GHC ImGuiColorEditFlags-NoSmallPreview             = DearImGui.Raw.ImGuiColorEditFlags_NoSmallPreview             #-}
{-# COMPILE GHC ImGuiColorEditFlags-NoOptions                  = DearImGui.Raw.ImGuiColorEditFlags_NoOptions                  #-}
{-# COMPILE GHC ImGuiColorEditFlags-NoPicker                   = DearImGui.Raw.ImGuiColorEditFlags_NoPicker                   #-}
{-# COMPILE GHC ImGuiColorEditFlags-NoAlpha                    = DearImGui.Raw.ImGuiColorEditFlags_NoAlpha                    #-}
{-# COMPILE GHC ImGuiColorEditFlags-None                       = DearImGui.Raw.ImGuiColorEditFlags_None                       #-}
{-# COMPILE GHC ImGuiButtonFlags-MouseButtonDefault-           = DearImGui.Raw.ImGuiButtonFlags_MouseButtonDefault_           #-}
{-# COMPILE GHC ImGuiButtonFlags-MouseButtonMask-              = DearImGui.Raw.ImGuiButtonFlags_MouseButtonMask_              #-}
{-# COMPILE GHC ImGuiButtonFlags-MouseButtonMiddle             = DearImGui.Raw.ImGuiButtonFlags_MouseButtonMiddle             #-}
{-# COMPILE GHC ImGuiButtonFlags-MouseButtonRight              = DearImGui.Raw.ImGuiButtonFlags_MouseButtonRight              #-}
{-# COMPILE GHC ImGuiButtonFlags-MouseButtonLeft               = DearImGui.Raw.ImGuiButtonFlags_MouseButtonLeft               #-}
{-# COMPILE GHC ImGuiButtonFlags-None                          = DearImGui.Raw.ImGuiButtonFlags_None                          #-}
{-# COMPILE GHC ImGuiStyleVar-SelectableTextAlign              = DearImGui.Raw.ImGuiStyleVar_SelectableTextAlign              #-}
{-# COMPILE GHC ImGuiStyleVar-ButtonTextAlign                  = DearImGui.Raw.ImGuiStyleVar_ButtonTextAlign                  #-}
{-# COMPILE GHC ImGuiStyleVar-TabRounding                      = DearImGui.Raw.ImGuiStyleVar_TabRounding                      #-}
{-# COMPILE GHC ImGuiStyleVar-GrabRounding                     = DearImGui.Raw.ImGuiStyleVar_GrabRounding                     #-}
{-# COMPILE GHC ImGuiStyleVar-GrabMinSize                      = DearImGui.Raw.ImGuiStyleVar_GrabMinSize                      #-}
{-# COMPILE GHC ImGuiStyleVar-ScrollbarRounding                = DearImGui.Raw.ImGuiStyleVar_ScrollbarRounding                #-}
{-# COMPILE GHC ImGuiStyleVar-ScrollbarSize                    = DearImGui.Raw.ImGuiStyleVar_ScrollbarSize                    #-}
{-# COMPILE GHC ImGuiStyleVar-CellPadding                      = DearImGui.Raw.ImGuiStyleVar_CellPadding                      #-}
{-# COMPILE GHC ImGuiStyleVar-IndentSpacing                    = DearImGui.Raw.ImGuiStyleVar_IndentSpacing                    #-}
{-# COMPILE GHC ImGuiStyleVar-ItemInnerSpacing                 = DearImGui.Raw.ImGuiStyleVar_ItemInnerSpacing                 #-}
{-# COMPILE GHC ImGuiStyleVar-ItemSpacing                      = DearImGui.Raw.ImGuiStyleVar_ItemSpacing                      #-}
{-# COMPILE GHC ImGuiStyleVar-FrameBorderSize                  = DearImGui.Raw.ImGuiStyleVar_FrameBorderSize                  #-}
{-# COMPILE GHC ImGuiStyleVar-FrameRounding                    = DearImGui.Raw.ImGuiStyleVar_FrameRounding                    #-}
{-# COMPILE GHC ImGuiStyleVar-FramePadding                     = DearImGui.Raw.ImGuiStyleVar_FramePadding                     #-}
{-# COMPILE GHC ImGuiStyleVar-PopupBorderSize                  = DearImGui.Raw.ImGuiStyleVar_PopupBorderSize                  #-}
{-# COMPILE GHC ImGuiStyleVar-PopupRounding                    = DearImGui.Raw.ImGuiStyleVar_PopupRounding                    #-}
{-# COMPILE GHC ImGuiStyleVar-ChildBorderSize                  = DearImGui.Raw.ImGuiStyleVar_ChildBorderSize                  #-}
{-# COMPILE GHC ImGuiStyleVar-ChildRounding                    = DearImGui.Raw.ImGuiStyleVar_ChildRounding                    #-}
{-# COMPILE GHC ImGuiStyleVar-WindowTitleAlign                 = DearImGui.Raw.ImGuiStyleVar_WindowTitleAlign                 #-}
{-# COMPILE GHC ImGuiStyleVar-WindowMinSize                    = DearImGui.Raw.ImGuiStyleVar_WindowMinSize                    #-}
{-# COMPILE GHC ImGuiStyleVar-WindowBorderSize                 = DearImGui.Raw.ImGuiStyleVar_WindowBorderSize                 #-}
{-# COMPILE GHC ImGuiStyleVar-WindowRounding                   = DearImGui.Raw.ImGuiStyleVar_WindowRounding                   #-}
{-# COMPILE GHC ImGuiStyleVar-WindowPadding                    = DearImGui.Raw.ImGuiStyleVar_WindowPadding                    #-}
{-# COMPILE GHC ImGuiStyleVar-DisabledAlpha                    = DearImGui.Raw.ImGuiStyleVar_DisabledAlpha                    #-}
{-# COMPILE GHC ImGuiStyleVar-Alpha                            = DearImGui.Raw.ImGuiStyleVar_Alpha                            #-}
{-# COMPILE GHC ImGuiCol-ModalWindowDimBg                      = DearImGui.Raw.ImGuiCol_ModalWindowDimBg                      #-}
{-# COMPILE GHC ImGuiCol-NavWindowingDimBg                     = DearImGui.Raw.ImGuiCol_NavWindowingDimBg                     #-}
{-# COMPILE GHC ImGuiCol-NavWindowingHighlight                 = DearImGui.Raw.ImGuiCol_NavWindowingHighlight                 #-}
{-# COMPILE GHC ImGuiCol-NavHighlight                          = DearImGui.Raw.ImGuiCol_NavHighlight                          #-}
{-# COMPILE GHC ImGuiCol-DragDropTarget                        = DearImGui.Raw.ImGuiCol_DragDropTarget                        #-}
{-# COMPILE GHC ImGuiCol-TextSelectedBg                        = DearImGui.Raw.ImGuiCol_TextSelectedBg                        #-}
{-# COMPILE GHC ImGuiCol-TableRowBgAlt                         = DearImGui.Raw.ImGuiCol_TableRowBgAlt                         #-}
{-# COMPILE GHC ImGuiCol-TableRowBg                            = DearImGui.Raw.ImGuiCol_TableRowBg                            #-}
{-# COMPILE GHC ImGuiCol-TableBorderLight                      = DearImGui.Raw.ImGuiCol_TableBorderLight                      #-}
{-# COMPILE GHC ImGuiCol-TableBorderStrong                     = DearImGui.Raw.ImGuiCol_TableBorderStrong                     #-}
{-# COMPILE GHC ImGuiCol-TableHeaderBg                         = DearImGui.Raw.ImGuiCol_TableHeaderBg                         #-}
{-# COMPILE GHC ImGuiCol-PlotHistogramHovered                  = DearImGui.Raw.ImGuiCol_PlotHistogramHovered                  #-}
{-# COMPILE GHC ImGuiCol-PlotHistogram                         = DearImGui.Raw.ImGuiCol_PlotHistogram                         #-}
{-# COMPILE GHC ImGuiCol-PlotLinesHovered                      = DearImGui.Raw.ImGuiCol_PlotLinesHovered                      #-}
{-# COMPILE GHC ImGuiCol-PlotLines                             = DearImGui.Raw.ImGuiCol_PlotLines                             #-}
{-# COMPILE GHC ImGuiCol-TabUnfocusedActive                    = DearImGui.Raw.ImGuiCol_TabUnfocusedActive                    #-}
{-# COMPILE GHC ImGuiCol-TabUnfocused                          = DearImGui.Raw.ImGuiCol_TabUnfocused                          #-}
{-# COMPILE GHC ImGuiCol-TabActive                             = DearImGui.Raw.ImGuiCol_TabActive                             #-}
{-# COMPILE GHC ImGuiCol-TabHovered                            = DearImGui.Raw.ImGuiCol_TabHovered                            #-}
{-# COMPILE GHC ImGuiCol-Tab                                   = DearImGui.Raw.ImGuiCol_Tab                                   #-}
{-# COMPILE GHC ImGuiCol-ResizeGripActive                      = DearImGui.Raw.ImGuiCol_ResizeGripActive                      #-}
{-# COMPILE GHC ImGuiCol-ResizeGripHovered                     = DearImGui.Raw.ImGuiCol_ResizeGripHovered                     #-}
{-# COMPILE GHC ImGuiCol-ResizeGrip                            = DearImGui.Raw.ImGuiCol_ResizeGrip                            #-}
{-# COMPILE GHC ImGuiCol-SeparatorActive                       = DearImGui.Raw.ImGuiCol_SeparatorActive                       #-}
{-# COMPILE GHC ImGuiCol-SeparatorHovered                      = DearImGui.Raw.ImGuiCol_SeparatorHovered                      #-}
{-# COMPILE GHC ImGuiCol-Separator                             = DearImGui.Raw.ImGuiCol_Separator                             #-}
{-# COMPILE GHC ImGuiCol-HeaderActive                          = DearImGui.Raw.ImGuiCol_HeaderActive                          #-}
{-# COMPILE GHC ImGuiCol-HeaderHovered                         = DearImGui.Raw.ImGuiCol_HeaderHovered                         #-}
{-# COMPILE GHC ImGuiCol-Header                                = DearImGui.Raw.ImGuiCol_Header                                #-}
{-# COMPILE GHC ImGuiCol-ButtonActive                          = DearImGui.Raw.ImGuiCol_ButtonActive                          #-}
{-# COMPILE GHC ImGuiCol-ButtonHovered                         = DearImGui.Raw.ImGuiCol_ButtonHovered                         #-}
{-# COMPILE GHC ImGuiCol-Button                                = DearImGui.Raw.ImGuiCol_Button                                #-}
{-# COMPILE GHC ImGuiCol-SliderGrabActive                      = DearImGui.Raw.ImGuiCol_SliderGrabActive                      #-}
{-# COMPILE GHC ImGuiCol-SliderGrab                            = DearImGui.Raw.ImGuiCol_SliderGrab                            #-}
{-# COMPILE GHC ImGuiCol-CheckMark                             = DearImGui.Raw.ImGuiCol_CheckMark                             #-}
{-# COMPILE GHC ImGuiCol-ScrollbarGrabActive                   = DearImGui.Raw.ImGuiCol_ScrollbarGrabActive                   #-}
{-# COMPILE GHC ImGuiCol-ScrollbarGrabHovered                  = DearImGui.Raw.ImGuiCol_ScrollbarGrabHovered                  #-}
{-# COMPILE GHC ImGuiCol-ScrollbarGrab                         = DearImGui.Raw.ImGuiCol_ScrollbarGrab                         #-}
{-# COMPILE GHC ImGuiCol-ScrollbarBg                           = DearImGui.Raw.ImGuiCol_ScrollbarBg                           #-}
{-# COMPILE GHC ImGuiCol-MenuBarBg                             = DearImGui.Raw.ImGuiCol_MenuBarBg                             #-}
{-# COMPILE GHC ImGuiCol-TitleBgCollapsed                      = DearImGui.Raw.ImGuiCol_TitleBgCollapsed                      #-}
{-# COMPILE GHC ImGuiCol-TitleBgActive                         = DearImGui.Raw.ImGuiCol_TitleBgActive                         #-}
{-# COMPILE GHC ImGuiCol-TitleBg                               = DearImGui.Raw.ImGuiCol_TitleBg                               #-}
{-# COMPILE GHC ImGuiCol-FrameBgActive                         = DearImGui.Raw.ImGuiCol_FrameBgActive                         #-}
{-# COMPILE GHC ImGuiCol-FrameBgHovered                        = DearImGui.Raw.ImGuiCol_FrameBgHovered                        #-}
{-# COMPILE GHC ImGuiCol-FrameBg                               = DearImGui.Raw.ImGuiCol_FrameBg                               #-}
{-# COMPILE GHC ImGuiCol-BorderShadow                          = DearImGui.Raw.ImGuiCol_BorderShadow                          #-}
{-# COMPILE GHC ImGuiCol-Border                                = DearImGui.Raw.ImGuiCol_Border                                #-}
{-# COMPILE GHC ImGuiCol-PopupBg                               = DearImGui.Raw.ImGuiCol_PopupBg                               #-}
{-# COMPILE GHC ImGuiCol-ChildBg                               = DearImGui.Raw.ImGuiCol_ChildBg                               #-}
{-# COMPILE GHC ImGuiCol-WindowBg                              = DearImGui.Raw.ImGuiCol_WindowBg                              #-}
{-# COMPILE GHC ImGuiCol-TextDisabled                          = DearImGui.Raw.ImGuiCol_TextDisabled                          #-}
{-# COMPILE GHC ImGuiCol-Text                                  = DearImGui.Raw.ImGuiCol_Text                                  #-}
{-# COMPILE GHC ImGuiBackendFlags-RendererHasVtxOffset         = DearImGui.Raw.ImGuiBackendFlags_RendererHasVtxOffset         #-}
{-# COMPILE GHC ImGuiBackendFlags-HasSetMousePos               = DearImGui.Raw.ImGuiBackendFlags_HasSetMousePos               #-}
{-# COMPILE GHC ImGuiBackendFlags-HasMouseCursors              = DearImGui.Raw.ImGuiBackendFlags_HasMouseCursors              #-}
{-# COMPILE GHC ImGuiBackendFlags-HasGamepad                   = DearImGui.Raw.ImGuiBackendFlags_HasGamepad                   #-}
{-# COMPILE GHC ImGuiBackendFlags-None                         = DearImGui.Raw.ImGuiBackendFlags_None                         #-}
{-# COMPILE GHC ImGuiConfigFlags-IsTouchScreen                 = DearImGui.Raw.ImGuiConfigFlags_IsTouchScreen                 #-}
{-# COMPILE GHC ImGuiConfigFlags-IsSRGB                        = DearImGui.Raw.ImGuiConfigFlags_IsSRGB                        #-}
{-# COMPILE GHC ImGuiConfigFlags-NoMouseCursorChange           = DearImGui.Raw.ImGuiConfigFlags_NoMouseCursorChange           #-}
{-# COMPILE GHC ImGuiConfigFlags-NoMouse                       = DearImGui.Raw.ImGuiConfigFlags_NoMouse                       #-}
{-# COMPILE GHC ImGuiConfigFlags-NavNoCaptureKeyboard          = DearImGui.Raw.ImGuiConfigFlags_NavNoCaptureKeyboard          #-}
{-# COMPILE GHC ImGuiConfigFlags-NavEnableSetMousePos          = DearImGui.Raw.ImGuiConfigFlags_NavEnableSetMousePos          #-}
{-# COMPILE GHC ImGuiConfigFlags-NavEnableGamepad              = DearImGui.Raw.ImGuiConfigFlags_NavEnableGamepad              #-}
{-# COMPILE GHC ImGuiConfigFlags-NavEnableKeyboard             = DearImGui.Raw.ImGuiConfigFlags_NavEnableKeyboard             #-}
{-# COMPILE GHC ImGuiConfigFlags-None                          = DearImGui.Raw.ImGuiConfigFlags_None                          #-}
{-# COMPILE GHC ImGuiNavInput-KeyDown-                         = DearImGui.Raw.ImGuiNavInput_KeyDown_                         #-}
{-# COMPILE GHC ImGuiNavInput-KeyUp-                           = DearImGui.Raw.ImGuiNavInput_KeyUp_                           #-}
{-# COMPILE GHC ImGuiNavInput-KeyRight-                        = DearImGui.Raw.ImGuiNavInput_KeyRight_                        #-}
{-# COMPILE GHC ImGuiNavInput-KeyLeft-                         = DearImGui.Raw.ImGuiNavInput_KeyLeft_                         #-}
{-# COMPILE GHC ImGuiNavInput-TweakFast                        = DearImGui.Raw.ImGuiNavInput_TweakFast                        #-}
{-# COMPILE GHC ImGuiNavInput-TweakSlow                        = DearImGui.Raw.ImGuiNavInput_TweakSlow                        #-}
{-# COMPILE GHC ImGuiNavInput-FocusNext                        = DearImGui.Raw.ImGuiNavInput_FocusNext                        #-}
{-# COMPILE GHC ImGuiNavInput-FocusPrev                        = DearImGui.Raw.ImGuiNavInput_FocusPrev                        #-}
{-# COMPILE GHC ImGuiNavInput-LStickDown                       = DearImGui.Raw.ImGuiNavInput_LStickDown                       #-}
{-# COMPILE GHC ImGuiNavInput-LStickUp                         = DearImGui.Raw.ImGuiNavInput_LStickUp                         #-}
{-# COMPILE GHC ImGuiNavInput-LStickRight                      = DearImGui.Raw.ImGuiNavInput_LStickRight                      #-}
{-# COMPILE GHC ImGuiNavInput-LStickLeft                       = DearImGui.Raw.ImGuiNavInput_LStickLeft                       #-}
{-# COMPILE GHC ImGuiNavInput-DpadDown                         = DearImGui.Raw.ImGuiNavInput_DpadDown                         #-}
{-# COMPILE GHC ImGuiNavInput-DpadUp                           = DearImGui.Raw.ImGuiNavInput_DpadUp                           #-}
{-# COMPILE GHC ImGuiNavInput-DpadRight                        = DearImGui.Raw.ImGuiNavInput_DpadRight                        #-}
{-# COMPILE GHC ImGuiNavInput-DpadLeft                         = DearImGui.Raw.ImGuiNavInput_DpadLeft                         #-}
{-# COMPILE GHC ImGuiNavInput-Menu                             = DearImGui.Raw.ImGuiNavInput_Menu                             #-}
{-# COMPILE GHC ImGuiNavInput-Input                            = DearImGui.Raw.ImGuiNavInput_Input                            #-}
{-# COMPILE GHC ImGuiNavInput-Cancel                           = DearImGui.Raw.ImGuiNavInput_Cancel                           #-}
{-# COMPILE GHC ImGuiNavInput-Activate                         = DearImGui.Raw.ImGuiNavInput_Activate                         #-}
{-# COMPILE GHC ImGuiModFlags-Super                            = DearImGui.Raw.ImGuiModFlags_Super                            #-}
{-# COMPILE GHC ImGuiModFlags-Alt                              = DearImGui.Raw.ImGuiModFlags_Alt                              #-}
{-# COMPILE GHC ImGuiModFlags-Shift                            = DearImGui.Raw.ImGuiModFlags_Shift                            #-}
{-# COMPILE GHC ImGuiModFlags-Ctrl                             = DearImGui.Raw.ImGuiModFlags_Ctrl                             #-}
{-# COMPILE GHC ImGuiModFlags-None                             = DearImGui.Raw.ImGuiModFlags_None                             #-}
{-# COMPILE GHC ImGuiKey-NamedKey-COUNT                        = DearImGui.Raw.ImGuiKey_NamedKey_COUNT                        #-}
{-# COMPILE GHC ImGuiKey-NamedKey-END                          = DearImGui.Raw.ImGuiKey_NamedKey_END                          #-}
{-# COMPILE GHC ImGuiKey-NamedKey-BEGIN                        = DearImGui.Raw.ImGuiKey_NamedKey_BEGIN                        #-}
{-# COMPILE GHC ImGuiKey-ModSuper                              = DearImGui.Raw.ImGuiKey_ModSuper                              #-}
{-# COMPILE GHC ImGuiKey-ModAlt                                = DearImGui.Raw.ImGuiKey_ModAlt                                #-}
{-# COMPILE GHC ImGuiKey-ModShift                              = DearImGui.Raw.ImGuiKey_ModShift                              #-}
{-# COMPILE GHC ImGuiKey-ModCtrl                               = DearImGui.Raw.ImGuiKey_ModCtrl                               #-}
{-# COMPILE GHC ImGuiKey-GamepadRStickRight                    = DearImGui.Raw.ImGuiKey_GamepadRStickRight                    #-}
{-# COMPILE GHC ImGuiKey-GamepadRStickLeft                     = DearImGui.Raw.ImGuiKey_GamepadRStickLeft                     #-}
{-# COMPILE GHC ImGuiKey-GamepadRStickDown                     = DearImGui.Raw.ImGuiKey_GamepadRStickDown                     #-}
{-# COMPILE GHC ImGuiKey-GamepadRStickUp                       = DearImGui.Raw.ImGuiKey_GamepadRStickUp                       #-}
{-# COMPILE GHC ImGuiKey-GamepadLStickRight                    = DearImGui.Raw.ImGuiKey_GamepadLStickRight                    #-}
{-# COMPILE GHC ImGuiKey-GamepadLStickLeft                     = DearImGui.Raw.ImGuiKey_GamepadLStickLeft                     #-}
{-# COMPILE GHC ImGuiKey-GamepadLStickDown                     = DearImGui.Raw.ImGuiKey_GamepadLStickDown                     #-}
{-# COMPILE GHC ImGuiKey-GamepadLStickUp                       = DearImGui.Raw.ImGuiKey_GamepadLStickUp                       #-}
{-# COMPILE GHC ImGuiKey-GamepadR3                             = DearImGui.Raw.ImGuiKey_GamepadR3                             #-}
{-# COMPILE GHC ImGuiKey-GamepadL3                             = DearImGui.Raw.ImGuiKey_GamepadL3                             #-}
{-# COMPILE GHC ImGuiKey-GamepadR2                             = DearImGui.Raw.ImGuiKey_GamepadR2                             #-}
{-# COMPILE GHC ImGuiKey-GamepadL2                             = DearImGui.Raw.ImGuiKey_GamepadL2                             #-}
{-# COMPILE GHC ImGuiKey-GamepadR1                             = DearImGui.Raw.ImGuiKey_GamepadR1                             #-}
{-# COMPILE GHC ImGuiKey-GamepadL1                             = DearImGui.Raw.ImGuiKey_GamepadL1                             #-}
{-# COMPILE GHC ImGuiKey-GamepadDpadRight                      = DearImGui.Raw.ImGuiKey_GamepadDpadRight                      #-}
{-# COMPILE GHC ImGuiKey-GamepadDpadLeft                       = DearImGui.Raw.ImGuiKey_GamepadDpadLeft                       #-}
{-# COMPILE GHC ImGuiKey-GamepadDpadDown                       = DearImGui.Raw.ImGuiKey_GamepadDpadDown                       #-}
{-# COMPILE GHC ImGuiKey-GamepadDpadUp                         = DearImGui.Raw.ImGuiKey_GamepadDpadUp                         #-}
{-# COMPILE GHC ImGuiKey-GamepadFaceRight                      = DearImGui.Raw.ImGuiKey_GamepadFaceRight                      #-}
{-# COMPILE GHC ImGuiKey-GamepadFaceLeft                       = DearImGui.Raw.ImGuiKey_GamepadFaceLeft                       #-}
{-# COMPILE GHC ImGuiKey-GamepadFaceDown                       = DearImGui.Raw.ImGuiKey_GamepadFaceDown                       #-}
{-# COMPILE GHC ImGuiKey-GamepadFaceUp                         = DearImGui.Raw.ImGuiKey_GamepadFaceUp                         #-}
{-# COMPILE GHC ImGuiKey-GamepadBack                           = DearImGui.Raw.ImGuiKey_GamepadBack                           #-}
{-# COMPILE GHC ImGuiKey-GamepadStart                          = DearImGui.Raw.ImGuiKey_GamepadStart                          #-}
{-# COMPILE GHC ImGuiKey-KeypadEqual                           = DearImGui.Raw.ImGuiKey_KeypadEqual                           #-}
{-# COMPILE GHC ImGuiKey-KeypadEnter                           = DearImGui.Raw.ImGuiKey_KeypadEnter                           #-}
{-# COMPILE GHC ImGuiKey-KeypadAdd                             = DearImGui.Raw.ImGuiKey_KeypadAdd                             #-}
{-# COMPILE GHC ImGuiKey-KeypadSubtract                        = DearImGui.Raw.ImGuiKey_KeypadSubtract                        #-}
{-# COMPILE GHC ImGuiKey-KeypadMultiply                        = DearImGui.Raw.ImGuiKey_KeypadMultiply                        #-}
{-# COMPILE GHC ImGuiKey-KeypadDivide                          = DearImGui.Raw.ImGuiKey_KeypadDivide                          #-}
{-# COMPILE GHC ImGuiKey-KeypadDecimal                         = DearImGui.Raw.ImGuiKey_KeypadDecimal                         #-}
{-# COMPILE GHC ImGuiKey-Keypad9                               = DearImGui.Raw.ImGuiKey_Keypad9                               #-}
{-# COMPILE GHC ImGuiKey-Keypad8                               = DearImGui.Raw.ImGuiKey_Keypad8                               #-}
{-# COMPILE GHC ImGuiKey-Keypad7                               = DearImGui.Raw.ImGuiKey_Keypad7                               #-}
{-# COMPILE GHC ImGuiKey-Keypad6                               = DearImGui.Raw.ImGuiKey_Keypad6                               #-}
{-# COMPILE GHC ImGuiKey-Keypad5                               = DearImGui.Raw.ImGuiKey_Keypad5                               #-}
{-# COMPILE GHC ImGuiKey-Keypad4                               = DearImGui.Raw.ImGuiKey_Keypad4                               #-}
{-# COMPILE GHC ImGuiKey-Keypad3                               = DearImGui.Raw.ImGuiKey_Keypad3                               #-}
{-# COMPILE GHC ImGuiKey-Keypad2                               = DearImGui.Raw.ImGuiKey_Keypad2                               #-}
{-# COMPILE GHC ImGuiKey-Keypad1                               = DearImGui.Raw.ImGuiKey_Keypad1                               #-}
{-# COMPILE GHC ImGuiKey-Keypad0                               = DearImGui.Raw.ImGuiKey_Keypad0                               #-}
{-# COMPILE GHC ImGuiKey-Pause                                 = DearImGui.Raw.ImGuiKey_Pause                                 #-}
{-# COMPILE GHC ImGuiKey-PrintScreen                           = DearImGui.Raw.ImGuiKey_PrintScreen                           #-}
{-# COMPILE GHC ImGuiKey-NumLock                               = DearImGui.Raw.ImGuiKey_NumLock                               #-}
{-# COMPILE GHC ImGuiKey-ScrollLock                            = DearImGui.Raw.ImGuiKey_ScrollLock                            #-}
{-# COMPILE GHC ImGuiKey-CapsLock                              = DearImGui.Raw.ImGuiKey_CapsLock                              #-}
{-# COMPILE GHC ImGuiKey-GraveAccent                           = DearImGui.Raw.ImGuiKey_GraveAccent                           #-}
{-# COMPILE GHC ImGuiKey-RightBracket                          = DearImGui.Raw.ImGuiKey_RightBracket                          #-}
{-# COMPILE GHC ImGuiKey-Backslash                             = DearImGui.Raw.ImGuiKey_Backslash                             #-}
{-# COMPILE GHC ImGuiKey-LeftBracket                           = DearImGui.Raw.ImGuiKey_LeftBracket                           #-}
{-# COMPILE GHC ImGuiKey-Equal                                 = DearImGui.Raw.ImGuiKey_Equal                                 #-}
{-# COMPILE GHC ImGuiKey-Semicolon                             = DearImGui.Raw.ImGuiKey_Semicolon                             #-}
{-# COMPILE GHC ImGuiKey-Slash                                 = DearImGui.Raw.ImGuiKey_Slash                                 #-}
{-# COMPILE GHC ImGuiKey-Period                                = DearImGui.Raw.ImGuiKey_Period                                #-}
{-# COMPILE GHC ImGuiKey-Minus                                 = DearImGui.Raw.ImGuiKey_Minus                                 #-}
{-# COMPILE GHC ImGuiKey-Comma                                 = DearImGui.Raw.ImGuiKey_Comma                                 #-}
{-# COMPILE GHC ImGuiKey-Apostrophe                            = DearImGui.Raw.ImGuiKey_Apostrophe                            #-}
{-# COMPILE GHC ImGuiKey-F12                                   = DearImGui.Raw.ImGuiKey_F12                                   #-}
{-# COMPILE GHC ImGuiKey-F11                                   = DearImGui.Raw.ImGuiKey_F11                                   #-}
{-# COMPILE GHC ImGuiKey-F10                                   = DearImGui.Raw.ImGuiKey_F10                                   #-}
{-# COMPILE GHC ImGuiKey-F9                                    = DearImGui.Raw.ImGuiKey_F9                                    #-}
{-# COMPILE GHC ImGuiKey-F8                                    = DearImGui.Raw.ImGuiKey_F8                                    #-}
{-# COMPILE GHC ImGuiKey-F7                                    = DearImGui.Raw.ImGuiKey_F7                                    #-}
{-# COMPILE GHC ImGuiKey-F6                                    = DearImGui.Raw.ImGuiKey_F6                                    #-}
{-# COMPILE GHC ImGuiKey-F5                                    = DearImGui.Raw.ImGuiKey_F5                                    #-}
{-# COMPILE GHC ImGuiKey-F4                                    = DearImGui.Raw.ImGuiKey_F4                                    #-}
{-# COMPILE GHC ImGuiKey-F3                                    = DearImGui.Raw.ImGuiKey_F3                                    #-}
{-# COMPILE GHC ImGuiKey-F2                                    = DearImGui.Raw.ImGuiKey_F2                                    #-}
{-# COMPILE GHC ImGuiKey-F1                                    = DearImGui.Raw.ImGuiKey_F1                                    #-}
{-# COMPILE GHC ImGuiKey-Z                                     = DearImGui.Raw.ImGuiKey_Z                                     #-}
{-# COMPILE GHC ImGuiKey-Y                                     = DearImGui.Raw.ImGuiKey_Y                                     #-}
{-# COMPILE GHC ImGuiKey-X                                     = DearImGui.Raw.ImGuiKey_X                                     #-}
{-# COMPILE GHC ImGuiKey-W                                     = DearImGui.Raw.ImGuiKey_W                                     #-}
{-# COMPILE GHC ImGuiKey-V                                     = DearImGui.Raw.ImGuiKey_V                                     #-}
{-# COMPILE GHC ImGuiKey-U                                     = DearImGui.Raw.ImGuiKey_U                                     #-}
{-# COMPILE GHC ImGuiKey-T                                     = DearImGui.Raw.ImGuiKey_T                                     #-}
{-# COMPILE GHC ImGuiKey-S                                     = DearImGui.Raw.ImGuiKey_S                                     #-}
{-# COMPILE GHC ImGuiKey-R                                     = DearImGui.Raw.ImGuiKey_R                                     #-}
{-# COMPILE GHC ImGuiKey-Q                                     = DearImGui.Raw.ImGuiKey_Q                                     #-}
{-# COMPILE GHC ImGuiKey-P                                     = DearImGui.Raw.ImGuiKey_P                                     #-}
{-# COMPILE GHC ImGuiKey-O                                     = DearImGui.Raw.ImGuiKey_O                                     #-}
{-# COMPILE GHC ImGuiKey-N                                     = DearImGui.Raw.ImGuiKey_N                                     #-}
{-# COMPILE GHC ImGuiKey-M                                     = DearImGui.Raw.ImGuiKey_M                                     #-}
{-# COMPILE GHC ImGuiKey-L                                     = DearImGui.Raw.ImGuiKey_L                                     #-}
{-# COMPILE GHC ImGuiKey-K                                     = DearImGui.Raw.ImGuiKey_K                                     #-}
{-# COMPILE GHC ImGuiKey-J                                     = DearImGui.Raw.ImGuiKey_J                                     #-}
{-# COMPILE GHC ImGuiKey-I                                     = DearImGui.Raw.ImGuiKey_I                                     #-}
{-# COMPILE GHC ImGuiKey-H                                     = DearImGui.Raw.ImGuiKey_H                                     #-}
{-# COMPILE GHC ImGuiKey-G                                     = DearImGui.Raw.ImGuiKey_G                                     #-}
{-# COMPILE GHC ImGuiKey-F                                     = DearImGui.Raw.ImGuiKey_F                                     #-}
{-# COMPILE GHC ImGuiKey-E                                     = DearImGui.Raw.ImGuiKey_E                                     #-}
{-# COMPILE GHC ImGuiKey-D                                     = DearImGui.Raw.ImGuiKey_D                                     #-}
{-# COMPILE GHC ImGuiKey-C                                     = DearImGui.Raw.ImGuiKey_C                                     #-}
{-# COMPILE GHC ImGuiKey-B                                     = DearImGui.Raw.ImGuiKey_B                                     #-}
{-# COMPILE GHC ImGuiKey-A                                     = DearImGui.Raw.ImGuiKey_A                                     #-}
{-# COMPILE GHC ImGuiKey-9                                     = DearImGui.Raw.ImGuiKey_9                                     #-}
{-# COMPILE GHC ImGuiKey-8                                     = DearImGui.Raw.ImGuiKey_8                                     #-}
{-# COMPILE GHC ImGuiKey-7                                     = DearImGui.Raw.ImGuiKey_7                                     #-}
{-# COMPILE GHC ImGuiKey-6                                     = DearImGui.Raw.ImGuiKey_6                                     #-}
{-# COMPILE GHC ImGuiKey-5                                     = DearImGui.Raw.ImGuiKey_5                                     #-}
{-# COMPILE GHC ImGuiKey-4                                     = DearImGui.Raw.ImGuiKey_4                                     #-}
{-# COMPILE GHC ImGuiKey-3                                     = DearImGui.Raw.ImGuiKey_3                                     #-}
{-# COMPILE GHC ImGuiKey-2                                     = DearImGui.Raw.ImGuiKey_2                                     #-}
{-# COMPILE GHC ImGuiKey-1                                     = DearImGui.Raw.ImGuiKey_1                                     #-}
{-# COMPILE GHC ImGuiKey-0                                     = DearImGui.Raw.ImGuiKey_0                                     #-}
{-# COMPILE GHC ImGuiKey-Menu                                  = DearImGui.Raw.ImGuiKey_Menu                                  #-}
{-# COMPILE GHC ImGuiKey-RightSuper                            = DearImGui.Raw.ImGuiKey_RightSuper                            #-}
{-# COMPILE GHC ImGuiKey-RightAlt                              = DearImGui.Raw.ImGuiKey_RightAlt                              #-}
{-# COMPILE GHC ImGuiKey-RightShift                            = DearImGui.Raw.ImGuiKey_RightShift                            #-}
{-# COMPILE GHC ImGuiKey-RightCtrl                             = DearImGui.Raw.ImGuiKey_RightCtrl                             #-}
{-# COMPILE GHC ImGuiKey-LeftSuper                             = DearImGui.Raw.ImGuiKey_LeftSuper                             #-}
{-# COMPILE GHC ImGuiKey-LeftAlt                               = DearImGui.Raw.ImGuiKey_LeftAlt                               #-}
{-# COMPILE GHC ImGuiKey-LeftShift                             = DearImGui.Raw.ImGuiKey_LeftShift                             #-}
{-# COMPILE GHC ImGuiKey-LeftCtrl                              = DearImGui.Raw.ImGuiKey_LeftCtrl                              #-}
{-# COMPILE GHC ImGuiKey-Escape                                = DearImGui.Raw.ImGuiKey_Escape                                #-}
{-# COMPILE GHC ImGuiKey-Enter                                 = DearImGui.Raw.ImGuiKey_Enter                                 #-}
{-# COMPILE GHC ImGuiKey-Space                                 = DearImGui.Raw.ImGuiKey_Space                                 #-}
{-# COMPILE GHC ImGuiKey-Backspace                             = DearImGui.Raw.ImGuiKey_Backspace                             #-}
{-# COMPILE GHC ImGuiKey-Delete                                = DearImGui.Raw.ImGuiKey_Delete                                #-}
{-# COMPILE GHC ImGuiKey-Insert                                = DearImGui.Raw.ImGuiKey_Insert                                #-}
{-# COMPILE GHC ImGuiKey-End                                   = DearImGui.Raw.ImGuiKey_End                                   #-}
{-# COMPILE GHC ImGuiKey-Home                                  = DearImGui.Raw.ImGuiKey_Home                                  #-}
{-# COMPILE GHC ImGuiKey-PageDown                              = DearImGui.Raw.ImGuiKey_PageDown                              #-}
{-# COMPILE GHC ImGuiKey-PageUp                                = DearImGui.Raw.ImGuiKey_PageUp                                #-}
{-# COMPILE GHC ImGuiKey-DownArrow                             = DearImGui.Raw.ImGuiKey_DownArrow                             #-}
{-# COMPILE GHC ImGuiKey-UpArrow                               = DearImGui.Raw.ImGuiKey_UpArrow                               #-}
{-# COMPILE GHC ImGuiKey-RightArrow                            = DearImGui.Raw.ImGuiKey_RightArrow                            #-}
{-# COMPILE GHC ImGuiKey-LeftArrow                             = DearImGui.Raw.ImGuiKey_LeftArrow                             #-}
{-# COMPILE GHC ImGuiKey-Tab                                   = DearImGui.Raw.ImGuiKey_Tab                                   #-}
{-# COMPILE GHC ImGuiKey-None                                  = DearImGui.Raw.ImGuiKey_None                                  #-}
{-# COMPILE GHC ImGuiSortDirection-Descending                  = DearImGui.Raw.ImGuiSortDirection_Descending                  #-}
{-# COMPILE GHC ImGuiSortDirection-Ascending                   = DearImGui.Raw.ImGuiSortDirection_Ascending                   #-}
{-# COMPILE GHC ImGuiSortDirection-None                        = DearImGui.Raw.ImGuiSortDirection_None                        #-}
{-# COMPILE GHC ImGuiDir-Down                                  = DearImGui.Raw.ImGuiDir_Down                                  #-}
{-# COMPILE GHC ImGuiDir-Up                                    = DearImGui.Raw.ImGuiDir_Up                                    #-}
{-# COMPILE GHC ImGuiDir-Right                                 = DearImGui.Raw.ImGuiDir_Right                                 #-}
{-# COMPILE GHC ImGuiDir-Left                                  = DearImGui.Raw.ImGuiDir_Left                                  #-}
{-# COMPILE GHC ImGuiDir-None                                  = DearImGui.Raw.ImGuiDir_None                                  #-}
{-# COMPILE GHC ImGuiDataType-Double                           = DearImGui.Raw.ImGuiDataType_Double                           #-}
{-# COMPILE GHC ImGuiDataType-Float                            = DearImGui.Raw.ImGuiDataType_Float                            #-}
{-# COMPILE GHC ImGuiDataType-U64                              = DearImGui.Raw.ImGuiDataType_U64                              #-}
{-# COMPILE GHC ImGuiDataType-S64                              = DearImGui.Raw.ImGuiDataType_S64                              #-}
{-# COMPILE GHC ImGuiDataType-U32                              = DearImGui.Raw.ImGuiDataType_U32                              #-}
{-# COMPILE GHC ImGuiDataType-S32                              = DearImGui.Raw.ImGuiDataType_S32                              #-}
{-# COMPILE GHC ImGuiDataType-U16                              = DearImGui.Raw.ImGuiDataType_U16                              #-}
{-# COMPILE GHC ImGuiDataType-S16                              = DearImGui.Raw.ImGuiDataType_S16                              #-}
{-# COMPILE GHC ImGuiDataType-U8                               = DearImGui.Raw.ImGuiDataType_U8                               #-}
{-# COMPILE GHC ImGuiDataType-S8                               = DearImGui.Raw.ImGuiDataType_S8                               #-}
{-# COMPILE GHC ImGuiDragDropFlags-AcceptPeekOnly              = DearImGui.Raw.ImGuiDragDropFlags_AcceptPeekOnly              #-}
{-# COMPILE GHC ImGuiDragDropFlags-AcceptNoPreviewTooltip      = DearImGui.Raw.ImGuiDragDropFlags_AcceptNoPreviewTooltip      #-}
{-# COMPILE GHC ImGuiDragDropFlags-AcceptNoDrawDefaultRect     = DearImGui.Raw.ImGuiDragDropFlags_AcceptNoDrawDefaultRect     #-}
{-# COMPILE GHC ImGuiDragDropFlags-AcceptBeforeDelivery        = DearImGui.Raw.ImGuiDragDropFlags_AcceptBeforeDelivery        #-}
{-# COMPILE GHC ImGuiDragDropFlags-SourceAutoExpirePayload     = DearImGui.Raw.ImGuiDragDropFlags_SourceAutoExpirePayload     #-}
{-# COMPILE GHC ImGuiDragDropFlags-SourceExtern                = DearImGui.Raw.ImGuiDragDropFlags_SourceExtern                #-}
{-# COMPILE GHC ImGuiDragDropFlags-SourceAllowNullID           = DearImGui.Raw.ImGuiDragDropFlags_SourceAllowNullID           #-}
{-# COMPILE GHC ImGuiDragDropFlags-SourceNoHoldToOpenOthers    = DearImGui.Raw.ImGuiDragDropFlags_SourceNoHoldToOpenOthers    #-}
{-# COMPILE GHC ImGuiDragDropFlags-SourceNoDisableHover        = DearImGui.Raw.ImGuiDragDropFlags_SourceNoDisableHover        #-}
{-# COMPILE GHC ImGuiDragDropFlags-SourceNoPreviewTooltip      = DearImGui.Raw.ImGuiDragDropFlags_SourceNoPreviewTooltip      #-}
{-# COMPILE GHC ImGuiDragDropFlags-None                        = DearImGui.Raw.ImGuiDragDropFlags_None                        #-}
{-# COMPILE GHC ImGuiHoveredFlags-RootAndChildWindows          = DearImGui.Raw.ImGuiHoveredFlags_RootAndChildWindows          #-}
{-# COMPILE GHC ImGuiHoveredFlags-RectOnly                     = DearImGui.Raw.ImGuiHoveredFlags_RectOnly                     #-}
{-# COMPILE GHC ImGuiHoveredFlags-NoNavOverride                = DearImGui.Raw.ImGuiHoveredFlags_NoNavOverride                #-}
{-# COMPILE GHC ImGuiHoveredFlags-AllowWhenDisabled            = DearImGui.Raw.ImGuiHoveredFlags_AllowWhenDisabled            #-}
{-# COMPILE GHC ImGuiHoveredFlags-AllowWhenOverlapped          = DearImGui.Raw.ImGuiHoveredFlags_AllowWhenOverlapped          #-}
{-# COMPILE GHC ImGuiHoveredFlags-AllowWhenBlockedByActiveItem = DearImGui.Raw.ImGuiHoveredFlags_AllowWhenBlockedByActiveItem #-}
{-# COMPILE GHC ImGuiHoveredFlags-AllowWhenBlockedByPopup      = DearImGui.Raw.ImGuiHoveredFlags_AllowWhenBlockedByPopup      #-}
{-# COMPILE GHC ImGuiHoveredFlags-NoPopupHierarchy             = DearImGui.Raw.ImGuiHoveredFlags_NoPopupHierarchy             #-}
{-# COMPILE GHC ImGuiHoveredFlags-AnyWindow                    = DearImGui.Raw.ImGuiHoveredFlags_AnyWindow                    #-}
{-# COMPILE GHC ImGuiHoveredFlags-RootWindow                   = DearImGui.Raw.ImGuiHoveredFlags_RootWindow                   #-}
{-# COMPILE GHC ImGuiHoveredFlags-ChildWindows                 = DearImGui.Raw.ImGuiHoveredFlags_ChildWindows                 #-}
{-# COMPILE GHC ImGuiHoveredFlags-None                         = DearImGui.Raw.ImGuiHoveredFlags_None                         #-}
{-# COMPILE GHC ImGuiFocusedFlags-RootAndChildWindows          = DearImGui.Raw.ImGuiFocusedFlags_RootAndChildWindows          #-}
{-# COMPILE GHC ImGuiFocusedFlags-NoPopupHierarchy             = DearImGui.Raw.ImGuiFocusedFlags_NoPopupHierarchy             #-}
{-# COMPILE GHC ImGuiFocusedFlags-AnyWindow                    = DearImGui.Raw.ImGuiFocusedFlags_AnyWindow                    #-}
{-# COMPILE GHC ImGuiFocusedFlags-RootWindow                   = DearImGui.Raw.ImGuiFocusedFlags_RootWindow                   #-}
{-# COMPILE GHC ImGuiFocusedFlags-ChildWindows                 = DearImGui.Raw.ImGuiFocusedFlags_ChildWindows                 #-}
{-# COMPILE GHC ImGuiFocusedFlags-None                         = DearImGui.Raw.ImGuiFocusedFlags_None                         #-}
{-# COMPILE GHC ImGuiTableBgTarget-CellBg                      = DearImGui.Raw.ImGuiTableBgTarget_CellBg                      #-}
{-# COMPILE GHC ImGuiTableBgTarget-RowBg1                      = DearImGui.Raw.ImGuiTableBgTarget_RowBg1                      #-}
{-# COMPILE GHC ImGuiTableBgTarget-RowBg0                      = DearImGui.Raw.ImGuiTableBgTarget_RowBg0                      #-}
{-# COMPILE GHC ImGuiTableBgTarget-None                        = DearImGui.Raw.ImGuiTableBgTarget_None                        #-}
{-# COMPILE GHC ImGuiTableRowFlags-Headers                     = DearImGui.Raw.ImGuiTableRowFlags_Headers                     #-}
{-# COMPILE GHC ImGuiTableRowFlags-None                        = DearImGui.Raw.ImGuiTableRowFlags_None                        #-}
{-# COMPILE GHC ImGuiTableColumnFlags-NoDirectResize-          = DearImGui.Raw.ImGuiTableColumnFlags_NoDirectResize_          #-}
{-# COMPILE GHC ImGuiTableColumnFlags-StatusMask-              = DearImGui.Raw.ImGuiTableColumnFlags_StatusMask_              #-}
{-# COMPILE GHC ImGuiTableColumnFlags-IndentMask-              = DearImGui.Raw.ImGuiTableColumnFlags_IndentMask_              #-}
{-# COMPILE GHC ImGuiTableColumnFlags-WidthMask-               = DearImGui.Raw.ImGuiTableColumnFlags_WidthMask_               #-}
{-# COMPILE GHC ImGuiTableColumnFlags-IsHovered                = DearImGui.Raw.ImGuiTableColumnFlags_IsHovered                #-}
{-# COMPILE GHC ImGuiTableColumnFlags-IsSorted                 = DearImGui.Raw.ImGuiTableColumnFlags_IsSorted                 #-}
{-# COMPILE GHC ImGuiTableColumnFlags-IsVisible                = DearImGui.Raw.ImGuiTableColumnFlags_IsVisible                #-}
{-# COMPILE GHC ImGuiTableColumnFlags-IsEnabled                = DearImGui.Raw.ImGuiTableColumnFlags_IsEnabled                #-}
{-# COMPILE GHC ImGuiTableColumnFlags-IndentDisable            = DearImGui.Raw.ImGuiTableColumnFlags_IndentDisable            #-}
{-# COMPILE GHC ImGuiTableColumnFlags-IndentEnable             = DearImGui.Raw.ImGuiTableColumnFlags_IndentEnable             #-}
{-# COMPILE GHC ImGuiTableColumnFlags-PreferSortDescending     = DearImGui.Raw.ImGuiTableColumnFlags_PreferSortDescending     #-}
{-# COMPILE GHC ImGuiTableColumnFlags-PreferSortAscending      = DearImGui.Raw.ImGuiTableColumnFlags_PreferSortAscending      #-}
{-# COMPILE GHC ImGuiTableColumnFlags-NoHeaderWidth            = DearImGui.Raw.ImGuiTableColumnFlags_NoHeaderWidth            #-}
{-# COMPILE GHC ImGuiTableColumnFlags-NoHeaderLabel            = DearImGui.Raw.ImGuiTableColumnFlags_NoHeaderLabel            #-}
{-# COMPILE GHC ImGuiTableColumnFlags-NoSortDescending         = DearImGui.Raw.ImGuiTableColumnFlags_NoSortDescending         #-}
{-# COMPILE GHC ImGuiTableColumnFlags-NoSortAscending          = DearImGui.Raw.ImGuiTableColumnFlags_NoSortAscending          #-}
{-# COMPILE GHC ImGuiTableColumnFlags-NoSort                   = DearImGui.Raw.ImGuiTableColumnFlags_NoSort                   #-}
{-# COMPILE GHC ImGuiTableColumnFlags-NoClip                   = DearImGui.Raw.ImGuiTableColumnFlags_NoClip                   #-}
{-# COMPILE GHC ImGuiTableColumnFlags-NoHide                   = DearImGui.Raw.ImGuiTableColumnFlags_NoHide                   #-}
{-# COMPILE GHC ImGuiTableColumnFlags-NoReorder                = DearImGui.Raw.ImGuiTableColumnFlags_NoReorder                #-}
{-# COMPILE GHC ImGuiTableColumnFlags-NoResize                 = DearImGui.Raw.ImGuiTableColumnFlags_NoResize                 #-}
{-# COMPILE GHC ImGuiTableColumnFlags-WidthFixed               = DearImGui.Raw.ImGuiTableColumnFlags_WidthFixed               #-}
{-# COMPILE GHC ImGuiTableColumnFlags-WidthStretch             = DearImGui.Raw.ImGuiTableColumnFlags_WidthStretch             #-}
{-# COMPILE GHC ImGuiTableColumnFlags-DefaultSort              = DearImGui.Raw.ImGuiTableColumnFlags_DefaultSort              #-}
{-# COMPILE GHC ImGuiTableColumnFlags-DefaultHide              = DearImGui.Raw.ImGuiTableColumnFlags_DefaultHide              #-}
{-# COMPILE GHC ImGuiTableColumnFlags-Disabled                 = DearImGui.Raw.ImGuiTableColumnFlags_Disabled                 #-}
{-# COMPILE GHC ImGuiTableColumnFlags-None                     = DearImGui.Raw.ImGuiTableColumnFlags_None                     #-}
{-# COMPILE GHC ImGuiTableFlags-SizingMask-                    = DearImGui.Raw.ImGuiTableFlags_SizingMask_                    #-}
{-# COMPILE GHC ImGuiTableFlags-SortTristate                   = DearImGui.Raw.ImGuiTableFlags_SortTristate                   #-}
{-# COMPILE GHC ImGuiTableFlags-SortMulti                      = DearImGui.Raw.ImGuiTableFlags_SortMulti                      #-}
{-# COMPILE GHC ImGuiTableFlags-ScrollY                        = DearImGui.Raw.ImGuiTableFlags_ScrollY                        #-}
{-# COMPILE GHC ImGuiTableFlags-ScrollX                        = DearImGui.Raw.ImGuiTableFlags_ScrollX                        #-}
{-# COMPILE GHC ImGuiTableFlags-NoPadInnerX                    = DearImGui.Raw.ImGuiTableFlags_NoPadInnerX                    #-}
{-# COMPILE GHC ImGuiTableFlags-NoPadOuterX                    = DearImGui.Raw.ImGuiTableFlags_NoPadOuterX                    #-}
{-# COMPILE GHC ImGuiTableFlags-PadOuterX                      = DearImGui.Raw.ImGuiTableFlags_PadOuterX                      #-}
{-# COMPILE GHC ImGuiTableFlags-NoClip                         = DearImGui.Raw.ImGuiTableFlags_NoClip                         #-}
{-# COMPILE GHC ImGuiTableFlags-PreciseWidths                  = DearImGui.Raw.ImGuiTableFlags_PreciseWidths                  #-}
{-# COMPILE GHC ImGuiTableFlags-NoKeepColumnsVisible           = DearImGui.Raw.ImGuiTableFlags_NoKeepColumnsVisible           #-}
{-# COMPILE GHC ImGuiTableFlags-NoHostExtendY                  = DearImGui.Raw.ImGuiTableFlags_NoHostExtendY                  #-}
{-# COMPILE GHC ImGuiTableFlags-NoHostExtendX                  = DearImGui.Raw.ImGuiTableFlags_NoHostExtendX                  #-}
{-# COMPILE GHC ImGuiTableFlags-SizingStretchSame              = DearImGui.Raw.ImGuiTableFlags_SizingStretchSame              #-}
{-# COMPILE GHC ImGuiTableFlags-SizingStretchProp              = DearImGui.Raw.ImGuiTableFlags_SizingStretchProp              #-}
{-# COMPILE GHC ImGuiTableFlags-SizingFixedSame                = DearImGui.Raw.ImGuiTableFlags_SizingFixedSame                #-}
{-# COMPILE GHC ImGuiTableFlags-SizingFixedFit                 = DearImGui.Raw.ImGuiTableFlags_SizingFixedFit                 #-}
{-# COMPILE GHC ImGuiTableFlags-NoBordersInBodyUntilResize     = DearImGui.Raw.ImGuiTableFlags_NoBordersInBodyUntilResize     #-}
{-# COMPILE GHC ImGuiTableFlags-NoBordersInBody                = DearImGui.Raw.ImGuiTableFlags_NoBordersInBody                #-}
{-# COMPILE GHC ImGuiTableFlags-Borders                        = DearImGui.Raw.ImGuiTableFlags_Borders                        #-}
{-# COMPILE GHC ImGuiTableFlags-BordersOuter                   = DearImGui.Raw.ImGuiTableFlags_BordersOuter                   #-}
{-# COMPILE GHC ImGuiTableFlags-BordersInner                   = DearImGui.Raw.ImGuiTableFlags_BordersInner                   #-}
{-# COMPILE GHC ImGuiTableFlags-BordersV                       = DearImGui.Raw.ImGuiTableFlags_BordersV                       #-}
{-# COMPILE GHC ImGuiTableFlags-BordersH                       = DearImGui.Raw.ImGuiTableFlags_BordersH                       #-}
{-# COMPILE GHC ImGuiTableFlags-BordersOuterV                  = DearImGui.Raw.ImGuiTableFlags_BordersOuterV                  #-}
{-# COMPILE GHC ImGuiTableFlags-BordersInnerV                  = DearImGui.Raw.ImGuiTableFlags_BordersInnerV                  #-}
{-# COMPILE GHC ImGuiTableFlags-BordersOuterH                  = DearImGui.Raw.ImGuiTableFlags_BordersOuterH                  #-}
{-# COMPILE GHC ImGuiTableFlags-BordersInnerH                  = DearImGui.Raw.ImGuiTableFlags_BordersInnerH                  #-}
{-# COMPILE GHC ImGuiTableFlags-RowBg                          = DearImGui.Raw.ImGuiTableFlags_RowBg                          #-}
{-# COMPILE GHC ImGuiTableFlags-ContextMenuInBody              = DearImGui.Raw.ImGuiTableFlags_ContextMenuInBody              #-}
{-# COMPILE GHC ImGuiTableFlags-NoSavedSettings                = DearImGui.Raw.ImGuiTableFlags_NoSavedSettings                #-}
{-# COMPILE GHC ImGuiTableFlags-Sortable                       = DearImGui.Raw.ImGuiTableFlags_Sortable                       #-}
{-# COMPILE GHC ImGuiTableFlags-Hideable                       = DearImGui.Raw.ImGuiTableFlags_Hideable                       #-}
{-# COMPILE GHC ImGuiTableFlags-Reorderable                    = DearImGui.Raw.ImGuiTableFlags_Reorderable                    #-}
{-# COMPILE GHC ImGuiTableFlags-Resizable                      = DearImGui.Raw.ImGuiTableFlags_Resizable                      #-}
{-# COMPILE GHC ImGuiTableFlags-None                           = DearImGui.Raw.ImGuiTableFlags_None                           #-}
{-# COMPILE GHC ImGuiTabItemFlags-Trailing                     = DearImGui.Raw.ImGuiTabItemFlags_Trailing                     #-}
{-# COMPILE GHC ImGuiTabItemFlags-Leading                      = DearImGui.Raw.ImGuiTabItemFlags_Leading                      #-}
{-# COMPILE GHC ImGuiTabItemFlags-NoReorder                    = DearImGui.Raw.ImGuiTabItemFlags_NoReorder                    #-}
{-# COMPILE GHC ImGuiTabItemFlags-NoTooltip                    = DearImGui.Raw.ImGuiTabItemFlags_NoTooltip                    #-}
{-# COMPILE GHC ImGuiTabItemFlags-NoPushId                     = DearImGui.Raw.ImGuiTabItemFlags_NoPushId                     #-}
{-# COMPILE GHC ImGuiTabItemFlags-NoCloseWithMiddleMouseButton = DearImGui.Raw.ImGuiTabItemFlags_NoCloseWithMiddleMouseButton #-}
{-# COMPILE GHC ImGuiTabItemFlags-SetSelected                  = DearImGui.Raw.ImGuiTabItemFlags_SetSelected                  #-}
{-# COMPILE GHC ImGuiTabItemFlags-UnsavedDocument              = DearImGui.Raw.ImGuiTabItemFlags_UnsavedDocument              #-}
{-# COMPILE GHC ImGuiTabItemFlags-None                         = DearImGui.Raw.ImGuiTabItemFlags_None                         #-}
{-# COMPILE GHC ImGuiTabBarFlags-FittingPolicyDefault-         = DearImGui.Raw.ImGuiTabBarFlags_FittingPolicyDefault_         #-}
{-# COMPILE GHC ImGuiTabBarFlags-FittingPolicyMask-            = DearImGui.Raw.ImGuiTabBarFlags_FittingPolicyMask_            #-}
{-# COMPILE GHC ImGuiTabBarFlags-FittingPolicyScroll           = DearImGui.Raw.ImGuiTabBarFlags_FittingPolicyScroll           #-}
{-# COMPILE GHC ImGuiTabBarFlags-FittingPolicyResizeDown       = DearImGui.Raw.ImGuiTabBarFlags_FittingPolicyResizeDown       #-}
{-# COMPILE GHC ImGuiTabBarFlags-NoTooltip                     = DearImGui.Raw.ImGuiTabBarFlags_NoTooltip                     #-}
{-# COMPILE GHC ImGuiTabBarFlags-NoTabListScrollingButtons     = DearImGui.Raw.ImGuiTabBarFlags_NoTabListScrollingButtons     #-}
{-# COMPILE GHC ImGuiTabBarFlags-NoCloseWithMiddleMouseButton  = DearImGui.Raw.ImGuiTabBarFlags_NoCloseWithMiddleMouseButton  #-}
{-# COMPILE GHC ImGuiTabBarFlags-TabListPopupButton            = DearImGui.Raw.ImGuiTabBarFlags_TabListPopupButton            #-}
{-# COMPILE GHC ImGuiTabBarFlags-AutoSelectNewTabs             = DearImGui.Raw.ImGuiTabBarFlags_AutoSelectNewTabs             #-}
{-# COMPILE GHC ImGuiTabBarFlags-Reorderable                   = DearImGui.Raw.ImGuiTabBarFlags_Reorderable                   #-}
{-# COMPILE GHC ImGuiTabBarFlags-None                          = DearImGui.Raw.ImGuiTabBarFlags_None                          #-}
{-# COMPILE GHC ImGuiComboFlags-HeightMask-                    = DearImGui.Raw.ImGuiComboFlags_HeightMask_                    #-}
{-# COMPILE GHC ImGuiComboFlags-NoPreview                      = DearImGui.Raw.ImGuiComboFlags_NoPreview                      #-}
{-# COMPILE GHC ImGuiComboFlags-NoArrowButton                  = DearImGui.Raw.ImGuiComboFlags_NoArrowButton                  #-}
{-# COMPILE GHC ImGuiComboFlags-HeightLargest                  = DearImGui.Raw.ImGuiComboFlags_HeightLargest                  #-}
{-# COMPILE GHC ImGuiComboFlags-HeightLarge                    = DearImGui.Raw.ImGuiComboFlags_HeightLarge                    #-}
{-# COMPILE GHC ImGuiComboFlags-HeightRegular                  = DearImGui.Raw.ImGuiComboFlags_HeightRegular                  #-}
{-# COMPILE GHC ImGuiComboFlags-HeightSmall                    = DearImGui.Raw.ImGuiComboFlags_HeightSmall                    #-}
{-# COMPILE GHC ImGuiComboFlags-PopupAlignLeft                 = DearImGui.Raw.ImGuiComboFlags_PopupAlignLeft                 #-}
{-# COMPILE GHC ImGuiComboFlags-None                           = DearImGui.Raw.ImGuiComboFlags_None                           #-}
{-# COMPILE GHC ImGuiSelectableFlags-AllowItemOverlap          = DearImGui.Raw.ImGuiSelectableFlags_AllowItemOverlap          #-}
{-# COMPILE GHC ImGuiSelectableFlags-Disabled                  = DearImGui.Raw.ImGuiSelectableFlags_Disabled                  #-}
{-# COMPILE GHC ImGuiSelectableFlags-AllowDoubleClick          = DearImGui.Raw.ImGuiSelectableFlags_AllowDoubleClick          #-}
{-# COMPILE GHC ImGuiSelectableFlags-SpanAllColumns            = DearImGui.Raw.ImGuiSelectableFlags_SpanAllColumns            #-}
{-# COMPILE GHC ImGuiSelectableFlags-DontClosePopups           = DearImGui.Raw.ImGuiSelectableFlags_DontClosePopups           #-}
{-# COMPILE GHC ImGuiSelectableFlags-None                      = DearImGui.Raw.ImGuiSelectableFlags_None                      #-}
{-# COMPILE GHC ImGuiPopupFlags-AnyPopup                       = DearImGui.Raw.ImGuiPopupFlags_AnyPopup                       #-}
{-# COMPILE GHC ImGuiPopupFlags-AnyPopupLevel                  = DearImGui.Raw.ImGuiPopupFlags_AnyPopupLevel                  #-}
{-# COMPILE GHC ImGuiPopupFlags-AnyPopupId                     = DearImGui.Raw.ImGuiPopupFlags_AnyPopupId                     #-}
{-# COMPILE GHC ImGuiPopupFlags-NoOpenOverItems                = DearImGui.Raw.ImGuiPopupFlags_NoOpenOverItems                #-}
{-# COMPILE GHC ImGuiPopupFlags-NoOpenOverExistingPopup        = DearImGui.Raw.ImGuiPopupFlags_NoOpenOverExistingPopup        #-}
{-# COMPILE GHC ImGuiPopupFlags-MouseButtonDefault-            = DearImGui.Raw.ImGuiPopupFlags_MouseButtonDefault_            #-}
{-# COMPILE GHC ImGuiPopupFlags-MouseButtonMask-               = DearImGui.Raw.ImGuiPopupFlags_MouseButtonMask_               #-}
{-# COMPILE GHC ImGuiPopupFlags-MouseButtonMiddle              = DearImGui.Raw.ImGuiPopupFlags_MouseButtonMiddle              #-}
{-# COMPILE GHC ImGuiPopupFlags-MouseButtonRight               = DearImGui.Raw.ImGuiPopupFlags_MouseButtonRight               #-}
{-# COMPILE GHC ImGuiPopupFlags-MouseButtonLeft                = DearImGui.Raw.ImGuiPopupFlags_MouseButtonLeft                #-}
{-# COMPILE GHC ImGuiPopupFlags-None                           = DearImGui.Raw.ImGuiPopupFlags_None                           #-}
{-# COMPILE GHC ImGuiTreeNodeFlags-CollapsingHeader            = DearImGui.Raw.ImGuiTreeNodeFlags_CollapsingHeader            #-}
{-# COMPILE GHC ImGuiTreeNodeFlags-NavLeftJumpsBackHere        = DearImGui.Raw.ImGuiTreeNodeFlags_NavLeftJumpsBackHere        #-}
{-# COMPILE GHC ImGuiTreeNodeFlags-SpanFullWidth               = DearImGui.Raw.ImGuiTreeNodeFlags_SpanFullWidth               #-}
{-# COMPILE GHC ImGuiTreeNodeFlags-SpanAvailWidth              = DearImGui.Raw.ImGuiTreeNodeFlags_SpanAvailWidth              #-}
{-# COMPILE GHC ImGuiTreeNodeFlags-FramePadding                = DearImGui.Raw.ImGuiTreeNodeFlags_FramePadding                #-}
{-# COMPILE GHC ImGuiTreeNodeFlags-Bullet                      = DearImGui.Raw.ImGuiTreeNodeFlags_Bullet                      #-}
{-# COMPILE GHC ImGuiTreeNodeFlags-Leaf                        = DearImGui.Raw.ImGuiTreeNodeFlags_Leaf                        #-}
{-# COMPILE GHC ImGuiTreeNodeFlags-OpenOnArrow                 = DearImGui.Raw.ImGuiTreeNodeFlags_OpenOnArrow                 #-}
{-# COMPILE GHC ImGuiTreeNodeFlags-OpenOnDoubleClick           = DearImGui.Raw.ImGuiTreeNodeFlags_OpenOnDoubleClick           #-}
{-# COMPILE GHC ImGuiTreeNodeFlags-DefaultOpen                 = DearImGui.Raw.ImGuiTreeNodeFlags_DefaultOpen                 #-}
{-# COMPILE GHC ImGuiTreeNodeFlags-NoAutoOpenOnLog             = DearImGui.Raw.ImGuiTreeNodeFlags_NoAutoOpenOnLog             #-}
{-# COMPILE GHC ImGuiTreeNodeFlags-NoTreePushOnOpen            = DearImGui.Raw.ImGuiTreeNodeFlags_NoTreePushOnOpen            #-}
{-# COMPILE GHC ImGuiTreeNodeFlags-AllowItemOverlap            = DearImGui.Raw.ImGuiTreeNodeFlags_AllowItemOverlap            #-}
{-# COMPILE GHC ImGuiTreeNodeFlags-Framed                      = DearImGui.Raw.ImGuiTreeNodeFlags_Framed                      #-}
{-# COMPILE GHC ImGuiTreeNodeFlags-Selected                    = DearImGui.Raw.ImGuiTreeNodeFlags_Selected                    #-}
{-# COMPILE GHC ImGuiTreeNodeFlags-None                        = DearImGui.Raw.ImGuiTreeNodeFlags_None                        #-}
{-# COMPILE GHC ImGuiInputTextFlags-CallbackEdit               = DearImGui.Raw.ImGuiInputTextFlags_CallbackEdit               #-}
{-# COMPILE GHC ImGuiInputTextFlags-CallbackResize             = DearImGui.Raw.ImGuiInputTextFlags_CallbackResize             #-}
{-# COMPILE GHC ImGuiInputTextFlags-CharsScientific            = DearImGui.Raw.ImGuiInputTextFlags_CharsScientific            #-}
{-# COMPILE GHC ImGuiInputTextFlags-NoUndoRedo                 = DearImGui.Raw.ImGuiInputTextFlags_NoUndoRedo                 #-}
{-# COMPILE GHC ImGuiInputTextFlags-Password                   = DearImGui.Raw.ImGuiInputTextFlags_Password                   #-}
{-# COMPILE GHC ImGuiInputTextFlags-ReadOnly                   = DearImGui.Raw.ImGuiInputTextFlags_ReadOnly                   #-}
{-# COMPILE GHC ImGuiInputTextFlags-AlwaysOverwrite            = DearImGui.Raw.ImGuiInputTextFlags_AlwaysOverwrite            #-}
{-# COMPILE GHC ImGuiInputTextFlags-NoHorizontalScroll         = DearImGui.Raw.ImGuiInputTextFlags_NoHorizontalScroll         #-}
{-# COMPILE GHC ImGuiInputTextFlags-CtrlEnterForNewLine        = DearImGui.Raw.ImGuiInputTextFlags_CtrlEnterForNewLine        #-}
{-# COMPILE GHC ImGuiInputTextFlags-AllowTabInput              = DearImGui.Raw.ImGuiInputTextFlags_AllowTabInput              #-}
{-# COMPILE GHC ImGuiInputTextFlags-CallbackCharFilter         = DearImGui.Raw.ImGuiInputTextFlags_CallbackCharFilter         #-}
{-# COMPILE GHC ImGuiInputTextFlags-CallbackAlways             = DearImGui.Raw.ImGuiInputTextFlags_CallbackAlways             #-}
{-# COMPILE GHC ImGuiInputTextFlags-CallbackHistory            = DearImGui.Raw.ImGuiInputTextFlags_CallbackHistory            #-}
{-# COMPILE GHC ImGuiInputTextFlags-CallbackCompletion         = DearImGui.Raw.ImGuiInputTextFlags_CallbackCompletion         #-}
{-# COMPILE GHC ImGuiInputTextFlags-EnterReturnsTrue           = DearImGui.Raw.ImGuiInputTextFlags_EnterReturnsTrue           #-}
{-# COMPILE GHC ImGuiInputTextFlags-AutoSelectAll              = DearImGui.Raw.ImGuiInputTextFlags_AutoSelectAll              #-}
{-# COMPILE GHC ImGuiInputTextFlags-CharsNoBlank               = DearImGui.Raw.ImGuiInputTextFlags_CharsNoBlank               #-}
{-# COMPILE GHC ImGuiInputTextFlags-CharsUppercase             = DearImGui.Raw.ImGuiInputTextFlags_CharsUppercase             #-}
{-# COMPILE GHC ImGuiInputTextFlags-CharsHexadecimal           = DearImGui.Raw.ImGuiInputTextFlags_CharsHexadecimal           #-}
{-# COMPILE GHC ImGuiInputTextFlags-CharsDecimal               = DearImGui.Raw.ImGuiInputTextFlags_CharsDecimal               #-}
{-# COMPILE GHC ImGuiInputTextFlags-None                       = DearImGui.Raw.ImGuiInputTextFlags_None                       #-}
{-# COMPILE GHC ImGuiWindowFlags-ChildMenu                     = DearImGui.Raw.ImGuiWindowFlags_ChildMenu                     #-}
{-# COMPILE GHC ImGuiWindowFlags-Modal                         = DearImGui.Raw.ImGuiWindowFlags_Modal                         #-}
{-# COMPILE GHC ImGuiWindowFlags-Popup                         = DearImGui.Raw.ImGuiWindowFlags_Popup                         #-}
{-# COMPILE GHC ImGuiWindowFlags-Tooltip                       = DearImGui.Raw.ImGuiWindowFlags_Tooltip                       #-}
{-# COMPILE GHC ImGuiWindowFlags-ChildWindow                   = DearImGui.Raw.ImGuiWindowFlags_ChildWindow                   #-}
{-# COMPILE GHC ImGuiWindowFlags-NavFlattened                  = DearImGui.Raw.ImGuiWindowFlags_NavFlattened                  #-}
{-# COMPILE GHC ImGuiWindowFlags-NoInputs                      = DearImGui.Raw.ImGuiWindowFlags_NoInputs                      #-}
{-# COMPILE GHC ImGuiWindowFlags-NoDecoration                  = DearImGui.Raw.ImGuiWindowFlags_NoDecoration                  #-}
{-# COMPILE GHC ImGuiWindowFlags-NoNav                         = DearImGui.Raw.ImGuiWindowFlags_NoNav                         #-}
{-# COMPILE GHC ImGuiWindowFlags-UnsavedDocument               = DearImGui.Raw.ImGuiWindowFlags_UnsavedDocument               #-}
{-# COMPILE GHC ImGuiWindowFlags-NoNavFocus                    = DearImGui.Raw.ImGuiWindowFlags_NoNavFocus                    #-}
{-# COMPILE GHC ImGuiWindowFlags-NoNavInputs                   = DearImGui.Raw.ImGuiWindowFlags_NoNavInputs                   #-}
{-# COMPILE GHC ImGuiWindowFlags-AlwaysUseWindowPadding        = DearImGui.Raw.ImGuiWindowFlags_AlwaysUseWindowPadding        #-}
{-# COMPILE GHC ImGuiWindowFlags-AlwaysHorizontalScrollbar     = DearImGui.Raw.ImGuiWindowFlags_AlwaysHorizontalScrollbar     #-}
{-# COMPILE GHC ImGuiWindowFlags-AlwaysVerticalScrollbar       = DearImGui.Raw.ImGuiWindowFlags_AlwaysVerticalScrollbar       #-}
{-# COMPILE GHC ImGuiWindowFlags-NoBringToFrontOnFocus         = DearImGui.Raw.ImGuiWindowFlags_NoBringToFrontOnFocus         #-}
{-# COMPILE GHC ImGuiWindowFlags-NoFocusOnAppearing            = DearImGui.Raw.ImGuiWindowFlags_NoFocusOnAppearing            #-}
{-# COMPILE GHC ImGuiWindowFlags-HorizontalScrollbar           = DearImGui.Raw.ImGuiWindowFlags_HorizontalScrollbar           #-}
{-# COMPILE GHC ImGuiWindowFlags-MenuBar                       = DearImGui.Raw.ImGuiWindowFlags_MenuBar                       #-}
{-# COMPILE GHC ImGuiWindowFlags-NoMouseInputs                 = DearImGui.Raw.ImGuiWindowFlags_NoMouseInputs                 #-}
{-# COMPILE GHC ImGuiWindowFlags-NoSavedSettings               = DearImGui.Raw.ImGuiWindowFlags_NoSavedSettings               #-}
{-# COMPILE GHC ImGuiWindowFlags-NoBackground                  = DearImGui.Raw.ImGuiWindowFlags_NoBackground                  #-}
{-# COMPILE GHC ImGuiWindowFlags-AlwaysAutoResize              = DearImGui.Raw.ImGuiWindowFlags_AlwaysAutoResize              #-}
{-# COMPILE GHC ImGuiWindowFlags-NoCollapse                    = DearImGui.Raw.ImGuiWindowFlags_NoCollapse                    #-}
{-# COMPILE GHC ImGuiWindowFlags-NoScrollWithMouse             = DearImGui.Raw.ImGuiWindowFlags_NoScrollWithMouse             #-}
{-# COMPILE GHC ImGuiWindowFlags-NoScrollbar                   = DearImGui.Raw.ImGuiWindowFlags_NoScrollbar                   #-}
{-# COMPILE GHC ImGuiWindowFlags-NoMove                        = DearImGui.Raw.ImGuiWindowFlags_NoMove                        #-}
{-# COMPILE GHC ImGuiWindowFlags-NoResize                      = DearImGui.Raw.ImGuiWindowFlags_NoResize                      #-}
{-# COMPILE GHC ImGuiWindowFlags-NoTitleBar                    = DearImGui.Raw.ImGuiWindowFlags_NoTitleBar                    #-}
{-# COMPILE GHC ImGuiWindowFlags-None                          = DearImGui.Raw.ImGuiWindowFlags_None                          #-}


record ImVec2 : Set where
    constructor mkImVec2
    field
        x y : Float

{-# COMPILE GHC ImVec2 = data DearImGui.Raw.ImVec2 (DearImGui.Raw.ImVec2) #-}

postulate
    Storable[ImVec2] : Storable ImVec2
    Show[ImVec2]     : Show ImVec2

{-# COMPILE GHC Storable[ImVec2] = AgdaStorable #-}
{-# COMPILE GHC Show[ImVec2]     = AgdaShow     #-}


record ImVec3 : Set where
    constructor mkImVec3
    field
        x y z : Float

{-# COMPILE GHC ImVec3 = data DearImGui.Raw.ImVec3 (DearImGui.Raw.ImVec3) #-}

postulate
    Storable[ImVec3] : Storable ImVec3
    Show[ImVec3]     : Show ImVec3

{-# COMPILE GHC Storable[ImVec3] = AgdaStorable #-}
{-# COMPILE GHC Show[ImVec3]     = AgdaShow     #-}


record ImVec4 : Set where
    constructor mkImVec4
    field
        x y z w : Float

{-# COMPILE GHC ImVec4 = data DearImGui.Raw.ImVec4 (DearImGui.Raw.ImVec4) #-}

postulate
    Storable[ImVec4] : Storable ImVec4
    Show[ImVec4]     : Show ImVec4

{-# COMPILE GHC Storable[ImVec4] = AgdaStorable #-}
{-# COMPILE GHC Show[ImVec4]     = AgdaShow     #-}


postulate
    ImGuiContext             : Set
    ImFont                   : Set
    ImFontConfig             : Set
    ImFontGlyphRangesBuilder : Set
    ImDrawList               : Set
    ImGuiListClipper         : Set

{-# COMPILE GHC ImGuiContext             = type DearImGui.Raw.ImGuiContext             #-}
{-# COMPILE GHC ImFont                   = type DearImGui.Raw.ImFont                   #-}
{-# COMPILE GHC ImFontConfig             = type DearImGui.Raw.ImFontConfig             #-}
{-# COMPILE GHC ImFontGlyphRangesBuilder = type DearImGui.Raw.ImFontGlyphRangesBuilder #-}
{-# COMPILE GHC ImDrawList               = type DearImGui.Raw.ImDrawList               #-}
{-# COMPILE GHC ImGuiListClipper         = type DearImGui.Raw.ImGuiListClipper         #-}


ImU32 : Set
ImU32 = Word32

ImGuiID : Set
ImGuiID = ImU32

ImS16 : Set
ImS16 = Int16

ImWchar : Set
ImWchar = Word32
