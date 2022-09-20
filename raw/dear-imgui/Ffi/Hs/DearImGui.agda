{-# OPTIONS --without-K #-}

module Ffi.Hs.DearImGui where

open import Agda.Builtin.Bool              using (Bool)
open import Agda.Builtin.Int               using () renaming (Int to Integer)
open import Agda.Builtin.List              using (List)
open import Agda.Builtin.Maybe             using (Maybe)
open import Agda.Builtin.String            using () renaming (String to Text)
open import Agda.Primitive
open import Ffi.Hs.-base.Class
open import Ffi.Hs.-base.Float             using (Float)
open import Ffi.Hs.-base.Level             using (Liftℓ)
open import Ffi.Hs.-base.Unit              using (⊤; ⊤′)
open import Ffi.Hs.Control.Monad.IO.Unlift using (MonadUnliftIO)
open import Ffi.Hs.Data.Int                using (Int; Int16)
open import Ffi.Hs.Data.StateVar           using (HasGetter; HasSetter)
open import Ffi.Hs.Data.Tuple              using (Tuple2; Tuple3; Tuple4)
open import Ffi.Hs.Data.Vector.Storable    using () renaming (Vector to SVector)
open import Ffi.Hs.Data.Word               using (Word32)
open import Ffi.Hs.Foreign.C.String        using (CString)
open import Ffi.Hs.Foreign.C.Types         using (CInt; CFloat; CBool; CChar; CUChar)
open import Ffi.Hs.Foreign.Ptr             using (Ptr)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified DearImGui
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
import MAlonzo.Code.Ffi.Hs.Control.Monad.IO.Unlift (AgdaMonadUnliftIO(AgdaMonadUnliftIO))
import MAlonzo.Code.Ffi.Hs.Data.StateVar (AgdaHasGetter(AgdaHasGetter), AgdaHasSetter(AgdaHasSetter))
#-}

private
    variable
        aℓ bℓ : Level
        A B R Range Value : Set aℓ
        M : Set aℓ → Set aℓ

-- todo: Move most to DearImGui.Raw
-- todo: FiniteEnum (req KnownNat)

data ImGuiWindowFlags : Set where
    mkImGuiWindowFlags : CInt → ImGuiWindowFlags

{-# COMPILE GHC ImGuiWindowFlags = data DearImGui.ImGuiWindowFlags (DearImGui.ImGuiWindowFlags) #-}

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

{-# COMPILE GHC ImGuiInputTextFlags = data DearImGui.ImGuiInputTextFlags (DearImGui.ImGuiInputTextFlags) #-}

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

{-# COMPILE GHC ImGuiTreeNodeFlags = data DearImGui.ImGuiTreeNodeFlags (DearImGui.ImGuiTreeNodeFlags) #-}

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

{-# COMPILE GHC ImGuiPopupFlags = data DearImGui.ImGuiPopupFlags (DearImGui.ImGuiPopupFlags) #-}

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

{-# COMPILE GHC ImGuiSelectableFlags = data DearImGui.ImGuiSelectableFlags (DearImGui.ImGuiSelectableFlags) #-}

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

{-# COMPILE GHC ImGuiComboFlags = data DearImGui.ImGuiComboFlags (DearImGui.ImGuiComboFlags) #-}

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

{-# COMPILE GHC ImGuiTabBarFlags = data DearImGui.ImGuiTabBarFlags (DearImGui.ImGuiTabBarFlags) #-}

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

{-# COMPILE GHC ImGuiTabItemFlags = data DearImGui.ImGuiTabItemFlags (DearImGui.ImGuiTabItemFlags) #-}

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

{-# COMPILE GHC ImGuiTableFlags = data DearImGui.ImGuiTableFlags (DearImGui.ImGuiTableFlags) #-}

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

{-# COMPILE GHC ImGuiTableColumnFlags = data DearImGui.ImGuiTableColumnFlags (DearImGui.ImGuiTableColumnFlags) #-}

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

{-# COMPILE GHC ImGuiTableRowFlags = data DearImGui.ImGuiTableRowFlags (DearImGui.ImGuiTableRowFlags) #-}

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

{-# COMPILE GHC ImGuiTableBgTarget = data DearImGui.ImGuiTableBgTarget (DearImGui.ImGuiTableBgTarget) #-}

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

{-# COMPILE GHC ImGuiFocusedFlags = data DearImGui.ImGuiFocusedFlags (DearImGui.ImGuiFocusedFlags) #-}

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

{-# COMPILE GHC ImGuiHoveredFlags = data DearImGui.ImGuiHoveredFlags (DearImGui.ImGuiHoveredFlags) #-}

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

{-# COMPILE GHC ImGuiDragDropFlags = data DearImGui.ImGuiDragDropFlags (DearImGui.ImGuiDragDropFlags) #-}

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

{-# COMPILE GHC ImGuiDataType = data DearImGui.ImGuiDataType (DearImGui.ImGuiDataType) #-}

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

{-# COMPILE GHC ImGuiDir = data DearImGui.ImGuiDir (DearImGui.ImGuiDir) #-}

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

{-# COMPILE GHC ImGuiSortDirection = data DearImGui.ImGuiSortDirection (DearImGui.ImGuiSortDirection) #-}

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

{-# COMPILE GHC ImGuiKey = data DearImGui.ImGuiKey (DearImGui.ImGuiKey) #-}

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

{-# COMPILE GHC ImGuiModFlags = data DearImGui.ImGuiModFlags (DearImGui.ImGuiModFlags) #-}

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

{-# COMPILE GHC ImGuiNavInput = data DearImGui.ImGuiNavInput (DearImGui.ImGuiNavInput) #-}

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

{-# COMPILE GHC ImGuiConfigFlags = data DearImGui.ImGuiConfigFlags (DearImGui.ImGuiConfigFlags) #-}

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

{-# COMPILE GHC ImGuiBackendFlags = data DearImGui.ImGuiBackendFlags (DearImGui.ImGuiBackendFlags) #-}

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

{-# COMPILE GHC ImGuiCol = data DearImGui.ImGuiCol (DearImGui.ImGuiCol) #-}

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

{-# COMPILE GHC ImGuiStyleVar = data DearImGui.ImGuiStyleVar (DearImGui.ImGuiStyleVar) #-}

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

{-# COMPILE GHC ImGuiButtonFlags = data DearImGui.ImGuiButtonFlags (DearImGui.ImGuiButtonFlags) #-}

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

{-# COMPILE GHC ImGuiColorEditFlags = data DearImGui.ImGuiColorEditFlags (DearImGui.ImGuiColorEditFlags) #-}

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

{-# COMPILE GHC ImGuiSliderFlags = data DearImGui.ImGuiSliderFlags (DearImGui.ImGuiSliderFlags) #-}

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

{-# COMPILE GHC ImGuiMouseButton = data DearImGui.ImGuiMouseButton (DearImGui.ImGuiMouseButton) #-}

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

{-# COMPILE GHC ImGuiMouseCursor = data DearImGui.ImGuiMouseCursor (DearImGui.ImGuiMouseCursor) #-}

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

{-# COMPILE GHC ImGuiCond = data DearImGui.ImGuiCond (DearImGui.ImGuiCond) #-}

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

{-# COMPILE GHC ImDrawFlags = data DearImGui.ImDrawFlags (DearImGui.ImDrawFlags) #-}

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

{-# COMPILE GHC ImDrawListFlags = data DearImGui.ImDrawListFlags (DearImGui.ImDrawListFlags) #-}

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

{-# COMPILE GHC ImFontAtlasFlags = data DearImGui.ImFontAtlasFlags (DearImGui.ImFontAtlasFlags) #-}

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

{-# COMPILE GHC ImFontAtlasFlags-NoBakedLines                  = DearImGui.ImFontAtlasFlags_NoBakedLines                  #-}
{-# COMPILE GHC ImFontAtlasFlags-NoMouseCursors                = DearImGui.ImFontAtlasFlags_NoMouseCursors                #-}
{-# COMPILE GHC ImFontAtlasFlags-NoPowerOfTwoHeight            = DearImGui.ImFontAtlasFlags_NoPowerOfTwoHeight            #-}
{-# COMPILE GHC ImFontAtlasFlags-None                          = DearImGui.ImFontAtlasFlags_None                          #-}
{-# COMPILE GHC ImDrawListFlags-AllowVtxOffset                 = DearImGui.ImDrawListFlags_AllowVtxOffset                 #-}
{-# COMPILE GHC ImDrawListFlags-AntiAliasedFill                = DearImGui.ImDrawListFlags_AntiAliasedFill                #-}
{-# COMPILE GHC ImDrawListFlags-AntiAliasedLinesUseTex         = DearImGui.ImDrawListFlags_AntiAliasedLinesUseTex         #-}
{-# COMPILE GHC ImDrawListFlags-AntiAliasedLines               = DearImGui.ImDrawListFlags_AntiAliasedLines               #-}
{-# COMPILE GHC ImDrawListFlags-None                           = DearImGui.ImDrawListFlags_None                           #-}
{-# COMPILE GHC ImDrawFlags-RoundCornersMask-                  = DearImGui.ImDrawFlags_RoundCornersMask_                  #-}
{-# COMPILE GHC ImDrawFlags-RoundCornersDefault-               = DearImGui.ImDrawFlags_RoundCornersDefault_               #-}
{-# COMPILE GHC ImDrawFlags-RoundCornersAll                    = DearImGui.ImDrawFlags_RoundCornersAll                    #-}
{-# COMPILE GHC ImDrawFlags-RoundCornersRight                  = DearImGui.ImDrawFlags_RoundCornersRight                  #-}
{-# COMPILE GHC ImDrawFlags-RoundCornersLeft                   = DearImGui.ImDrawFlags_RoundCornersLeft                   #-}
{-# COMPILE GHC ImDrawFlags-RoundCornersBottom                 = DearImGui.ImDrawFlags_RoundCornersBottom                 #-}
{-# COMPILE GHC ImDrawFlags-RoundCornersTop                    = DearImGui.ImDrawFlags_RoundCornersTop                    #-}
{-# COMPILE GHC ImDrawFlags-RoundCornersNone                   = DearImGui.ImDrawFlags_RoundCornersNone                   #-}
{-# COMPILE GHC ImDrawFlags-RoundCornersBottomRight            = DearImGui.ImDrawFlags_RoundCornersBottomRight            #-}
{-# COMPILE GHC ImDrawFlags-RoundCornersBottomLeft             = DearImGui.ImDrawFlags_RoundCornersBottomLeft             #-}
{-# COMPILE GHC ImDrawFlags-RoundCornersTopRight               = DearImGui.ImDrawFlags_RoundCornersTopRight               #-}
{-# COMPILE GHC ImDrawFlags-RoundCornersTopLeft                = DearImGui.ImDrawFlags_RoundCornersTopLeft                #-}
{-# COMPILE GHC ImDrawFlags-Closed                             = DearImGui.ImDrawFlags_Closed                             #-}
{-# COMPILE GHC ImDrawFlags-None                               = DearImGui.ImDrawFlags_None                               #-}
{-# COMPILE GHC ImGuiCond-Appearing                            = DearImGui.ImGuiCond_Appearing                            #-}
{-# COMPILE GHC ImGuiCond-FirstUseEver                         = DearImGui.ImGuiCond_FirstUseEver                         #-}
{-# COMPILE GHC ImGuiCond-Once                                 = DearImGui.ImGuiCond_Once                                 #-}
{-# COMPILE GHC ImGuiCond-Always                               = DearImGui.ImGuiCond_Always                               #-}
{-# COMPILE GHC ImGuiCond-None                                 = DearImGui.ImGuiCond_None                                 #-}
{-# COMPILE GHC ImGuiMouseCursor-NotAllowed                    = DearImGui.ImGuiMouseCursor_NotAllowed                    #-}
{-# COMPILE GHC ImGuiMouseCursor-Hand                          = DearImGui.ImGuiMouseCursor_Hand                          #-}
{-# COMPILE GHC ImGuiMouseCursor-ResizeNWSE                    = DearImGui.ImGuiMouseCursor_ResizeNWSE                    #-}
{-# COMPILE GHC ImGuiMouseCursor-ResizeNESW                    = DearImGui.ImGuiMouseCursor_ResizeNESW                    #-}
{-# COMPILE GHC ImGuiMouseCursor-ResizeEW                      = DearImGui.ImGuiMouseCursor_ResizeEW                      #-}
{-# COMPILE GHC ImGuiMouseCursor-ResizeNS                      = DearImGui.ImGuiMouseCursor_ResizeNS                      #-}
{-# COMPILE GHC ImGuiMouseCursor-ResizeAll                     = DearImGui.ImGuiMouseCursor_ResizeAll                     #-}
{-# COMPILE GHC ImGuiMouseCursor-TextInput                     = DearImGui.ImGuiMouseCursor_TextInput                     #-}
{-# COMPILE GHC ImGuiMouseCursor-Arrow                         = DearImGui.ImGuiMouseCursor_Arrow                         #-}
{-# COMPILE GHC ImGuiMouseCursor-None                          = DearImGui.ImGuiMouseCursor_None                          #-}
{-# COMPILE GHC ImGuiMouseButton-Middle                        = DearImGui.ImGuiMouseButton_Middle                        #-}
{-# COMPILE GHC ImGuiMouseButton-Right                         = DearImGui.ImGuiMouseButton_Right                         #-}
{-# COMPILE GHC ImGuiMouseButton-Left                          = DearImGui.ImGuiMouseButton_Left                          #-}
{-# COMPILE GHC ImGuiSliderFlags-InvalidMask-                  = DearImGui.ImGuiSliderFlags_InvalidMask_                  #-}
{-# COMPILE GHC ImGuiSliderFlags-NoInput                       = DearImGui.ImGuiSliderFlags_NoInput                       #-}
{-# COMPILE GHC ImGuiSliderFlags-NoRoundToFormat               = DearImGui.ImGuiSliderFlags_NoRoundToFormat               #-}
{-# COMPILE GHC ImGuiSliderFlags-Logarithmic                   = DearImGui.ImGuiSliderFlags_Logarithmic                   #-}
{-# COMPILE GHC ImGuiSliderFlags-AlwaysClamp                   = DearImGui.ImGuiSliderFlags_AlwaysClamp                   #-}
{-# COMPILE GHC ImGuiSliderFlags-None                          = DearImGui.ImGuiSliderFlags_None                          #-}
{-# COMPILE GHC ImGuiColorEditFlags-InputMask-                 = DearImGui.ImGuiColorEditFlags_InputMask_                 #-}
{-# COMPILE GHC ImGuiColorEditFlags-PickerMask-                = DearImGui.ImGuiColorEditFlags_PickerMask_                #-}
{-# COMPILE GHC ImGuiColorEditFlags-DataTypeMask-              = DearImGui.ImGuiColorEditFlags_DataTypeMask_              #-}
{-# COMPILE GHC ImGuiColorEditFlags-DisplayMask-               = DearImGui.ImGuiColorEditFlags_DisplayMask_               #-}
{-# COMPILE GHC ImGuiColorEditFlags-DefaultOptions-            = DearImGui.ImGuiColorEditFlags_DefaultOptions_            #-}
{-# COMPILE GHC ImGuiColorEditFlags-InputHSV                   = DearImGui.ImGuiColorEditFlags_InputHSV                   #-}
{-# COMPILE GHC ImGuiColorEditFlags-InputRGB                   = DearImGui.ImGuiColorEditFlags_InputRGB                   #-}
{-# COMPILE GHC ImGuiColorEditFlags-PickerHueWheel             = DearImGui.ImGuiColorEditFlags_PickerHueWheel             #-}
{-# COMPILE GHC ImGuiColorEditFlags-PickerHueBar               = DearImGui.ImGuiColorEditFlags_PickerHueBar               #-}
{-# COMPILE GHC ImGuiColorEditFlags-Float                      = DearImGui.ImGuiColorEditFlags_Float                      #-}
{-# COMPILE GHC ImGuiColorEditFlags-Uint8                      = DearImGui.ImGuiColorEditFlags_Uint8                      #-}
{-# COMPILE GHC ImGuiColorEditFlags-DisplayHex                 = DearImGui.ImGuiColorEditFlags_DisplayHex                 #-}
{-# COMPILE GHC ImGuiColorEditFlags-DisplayHSV                 = DearImGui.ImGuiColorEditFlags_DisplayHSV                 #-}
{-# COMPILE GHC ImGuiColorEditFlags-DisplayRGB                 = DearImGui.ImGuiColorEditFlags_DisplayRGB                 #-}
{-# COMPILE GHC ImGuiColorEditFlags-HDR                        = DearImGui.ImGuiColorEditFlags_HDR                        #-}
{-# COMPILE GHC ImGuiColorEditFlags-AlphaPreviewHalf           = DearImGui.ImGuiColorEditFlags_AlphaPreviewHalf           #-}
{-# COMPILE GHC ImGuiColorEditFlags-AlphaPreview               = DearImGui.ImGuiColorEditFlags_AlphaPreview               #-}
{-# COMPILE GHC ImGuiColorEditFlags-AlphaBar                   = DearImGui.ImGuiColorEditFlags_AlphaBar                   #-}
{-# COMPILE GHC ImGuiColorEditFlags-NoBorder                   = DearImGui.ImGuiColorEditFlags_NoBorder                   #-}
{-# COMPILE GHC ImGuiColorEditFlags-NoDragDrop                 = DearImGui.ImGuiColorEditFlags_NoDragDrop                 #-}
{-# COMPILE GHC ImGuiColorEditFlags-NoSidePreview              = DearImGui.ImGuiColorEditFlags_NoSidePreview              #-}
{-# COMPILE GHC ImGuiColorEditFlags-NoLabel                    = DearImGui.ImGuiColorEditFlags_NoLabel                    #-}
{-# COMPILE GHC ImGuiColorEditFlags-NoTooltip                  = DearImGui.ImGuiColorEditFlags_NoTooltip                  #-}
{-# COMPILE GHC ImGuiColorEditFlags-NoInputs                   = DearImGui.ImGuiColorEditFlags_NoInputs                   #-}
{-# COMPILE GHC ImGuiColorEditFlags-NoSmallPreview             = DearImGui.ImGuiColorEditFlags_NoSmallPreview             #-}
{-# COMPILE GHC ImGuiColorEditFlags-NoOptions                  = DearImGui.ImGuiColorEditFlags_NoOptions                  #-}
{-# COMPILE GHC ImGuiColorEditFlags-NoPicker                   = DearImGui.ImGuiColorEditFlags_NoPicker                   #-}
{-# COMPILE GHC ImGuiColorEditFlags-NoAlpha                    = DearImGui.ImGuiColorEditFlags_NoAlpha                    #-}
{-# COMPILE GHC ImGuiColorEditFlags-None                       = DearImGui.ImGuiColorEditFlags_None                       #-}
{-# COMPILE GHC ImGuiButtonFlags-MouseButtonDefault-           = DearImGui.ImGuiButtonFlags_MouseButtonDefault_           #-}
{-# COMPILE GHC ImGuiButtonFlags-MouseButtonMask-              = DearImGui.ImGuiButtonFlags_MouseButtonMask_              #-}
{-# COMPILE GHC ImGuiButtonFlags-MouseButtonMiddle             = DearImGui.ImGuiButtonFlags_MouseButtonMiddle             #-}
{-# COMPILE GHC ImGuiButtonFlags-MouseButtonRight              = DearImGui.ImGuiButtonFlags_MouseButtonRight              #-}
{-# COMPILE GHC ImGuiButtonFlags-MouseButtonLeft               = DearImGui.ImGuiButtonFlags_MouseButtonLeft               #-}
{-# COMPILE GHC ImGuiButtonFlags-None                          = DearImGui.ImGuiButtonFlags_None                          #-}
{-# COMPILE GHC ImGuiStyleVar-SelectableTextAlign              = DearImGui.ImGuiStyleVar_SelectableTextAlign              #-}
{-# COMPILE GHC ImGuiStyleVar-ButtonTextAlign                  = DearImGui.ImGuiStyleVar_ButtonTextAlign                  #-}
{-# COMPILE GHC ImGuiStyleVar-TabRounding                      = DearImGui.ImGuiStyleVar_TabRounding                      #-}
{-# COMPILE GHC ImGuiStyleVar-GrabRounding                     = DearImGui.ImGuiStyleVar_GrabRounding                     #-}
{-# COMPILE GHC ImGuiStyleVar-GrabMinSize                      = DearImGui.ImGuiStyleVar_GrabMinSize                      #-}
{-# COMPILE GHC ImGuiStyleVar-ScrollbarRounding                = DearImGui.ImGuiStyleVar_ScrollbarRounding                #-}
{-# COMPILE GHC ImGuiStyleVar-ScrollbarSize                    = DearImGui.ImGuiStyleVar_ScrollbarSize                    #-}
{-# COMPILE GHC ImGuiStyleVar-CellPadding                      = DearImGui.ImGuiStyleVar_CellPadding                      #-}
{-# COMPILE GHC ImGuiStyleVar-IndentSpacing                    = DearImGui.ImGuiStyleVar_IndentSpacing                    #-}
{-# COMPILE GHC ImGuiStyleVar-ItemInnerSpacing                 = DearImGui.ImGuiStyleVar_ItemInnerSpacing                 #-}
{-# COMPILE GHC ImGuiStyleVar-ItemSpacing                      = DearImGui.ImGuiStyleVar_ItemSpacing                      #-}
{-# COMPILE GHC ImGuiStyleVar-FrameBorderSize                  = DearImGui.ImGuiStyleVar_FrameBorderSize                  #-}
{-# COMPILE GHC ImGuiStyleVar-FrameRounding                    = DearImGui.ImGuiStyleVar_FrameRounding                    #-}
{-# COMPILE GHC ImGuiStyleVar-FramePadding                     = DearImGui.ImGuiStyleVar_FramePadding                     #-}
{-# COMPILE GHC ImGuiStyleVar-PopupBorderSize                  = DearImGui.ImGuiStyleVar_PopupBorderSize                  #-}
{-# COMPILE GHC ImGuiStyleVar-PopupRounding                    = DearImGui.ImGuiStyleVar_PopupRounding                    #-}
{-# COMPILE GHC ImGuiStyleVar-ChildBorderSize                  = DearImGui.ImGuiStyleVar_ChildBorderSize                  #-}
{-# COMPILE GHC ImGuiStyleVar-ChildRounding                    = DearImGui.ImGuiStyleVar_ChildRounding                    #-}
{-# COMPILE GHC ImGuiStyleVar-WindowTitleAlign                 = DearImGui.ImGuiStyleVar_WindowTitleAlign                 #-}
{-# COMPILE GHC ImGuiStyleVar-WindowMinSize                    = DearImGui.ImGuiStyleVar_WindowMinSize                    #-}
{-# COMPILE GHC ImGuiStyleVar-WindowBorderSize                 = DearImGui.ImGuiStyleVar_WindowBorderSize                 #-}
{-# COMPILE GHC ImGuiStyleVar-WindowRounding                   = DearImGui.ImGuiStyleVar_WindowRounding                   #-}
{-# COMPILE GHC ImGuiStyleVar-WindowPadding                    = DearImGui.ImGuiStyleVar_WindowPadding                    #-}
{-# COMPILE GHC ImGuiStyleVar-DisabledAlpha                    = DearImGui.ImGuiStyleVar_DisabledAlpha                    #-}
{-# COMPILE GHC ImGuiStyleVar-Alpha                            = DearImGui.ImGuiStyleVar_Alpha                            #-}
{-# COMPILE GHC ImGuiCol-ModalWindowDimBg                      = DearImGui.ImGuiCol_ModalWindowDimBg                      #-}
{-# COMPILE GHC ImGuiCol-NavWindowingDimBg                     = DearImGui.ImGuiCol_NavWindowingDimBg                     #-}
{-# COMPILE GHC ImGuiCol-NavWindowingHighlight                 = DearImGui.ImGuiCol_NavWindowingHighlight                 #-}
{-# COMPILE GHC ImGuiCol-NavHighlight                          = DearImGui.ImGuiCol_NavHighlight                          #-}
{-# COMPILE GHC ImGuiCol-DragDropTarget                        = DearImGui.ImGuiCol_DragDropTarget                        #-}
{-# COMPILE GHC ImGuiCol-TextSelectedBg                        = DearImGui.ImGuiCol_TextSelectedBg                        #-}
{-# COMPILE GHC ImGuiCol-TableRowBgAlt                         = DearImGui.ImGuiCol_TableRowBgAlt                         #-}
{-# COMPILE GHC ImGuiCol-TableRowBg                            = DearImGui.ImGuiCol_TableRowBg                            #-}
{-# COMPILE GHC ImGuiCol-TableBorderLight                      = DearImGui.ImGuiCol_TableBorderLight                      #-}
{-# COMPILE GHC ImGuiCol-TableBorderStrong                     = DearImGui.ImGuiCol_TableBorderStrong                     #-}
{-# COMPILE GHC ImGuiCol-TableHeaderBg                         = DearImGui.ImGuiCol_TableHeaderBg                         #-}
{-# COMPILE GHC ImGuiCol-PlotHistogramHovered                  = DearImGui.ImGuiCol_PlotHistogramHovered                  #-}
{-# COMPILE GHC ImGuiCol-PlotHistogram                         = DearImGui.ImGuiCol_PlotHistogram                         #-}
{-# COMPILE GHC ImGuiCol-PlotLinesHovered                      = DearImGui.ImGuiCol_PlotLinesHovered                      #-}
{-# COMPILE GHC ImGuiCol-PlotLines                             = DearImGui.ImGuiCol_PlotLines                             #-}
{-# COMPILE GHC ImGuiCol-TabUnfocusedActive                    = DearImGui.ImGuiCol_TabUnfocusedActive                    #-}
{-# COMPILE GHC ImGuiCol-TabUnfocused                          = DearImGui.ImGuiCol_TabUnfocused                          #-}
{-# COMPILE GHC ImGuiCol-TabActive                             = DearImGui.ImGuiCol_TabActive                             #-}
{-# COMPILE GHC ImGuiCol-TabHovered                            = DearImGui.ImGuiCol_TabHovered                            #-}
{-# COMPILE GHC ImGuiCol-Tab                                   = DearImGui.ImGuiCol_Tab                                   #-}
{-# COMPILE GHC ImGuiCol-ResizeGripActive                      = DearImGui.ImGuiCol_ResizeGripActive                      #-}
{-# COMPILE GHC ImGuiCol-ResizeGripHovered                     = DearImGui.ImGuiCol_ResizeGripHovered                     #-}
{-# COMPILE GHC ImGuiCol-ResizeGrip                            = DearImGui.ImGuiCol_ResizeGrip                            #-}
{-# COMPILE GHC ImGuiCol-SeparatorActive                       = DearImGui.ImGuiCol_SeparatorActive                       #-}
{-# COMPILE GHC ImGuiCol-SeparatorHovered                      = DearImGui.ImGuiCol_SeparatorHovered                      #-}
{-# COMPILE GHC ImGuiCol-Separator                             = DearImGui.ImGuiCol_Separator                             #-}
{-# COMPILE GHC ImGuiCol-HeaderActive                          = DearImGui.ImGuiCol_HeaderActive                          #-}
{-# COMPILE GHC ImGuiCol-HeaderHovered                         = DearImGui.ImGuiCol_HeaderHovered                         #-}
{-# COMPILE GHC ImGuiCol-Header                                = DearImGui.ImGuiCol_Header                                #-}
{-# COMPILE GHC ImGuiCol-ButtonActive                          = DearImGui.ImGuiCol_ButtonActive                          #-}
{-# COMPILE GHC ImGuiCol-ButtonHovered                         = DearImGui.ImGuiCol_ButtonHovered                         #-}
{-# COMPILE GHC ImGuiCol-Button                                = DearImGui.ImGuiCol_Button                                #-}
{-# COMPILE GHC ImGuiCol-SliderGrabActive                      = DearImGui.ImGuiCol_SliderGrabActive                      #-}
{-# COMPILE GHC ImGuiCol-SliderGrab                            = DearImGui.ImGuiCol_SliderGrab                            #-}
{-# COMPILE GHC ImGuiCol-CheckMark                             = DearImGui.ImGuiCol_CheckMark                             #-}
{-# COMPILE GHC ImGuiCol-ScrollbarGrabActive                   = DearImGui.ImGuiCol_ScrollbarGrabActive                   #-}
{-# COMPILE GHC ImGuiCol-ScrollbarGrabHovered                  = DearImGui.ImGuiCol_ScrollbarGrabHovered                  #-}
{-# COMPILE GHC ImGuiCol-ScrollbarGrab                         = DearImGui.ImGuiCol_ScrollbarGrab                         #-}
{-# COMPILE GHC ImGuiCol-ScrollbarBg                           = DearImGui.ImGuiCol_ScrollbarBg                           #-}
{-# COMPILE GHC ImGuiCol-MenuBarBg                             = DearImGui.ImGuiCol_MenuBarBg                             #-}
{-# COMPILE GHC ImGuiCol-TitleBgCollapsed                      = DearImGui.ImGuiCol_TitleBgCollapsed                      #-}
{-# COMPILE GHC ImGuiCol-TitleBgActive                         = DearImGui.ImGuiCol_TitleBgActive                         #-}
{-# COMPILE GHC ImGuiCol-TitleBg                               = DearImGui.ImGuiCol_TitleBg                               #-}
{-# COMPILE GHC ImGuiCol-FrameBgActive                         = DearImGui.ImGuiCol_FrameBgActive                         #-}
{-# COMPILE GHC ImGuiCol-FrameBgHovered                        = DearImGui.ImGuiCol_FrameBgHovered                        #-}
{-# COMPILE GHC ImGuiCol-FrameBg                               = DearImGui.ImGuiCol_FrameBg                               #-}
{-# COMPILE GHC ImGuiCol-BorderShadow                          = DearImGui.ImGuiCol_BorderShadow                          #-}
{-# COMPILE GHC ImGuiCol-Border                                = DearImGui.ImGuiCol_Border                                #-}
{-# COMPILE GHC ImGuiCol-PopupBg                               = DearImGui.ImGuiCol_PopupBg                               #-}
{-# COMPILE GHC ImGuiCol-ChildBg                               = DearImGui.ImGuiCol_ChildBg                               #-}
{-# COMPILE GHC ImGuiCol-WindowBg                              = DearImGui.ImGuiCol_WindowBg                              #-}
{-# COMPILE GHC ImGuiCol-TextDisabled                          = DearImGui.ImGuiCol_TextDisabled                          #-}
{-# COMPILE GHC ImGuiCol-Text                                  = DearImGui.ImGuiCol_Text                                  #-}
{-# COMPILE GHC ImGuiBackendFlags-RendererHasVtxOffset         = DearImGui.ImGuiBackendFlags_RendererHasVtxOffset         #-}
{-# COMPILE GHC ImGuiBackendFlags-HasSetMousePos               = DearImGui.ImGuiBackendFlags_HasSetMousePos               #-}
{-# COMPILE GHC ImGuiBackendFlags-HasMouseCursors              = DearImGui.ImGuiBackendFlags_HasMouseCursors              #-}
{-# COMPILE GHC ImGuiBackendFlags-HasGamepad                   = DearImGui.ImGuiBackendFlags_HasGamepad                   #-}
{-# COMPILE GHC ImGuiBackendFlags-None                         = DearImGui.ImGuiBackendFlags_None                         #-}
{-# COMPILE GHC ImGuiConfigFlags-IsTouchScreen                 = DearImGui.ImGuiConfigFlags_IsTouchScreen                 #-}
{-# COMPILE GHC ImGuiConfigFlags-IsSRGB                        = DearImGui.ImGuiConfigFlags_IsSRGB                        #-}
{-# COMPILE GHC ImGuiConfigFlags-NoMouseCursorChange           = DearImGui.ImGuiConfigFlags_NoMouseCursorChange           #-}
{-# COMPILE GHC ImGuiConfigFlags-NoMouse                       = DearImGui.ImGuiConfigFlags_NoMouse                       #-}
{-# COMPILE GHC ImGuiConfigFlags-NavNoCaptureKeyboard          = DearImGui.ImGuiConfigFlags_NavNoCaptureKeyboard          #-}
{-# COMPILE GHC ImGuiConfigFlags-NavEnableSetMousePos          = DearImGui.ImGuiConfigFlags_NavEnableSetMousePos          #-}
{-# COMPILE GHC ImGuiConfigFlags-NavEnableGamepad              = DearImGui.ImGuiConfigFlags_NavEnableGamepad              #-}
{-# COMPILE GHC ImGuiConfigFlags-NavEnableKeyboard             = DearImGui.ImGuiConfigFlags_NavEnableKeyboard             #-}
{-# COMPILE GHC ImGuiConfigFlags-None                          = DearImGui.ImGuiConfigFlags_None                          #-}
{-# COMPILE GHC ImGuiNavInput-KeyDown-                         = DearImGui.ImGuiNavInput_KeyDown_                         #-}
{-# COMPILE GHC ImGuiNavInput-KeyUp-                           = DearImGui.ImGuiNavInput_KeyUp_                           #-}
{-# COMPILE GHC ImGuiNavInput-KeyRight-                        = DearImGui.ImGuiNavInput_KeyRight_                        #-}
{-# COMPILE GHC ImGuiNavInput-KeyLeft-                         = DearImGui.ImGuiNavInput_KeyLeft_                         #-}
{-# COMPILE GHC ImGuiNavInput-TweakFast                        = DearImGui.ImGuiNavInput_TweakFast                        #-}
{-# COMPILE GHC ImGuiNavInput-TweakSlow                        = DearImGui.ImGuiNavInput_TweakSlow                        #-}
{-# COMPILE GHC ImGuiNavInput-FocusNext                        = DearImGui.ImGuiNavInput_FocusNext                        #-}
{-# COMPILE GHC ImGuiNavInput-FocusPrev                        = DearImGui.ImGuiNavInput_FocusPrev                        #-}
{-# COMPILE GHC ImGuiNavInput-LStickDown                       = DearImGui.ImGuiNavInput_LStickDown                       #-}
{-# COMPILE GHC ImGuiNavInput-LStickUp                         = DearImGui.ImGuiNavInput_LStickUp                         #-}
{-# COMPILE GHC ImGuiNavInput-LStickRight                      = DearImGui.ImGuiNavInput_LStickRight                      #-}
{-# COMPILE GHC ImGuiNavInput-LStickLeft                       = DearImGui.ImGuiNavInput_LStickLeft                       #-}
{-# COMPILE GHC ImGuiNavInput-DpadDown                         = DearImGui.ImGuiNavInput_DpadDown                         #-}
{-# COMPILE GHC ImGuiNavInput-DpadUp                           = DearImGui.ImGuiNavInput_DpadUp                           #-}
{-# COMPILE GHC ImGuiNavInput-DpadRight                        = DearImGui.ImGuiNavInput_DpadRight                        #-}
{-# COMPILE GHC ImGuiNavInput-DpadLeft                         = DearImGui.ImGuiNavInput_DpadLeft                         #-}
{-# COMPILE GHC ImGuiNavInput-Menu                             = DearImGui.ImGuiNavInput_Menu                             #-}
{-# COMPILE GHC ImGuiNavInput-Input                            = DearImGui.ImGuiNavInput_Input                            #-}
{-# COMPILE GHC ImGuiNavInput-Cancel                           = DearImGui.ImGuiNavInput_Cancel                           #-}
{-# COMPILE GHC ImGuiNavInput-Activate                         = DearImGui.ImGuiNavInput_Activate                         #-}
{-# COMPILE GHC ImGuiModFlags-Super                            = DearImGui.ImGuiModFlags_Super                            #-}
{-# COMPILE GHC ImGuiModFlags-Alt                              = DearImGui.ImGuiModFlags_Alt                              #-}
{-# COMPILE GHC ImGuiModFlags-Shift                            = DearImGui.ImGuiModFlags_Shift                            #-}
{-# COMPILE GHC ImGuiModFlags-Ctrl                             = DearImGui.ImGuiModFlags_Ctrl                             #-}
{-# COMPILE GHC ImGuiModFlags-None                             = DearImGui.ImGuiModFlags_None                             #-}
{-# COMPILE GHC ImGuiKey-NamedKey-COUNT                        = DearImGui.ImGuiKey_NamedKey_COUNT                        #-}
{-# COMPILE GHC ImGuiKey-NamedKey-END                          = DearImGui.ImGuiKey_NamedKey_END                          #-}
{-# COMPILE GHC ImGuiKey-NamedKey-BEGIN                        = DearImGui.ImGuiKey_NamedKey_BEGIN                        #-}
{-# COMPILE GHC ImGuiKey-ModSuper                              = DearImGui.ImGuiKey_ModSuper                              #-}
{-# COMPILE GHC ImGuiKey-ModAlt                                = DearImGui.ImGuiKey_ModAlt                                #-}
{-# COMPILE GHC ImGuiKey-ModShift                              = DearImGui.ImGuiKey_ModShift                              #-}
{-# COMPILE GHC ImGuiKey-ModCtrl                               = DearImGui.ImGuiKey_ModCtrl                               #-}
{-# COMPILE GHC ImGuiKey-GamepadRStickRight                    = DearImGui.ImGuiKey_GamepadRStickRight                    #-}
{-# COMPILE GHC ImGuiKey-GamepadRStickLeft                     = DearImGui.ImGuiKey_GamepadRStickLeft                     #-}
{-# COMPILE GHC ImGuiKey-GamepadRStickDown                     = DearImGui.ImGuiKey_GamepadRStickDown                     #-}
{-# COMPILE GHC ImGuiKey-GamepadRStickUp                       = DearImGui.ImGuiKey_GamepadRStickUp                       #-}
{-# COMPILE GHC ImGuiKey-GamepadLStickRight                    = DearImGui.ImGuiKey_GamepadLStickRight                    #-}
{-# COMPILE GHC ImGuiKey-GamepadLStickLeft                     = DearImGui.ImGuiKey_GamepadLStickLeft                     #-}
{-# COMPILE GHC ImGuiKey-GamepadLStickDown                     = DearImGui.ImGuiKey_GamepadLStickDown                     #-}
{-# COMPILE GHC ImGuiKey-GamepadLStickUp                       = DearImGui.ImGuiKey_GamepadLStickUp                       #-}
{-# COMPILE GHC ImGuiKey-GamepadR3                             = DearImGui.ImGuiKey_GamepadR3                             #-}
{-# COMPILE GHC ImGuiKey-GamepadL3                             = DearImGui.ImGuiKey_GamepadL3                             #-}
{-# COMPILE GHC ImGuiKey-GamepadR2                             = DearImGui.ImGuiKey_GamepadR2                             #-}
{-# COMPILE GHC ImGuiKey-GamepadL2                             = DearImGui.ImGuiKey_GamepadL2                             #-}
{-# COMPILE GHC ImGuiKey-GamepadR1                             = DearImGui.ImGuiKey_GamepadR1                             #-}
{-# COMPILE GHC ImGuiKey-GamepadL1                             = DearImGui.ImGuiKey_GamepadL1                             #-}
{-# COMPILE GHC ImGuiKey-GamepadDpadRight                      = DearImGui.ImGuiKey_GamepadDpadRight                      #-}
{-# COMPILE GHC ImGuiKey-GamepadDpadLeft                       = DearImGui.ImGuiKey_GamepadDpadLeft                       #-}
{-# COMPILE GHC ImGuiKey-GamepadDpadDown                       = DearImGui.ImGuiKey_GamepadDpadDown                       #-}
{-# COMPILE GHC ImGuiKey-GamepadDpadUp                         = DearImGui.ImGuiKey_GamepadDpadUp                         #-}
{-# COMPILE GHC ImGuiKey-GamepadFaceRight                      = DearImGui.ImGuiKey_GamepadFaceRight                      #-}
{-# COMPILE GHC ImGuiKey-GamepadFaceLeft                       = DearImGui.ImGuiKey_GamepadFaceLeft                       #-}
{-# COMPILE GHC ImGuiKey-GamepadFaceDown                       = DearImGui.ImGuiKey_GamepadFaceDown                       #-}
{-# COMPILE GHC ImGuiKey-GamepadFaceUp                         = DearImGui.ImGuiKey_GamepadFaceUp                         #-}
{-# COMPILE GHC ImGuiKey-GamepadBack                           = DearImGui.ImGuiKey_GamepadBack                           #-}
{-# COMPILE GHC ImGuiKey-GamepadStart                          = DearImGui.ImGuiKey_GamepadStart                          #-}
{-# COMPILE GHC ImGuiKey-KeypadEqual                           = DearImGui.ImGuiKey_KeypadEqual                           #-}
{-# COMPILE GHC ImGuiKey-KeypadEnter                           = DearImGui.ImGuiKey_KeypadEnter                           #-}
{-# COMPILE GHC ImGuiKey-KeypadAdd                             = DearImGui.ImGuiKey_KeypadAdd                             #-}
{-# COMPILE GHC ImGuiKey-KeypadSubtract                        = DearImGui.ImGuiKey_KeypadSubtract                        #-}
{-# COMPILE GHC ImGuiKey-KeypadMultiply                        = DearImGui.ImGuiKey_KeypadMultiply                        #-}
{-# COMPILE GHC ImGuiKey-KeypadDivide                          = DearImGui.ImGuiKey_KeypadDivide                          #-}
{-# COMPILE GHC ImGuiKey-KeypadDecimal                         = DearImGui.ImGuiKey_KeypadDecimal                         #-}
{-# COMPILE GHC ImGuiKey-Keypad9                               = DearImGui.ImGuiKey_Keypad9                               #-}
{-# COMPILE GHC ImGuiKey-Keypad8                               = DearImGui.ImGuiKey_Keypad8                               #-}
{-# COMPILE GHC ImGuiKey-Keypad7                               = DearImGui.ImGuiKey_Keypad7                               #-}
{-# COMPILE GHC ImGuiKey-Keypad6                               = DearImGui.ImGuiKey_Keypad6                               #-}
{-# COMPILE GHC ImGuiKey-Keypad5                               = DearImGui.ImGuiKey_Keypad5                               #-}
{-# COMPILE GHC ImGuiKey-Keypad4                               = DearImGui.ImGuiKey_Keypad4                               #-}
{-# COMPILE GHC ImGuiKey-Keypad3                               = DearImGui.ImGuiKey_Keypad3                               #-}
{-# COMPILE GHC ImGuiKey-Keypad2                               = DearImGui.ImGuiKey_Keypad2                               #-}
{-# COMPILE GHC ImGuiKey-Keypad1                               = DearImGui.ImGuiKey_Keypad1                               #-}
{-# COMPILE GHC ImGuiKey-Keypad0                               = DearImGui.ImGuiKey_Keypad0                               #-}
{-# COMPILE GHC ImGuiKey-Pause                                 = DearImGui.ImGuiKey_Pause                                 #-}
{-# COMPILE GHC ImGuiKey-PrintScreen                           = DearImGui.ImGuiKey_PrintScreen                           #-}
{-# COMPILE GHC ImGuiKey-NumLock                               = DearImGui.ImGuiKey_NumLock                               #-}
{-# COMPILE GHC ImGuiKey-ScrollLock                            = DearImGui.ImGuiKey_ScrollLock                            #-}
{-# COMPILE GHC ImGuiKey-CapsLock                              = DearImGui.ImGuiKey_CapsLock                              #-}
{-# COMPILE GHC ImGuiKey-GraveAccent                           = DearImGui.ImGuiKey_GraveAccent                           #-}
{-# COMPILE GHC ImGuiKey-RightBracket                          = DearImGui.ImGuiKey_RightBracket                          #-}
{-# COMPILE GHC ImGuiKey-Backslash                             = DearImGui.ImGuiKey_Backslash                             #-}
{-# COMPILE GHC ImGuiKey-LeftBracket                           = DearImGui.ImGuiKey_LeftBracket                           #-}
{-# COMPILE GHC ImGuiKey-Equal                                 = DearImGui.ImGuiKey_Equal                                 #-}
{-# COMPILE GHC ImGuiKey-Semicolon                             = DearImGui.ImGuiKey_Semicolon                             #-}
{-# COMPILE GHC ImGuiKey-Slash                                 = DearImGui.ImGuiKey_Slash                                 #-}
{-# COMPILE GHC ImGuiKey-Period                                = DearImGui.ImGuiKey_Period                                #-}
{-# COMPILE GHC ImGuiKey-Minus                                 = DearImGui.ImGuiKey_Minus                                 #-}
{-# COMPILE GHC ImGuiKey-Comma                                 = DearImGui.ImGuiKey_Comma                                 #-}
{-# COMPILE GHC ImGuiKey-Apostrophe                            = DearImGui.ImGuiKey_Apostrophe                            #-}
{-# COMPILE GHC ImGuiKey-F12                                   = DearImGui.ImGuiKey_F12                                   #-}
{-# COMPILE GHC ImGuiKey-F11                                   = DearImGui.ImGuiKey_F11                                   #-}
{-# COMPILE GHC ImGuiKey-F10                                   = DearImGui.ImGuiKey_F10                                   #-}
{-# COMPILE GHC ImGuiKey-F9                                    = DearImGui.ImGuiKey_F9                                    #-}
{-# COMPILE GHC ImGuiKey-F8                                    = DearImGui.ImGuiKey_F8                                    #-}
{-# COMPILE GHC ImGuiKey-F7                                    = DearImGui.ImGuiKey_F7                                    #-}
{-# COMPILE GHC ImGuiKey-F6                                    = DearImGui.ImGuiKey_F6                                    #-}
{-# COMPILE GHC ImGuiKey-F5                                    = DearImGui.ImGuiKey_F5                                    #-}
{-# COMPILE GHC ImGuiKey-F4                                    = DearImGui.ImGuiKey_F4                                    #-}
{-# COMPILE GHC ImGuiKey-F3                                    = DearImGui.ImGuiKey_F3                                    #-}
{-# COMPILE GHC ImGuiKey-F2                                    = DearImGui.ImGuiKey_F2                                    #-}
{-# COMPILE GHC ImGuiKey-F1                                    = DearImGui.ImGuiKey_F1                                    #-}
{-# COMPILE GHC ImGuiKey-Z                                     = DearImGui.ImGuiKey_Z                                     #-}
{-# COMPILE GHC ImGuiKey-Y                                     = DearImGui.ImGuiKey_Y                                     #-}
{-# COMPILE GHC ImGuiKey-X                                     = DearImGui.ImGuiKey_X                                     #-}
{-# COMPILE GHC ImGuiKey-W                                     = DearImGui.ImGuiKey_W                                     #-}
{-# COMPILE GHC ImGuiKey-V                                     = DearImGui.ImGuiKey_V                                     #-}
{-# COMPILE GHC ImGuiKey-U                                     = DearImGui.ImGuiKey_U                                     #-}
{-# COMPILE GHC ImGuiKey-T                                     = DearImGui.ImGuiKey_T                                     #-}
{-# COMPILE GHC ImGuiKey-S                                     = DearImGui.ImGuiKey_S                                     #-}
{-# COMPILE GHC ImGuiKey-R                                     = DearImGui.ImGuiKey_R                                     #-}
{-# COMPILE GHC ImGuiKey-Q                                     = DearImGui.ImGuiKey_Q                                     #-}
{-# COMPILE GHC ImGuiKey-P                                     = DearImGui.ImGuiKey_P                                     #-}
{-# COMPILE GHC ImGuiKey-O                                     = DearImGui.ImGuiKey_O                                     #-}
{-# COMPILE GHC ImGuiKey-N                                     = DearImGui.ImGuiKey_N                                     #-}
{-# COMPILE GHC ImGuiKey-M                                     = DearImGui.ImGuiKey_M                                     #-}
{-# COMPILE GHC ImGuiKey-L                                     = DearImGui.ImGuiKey_L                                     #-}
{-# COMPILE GHC ImGuiKey-K                                     = DearImGui.ImGuiKey_K                                     #-}
{-# COMPILE GHC ImGuiKey-J                                     = DearImGui.ImGuiKey_J                                     #-}
{-# COMPILE GHC ImGuiKey-I                                     = DearImGui.ImGuiKey_I                                     #-}
{-# COMPILE GHC ImGuiKey-H                                     = DearImGui.ImGuiKey_H                                     #-}
{-# COMPILE GHC ImGuiKey-G                                     = DearImGui.ImGuiKey_G                                     #-}
{-# COMPILE GHC ImGuiKey-F                                     = DearImGui.ImGuiKey_F                                     #-}
{-# COMPILE GHC ImGuiKey-E                                     = DearImGui.ImGuiKey_E                                     #-}
{-# COMPILE GHC ImGuiKey-D                                     = DearImGui.ImGuiKey_D                                     #-}
{-# COMPILE GHC ImGuiKey-C                                     = DearImGui.ImGuiKey_C                                     #-}
{-# COMPILE GHC ImGuiKey-B                                     = DearImGui.ImGuiKey_B                                     #-}
{-# COMPILE GHC ImGuiKey-A                                     = DearImGui.ImGuiKey_A                                     #-}
{-# COMPILE GHC ImGuiKey-9                                     = DearImGui.ImGuiKey_9                                     #-}
{-# COMPILE GHC ImGuiKey-8                                     = DearImGui.ImGuiKey_8                                     #-}
{-# COMPILE GHC ImGuiKey-7                                     = DearImGui.ImGuiKey_7                                     #-}
{-# COMPILE GHC ImGuiKey-6                                     = DearImGui.ImGuiKey_6                                     #-}
{-# COMPILE GHC ImGuiKey-5                                     = DearImGui.ImGuiKey_5                                     #-}
{-# COMPILE GHC ImGuiKey-4                                     = DearImGui.ImGuiKey_4                                     #-}
{-# COMPILE GHC ImGuiKey-3                                     = DearImGui.ImGuiKey_3                                     #-}
{-# COMPILE GHC ImGuiKey-2                                     = DearImGui.ImGuiKey_2                                     #-}
{-# COMPILE GHC ImGuiKey-1                                     = DearImGui.ImGuiKey_1                                     #-}
{-# COMPILE GHC ImGuiKey-0                                     = DearImGui.ImGuiKey_0                                     #-}
{-# COMPILE GHC ImGuiKey-Menu                                  = DearImGui.ImGuiKey_Menu                                  #-}
{-# COMPILE GHC ImGuiKey-RightSuper                            = DearImGui.ImGuiKey_RightSuper                            #-}
{-# COMPILE GHC ImGuiKey-RightAlt                              = DearImGui.ImGuiKey_RightAlt                              #-}
{-# COMPILE GHC ImGuiKey-RightShift                            = DearImGui.ImGuiKey_RightShift                            #-}
{-# COMPILE GHC ImGuiKey-RightCtrl                             = DearImGui.ImGuiKey_RightCtrl                             #-}
{-# COMPILE GHC ImGuiKey-LeftSuper                             = DearImGui.ImGuiKey_LeftSuper                             #-}
{-# COMPILE GHC ImGuiKey-LeftAlt                               = DearImGui.ImGuiKey_LeftAlt                               #-}
{-# COMPILE GHC ImGuiKey-LeftShift                             = DearImGui.ImGuiKey_LeftShift                             #-}
{-# COMPILE GHC ImGuiKey-LeftCtrl                              = DearImGui.ImGuiKey_LeftCtrl                              #-}
{-# COMPILE GHC ImGuiKey-Escape                                = DearImGui.ImGuiKey_Escape                                #-}
{-# COMPILE GHC ImGuiKey-Enter                                 = DearImGui.ImGuiKey_Enter                                 #-}
{-# COMPILE GHC ImGuiKey-Space                                 = DearImGui.ImGuiKey_Space                                 #-}
{-# COMPILE GHC ImGuiKey-Backspace                             = DearImGui.ImGuiKey_Backspace                             #-}
{-# COMPILE GHC ImGuiKey-Delete                                = DearImGui.ImGuiKey_Delete                                #-}
{-# COMPILE GHC ImGuiKey-Insert                                = DearImGui.ImGuiKey_Insert                                #-}
{-# COMPILE GHC ImGuiKey-End                                   = DearImGui.ImGuiKey_End                                   #-}
{-# COMPILE GHC ImGuiKey-Home                                  = DearImGui.ImGuiKey_Home                                  #-}
{-# COMPILE GHC ImGuiKey-PageDown                              = DearImGui.ImGuiKey_PageDown                              #-}
{-# COMPILE GHC ImGuiKey-PageUp                                = DearImGui.ImGuiKey_PageUp                                #-}
{-# COMPILE GHC ImGuiKey-DownArrow                             = DearImGui.ImGuiKey_DownArrow                             #-}
{-# COMPILE GHC ImGuiKey-UpArrow                               = DearImGui.ImGuiKey_UpArrow                               #-}
{-# COMPILE GHC ImGuiKey-RightArrow                            = DearImGui.ImGuiKey_RightArrow                            #-}
{-# COMPILE GHC ImGuiKey-LeftArrow                             = DearImGui.ImGuiKey_LeftArrow                             #-}
{-# COMPILE GHC ImGuiKey-Tab                                   = DearImGui.ImGuiKey_Tab                                   #-}
{-# COMPILE GHC ImGuiKey-None                                  = DearImGui.ImGuiKey_None                                  #-}
{-# COMPILE GHC ImGuiSortDirection-Descending                  = DearImGui.ImGuiSortDirection_Descending                  #-}
{-# COMPILE GHC ImGuiSortDirection-Ascending                   = DearImGui.ImGuiSortDirection_Ascending                   #-}
{-# COMPILE GHC ImGuiSortDirection-None                        = DearImGui.ImGuiSortDirection_None                        #-}
{-# COMPILE GHC ImGuiDir-Down                                  = DearImGui.ImGuiDir_Down                                  #-}
{-# COMPILE GHC ImGuiDir-Up                                    = DearImGui.ImGuiDir_Up                                    #-}
{-# COMPILE GHC ImGuiDir-Right                                 = DearImGui.ImGuiDir_Right                                 #-}
{-# COMPILE GHC ImGuiDir-Left                                  = DearImGui.ImGuiDir_Left                                  #-}
{-# COMPILE GHC ImGuiDir-None                                  = DearImGui.ImGuiDir_None                                  #-}
{-# COMPILE GHC ImGuiDataType-Double                           = DearImGui.ImGuiDataType_Double                           #-}
{-# COMPILE GHC ImGuiDataType-Float                            = DearImGui.ImGuiDataType_Float                            #-}
{-# COMPILE GHC ImGuiDataType-U64                              = DearImGui.ImGuiDataType_U64                              #-}
{-# COMPILE GHC ImGuiDataType-S64                              = DearImGui.ImGuiDataType_S64                              #-}
{-# COMPILE GHC ImGuiDataType-U32                              = DearImGui.ImGuiDataType_U32                              #-}
{-# COMPILE GHC ImGuiDataType-S32                              = DearImGui.ImGuiDataType_S32                              #-}
{-# COMPILE GHC ImGuiDataType-U16                              = DearImGui.ImGuiDataType_U16                              #-}
{-# COMPILE GHC ImGuiDataType-S16                              = DearImGui.ImGuiDataType_S16                              #-}
{-# COMPILE GHC ImGuiDataType-U8                               = DearImGui.ImGuiDataType_U8                               #-}
{-# COMPILE GHC ImGuiDataType-S8                               = DearImGui.ImGuiDataType_S8                               #-}
{-# COMPILE GHC ImGuiDragDropFlags-AcceptPeekOnly              = DearImGui.ImGuiDragDropFlags_AcceptPeekOnly              #-}
{-# COMPILE GHC ImGuiDragDropFlags-AcceptNoPreviewTooltip      = DearImGui.ImGuiDragDropFlags_AcceptNoPreviewTooltip      #-}
{-# COMPILE GHC ImGuiDragDropFlags-AcceptNoDrawDefaultRect     = DearImGui.ImGuiDragDropFlags_AcceptNoDrawDefaultRect     #-}
{-# COMPILE GHC ImGuiDragDropFlags-AcceptBeforeDelivery        = DearImGui.ImGuiDragDropFlags_AcceptBeforeDelivery        #-}
{-# COMPILE GHC ImGuiDragDropFlags-SourceAutoExpirePayload     = DearImGui.ImGuiDragDropFlags_SourceAutoExpirePayload     #-}
{-# COMPILE GHC ImGuiDragDropFlags-SourceExtern                = DearImGui.ImGuiDragDropFlags_SourceExtern                #-}
{-# COMPILE GHC ImGuiDragDropFlags-SourceAllowNullID           = DearImGui.ImGuiDragDropFlags_SourceAllowNullID           #-}
{-# COMPILE GHC ImGuiDragDropFlags-SourceNoHoldToOpenOthers    = DearImGui.ImGuiDragDropFlags_SourceNoHoldToOpenOthers    #-}
{-# COMPILE GHC ImGuiDragDropFlags-SourceNoDisableHover        = DearImGui.ImGuiDragDropFlags_SourceNoDisableHover        #-}
{-# COMPILE GHC ImGuiDragDropFlags-SourceNoPreviewTooltip      = DearImGui.ImGuiDragDropFlags_SourceNoPreviewTooltip      #-}
{-# COMPILE GHC ImGuiDragDropFlags-None                        = DearImGui.ImGuiDragDropFlags_None                        #-}
{-# COMPILE GHC ImGuiHoveredFlags-RootAndChildWindows          = DearImGui.ImGuiHoveredFlags_RootAndChildWindows          #-}
{-# COMPILE GHC ImGuiHoveredFlags-RectOnly                     = DearImGui.ImGuiHoveredFlags_RectOnly                     #-}
{-# COMPILE GHC ImGuiHoveredFlags-NoNavOverride                = DearImGui.ImGuiHoveredFlags_NoNavOverride                #-}
{-# COMPILE GHC ImGuiHoveredFlags-AllowWhenDisabled            = DearImGui.ImGuiHoveredFlags_AllowWhenDisabled            #-}
{-# COMPILE GHC ImGuiHoveredFlags-AllowWhenOverlapped          = DearImGui.ImGuiHoveredFlags_AllowWhenOverlapped          #-}
{-# COMPILE GHC ImGuiHoveredFlags-AllowWhenBlockedByActiveItem = DearImGui.ImGuiHoveredFlags_AllowWhenBlockedByActiveItem #-}
{-# COMPILE GHC ImGuiHoveredFlags-AllowWhenBlockedByPopup      = DearImGui.ImGuiHoveredFlags_AllowWhenBlockedByPopup      #-}
{-# COMPILE GHC ImGuiHoveredFlags-NoPopupHierarchy             = DearImGui.ImGuiHoveredFlags_NoPopupHierarchy             #-}
{-# COMPILE GHC ImGuiHoveredFlags-AnyWindow                    = DearImGui.ImGuiHoveredFlags_AnyWindow                    #-}
{-# COMPILE GHC ImGuiHoveredFlags-RootWindow                   = DearImGui.ImGuiHoveredFlags_RootWindow                   #-}
{-# COMPILE GHC ImGuiHoveredFlags-ChildWindows                 = DearImGui.ImGuiHoveredFlags_ChildWindows                 #-}
{-# COMPILE GHC ImGuiHoveredFlags-None                         = DearImGui.ImGuiHoveredFlags_None                         #-}
{-# COMPILE GHC ImGuiFocusedFlags-RootAndChildWindows          = DearImGui.ImGuiFocusedFlags_RootAndChildWindows          #-}
{-# COMPILE GHC ImGuiFocusedFlags-NoPopupHierarchy             = DearImGui.ImGuiFocusedFlags_NoPopupHierarchy             #-}
{-# COMPILE GHC ImGuiFocusedFlags-AnyWindow                    = DearImGui.ImGuiFocusedFlags_AnyWindow                    #-}
{-# COMPILE GHC ImGuiFocusedFlags-RootWindow                   = DearImGui.ImGuiFocusedFlags_RootWindow                   #-}
{-# COMPILE GHC ImGuiFocusedFlags-ChildWindows                 = DearImGui.ImGuiFocusedFlags_ChildWindows                 #-}
{-# COMPILE GHC ImGuiFocusedFlags-None                         = DearImGui.ImGuiFocusedFlags_None                         #-}
{-# COMPILE GHC ImGuiTableBgTarget-CellBg                      = DearImGui.ImGuiTableBgTarget_CellBg                      #-}
{-# COMPILE GHC ImGuiTableBgTarget-RowBg1                      = DearImGui.ImGuiTableBgTarget_RowBg1                      #-}
{-# COMPILE GHC ImGuiTableBgTarget-RowBg0                      = DearImGui.ImGuiTableBgTarget_RowBg0                      #-}
{-# COMPILE GHC ImGuiTableBgTarget-None                        = DearImGui.ImGuiTableBgTarget_None                        #-}
{-# COMPILE GHC ImGuiTableRowFlags-Headers                     = DearImGui.ImGuiTableRowFlags_Headers                     #-}
{-# COMPILE GHC ImGuiTableRowFlags-None                        = DearImGui.ImGuiTableRowFlags_None                        #-}
{-# COMPILE GHC ImGuiTableColumnFlags-NoDirectResize-          = DearImGui.ImGuiTableColumnFlags_NoDirectResize_          #-}
{-# COMPILE GHC ImGuiTableColumnFlags-StatusMask-              = DearImGui.ImGuiTableColumnFlags_StatusMask_              #-}
{-# COMPILE GHC ImGuiTableColumnFlags-IndentMask-              = DearImGui.ImGuiTableColumnFlags_IndentMask_              #-}
{-# COMPILE GHC ImGuiTableColumnFlags-WidthMask-               = DearImGui.ImGuiTableColumnFlags_WidthMask_               #-}
{-# COMPILE GHC ImGuiTableColumnFlags-IsHovered                = DearImGui.ImGuiTableColumnFlags_IsHovered                #-}
{-# COMPILE GHC ImGuiTableColumnFlags-IsSorted                 = DearImGui.ImGuiTableColumnFlags_IsSorted                 #-}
{-# COMPILE GHC ImGuiTableColumnFlags-IsVisible                = DearImGui.ImGuiTableColumnFlags_IsVisible                #-}
{-# COMPILE GHC ImGuiTableColumnFlags-IsEnabled                = DearImGui.ImGuiTableColumnFlags_IsEnabled                #-}
{-# COMPILE GHC ImGuiTableColumnFlags-IndentDisable            = DearImGui.ImGuiTableColumnFlags_IndentDisable            #-}
{-# COMPILE GHC ImGuiTableColumnFlags-IndentEnable             = DearImGui.ImGuiTableColumnFlags_IndentEnable             #-}
{-# COMPILE GHC ImGuiTableColumnFlags-PreferSortDescending     = DearImGui.ImGuiTableColumnFlags_PreferSortDescending     #-}
{-# COMPILE GHC ImGuiTableColumnFlags-PreferSortAscending      = DearImGui.ImGuiTableColumnFlags_PreferSortAscending      #-}
{-# COMPILE GHC ImGuiTableColumnFlags-NoHeaderWidth            = DearImGui.ImGuiTableColumnFlags_NoHeaderWidth            #-}
{-# COMPILE GHC ImGuiTableColumnFlags-NoHeaderLabel            = DearImGui.ImGuiTableColumnFlags_NoHeaderLabel            #-}
{-# COMPILE GHC ImGuiTableColumnFlags-NoSortDescending         = DearImGui.ImGuiTableColumnFlags_NoSortDescending         #-}
{-# COMPILE GHC ImGuiTableColumnFlags-NoSortAscending          = DearImGui.ImGuiTableColumnFlags_NoSortAscending          #-}
{-# COMPILE GHC ImGuiTableColumnFlags-NoSort                   = DearImGui.ImGuiTableColumnFlags_NoSort                   #-}
{-# COMPILE GHC ImGuiTableColumnFlags-NoClip                   = DearImGui.ImGuiTableColumnFlags_NoClip                   #-}
{-# COMPILE GHC ImGuiTableColumnFlags-NoHide                   = DearImGui.ImGuiTableColumnFlags_NoHide                   #-}
{-# COMPILE GHC ImGuiTableColumnFlags-NoReorder                = DearImGui.ImGuiTableColumnFlags_NoReorder                #-}
{-# COMPILE GHC ImGuiTableColumnFlags-NoResize                 = DearImGui.ImGuiTableColumnFlags_NoResize                 #-}
{-# COMPILE GHC ImGuiTableColumnFlags-WidthFixed               = DearImGui.ImGuiTableColumnFlags_WidthFixed               #-}
{-# COMPILE GHC ImGuiTableColumnFlags-WidthStretch             = DearImGui.ImGuiTableColumnFlags_WidthStretch             #-}
{-# COMPILE GHC ImGuiTableColumnFlags-DefaultSort              = DearImGui.ImGuiTableColumnFlags_DefaultSort              #-}
{-# COMPILE GHC ImGuiTableColumnFlags-DefaultHide              = DearImGui.ImGuiTableColumnFlags_DefaultHide              #-}
{-# COMPILE GHC ImGuiTableColumnFlags-Disabled                 = DearImGui.ImGuiTableColumnFlags_Disabled                 #-}
{-# COMPILE GHC ImGuiTableColumnFlags-None                     = DearImGui.ImGuiTableColumnFlags_None                     #-}
{-# COMPILE GHC ImGuiTableFlags-SizingMask-                    = DearImGui.ImGuiTableFlags_SizingMask_                    #-}
{-# COMPILE GHC ImGuiTableFlags-SortTristate                   = DearImGui.ImGuiTableFlags_SortTristate                   #-}
{-# COMPILE GHC ImGuiTableFlags-SortMulti                      = DearImGui.ImGuiTableFlags_SortMulti                      #-}
{-# COMPILE GHC ImGuiTableFlags-ScrollY                        = DearImGui.ImGuiTableFlags_ScrollY                        #-}
{-# COMPILE GHC ImGuiTableFlags-ScrollX                        = DearImGui.ImGuiTableFlags_ScrollX                        #-}
{-# COMPILE GHC ImGuiTableFlags-NoPadInnerX                    = DearImGui.ImGuiTableFlags_NoPadInnerX                    #-}
{-# COMPILE GHC ImGuiTableFlags-NoPadOuterX                    = DearImGui.ImGuiTableFlags_NoPadOuterX                    #-}
{-# COMPILE GHC ImGuiTableFlags-PadOuterX                      = DearImGui.ImGuiTableFlags_PadOuterX                      #-}
{-# COMPILE GHC ImGuiTableFlags-NoClip                         = DearImGui.ImGuiTableFlags_NoClip                         #-}
{-# COMPILE GHC ImGuiTableFlags-PreciseWidths                  = DearImGui.ImGuiTableFlags_PreciseWidths                  #-}
{-# COMPILE GHC ImGuiTableFlags-NoKeepColumnsVisible           = DearImGui.ImGuiTableFlags_NoKeepColumnsVisible           #-}
{-# COMPILE GHC ImGuiTableFlags-NoHostExtendY                  = DearImGui.ImGuiTableFlags_NoHostExtendY                  #-}
{-# COMPILE GHC ImGuiTableFlags-NoHostExtendX                  = DearImGui.ImGuiTableFlags_NoHostExtendX                  #-}
{-# COMPILE GHC ImGuiTableFlags-SizingStretchSame              = DearImGui.ImGuiTableFlags_SizingStretchSame              #-}
{-# COMPILE GHC ImGuiTableFlags-SizingStretchProp              = DearImGui.ImGuiTableFlags_SizingStretchProp              #-}
{-# COMPILE GHC ImGuiTableFlags-SizingFixedSame                = DearImGui.ImGuiTableFlags_SizingFixedSame                #-}
{-# COMPILE GHC ImGuiTableFlags-SizingFixedFit                 = DearImGui.ImGuiTableFlags_SizingFixedFit                 #-}
{-# COMPILE GHC ImGuiTableFlags-NoBordersInBodyUntilResize     = DearImGui.ImGuiTableFlags_NoBordersInBodyUntilResize     #-}
{-# COMPILE GHC ImGuiTableFlags-NoBordersInBody                = DearImGui.ImGuiTableFlags_NoBordersInBody                #-}
{-# COMPILE GHC ImGuiTableFlags-Borders                        = DearImGui.ImGuiTableFlags_Borders                        #-}
{-# COMPILE GHC ImGuiTableFlags-BordersOuter                   = DearImGui.ImGuiTableFlags_BordersOuter                   #-}
{-# COMPILE GHC ImGuiTableFlags-BordersInner                   = DearImGui.ImGuiTableFlags_BordersInner                   #-}
{-# COMPILE GHC ImGuiTableFlags-BordersV                       = DearImGui.ImGuiTableFlags_BordersV                       #-}
{-# COMPILE GHC ImGuiTableFlags-BordersH                       = DearImGui.ImGuiTableFlags_BordersH                       #-}
{-# COMPILE GHC ImGuiTableFlags-BordersOuterV                  = DearImGui.ImGuiTableFlags_BordersOuterV                  #-}
{-# COMPILE GHC ImGuiTableFlags-BordersInnerV                  = DearImGui.ImGuiTableFlags_BordersInnerV                  #-}
{-# COMPILE GHC ImGuiTableFlags-BordersOuterH                  = DearImGui.ImGuiTableFlags_BordersOuterH                  #-}
{-# COMPILE GHC ImGuiTableFlags-BordersInnerH                  = DearImGui.ImGuiTableFlags_BordersInnerH                  #-}
{-# COMPILE GHC ImGuiTableFlags-RowBg                          = DearImGui.ImGuiTableFlags_RowBg                          #-}
{-# COMPILE GHC ImGuiTableFlags-ContextMenuInBody              = DearImGui.ImGuiTableFlags_ContextMenuInBody              #-}
{-# COMPILE GHC ImGuiTableFlags-NoSavedSettings                = DearImGui.ImGuiTableFlags_NoSavedSettings                #-}
{-# COMPILE GHC ImGuiTableFlags-Sortable                       = DearImGui.ImGuiTableFlags_Sortable                       #-}
{-# COMPILE GHC ImGuiTableFlags-Hideable                       = DearImGui.ImGuiTableFlags_Hideable                       #-}
{-# COMPILE GHC ImGuiTableFlags-Reorderable                    = DearImGui.ImGuiTableFlags_Reorderable                    #-}
{-# COMPILE GHC ImGuiTableFlags-Resizable                      = DearImGui.ImGuiTableFlags_Resizable                      #-}
{-# COMPILE GHC ImGuiTableFlags-None                           = DearImGui.ImGuiTableFlags_None                           #-}
{-# COMPILE GHC ImGuiTabItemFlags-Trailing                     = DearImGui.ImGuiTabItemFlags_Trailing                     #-}
{-# COMPILE GHC ImGuiTabItemFlags-Leading                      = DearImGui.ImGuiTabItemFlags_Leading                      #-}
{-# COMPILE GHC ImGuiTabItemFlags-NoReorder                    = DearImGui.ImGuiTabItemFlags_NoReorder                    #-}
{-# COMPILE GHC ImGuiTabItemFlags-NoTooltip                    = DearImGui.ImGuiTabItemFlags_NoTooltip                    #-}
{-# COMPILE GHC ImGuiTabItemFlags-NoPushId                     = DearImGui.ImGuiTabItemFlags_NoPushId                     #-}
{-# COMPILE GHC ImGuiTabItemFlags-NoCloseWithMiddleMouseButton = DearImGui.ImGuiTabItemFlags_NoCloseWithMiddleMouseButton #-}
{-# COMPILE GHC ImGuiTabItemFlags-SetSelected                  = DearImGui.ImGuiTabItemFlags_SetSelected                  #-}
{-# COMPILE GHC ImGuiTabItemFlags-UnsavedDocument              = DearImGui.ImGuiTabItemFlags_UnsavedDocument              #-}
{-# COMPILE GHC ImGuiTabItemFlags-None                         = DearImGui.ImGuiTabItemFlags_None                         #-}
{-# COMPILE GHC ImGuiTabBarFlags-FittingPolicyDefault-         = DearImGui.ImGuiTabBarFlags_FittingPolicyDefault_         #-}
{-# COMPILE GHC ImGuiTabBarFlags-FittingPolicyMask-            = DearImGui.ImGuiTabBarFlags_FittingPolicyMask_            #-}
{-# COMPILE GHC ImGuiTabBarFlags-FittingPolicyScroll           = DearImGui.ImGuiTabBarFlags_FittingPolicyScroll           #-}
{-# COMPILE GHC ImGuiTabBarFlags-FittingPolicyResizeDown       = DearImGui.ImGuiTabBarFlags_FittingPolicyResizeDown       #-}
{-# COMPILE GHC ImGuiTabBarFlags-NoTooltip                     = DearImGui.ImGuiTabBarFlags_NoTooltip                     #-}
{-# COMPILE GHC ImGuiTabBarFlags-NoTabListScrollingButtons     = DearImGui.ImGuiTabBarFlags_NoTabListScrollingButtons     #-}
{-# COMPILE GHC ImGuiTabBarFlags-NoCloseWithMiddleMouseButton  = DearImGui.ImGuiTabBarFlags_NoCloseWithMiddleMouseButton  #-}
{-# COMPILE GHC ImGuiTabBarFlags-TabListPopupButton            = DearImGui.ImGuiTabBarFlags_TabListPopupButton            #-}
{-# COMPILE GHC ImGuiTabBarFlags-AutoSelectNewTabs             = DearImGui.ImGuiTabBarFlags_AutoSelectNewTabs             #-}
{-# COMPILE GHC ImGuiTabBarFlags-Reorderable                   = DearImGui.ImGuiTabBarFlags_Reorderable                   #-}
{-# COMPILE GHC ImGuiTabBarFlags-None                          = DearImGui.ImGuiTabBarFlags_None                          #-}
{-# COMPILE GHC ImGuiComboFlags-HeightMask-                    = DearImGui.ImGuiComboFlags_HeightMask_                    #-}
{-# COMPILE GHC ImGuiComboFlags-NoPreview                      = DearImGui.ImGuiComboFlags_NoPreview                      #-}
{-# COMPILE GHC ImGuiComboFlags-NoArrowButton                  = DearImGui.ImGuiComboFlags_NoArrowButton                  #-}
{-# COMPILE GHC ImGuiComboFlags-HeightLargest                  = DearImGui.ImGuiComboFlags_HeightLargest                  #-}
{-# COMPILE GHC ImGuiComboFlags-HeightLarge                    = DearImGui.ImGuiComboFlags_HeightLarge                    #-}
{-# COMPILE GHC ImGuiComboFlags-HeightRegular                  = DearImGui.ImGuiComboFlags_HeightRegular                  #-}
{-# COMPILE GHC ImGuiComboFlags-HeightSmall                    = DearImGui.ImGuiComboFlags_HeightSmall                    #-}
{-# COMPILE GHC ImGuiComboFlags-PopupAlignLeft                 = DearImGui.ImGuiComboFlags_PopupAlignLeft                 #-}
{-# COMPILE GHC ImGuiComboFlags-None                           = DearImGui.ImGuiComboFlags_None                           #-}
{-# COMPILE GHC ImGuiSelectableFlags-AllowItemOverlap          = DearImGui.ImGuiSelectableFlags_AllowItemOverlap          #-}
{-# COMPILE GHC ImGuiSelectableFlags-Disabled                  = DearImGui.ImGuiSelectableFlags_Disabled                  #-}
{-# COMPILE GHC ImGuiSelectableFlags-AllowDoubleClick          = DearImGui.ImGuiSelectableFlags_AllowDoubleClick          #-}
{-# COMPILE GHC ImGuiSelectableFlags-SpanAllColumns            = DearImGui.ImGuiSelectableFlags_SpanAllColumns            #-}
{-# COMPILE GHC ImGuiSelectableFlags-DontClosePopups           = DearImGui.ImGuiSelectableFlags_DontClosePopups           #-}
{-# COMPILE GHC ImGuiSelectableFlags-None                      = DearImGui.ImGuiSelectableFlags_None                      #-}
{-# COMPILE GHC ImGuiPopupFlags-AnyPopup                       = DearImGui.ImGuiPopupFlags_AnyPopup                       #-}
{-# COMPILE GHC ImGuiPopupFlags-AnyPopupLevel                  = DearImGui.ImGuiPopupFlags_AnyPopupLevel                  #-}
{-# COMPILE GHC ImGuiPopupFlags-AnyPopupId                     = DearImGui.ImGuiPopupFlags_AnyPopupId                     #-}
{-# COMPILE GHC ImGuiPopupFlags-NoOpenOverItems                = DearImGui.ImGuiPopupFlags_NoOpenOverItems                #-}
{-# COMPILE GHC ImGuiPopupFlags-NoOpenOverExistingPopup        = DearImGui.ImGuiPopupFlags_NoOpenOverExistingPopup        #-}
{-# COMPILE GHC ImGuiPopupFlags-MouseButtonDefault-            = DearImGui.ImGuiPopupFlags_MouseButtonDefault_            #-}
{-# COMPILE GHC ImGuiPopupFlags-MouseButtonMask-               = DearImGui.ImGuiPopupFlags_MouseButtonMask_               #-}
{-# COMPILE GHC ImGuiPopupFlags-MouseButtonMiddle              = DearImGui.ImGuiPopupFlags_MouseButtonMiddle              #-}
{-# COMPILE GHC ImGuiPopupFlags-MouseButtonRight               = DearImGui.ImGuiPopupFlags_MouseButtonRight               #-}
{-# COMPILE GHC ImGuiPopupFlags-MouseButtonLeft                = DearImGui.ImGuiPopupFlags_MouseButtonLeft                #-}
{-# COMPILE GHC ImGuiPopupFlags-None                           = DearImGui.ImGuiPopupFlags_None                           #-}
{-# COMPILE GHC ImGuiTreeNodeFlags-CollapsingHeader            = DearImGui.ImGuiTreeNodeFlags_CollapsingHeader            #-}
{-# COMPILE GHC ImGuiTreeNodeFlags-NavLeftJumpsBackHere        = DearImGui.ImGuiTreeNodeFlags_NavLeftJumpsBackHere        #-}
{-# COMPILE GHC ImGuiTreeNodeFlags-SpanFullWidth               = DearImGui.ImGuiTreeNodeFlags_SpanFullWidth               #-}
{-# COMPILE GHC ImGuiTreeNodeFlags-SpanAvailWidth              = DearImGui.ImGuiTreeNodeFlags_SpanAvailWidth              #-}
{-# COMPILE GHC ImGuiTreeNodeFlags-FramePadding                = DearImGui.ImGuiTreeNodeFlags_FramePadding                #-}
{-# COMPILE GHC ImGuiTreeNodeFlags-Bullet                      = DearImGui.ImGuiTreeNodeFlags_Bullet                      #-}
{-# COMPILE GHC ImGuiTreeNodeFlags-Leaf                        = DearImGui.ImGuiTreeNodeFlags_Leaf                        #-}
{-# COMPILE GHC ImGuiTreeNodeFlags-OpenOnArrow                 = DearImGui.ImGuiTreeNodeFlags_OpenOnArrow                 #-}
{-# COMPILE GHC ImGuiTreeNodeFlags-OpenOnDoubleClick           = DearImGui.ImGuiTreeNodeFlags_OpenOnDoubleClick           #-}
{-# COMPILE GHC ImGuiTreeNodeFlags-DefaultOpen                 = DearImGui.ImGuiTreeNodeFlags_DefaultOpen                 #-}
{-# COMPILE GHC ImGuiTreeNodeFlags-NoAutoOpenOnLog             = DearImGui.ImGuiTreeNodeFlags_NoAutoOpenOnLog             #-}
{-# COMPILE GHC ImGuiTreeNodeFlags-NoTreePushOnOpen            = DearImGui.ImGuiTreeNodeFlags_NoTreePushOnOpen            #-}
{-# COMPILE GHC ImGuiTreeNodeFlags-AllowItemOverlap            = DearImGui.ImGuiTreeNodeFlags_AllowItemOverlap            #-}
{-# COMPILE GHC ImGuiTreeNodeFlags-Framed                      = DearImGui.ImGuiTreeNodeFlags_Framed                      #-}
{-# COMPILE GHC ImGuiTreeNodeFlags-Selected                    = DearImGui.ImGuiTreeNodeFlags_Selected                    #-}
{-# COMPILE GHC ImGuiTreeNodeFlags-None                        = DearImGui.ImGuiTreeNodeFlags_None                        #-}
{-# COMPILE GHC ImGuiInputTextFlags-CallbackEdit               = DearImGui.ImGuiInputTextFlags_CallbackEdit               #-}
{-# COMPILE GHC ImGuiInputTextFlags-CallbackResize             = DearImGui.ImGuiInputTextFlags_CallbackResize             #-}
{-# COMPILE GHC ImGuiInputTextFlags-CharsScientific            = DearImGui.ImGuiInputTextFlags_CharsScientific            #-}
{-# COMPILE GHC ImGuiInputTextFlags-NoUndoRedo                 = DearImGui.ImGuiInputTextFlags_NoUndoRedo                 #-}
{-# COMPILE GHC ImGuiInputTextFlags-Password                   = DearImGui.ImGuiInputTextFlags_Password                   #-}
{-# COMPILE GHC ImGuiInputTextFlags-ReadOnly                   = DearImGui.ImGuiInputTextFlags_ReadOnly                   #-}
{-# COMPILE GHC ImGuiInputTextFlags-AlwaysOverwrite            = DearImGui.ImGuiInputTextFlags_AlwaysOverwrite            #-}
{-# COMPILE GHC ImGuiInputTextFlags-NoHorizontalScroll         = DearImGui.ImGuiInputTextFlags_NoHorizontalScroll         #-}
{-# COMPILE GHC ImGuiInputTextFlags-CtrlEnterForNewLine        = DearImGui.ImGuiInputTextFlags_CtrlEnterForNewLine        #-}
{-# COMPILE GHC ImGuiInputTextFlags-AllowTabInput              = DearImGui.ImGuiInputTextFlags_AllowTabInput              #-}
{-# COMPILE GHC ImGuiInputTextFlags-CallbackCharFilter         = DearImGui.ImGuiInputTextFlags_CallbackCharFilter         #-}
{-# COMPILE GHC ImGuiInputTextFlags-CallbackAlways             = DearImGui.ImGuiInputTextFlags_CallbackAlways             #-}
{-# COMPILE GHC ImGuiInputTextFlags-CallbackHistory            = DearImGui.ImGuiInputTextFlags_CallbackHistory            #-}
{-# COMPILE GHC ImGuiInputTextFlags-CallbackCompletion         = DearImGui.ImGuiInputTextFlags_CallbackCompletion         #-}
{-# COMPILE GHC ImGuiInputTextFlags-EnterReturnsTrue           = DearImGui.ImGuiInputTextFlags_EnterReturnsTrue           #-}
{-# COMPILE GHC ImGuiInputTextFlags-AutoSelectAll              = DearImGui.ImGuiInputTextFlags_AutoSelectAll              #-}
{-# COMPILE GHC ImGuiInputTextFlags-CharsNoBlank               = DearImGui.ImGuiInputTextFlags_CharsNoBlank               #-}
{-# COMPILE GHC ImGuiInputTextFlags-CharsUppercase             = DearImGui.ImGuiInputTextFlags_CharsUppercase             #-}
{-# COMPILE GHC ImGuiInputTextFlags-CharsHexadecimal           = DearImGui.ImGuiInputTextFlags_CharsHexadecimal           #-}
{-# COMPILE GHC ImGuiInputTextFlags-CharsDecimal               = DearImGui.ImGuiInputTextFlags_CharsDecimal               #-}
{-# COMPILE GHC ImGuiInputTextFlags-None                       = DearImGui.ImGuiInputTextFlags_None                       #-}
{-# COMPILE GHC ImGuiWindowFlags-ChildMenu                     = DearImGui.ImGuiWindowFlags_ChildMenu                     #-}
{-# COMPILE GHC ImGuiWindowFlags-Modal                         = DearImGui.ImGuiWindowFlags_Modal                         #-}
{-# COMPILE GHC ImGuiWindowFlags-Popup                         = DearImGui.ImGuiWindowFlags_Popup                         #-}
{-# COMPILE GHC ImGuiWindowFlags-Tooltip                       = DearImGui.ImGuiWindowFlags_Tooltip                       #-}
{-# COMPILE GHC ImGuiWindowFlags-ChildWindow                   = DearImGui.ImGuiWindowFlags_ChildWindow                   #-}
{-# COMPILE GHC ImGuiWindowFlags-NavFlattened                  = DearImGui.ImGuiWindowFlags_NavFlattened                  #-}
{-# COMPILE GHC ImGuiWindowFlags-NoInputs                      = DearImGui.ImGuiWindowFlags_NoInputs                      #-}
{-# COMPILE GHC ImGuiWindowFlags-NoDecoration                  = DearImGui.ImGuiWindowFlags_NoDecoration                  #-}
{-# COMPILE GHC ImGuiWindowFlags-NoNav                         = DearImGui.ImGuiWindowFlags_NoNav                         #-}
{-# COMPILE GHC ImGuiWindowFlags-UnsavedDocument               = DearImGui.ImGuiWindowFlags_UnsavedDocument               #-}
{-# COMPILE GHC ImGuiWindowFlags-NoNavFocus                    = DearImGui.ImGuiWindowFlags_NoNavFocus                    #-}
{-# COMPILE GHC ImGuiWindowFlags-NoNavInputs                   = DearImGui.ImGuiWindowFlags_NoNavInputs                   #-}
{-# COMPILE GHC ImGuiWindowFlags-AlwaysUseWindowPadding        = DearImGui.ImGuiWindowFlags_AlwaysUseWindowPadding        #-}
{-# COMPILE GHC ImGuiWindowFlags-AlwaysHorizontalScrollbar     = DearImGui.ImGuiWindowFlags_AlwaysHorizontalScrollbar     #-}
{-# COMPILE GHC ImGuiWindowFlags-AlwaysVerticalScrollbar       = DearImGui.ImGuiWindowFlags_AlwaysVerticalScrollbar       #-}
{-# COMPILE GHC ImGuiWindowFlags-NoBringToFrontOnFocus         = DearImGui.ImGuiWindowFlags_NoBringToFrontOnFocus         #-}
{-# COMPILE GHC ImGuiWindowFlags-NoFocusOnAppearing            = DearImGui.ImGuiWindowFlags_NoFocusOnAppearing            #-}
{-# COMPILE GHC ImGuiWindowFlags-HorizontalScrollbar           = DearImGui.ImGuiWindowFlags_HorizontalScrollbar           #-}
{-# COMPILE GHC ImGuiWindowFlags-MenuBar                       = DearImGui.ImGuiWindowFlags_MenuBar                       #-}
{-# COMPILE GHC ImGuiWindowFlags-NoMouseInputs                 = DearImGui.ImGuiWindowFlags_NoMouseInputs                 #-}
{-# COMPILE GHC ImGuiWindowFlags-NoSavedSettings               = DearImGui.ImGuiWindowFlags_NoSavedSettings               #-}
{-# COMPILE GHC ImGuiWindowFlags-NoBackground                  = DearImGui.ImGuiWindowFlags_NoBackground                  #-}
{-# COMPILE GHC ImGuiWindowFlags-AlwaysAutoResize              = DearImGui.ImGuiWindowFlags_AlwaysAutoResize              #-}
{-# COMPILE GHC ImGuiWindowFlags-NoCollapse                    = DearImGui.ImGuiWindowFlags_NoCollapse                    #-}
{-# COMPILE GHC ImGuiWindowFlags-NoScrollWithMouse             = DearImGui.ImGuiWindowFlags_NoScrollWithMouse             #-}
{-# COMPILE GHC ImGuiWindowFlags-NoScrollbar                   = DearImGui.ImGuiWindowFlags_NoScrollbar                   #-}
{-# COMPILE GHC ImGuiWindowFlags-NoMove                        = DearImGui.ImGuiWindowFlags_NoMove                        #-}
{-# COMPILE GHC ImGuiWindowFlags-NoResize                      = DearImGui.ImGuiWindowFlags_NoResize                      #-}
{-# COMPILE GHC ImGuiWindowFlags-NoTitleBar                    = DearImGui.ImGuiWindowFlags_NoTitleBar                    #-}
{-# COMPILE GHC ImGuiWindowFlags-None                          = DearImGui.ImGuiWindowFlags_None                          #-}


record ImVec2 : Set where
    constructor mkImVec2
    field
        x y : Float

{-# COMPILE GHC ImVec2 = data DearImGui.ImVec2 (DearImGui.ImVec2) #-}

postulate
    Storable[ImVec2] : Storable ImVec2
    Show[ImVec2]     : Show ImVec2

{-# COMPILE GHC Storable[ImVec2] = AgdaStorable #-}
{-# COMPILE GHC Show[ImVec2]     = AgdaShow     #-}


record ImVec3 : Set where
    constructor mkImVec3
    field
        x y z : Float

{-# COMPILE GHC ImVec3 = data DearImGui.ImVec3 (DearImGui.ImVec3) #-}

postulate
    Storable[ImVec3] : Storable ImVec3
    Show[ImVec3]     : Show ImVec3

{-# COMPILE GHC Storable[ImVec3] = AgdaStorable #-}
{-# COMPILE GHC Show[ImVec3]     = AgdaShow     #-}


record ImVec4 : Set where
    constructor mkImVec4
    field
        x y z w : Float

{-# COMPILE GHC ImVec4 = data DearImGui.ImVec4 (DearImGui.ImVec4) #-}

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

{-# COMPILE GHC ImGuiContext             = type DearImGui.ImGuiContext             #-}
{-# COMPILE GHC ImFont                   = type DearImGui.ImFont                   #-}
{-# COMPILE GHC ImFontConfig             = type DearImGui.ImFontConfig             #-}
{-# COMPILE GHC ImFontGlyphRangesBuilder = type DearImGui.ImFontGlyphRangesBuilder #-}
{-# COMPILE GHC ImDrawList               = type DearImGui.ImDrawList               #-}
{-# COMPILE GHC ImGuiListClipper         = type DearImGui.ImGuiListClipper         #-}


ImU32 : Set
ImU32 = Word32

ImGuiID : Set
ImGuiID = ImU32

ImS16 : Set
ImS16 = Int16

ImWchar : Set
ImWchar = Word32


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

{-# COMPILE GHC Context = type DearImGui.Context #-}

postulate
    createContext     : ⦃ MonadIO M ⦄ → M (Liftℓ _ Context)
    destroyContext    : ⦃ MonadIO M ⦄ → Context → M ⊤′
    getCurrentContext : ⦃ MonadIO M ⦄ → M (Liftℓ _ Context)
    setCurrentContext : ⦃ MonadIO M ⦄ → Context → M ⊤′

{-# COMPILE GHC createContext     = \ mℓ m AgdaMonadIO -> DearImGui.createContext     #-}
{-# COMPILE GHC destroyContext    = \ mℓ m AgdaMonadIO -> DearImGui.destroyContext    #-}
{-# COMPILE GHC getCurrentContext = \ mℓ m AgdaMonadIO -> DearImGui.getCurrentContext #-}
{-# COMPILE GHC setCurrentContext = \ mℓ m AgdaMonadIO -> DearImGui.setCurrentContext #-}


-- Main

data DrawData : Set where
    mkDrawData : Ptr ⊤ → DrawData

{-# COMPILE GHC DrawData = type DearImGui.DrawData #-}

postulate
    newFrame     : ⦃ MonadIO M ⦄ → M ⊤′
    endFrame     : ⦃ MonadIO M ⦄ → M ⊤′
    render       : ⦃ MonadIO M ⦄ → M ⊤′
    getDrawData  : ⦃ MonadIO M ⦄ → M (Liftℓ _ DrawData)
    checkVersion : ⦃ MonadIO M ⦄ → M ⊤′

{-# COMPILE GHC newFrame     = \ mℓ m AgdaMonadIO -> DearImGui.newFrame     #-}
{-# COMPILE GHC endFrame     = \ mℓ m AgdaMonadIO -> DearImGui.endFrame     #-}
{-# COMPILE GHC render       = \ mℓ m AgdaMonadIO -> DearImGui.render       #-}
{-# COMPILE GHC getDrawData  = \ mℓ m AgdaMonadIO -> DearImGui.getDrawData  #-}
{-# COMPILE GHC checkVersion = \ mℓ m AgdaMonadIO -> DearImGui.checkVersion #-}


-- Demo, Debug, Information

postulate
    showDemoWindow    : ⦃ MonadIO M ⦄ → M ⊤′
    showMetricsWindow : ⦃ MonadIO M ⦄ → M ⊤′
    showAboutWindow   : ⦃ MonadIO M ⦄ → M ⊤′
    showUserGuide     : ⦃ MonadIO M ⦄ → M ⊤′
    getVersion        : ⦃ MonadIO M ⦄ → M (Liftℓ _ Text)

{-# COMPILE GHC showDemoWindow    = \ mℓ m AgdaMonadIO -> DearImGui.showDemoWindow    #-}
{-# COMPILE GHC showMetricsWindow = \ mℓ m AgdaMonadIO -> DearImGui.showMetricsWindow #-}
{-# COMPILE GHC showAboutWindow   = \ mℓ m AgdaMonadIO -> DearImGui.showAboutWindow   #-}
{-# COMPILE GHC showUserGuide     = \ mℓ m AgdaMonadIO -> DearImGui.showUserGuide     #-}
{-# COMPILE GHC getVersion        = \ mℓ m AgdaMonadIO -> DearImGui.getVersion        #-}


-- Styles

postulate
    styleColorsDark    : ⦃ MonadIO M ⦄ → M ⊤′
    styleColorsLight   : ⦃ MonadIO M ⦄ → M ⊤′
    styleColorsClassic : ⦃ MonadIO M ⦄ → M ⊤′

{-# COMPILE GHC styleColorsDark    = \ mℓ m AgdaMonadIO -> DearImGui.styleColorsDark    #-}
{-# COMPILE GHC styleColorsLight   = \ mℓ m AgdaMonadIO -> DearImGui.styleColorsLight   #-}
{-# COMPILE GHC styleColorsClassic = \ mℓ m AgdaMonadIO -> DearImGui.styleColorsClassic #-}


-- Windows

postulate
    withWindow      : ⦃ MonadUnliftIO M ⦄ → Text → (Bool → M A) → M A
    withWindowOpen  : ⦃ MonadUnliftIO M ⦄ → Text → M ⊤′ → M ⊤′
    withFullscreen  : ⦃ MonadUnliftIO M ⦄ → M ⊤′ → M ⊤′
    fullscreenFlags : ImGuiWindowFlags
    begin           : ⦃ MonadIO M ⦄ → Text → M (Liftℓ _ Bool)
    end             : ⦃ MonadIO M ⦄ → M ⊤′

{-# COMPILE GHC withWindow      = \ mℓ m a AgdaMonadUnliftIO -> DearImGui.withWindow      #-}
{-# COMPILE GHC withWindowOpen  = \ mℓ m AgdaMonadUnliftIO   -> DearImGui.withWindowOpen  #-}
{-# COMPILE GHC withFullscreen  = \ mℓ m AgdaMonadUnliftIO   -> DearImGui.withFullscreen  #-}
{-# COMPILE GHC fullscreenFlags =                               DearImGui.fullscreenFlags #-}
{-# COMPILE GHC begin           = \ mℓ m AgdaMonadIO         -> DearImGui.begin           #-}
{-# COMPILE GHC end             = \ mℓ m AgdaMonadIO         -> DearImGui.end             #-}


-- Utilities

postulate
    -- todo: (requires DearImGui.Raw(.DrawList)) getWindowDrawList : ⦃ MonadIO M ⦄ → M (Liftℓ _ DrawList)
    getWindowPos      : ⦃ MonadIO M ⦄ → M (Liftℓ _ ImVec2)
    getWindowSize     : ⦃ MonadIO M ⦄ → M (Liftℓ _ ImVec2)
    getWindowWidth    : ⦃ MonadIO M ⦄ → M (Liftℓ _ CFloat)
    getWindowHeight   : ⦃ MonadIO M ⦄ → M (Liftℓ _ CFloat)

{-# COMPILE GHC getWindowPos    = \ mℓ m AgdaMonadIO -> DearImGui.getWindowPos    #-}
{-# COMPILE GHC getWindowSize   = \ mℓ m AgdaMonadIO -> DearImGui.getWindowSize   #-}
{-# COMPILE GHC getWindowWidth  = \ mℓ m AgdaMonadIO -> DearImGui.getWindowWidth  #-}
{-# COMPILE GHC getWindowHeight = \ mℓ m AgdaMonadIO -> DearImGui.getWindowHeight #-}


-- Manipulation

postulate
    setNextWindowPos             : ⦃ MonadIO M ⦄ → ⦃ HasGetter R ImVec2 ⦄ → R → ImGuiCond → Maybe R → M ⊤′
    setNextWindowSize            : ⦃ MonadIO M ⦄ → ⦃ HasGetter R ImVec2 ⦄ → R → ImGuiCond → M ⊤′
    setNextWindowFullscreen      : ⦃ MonadIO M ⦄ → M ⊤′
    setNextWindowContentSize     : ⦃ MonadIO M ⦄ → ⦃ HasGetter R ImVec2 ⦄ → R → M ⊤′
    setNextWindowSizeConstraints : ⦃ MonadIO M ⦄ → ⦃ HasGetter R ImVec2 ⦄ → R → R → M ⊤′
    setNextWindowCollapsed       : ⦃ MonadIO M ⦄ → Bool → ImGuiCond → M ⊤′
    setNextWindowBgAlpha         : ⦃ MonadIO M ⦄ → Float → M ⊤′

{-# COMPILE GHC setNextWindowPos             = \ mℓ m rℓ r AgdaMonadIO AgdaHasGetter -> DearImGui.setNextWindowPos             #-}
{-# COMPILE GHC setNextWindowSize            = \ mℓ m rℓ r AgdaMonadIO AgdaHasGetter -> DearImGui.setNextWindowSize            #-}
{-# COMPILE GHC setNextWindowFullscreen      = \ mℓ m AgdaMonadIO                    -> DearImGui.setNextWindowFullscreen      #-}
{-# COMPILE GHC setNextWindowContentSize     = \ mℓ m rℓ r AgdaMonadIO AgdaHasGetter -> DearImGui.setNextWindowContentSize     #-}
{-# COMPILE GHC setNextWindowSizeConstraints = \ mℓ m rℓ r AgdaMonadIO AgdaHasGetter -> DearImGui.setNextWindowSizeConstraints #-}
{-# COMPILE GHC setNextWindowCollapsed       = \ mℓ m AgdaMonadIO                    -> DearImGui.setNextWindowCollapsed       #-}
{-# COMPILE GHC setNextWindowBgAlpha         = \ mℓ m AgdaMonadIO                    -> DearImGui.setNextWindowBgAlpha         #-}


-- Child Windows

postulate
    withChild        : ⦃ MonadUnliftIO M ⦄ → Text → ImVec2 → Bool → ImGuiWindowFlags → (Bool → M A) → M A
    withChildOpen    : ⦃ MonadUnliftIO M ⦄ → Text → ImVec2 → Bool → ImGuiWindowFlags → M ⊤′ → M ⊤′
    withChildContext : ⦃ MonadUnliftIO M ⦄ → Text → (Bool → M A) → M A
    beginChild       : ⦃ MonadIO M ⦄ → Text → ImVec2 → Bool → ImGuiWindowFlags → M (Liftℓ _ Bool)
    endChild         : ⦃ MonadIO M ⦄ → M ⊤′

{-# COMPILE GHC withChild        = \ mℓ m a AgdaMonadUnliftIO -> DearImGui.withChild        #-}
{-# COMPILE GHC withChildOpen    = \ mℓ m AgdaMonadUnliftIO   -> DearImGui.withChildOpen    #-}
{-# COMPILE GHC withChildContext = \ mℓ m a AgdaMonadUnliftIO -> DearImGui.withChildContext #-}
{-# COMPILE GHC beginChild       = \ mℓ m AgdaMonadIO         -> DearImGui.beginChild       #-}
{-# COMPILE GHC endChild         = \ mℓ m AgdaMonadIO         -> DearImGui.endChild         #-}


-- Parameter stacks

postulate
    Font : Set

    withStyleColor : ⦃ MonadUnliftIO M ⦄ → ⦃ HasGetter R ImVec4 ⦄ → ImGuiCol → R → M A → M A
    pushStyleColor : ⦃ MonadIO M ⦄ → ⦃ HasGetter R ImVec4 ⦄ → ImGuiCol → R → M ⊤′
    popStyleColor  : ⦃ MonadIO M ⦄ → CInt → M ⊤′
    withStyleVar   : ⦃ MonadUnliftIO M ⦄ → ⦃ HasGetter R ImVec2 ⦄ → ImGuiStyleVar → R → M A → M A
    pushStyleVar   : ⦃ MonadIO M ⦄ → ⦃ HasGetter R ImVec2 ⦄ → ImGuiStyleVar → R → M ⊤′
    popStyleVar    : ⦃ MonadIO M ⦄ → Int → M ⊤′
    withFont       : ⦃ MonadUnliftIO M ⦄ → Font → M A → M A
    pushFont       : ⦃ MonadIO M ⦄ → Font → M ⊤′
    popFont        : ⦃ MonadIO M ⦄ → M ⊤′

{-# COMPILE GHC Font = type DearImGui.Font #-}

{-# COMPILE GHC withStyleColor = \ mℓ m rℓ r a AgdaMonadUnliftIO AgdaHasGetter -> DearImGui.withStyleColor #-}
{-# COMPILE GHC pushStyleColor = \ mℓ m rℓ r AgdaMonadIO AgdaHasGetter         -> DearImGui.pushStyleColor #-}
{-# COMPILE GHC popStyleColor  = \ mℓ m AgdaMonadIO                            -> DearImGui.popStyleColor  #-}
{-# COMPILE GHC withStyleVar   = \ mℓ m rℓ r a AgdaMonadUnliftIO AgdaHasGetter -> DearImGui.withStyleVar   #-}
{-# COMPILE GHC pushStyleVar   = \ mℓ m rℓ r AgdaMonadIO AgdaHasGetter         -> DearImGui.pushStyleVar   #-}
{-# COMPILE GHC popStyleVar    = \ mℓ m AgdaMonadIO                            -> DearImGui.popStyleVar    #-}
{-# COMPILE GHC withFont       = \ mℓ m a AgdaMonadUnliftIO                    -> DearImGui.withFont       #-}
{-# COMPILE GHC pushFont       = \ mℓ m AgdaMonadIO                            -> DearImGui.pushFont       #-}
{-# COMPILE GHC popFont        = \ mℓ m AgdaMonadIO                            -> DearImGui.popFont        #-}


-- Cursor/Layout

postulate
    separator               : ⦃ MonadIO M ⦄ → M ⊤′
    sameLine                : ⦃ MonadIO M ⦄ → M ⊤′
    newLine                 : ⦃ MonadIO M ⦄ → M ⊤′
    spacing                 : ⦃ MonadIO M ⦄ → M ⊤′
    dummy                   : ⦃ MonadIO M ⦄ → ⦃ HasGetter R ImVec2 ⦄ → R → M ⊤′
    withIndent              : ⦃ MonadUnliftIO M ⦄ → Float → M A → M A
    indent                  : ⦃ MonadIO M ⦄ → Float → M ⊤′
    unindent                : ⦃ MonadIO M ⦄ → Float → M ⊤′
    setNextItemWidth        : ⦃ MonadIO M ⦄ → Float → M ⊤′
    withItemWidth           : ⦃ MonadUnliftIO M ⦄ → Float → M A → M A
    pushItemWidth           : ⦃ MonadIO M ⦄ → Float → M ⊤′
    popItemWidth            : ⦃ MonadIO M ⦄ → M ⊤′
    withGroup               : ⦃ MonadUnliftIO M ⦄ → M A → M A
    beginGroup              : ⦃ MonadIO M ⦄ → M ⊤′
    endGroup                : ⦃ MonadIO M ⦄ → M ⊤′
    setCursorPos            : ⦃ MonadIO M ⦄ → ⦃ HasGetter R ImVec2 ⦄ → R → M ⊤′
    alignTextToFramePadding : ⦃ MonadIO M ⦄ → M ⊤′

{-# COMPILE GHC separator               = \ mℓ m AgdaMonadIO                    -> DearImGui.separator               #-}
{-# COMPILE GHC sameLine                = \ mℓ m AgdaMonadIO                    -> DearImGui.sameLine                #-}
{-# COMPILE GHC newLine                 = \ mℓ m AgdaMonadIO                    -> DearImGui.newLine                 #-}
{-# COMPILE GHC spacing                 = \ mℓ m AgdaMonadIO                    -> DearImGui.spacing                 #-}
{-# COMPILE GHC dummy                   = \ mℓ m rℓ r AgdaMonadIO AgdaHasGetter -> DearImGui.dummy                   #-}
{-# COMPILE GHC withIndent              = \ mℓ m a AgdaMonadUnliftIO            -> DearImGui.withIndent              #-}
{-# COMPILE GHC indent                  = \ mℓ m AgdaMonadIO                    -> DearImGui.indent                  #-}
{-# COMPILE GHC unindent                = \ mℓ m AgdaMonadIO                    -> DearImGui.unindent                #-}
{-# COMPILE GHC setNextItemWidth        = \ mℓ m AgdaMonadIO                    -> DearImGui.setNextItemWidth        #-}
{-# COMPILE GHC withItemWidth           = \ mℓ m a AgdaMonadUnliftIO            -> DearImGui.withItemWidth           #-}
{-# COMPILE GHC pushItemWidth           = \ mℓ m AgdaMonadIO                    -> DearImGui.pushItemWidth           #-}
{-# COMPILE GHC popItemWidth            = \ mℓ m AgdaMonadIO                    -> DearImGui.popItemWidth            #-}
{-# COMPILE GHC withGroup               = \ mℓ m a AgdaMonadUnliftIO            -> DearImGui.withGroup               #-}
{-# COMPILE GHC beginGroup              = \ mℓ m AgdaMonadIO                    -> DearImGui.beginGroup              #-}
{-# COMPILE GHC endGroup                = \ mℓ m AgdaMonadIO                    -> DearImGui.endGroup                #-}
{-# COMPILE GHC setCursorPos            = \ mℓ m rℓ r AgdaMonadIO AgdaHasGetter -> DearImGui.setCursorPos            #-}
{-# COMPILE GHC alignTextToFramePadding = \ mℓ m AgdaMonadIO                    -> DearImGui.alignTextToFramePadding #-}


-- ID stack

postulate
    ToID : Set aℓ → Set aℓ

    ToID[CInt]                   : ToID CInt
    ToID[Text]                   : ToID Text
    ToID[Integer]                : ToID Integer
    ToID[Int]                    : ToID Int
    ToID[Ptr[CChar]]             : ToID (Ptr CChar)
    ToID[Ptr[A]]                 : ToID (Ptr A)
    ToID[Tuple2[Ptr[CChar],Int]] : ToID (Tuple2 (Ptr CChar) Int)

    pushID : ⦃ ToID A ⦄ → ⦃ MonadIO M ⦄ → A → M ⊤′
    withID : ⦃ MonadUnliftIO M ⦄ → ⦃ ToID A ⦄ → A → M B → M B

{-# FOREIGN GHC data AgdaToID aℓ a = DearImGui.ToID a => AgdaToID #-}
{-# COMPILE GHC ToID = type(0) AgdaToID #-}

{-# COMPILE GHC ToID[CInt]                   =           AgdaToID #-}
{-# COMPILE GHC ToID[Text]                   =           AgdaToID #-}
{-# COMPILE GHC ToID[Integer]                =           AgdaToID #-}
{-# COMPILE GHC ToID[Int]                    =           AgdaToID #-}
{-# COMPILE GHC ToID[Ptr[CChar]]             =           AgdaToID #-}
{-# COMPILE GHC ToID[Ptr[A]]                 = \ aℓ a -> AgdaToID #-}
{-# COMPILE GHC ToID[Tuple2[Ptr[CChar],Int]] =           AgdaToID #-}

{-# COMPILE GHC pushID = \ aℓ a mℓ m AgdaToID AgdaMonadIO         -> DearImGui.pushID #-}
{-# COMPILE GHC withID = \ mℓ m aℓ a b AgdaMonadUnliftIO AgdaToID -> DearImGui.withID #-}


-- Widgets

record TableOptions : Set where
    constructor mkTableOptions
    field
        tableFlags      : ImGuiTableFlags
        tableOuterSize  : ImVec2
        tableInnerWidth : Float

{-# COMPILE GHC TableOptions = data DearImGui.TableOptions (DearImGui.TableOptions) #-}

postulate
    Show[TableOptions] : Show TableOptions

{-# COMPILE GHC Show[TableOptions] = AgdaShow #-}

record TableColumnOptions : Set where
    constructor mkTableColumnOptions
    field
        tableColumnFlags             : ImGuiTableColumnFlags
        tableColumnInitWidthOrWeight : Float
        tableColumnUserId            : ImGuiID

{-# COMPILE GHC TableColumnOptions = data DearImGui.TableColumnOptions (DearImGui.TableColumnOptions) #-}

postulate
    Show[TableColumnOptions] : Show TableColumnOptions

{-# COMPILE GHC Show[TableColumnOptions] = AgdaShow #-}

record TableRowOptions : Set where
    constructor mkTableRowOptions
    field
        tableRowFlags     : ImGuiTableRowFlags
        tableRowMinHeight : Float

{-# COMPILE GHC TableRowOptions = data DearImGui.TableRowOptions (DearImGui.TableRowOptions) #-}

postulate
    Show[TableRowOptions] : Show TableRowOptions

{-# COMPILE GHC Show[TableRowOptions] = AgdaShow #-}

record TableSortingSpecs : Set where
    constructor mkTableSortingSpecs
    field
        tableSortingColumn  : Int
        tableSortingReverse : Bool
        tableSortingUserId  : ImGuiID

{-# COMPILE GHC TableSortingSpecs = data DearImGui.TableSortingSpecs (DearImGui.TableSortingSpecs) #-}

postulate
    Show[TableSortingSpecs] : Show TableSortingSpecs
    Eq[TableSortingSpecs]   : Eq TableSortingSpecs
    Ord[TableSortingSpecs]  : Ord TableSortingSpecs

{-# COMPILE GHC Show[TableSortingSpecs] = AgdaShow #-}
{-# COMPILE GHC Eq[TableSortingSpecs]   = AgdaEq   #-}
{-# COMPILE GHC Ord[TableSortingSpecs]  = AgdaOrd  #-}

record SelectableOptions : Set where
    constructor mkSelectableOptions
    field
        selected : Bool
        flags    : ImGuiSelectableFlags
        size     : ImVec2

{-# COMPILE GHC SelectableOptions = data DearImGui.SelectableOptions (DearImGui.SelectableOptions) #-}

postulate
    Show[SelectableOptions] : Show SelectableOptions

{-# COMPILE GHC Show[SelectableOptions] = AgdaShow #-}


postulate

    -- Text
    text         : ⦃ MonadIO M ⦄ → Text → M ⊤′
    textColored  : ⦃ HasGetter R ImVec4 ⦄ → ⦃ MonadIO M ⦄ → R → Text → M ⊤′
    textDisabled : ⦃ MonadIO M ⦄ → Text → M ⊤′
    textWrapped  : ⦃ MonadIO M ⦄ → Text → M ⊤′
    labelText    : ⦃ MonadIO M ⦄ → Text → Text → M ⊤′
    bulletText   : ⦃ MonadIO M ⦄ → Text → M ⊤′

    -- Main
    button          : ⦃ MonadIO M ⦄ → Text → M (Liftℓ _ Bool)
    smallButton     : ⦃ MonadIO M ⦄ → Text → M (Liftℓ _ Bool)
    invisibleButton : ⦃ MonadIO M ⦄ → Text → ImVec2 → ImGuiButtonFlags → M (Liftℓ _ Bool)
    arrowButton     : ⦃ MonadIO M ⦄ → Text → ImGuiDir → M (Liftℓ _ Bool)
    image           : ⦃ MonadIO M ⦄ → Ptr ⊤ → Ptr ImVec2 → Ptr ImVec2 → Ptr ImVec2 → Ptr ImVec4 → Ptr ImVec4 → M ⊤′
    checkbox        : ⦃ HasSetter R Bool ⦄ → ⦃ HasGetter R Bool ⦄ → ⦃ MonadIO M ⦄ → Text → R → M (Liftℓ _ Bool)
    progressBar     : ⦃ MonadIO M ⦄ → Float → Maybe Text → M ⊤′
    bullet          : ⦃ MonadIO M ⦄ → M ⊤′

    -- Combo Box
    withCombo     : ⦃ MonadUnliftIO M ⦄ → Text → Text → (Bool → M A) → M A
    withComboOpen : ⦃ MonadUnliftIO M ⦄ → Text → Text → M ⊤′ → M ⊤′
    beginCombo    : ⦃ MonadIO M ⦄ → Text → Text → M (Liftℓ _ Bool)
    endCombo      : ⦃ MonadIO M ⦄ → M ⊤′
    combo         : ⦃ MonadIO M ⦄ → ⦃ HasGetter R Int ⦄ → ⦃ HasSetter R Int ⦄ → Text → R → List Text → M (Liftℓ _ Bool)

    -- Drag Sliders
    dragFloat       : ⦃ MonadIO M ⦄ → ⦃ HasSetter R Float ⦄ → ⦃ HasGetter R Float ⦄ → Text → R → Float → Float → Float → M (Liftℓ _ Bool)
    dragFloat2      : ⦃ MonadIO M ⦄ → ⦃ HasSetter R (Tuple2 Float Float) ⦄ → ⦃ HasGetter R (Tuple2 Float Float) ⦄ → Text → R → Float → Float → Float → M (Liftℓ _ Bool)
    dragFloat3      : ⦃ MonadIO M ⦄ → ⦃ HasSetter R (Tuple3 Float Float Float) ⦄ → ⦃ HasGetter R (Tuple3 Float Float Float) ⦄ → Text → R → Float → Float → Float → M (Liftℓ _ Bool)
    dragFloat4      : ⦃ MonadIO M ⦄ → ⦃ HasSetter R (Tuple4 Float Float Float Float) ⦄ → ⦃ HasGetter R (Tuple4 Float Float Float Float) ⦄ → Text → R → Float → Float → Float → M (Liftℓ _ Bool)
    dragFloatRange2 : ⦃ MonadIO M ⦄ → ⦃ HasSetter R Float ⦄ → ⦃ HasGetter R Float ⦄ → Text → R → R → Float → Float → Float → Text → Text → M (Liftℓ _ Bool)
    dragInt         : ⦃ MonadIO M ⦄ → ⦃ HasSetter R Int ⦄ → ⦃ HasGetter R Int ⦄ → Text → R → Float → Int → Int → M (Liftℓ _ Bool)
    dragInt2        : ⦃ MonadIO M ⦄ → ⦃ HasSetter R (Tuple2 Int Int) ⦄ → ⦃ HasGetter R (Tuple2 Int Int) ⦄ → Text → R → Float → Int → Int → M (Liftℓ _ Bool)
    dragInt3        : ⦃ MonadIO M ⦄ → ⦃ HasSetter R (Tuple3 Int Int Int) ⦄ → ⦃ HasGetter R (Tuple3 Int Int Int) ⦄ → Text → R → Float → Int → Int → M (Liftℓ _ Bool)
    dragInt4        : ⦃ MonadIO M ⦄ → ⦃ HasSetter R (Tuple4 Int Int Int Int) ⦄ → ⦃ HasGetter R (Tuple4 Int Int Int Int) ⦄ → Text → R → Float → Int → Int → M (Liftℓ _ Bool)
    dragIntRange2   : ⦃ MonadIO M ⦄ → ⦃ HasSetter R Int ⦄ → ⦃ HasGetter R Int ⦄ → Text → R → R → Float → Int → Int → Text → Text → M (Liftℓ _ Bool)
    dragScalar      : ⦃ HasSetter R A ⦄ → ⦃ HasGetter R A ⦄ → ⦃ HasGetter Range A ⦄ → ⦃ Storable A ⦄ → ⦃ MonadIO M ⦄ → Text → ImGuiDataType → R → Float → Range → Range → Text → ImGuiSliderFlags → M (Liftℓ _ Bool)
    dragScalarN     : ⦃ HasSetter R (List A) ⦄ → ⦃ HasGetter R (List A) ⦄ → ⦃ HasGetter Range A ⦄ → ⦃ Storable A ⦄ → ⦃ MonadIO M ⦄ → Text → ImGuiDataType → R → Float → Range → Range → Text → ImGuiSliderFlags → M (Liftℓ _ Bool)

    -- Slider
    sliderFloat   : ⦃ MonadIO M ⦄ → ⦃ HasSetter R Float ⦄ → ⦃ HasGetter R Float ⦄ → Text → R → Float → Float → M (Liftℓ _ Bool)
    sliderFloat2  : ⦃ MonadIO M ⦄ → ⦃ HasSetter R (Tuple2 Float Float) ⦄ → ⦃ HasGetter R (Tuple2 Float Float) ⦄ → Text → R → Float → Float → M (Liftℓ _ Bool)
    sliderFloat3  : ⦃ MonadIO M ⦄ → ⦃ HasSetter R (Tuple3 Float Float Float) ⦄ → ⦃ HasGetter R (Tuple3 Float Float Float) ⦄ → Text → R → Float → Float → M (Liftℓ _ Bool)
    sliderFloat4  : ⦃ MonadIO M ⦄ → ⦃ HasSetter R (Tuple4 Float Float Float Float) ⦄ → ⦃ HasGetter R (Tuple4 Float Float Float Float) ⦄ → Text → R → Float → Float → M (Liftℓ _ Bool)
    sliderAngle   : ⦃ MonadIO M ⦄ → ⦃ HasSetter R Float ⦄ → ⦃ HasGetter R Float ⦄ → Text → R → Float → Float → M (Liftℓ _ Bool)
    sliderInt     : ⦃ MonadIO M ⦄ → ⦃ HasSetter R Int ⦄ → ⦃ HasGetter R Int ⦄ → Text → R → Int → Int → M (Liftℓ _ Bool)
    sliderInt2    : ⦃ MonadIO M ⦄ → ⦃ HasSetter R (Tuple2 Int Int) ⦄ → ⦃ HasGetter R (Tuple2 Int Int) ⦄ → Text → R → Int → Int → M (Liftℓ _ Bool)
    sliderInt3    : ⦃ MonadIO M ⦄ → ⦃ HasSetter R (Tuple3 Int Int Int) ⦄ → ⦃ HasGetter R (Tuple3 Int Int Int) ⦄ → Text → R → Int → Int → M (Liftℓ _ Bool)
    sliderInt4    : ⦃ MonadIO M ⦄ → ⦃ HasSetter R (Tuple4 Int Int Int Int) ⦄ → ⦃ HasGetter R (Tuple4 Int Int Int Int) ⦄ → Text → R → Int → Int → M (Liftℓ _ Bool)
    sliderScalar  : ⦃ HasGetter R A ⦄ → ⦃ HasSetter R A ⦄ → ⦃ HasGetter Range A ⦄ → ⦃ Storable A ⦄ → ⦃ MonadIO M ⦄ → Text → ImGuiDataType → R → Range → Range → Text → ImGuiSliderFlags → M (Liftℓ _ Bool)
    sliderScalarN : ⦃ HasSetter Value (List A) ⦄ → ⦃ HasGetter Value (List A) ⦄ → ⦃ HasGetter Range A ⦄ → ⦃ Storable A ⦄ → ⦃ MonadIO M ⦄ → Text → ImGuiDataType → Value → Range → Range → Text → ImGuiSliderFlags → M (Liftℓ _ Bool)
    vSliderFloat  : ⦃ HasSetter R Float ⦄ → ⦃ HasGetter R Float ⦄ → ⦃ MonadIO M ⦄ → Text → ImVec2 → R → Float → Float → M (Liftℓ _ Bool)
    vSliderInt    : ⦃ HasSetter R Int ⦄ → ⦃ HasGetter R Int ⦄ → ⦃ MonadIO M ⦄ → Text → ImVec2 → R → Int → Int → M (Liftℓ _ Bool)
    vSliderScalar : ⦃ HasSetter R A ⦄ → ⦃ HasGetter R A ⦄ → ⦃ HasGetter Range A ⦄ → ⦃ Storable A ⦄ → ⦃ MonadIO M ⦄ → Text → ImVec2 → ImGuiDataType → R → Range → Range → Text → ImGuiSliderFlags → M (Liftℓ _ Bool)

    -- Text Input
    inputText          : ⦃ MonadIO M ⦄ → ⦃ HasSetter R Text ⦄ → ⦃ HasGetter R Text ⦄ → Text → R → Int → M (Liftℓ _ Bool)
    inputTextMultiline : ⦃ MonadIO M ⦄ → ⦃ HasSetter R Text ⦄ → ⦃ HasGetter R Text ⦄ → Text → R → Int → ImVec2 → M (Liftℓ _ Bool)
    inputTextWithHint  : ⦃ MonadIO M ⦄ → ⦃ HasSetter R Text ⦄ → ⦃ HasGetter R Text ⦄ → Text → Text → R → Int → M (Liftℓ _ Bool)

    -- Color Editor/Picker
    colorPicker3 : ⦃ MonadIO M ⦄ → ⦃ HasSetter R ImVec3 ⦄ → ⦃ HasGetter R ImVec3 ⦄ → Text → R → M (Liftℓ _ Bool)
    colorButton  : ⦃ MonadIO M ⦄ → ⦃ HasSetter R ImVec4 ⦄ → ⦃ HasGetter R ImVec4 ⦄ → Text → R → M (Liftℓ _ Bool)

    -- Tables
    withTable       : ⦃ MonadUnliftIO M ⦄ → TableOptions → Text → Int → (Bool → M A) → M A
    withTableOpen   : ⦃ MonadUnliftIO M ⦄ → TableOptions → Text → Int → M ⊤′ → M ⊤′
    defTableOptions : TableOptions
    beginTable      : ⦃ MonadIO M ⦄ → TableOptions → Text → Int → M (Liftℓ _ Bool)
    endTable        : ⦃ MonadIO M ⦄ → M ⊤′

    -- Setup
    tableSetupColumn       : ⦃ MonadIO M ⦄ → Text → M ⊤′
    tableSetupColumnWith   : ⦃ MonadIO M ⦄ → TableColumnOptions → Text → M ⊤′
    defTableColumnOptions  : TableColumnOptions
    tableHeadersRow        : ⦃ MonadIO M ⦄ → M ⊤′
    tableHeader            : ⦃ MonadIO M ⦄ → CString → M ⊤′
    tableSetupScrollFreeze : ⦃ MonadIO M ⦄ → Int → Int → M ⊤′

    -- Rows
    tableNextRow       : ⦃ MonadIO M ⦄ → M ⊤′
    tableNextRowWith   : ⦃ MonadIO M ⦄ → TableRowOptions → M ⊤′
    defTableRowOptions : TableRowOptions

    -- Columns
    tableNextColumn     : ⦃ MonadIO M ⦄ → M ⊤′ → M ⊤′
    tableSetColumnIndex : ⦃ MonadIO M ⦄ → Int → M (Liftℓ _ Bool)

    -- Sorting
    withSortableTable : ⦃ MonadIO M ⦄ → (Bool → List TableSortingSpecs → M ⊤) → M ⊤′

    -- Queries
    tableGetColumnCount   : ⦃ MonadIO M ⦄ → M (Liftℓ _ Int)
    tableGetColumnIndex   : ⦃ MonadIO M ⦄ → M (Liftℓ _ Int)
    tableGetRowIndex      : ⦃ MonadIO M ⦄ → M (Liftℓ _ Int)
    tableGetColumnName    : ⦃ MonadIO M ⦄ → Maybe Int → M (Liftℓ _ Text)
    tableGetColumnFlags   : ⦃ MonadIO M ⦄ → Maybe Int → M (Liftℓ _ ImGuiTableColumnFlags)
    tableSetColumnEnabled : ⦃ MonadIO M ⦄ → Int → Bool → M ⊤′
    tableSetBgColor       : ⦃ MonadIO M ⦄ → ImGuiTableBgTarget → ImU32 → Maybe Int → M ⊤′

    -- Trees
    treeNode : ⦃ MonadIO M ⦄ → Text → M (Liftℓ _ Bool)
    treePush : ⦃ MonadIO M ⦄ → Text → M ⊤′
    treePop  : ⦃ MonadIO M ⦄ → M ⊤′

    -- Selectables
    selectable           : ⦃ MonadIO M ⦄ → Text → M (Liftℓ _ Bool)
    selectableWith       : ⦃ MonadIO M ⦄ → SelectableOptions → Text → M (Liftℓ _ Bool)
    defSelectableOptions : SelectableOptions

    -- List Boxes
    listBox : ⦃ MonadIO M ⦄ → ⦃ HasGetter R Int ⦄ → ⦃ HasSetter R Int ⦄ → Text → R → List Text → M (Liftℓ _ Bool)

    -- Data Plotting
    plotHistogram : ⦃ MonadIO M ⦄ → Text → List CFloat → M ⊤′

    -- Menus
    withMenuBar         : ⦃ MonadUnliftIO M ⦄ → (Bool → M A) → M A
    withMenuBarOpen     : ⦃ MonadUnliftIO M ⦄ → M ⊤′ → M ⊤′
    beginMenuBar        : ⦃ MonadIO M ⦄ → M (Liftℓ _ Bool)
    endMenuBar          : ⦃ MonadIO M ⦄ → M ⊤′
    withMainMenuBar     : ⦃ MonadUnliftIO M ⦄ → (Bool → M A) → M A
    withMainMenuBarOpen : ⦃ MonadUnliftIO M ⦄ → M ⊤′ → M ⊤′
    beginMainMenuBar    : ⦃ MonadIO M ⦄ → M (Liftℓ _ Bool)
    endMainMenuBar      : ⦃ MonadIO M ⦄ → M ⊤′
    withMenu            : ⦃ MonadUnliftIO M ⦄ → Text → (Bool → M A) → M A
    withMenuOpen        : ⦃ MonadUnliftIO M ⦄ → Text → M ⊤′ → M ⊤′
    beginMenu           : ⦃ MonadIO M ⦄ → Text → M (Liftℓ _ Bool)
    endMenu             : ⦃ MonadIO M ⦄ → M ⊤′
    menuItem            : ⦃ MonadIO M ⦄ → Text → M (Liftℓ _ Bool)

    -- Tabs, tab bar
    withTabBar       : ⦃ MonadUnliftIO M ⦄ → Text → ImGuiTabBarFlags → (Bool → M A) → M A
    withTabBarOpen   : ⦃ MonadUnliftIO M ⦄ → Text → ImGuiTabBarFlags → M ⊤′ → M ⊤′
    beginTabBar      : ⦃ MonadIO M ⦄ → Text → ImGuiTabBarFlags → M (Liftℓ _ Bool)
    endTabBar        : ⦃ MonadIO M ⦄ → M ⊤′
    withTabItem      : ⦃ MonadUnliftIO M ⦄ → ⦃ HasGetter R Bool ⦄ → ⦃ HasSetter R Bool ⦄ → Text → R → ImGuiTabBarFlags → (Bool → M A) → M A
    withTabItemOpen  : ⦃ MonadUnliftIO M ⦄ → ⦃ HasGetter R Bool ⦄ → ⦃ HasSetter R Bool ⦄ → Text → R → ImGuiTabBarFlags → M ⊤′ → M ⊤′
    beginTabItem     : ⦃ MonadIO M ⦄ → ⦃ HasGetter R Bool ⦄ → ⦃ HasSetter R Bool ⦄ → Text → R → ImGuiTabBarFlags → M (Liftℓ _ Bool)
    endTabItem       : ⦃ MonadIO M ⦄ → M ⊤′
    tabItemButton    : ⦃ MonadIO M ⦄ → Text → ImGuiTabItemFlags → M (Liftℓ _ Bool)
    setTabItemClosed : ⦃ MonadIO M ⦄ → Text → M ⊤′

    -- Tooltips
    withTooltip  : ⦃ MonadUnliftIO M ⦄ → M A → M A
    beginTooltip : ⦃ MonadIO M ⦄ → M ⊤′
    endTooltip   : ⦃ MonadIO M ⦄ → M ⊤′

{-# COMPILE GHC text         = \ mℓ m AgdaMonadIO                    -> DearImGui.text         #-}
{-# COMPILE GHC textColored  = \ mℓ m rℓ r AgdaHasGetter AgdaMonadIO -> DearImGui.textColored  #-}
{-# COMPILE GHC textDisabled = \ mℓ m AgdaMonadIO                    -> DearImGui.textDisabled #-}
{-# COMPILE GHC textWrapped  = \ mℓ m AgdaMonadIO                    -> DearImGui.textWrapped  #-}
{-# COMPILE GHC labelText    = \ mℓ m AgdaMonadIO                    -> DearImGui.labelText    #-}
{-# COMPILE GHC bulletText   = \ mℓ m AgdaMonadIO                    -> DearImGui.bulletText   #-}

{-# COMPILE GHC button          = \ mℓ m AgdaMonadIO                                  -> DearImGui.button          #-}
{-# COMPILE GHC smallButton     = \ mℓ m AgdaMonadIO                                  -> DearImGui.smallButton     #-}
{-# COMPILE GHC invisibleButton = \ mℓ m AgdaMonadIO                                  -> DearImGui.invisibleButton #-}
{-# COMPILE GHC arrowButton     = \ mℓ m AgdaMonadIO                                  -> DearImGui.arrowButton     #-}
{-# COMPILE GHC image           = \ mℓ m AgdaMonadIO                                  -> DearImGui.image           #-}
{-# COMPILE GHC checkbox        = \ mℓ m rℓ r AgdaHasSetter AgdaHasGetter AgdaMonadIO -> DearImGui.checkbox        #-}
{-# COMPILE GHC progressBar     = \ mℓ m AgdaMonadIO                                  -> DearImGui.progressBar     #-}
{-# COMPILE GHC bullet          = \ mℓ m AgdaMonadIO                                  -> DearImGui.bullet          #-}

{-# COMPILE GHC withCombo     = \ mℓ m a AgdaMonadUnliftIO                          -> DearImGui.withCombo     #-}
{-# COMPILE GHC withComboOpen = \ mℓ m AgdaMonadUnliftIO                            -> DearImGui.withComboOpen #-}
{-# COMPILE GHC beginCombo    = \ mℓ m AgdaMonadIO                                  -> DearImGui.beginCombo    #-}
{-# COMPILE GHC endCombo      = \ mℓ m AgdaMonadIO                                  -> DearImGui.endCombo      #-}
{-# COMPILE GHC combo         = \ mℓ m rℓ r AgdaMonadIO AgdaHasGetter AgdaHasSetter -> DearImGui.combo         #-}

{-# COMPILE GHC dragFloat       = \ mℓ m rℓ r AgdaMonadIO AgdaHasGetter AgdaHasSetter -> DearImGui.dragFloat       #-}
{-# COMPILE GHC dragFloat2      = \ mℓ m rℓ r AgdaMonadIO AgdaHasGetter AgdaHasSetter -> DearImGui.dragFloat2      #-}
{-# COMPILE GHC dragFloat3      = \ mℓ m rℓ r AgdaMonadIO AgdaHasGetter AgdaHasSetter -> DearImGui.dragFloat3      #-}
{-# COMPILE GHC dragFloat4      = \ mℓ m rℓ r AgdaMonadIO AgdaHasGetter AgdaHasSetter -> DearImGui.dragFloat4      #-}
{-# COMPILE GHC dragFloatRange2 = \ mℓ m rℓ r AgdaMonadIO AgdaHasGetter AgdaHasSetter -> DearImGui.dragFloatRange2 #-}
{-# COMPILE GHC dragInt         = \ mℓ m rℓ r AgdaMonadIO AgdaHasGetter AgdaHasSetter -> DearImGui.dragInt         #-}
{-# COMPILE GHC dragInt2        = \ mℓ m rℓ r AgdaMonadIO AgdaHasGetter AgdaHasSetter -> DearImGui.dragInt2        #-}
{-# COMPILE GHC dragInt3        = \ mℓ m rℓ r AgdaMonadIO AgdaHasGetter AgdaHasSetter -> DearImGui.dragInt3        #-}
{-# COMPILE GHC dragInt4        = \ mℓ m rℓ r AgdaMonadIO AgdaHasGetter AgdaHasSetter -> DearImGui.dragInt4        #-}
{-# COMPILE GHC dragIntRange2   = \ mℓ m rℓ r AgdaMonadIO AgdaHasGetter AgdaHasSetter -> DearImGui.dragIntRange2   #-}
{-# COMPILE GHC dragScalar  = \ mℓ m rℓ r aℓ a rangeℓ range AgdaHasSetter AgdaHasGetter AgdaHasGetter AgdaStorable AgdaMonadIO -> DearImGui.dragScalar  #-}
{-# COMPILE GHC dragScalarN = \ mℓ m rℓ r aℓ a rangeℓ range AgdaHasSetter AgdaHasGetter AgdaHasGetter AgdaStorable AgdaMonadIO -> DearImGui.dragScalarN #-}

{-# COMPILE GHC sliderFloat   = \ mℓ m rℓ r AgdaMonadIO AgdaHasSetter AgdaHasGetter -> DearImGui.sliderFloat   #-}
{-# COMPILE GHC sliderFloat2  = \ mℓ m rℓ r AgdaMonadIO AgdaHasSetter AgdaHasGetter -> DearImGui.sliderFloat2  #-}
{-# COMPILE GHC sliderFloat3  = \ mℓ m rℓ r AgdaMonadIO AgdaHasSetter AgdaHasGetter -> DearImGui.sliderFloat3  #-}
{-# COMPILE GHC sliderFloat4  = \ mℓ m rℓ r AgdaMonadIO AgdaHasSetter AgdaHasGetter -> DearImGui.sliderFloat4  #-}
{-# COMPILE GHC sliderAngle   = \ mℓ m rℓ r AgdaMonadIO AgdaHasSetter AgdaHasGetter -> DearImGui.sliderAngle   #-}
{-# COMPILE GHC sliderInt     = \ mℓ m rℓ r AgdaMonadIO AgdaHasSetter AgdaHasGetter -> DearImGui.sliderInt     #-}
{-# COMPILE GHC sliderInt2    = \ mℓ m rℓ r AgdaMonadIO AgdaHasSetter AgdaHasGetter -> DearImGui.sliderInt2    #-}
{-# COMPILE GHC sliderInt3    = \ mℓ m rℓ r AgdaMonadIO AgdaHasSetter AgdaHasGetter -> DearImGui.sliderInt3    #-}
{-# COMPILE GHC sliderInt4    = \ mℓ m rℓ r AgdaMonadIO AgdaHasSetter AgdaHasGetter -> DearImGui.sliderInt4    #-}
{-# COMPILE GHC sliderScalar  = \ mℓ m rℓ r aℓ a rangeℓ range AgdaHasGetter AgdaHasSetter AgdaHasGetter AgdaStorable AgdaMonadIO -> DearImGui.sliderScalar #-}
{-# COMPILE GHC sliderScalarN = \ mℓ m rℓ r aℓ a valueℓ value rangeℓ range AgdaHasSetter AgdaHasGetter AgdaHasGetter AgdaStorable AgdaMonadIO -> DearImGui.sliderScalarN #-}
{-# COMPILE GHC vSliderFloat = \ mℓ m rℓ r AgdaHasSetter AgdaHasGetter AgdaMonadIO -> DearImGui.vSliderFloat #-}
{-# COMPILE GHC vSliderInt = \ mℓ m rℓ r AgdaHasSetter AgdaHasGetter AgdaMonadIO -> DearImGui.vSliderInt #-}
{-# COMPILE GHC vSliderScalar = \ mℓ m rℓ r aℓ a rangeℓ range AgdaHasSetter AgdaHasGetter AgdaHasGetter AgdaStorable AgdaMonadIO -> DearImGui.vSliderScalar #-}

{-# COMPILE GHC inputText          = \ mℓ m rℓ r AgdaMonadIO AgdaHasSetter AgdaHasGetter -> DearImGui.inputText          #-}
{-# COMPILE GHC inputTextMultiline = \ mℓ m rℓ r AgdaMonadIO AgdaHasSetter AgdaHasGetter -> DearImGui.inputTextMultiline #-}
{-# COMPILE GHC inputTextWithHint  = \ mℓ m rℓ r AgdaMonadIO AgdaHasSetter AgdaHasGetter -> DearImGui.inputTextWithHint  #-}

{-# COMPILE GHC colorPicker3 = \ mℓ m rℓ r AgdaMonadIO AgdaHasSetter AgdaHasGetter -> DearImGui.colorPicker3 #-}
{-# COMPILE GHC colorButton  = \ mℓ m rℓ r AgdaMonadIO AgdaHasSetter AgdaHasGetter -> DearImGui.colorButton  #-}

{-# COMPILE GHC withTable       = \ mℓ m a AgdaMonadUnliftIO -> DearImGui.withTable       #-}
{-# COMPILE GHC withTableOpen   = \ mℓ m AgdaMonadUnliftIO   -> DearImGui.withTableOpen   #-}
{-# COMPILE GHC defTableOptions =                               DearImGui.defTableOptions #-}
{-# COMPILE GHC beginTable      = \ mℓ m AgdaMonadIO         -> DearImGui.beginTable      #-}
{-# COMPILE GHC endTable        = \ mℓ m AgdaMonadIO         -> DearImGui.endTable        #-}

{-# COMPILE GHC tableSetupColumn       = \ mℓ m AgdaMonadIO -> DearImGui.tableSetupColumn       #-}
{-# COMPILE GHC tableSetupColumnWith   = \ mℓ m AgdaMonadIO -> DearImGui.tableSetupColumnWith   #-}
{-# COMPILE GHC defTableColumnOptions  =                       DearImGui.defTableColumnOptions  #-}
{-# COMPILE GHC tableHeadersRow        = \ mℓ m AgdaMonadIO -> DearImGui.tableHeadersRow        #-}
{-# COMPILE GHC tableHeader            = \ mℓ m AgdaMonadIO -> DearImGui.tableHeader            #-}
{-# COMPILE GHC tableSetupScrollFreeze = \ mℓ m AgdaMonadIO -> DearImGui.tableSetupScrollFreeze #-}

{-# COMPILE GHC tableNextRow       = \ mℓ m AgdaMonadIO -> DearImGui.tableNextRow       #-}
{-# COMPILE GHC tableNextRowWith   = \ mℓ m AgdaMonadIO -> DearImGui.tableNextRowWith   #-}
{-# COMPILE GHC defTableRowOptions =                       DearImGui.defTableRowOptions #-}

{-# COMPILE GHC tableNextColumn     = \ mℓ m AgdaMonadIO -> DearImGui.tableNextColumn     #-}
{-# COMPILE GHC tableSetColumnIndex = \ mℓ m AgdaMonadIO -> DearImGui.tableSetColumnIndex #-}

{-# COMPILE GHC withSortableTable = \ mℓ m AgdaMonadIO -> DearImGui.withSortableTable #-}

{-# COMPILE GHC tableGetColumnCount   = \ mℓ m AgdaMonadIO -> DearImGui.tableGetColumnCount   #-}
{-# COMPILE GHC tableGetColumnIndex   = \ mℓ m AgdaMonadIO -> DearImGui.tableGetColumnIndex   #-}
{-# COMPILE GHC tableGetRowIndex      = \ mℓ m AgdaMonadIO -> DearImGui.tableGetRowIndex      #-}
{-# COMPILE GHC tableGetColumnName    = \ mℓ m AgdaMonadIO -> DearImGui.tableGetColumnName    #-}
{-# COMPILE GHC tableGetColumnFlags   = \ mℓ m AgdaMonadIO -> DearImGui.tableGetColumnFlags   #-}
{-# COMPILE GHC tableSetColumnEnabled = \ mℓ m AgdaMonadIO -> DearImGui.tableSetColumnEnabled #-}
{-# COMPILE GHC tableSetBgColor       = \ mℓ m AgdaMonadIO -> DearImGui.tableSetBgColor       #-}

{-# COMPILE GHC treeNode = \ mℓ m AgdaMonadIO -> DearImGui.treeNode #-}
{-# COMPILE GHC treePush = \ mℓ m AgdaMonadIO -> DearImGui.treePush #-}
{-# COMPILE GHC treePop  = \ mℓ m AgdaMonadIO -> DearImGui.treePop  #-}

{-# COMPILE GHC selectable           = \ mℓ m AgdaMonadIO -> DearImGui.selectable           #-}
{-# COMPILE GHC selectableWith       = \ mℓ m AgdaMonadIO -> DearImGui.selectableWith       #-}
{-# COMPILE GHC defSelectableOptions =                       DearImGui.defSelectableOptions #-}

{-# COMPILE GHC listBox = \ mℓ m rℓ r AgdaMonadIO AgdaHasGetter AgdaHasSetter -> DearImGui.listBox #-}

{-# COMPILE GHC plotHistogram = \ mℓ m AgdaMonadIO -> DearImGui.plotHistogram #-}

{-# COMPILE GHC withMenuBar         = \ mℓ m a AgdaMonadUnliftIO -> DearImGui.withMenuBar         #-}
{-# COMPILE GHC withMenuBarOpen     = \ mℓ m AgdaMonadUnliftIO   -> DearImGui.withMenuBarOpen     #-}
{-# COMPILE GHC beginMenuBar        = \ mℓ m AgdaMonadIO         -> DearImGui.beginMenuBar        #-}
{-# COMPILE GHC endMenuBar          = \ mℓ m AgdaMonadIO         -> DearImGui.endMenuBar          #-}
{-# COMPILE GHC withMainMenuBar     = \ mℓ m a AgdaMonadUnliftIO -> DearImGui.withMainMenuBar     #-}
{-# COMPILE GHC withMainMenuBarOpen = \ mℓ m AgdaMonadUnliftIO   -> DearImGui.withMainMenuBarOpen #-}
{-# COMPILE GHC beginMainMenuBar    = \ mℓ m AgdaMonadIO         -> DearImGui.beginMainMenuBar    #-}
{-# COMPILE GHC endMainMenuBar      = \ mℓ m AgdaMonadIO         -> DearImGui.endMainMenuBar      #-}
{-# COMPILE GHC withMenu            = \ mℓ m a AgdaMonadUnliftIO -> DearImGui.withMenu            #-}
{-# COMPILE GHC withMenuOpen        = \ mℓ m AgdaMonadUnliftIO   -> DearImGui.withMenuOpen        #-}
{-# COMPILE GHC beginMenu           = \ mℓ m AgdaMonadIO         -> DearImGui.beginMenu           #-}
{-# COMPILE GHC endMenu             = \ mℓ m AgdaMonadIO         -> DearImGui.endMenu             #-}
{-# COMPILE GHC menuItem            = \ mℓ m AgdaMonadIO         -> DearImGui.menuItem            #-}

{-# COMPILE GHC withTabBar       = \ mℓ m a AgdaMonadUnliftIO                                  -> DearImGui.withTabBar       #-}
{-# COMPILE GHC withTabBarOpen   = \ mℓ m AgdaMonadUnliftIO                                    -> DearImGui.withTabBarOpen   #-}
{-# COMPILE GHC beginTabBar      = \ mℓ m AgdaMonadIO                                          -> DearImGui.beginTabBar      #-}
{-# COMPILE GHC endTabBar        = \ mℓ m AgdaMonadIO                                          -> DearImGui.endTabBar        #-}
{-# COMPILE GHC withTabItem      = \ mℓ m rℓ r a AgdaMonadUnliftIO AgdaHasGetter AgdaHasSetter -> DearImGui.withTabItem      #-}
{-# COMPILE GHC withTabItemOpen  = \ mℓ m rℓ r AgdaMonadUnliftIO AgdaHasGetter AgdaHasSetter   -> DearImGui.withTabItemOpen  #-}
{-# COMPILE GHC beginTabItem     = \ mℓ m rℓ r AgdaMonadIO AgdaHasGetter AgdaHasSetter         -> DearImGui.beginTabItem     #-}
{-# COMPILE GHC endTabItem       = \ mℓ m AgdaMonadIO                                          -> DearImGui.endTabItem       #-}
{-# COMPILE GHC tabItemButton    = \ mℓ m AgdaMonadIO                                          -> DearImGui.tabItemButton    #-}
{-# COMPILE GHC setTabItemClosed = \ mℓ m AgdaMonadIO                                          -> DearImGui.setTabItemClosed #-}

{-# COMPILE GHC withTooltip  = \ mℓ m a AgdaMonadUnliftIO -> DearImGui.withTooltip  #-}
{-# COMPILE GHC beginTooltip = \ mℓ m AgdaMonadIO         -> DearImGui.beginTooltip #-}
{-# COMPILE GHC endTooltip   = \ mℓ m AgdaMonadIO         -> DearImGui.endTooltip   #-}


-- Popups/Modals

postulate

    -- Generic
    withPopup     : ⦃ MonadUnliftIO M ⦄ → Text → (Bool → M A) → M A
    withPopupOpen : ⦃ MonadUnliftIO M ⦄ → Text → M ⊤′ → M ⊤′
    beginPopup    : ⦃ MonadIO M ⦄ → Text → M (Liftℓ _ Bool)
    endPopup      : ⦃ MonadIO M ⦄ → M ⊤′

    -- Modal
    withPopupModal     : ⦃ MonadUnliftIO M ⦄ → Text → (Bool → M A) → M A
    withPopupModalOpen : ⦃ MonadUnliftIO M ⦄ → Text → M ⊤′ → M ⊤′
    beginPopupModal    : ⦃ MonadIO M ⦄ → Text → M (Liftℓ _ Bool)

    -- Item context
    itemContextPopup         : ⦃ MonadUnliftIO M ⦄ → M ⊤′ → M ⊤′
    withPopupContextItemOpen : ⦃ MonadUnliftIO M ⦄ → Maybe Text → ImGuiPopupFlags → M ⊤′ → M ⊤′
    withPopupContextItem     : ⦃ MonadUnliftIO M ⦄ → Maybe Text → ImGuiPopupFlags → (Bool → M A) → M A
    beginPopupContextItem    : ⦃ MonadIO M ⦄ → Maybe Text → ImGuiPopupFlags → M (Liftℓ _ Bool)

    -- Window context
    windowContextPopup         : ⦃ MonadUnliftIO M ⦄ → M ⊤′ → M ⊤′
    withPopupContextWindowOpen : ⦃ MonadUnliftIO M ⦄ → Maybe Text → ImGuiPopupFlags → M ⊤′ → M ⊤′
    withPopupContextWindow     : ⦃ MonadUnliftIO M ⦄ → Maybe Text → ImGuiPopupFlags → (Bool → M A) → M A
    beginPopupContextWindow    : ⦃ MonadIO M ⦄ → Maybe Text → ImGuiPopupFlags → M (Liftℓ _ Bool)

    -- Void context
    voidContextPopup         : ⦃ MonadUnliftIO M ⦄ → M ⊤′ → M ⊤′
    withPopupContextVoidOpen : ⦃ MonadUnliftIO M ⦄ → Maybe Text → ImGuiPopupFlags → M ⊤′ → M ⊤′
    withPopupContextVoid     : ⦃ MonadUnliftIO M ⦄ → Maybe Text → ImGuiPopupFlags → (Bool → M A) → M A
    beginPopupContextVoid    : ⦃ MonadIO M ⦄ → Maybe Text → ImGuiPopupFlags → M (Liftℓ _ Bool)

    -- Manual
    openPopup            : ⦃ MonadIO M ⦄ → Text → M ⊤′
    openPopupOnItemClick : ⦃ MonadIO M ⦄ → Text → ImGuiPopupFlags → M ⊤′
    closeCurrentPopup    : ⦃ MonadIO M ⦄ → M ⊤′

    -- Queries
    isCurrentPopupOpen  : ⦃ MonadIO M ⦄ → Text → M (Liftℓ _ Bool)
    isAnyPopupOpen      : ⦃ MonadIO M ⦄ → Text → M (Liftℓ _ Bool)
    isAnyLevelPopupOpen : ⦃ MonadIO M ⦄ → Text → M (Liftℓ _ Bool)

{-# COMPILE GHC withPopup     = \ mℓ m a AgdaMonadUnliftIO -> DearImGui.withPopup     #-}
{-# COMPILE GHC withPopupOpen = \ mℓ m AgdaMonadUnliftIO   -> DearImGui.withPopupOpen #-}
{-# COMPILE GHC beginPopup    = \ mℓ m AgdaMonadIO         -> DearImGui.beginPopup    #-}
{-# COMPILE GHC endPopup      = \ mℓ m AgdaMonadIO         -> DearImGui.endPopup      #-}

{-# COMPILE GHC withPopupModal     = \ mℓ m a AgdaMonadUnliftIO -> DearImGui.withPopupModal     #-}
{-# COMPILE GHC withPopupModalOpen = \ mℓ m AgdaMonadUnliftIO   -> DearImGui.withPopupModalOpen #-}
{-# COMPILE GHC beginPopupModal    = \ mℓ m AgdaMonadIO         -> DearImGui.beginPopupModal    #-}

{-# COMPILE GHC itemContextPopup         = \ mℓ m AgdaMonadUnliftIO   -> DearImGui.itemContextPopup         #-}
{-# COMPILE GHC withPopupContextItemOpen = \ mℓ m AgdaMonadUnliftIO   -> DearImGui.withPopupContextItemOpen #-}
{-# COMPILE GHC withPopupContextItem     = \ mℓ m a AgdaMonadUnliftIO -> DearImGui.withPopupContextItem     #-}
{-# COMPILE GHC beginPopupContextItem    = \ mℓ m AgdaMonadIO         -> DearImGui.beginPopupContextItem    #-}

{-# COMPILE GHC windowContextPopup         = \ mℓ m AgdaMonadUnliftIO   -> DearImGui.windowContextPopup         #-}
{-# COMPILE GHC withPopupContextWindowOpen = \ mℓ m AgdaMonadUnliftIO   -> DearImGui.withPopupContextWindowOpen #-}
{-# COMPILE GHC withPopupContextWindow     = \ mℓ m a AgdaMonadUnliftIO -> DearImGui.withPopupContextWindow     #-}
{-# COMPILE GHC beginPopupContextWindow    = \ mℓ m AgdaMonadIO         -> DearImGui.beginPopupContextWindow    #-}

{-# COMPILE GHC voidContextPopup         = \ mℓ m AgdaMonadUnliftIO   -> DearImGui.voidContextPopup         #-}
{-# COMPILE GHC withPopupContextVoidOpen = \ mℓ m AgdaMonadUnliftIO   -> DearImGui.withPopupContextVoidOpen #-}
{-# COMPILE GHC withPopupContextVoid     = \ mℓ m a AgdaMonadUnliftIO -> DearImGui.withPopupContextVoid     #-}
{-# COMPILE GHC beginPopupContextVoid    = \ mℓ m AgdaMonadIO         -> DearImGui.beginPopupContextVoid    #-}

{-# COMPILE GHC openPopup            = \ mℓ m AgdaMonadIO -> DearImGui.openPopup            #-}
{-# COMPILE GHC openPopupOnItemClick = \ mℓ m AgdaMonadIO -> DearImGui.openPopupOnItemClick #-}
{-# COMPILE GHC closeCurrentPopup    = \ mℓ m AgdaMonadIO -> DearImGui.closeCurrentPopup    #-}

{-# COMPILE GHC isCurrentPopupOpen  = \ mℓ m AgdaMonadIO -> DearImGui.isCurrentPopupOpen  #-}
{-# COMPILE GHC isAnyPopupOpen      = \ mℓ m AgdaMonadIO -> DearImGui.isAnyPopupOpen      #-}
{-# COMPILE GHC isAnyLevelPopupOpen = \ mℓ m AgdaMonadIO -> DearImGui.isAnyLevelPopupOpen #-}


-- Item/Widgets Utilities

postulate
    isItemHovered       : ⦃ MonadIO M ⦄ → M (Liftℓ _ Bool)
    wantCaptureMouse    : ⦃ MonadIO M ⦄ → M (Liftℓ _ Bool)
    wantCaptureKeyboard : ⦃ MonadIO M ⦄ → M (Liftℓ _ Bool)

{-# COMPILE GHC isItemHovered       = \ mℓ m AgdaMonadIO -> DearImGui.isItemHovered       #-}
{-# COMPILE GHC wantCaptureMouse    = \ mℓ m AgdaMonadIO -> DearImGui.wantCaptureMouse    #-}
{-# COMPILE GHC wantCaptureKeyboard = \ mℓ m AgdaMonadIO -> DearImGui.wantCaptureKeyboard #-}

-- Utilities

data ClipRange (A : Set aℓ) : Set aℓ where
    mkClipRange : A → A → ClipRange A

{-# FOREIGN GHC type AgdaClipRange aℓ = DearImGui.ClipRange #-}
{-# COMPILE GHC ClipRange = data(1) AgdaClipRange (DearImGui.ClipRange) #-}

postulate
    Show[ClipRange[A]] : ⦃ Show A ⦄ → Show (ClipRange A)
    Eq[ClipRange[A]]   : ⦃ Eq A ⦄ → Eq (ClipRange A)
    Ord[ClipRange[A]]  : ⦃ Ord A ⦄ → Ord (ClipRange A)

{-# COMPILE GHC Show[ClipRange[A]] = \ aℓ a AgdaShow -> AgdaShow #-}
{-# COMPILE GHC Eq[ClipRange[A]]   = \ aℓ a AgdaEq   -> AgdaEq   #-}
{-# COMPILE GHC Ord[ClipRange[A]]  = \ aℓ a AgdaOrd  -> AgdaOrd  #-}

postulate
    ClipItems : (Set aℓ → Set bℓ) → Set aℓ → Set (aℓ ⊔ bℓ)

    ClipItems[ClipRange,A] : ⦃ Ord A ⦄ → ⦃ Enum A ⦄ → ⦃ Num A ⦄ → ClipItems ClipRange A
    -- todo: ClipItems[Vector,A] : ClipItems Vector A
    ClipItems[SVector,A]   : ⦃ Storable A ⦄ → ClipItems SVector A
    -- todo: ClipItems[UVector,A] : ⦃ Unbox A ⦄ → ClipItems UVector A
    ClipItems[List,A]      : ClipItems List A

{-# FOREIGN GHC data AgdaClipItems aℓ bℓ t a = DearImGui.ClipItems t a => AgdaClipItems #-}
{-# COMPILE GHC ClipItems = type(0) AgdaClipItems #-}

{-# COMPILE GHC ClipItems[ClipRange,A] = \ aℓ a AgdaOrd AgdaEnum AgdaNum -> AgdaClipItems #-}
{-# COMPILE GHC ClipItems[SVector,A]   = \ aℓ a AgdaStorable             -> AgdaClipItems #-}
{-# COMPILE GHC ClipItems[List,A]      = \ aℓ a                          -> AgdaClipItems #-}

module _ {T : Set aℓ → Set bℓ} where
    postulate
        itemCount : ⦃ ClipItems T A ⦄ → T A → Maybe Int
        clipItems : ⦃ ClipItems T A ⦄ → Int → Int → T A → T A
        stepItems : ⦃ ClipItems T A ⦄ → ⦃ Monad M ⦄ → (A → M ⊤) → T A → M ⊤′

{-# COMPILE GHC itemCount = \ aℓ bℓ t a AgdaClipItems -> DearImGui.itemCount #-}
{-# COMPILE GHC clipItems = \ aℓ bℓ t a AgdaClipItems -> DearImGui.clipItems #-}
{-# COMPILE GHC stepItems = \ aℓ bℓ t a AgdaClipItems -> DearImGui.stepItems #-}

postulate

    -- ListClipper
    withListClipper : {T : Set aℓ → Set bℓ} → ⦃ ClipItems T A ⦄ → ⦃ MonadUnliftIO M ⦄ → Maybe Float → T A → (A → M ⊤) → M ⊤′

    -- Miscellaneous
    -- todo: (req raw DrawList) getBackgroundDrawList : ⦃ MonadIO M ⦄ → M (Liftℓ _ DrawList)
    -- todo: (req raw DrawList) getForegroundDrawList : ⦃ MonadIO M ⦄ → M (Liftℓ _ DrawList)
    imCol32 : CUChar → CUChar → CUChar → CUChar → ImU32

{-# COMPILE GHC withListClipper = \ aℓ bℓ a t AgdaClipItems AgdaMonadUnliftIO -> DearImGui.withListClipper #-}

{-# COMPILE GHC imCol32 = DearImGui.imCol32 #-}
