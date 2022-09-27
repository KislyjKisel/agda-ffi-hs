{-# OPTIONS --without-K #-}

module Ffi.Hs.DearImGui where

open import Agda.Builtin.Bool              using (Bool)
open import Agda.Builtin.Int               using () renaming (Int to Integer)
open import Agda.Builtin.List              using (List)
open import Agda.Builtin.Maybe             using (Maybe)
open import Agda.Primitive
open import Ffi.Hs.-base.Class
open import Ffi.Hs.-base.Level             using (Liftℓ)
open import Ffi.Hs.-base.Unit              using (⊤; ⊤′)
open import Ffi.Hs.Control.Monad.IO.Unlift using (MonadUnliftIO)
open import Ffi.Hs.Data.Int                using (Int)
open import Ffi.Hs.Data.StateVar           using (HasGetter; HasSetter)
open import Ffi.Hs.Data.Text               using (Text)
open import Ffi.Hs.Data.Tuple              using (Tuple2; Tuple3; Tuple4)
open import Ffi.Hs.Data.Vector.Storable    using () renaming (Vector to SVector)
open import Ffi.Hs.Foreign.C.Types         using (CInt; CFloat; CBool; CChar; CUChar)
open import Ffi.Hs.Foreign.Ptr             using (Ptr)
open import Ffi.Hs.GHC.Float               using (Float)

open import Ffi.Hs.-dear-imgui.Types public

open import Ffi.Hs.DearImGui.Raw public
    using
    ( Context
    ; mkContext
    ; createContext
    ; destroyContext
    ; getCurrentContext
    ; setCurrentContext

    ; newFrame
    ; endFrame
    ; render
    ; DrawData
    ; mkDrawData
    ; getDrawData
    ; checkVersion

    ; showDemoWindow
    ; showMetricsWindow
    ; showAboutWindow
    ; showUserGuide

    ; styleColorsDark
    ; styleColorsLight
    ; styleColorsClassic

    ; end

    ; getWindowDrawList
    ; getWindowPos
    ; getWindowSize
    ; getWindowWidth
    ; getWindowHeight

    ; setNextWindowFullscreen

    ; endChild

    ; popStyleColor

    ; separator
    ; sameLine
    ; newLine
    ; spacing

    ; popItemWidth

    ; beginGroup
    ; endGroup

    ; alignTextToFramePadding

    ; image
    ; bullet

    ; endCombo

    ; endTable

    ; tableHeadersRow
    ; tableHeader

    ; treePop

    ; beginMenuBar
    ; endMenuBar

    ; beginMainMenuBar
    ; endMainMenuBar

    ; endMenu

    ; endTabBar

    ; endTabItem

    ; beginTooltip
    ; endTooltip

    ; endPopup

    ; closeCurrentPopup

    ; isItemHovered
    ; wantCaptureKeyboard
    ; wantCaptureMouse

    ; getBackgroundDrawList
    ; getForegroundDrawList
    ; imCol32
    )

open import Ffi.Hs.DearImGui.Raw.Font public
    using
    ( pushFont
    ; popFont
    ; Font
    )

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


-- Windows

postulate
    withWindow      : ⦃ MonadUnliftIO M ⦄ → Text → (Bool → M A) → M A
    withWindowOpen  : ⦃ MonadUnliftIO M ⦄ → Text → M ⊤′ → M ⊤′
    withFullscreen  : ⦃ MonadUnliftIO M ⦄ → M ⊤′ → M ⊤′
    fullscreenFlags : ImGuiWindowFlags
    begin           : ⦃ MonadIO M ⦄ → Text → M (Liftℓ _ Bool)

{-# COMPILE GHC withWindow      = \ mℓ m a AgdaMonadUnliftIO -> DearImGui.withWindow      #-}
{-# COMPILE GHC withWindowOpen  = \ mℓ m AgdaMonadUnliftIO   -> DearImGui.withWindowOpen  #-}
{-# COMPILE GHC withFullscreen  = \ mℓ m AgdaMonadUnliftIO   -> DearImGui.withFullscreen  #-}
{-# COMPILE GHC fullscreenFlags =                               DearImGui.fullscreenFlags #-}
{-# COMPILE GHC begin           = \ mℓ m AgdaMonadIO         -> DearImGui.begin           #-}


-- Manipulation

postulate
    setNextWindowPos             : ⦃ MonadIO M ⦄ → ⦃ HasGetter R ImVec2 ⦄ → R → ImGuiCond → Maybe R → M ⊤′
    setNextWindowSize            : ⦃ MonadIO M ⦄ → ⦃ HasGetter R ImVec2 ⦄ → R → ImGuiCond → M ⊤′
    setNextWindowContentSize     : ⦃ MonadIO M ⦄ → ⦃ HasGetter R ImVec2 ⦄ → R → M ⊤′
    setNextWindowSizeConstraints : ⦃ MonadIO M ⦄ → ⦃ HasGetter R ImVec2 ⦄ → R → R → M ⊤′
    setNextWindowCollapsed       : ⦃ MonadIO M ⦄ → Bool → ImGuiCond → M ⊤′
    setNextWindowBgAlpha         : ⦃ MonadIO M ⦄ → Float → M ⊤′

{-# COMPILE GHC setNextWindowPos             = \ mℓ m rℓ r AgdaMonadIO AgdaHasGetter -> DearImGui.setNextWindowPos             #-}
{-# COMPILE GHC setNextWindowSize            = \ mℓ m rℓ r AgdaMonadIO AgdaHasGetter -> DearImGui.setNextWindowSize            #-}
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

{-# COMPILE GHC withChild        = \ mℓ m a AgdaMonadUnliftIO -> DearImGui.withChild        #-}
{-# COMPILE GHC withChildOpen    = \ mℓ m AgdaMonadUnliftIO   -> DearImGui.withChildOpen    #-}
{-# COMPILE GHC withChildContext = \ mℓ m a AgdaMonadUnliftIO -> DearImGui.withChildContext #-}
{-# COMPILE GHC beginChild       = \ mℓ m AgdaMonadIO         -> DearImGui.beginChild       #-}


-- Parameter stacks

postulate
    withStyleColor : ⦃ MonadUnliftIO M ⦄ → ⦃ HasGetter R ImVec4 ⦄ → ImGuiCol → R → M A → M A
    pushStyleColor : ⦃ MonadIO M ⦄ → ⦃ HasGetter R ImVec4 ⦄ → ImGuiCol → R → M ⊤′
    withStyleVar   : ⦃ MonadUnliftIO M ⦄ → ⦃ HasGetter R ImVec2 ⦄ → ImGuiStyleVar → R → M A → M A
    pushStyleVar   : ⦃ MonadIO M ⦄ → ⦃ HasGetter R ImVec2 ⦄ → ImGuiStyleVar → R → M ⊤′
    popStyleVar    : ⦃ MonadIO M ⦄ → Int → M ⊤′
    withFont       : ⦃ MonadUnliftIO M ⦄ → Font → M A → M A

{-# COMPILE GHC withStyleColor = \ mℓ m rℓ r a AgdaMonadUnliftIO AgdaHasGetter -> DearImGui.withStyleColor #-}
{-# COMPILE GHC pushStyleColor = \ mℓ m rℓ r AgdaMonadIO AgdaHasGetter         -> DearImGui.pushStyleColor #-}
{-# COMPILE GHC withStyleVar   = \ mℓ m rℓ r a AgdaMonadUnliftIO AgdaHasGetter -> DearImGui.withStyleVar   #-}
{-# COMPILE GHC pushStyleVar   = \ mℓ m rℓ r AgdaMonadIO AgdaHasGetter         -> DearImGui.pushStyleVar   #-}
{-# COMPILE GHC popStyleVar    = \ mℓ m AgdaMonadIO                            -> DearImGui.popStyleVar    #-}
{-# COMPILE GHC withFont       = \ mℓ m a AgdaMonadUnliftIO                    -> DearImGui.withFont       #-}

-- Cursor/Layout

postulate
    dummy                   : ⦃ MonadIO M ⦄ → ⦃ HasGetter R ImVec2 ⦄ → R → M ⊤′
    withIndent              : ⦃ MonadUnliftIO M ⦄ → Float → M A → M A
    indent                  : ⦃ MonadIO M ⦄ → Float → M ⊤′
    unindent                : ⦃ MonadIO M ⦄ → Float → M ⊤′
    setNextItemWidth        : ⦃ MonadIO M ⦄ → Float → M ⊤′
    withItemWidth           : ⦃ MonadUnliftIO M ⦄ → Float → M A → M A
    pushItemWidth           : ⦃ MonadIO M ⦄ → Float → M ⊤′
    withGroup               : ⦃ MonadUnliftIO M ⦄ → M A → M A
    setCursorPos            : ⦃ MonadIO M ⦄ → ⦃ HasGetter R ImVec2 ⦄ → R → M ⊤′

{-# COMPILE GHC dummy                   = \ mℓ m rℓ r AgdaMonadIO AgdaHasGetter -> DearImGui.dummy                   #-}
{-# COMPILE GHC withIndent              = \ mℓ m a AgdaMonadUnliftIO            -> DearImGui.withIndent              #-}
{-# COMPILE GHC indent                  = \ mℓ m AgdaMonadIO                    -> DearImGui.indent                  #-}
{-# COMPILE GHC unindent                = \ mℓ m AgdaMonadIO                    -> DearImGui.unindent                #-}
{-# COMPILE GHC setNextItemWidth        = \ mℓ m AgdaMonadIO                    -> DearImGui.setNextItemWidth        #-}
{-# COMPILE GHC withItemWidth           = \ mℓ m a AgdaMonadUnliftIO            -> DearImGui.withItemWidth           #-}
{-# COMPILE GHC pushItemWidth           = \ mℓ m AgdaMonadIO                    -> DearImGui.pushItemWidth           #-}
{-# COMPILE GHC withGroup               = \ mℓ m a AgdaMonadUnliftIO            -> DearImGui.withGroup               #-}
{-# COMPILE GHC setCursorPos            = \ mℓ m rℓ r AgdaMonadIO AgdaHasGetter -> DearImGui.setCursorPos            #-}


-- ID stack

postulate
    ToID : Set aℓ → Set aℓ

    ToID[CInt]                   : ToID CInt
    ToID[Text]                   : ToID Text
    ToID[Integer]                : ToID Integer
    ToID[Int]                    : ToID Int
    ToID[Ptr[CChar]]             : ToID (Ptr CChar)
    -- todo: (overlaps, but should be ok?) ToID[Ptr[A]]                 : ToID (Ptr A)
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
-- {-# COMPILE GHC ToID[Ptr[A]]                 = \ aℓ a -> AgdaToID #-}
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
    checkbox        : ⦃ HasSetter R Bool ⦄ → ⦃ HasGetter R Bool ⦄ → ⦃ MonadIO M ⦄ → Text → R → M (Liftℓ _ Bool)
    progressBar     : ⦃ MonadIO M ⦄ → Float → Maybe Text → M ⊤′

    -- Combo Box
    withCombo     : ⦃ MonadUnliftIO M ⦄ → Text → Text → (Bool → M A) → M A
    withComboOpen : ⦃ MonadUnliftIO M ⦄ → Text → Text → M ⊤′ → M ⊤′
    beginCombo    : ⦃ MonadIO M ⦄ → Text → Text → M (Liftℓ _ Bool)
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

    -- Setup
    tableSetupColumn       : ⦃ MonadIO M ⦄ → Text → M ⊤′
    tableSetupColumnWith   : ⦃ MonadIO M ⦄ → TableColumnOptions → Text → M ⊤′
    defTableColumnOptions  : TableColumnOptions
    tableSetupScrollFreeze : ⦃ MonadIO M ⦄ → Int → Int → M ⊤′

    -- Rows
    tableNextRow       : ⦃ MonadIO M ⦄ → M ⊤′
    tableNextRowWith   : ⦃ MonadIO M ⦄ → TableRowOptions → M ⊤′
    defTableRowOptions : TableRowOptions

    -- Columns
    tableNextColumn     : ⦃ MonadIO M ⦄ → M ⊤′ → M ⊤′
    tableSetColumnIndex : ⦃ MonadIO M ⦄ → Int → M (Liftℓ _ Bool)

    -- Sorting
    withSortableTable : ⦃ MonadIO M ⦄ → (Bool → List TableSortingSpecs → M ⊤′) → M ⊤′

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
    withMainMenuBar     : ⦃ MonadUnliftIO M ⦄ → (Bool → M A) → M A
    withMainMenuBarOpen : ⦃ MonadUnliftIO M ⦄ → M ⊤′ → M ⊤′
    withMenu            : ⦃ MonadUnliftIO M ⦄ → Text → (Bool → M A) → M A
    withMenuOpen        : ⦃ MonadUnliftIO M ⦄ → Text → M ⊤′ → M ⊤′
    beginMenu           : ⦃ MonadIO M ⦄ → Text → M (Liftℓ _ Bool)
    menuItem            : ⦃ MonadIO M ⦄ → Text → M (Liftℓ _ Bool)

    -- Tabs, tab bar
    withTabBar       : ⦃ MonadUnliftIO M ⦄ → Text → ImGuiTabBarFlags → (Bool → M A) → M A
    withTabBarOpen   : ⦃ MonadUnliftIO M ⦄ → Text → ImGuiTabBarFlags → M ⊤′ → M ⊤′
    beginTabBar      : ⦃ MonadIO M ⦄ → Text → ImGuiTabBarFlags → M (Liftℓ _ Bool)
    withTabItem      : ⦃ MonadUnliftIO M ⦄ → ⦃ HasGetter R Bool ⦄ → ⦃ HasSetter R Bool ⦄ → Text → R → ImGuiTabBarFlags → (Bool → M A) → M A
    withTabItemOpen  : ⦃ MonadUnliftIO M ⦄ → ⦃ HasGetter R Bool ⦄ → ⦃ HasSetter R Bool ⦄ → Text → R → ImGuiTabBarFlags → M ⊤′ → M ⊤′
    beginTabItem     : ⦃ MonadIO M ⦄ → ⦃ HasGetter R Bool ⦄ → ⦃ HasSetter R Bool ⦄ → Text → R → ImGuiTabBarFlags → M (Liftℓ _ Bool)
    tabItemButton    : ⦃ MonadIO M ⦄ → Text → ImGuiTabItemFlags → M (Liftℓ _ Bool)
    setTabItemClosed : ⦃ MonadIO M ⦄ → Text → M ⊤′

    -- Tooltips
    withTooltip  : ⦃ MonadUnliftIO M ⦄ → M A → M A

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
{-# COMPILE GHC checkbox        = \ mℓ m rℓ r AgdaHasSetter AgdaHasGetter AgdaMonadIO -> DearImGui.checkbox        #-}
{-# COMPILE GHC progressBar     = \ mℓ m AgdaMonadIO                                  -> DearImGui.progressBar     #-}

{-# COMPILE GHC withCombo     = \ mℓ m a AgdaMonadUnliftIO                          -> DearImGui.withCombo     #-}
{-# COMPILE GHC withComboOpen = \ mℓ m AgdaMonadUnliftIO                            -> DearImGui.withComboOpen #-}
{-# COMPILE GHC beginCombo    = \ mℓ m AgdaMonadIO                                  -> DearImGui.beginCombo    #-}
{-# COMPILE GHC combo         = \ mℓ m rℓ r AgdaMonadIO AgdaHasGetter AgdaHasSetter -> DearImGui.combo         #-}

{-# COMPILE GHC dragFloat       = \ mℓ m rℓ r AgdaMonadIO AgdaHasSetter AgdaHasGetter -> DearImGui.dragFloat       #-}
{-# COMPILE GHC dragFloat2      = \ mℓ m rℓ r AgdaMonadIO AgdaHasSetter AgdaHasGetter -> DearImGui.dragFloat2      #-}
{-# COMPILE GHC dragFloat3      = \ mℓ m rℓ r AgdaMonadIO AgdaHasSetter AgdaHasGetter -> DearImGui.dragFloat3      #-}
{-# COMPILE GHC dragFloat4      = \ mℓ m rℓ r AgdaMonadIO AgdaHasSetter AgdaHasGetter -> DearImGui.dragFloat4      #-}
{-# COMPILE GHC dragFloatRange2 = \ mℓ m rℓ r AgdaMonadIO AgdaHasSetter AgdaHasGetter -> DearImGui.dragFloatRange2 #-}
{-# COMPILE GHC dragInt         = \ mℓ m rℓ r AgdaMonadIO AgdaHasSetter AgdaHasGetter -> DearImGui.dragInt         #-}
{-# COMPILE GHC dragInt2        = \ mℓ m rℓ r AgdaMonadIO AgdaHasSetter AgdaHasGetter -> DearImGui.dragInt2        #-}
{-# COMPILE GHC dragInt3        = \ mℓ m rℓ r AgdaMonadIO AgdaHasSetter AgdaHasGetter -> DearImGui.dragInt3        #-}
{-# COMPILE GHC dragInt4        = \ mℓ m rℓ r AgdaMonadIO AgdaHasSetter AgdaHasGetter -> DearImGui.dragInt4        #-}
{-# COMPILE GHC dragIntRange2   = \ mℓ m rℓ r AgdaMonadIO AgdaHasSetter AgdaHasGetter -> DearImGui.dragIntRange2   #-}
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
{-# COMPILE GHC sliderScalarN = \ mℓ m aℓ a valueℓ value rangeℓ range AgdaHasSetter AgdaHasGetter AgdaHasGetter AgdaStorable AgdaMonadIO -> DearImGui.sliderScalarN #-}
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

{-# COMPILE GHC tableSetupColumn       = \ mℓ m AgdaMonadIO -> DearImGui.tableSetupColumn       #-}
{-# COMPILE GHC tableSetupColumnWith   = \ mℓ m AgdaMonadIO -> DearImGui.tableSetupColumnWith   #-}
{-# COMPILE GHC defTableColumnOptions  =                       DearImGui.defTableColumnOptions  #-}
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

{-# COMPILE GHC selectable           = \ mℓ m AgdaMonadIO -> DearImGui.selectable           #-}
{-# COMPILE GHC selectableWith       = \ mℓ m AgdaMonadIO -> DearImGui.selectableWith       #-}
{-# COMPILE GHC defSelectableOptions =                       DearImGui.defSelectableOptions #-}

{-# COMPILE GHC listBox = \ mℓ m rℓ r AgdaMonadIO AgdaHasGetter AgdaHasSetter -> DearImGui.listBox #-}

{-# COMPILE GHC plotHistogram = \ mℓ m AgdaMonadIO -> DearImGui.plotHistogram #-}

{-# COMPILE GHC withMenuBar         = \ mℓ m a AgdaMonadUnliftIO -> DearImGui.withMenuBar         #-}
{-# COMPILE GHC withMenuBarOpen     = \ mℓ m AgdaMonadUnliftIO   -> DearImGui.withMenuBarOpen     #-}
{-# COMPILE GHC withMainMenuBar     = \ mℓ m a AgdaMonadUnliftIO -> DearImGui.withMainMenuBar     #-}
{-# COMPILE GHC withMainMenuBarOpen = \ mℓ m AgdaMonadUnliftIO   -> DearImGui.withMainMenuBarOpen #-}
{-# COMPILE GHC withMenu            = \ mℓ m a AgdaMonadUnliftIO -> DearImGui.withMenu            #-}
{-# COMPILE GHC withMenuOpen        = \ mℓ m AgdaMonadUnliftIO   -> DearImGui.withMenuOpen        #-}
{-# COMPILE GHC beginMenu           = \ mℓ m AgdaMonadIO         -> DearImGui.beginMenu           #-}
{-# COMPILE GHC menuItem            = \ mℓ m AgdaMonadIO         -> DearImGui.menuItem            #-}

{-# COMPILE GHC withTabBar       = \ mℓ m a AgdaMonadUnliftIO                                  -> DearImGui.withTabBar       #-}
{-# COMPILE GHC withTabBarOpen   = \ mℓ m AgdaMonadUnliftIO                                    -> DearImGui.withTabBarOpen   #-}
{-# COMPILE GHC beginTabBar      = \ mℓ m AgdaMonadIO                                          -> DearImGui.beginTabBar      #-}
{-# COMPILE GHC withTabItem      = \ mℓ m rℓ r a AgdaMonadUnliftIO AgdaHasGetter AgdaHasSetter -> DearImGui.withTabItem      #-}
{-# COMPILE GHC withTabItemOpen  = \ mℓ m rℓ r AgdaMonadUnliftIO AgdaHasGetter AgdaHasSetter   -> DearImGui.withTabItemOpen  #-}
{-# COMPILE GHC beginTabItem     = \ mℓ m rℓ r AgdaMonadIO AgdaHasGetter AgdaHasSetter         -> DearImGui.beginTabItem     #-}
{-# COMPILE GHC tabItemButton    = \ mℓ m AgdaMonadIO                                          -> DearImGui.tabItemButton    #-}
{-# COMPILE GHC setTabItemClosed = \ mℓ m AgdaMonadIO                                          -> DearImGui.setTabItemClosed #-}

{-# COMPILE GHC withTooltip  = \ mℓ m a AgdaMonadUnliftIO -> DearImGui.withTooltip  #-}


-- Popups/Modals

postulate

    -- Generic
    withPopup     : ⦃ MonadUnliftIO M ⦄ → Text → (Bool → M A) → M A
    withPopupOpen : ⦃ MonadUnliftIO M ⦄ → Text → M ⊤′ → M ⊤′
    beginPopup    : ⦃ MonadIO M ⦄ → Text → M (Liftℓ _ Bool)

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

    -- Queries
    isCurrentPopupOpen  : ⦃ MonadIO M ⦄ → Text → M (Liftℓ _ Bool)
    isAnyPopupOpen      : ⦃ MonadIO M ⦄ → Text → M (Liftℓ _ Bool)
    isAnyLevelPopupOpen : ⦃ MonadIO M ⦄ → Text → M (Liftℓ _ Bool)

{-# COMPILE GHC withPopup     = \ mℓ m a AgdaMonadUnliftIO -> DearImGui.withPopup     #-}
{-# COMPILE GHC withPopupOpen = \ mℓ m AgdaMonadUnliftIO   -> DearImGui.withPopupOpen #-}
{-# COMPILE GHC beginPopup    = \ mℓ m AgdaMonadIO         -> DearImGui.beginPopup    #-}

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

{-# COMPILE GHC isCurrentPopupOpen  = \ mℓ m AgdaMonadIO -> DearImGui.isCurrentPopupOpen  #-}
{-# COMPILE GHC isAnyPopupOpen      = \ mℓ m AgdaMonadIO -> DearImGui.isAnyPopupOpen      #-}
{-# COMPILE GHC isAnyLevelPopupOpen = \ mℓ m AgdaMonadIO -> DearImGui.isAnyLevelPopupOpen #-}


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
        stepItems : ⦃ ClipItems T A ⦄ → ⦃ Monad M ⦄ → (A → M ⊤′) → T A → M ⊤′

{-# COMPILE GHC itemCount = \ aℓ bℓ t a      AgdaClipItems           -> DearImGui.itemCount #-}
{-# COMPILE GHC clipItems = \ aℓ bℓ t a      AgdaClipItems           -> DearImGui.clipItems #-}
{-# COMPILE GHC stepItems = \ aℓ bℓ t a mℓ m AgdaClipItems AgdaMonad -> DearImGui.stepItems #-}

postulate

    -- ListClipper
    withListClipper : {T : Set aℓ → Set bℓ} → ⦃ ClipItems T A ⦄ → ⦃ MonadUnliftIO M ⦄ → Maybe Float → T A → (A → M ⊤′) → M ⊤′

{-# COMPILE GHC withListClipper = \ aℓ bℓ a t mℓ m AgdaClipItems AgdaMonadUnliftIO -> DearImGui.withListClipper #-}
