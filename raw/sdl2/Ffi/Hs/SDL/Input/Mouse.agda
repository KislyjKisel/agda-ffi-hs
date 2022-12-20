{-# OPTIONS --without-K #-}

module Ffi.Hs.SDL.Input.Mouse where

open import Agda.Builtin.Bool           using (Bool)
open import Agda.Primitive
open import Ffi.Hs.-base.Class
open import Ffi.Hs.-base.Level          using (Liftℓ)
open import Ffi.Hs.-base.Unit           using (⊤′)
open import Ffi.Hs.Data.Int             using (Int)
open import Ffi.Hs.Data.StateVar        using (StateVar)
open import Ffi.Hs.Data.Vector.Storable using (Vector)
open import Ffi.Hs.Data.Word            using (Word8)
open import Ffi.Hs.Foreign.C.Types      using (CInt)
open import Ffi.Hs.SDL.Internal.Types   using (Window)
open import Ffi.Hs.SDL.Vect             using (Point; V2)
open import Ffi.Hs.SDL.Video.Renderer   using (Surface)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified SDL.Input.Mouse
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        mℓ : Level
        M : Set mℓ → Set mℓ


data LocationMode : Set where
    AbsoluteLocation : LocationMode
    RelativeLocation : LocationMode

{-# COMPILE GHC LocationMode = data SDL.Input.Mouse.LocationMode
    ( SDL.Input.Mouse.AbsoluteLocation
    | SDL.Input.Mouse.RelativeLocation
    ) #-}

postulate
    Bounded[LocationMode] : Bounded LocationMode
    Enum[LocationMode]    : Enum LocationMode
    Eq[LocationMode]      : Eq LocationMode
    Data[LocationMode]    : Data LocationMode
    Ord[LocationMode]     : Ord LocationMode
    Read[LocationMode]    : Read LocationMode
    Show[LocationMode]    : Show LocationMode

{-# COMPILE GHC Bounded[LocationMode] = AgdaBounded #-}
{-# COMPILE GHC Enum[LocationMode]    = AgdaEnum    #-}
{-# COMPILE GHC Eq[LocationMode]      = AgdaEq      #-}
{-# COMPILE GHC Data[LocationMode]    = AgdaData    #-}
{-# COMPILE GHC Ord[LocationMode]     = AgdaOrd     #-}
{-# COMPILE GHC Read[LocationMode]    = AgdaRead    #-}
{-# COMPILE GHC Show[LocationMode]    = AgdaShow    #-}


postulate
    setMouseLocationMode : ⦃ MonadIO M ⦄ → LocationMode → M (Liftℓ _ LocationMode)
    getMouseLocationMode : ⦃ MonadIO M ⦄ → M (Liftℓ _ LocationMode)

{-# COMPILE GHC setMouseLocationMode = \ mℓ m AgdaMonadIO -> SDL.Input.Mouse.setMouseLocationMode #-}
{-# COMPILE GHC getMouseLocationMode = \ mℓ m AgdaMonadIO -> SDL.Input.Mouse.getMouseLocationMode #-}


data MouseButton : Set where
    ButtonLeft   : MouseButton
    ButtonMiddle : MouseButton
    ButtonRight  : MouseButton
    ButtonX1     : MouseButton
    ButtonX2     : MouseButton
    ButtonExtra  : Int → MouseButton

{-# COMPILE GHC MouseButton = data SDL.Input.Mouse.MouseButton
    ( SDL.Input.Mouse.ButtonLeft
    | SDL.Input.Mouse.ButtonMiddle
    | SDL.Input.Mouse.ButtonRight
    | SDL.Input.Mouse.ButtonX1
    | SDL.Input.Mouse.ButtonX2
    | SDL.Input.Mouse.ButtonExtra
    ) #-}

postulate
    Eq[MouseButton]   : Eq MouseButton
    Data[MouseButton] : Data MouseButton
    Ord[MouseButton]  : Ord MouseButton
    Read[MouseButton] : Read MouseButton
    Show[MouseButton] : Show MouseButton

{-# COMPILE GHC Eq[MouseButton]   = AgdaEq   #-}
{-# COMPILE GHC Data[MouseButton] = AgdaData #-}
{-# COMPILE GHC Ord[MouseButton]  = AgdaOrd  #-}
{-# COMPILE GHC Read[MouseButton] = AgdaRead #-}
{-# COMPILE GHC Show[MouseButton] = AgdaShow #-}


data MouseDevice : Set where
    Mouse : Int → MouseDevice
    Touch : MouseDevice

{-# COMPILE GHC MouseDevice = data SDL.Input.Mouse.MouseDevice
    ( SDL.Input.Mouse.Mouse
    | SDL.Input.Mouse.Touch
    ) #-}

postulate
    Eq[MouseDevice]   : Eq MouseDevice
    Data[MouseDevice] : Data MouseDevice
    Ord[MouseDevice]  : Ord MouseDevice
    Read[MouseDevice] : Read MouseDevice
    Show[MouseDevice] : Show MouseDevice

{-# COMPILE GHC Eq[MouseDevice]   = AgdaEq   #-}
{-# COMPILE GHC Data[MouseDevice] = AgdaData #-}
{-# COMPILE GHC Ord[MouseDevice]  = AgdaOrd  #-}
{-# COMPILE GHC Read[MouseDevice] = AgdaRead #-}
{-# COMPILE GHC Show[MouseDevice] = AgdaShow #-}


data MouseScrollDirection : Set where
    ScrollNormal  : MouseScrollDirection
    ScrollFlipped : MouseScrollDirection

{-# COMPILE GHC MouseScrollDirection = data SDL.Input.Mouse.MouseScrollDirection
    ( SDL.Input.Mouse.ScrollNormal
    | SDL.Input.Mouse.ScrollFlipped
    ) #-}

postulate
    Bounded[MouseScrollDirection] : Bounded MouseScrollDirection
    Enum[MouseScrollDirection]    : Enum MouseScrollDirection
    Eq[MouseScrollDirection]      : Eq MouseScrollDirection
    Data[MouseScrollDirection]    : Data MouseScrollDirection
    Ord[MouseScrollDirection]     : Ord MouseScrollDirection
    Read[MouseScrollDirection]    : Read MouseScrollDirection
    Show[MouseScrollDirection]    : Show MouseScrollDirection

{-# COMPILE GHC Bounded[MouseScrollDirection] = AgdaBounded #-}
{-# COMPILE GHC Enum[MouseScrollDirection]    = AgdaEnum    #-}
{-# COMPILE GHC Eq[MouseScrollDirection]      = AgdaEq      #-}
{-# COMPILE GHC Data[MouseScrollDirection]    = AgdaData    #-}
{-# COMPILE GHC Ord[MouseScrollDirection]     = AgdaOrd     #-}
{-# COMPILE GHC Read[MouseScrollDirection]    = AgdaRead    #-}
{-# COMPILE GHC Show[MouseScrollDirection]    = AgdaShow    #-}


data ModalLocation : Set where
    AbsoluteModalLocation : Point V2 CInt → ModalLocation
    RelativeModalLocation : V2 CInt → ModalLocation

{-# COMPILE GHC ModalLocation = data SDL.Input.Mouse.ModalLocation
    ( SDL.Input.Mouse.AbsoluteModalLocation
    | SDL.Input.Mouse.RelativeModalLocation
    ) #-}

postulate
    Eq[ModalLocation]   : Eq ModalLocation
    Ord[ModalLocation]  : Ord ModalLocation
    Read[ModalLocation] : Read ModalLocation
    Show[ModalLocation] : Show ModalLocation

    getModalMouseLocation    : ⦃ MonadIO M ⦄ → M (Liftℓ _ ModalLocation)
    getAbsoluteMouseLocation : ⦃ MonadIO M ⦄ → M (Liftℓ _ (Point V2 CInt))
    getRelativeMouseLocation : ⦃ MonadIO M ⦄ → M (Liftℓ _ (V2 CInt))
    getMouseButtons          : ⦃ MonadIO M ⦄ → M (Liftℓ _ (MouseButton → Bool))

{-# COMPILE GHC Eq[ModalLocation]   = AgdaEq   #-}
{-# COMPILE GHC Ord[ModalLocation]  = AgdaOrd  #-}
{-# COMPILE GHC Read[ModalLocation] = AgdaRead #-}
{-# COMPILE GHC Show[ModalLocation] = AgdaShow #-}

{-# COMPILE GHC getModalMouseLocation    = \ mℓ m AgdaMonadIO -> SDL.Input.Mouse.getModalMouseLocation    #-}
{-# COMPILE GHC getAbsoluteMouseLocation = \ mℓ m AgdaMonadIO -> SDL.Input.Mouse.getAbsoluteMouseLocation #-}
{-# COMPILE GHC getRelativeMouseLocation = \ mℓ m AgdaMonadIO -> SDL.Input.Mouse.getRelativeMouseLocation #-}
{-# COMPILE GHC getMouseButtons          = \ mℓ m AgdaMonadIO -> SDL.Input.Mouse.getMouseButtons          #-}


data WarpMouseOrigin : Set where
    WarpInWindow     : Window → WarpMouseOrigin
    WarpCurrentFocus : WarpMouseOrigin
    WarpGlobal       : WarpMouseOrigin

{-# COMPILE GHC WarpMouseOrigin = data SDL.Input.Mouse.WarpMouseOrigin
    ( SDL.Input.Mouse.WarpInWindow
    | SDL.Input.Mouse.WarpCurrentFocus
    | SDL.Input.Mouse.WarpGlobal
    ) #-}

postulate
    Eq[WarpMouseOrigin]   : Eq WarpMouseOrigin
    Data[WarpMouseOrigin] : Data WarpMouseOrigin
    Ord[WarpMouseOrigin]  : Ord WarpMouseOrigin
    Show[WarpMouseOrigin] : Show WarpMouseOrigin

{-# COMPILE GHC Eq[WarpMouseOrigin]   = AgdaEq   #-}
{-# COMPILE GHC Data[WarpMouseOrigin] = AgdaData #-}
{-# COMPILE GHC Ord[WarpMouseOrigin]  = AgdaOrd  #-}
{-# COMPILE GHC Show[WarpMouseOrigin] = AgdaShow #-}

postulate
    warpMouse : ⦃ MonadIO M ⦄ → WarpMouseOrigin → Point V2 CInt → M ⊤′

{-# COMPILE GHC warpMouse = \ mℓ m AgdaMonadIO -> SDL.Input.Mouse.warpMouse #-}


data SystemCursor : Set where
    SystemCursorArrow     : SystemCursor
    SystemCursorIBeam     : SystemCursor
    SystemCursorWait      : SystemCursor
    SystemCursorCrossHair : SystemCursor
    SystemCursorWaitArrow : SystemCursor
    SystemCursorSizeNWSE  : SystemCursor
    SystemCursorSizeNESW  : SystemCursor
    SystemCursorSizeWE    : SystemCursor
    SystemCursorSizeNS    : SystemCursor
    SystemCursorSizeAll   : SystemCursor
    SystemCursorNo        : SystemCursor
    SystemCursorHand      : SystemCursor

{-# COMPILE GHC SystemCursor = data SDL.Input.Mouse.SystemCursor
    ( SDL.Input.Mouse.SystemCursorArrow
    | SDL.Input.Mouse.SystemCursorIBeam
    | SDL.Input.Mouse.SystemCursorWait
    | SDL.Input.Mouse.SystemCursorCrossHair
    | SDL.Input.Mouse.SystemCursorWaitArrow
    | SDL.Input.Mouse.SystemCursorSizeNWSE
    | SDL.Input.Mouse.SystemCursorSizeNESW
    | SDL.Input.Mouse.SystemCursorSizeWE
    | SDL.Input.Mouse.SystemCursorSizeNS
    | SDL.Input.Mouse.SystemCursorSizeAll
    | SDL.Input.Mouse.SystemCursorNo
    | SDL.Input.Mouse.SystemCursorHand
    ) #-}

postulate
    Cursor : Set
    Eq[Cursor] : Eq Cursor

    cursorVisible      : StateVar Bool
    activeCursor       : StateVar Cursor
    createCursor       : ⦃ MonadIO M ⦄ → Vector Word8 → Vector Word8 → V2 CInt → Point V2 CInt → M (Liftℓ _ Cursor)
    freeCursor         : ⦃ MonadIO M ⦄ → Cursor → M ⊤′
    createColorCursor  : ⦃ MonadIO M ⦄ → Surface → Point V2 CInt → M (Liftℓ _ Cursor)
    createSystemCursor : ⦃ MonadIO M ⦄ → SystemCursor → M (Liftℓ _ Cursor)

{-# COMPILE GHC Cursor = type SDL.Input.Mouse.Cursor #-}
{-# COMPILE GHC Eq[Cursor] = AgdaEq #-}

{-# COMPILE GHC cursorVisible      =                       SDL.Input.Mouse.cursorVisible      #-}
{-# COMPILE GHC activeCursor       =                       SDL.Input.Mouse.activeCursor       #-}
{-# COMPILE GHC createCursor       = \ mℓ m AgdaMonadIO -> SDL.Input.Mouse.createCursor       #-}
{-# COMPILE GHC freeCursor         = \ mℓ m AgdaMonadIO -> SDL.Input.Mouse.freeCursor         #-}
{-# COMPILE GHC createColorCursor  = \ mℓ m AgdaMonadIO -> SDL.Input.Mouse.createColorCursor  #-}
{-# COMPILE GHC createSystemCursor = \ mℓ m AgdaMonadIO -> SDL.Input.Mouse.createSystemCursor #-}
