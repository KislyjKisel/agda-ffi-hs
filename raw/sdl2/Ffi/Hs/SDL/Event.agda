{-# OPTIONS --without-K #-}

module Ffi.Hs.SDL.Event where

open import Agda.Builtin.Bool               using (Bool)
open import Agda.Builtin.IO                 using (IO)
open import Agda.Builtin.List               using (List)
open import Agda.Builtin.Maybe              using (Maybe)
open import Agda.Builtin.String             using () renaming (String to Text)
open import Agda.Primitive
open import Ffi.Hs.-base.Class
open import Ffi.Hs.-base.Level              using (Liftℓ)
open import Ffi.Hs.-base.Unit               using (⊤; ⊤′)
open import Ffi.Hs.Data.Int                 using (Int; Int16; Int32)
open import Ffi.Hs.Data.Word                using (Word8; Word16; Word32)
open import Ffi.Hs.Foreign.C.String         using (CString)
open import Ffi.Hs.Foreign.C.Types          using (CInt; CFloat)
open import Ffi.Hs.Foreign.Ptr              using (Ptr)
open import Ffi.Hs.SDL.Input.GameController using (ControllerButton; ControllerButtonState; ControllerDeviceConnection)
open import Ffi.Hs.SDL.Input.Joystick       using (JoyHatPosition; JoyButtonState; JoyDeviceConnection)
open import Ffi.Hs.SDL.Input.Mouse          using (MouseDevice; MouseScrollDirection)
open import Ffi.Hs.SDL.Internal.Types       using (Window)
open import Ffi.Hs.SDL.Raw.Types as Raw     using ()
open import Ffi.Hs.SDL.Vect                 using (Point; V2)

open Ffi.Hs.SDL.Input.Mouse public
    using
    ( MouseButton
    ; ButtonLeft
    ; ButtonMiddle
    ; ButtonRight
    ; ButtonX1
    ; ButtonX2
    ; ButtonExtra
    )

private
    variable
        aℓ : Level
        A : Set aℓ
        M : Set aℓ → Set aℓ

-- Auxiliary event data

data InputMotion : Set where
    Released : InputMotion
    Pressed  : InputMotion

{-# COMPILE GHC InputMotion = data SDL.Event.InputMotion
    ( SDL.Event.Released
    | SDL.Event.Pressed
    ) #-}

postulate
    Bounded[InputMotion] : Bounded InputMotion
    Enum[InputMotion]    : Enum InputMotion
    Eq[InputMotion]      : Eq InputMotion
    Data[InputMotion]    : Data InputMotion
    Ord[InputMotion]     : Ord InputMotion
    Read[InputMotion]    : Read InputMotion
    Show[InputMotion]    : Show InputMotion

{-# COMPILE GHC Bounded[InputMotion] = AgdaBounded #-}
{-# COMPILE GHC Enum[InputMotion]    = AgdaEnum    #-}
{-# COMPILE GHC Eq[InputMotion]      = AgdaEq      #-}
{-# COMPILE GHC Data[InputMotion]    = AgdaData    #-}
{-# COMPILE GHC Ord[InputMotion]     = AgdaOrd     #-}
{-# COMPILE GHC Read[InputMotion]    = AgdaRead    #-}
{-# COMPILE GHC Show[InputMotion]    = AgdaShow    #-}

-- Event data

Timestamp : Set
Timestamp = Word32

record WindowShownEventData : Set where
    constructor mkWindowShownEventData
    field
        windowShownEventWindow : Window

{-# COMPILE GHC WindowShownEventData = data SDL.Event.WindowShownEventData (SDL.Event.WindowShownEventData) #-}

postulate
    Eq[WindowShownEventData]   : Eq WindowShownEventData
    Ord[WindowShownEventData]  : Ord WindowShownEventData
    Show[WindowShownEventData] : Show WindowShownEventData

{-# COMPILE GHC Eq[WindowShownEventData]   = AgdaEq   #-}
{-# COMPILE GHC Ord[WindowShownEventData]  = AgdaOrd  #-}
{-# COMPILE GHC Show[WindowShownEventData] = AgdaShow #-}

record WindowHiddenEventData : Set where
    constructor mkWindowHiddenEventData
    field
        windowHiddenEventWindow : Window

{-# COMPILE GHC WindowHiddenEventData = data SDL.Event.WindowHiddenEventData (SDL.Event.WindowHiddenEventData) #-}

postulate
    Eq[WindowHiddenEventData]   : Eq WindowHiddenEventData
    Ord[WindowHiddenEventData]  : Ord WindowHiddenEventData
    Show[WindowHiddenEventData] : Show WindowHiddenEventData

{-# COMPILE GHC Eq[WindowHiddenEventData]   = AgdaEq   #-}
{-# COMPILE GHC Ord[WindowHiddenEventData]  = AgdaOrd  #-}
{-# COMPILE GHC Show[WindowHiddenEventData] = AgdaShow #-}

record WindowExposedEventData : Set where
    constructor mkWindowExposedEventData
    field
        windowExposedEventWindow : Window

{-# COMPILE GHC WindowExposedEventData = data SDL.Event.WindowExposedEventData (SDL.Event.WindowExposedEventData) #-}

postulate
    Eq[WindowExposedEventData]   : Eq WindowExposedEventData
    Ord[WindowExposedEventData]  : Ord WindowExposedEventData
    Show[WindowExposedEventData] : Show WindowExposedEventData

{-# COMPILE GHC Eq[WindowExposedEventData]   = AgdaEq   #-}
{-# COMPILE GHC Ord[WindowExposedEventData]  = AgdaOrd  #-}
{-# COMPILE GHC Show[WindowExposedEventData] = AgdaShow #-}

record WindowMovedEventData : Set where
    constructor mkWindowMovedEventData
    field
        windowMovedEventWindow   : Window
        windowMovedEventPosition : Point V2 Int32

{-# COMPILE GHC WindowMovedEventData = data SDL.Event.WindowMovedEventData (SDL.Event.WindowMovedEventData) #-}

postulate
    Eq[WindowMovedEventData]   : Eq WindowMovedEventData
    Ord[WindowMovedEventData]  : Ord WindowMovedEventData
    Show[WindowMovedEventData] : Show WindowMovedEventData

{-# COMPILE GHC Eq[WindowMovedEventData]   = AgdaEq   #-}
{-# COMPILE GHC Ord[WindowMovedEventData]  = AgdaOrd  #-}
{-# COMPILE GHC Show[WindowMovedEventData] = AgdaShow #-}

record WindowResizedEventData : Set where
    constructor mkWindowResizedEventData
    field
        windowResizedEventWindow : Window
        windowResizedEventSize   : V2 Int32

{-# COMPILE GHC WindowResizedEventData = data SDL.Event.WindowResizedEventData (SDL.Event.WindowResizedEventData) #-}

postulate
    Eq[WindowResizedEventData]   : Eq WindowResizedEventData
    Ord[WindowResizedEventData]  : Ord WindowResizedEventData
    Show[WindowResizedEventData] : Show WindowResizedEventData

{-# COMPILE GHC Eq[WindowResizedEventData]   = AgdaEq   #-}
{-# COMPILE GHC Ord[WindowResizedEventData]  = AgdaOrd  #-}
{-# COMPILE GHC Show[WindowResizedEventData] = AgdaShow #-}

record WindowSizeChangedEventData : Set where
    constructor mkWindowSizeChangedEventData
    field
        windowSizeChangedEventWindow : Window
        windowSizeChangedEventSize   : V2 Int32

{-# COMPILE GHC WindowSizeChangedEventData = data SDL.Event.WindowSizeChangedEventData (SDL.Event.WindowSizeChangedEventData) #-}

postulate
    Eq[WindowSizeChangedEventData]   : Eq WindowSizeChangedEventData
    Ord[WindowSizeChangedEventData]  : Ord WindowSizeChangedEventData
    Show[WindowSizeChangedEventData] : Show WindowSizeChangedEventData

{-# COMPILE GHC Eq[WindowSizeChangedEventData]   = AgdaEq   #-}
{-# COMPILE GHC Ord[WindowSizeChangedEventData]  = AgdaOrd  #-}
{-# COMPILE GHC Show[WindowSizeChangedEventData] = AgdaShow #-}

record WindowMinimizedEventData : Set where
    constructor mkWindowMinimizedEventData
    field
        windowMinimizedEventWindow : Window

{-# COMPILE GHC WindowMinimizedEventData = data SDL.Event.WindowMinimizedEventData (SDL.Event.WindowMinimizedEventData) #-}

postulate
    Eq[WindowMinimizedEventData]   : Eq WindowMinimizedEventData
    Ord[WindowMinimizedEventData]  : Ord WindowMinimizedEventData
    Show[WindowMinimizedEventData] : Show WindowMinimizedEventData

{-# COMPILE GHC Eq[WindowMinimizedEventData]   = AgdaEq   #-}
{-# COMPILE GHC Ord[WindowMinimizedEventData]  = AgdaOrd  #-}
{-# COMPILE GHC Show[WindowMinimizedEventData] = AgdaShow #-}

record WindowMaximizedEventData : Set where
    constructor mkWindowMaximizedEventData
    field
        windowMaximizedEventWindow : Window

{-# COMPILE GHC WindowMaximizedEventData = data SDL.Event.WindowMaximizedEventData (SDL.Event.WindowMaximizedEventData) #-}

postulate
    Eq[WindowMaximizedEventData]   : Eq WindowMaximizedEventData
    Ord[WindowMaximizedEventData]  : Ord WindowMaximizedEventData
    Show[WindowMaximizedEventData] : Show WindowMaximizedEventData

{-# COMPILE GHC Eq[WindowMaximizedEventData]   = AgdaEq   #-}
{-# COMPILE GHC Ord[WindowMaximizedEventData]  = AgdaOrd  #-}
{-# COMPILE GHC Show[WindowMaximizedEventData] = AgdaShow #-}

record WindowRestoredEventData : Set where
    constructor mkWindowRestoredEventData
    field
        windowRestoredEventWindow : Window

{-# COMPILE GHC WindowRestoredEventData = data SDL.Event.WindowRestoredEventData (SDL.Event.WindowRestoredEventData) #-}

postulate
    Eq[WindowRestoredEventData]   : Eq WindowRestoredEventData
    Ord[WindowRestoredEventData]  : Ord WindowRestoredEventData
    Show[WindowRestoredEventData] : Show WindowRestoredEventData

{-# COMPILE GHC Eq[WindowRestoredEventData]   = AgdaEq   #-}
{-# COMPILE GHC Ord[WindowRestoredEventData]  = AgdaOrd  #-}
{-# COMPILE GHC Show[WindowRestoredEventData] = AgdaShow #-}

record WindowGainedMouseFocusEventData : Set where
    constructor mkWindowGainedMouseFocusEventData
    field
        windowGainedMouseFocusEventWindow : Window

{-# COMPILE GHC WindowGainedMouseFocusEventData = data SDL.Event.WindowGainedMouseFocusEventData (SDL.Event.WindowGainedMouseFocusEventData) #-}

postulate
    Eq[WindowGainedMouseFocusEventData]   : Eq WindowGainedMouseFocusEventData
    Ord[WindowGainedMouseFocusEventData]  : Ord WindowGainedMouseFocusEventData
    Show[WindowGainedMouseFocusEventData] : Show WindowGainedMouseFocusEventData

{-# COMPILE GHC Eq[WindowGainedMouseFocusEventData]   = AgdaEq   #-}
{-# COMPILE GHC Ord[WindowGainedMouseFocusEventData]  = AgdaOrd  #-}
{-# COMPILE GHC Show[WindowGainedMouseFocusEventData] = AgdaShow #-}

record WindowLostMouseFocusEventData : Set where
    constructor mkWindowLostMouseFocusEventData
    field
        windowLostMouseFocusEventWindow : Window

{-# COMPILE GHC WindowLostMouseFocusEventData = data SDL.Event.WindowLostMouseFocusEventData (SDL.Event.WindowLostMouseFocusEventData) #-}

postulate
    Eq[WindowLostMouseFocusEventData]   : Eq WindowLostMouseFocusEventData
    Ord[WindowLostMouseFocusEventData]  : Ord WindowLostMouseFocusEventData
    Show[WindowLostMouseFocusEventData] : Show WindowLostMouseFocusEventData

{-# COMPILE GHC Eq[WindowLostMouseFocusEventData]   = AgdaEq   #-}
{-# COMPILE GHC Ord[WindowLostMouseFocusEventData]  = AgdaOrd  #-}
{-# COMPILE GHC Show[WindowLostMouseFocusEventData] = AgdaShow #-}

record WindowGainedKeyboardFocusEventData : Set where
    constructor mkWindowGainedKeyboardFocusEventData
    field
        windowGainedKeyboardFocusEventWindow : Window

{-# COMPILE GHC WindowGainedKeyboardFocusEventData = data SDL.Event.WindowGainedKeyboardFocusEventData (SDL.Event.WindowGainedKeyboardFocusEventData) #-}

postulate
    Eq[WindowGainedKeyboardFocusEventData]   : Eq WindowGainedKeyboardFocusEventData
    Ord[WindowGainedKeyboardFocusEventData]  : Ord WindowGainedKeyboardFocusEventData
    Show[WindowGainedKeyboardFocusEventData] : Show WindowGainedKeyboardFocusEventData

{-# COMPILE GHC Eq[WindowGainedKeyboardFocusEventData]   = AgdaEq   #-}
{-# COMPILE GHC Ord[WindowGainedKeyboardFocusEventData]  = AgdaOrd  #-}
{-# COMPILE GHC Show[WindowGainedKeyboardFocusEventData] = AgdaShow #-}

record WindowLostKeyboardFocusEventData : Set where
    constructor mkWindowLostKeyboardFocusEventData
    field
        windowLostKeyboardFocusEventWindow : Window

{-# COMPILE GHC WindowLostKeyboardFocusEventData = data SDL.Event.WindowLostKeyboardFocusEventData (SDL.Event.WindowLostKeyboardFocusEventData) #-}

postulate
    Eq[WindowLostKeyboardFocusEventData]   : Eq WindowLostKeyboardFocusEventData
    Ord[WindowLostKeyboardFocusEventData]  : Ord WindowLostKeyboardFocusEventData
    Show[WindowLostKeyboardFocusEventData] : Show WindowLostKeyboardFocusEventData

{-# COMPILE GHC Eq[WindowLostKeyboardFocusEventData]   = AgdaEq   #-}
{-# COMPILE GHC Ord[WindowLostKeyboardFocusEventData]  = AgdaOrd  #-}
{-# COMPILE GHC Show[WindowLostKeyboardFocusEventData] = AgdaShow #-}

record WindowClosedEventData : Set where
    constructor mkWindowClosedEventData
    field
        windowClosedEventWindow : Window

{-# COMPILE GHC WindowClosedEventData = data SDL.Event.WindowClosedEventData (SDL.Event.WindowClosedEventData) #-}

postulate
    Eq[WindowClosedEventData]   : Eq WindowClosedEventData
    Ord[WindowClosedEventData]  : Ord WindowClosedEventData
    Show[WindowClosedEventData] : Show WindowClosedEventData

{-# COMPILE GHC Eq[WindowClosedEventData]   = AgdaEq   #-}
{-# COMPILE GHC Ord[WindowClosedEventData]  = AgdaOrd  #-}
{-# COMPILE GHC Show[WindowClosedEventData] = AgdaShow #-}

record SysWMEventData : Set where
    constructor mkSysWMEventData
    field
        sysWMEventMsg : Raw.SysWMmsg

{-# COMPILE GHC SysWMEventData = data SDL.Event.SysWMEventData (SDL.Event.SysWMEventData) #-}

postulate
    Eq[SysWMEventData]   : Eq SysWMEventData
    Ord[SysWMEventData]  : Ord SysWMEventData
    Show[SysWMEventData] : Show SysWMEventData

{-# COMPILE GHC Eq[SysWMEventData]   = AgdaEq   #-}
{-# COMPILE GHC Ord[SysWMEventData]  = AgdaOrd  #-}
{-# COMPILE GHC Show[SysWMEventData] = AgdaShow #-}

record KeyboardEventData : Set where
    constructor mkKeyboardEventData
    field
        keyboardEventWindow    : Maybe Window
        keyboardEventKeyMotion : InputMotion
        keyboardEventRepeat    : Bool
        keyboardEventKeysym    : Raw.Keysym

{-# COMPILE GHC KeyboardEventData = data SDL.Event.KeyboardEventData (SDL.Event.KeyboardEventData) #-}

postulate
    Eq[KeyboardEventData]   : Eq KeyboardEventData
    Ord[KeyboardEventData]  : Ord KeyboardEventData
    Show[KeyboardEventData] : Show KeyboardEventData

{-# COMPILE GHC Eq[KeyboardEventData]   = AgdaEq   #-}
{-# COMPILE GHC Ord[KeyboardEventData]  = AgdaOrd  #-}
{-# COMPILE GHC Show[KeyboardEventData] = AgdaShow #-}

record TextEditingEventData : Set where
    constructor mkTextEditingEventData
    field
        textEditingEventWindow : Maybe Window
        textEditingEventText   : Text
        textEditingEventStart  : Int32
        textEditingEventLength : Int32

{-# COMPILE GHC TextEditingEventData = data SDL.Event.TextEditingEventData (SDL.Event.TextEditingEventData) #-}

postulate
    Eq[TextEditingEventData]   : Eq TextEditingEventData
    Ord[TextEditingEventData]  : Ord TextEditingEventData
    Show[TextEditingEventData] : Show TextEditingEventData

{-# COMPILE GHC Eq[TextEditingEventData]   = AgdaEq   #-}
{-# COMPILE GHC Ord[TextEditingEventData]  = AgdaOrd  #-}
{-# COMPILE GHC Show[TextEditingEventData] = AgdaShow #-}

record TextInputEventData : Set where
    constructor mkTextInputEventData
    field
        textInputEventWindow : Maybe Window
        textInputEventText   : Text

{-# COMPILE GHC TextInputEventData = data SDL.Event.TextInputEventData (SDL.Event.TextInputEventData) #-}

postulate
    Eq[TextInputEventData]   : Eq TextInputEventData
    Ord[TextInputEventData]  : Ord TextInputEventData
    Show[TextInputEventData] : Show TextInputEventData

{-# COMPILE GHC Eq[TextInputEventData]   = AgdaEq   #-}
{-# COMPILE GHC Ord[TextInputEventData]  = AgdaOrd  #-}
{-# COMPILE GHC Show[TextInputEventData] = AgdaShow #-}

record MouseMotionEventData : Set where
    constructor mkMouseMotionEventData
    field
        mouseMotionEventWindow    : Maybe Window
        mouseMotionEventWhich     : MouseDevice
        mouseMotionEventState     : List MouseButton
        mouseMotionEventPos       : Point V2 Int32
        mouseMotionEventRelMotion : V2 Int32

{-# COMPILE GHC MouseMotionEventData = data SDL.Event.MouseMotionEventData (SDL.Event.MouseMotionEventData) #-}

postulate
    Eq[MouseMotionEventData]   : Eq MouseMotionEventData
    Ord[MouseMotionEventData]  : Ord MouseMotionEventData
    Show[MouseMotionEventData] : Show MouseMotionEventData

{-# COMPILE GHC Eq[MouseMotionEventData]   = AgdaEq   #-}
{-# COMPILE GHC Ord[MouseMotionEventData]  = AgdaOrd  #-}
{-# COMPILE GHC Show[MouseMotionEventData] = AgdaShow #-}

record MouseButtonEventData : Set where
    constructor mkMouseButtonEventData
    field
        mouseButtonEventWindow : Maybe Window
        mouseButtonEventMotion : InputMotion
        mouseButtonEventWhich  : MouseDevice
        mouseButtonEventButton : MouseButton
        mouseButtonEventClicks : Word8
        mouseButtonEventPos    : Point V2 Int32

{-# COMPILE GHC MouseButtonEventData = data SDL.Event.MouseButtonEventData (SDL.Event.MouseButtonEventData) #-}

postulate
    Eq[MouseButtonEventData]   : Eq MouseButtonEventData
    Ord[MouseButtonEventData]  : Ord MouseButtonEventData
    Show[MouseButtonEventData] : Show MouseButtonEventData

{-# COMPILE GHC Eq[MouseButtonEventData]   = AgdaEq   #-}
{-# COMPILE GHC Ord[MouseButtonEventData]  = AgdaOrd  #-}
{-# COMPILE GHC Show[MouseButtonEventData] = AgdaShow #-}

record MouseWheelEventData : Set where
    constructor mkMouseWheelEventData
    field
        mouseWheelEventWindow    : Maybe Window
        mouseWheelEventWhich     : MouseDevice
        mouseWheelEventPos       : V2 Int32
        mouseWheelEventDirection : MouseScrollDirection

{-# COMPILE GHC MouseWheelEventData = data SDL.Event.MouseWheelEventData (SDL.Event.MouseWheelEventData) #-}

postulate
    Eq[MouseWheelEventData]   : Eq MouseWheelEventData
    Ord[MouseWheelEventData]  : Ord MouseWheelEventData
    Show[MouseWheelEventData] : Show MouseWheelEventData

{-# COMPILE GHC Eq[MouseWheelEventData]   = AgdaEq   #-}
{-# COMPILE GHC Ord[MouseWheelEventData]  = AgdaOrd  #-}
{-# COMPILE GHC Show[MouseWheelEventData] = AgdaShow #-}

record JoyAxisEventData : Set where
    constructor mkJoyAxisEventData
    field
        joyAxisEventWhich : Raw.JoystickID
        joyAxisEventAxis  : Word8
        joyAxisEventValue : Int16

{-# COMPILE GHC JoyAxisEventData = data SDL.Event.JoyAxisEventData (SDL.Event.JoyAxisEventData) #-}

postulate
    Eq[JoyAxisEventData]   : Eq JoyAxisEventData
    Ord[JoyAxisEventData]  : Ord JoyAxisEventData
    Show[JoyAxisEventData] : Show JoyAxisEventData

{-# COMPILE GHC Eq[JoyAxisEventData]   = AgdaEq   #-}
{-# COMPILE GHC Ord[JoyAxisEventData]  = AgdaOrd  #-}
{-# COMPILE GHC Show[JoyAxisEventData] = AgdaShow #-}

record JoyBallEventData : Set where
    constructor mkJoyBallEventData
    field
        joyBallEventWhich     : Raw.JoystickID
        joyBallEventBall      : Word8
        joyBallEventRelMotion : V2 Int16

{-# COMPILE GHC JoyBallEventData = data SDL.Event.JoyBallEventData (SDL.Event.JoyBallEventData) #-}

postulate
    Eq[JoyBallEventData]   : Eq JoyBallEventData
    Ord[JoyBallEventData]  : Ord JoyBallEventData
    Show[JoyBallEventData] : Show JoyBallEventData

{-# COMPILE GHC Eq[JoyBallEventData]   = AgdaEq   #-}
{-# COMPILE GHC Ord[JoyBallEventData]  = AgdaOrd  #-}
{-# COMPILE GHC Show[JoyBallEventData] = AgdaShow #-}

record JoyHatEventData : Set where
    constructor mkJoyHatEventData
    field
        joyHatEventWhich : Raw.JoystickID
        joyHatEventHat   : Word8
        joyHatEventValue : JoyHatPosition

{-# COMPILE GHC JoyHatEventData = data SDL.Event.JoyHatEventData (SDL.Event.JoyHatEventData) #-}

postulate
    Eq[JoyHatEventData]   : Eq JoyHatEventData
    Ord[JoyHatEventData]  : Ord JoyHatEventData
    Show[JoyHatEventData] : Show JoyHatEventData

{-# COMPILE GHC Eq[JoyHatEventData]   = AgdaEq   #-}
{-# COMPILE GHC Ord[JoyHatEventData]  = AgdaOrd  #-}
{-# COMPILE GHC Show[JoyHatEventData] = AgdaShow #-}

record JoyButtonEventData : Set where
    constructor mkJoyButtonEventData
    field
        joyButtonEventWhich  : Raw.JoystickID
        joyButtonEventButton : Word8
        joyButtonEventState  : JoyButtonState

{-# COMPILE GHC JoyButtonEventData = data SDL.Event.JoyButtonEventData (SDL.Event.JoyButtonEventData) #-}

postulate
    Eq[JoyButtonEventData]   : Eq JoyButtonEventData
    Ord[JoyButtonEventData]  : Ord JoyButtonEventData
    Show[JoyButtonEventData] : Show JoyButtonEventData

{-# COMPILE GHC Eq[JoyButtonEventData]   = AgdaEq   #-}
{-# COMPILE GHC Ord[JoyButtonEventData]  = AgdaOrd  #-}
{-# COMPILE GHC Show[JoyButtonEventData] = AgdaShow #-}

record JoyDeviceEventData : Set where
    constructor mkJoyDeviceEventData
    field
        joyDeviceEventConnection : JoyDeviceConnection
        joyDeviceEventWhich      : Int32

{-# COMPILE GHC JoyDeviceEventData = data SDL.Event.JoyDeviceEventData (SDL.Event.JoyDeviceEventData) #-}

postulate
    Eq[JoyDeviceEventData]   : Eq JoyDeviceEventData
    Ord[JoyDeviceEventData]  : Ord JoyDeviceEventData
    Show[JoyDeviceEventData] : Show JoyDeviceEventData

{-# COMPILE GHC Eq[JoyDeviceEventData]   = AgdaEq   #-}
{-# COMPILE GHC Ord[JoyDeviceEventData]  = AgdaOrd  #-}
{-# COMPILE GHC Show[JoyDeviceEventData] = AgdaShow #-}

record ControllerAxisEventData : Set where
    constructor mkControllerAxisEventData
    field
        controllerAxisEventWhich : Raw.JoystickID
        controllerAxisEventAxis  : Word8
        controllerAxisEventValue : Int16

{-# COMPILE GHC ControllerAxisEventData = data SDL.Event.ControllerAxisEventData (SDL.Event.ControllerAxisEventData) #-}

postulate
    Eq[ControllerAxisEventData]   : Eq ControllerAxisEventData
    Ord[ControllerAxisEventData]  : Ord ControllerAxisEventData
    Show[ControllerAxisEventData] : Show ControllerAxisEventData

{-# COMPILE GHC Eq[ControllerAxisEventData]   = AgdaEq   #-}
{-# COMPILE GHC Ord[ControllerAxisEventData]  = AgdaOrd  #-}
{-# COMPILE GHC Show[ControllerAxisEventData] = AgdaShow #-}

record ControllerButtonEventData : Set where
    constructor mkControllerButtonEventData
    field
        controllerButtonEventWhich  : Raw.JoystickID
        controllerButtonEventButton : ControllerButton
        controllerButtonEventState  : ControllerButtonState

{-# COMPILE GHC ControllerButtonEventData = data SDL.Event.ControllerButtonEventData (SDL.Event.ControllerButtonEventData) #-}

postulate
    Eq[ControllerButtonEventData]   : Eq ControllerButtonEventData
    Ord[ControllerButtonEventData]  : Ord ControllerButtonEventData
    Show[ControllerButtonEventData] : Show ControllerButtonEventData

{-# COMPILE GHC Eq[ControllerButtonEventData]   = AgdaEq   #-}
{-# COMPILE GHC Ord[ControllerButtonEventData]  = AgdaOrd  #-}
{-# COMPILE GHC Show[ControllerButtonEventData] = AgdaShow #-}

record ControllerDeviceEventData : Set where
    constructor mkControllerDeviceEventData
    field
        controllerDeviceEventConnection : ControllerDeviceConnection
        controllerDeviceEventWhich      : Int32

{-# COMPILE GHC ControllerDeviceEventData = data SDL.Event.ControllerDeviceEventData (SDL.Event.ControllerDeviceEventData) #-}

postulate
    Eq[ControllerDeviceEventData]   : Eq ControllerDeviceEventData
    Ord[ControllerDeviceEventData]  : Ord ControllerDeviceEventData
    Show[ControllerDeviceEventData] : Show ControllerDeviceEventData

{-# COMPILE GHC Eq[ControllerDeviceEventData]   = AgdaEq   #-}
{-# COMPILE GHC Ord[ControllerDeviceEventData]  = AgdaOrd  #-}
{-# COMPILE GHC Show[ControllerDeviceEventData] = AgdaShow #-}

record AudioDeviceEventData : Set where
    constructor mkAudioDeviceEventData
    field
        audioDeviceEventIsAddition : Bool
        audioDeviceEventWhich      : Word32
        audioDeviceEventIsCapture  : Bool

{-# COMPILE GHC AudioDeviceEventData = data SDL.Event.AudioDeviceEventData (SDL.Event.AudioDeviceEventData) #-}

postulate
    Eq[AudioDeviceEventData]   : Eq AudioDeviceEventData
    Ord[AudioDeviceEventData]  : Ord AudioDeviceEventData
    Show[AudioDeviceEventData] : Show AudioDeviceEventData

{-# COMPILE GHC Eq[AudioDeviceEventData]   = AgdaEq   #-}
{-# COMPILE GHC Ord[AudioDeviceEventData]  = AgdaOrd  #-}
{-# COMPILE GHC Show[AudioDeviceEventData] = AgdaShow #-}

record UserEventData : Set where
    constructor mkUserEventData
    field
        userEventType   : Word32
        userEventWindow : Maybe Window
        userEventCode   : Int32
        userEventData1  : Ptr ⊤
        userEventData2  : Ptr ⊤

{-# COMPILE GHC UserEventData = data SDL.Event.UserEventData (SDL.Event.UserEventData) #-}

postulate
    Eq[UserEventData]   : Eq UserEventData
    Ord[UserEventData]  : Ord UserEventData
    Show[UserEventData] : Show UserEventData

{-# COMPILE GHC Eq[UserEventData]   = AgdaEq   #-}
{-# COMPILE GHC Ord[UserEventData]  = AgdaOrd  #-}
{-# COMPILE GHC Show[UserEventData] = AgdaShow #-}

record TouchFingerEventData : Set where
    constructor mkTouchFingerEventData
    field
        touchFingerEventTouchID  : Raw.TouchID
        touchFingerEventFingerID : Raw.FingerID
        touchFingerEventMotion   : InputMotion
        touchFingerEventPos      : Point V2 CFloat
        touchFingerEventPressure : CFloat

{-# COMPILE GHC TouchFingerEventData = data SDL.Event.TouchFingerEventData (SDL.Event.TouchFingerEventData) #-}

postulate
    Eq[TouchFingerEventData]   : Eq TouchFingerEventData
    Ord[TouchFingerEventData]  : Ord TouchFingerEventData
    Show[TouchFingerEventData] : Show TouchFingerEventData

{-# COMPILE GHC Eq[TouchFingerEventData]   = AgdaEq   #-}
{-# COMPILE GHC Ord[TouchFingerEventData]  = AgdaOrd  #-}
{-# COMPILE GHC Show[TouchFingerEventData] = AgdaShow #-}

record TouchFingerMotionEventData : Set where
    constructor mkTouchFingerMotionEventData
    field
        touchFingerMotionEventTouchID   : Raw.TouchID
        touchFingerMotionEventFingerID  : Raw.FingerID
        touchFingerMotionEventPos       : Point V2 CFloat
        touchFingerMotionEventRelMotion : V2 CFloat
        touchFingerMotionEventPressure  : CFloat

{-# COMPILE GHC TouchFingerMotionEventData = data SDL.Event.TouchFingerMotionEventData (SDL.Event.TouchFingerMotionEventData) #-}

postulate
    Eq[TouchFingerMotionEventData]   : Eq TouchFingerMotionEventData
    Ord[TouchFingerMotionEventData]  : Ord TouchFingerMotionEventData
    Show[TouchFingerMotionEventData] : Show TouchFingerMotionEventData

{-# COMPILE GHC Eq[TouchFingerMotionEventData]   = AgdaEq   #-}
{-# COMPILE GHC Ord[TouchFingerMotionEventData]  = AgdaOrd  #-}
{-# COMPILE GHC Show[TouchFingerMotionEventData] = AgdaShow #-}

record MultiGestureEventData : Set where
    constructor mkMultiGestureEventData
    field
        multiGestureEventTouchID    : Raw.TouchID
        multiGestureEventDTheta     : CFloat
        multiGestureEventDDist      : CFloat
        multiGestureEventPos        : Point V2 CFloat
        multiGestureEventNumFingers : Word16

{-# COMPILE GHC MultiGestureEventData = data SDL.Event.MultiGestureEventData (SDL.Event.MultiGestureEventData) #-}

postulate
    Eq[MultiGestureEventData]   : Eq MultiGestureEventData
    Ord[MultiGestureEventData]  : Ord MultiGestureEventData
    Show[MultiGestureEventData] : Show MultiGestureEventData

{-# COMPILE GHC Eq[MultiGestureEventData]   = AgdaEq   #-}
{-# COMPILE GHC Ord[MultiGestureEventData]  = AgdaOrd  #-}
{-# COMPILE GHC Show[MultiGestureEventData] = AgdaShow #-}

record DollarGestureEventData : Set where
    constructor mkDollarGestureEventData
    field
        dollarGestureEventTouchID    : Raw.TouchID
        dollarGestureEventGestureID  : Raw.GestureID
        dollarGestureEventNumFingers : Word32
        dollarGestureEventError      : CFloat
        dollarGestureEventPos        : Point V2 CFloat

{-# COMPILE GHC DollarGestureEventData = data SDL.Event.DollarGestureEventData (SDL.Event.DollarGestureEventData) #-}

postulate
    Eq[DollarGestureEventData]   : Eq DollarGestureEventData
    Ord[DollarGestureEventData]  : Ord DollarGestureEventData
    Show[DollarGestureEventData] : Show DollarGestureEventData

{-# COMPILE GHC Eq[DollarGestureEventData]   = AgdaEq   #-}
{-# COMPILE GHC Ord[DollarGestureEventData]  = AgdaOrd  #-}
{-# COMPILE GHC Show[DollarGestureEventData] = AgdaShow #-}

record DropEventData : Set where
    constructor mkDropEventData
    field
        dropEventFile : CString

{-# COMPILE GHC DropEventData = data SDL.Event.DropEventData (SDL.Event.DropEventData) #-}

postulate
    Eq[DropEventData]   : Eq DropEventData
    Ord[DropEventData]  : Ord DropEventData
    Show[DropEventData] : Show DropEventData

{-# COMPILE GHC Eq[DropEventData]   = AgdaEq   #-}
{-# COMPILE GHC Ord[DropEventData]  = AgdaOrd  #-}
{-# COMPILE GHC Show[DropEventData] = AgdaShow #-}

record UnknownEventData : Set where
    constructor mkUnknownEventData
    field
        unknownEventType : Word32

{-# COMPILE GHC UnknownEventData = data SDL.Event.UnknownEventData (SDL.Event.UnknownEventData) #-}

postulate
    Eq[UnknownEventData]   : Eq UnknownEventData
    Ord[UnknownEventData]  : Ord UnknownEventData
    Show[UnknownEventData] : Show UnknownEventData

{-# COMPILE GHC Eq[UnknownEventData]   = AgdaEq   #-}
{-# COMPILE GHC Ord[UnknownEventData]  = AgdaOrd  #-}
{-# COMPILE GHC Show[UnknownEventData] = AgdaShow #-}

data EventPayload : Set where
    WindowShownEvent               : WindowShownEventData               → EventPayload
    WindowHiddenEvent              : WindowHiddenEventData              → EventPayload
    WindowExposedEvent             : WindowExposedEventData             → EventPayload
    WindowMovedEvent               : WindowMovedEventData               → EventPayload
    WindowResizedEvent             : WindowResizedEventData             → EventPayload
    WindowSizeChangedEvent         : WindowSizeChangedEventData         → EventPayload
    WindowMinimizedEvent           : WindowMinimizedEventData           → EventPayload
    WindowMaximizedEvent           : WindowMaximizedEventData           → EventPayload
    WindowRestoredEvent            : WindowRestoredEventData            → EventPayload
    WindowGainedMouseFocusEvent    : WindowGainedMouseFocusEventData    → EventPayload
    WindowLostMouseFocusEvent      : WindowLostMouseFocusEventData      → EventPayload
    WindowGainedKeyboardFocusEvent : WindowGainedKeyboardFocusEventData → EventPayload
    WindowLostKeyboardFocusEvent   : WindowLostKeyboardFocusEventData   → EventPayload
    WindowClosedEvent              : WindowClosedEventData              → EventPayload
    KeyboardEvent                  : KeyboardEventData                  → EventPayload
    TextEditingEvent               : TextEditingEventData               → EventPayload
    TextInputEvent                 : TextInputEventData                 → EventPayload
    KeymapChangedEvent             :                                      EventPayload
    MouseMotionEvent               : MouseMotionEventData               → EventPayload
    MouseButtonEvent               : MouseButtonEventData               → EventPayload
    MouseWheelEvent                : MouseWheelEventData                → EventPayload
    JoyAxisEvent                   : JoyAxisEventData                   → EventPayload
    JoyBallEvent                   : JoyBallEventData                   → EventPayload
    JoyHatEvent                    : JoyHatEventData                    → EventPayload
    JoyButtonEvent                 : JoyButtonEventData                 → EventPayload
    JoyDeviceEvent                 : JoyDeviceEventData                 → EventPayload
    ControllerAxisEvent            : ControllerAxisEventData            → EventPayload
    ControllerButtonEvent          : ControllerButtonEventData          → EventPayload
    ControllerDeviceEvent          : ControllerDeviceEventData          → EventPayload
    AudioDeviceEvent               : AudioDeviceEventData               → EventPayload
    QuitEvent                      :                                      EventPayload
    UserEvent                      : UserEventData                      → EventPayload
    SysWMEvent                     : SysWMEventData                     → EventPayload
    TouchFingerEvent               : TouchFingerEventData               → EventPayload
    TouchFingerMotionEvent         : TouchFingerMotionEventData         → EventPayload
    MultiGestureEvent              : MultiGestureEventData              → EventPayload
    DollarGestureEvent             : DollarGestureEventData             → EventPayload
    DropEvent                      : DropEventData                      → EventPayload
    ClipboardUpdateEvent           :                                      EventPayload
    UnknownEvent                   : UnknownEventData                   → EventPayload

{-# COMPILE GHC EventPayload = data SDL.Event.EventPayload
    ( SDL.Event.WindowShownEvent
    | SDL.Event.WindowHiddenEvent
    | SDL.Event.WindowExposedEvent
    | SDL.Event.WindowMovedEvent
    | SDL.Event.WindowResizedEvent
    | SDL.Event.WindowSizeChangedEvent
    | SDL.Event.WindowMinimizedEvent
    | SDL.Event.WindowMaximizedEvent
    | SDL.Event.WindowRestoredEvent
    | SDL.Event.WindowGainedMouseFocusEvent
    | SDL.Event.WindowLostMouseFocusEvent
    | SDL.Event.WindowGainedKeyboardFocusEvent
    | SDL.Event.WindowLostKeyboardFocusEvent
    | SDL.Event.WindowClosedEvent
    | SDL.Event.KeyboardEvent
    | SDL.Event.TextEditingEvent
    | SDL.Event.TextInputEvent
    | SDL.Event.KeymapChangedEvent
    | SDL.Event.MouseMotionEvent
    | SDL.Event.MouseButtonEvent
    | SDL.Event.MouseWheelEvent
    | SDL.Event.JoyAxisEvent
    | SDL.Event.JoyBallEvent
    | SDL.Event.JoyHatEvent
    | SDL.Event.JoyButtonEvent
    | SDL.Event.JoyDeviceEvent
    | SDL.Event.ControllerAxisEvent
    | SDL.Event.ControllerButtonEvent
    | SDL.Event.ControllerDeviceEvent
    | SDL.Event.AudioDeviceEvent
    | SDL.Event.QuitEvent
    | SDL.Event.UserEvent
    | SDL.Event.SysWMEvent
    | SDL.Event.TouchFingerEvent
    | SDL.Event.TouchFingerMotionEvent
    | SDL.Event.MultiGestureEvent
    | SDL.Event.DollarGestureEvent
    | SDL.Event.DropEvent
    | SDL.Event.ClipboardUpdateEvent
    | SDL.Event.UnknownEvent
    ) #-}

postulate
    Eq[EventPayload]   : Eq EventPayload
    Ord[EventPayload]  : Ord EventPayload
    Show[EventPayload] : Show EventPayload

{-# COMPILE GHC Eq[EventPayload]   = AgdaEq   #-}
{-# COMPILE GHC Ord[EventPayload]  = AgdaOrd  #-}
{-# COMPILE GHC Show[EventPayload] = AgdaShow #-}

record Event : Set where
    constructor mkEvent
    field
        eventTimestamp : Timestamp
        eventPayload : EventPayload

postulate
    Eq[Event]   : Eq Event
    Ord[Event]  : Ord Event
    Show[Event] : Show Event

{-# COMPILE GHC Eq[Event]   = AgdaEq   #-}
{-# COMPILE GHC Ord[Event]  = AgdaOrd  #-}
{-# COMPILE GHC Show[Event] = AgdaShow #-}

-- Polling events

postulate
    pollEvent        : ⦃ MonadIO M ⦄ → M (Liftℓ _ (Maybe Event))
    pollEvents       : ⦃ MonadIO M ⦄ → M (Liftℓ _ (List Event))
    mapEvents        : ⦃ MonadIO M ⦄ → (Event → M ⊤′) → M ⊤′
    pumpEvents       : ⦃ MonadIO M ⦄ → M ⊤′
    waitEvent        : ⦃ MonadIO M ⦄ → M (Liftℓ _ Event)
    waitEventTimeout : ⦃ MonadIO M ⦄ → CInt → M (Liftℓ _ (Maybe Event))

{-# COMPILE GHC pollEvent        = \ mℓ m AgdaMonadIO -> SDL.Event.pollEvent        #-}
{-# COMPILE GHC pollEvents       = \ mℓ m AgdaMonadIO -> SDL.Event.pollEvents       #-}
{-# COMPILE GHC mapEvents        = \ mℓ m AgdaMonadIO -> SDL.Event.mapEvents        #-}
{-# COMPILE GHC pumpEvents       = \ mℓ m AgdaMonadIO -> SDL.Event.pumpEvents       #-}
{-# COMPILE GHC waitEvent        = \ mℓ m AgdaMonadIO -> SDL.Event.waitEvent        #-}
{-# COMPILE GHC waitEventTimeout = \ mℓ m AgdaMonadIO -> SDL.Event.waitEventTimeout #-}


-- Registering user events

data EventPushResult : Set where
    EventPushSuccess  : EventPushResult
    EventPushFiltered : EventPushResult
    EventPushFailure  : Text → EventPushResult

{-# COMPILE GHC EventPushResult = data SDL.Event.EventPushResult
    ( SDL.Event.EventPushSuccess
    | SDL.Event.EventPushFiltered
    | SDL.Event.EventPushFailure
    ) #-}

postulate
    Eq[EventPushResult]   : Eq EventPushResult
    Data[EventPushResult] : Data EventPushResult
    Ord[EventPushResult]  : Ord EventPushResult
    Read[EventPushResult] : Read EventPushResult
    Show[EventPushResult] : Show EventPushResult

{-# COMPILE GHC Eq[EventPushResult]   = AgdaEq   #-}
{-# COMPILE GHC Data[EventPushResult] = AgdaData #-}
{-# COMPILE GHC Ord[EventPushResult]  = AgdaOrd  #-}
{-# COMPILE GHC Read[EventPushResult] = AgdaRead #-}
{-# COMPILE GHC Show[EventPushResult] = AgdaShow #-}


record RegisteredEventType (A : Set aℓ) : Set aℓ where
    constructor mkRegisteredEventType
    field
        pushRegisteredEvent : A → IO EventPushResult
        getRegisteredEvent  : Event → IO (Maybe A)

{-# FOREIGN GHC type AgdaRegisteredEventType aℓ = SDL.Event.RegisteredEventType #-}
{-# COMPILE GHC RegisteredEventType = data(1) AgdaRegisteredEventType (SDL.Event.RegisteredEventType) #-}


record RegisteredEventData : Set where
    constructor mkRegisteredEventData
    field
        registeredEventWindow : Maybe Window
        registeredEventCode   : Int32
        registeredEventData1  : Ptr ⊤
        registeredEventData2  : Ptr ⊤

{-# COMPILE GHC RegisteredEventData = data SDL.Event.RegisteredEventData (SDL.Event.RegisteredEventData) #-}

postulate
    Eq[RegisteredEventData]   : Eq RegisteredEventData
    Ord[RegisteredEventData]  : Ord RegisteredEventData
    Show[RegisteredEventData] : Show RegisteredEventData

{-# COMPILE GHC Eq[RegisteredEventData]   = AgdaEq   #-}
{-# COMPILE GHC Ord[RegisteredEventData]  = AgdaOrd  #-}
{-# COMPILE GHC Show[RegisteredEventData] = AgdaShow #-}


postulate
    emptyRegisteredEvent : RegisteredEventData
    registerEvent : ⦃ MonadIO M ⦄ → (RegisteredEventData → Timestamp → IO (Maybe A)) → (A → IO RegisteredEventData) → M (Maybe (RegisteredEventType A))

{-# COMPILE GHC emptyRegisteredEvent = SDL.Event.emptyRegisteredEvent #-}
{-# COMPILE GHC registerEvent = \ mℓ m a AgdaMonadIO -> SDL.Event.registerEvent #-}

-- Watching events

EventWatchCallback : Set
EventWatchCallback = Event → IO ⊤

postulate
    EventWatch : Set

    addEventWatch : ⦃ MonadIO M ⦄ → EventWatchCallback → M (Liftℓ _ EventWatch)
    delEventWatch : ⦃ MonadIO M ⦄ → EventWatch → M ⊤′

{-# COMPILE GHC EventWatch = type SDL.Event.EventWatch #-}

{-# COMPILE GHC addEventWatch = \ mℓ m AgdaMonadIO -> SDL.Event.addEventWatch #-}
{-# COMPILE GHC delEventWatch = \ mℓ m AgdaMonadIO -> SDL.Event.addEventWatch #-}
