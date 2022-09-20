{-# OPTIONS --without-K #-}

module Ffi.Hs.SDL.Raw.Types where

open import Agda.Builtin.IO         using (IO)
open import Agda.Builtin.List       using (List)
open import Agda.Primitive
open import Ffi.Hs.-base.Class      using (Eq; Show; Storable)
open import Ffi.Hs.-base.Unit       using (⊤; ⊤′)
open import Ffi.Hs.Data.Int         using (Int16; Int32; Int64)
open import Ffi.Hs.Data.Word        using (Word8; Word16; Word32; Word64)
open import Ffi.Hs.Foreign.C.String using (CString)
open import Ffi.Hs.Foreign.C.Types  using (CInt; CChar; CUInt; CULong; CSize; CFloat; CDouble)
open import Ffi.Hs.Foreign.Ptr      using (Ptr; FunPtr)
open import Ffi.Hs.SDL.Raw.Enum     using (Scancode; Keycode; LogPriority)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified SDL.Raw.Types
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

-- Common Types

AudioDeviceID : Set
AudioDeviceID = Word32

AudioFormat : Set
AudioFormat = Word16

Cond : Set
Cond = Ptr ⊤

Cursor : Set
Cursor = Ptr ⊤

FingerID : Set
FingerID = Int64

GameController : Set
GameController = Ptr ⊤

GestureID : Set
GestureID = Int64

GLContext : Set
GLContext = Ptr ⊤

Haptic : Set
Haptic = Ptr ⊤

Joystick : Set
Joystick = Ptr ⊤

JoystickID : Set
JoystickID = Int32

Mutex : Set
Mutex = Ptr ⊤

Renderer : Set
Renderer = Ptr ⊤

Sem : Set
Sem = Ptr ⊤

SpinLock : Set
SpinLock = CInt

SysWMinfo : Set
SysWMinfo = Ptr ⊤

SysWMmsg : Set
SysWMmsg = Ptr ⊤

Texture : Set
Texture = Ptr ⊤

Thread : Set
Thread = Ptr ⊤

ThreadID : Set
ThreadID = CULong

TimerID : Set
TimerID = CInt

TLSID : Set
TLSID = CUInt

TouchID : Set
TouchID = Int64

VkInstance : Set
VkInstance = Ptr ⊤

VkSurfaceKHR : Set
VkSurfaceKHR = Word64

Window : Set
Window = Ptr ⊤

-- Data Structures

record Atomic : Set where
    constructor mkAtomic
    field
        atomicValue : CInt

{-# COMPILE GHC Atomic = data SDL.Raw.Types.Atomic (SDL.Raw.Types.Atomic) #-}

postulate
    Eq[Atomic]       : Eq Atomic
    Show[Atomic]     : Show Atomic
    Storable[Atomic] : Storable Atomic

{-# COMPILE GHC Eq[Atomic]       = AgdaEq       #-}
{-# COMPILE GHC Show[Atomic]     = AgdaShow     #-}
{-# COMPILE GHC Storable[Atomic] = AgdaStorable #-}


record AudioCVT : Set where
    constructor mkAudioCVT
    field
        audioCVTNeeded    : CInt
        audioCVTSrcFormat : AudioFormat
        audioCVTDstFormat : AudioFormat
        audioCVTRateIncr  : CDouble
        audioCVTBuf       : Ptr Word8
        audioCVTLen       : CInt
        audioCVTLenCvt    : CInt
        audioCVTLenMult   : CInt
        audioCVTLenRatio  : CDouble

{-# COMPILE GHC AudioCVT = data SDL.Raw.Types.AudioCVT (SDL.Raw.Types.AudioCVT) #-}

postulate
    Eq[AudioCVT]       : Eq AudioCVT
    Show[AudioCVT]     : Show AudioCVT
    Storable[AudioCVT] : Storable AudioCVT

{-# COMPILE GHC Eq[AudioCVT]       = AgdaEq       #-}
{-# COMPILE GHC Show[AudioCVT]     = AgdaShow     #-}
{-# COMPILE GHC Storable[AudioCVT] = AgdaStorable #-}


record Color : Set where
    constructor mkColor
    field
        colorR : Word8
        colorG : Word8
        colorB : Word8
        colorA : Word8

{-# COMPILE GHC Color = data SDL.Raw.Types.Color (SDL.Raw.Types.Color) #-}

postulate
    Eq[Color]       : Eq Color
    Show[Color]     : Show Color
    Storable[Color] : Storable Color

{-# COMPILE GHC Eq[Color]       = AgdaEq       #-}
{-# COMPILE GHC Show[Color]     = AgdaShow     #-}
{-# COMPILE GHC Storable[Color] = AgdaStorable #-}


record DisplayMode : Set where
    constructor mkDisplayMode
    field
        displayModeFormat      : Word32
        displayModeW           : CInt
        displayModeH           : CInt
        displayModeRefreshRate : CInt
        displayModeDriverData  : Ptr ⊤

{-# COMPILE GHC DisplayMode = data SDL.Raw.Types.DisplayMode (SDL.Raw.Types.DisplayMode) #-}

postulate
    Eq[DisplayMode]       : Eq DisplayMode
    Show[DisplayMode]     : Show DisplayMode
    Storable[DisplayMode] : Storable DisplayMode

{-# COMPILE GHC Eq[DisplayMode]       = AgdaEq       #-}
{-# COMPILE GHC Show[DisplayMode]     = AgdaShow     #-}
{-# COMPILE GHC Storable[DisplayMode] = AgdaStorable #-}


record Keysym : Set where
    constructor mkKeysym
    field
        keysymScancode : Scancode
        keysymKeycode  : Keycode
        keysymMod      : Word16

{-# COMPILE GHC Keysym = data SDL.Raw.Types.Keysym (SDL.Raw.Types.Keysym) #-}

postulate
    Eq[Keysym]       : Eq Keysym
    Show[Keysym]     : Show Keysym
    Storable[Keysym] : Storable Keysym

{-# COMPILE GHC Eq[Keysym]       = AgdaEq       #-}
{-# COMPILE GHC Show[Keysym]     = AgdaShow     #-}
{-# COMPILE GHC Storable[Keysym] = AgdaStorable #-}


-- todo: getters (postulate + compile or in agda, using recselerror)
data Event : Set where
    WindowEvent           : Word32 → Word32 → Word32 → Word8 → Int32 → Int32 → Event
    KeyboardEvent         : Word32 → Word32 → Word32 → Word8 → Word8 → Keysym → Event
    TextEditingEvent      : Word32 → Word32 → Word32 → List CChar → Int32 → Int32 → Event
    TextInputEvent        : Word32 → Word32 → Word32 → List CChar → Event
    KeymapChangedEvent    : Word32 → Word32 → Event
    MouseMotionEvent      : Word32 → Word32 → Word32 → Word32 → Word32 → Int32 → Int32 → Int32 → Int32 → Event
    MouseButtonEvent      : Word32 → Word32 → Word32 → Word32 → Word8 → Word8 → Word8 → Int32 → Int32 → Event
    MouseWheelEvent       : Word32 → Word32 → Word32 → Word32 → Int32 → Int32 → Word32 → Event
    JoyAxisEvent          : Word32 → Word32 → JoystickID → Word8 → Int16 → Event
    JoyBallEvent          : Word32 → Word32 → JoystickID → Word8 → Int16 → Int16 → Event
    JoyHatEvent           : Word32 → Word32 → JoystickID → Word8 → Word8 → Event
    JoyButtonEvent        : Word32 → Word32 → JoystickID → Word8 → Word8 → Event
    JoyDeviceEvent        : Word32 → Word32 → Int32 → Event
    ControllerAxisEvent   : Word32 → Word32 → JoystickID → Word8 → Int16 → Event
    ControllerButtonEvent : Word32 → Word32 → JoystickID → Word8 → Word8 → Event
    ControllerDeviceEvent : Word32 → Word32 → Int32 → Event
    AudioDeviceEvent      : Word32 → Word32 → Word32 → Word8 → Event
    QuitEvent             : Word32 → Word32 → Event
    UserEvent             : Word32 → Word32 → Word32 → Int32 → Ptr ⊤ → Ptr ⊤ → Event
    SysWMEvent            : Word32 → Word32 → SysWMmsg → Event
    TouchFingerEvent      : Word32 → Word32 → TouchID → FingerID → CFloat → CFloat → CFloat → CFloat → CFloat → Event
    MultiGestureEvent     : Word32 → Word32 → TouchID → CFloat → CFloat → CFloat → CFloat → Word16 → Event
    DollarGestureEvent    : Word32 → Word32 → TouchID → GestureID → Word32 → CFloat → CFloat → CFloat → Event
    DropEvent             : Word32 → Word32 → CString → Event
    ClipboardUpdateEvent  : Word32 → Word32 → Event
    UnknownEvent          : Word32 → Word32 → Event

{-# COMPILE GHC Event = data SDL.Raw.Types.Event
    ( SDL.Raw.Types.WindowEvent
    | SDL.Raw.Types.KeyboardEvent
    | SDL.Raw.Types.TextEditingEvent
    | SDL.Raw.Types.TextInputEvent
    | SDL.Raw.Types.KeymapChangedEvent
    | SDL.Raw.Types.MouseMotionEvent
    | SDL.Raw.Types.MouseButtonEvent
    | SDL.Raw.Types.MouseWheelEvent
    | SDL.Raw.Types.JoyAxisEvent
    | SDL.Raw.Types.JoyBallEvent
    | SDL.Raw.Types.JoyHatEvent
    | SDL.Raw.Types.JoyButtonEvent
    | SDL.Raw.Types.JoyDeviceEvent
    | SDL.Raw.Types.ControllerAxisEvent
    | SDL.Raw.Types.ControllerButtonEvent
    | SDL.Raw.Types.ControllerDeviceEvent
    | SDL.Raw.Types.AudioDeviceEvent
    | SDL.Raw.Types.QuitEvent
    | SDL.Raw.Types.UserEvent
    | SDL.Raw.Types.SysWMEvent
    | SDL.Raw.Types.TouchFingerEvent
    | SDL.Raw.Types.MultiGestureEvent
    | SDL.Raw.Types.DollarGestureEvent
    | SDL.Raw.Types.DropEvent
    | SDL.Raw.Types.ClipboardUpdateEvent
    | SDL.Raw.Types.UnknownEvent
    ) #-}

postulate
    Eq[Event]   : Eq Event
    Show[Event] : Show Event

{-# COMPILE GHC Eq[Event]       = AgdaEq       #-}
{-# COMPILE GHC Show[Event]     = AgdaShow     #-}

eventType : Event → Word32
eventType (WindowEvent x _ _ _ _ _)            = x
eventType (KeyboardEvent x _ _ _ _ _)          = x
eventType (TextEditingEvent x _ _ _ _ _)       = x
eventType (TextInputEvent x _ _ _)             = x
eventType (KeymapChangedEvent x _)             = x
eventType (MouseMotionEvent x _ _ _ _ _ _ _ _) = x
eventType (MouseButtonEvent x _ _ _ _ _ _ _ _) = x
eventType (MouseWheelEvent x _ _ _ _ _ _)      = x
eventType (JoyAxisEvent x _ _ _ _)             = x
eventType (JoyBallEvent x _ _ _ _ _)           = x
eventType (JoyHatEvent x _ _ _ _)              = x
eventType (JoyButtonEvent x _ _ _ _)           = x
eventType (JoyDeviceEvent x _ _)               = x
eventType (ControllerAxisEvent x _ _ _ _)      = x
eventType (ControllerButtonEvent x _ _ _ _)    = x
eventType (ControllerDeviceEvent x _ _)        = x
eventType (AudioDeviceEvent x _ _ _)           = x
eventType (QuitEvent x _)                      = x
eventType (UserEvent x _ _ _ _ _)              = x
eventType (SysWMEvent x _ _)                   = x
eventType (TouchFingerEvent x _ _ _ _ _ _ _ _) = x
eventType (MultiGestureEvent x _ _ _ _ _ _ _)  = x
eventType (DollarGestureEvent x _ _ _ _ _ _ _) = x
eventType (DropEvent x _ _)                    = x
eventType (ClipboardUpdateEvent x _)           = x
eventType (UnknownEvent x _)                   = x

eventTimestamp : Event → Word32
eventTimestamp (WindowEvent _ x _ _ _ _)            = x
eventTimestamp (KeyboardEvent _ x _ _ _ _)          = x
eventTimestamp (TextEditingEvent _ x _ _ _ _)       = x
eventTimestamp (TextInputEvent _ x _ _)             = x
eventTimestamp (KeymapChangedEvent _ x)             = x
eventTimestamp (MouseMotionEvent _ x _ _ _ _ _ _ _) = x
eventTimestamp (MouseButtonEvent _ x _ _ _ _ _ _ _) = x
eventTimestamp (MouseWheelEvent _ x _ _ _ _ _)      = x
eventTimestamp (JoyAxisEvent _ x _ _ _)             = x
eventTimestamp (JoyBallEvent _ x _ _ _ _)           = x
eventTimestamp (JoyHatEvent _ x _ _ _)              = x
eventTimestamp (JoyButtonEvent _ x _ _ _)           = x
eventTimestamp (JoyDeviceEvent _ x _)               = x
eventTimestamp (ControllerAxisEvent _ x _ _ _)      = x
eventTimestamp (ControllerButtonEvent _ x _ _ _)    = x
eventTimestamp (ControllerDeviceEvent _ x _)        = x
eventTimestamp (AudioDeviceEvent _ x _ _)           = x
eventTimestamp (QuitEvent _ x)                      = x
eventTimestamp (UserEvent _ x _ _ _ _)              = x
eventTimestamp (SysWMEvent _ x _)                   = x
eventTimestamp (TouchFingerEvent _ x _ _ _ _ _ _ _) = x
eventTimestamp (MultiGestureEvent _ x _ _ _ _ _ _)  = x
eventTimestamp (DollarGestureEvent _ x _ _ _ _ _ _) = x
eventTimestamp (DropEvent _ x _)                    = x
eventTimestamp (ClipboardUpdateEvent _ x)           = x
eventTimestamp (UnknownEvent _ x)                   = x


record Finger : Set where
    constructor mkFinger
    field
        fingerID       : FingerID
        fingerX        : CFloat
        fingerY        : CFloat
        fingerPressure : CFloat

{-# COMPILE GHC Finger = data SDL.Raw.Types.Finger (SDL.Raw.Types.Finger) #-}

postulate
    Eq[Finger]   : Eq Finger
    Show[Finger] : Show Finger

{-# COMPILE GHC Eq[Finger]   = AgdaEq   #-}
{-# COMPILE GHC Show[Finger] = AgdaShow #-}


-- todo: getters
data GameControllerButtonBind : Set where
    GameControllerButtonBindNone   : GameControllerButtonBind
    GameControllerButtonBindButton : CInt → GameControllerButtonBind
    GameControllerButtonBindAxis   : CInt → GameControllerButtonBind
    GameControllerButtonBindHat    : CInt → CInt → GameControllerButtonBind

postulate
    Eq[GameControllerButtonBind]       : Eq GameControllerButtonBind
    Show[GameControllerButtonBind]     : Show GameControllerButtonBind
    Storable[GameControllerButtonBind] : Storable GameControllerButtonBind

{-# COMPILE GHC Eq[GameControllerButtonBind]       = AgdaEq       #-}
{-# COMPILE GHC Show[GameControllerButtonBind]     = AgdaShow     #-}
{-# COMPILE GHC Storable[GameControllerButtonBind] = AgdaStorable #-}


record HapticDirection : Set where
    constructor mkHapticDirection
    field
        hapticDirectionType : Word8
        hapticDirectionX    : Int32
        hapticDirectionY    : Int32
        hapticDirectionZ    : Int32

{-# COMPILE GHC HapticDirection = data SDL.Raw.Types.HapticDirection (SDL.Raw.Types.HapticDirection) #-}

postulate
    Eq[HapticDirection]       : Eq HapticDirection
    Show[HapticDirection]     : Show HapticDirection
    Storable[HapticDirection] : Storable HapticDirection

{-# COMPILE GHC Eq[HapticDirection]       = AgdaEq       #-}
{-# COMPILE GHC Show[HapticDirection]     = AgdaShow     #-}
{-# COMPILE GHC Storable[HapticDirection] = AgdaStorable #-}


-- todo: getters
data HapticEffect : Set where
    HapticConstant  : Word16 → HapticDirection → Word32 → Word16 → Word16 → Word16 → Int16 → Word16 → Word16 → Word16 → Word16 → HapticEffect
    HapticPeriodic  : Word16 → HapticDirection → Word32 → Word16 → Word16 → Word16 → Word16 → Int16 → Int16 → Word16 → Word16 → Word16 → Word16 → Word16 → HapticEffect
    HapticCondition : Word16 → Word32 → Word16 → Word16 → Word16 → List Word16 → List Word16 → List Int16 → List Int16 → List Word16 → List Int16 → HapticEffect
    HapticRamp      : Word16 → HapticDirection → Word32 → Word16 → Word16 → Word16 → Int16 → Int16 → Word16 → Word16 → Word16 → Word16 → HapticEffect
    HapticLeftRight : Word16 → Word32 → Word16 → Word16 → HapticEffect
    HapticCustom    : Word16 → HapticDirection → Word32 → Word16 → Word16 → Word16 → Word8 → Word16 → Word16 → Ptr Word16 → Word16 → Word16 → Word16 → Word16 → HapticEffect

hapticEffectType : HapticEffect → Word16
hapticEffectType (HapticConstant x _ _ _ _ _ _ _ _ _ _)       = x
hapticEffectType (HapticPeriodic x _ _ _ _ _ _ _ _ _ _ _ _ _) = x
hapticEffectType (HapticCondition x _ _ _ _ _ _ _ _ _ _)      = x
hapticEffectType (HapticRamp x _ _ _ _ _ _ _ _ _ _ _)         = x
hapticEffectType (HapticLeftRight x _ _ _)                    = x
hapticEffectType (HapticCustom x _ _ _ _ _ _ _ _ _ _ _ _ _)   = x

postulate
    Eq[HapticEffect]       : Eq HapticEffect
    Show[HapticEffect]     : Show HapticEffect
    Storable[HapticEffect] : Storable HapticEffect

{-# COMPILE GHC Eq[HapticEffect]       = AgdaEq       #-}
{-# COMPILE GHC Show[HapticEffect]     = AgdaShow     #-}
{-# COMPILE GHC Storable[HapticEffect] = AgdaStorable #-}


record JoystickGUID : Set where
    constructor mkJoystickGUID
    field
        joystickGUID : List Word8

{-# COMPILE GHC JoystickGUID = data SDL.Raw.Types.JoystickGUID (SDL.Raw.Types.JoystickGUID) #-}

postulate
    Eq[JoystickGUID]       : Eq JoystickGUID
    Show[JoystickGUID]     : Show JoystickGUID
    Storable[JoystickGUID] : Storable JoystickGUID

{-# COMPILE GHC Eq[JoystickGUID]       = AgdaEq       #-}
{-# COMPILE GHC Show[JoystickGUID]     = AgdaShow     #-}
{-# COMPILE GHC Storable[JoystickGUID] = AgdaStorable #-}


record MessageBoxButtonData : Set where
    constructor mkMessageBoxButtonData
    field
        messageBoxButtonDataFlags : Word32
        messageBoxButtonButtonID  : CInt
        messageBoxButtonText      : CString

{-# COMPILE GHC MessageBoxButtonData = data SDL.Raw.Types.MessageBoxButtonData (SDL.Raw.Types.MessageBoxButtonData) #-}

postulate
    Eq[MessageBoxButtonData]       : Eq MessageBoxButtonData
    Show[MessageBoxButtonData]     : Show MessageBoxButtonData
    Storable[MessageBoxButtonData] : Storable MessageBoxButtonData

{-# COMPILE GHC Eq[MessageBoxButtonData]       = AgdaEq       #-}
{-# COMPILE GHC Show[MessageBoxButtonData]     = AgdaShow     #-}
{-# COMPILE GHC Storable[MessageBoxButtonData] = AgdaStorable #-}


record MessageBoxColor : Set where
    constructor mkMessageBoxColor
    field
        messageBoxColorR : Word8
        messageBoxColorG : Word8
        messageBoxColorB : Word8

{-# COMPILE GHC MessageBoxColor = data SDL.Raw.Types.MessageBoxColor (SDL.Raw.Types.MessageBoxColor) #-}

postulate
    Eq[MessageBoxColor]       : Eq MessageBoxColor
    Show[MessageBoxColor]     : Show MessageBoxColor
    Storable[MessageBoxColor] : Storable MessageBoxColor

{-# COMPILE GHC Eq[MessageBoxColor]       = AgdaEq       #-}
{-# COMPILE GHC Show[MessageBoxColor]     = AgdaShow     #-}
{-# COMPILE GHC Storable[MessageBoxColor] = AgdaStorable #-}


record MessageBoxColorScheme : Set where
    constructor mkMessageBoxColorScheme
    field
        messageBoxColorSchemeColorBackground       : MessageBoxColor
        messageBoxColorSchemeColorText             : MessageBoxColor
        messageBoxColorSchemeColorButtonBorder     : MessageBoxColor
        messageBoxColorSchemeColorButtonBackground : MessageBoxColor
        messageBoxColorSchemeColorButtonSelected   : MessageBoxColor

{-# COMPILE GHC MessageBoxColorScheme = data SDL.Raw.Types.MessageBoxColorScheme (SDL.Raw.Types.MessageBoxColorScheme) #-}

postulate
    Eq[MessageBoxColorScheme]       : Eq MessageBoxColorScheme
    Show[MessageBoxColorScheme]     : Show MessageBoxColorScheme
    Storable[MessageBoxColorScheme] : Storable MessageBoxColorScheme

{-# COMPILE GHC Eq[MessageBoxColorScheme]       = AgdaEq       #-}
{-# COMPILE GHC Show[MessageBoxColorScheme]     = AgdaShow     #-}
{-# COMPILE GHC Storable[MessageBoxColorScheme] = AgdaStorable #-}


record MessageBoxData : Set where
    constructor mkMessageBoxData
    field
        messageBoxDataFlags       : Word32
        messageBoxDataWindow      : Window
        messageBoxDataTitle       : CString
        messageBoxDataMessage     : CString
        messageBoxDataNumButtons  : CInt
        messageBoxDataButtons     : Ptr MessageBoxButtonData
        messageBoxDataColorScheme : Ptr MessageBoxColorScheme

{-# COMPILE GHC MessageBoxData = data SDL.Raw.Types.MessageBoxData (SDL.Raw.Types.MessageBoxData) #-}

postulate
    Eq[MessageBoxData]       : Eq MessageBoxData
    Show[MessageBoxData]     : Show MessageBoxData
    Storable[MessageBoxData] : Storable MessageBoxData

{-# COMPILE GHC Eq[MessageBoxData]       = AgdaEq       #-}
{-# COMPILE GHC Show[MessageBoxData]     = AgdaShow     #-}
{-# COMPILE GHC Storable[MessageBoxData] = AgdaStorable #-}


record Palette : Set where
    constructor mkPalette
    field
        paletteNColors : CInt
        paletteColors  : Ptr Color

{-# COMPILE GHC Palette = data SDL.Raw.Types.Palette (SDL.Raw.Types.Palette) #-}

postulate
    Eq[Palette]       : Eq Palette
    Show[Palette]     : Show Palette
    Storable[Palette] : Storable Palette

{-# COMPILE GHC Eq[Palette]       = AgdaEq       #-}
{-# COMPILE GHC Show[Palette]     = AgdaShow     #-}
{-# COMPILE GHC Storable[Palette] = AgdaStorable #-}


record PixelFormat : Set where
    constructor mkPixelFormat
    field
        pixelFormatFormat        : Word32
        pixelFormatPalette       : Ptr Palette
        pixelFormatBitsPerPixel  : Word8
        pixelFormatBytesPerPixel : Word8
        pixelFormatRMask         : Word32
        pixelFormatGMask         : Word32
        pixelFormatBMask         : Word32
        pixelFormatAMask         : Word32

{-# COMPILE GHC PixelFormat = data SDL.Raw.Types.PixelFormat (SDL.Raw.Types.PixelFormat) #-}

postulate
    Eq[PixelFormat]       : Eq PixelFormat
    Show[PixelFormat]     : Show PixelFormat
    Storable[PixelFormat] : Storable PixelFormat

{-# COMPILE GHC Eq[PixelFormat]       = AgdaEq       #-}
{-# COMPILE GHC Show[PixelFormat]     = AgdaShow     #-}
{-# COMPILE GHC Storable[PixelFormat] = AgdaStorable #-}


record Point : Set where
    constructor mkPoint
    field
        pointX : CInt
        pointY : CInt

{-# COMPILE GHC Point = data SDL.Raw.Types.Point (SDL.Raw.Types.Point) #-}

postulate
    Eq[Point]       : Eq Point
    Show[Point]     : Show Point
    Storable[Point] : Storable Point

{-# COMPILE GHC Eq[Point]       = AgdaEq       #-}
{-# COMPILE GHC Show[Point]     = AgdaShow     #-}
{-# COMPILE GHC Storable[Point] = AgdaStorable #-}


record Rect : Set where
    constructor mkRect
    field
        rectX : CInt
        rectY : CInt
        rectW : CInt
        rectH : CInt

{-# COMPILE GHC Rect = data SDL.Raw.Types.Rect (SDL.Raw.Types.Rect) #-}

postulate
    Eq[Rect]       : Eq Rect
    Show[Rect]     : Show Rect
    Storable[Rect] : Storable Rect

{-# COMPILE GHC Eq[Rect]       = AgdaEq       #-}
{-# COMPILE GHC Show[Rect]     = AgdaShow     #-}
{-# COMPILE GHC Storable[Rect] = AgdaStorable #-}


record RendererInfo : Set where
    constructor mkRendererInfo
    field
        rendererInfoName              : CString
        rendererInfoFlags             : Word32
        rendererInfoNumTextureFormats : Word32
        rendererInfoTextureFormats    : List Word32
        rendererInfoMaxTextureWidth   : CInt
        rendererInfoMaxTextureHeight  : CInt

{-# COMPILE GHC RendererInfo = data SDL.Raw.Types.RendererInfo (SDL.Raw.Types.RendererInfo) #-}

postulate
    Eq[RendererInfo]       : Eq RendererInfo
    Show[RendererInfo]     : Show RendererInfo
    Storable[RendererInfo] : Storable RendererInfo

{-# COMPILE GHC Eq[RendererInfo]       = AgdaEq       #-}
{-# COMPILE GHC Show[RendererInfo]     = AgdaShow     #-}
{-# COMPILE GHC Storable[RendererInfo] = AgdaStorable #-}


{-# NO_POSITIVITY_CHECK #-}
record RWops : Set where
    constructor mkRWops
    field
        rwopsSize  : FunPtr (Ptr RWops → IO Int64)
        rwopsSeek  : FunPtr (Ptr RWops → Int64 → CInt → IO Int64)
        rwopsRead  : FunPtr (Ptr RWops → Ptr ⊤ → CSize → CSize → IO CSize)
        rwopsWrite : FunPtr (Ptr RWops → Ptr ⊤ → CSize → CSize → IO CSize)
        rwopsClose : FunPtr (Ptr RWops → IO CInt)
        rwopsType  : Word32

{-# COMPILE GHC RWops = data SDL.Raw.Types.RWops (SDL.Raw.Types.RWops) #-}

postulate
    Eq[RWops]       : Eq RWops
    Show[RWops]     : Show RWops
    Storable[RWops] : Storable RWops

{-# COMPILE GHC Eq[RWops]       = AgdaEq       #-}
{-# COMPILE GHC Show[RWops]     = AgdaShow     #-}
{-# COMPILE GHC Storable[RWops] = AgdaStorable #-}


record Surface : Set where
    constructor mkSurface
    field
        surfaceFormat   : Ptr PixelFormat
        surfaceW        : CInt
        surfaceH        : CInt
        surfacePixels   : Ptr ⊤
        surfaceUserdata : Ptr ⊤
        surfaceClipRect : Rect
        surfaceRefcount : CInt

{-# COMPILE GHC Surface = data SDL.Raw.Types.Surface (SDL.Raw.Types.Surface) #-}

postulate
    Eq[Surface]       : Eq Surface
    Show[Surface]     : Show Surface
    Storable[Surface] : Storable Surface

{-# COMPILE GHC Eq[Surface]       = AgdaEq       #-}
{-# COMPILE GHC Show[Surface]     = AgdaShow     #-}
{-# COMPILE GHC Storable[Surface] = AgdaStorable #-}


record Version : Set where
    constructor mkVersion
    field
        versionMajor : Word8
        versionMinor : Word8
        versionPatch : Word8

{-# COMPILE GHC Version = data SDL.Raw.Types.Version (SDL.Raw.Types.Version) #-}

postulate
    Eq[Version]       : Eq Version
    Show[Version]     : Show Version
    Storable[Version] : Storable Version

{-# COMPILE GHC Eq[Version]       = AgdaEq       #-}
{-# COMPILE GHC Show[Version]     = AgdaShow     #-}
{-# COMPILE GHC Storable[Version] = AgdaStorable #-}


-- Function Types

VkGetInstanceProcAddrFunc : Set
VkGetInstanceProcAddrFunc = VkInstance → CString → IO (FunPtr ⊤)

AudioCallback : Set
AudioCallback = FunPtr (Ptr ⊤ → Ptr Word8 → CInt → IO ⊤)

EventFilter : Set
EventFilter = FunPtr (Ptr ⊤ → Ptr Event → IO CInt)

HintCallback : Set
HintCallback = FunPtr (Ptr ⊤ → CString → CString → CString → IO ⊤)

LogOutputFunction : Set
LogOutputFunction = FunPtr (Ptr ⊤ → CInt → LogPriority → CString → IO ⊤)

ThreadFunction : Set
ThreadFunction = FunPtr (Ptr ⊤ → IO CInt)

TimerCallback : Set
TimerCallback = FunPtr (Word32 → Ptr ⊤ → IO Word32)

postulate
    mkAudioCallback     : (Ptr ⊤ → Ptr Word8 → CInt → IO ⊤) → IO AudioCallback
    mkEventFilter       : (Ptr ⊤ → Ptr Event → IO CInt) → IO EventFilter
    mkHintCallback      : (Ptr ⊤ → CString → CString → CString → IO ⊤) → IO HintCallback
    mkLogOutputFunction : (Ptr ⊤ → CInt → LogPriority → CString → IO ⊤) → IO LogOutputFunction
    mkThreadFunction    : (Ptr ⊤ → IO CInt) → IO ThreadFunction
    mkTimerCallback     : (Word32 → Ptr ⊤ → IO Word32) → IO TimerCallback

{-# COMPILE GHC mkAudioCallback     = SDL.Raw.Types.mkAudioCallback     #-}
{-# COMPILE GHC mkEventFilter       = SDL.Raw.Types.mkEventFilter       #-}
{-# COMPILE GHC mkHintCallback      = SDL.Raw.Types.mkHintCallback      #-}
{-# COMPILE GHC mkLogOutputFunction = SDL.Raw.Types.mkLogOutputFunction #-}
{-# COMPILE GHC mkThreadFunction    = SDL.Raw.Types.mkThreadFunction    #-}
{-# COMPILE GHC mkTimerCallback     = SDL.Raw.Types.mkTimerCallback     #-}

-- Data Structures

record AudioSpec : Set where
    constructor mkAudioSpec
    field
        audioSpecFreq     : CInt
        audioSpecFormat   : AudioFormat
        audioSpecChannels : Word8
        audioSpecSilence  : Word8
        audioSpecSamples  : Word16
        audioSpecSize     : Word32
        audioSpecCallback : AudioCallback
        audioSpecUserdata : Ptr ⊤

{-# COMPILE GHC AudioSpec = data SDL.Raw.Types.AudioSpec (SDL.Raw.Types.AudioSpec) #-}

postulate
    Eq[AudioSpec]       : Eq AudioSpec
    Show[AudioSpec]     : Show AudioSpec
    Storable[AudioSpec] : Storable AudioSpec

{-# COMPILE GHC Eq[AudioSpec]       = AgdaEq       #-}
{-# COMPILE GHC Show[AudioSpec]     = AgdaShow     #-}
{-# COMPILE GHC Storable[AudioSpec] = AgdaStorable #-}
