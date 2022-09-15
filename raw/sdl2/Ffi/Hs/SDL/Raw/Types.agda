{-# OPTIONS --without-K #-}

module Ffi.Hs.SDL.Raw.Types where

open import Agda.Builtin.IO         using (IO)
open import Agda.Builtin.List       using (List)
open import Agda.Primitive
open import Ffi.Hs.-base.Class      using (Eq; Show; Storable)
open import Ffi.Hs.-base.Unit       using (⊤)
open import Ffi.Hs.Data.Int         using (Int16; Int32; Int64)
open import Ffi.Hs.Data.Word        using (Word8; Word16; Word32; Word64)
open import Ffi.Hs.Foreign.C.String using (CString)
open import Ffi.Hs.Foreign.C.Types  using (CInt; CChar; CUInt; CULong; CSize; CFloat; CDouble)
open import Ffi.Hs.Foreign.Ptr      using (Ptr; FunPtr)
open import Ffi.Hs.SDL.Raw.Enum     using (Scancode; Keycode; LogPriority)

private
    variable
        aℓ bℓ cℓ dℓ ℓ : Level

-- todo: unify some levels
-- todo: compile pragmas

-- Common Types

AudioDeviceID : Set
AudioDeviceID = Word32

AudioFormat : Set
AudioFormat = Word16

Cond : ∀{aℓ} → Set aℓ
Cond {aℓ} = Ptr (⊤ {aℓ})

Cursor : ∀{aℓ} → Set aℓ
Cursor {aℓ} = Ptr (⊤ {aℓ})

FingerID : Set
FingerID = Int64

GameController : ∀{aℓ} → Set aℓ
GameController {aℓ} = Ptr (⊤ {aℓ})

GestureID : Set
GestureID = Int64

GLContext : ∀{aℓ} → Set aℓ
GLContext {aℓ} = Ptr (⊤ {aℓ})

Haptic : ∀{aℓ} → Set aℓ
Haptic {aℓ} = Ptr (⊤ {aℓ})

Joystick : ∀{aℓ} → Set aℓ
Joystick {aℓ} = Ptr (⊤ {aℓ})

JoystickID : Set
JoystickID = Int32

Mutex : ∀{aℓ} → Set aℓ
Mutex {aℓ} = Ptr (⊤ {aℓ})

Renderer : ∀{aℓ} → Set aℓ
Renderer {aℓ} = Ptr (⊤ {aℓ})

Sem : ∀{aℓ} → Set aℓ
Sem {aℓ} = Ptr (⊤ {aℓ})

SpinLock : Set
SpinLock = CInt

SysWMinfo : ∀{aℓ} → Set aℓ
SysWMinfo {aℓ} = Ptr (⊤ {aℓ})

SysWMmsg : ∀{aℓ} → Set aℓ
SysWMmsg {aℓ} = Ptr (⊤ {aℓ})

Texture : ∀{aℓ} → Set aℓ
Texture {aℓ} = Ptr (⊤ {aℓ})

Thread : ∀{aℓ} → Set aℓ
Thread {aℓ} = Ptr (⊤ {aℓ})

ThreadID : Set
ThreadID = CULong

TimerID : Set
TimerID = CInt

TLSID : Set
TLSID = CUInt

TouchID : Set
TouchID = Int64

VkInstance : ∀{aℓ} → Set aℓ
VkInstance {aℓ} = Ptr (⊤ {aℓ})

VkSurfaceKHR : Set
VkSurfaceKHR = Word64

Window : ∀{aℓ} → Set aℓ
Window {aℓ} = Ptr (⊤ {aℓ})

-- Data Structures

record Atomic : Set where
    constructor mkAtomic
    field
        atomicValue : CInt

postulate
    Eq[Atomic]       : Eq Atomic
    Show[Atomic]     : Show Atomic
    Storable[Atomic] : Storable Atomic

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

postulate
    Eq[AudioCVT]       : Eq AudioCVT
    Show[AudioCVT]     : Show AudioCVT
    Storable[AudioCVT] : Storable AudioCVT

record Color : Set where
    constructor mkColor
    field
        colorR : Word8
        colorG : Word8
        colorB : Word8
        colorA : Word8

postulate
    Eq[Color]       : Eq Color
    Show[Color]     : Show Color
    Storable[Color] : Storable Color

record DisplayMode {aℓ} : Set aℓ where
    constructor mkDisplayMode
    field
        displayModeFormat      : Word32
        displayModeW           : CInt
        displayModeH           : CInt
        displayModeRefreshRate : CInt
        displayModeDriverData  : Ptr (⊤ {aℓ})

postulate
    Eq[DisplayMode]       : Eq (DisplayMode {aℓ})
    Show[DisplayMode]     : Show (DisplayMode {aℓ})
    Storable[DisplayMode] : Storable (DisplayMode {aℓ})

record Keysym : Set where
    constructor mkKeysym
    field
        keysymScancode : Scancode
        keysymKeycode  : Keycode
        keysymMod      : Word16

postulate
    Eq[Keysym]       : Eq Keysym
    Show[Keysym]     : Show Keysym
    Storable[Keysym] : Storable Keysym


-- todo: getters (postulate + compile or in agda, using recselerror)
data Event {aℓ bℓ cℓ} : Set (aℓ ⊔ bℓ ⊔ cℓ) where
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
    UserEvent             : Word32 → Word32 → Word32 → Int32 → Ptr (⊤ {aℓ}) → Ptr (⊤ {bℓ}) → Event
    SysWMEvent            : Word32 → Word32 → SysWMmsg {cℓ} → Event
    TouchFingerEvent      : Word32 → Word32 → TouchID → FingerID → CFloat → CFloat → CFloat → CFloat → CFloat → Event
    MultiGestureEvent     : Word32 → Word32 → TouchID → CFloat → CFloat → CFloat → CFloat → Word16 → Event
    DollarGestureEvent    : Word32 → Word32 → TouchID → GestureID → Word32 → CFloat → CFloat → CFloat → Event
    DropEvent             : Word32 → Word32 → CString → Event
    ClipboardUpdateEvent  : Word32 → Word32 → Event
    UnknownEvent          : Word32 → Word32 → Event

postulate
    Eq[Event]   : Eq (Event {aℓ} {bℓ} {cℓ})
    Show[Event] : Show (Event {aℓ} {bℓ} {cℓ})

eventType : Event {aℓ} {bℓ} {cℓ} → Word32
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

eventTimestamp : Event {aℓ} {bℓ} {cℓ} → Word32
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

postulate
    Eq[Finger]   : Eq Finger
    Show[Finger] : Show Finger

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

record HapticDirection : Set where
    constructor mkHapticDirection
    field
        hapticDirectionType : Word8
        hapticDirectionX    : Int32
        hapticDirectionY    : Int32
        hapticDirectionZ    : Int32

postulate
    Eq[HapticDirection]       : Eq HapticDirection
    Show[HapticDirection]     : Show HapticDirection
    Storable[HapticDirection] : Storable HapticDirection

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

record JoystickGUID : Set where
    constructor mkJoystickGUID
    field
        joystickGUID : List Word8

postulate
    Eq[JoystickGUID]       : Eq JoystickGUID
    Show[JoystickGUID]     : Show JoystickGUID
    Storable[JoystickGUID] : Storable JoystickGUID

record MessageBoxButtonData : Set where
    constructor mkMessageBoxButtonData
    field
        messageBoxButtonDataFlags : Word32
        messageBoxButtonButtonID  : CInt
        messageBoxButtonText      : CString

postulate
    Eq[MessageBoxButtonData]       : Eq MessageBoxButtonData
    Show[MessageBoxButtonData]     : Show MessageBoxButtonData
    Storable[MessageBoxButtonData] : Storable MessageBoxButtonData

record MessageBoxColor : Set where
    constructor mkMessageBoxColor
    field
        messageBoxColorR : Word8
        messageBoxColorG : Word8
        messageBoxColorB : Word8

postulate
    Eq[MessageBoxColor]       : Eq MessageBoxColor
    Show[MessageBoxColor]     : Show MessageBoxColor
    Storable[MessageBoxColor] : Storable MessageBoxColor

record MessageBoxColorScheme : Set where
    constructor mkMessageBoxColorScheme
    field
        messageBoxColorSchemeColorBackground       : MessageBoxColor
        messageBoxColorSchemeColorText             : MessageBoxColor
        messageBoxColorSchemeColorButtonBorder     : MessageBoxColor
        messageBoxColorSchemeColorButtonBackground : MessageBoxColor
        messageBoxColorSchemeColorButtonSelected   : MessageBoxColor

postulate
    Eq[MessageBoxColorScheme]       : Eq MessageBoxColorScheme
    Show[MessageBoxColorScheme]     : Show MessageBoxColorScheme
    Storable[MessageBoxColorScheme] : Storable MessageBoxColorScheme

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

postulate
    Eq[MessageBoxData]       : Eq MessageBoxData
    Show[MessageBoxData]     : Show MessageBoxData
    Storable[MessageBoxData] : Storable MessageBoxData

record Palette : Set where
    constructor mkPalette
    field
        paletteNColors : CInt
        paletteColors  : Ptr Color

postulate
    Eq[Palette]       : Eq Palette
    Show[Palette]     : Show Palette
    Storable[Palette] : Storable Palette

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

postulate
    Eq[PixelFormat]       : Eq PixelFormat
    Show[PixelFormat]     : Show PixelFormat
    Storable[PixelFormat] : Storable PixelFormat

record Point : Set where
    constructor mkPoint
    field
        pointX : CInt
        pointY : CInt

postulate
    Eq[Point]       : Eq Point
    Show[Point]     : Show Point
    Storable[Point] : Storable Point

record Rect : Set where
    constructor mkRect
    field
        rectX : CInt
        rectY : CInt
        rectW : CInt
        rectH : CInt

postulate
    Eq[Rect]       : Eq Rect
    Show[Rect]     : Show Rect
    Storable[Rect] : Storable Rect

record RendererInfo : Set where
    constructor mkRendererInfo
    field
        rendererInfoName              : CString
        rendererInfoFlags             : Word32
        rendererInfoNumTextureFormats : Word32
        rendererInfoTextureFormats    : List Word32
        rendererInfoMaxTextureWidth   : CInt
        rendererInfoMaxTextureHeight  : CInt

postulate
    Eq[RendererInfo]       : Eq RendererInfo
    Show[RendererInfo]     : Show RendererInfo
    Storable[RendererInfo] : Storable RendererInfo

{-# NO_POSITIVITY_CHECK #-}
record RWops {ℓ} : Set ℓ where
    constructor mkRWops
    field
        rwopsSize  : FunPtr (Ptr (RWops {ℓ}) → IO Int64)
        rwopsSeek  : FunPtr (Ptr (RWops {ℓ}) → Int64 → CInt → IO Int64)
        rwopsRead  : FunPtr (Ptr (RWops {ℓ}) → Ptr (⊤ {ℓ}) → CSize → CSize → IO CSize)
        rwopsWrite : FunPtr (Ptr (RWops {ℓ}) → Ptr (⊤ {ℓ}) → CSize → CSize → IO CSize)
        rwopsClose : FunPtr (Ptr (RWops {ℓ}) → IO CInt)
        rwopsType  : Word32

postulate
    Eq[RWops]       : Eq (RWops {ℓ})
    Show[RWops]     : Show (RWops {ℓ})
    Storable[RWops] : Storable (RWops {ℓ})

record Surface {aℓ bℓ} : Set (aℓ ⊔ bℓ) where
    constructor mkSurface
    field
        surfaceFormat   : Ptr PixelFormat
        surfaceW        : CInt
        surfaceH        : CInt
        surfacePixels   : Ptr (⊤ {aℓ})
        surfaceUserdata : Ptr (⊤ {bℓ})
        surfaceClipRect : Rect
        surfaceRefcount : CInt

postulate
    Eq[Surface]       : Eq (Surface {aℓ} {bℓ})
    Show[Surface]     : Show (Surface {aℓ} {bℓ})
    Storable[Surface] : Storable (Surface {aℓ} {bℓ})

record Version : Set where
    constructor mkVersion
    field
        versionMajor : Word8
        versionMinor : Word8
        versionPatch : Word8

postulate
    Eq[Version]       : Eq Version
    Show[Version]     : Show Version
    Storable[Version] : Storable Version

-- Function Types

VkGetInstanceProcAddrFunc : ∀{aℓ ℓ} → Set (aℓ ⊔ ℓ)
VkGetInstanceProcAddrFunc {aℓ} {ℓ} = VkInstance {aℓ} → CString → IO (FunPtr (⊤ {ℓ}))

AudioCallback : ∀{aℓ ℓ} → Set (aℓ ⊔ ℓ)
AudioCallback {aℓ} {ℓ} = FunPtr (Ptr (⊤ {aℓ}) → Ptr Word8 → CInt → IO (⊤ {ℓ}))

EventFilter : ∀{aℓ bℓ cℓ dℓ} → Set (aℓ ⊔ bℓ ⊔ cℓ ⊔ dℓ)
EventFilter {aℓ} {bℓ} {cℓ} {dℓ} = FunPtr (Ptr (⊤ {aℓ}) → Ptr (Event {bℓ} {cℓ} {dℓ}) → IO CInt)

HintCallback : ∀{aℓ ℓ} → Set (aℓ ⊔ ℓ)
HintCallback {aℓ} {ℓ} = FunPtr (Ptr (⊤ {aℓ}) → CString → CString → CString → IO (⊤ {ℓ}))

LogOutputFunction : ∀{aℓ ℓ} → Set (aℓ ⊔ ℓ)
LogOutputFunction {aℓ} {ℓ} = FunPtr (Ptr (⊤ {aℓ}) → CInt → LogPriority → CString → IO (⊤ {ℓ}))

ThreadFunction : ∀{aℓ} → Set aℓ
ThreadFunction {aℓ} = FunPtr (Ptr (⊤ {aℓ}) → IO CInt)

TimerCallback : ∀{aℓ} → Set aℓ
TimerCallback {aℓ} = FunPtr (Word32 → Ptr (⊤ {aℓ}) → IO Word32)

postulate
    mkAudioCallback     : (Ptr (⊤ {aℓ}) → Ptr Word8 → CInt → IO (⊤ {ℓ})) → IO (AudioCallback {aℓ} {ℓ})
    mkEventFilter       : (Ptr (⊤ {aℓ}) → Ptr (Event {bℓ} {cℓ} {dℓ}) → IO CInt) → IO (EventFilter {aℓ} {bℓ} {cℓ} {dℓ})
    mkHintCallback      : (Ptr (⊤ {aℓ}) → CString → CString → CString → IO (⊤ {ℓ})) → IO (HintCallback {aℓ} {ℓ})
    mkLogOutputFunction : (Ptr (⊤ {aℓ}) → CInt → LogPriority → CString → IO (⊤ {ℓ})) → IO (LogOutputFunction {aℓ} {ℓ})
    mkThreadFunction    : (Ptr (⊤ {aℓ}) → IO CInt) → IO (ThreadFunction {aℓ})
    mkTimerCallback     : (Word32 → Ptr (⊤ {aℓ}) → IO Word32) → IO (TimerCallback {aℓ})

-- Data Structures

record AudioSpec {aℓ} {ℓ} : Set (aℓ ⊔ ℓ) where
    constructor mkAudioSpec
    field
        audioSpecFreq     : CInt
        audioSpecFormat   : AudioFormat
        audioSpecChannels : Word8
        audioSpecSilence  : Word8
        audioSpecSamples  : Word16
        audioSpecSize     : Word32
        audioSpecCallback : AudioCallback {aℓ} {ℓ} -- first arg to audio callback is userdata, thus same level
        audioSpecUserdata : Ptr (⊤ {aℓ})

postulate
    Eq[AudioSpec]       : Eq (AudioSpec {aℓ} {ℓ})
    Show[AudioSpec]     : Show (AudioSpec {aℓ} {ℓ})
    Storable[AudioSpec] : Storable (AudioSpec {aℓ} {ℓ})
