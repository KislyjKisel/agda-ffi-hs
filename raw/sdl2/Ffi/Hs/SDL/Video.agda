{-# OPTIONS --without-K #-}

module Ffi.Hs.SDL.Video where

open import Agda.Builtin.Bool           using (Bool)
open import Agda.Builtin.Char           using (Char)
open import Agda.Builtin.List           using (List)
open import Agda.Builtin.Maybe          using (Maybe)
open import Agda.Builtin.String         using () renaming (String to Text)
open import Agda.Primitive
open import Ffi.Hs.-base.Class
open import Ffi.Hs.-base.Unit           using (⊤; ⊤′)
open import Ffi.Hs.-base.Level          using (Liftℓ)
open import Ffi.Hs.Data.StateVar        using (StateVar)
open import Ffi.Hs.Data.Vector.Storable using (Vector)
open import Ffi.Hs.Data.Word            using (Word16)
open import Ffi.Hs.Foreign.C.String     using (CString)
open import Ffi.Hs.Foreign.C.Types      using (CInt)
open import Ffi.Hs.Foreign.Ptr          using (Ptr)
open import Ffi.Hs.GHC.Float            using (Float)
open import Ffi.Hs.SDL.Vect             using (Point; V2; V3; V4)

open import Ffi.Hs.SDL.Video.OpenGL   public
open import Ffi.Hs.SDL.Video.Renderer public

open import Ffi.Hs.SDL.Internal.Types public
    using
    ( Window
    ; Eq[Window]
    ; Data[Window]
    ; Ord[Window]
    ; Show[Window]
    )

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified SDL.Video
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ : Level
        M : Set aℓ → Set aℓ


data WindowGraphicsContext : Set where
    NoGraphicsContext : WindowGraphicsContext
    OpenGLContext     : OpenGLConfig → WindowGraphicsContext
    VulkanContext     : WindowGraphicsContext

{-# COMPILE GHC WindowGraphicsContext = data SDL.Video.WindowGraphicsContext
    ( SDL.Video.NoGraphicsContext
    | SDL.Video.OpenGLContext
    | SDL.Video.VulkanContext
    ) #-}

postulate
    Eq[WindowGraphicsContext]   : Eq WindowGraphicsContext
    Ord[WindowGraphicsContext]  : Ord WindowGraphicsContext
    Read[WindowGraphicsContext] : Read WindowGraphicsContext
    Show[WindowGraphicsContext] : Show WindowGraphicsContext

{-# COMPILE GHC Eq[WindowGraphicsContext]   = AgdaEq   #-}
{-# COMPILE GHC Ord[WindowGraphicsContext]  = AgdaOrd  #-}
{-# COMPILE GHC Read[WindowGraphicsContext] = AgdaRead #-}
{-# COMPILE GHC Show[WindowGraphicsContext] = AgdaShow #-}


data WindowMode : Set where
    Fullscreen        : WindowMode
    FullscreenDesktop : WindowMode
    Maximized         : WindowMode
    Minimized         : WindowMode
    Windowed          : WindowMode

{-# COMPILE GHC WindowMode = data SDL.Video.WindowMode
    ( SDL.Video.Fullscreen
    | SDL.Video.FullscreenDesktop
    | SDL.Video.Maximized
    | SDL.Video.Minimized
    | SDL.Video.Windowed
    ) #-}

postulate
    Bounded[WindowMode] : Bounded WindowMode
    Enum[WindowMode]    : Enum WindowMode
    Eq[WindowMode]      : Eq WindowMode
    Data[WindowMode]    : Data WindowMode
    Ord[WindowMode]     : Ord WindowMode
    Read[WindowMode]    : Read WindowMode
    Show[WindowMode]    : Show WindowMode

{-# COMPILE GHC Bounded[WindowMode] = AgdaBounded #-}
{-# COMPILE GHC Enum[WindowMode]    = AgdaEnum    #-}
{-# COMPILE GHC Eq[WindowMode]      = AgdaEq      #-}
{-# COMPILE GHC Data[WindowMode]    = AgdaData    #-}
{-# COMPILE GHC Ord[WindowMode]     = AgdaOrd     #-}
{-# COMPILE GHC Read[WindowMode]    = AgdaRead    #-}
{-# COMPILE GHC Show[WindowMode]    = AgdaShow    #-}


data WindowPosition : Set where
    Centered : WindowPosition
    Wherever : WindowPosition
    Absolute : Point V2 CInt → WindowPosition

{-# COMPILE GHC WindowPosition = data SDL.Video.WindowPosition
    ( SDL.Video.Centered
    | SDL.Video.Wherever
    | SDL.Video.Absolute
    ) #-}

postulate
    Eq[WindowPosition]   : Eq WindowPosition
    Ord[WindowPosition]  : Ord WindowPosition
    Read[WindowPosition] : Read WindowPosition
    Show[WindowPosition] : Show WindowPosition

{-# COMPILE GHC Eq[WindowPosition]   = AgdaEq   #-}
{-# COMPILE GHC Ord[WindowPosition]  = AgdaOrd  #-}
{-# COMPILE GHC Read[WindowPosition] = AgdaRead #-}
{-# COMPILE GHC Show[WindowPosition] = AgdaShow #-}


record WindowConfig : Set where
    constructor mkWindowConfig
    field
        windowBorder          : Bool
        windowHighDPI         : Bool
        windowInputGrabbed    : Bool
        windowMode            : WindowMode
        windowGraphicsContext : WindowGraphicsContext
        windowPosition        : WindowPosition
        windowResizable       : Bool
        windowInitialSize     : V2 CInt
        windowVisible         : Bool

{-# COMPILE GHC WindowConfig = data SDL.Video.WindowConfig (SDL.Video.WindowConfig) #-}

postulate
    Eq[WindowConfig]   : Eq WindowConfig
    Ord[WindowConfig]  : Ord WindowConfig
    Read[WindowConfig] : Read WindowConfig
    Show[WindowConfig] : Show WindowConfig

    defaultWindow : WindowConfig

{-# COMPILE GHC Eq[WindowConfig]   = AgdaEq   #-}
{-# COMPILE GHC Ord[WindowConfig]  = AgdaOrd  #-}
{-# COMPILE GHC Read[WindowConfig] = AgdaRead #-}
{-# COMPILE GHC Show[WindowConfig] = AgdaShow #-}

{-# COMPILE GHC defaultWindow = SDL.Video.defaultWindow #-}


postulate
    createWindow  : ⦃ MonadIO M ⦄ → Text → WindowConfig → M (Liftℓ _ Window)
    destroyWindow : ⦃ MonadIO M ⦄ → Window → M ⊤′
    hideWindow    : ⦃ MonadIO M ⦄ → Window → M ⊤′
    raiseWindow   : ⦃ MonadIO M ⦄ → Window → M ⊤′
    showWindow    : ⦃ MonadIO M ⦄ → Window → M ⊤′

    windowMinimumSize         : Window → StateVar (V2 CInt)
    windowMaximumSize         : Window → StateVar (V2 CInt)
    windowSize                : Window → StateVar (V2 CInt)
    windowBordered            : Window → StateVar Bool
    windowBrightness          : Window → StateVar Float
    windowGammaRamp           : Window → StateVar (V3 (Vector Word16))
    windowGrab                : Window → StateVar Bool
    setWindowMode             : ⦃ MonadIO M ⦄ → Window → WindowMode → M ⊤′
    getWindowAbsolutePosition : ⦃ MonadIO M ⦄ → Window → M (Liftℓ _ (V2 CInt))
    getWindowBordersSize      : ⦃ MonadIO M ⦄ → Window → M (Liftℓ _ (Maybe (V4 CInt)))
    setWindowIcon             : ⦃ MonadIO M ⦄ → Window → Surface → M ⊤′
    setWindowPosition         : ⦃ MonadIO M ⦄ → Window → WindowPosition → M ⊤′
    windowTitle               : Window → StateVar Text
    windowData                : Window → CString → StateVar (Ptr ⊤)
    getWindowConfig           : ⦃ MonadIO M ⦄ → Window → M (Liftℓ _ WindowConfig)
    getWindowPixelFormat      : ⦃ MonadIO M ⦄ → Window → M (Liftℓ _ PixelFormat)

{-# COMPILE GHC createWindow  = \ mℓ m AgdaMonadIO -> SDL.Video.createWindow  #-}
{-# COMPILE GHC destroyWindow = \ mℓ m AgdaMonadIO -> SDL.Video.destroyWindow #-}
{-# COMPILE GHC hideWindow    = \ mℓ m AgdaMonadIO -> SDL.Video.hideWindow    #-}
{-# COMPILE GHC raiseWindow   = \ mℓ m AgdaMonadIO -> SDL.Video.raiseWindow   #-}
{-# COMPILE GHC showWindow    = \ mℓ m AgdaMonadIO -> SDL.Video.showWindow    #-}

{-# COMPILE GHC windowMinimumSize         =                       SDL.Video.windowMinimumSize         #-}
{-# COMPILE GHC windowMaximumSize         =                       SDL.Video.windowMaximumSize         #-}
{-# COMPILE GHC windowSize                =                       SDL.Video.windowSize                #-}
{-# COMPILE GHC windowBordered            =                       SDL.Video.windowBordered            #-}
{-# COMPILE GHC windowBrightness          =                       SDL.Video.windowBrightness          #-}
{-# COMPILE GHC windowGammaRamp           =                       SDL.Video.windowGammaRamp           #-}
{-# COMPILE GHC windowGrab                =                       SDL.Video.windowGrab                #-}
{-# COMPILE GHC setWindowMode             = \ mℓ m AgdaMonadIO -> SDL.Video.setWindowMode             #-}
{-# COMPILE GHC getWindowAbsolutePosition = \ mℓ m AgdaMonadIO -> SDL.Video.getWindowAbsolutePosition #-}
{-# COMPILE GHC getWindowBordersSize      = \ mℓ m AgdaMonadIO -> SDL.Video.getWindowBordersSize      #-}
{-# COMPILE GHC setWindowIcon             = \ mℓ m AgdaMonadIO -> SDL.Video.setWindowIcon             #-}
{-# COMPILE GHC setWindowPosition         = \ mℓ m AgdaMonadIO -> SDL.Video.setWindowPosition         #-}
{-# COMPILE GHC windowTitle               =                       SDL.Video.windowTitle               #-}
{-# COMPILE GHC windowData                =                       SDL.Video.windowData                #-}
{-# COMPILE GHC getWindowConfig           = \ mℓ m AgdaMonadIO -> SDL.Video.getWindowConfig           #-}
{-# COMPILE GHC getWindowPixelFormat      = \ mℓ m AgdaMonadIO -> SDL.Video.getWindowPixelFormat      #-}


postulate
    createRenderer         : ⦃ MonadIO M ⦄ → Window → CInt → RendererConfig → M (Liftℓ _ Renderer)
    createSoftwareRenderer : ⦃ MonadIO M ⦄ → Surface → M (Liftℓ _ Renderer)
    destroyRenderer        : ⦃ MonadIO M ⦄ → Renderer → M ⊤′

{-# COMPILE GHC createRenderer         = \ mℓ m AgdaMonadIO -> SDL.Video.createRenderer #-}
{-# COMPILE GHC createSoftwareRenderer = \ mℓ m AgdaMonadIO -> SDL.Video.createSoftwareRenderer #-}
{-# COMPILE GHC destroyRenderer        = \ mℓ m AgdaMonadIO -> SDL.Video.destroyRenderer #-}


postulate
    getClipboardText : ⦃ MonadIO M ⦄ → M (Liftℓ _ Text)
    hasClipboardText : ⦃ MonadIO M ⦄ → M (Liftℓ _ Bool)
    setClipboardText : ⦃ MonadIO M ⦄ → Text → M ⊤′

{-# COMPILE GHC getClipboardText = \ mℓ m AgdaMonadIO -> SDL.Video.getClipboardText #-}
{-# COMPILE GHC hasClipboardText = \ mℓ m AgdaMonadIO -> SDL.Video.hasClipboardText #-}
{-# COMPILE GHC setClipboardText = \ mℓ m AgdaMonadIO -> SDL.Video.setClipboardText #-}


record DisplayMode : Set where
    constructor mkDisplayMode
    field
        displayModeFormat      : PixelFormat
        displayModeSize        : V2 CInt
        displayModeRefreshRate : CInt

{-# COMPILE GHC DisplayMode = data SDL.Video.DisplayMode (SDL.Video.DisplayMode) #-}

postulate
    Eq[DisplayMode]   : Eq DisplayMode
    Ord[DisplayMode]  : Ord DisplayMode
    Read[DisplayMode] : Read DisplayMode
    Show[DisplayMode] : Show DisplayMode

{-# COMPILE GHC Eq[DisplayMode]   = AgdaEq   #-}
{-# COMPILE GHC Ord[DisplayMode]  = AgdaOrd  #-}
{-# COMPILE GHC Read[DisplayMode] = AgdaRead #-}
{-# COMPILE GHC Show[DisplayMode] = AgdaShow #-}


record Display : Set where
    constructor mkDisplay
    field
        displayName           : List Char
        displayBoundsPosition : Point V2 CInt
        displayBoundsSize     : V2 CInt
        displayModes          : List DisplayMode

{-# COMPILE GHC Display = data SDL.Video.Display (SDL.Video.Display) #-}

postulate
    Eq[Display]   : Eq Display
    Ord[Display]  : Ord Display
    Read[Display] : Read Display
    Show[Display] : Show Display

{-# COMPILE GHC Eq[Display]   = AgdaEq   #-}
{-# COMPILE GHC Ord[Display]  = AgdaOrd  #-}
{-# COMPILE GHC Read[Display] = AgdaRead #-}
{-# COMPILE GHC Show[Display] = AgdaShow #-}


record VideoDriver : Set where
    constructor mkVideoDriver
    field
        videoDriverName : List Char

{-# COMPILE GHC VideoDriver = data SDL.Video.VideoDriver (SDL.Video.VideoDriver) #-}

postulate
    Eq[VideoDriver]   : Eq VideoDriver
    Data[VideoDriver] : Data VideoDriver
    Ord[VideoDriver]  : Ord VideoDriver
    Read[VideoDriver] : Read VideoDriver
    Show[VideoDriver] : Show VideoDriver

{-# COMPILE GHC Eq[VideoDriver]   = AgdaEq   #-}
{-# COMPILE GHC Data[VideoDriver] = AgdaData #-}
{-# COMPILE GHC Ord[VideoDriver]  = AgdaOrd  #-}
{-# COMPILE GHC Read[VideoDriver] = AgdaRead #-}
{-# COMPILE GHC Show[VideoDriver] = AgdaShow #-}


postulate
    screenSaverEnabled : StateVar Bool

{-# COMPILE GHC screenSaverEnabled = SDL.Video.screenSaverEnabled #-}


data MessageKind : Set where
    Error       : MessageKind
    Warning     : MessageKind
    Information : MessageKind

{-# COMPILE GHC MessageKind = data SDL.Video.MessageKind
    ( SDL.Video.Error
    | SDL.Video.Warning
    | SDL.Video.Information
    ) #-}

postulate
    Bounded[MessageKind] : Bounded MessageKind
    Enum[MessageKind]    : Enum MessageKind
    Eq[MessageKind]      : Eq MessageKind
    Data[MessageKind]    : Data MessageKind
    Ord[MessageKind]     : Ord MessageKind
    Read[MessageKind]    : Read MessageKind
    Show[MessageKind]    : Show MessageKind

    showSimpleMessageBox : ⦃ MonadIO M ⦄ → Maybe Window → MessageKind → Text → Text → M ⊤′

{-# COMPILE GHC Bounded[MessageKind] = AgdaBounded #-}
{-# COMPILE GHC Enum[MessageKind]    = AgdaEnum    #-}
{-# COMPILE GHC Eq[MessageKind]      = AgdaEq      #-}
{-# COMPILE GHC Data[MessageKind]    = AgdaData    #-}
{-# COMPILE GHC Ord[MessageKind]     = AgdaOrd     #-}
{-# COMPILE GHC Read[MessageKind]    = AgdaRead    #-}
{-# COMPILE GHC Show[MessageKind]    = AgdaShow    #-}

{-# COMPILE GHC showSimpleMessageBox = \ mℓ m AgdaMonadIO -> SDL.Video.showSimpleMessageBox #-}
