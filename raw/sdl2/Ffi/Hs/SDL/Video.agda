{-# OPTIONS --without-K #-}

module Ffi.Hs.SDL.Video where

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


data WindowGraphicsContext : Set where
    NoGraphicsContext : WindowGraphicsContext
    OpenGLContext     : OpenGLConfig → WindowGraphicsContext
    VulkanContext     : WindowGraphicsContext

postulate
    Eq[WindowGraphicsContext]   : Eq WindowGraphicsContext
    Ord[WindowGraphicsContext]  : Ord WindowGraphicsContext
    Read[WindowGraphicsContext] : Read WindowGraphicsContext
    Show[WindowGraphicsContext] : Show WindowGraphicsContext


data WindowMode : Set where
    Fullscreen        : WindowMode
    FullscreenDesktop : WindowMode
    Maximized         : WindowMode
    Minimized         : WindowMode
    Windowed          : WindowMode

postulate
    Bounded[WindowMode] : Bounded WindowMode
    Enum[WindowMode]    : Enum WindowMode
    Eq[WindowMode]      : Eq WindowMode
    Data[WindowMode]    : Data WindowMode
    Ord[WindowMode]     : Ord WindowMode
    Read[WindowMode]    : Read WindowMode
    Show[WindowMode]    : Show WindowMode


data WindowPosition : Set where
    Centered : WindowPosition
    Wherever : WindowPosition
    Absolute : Point V2 CInt → WindowPosition

postulate
    Eq[WindowPosition]   : Eq WindowPosition
    Ord[WindowPosition]  : Ord WindowPosition
    Read[WindowPosition] : Read WindowPosition
    Show[WindowPosition] : Show WindowPosition


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

postulate
    Eq[WindowConfig]   : Eq WindowConfig
    Ord[WindowConfig]  : Ord WindowConfig
    Read[WindowConfig] : Read WindowConfig
    Show[WindowConfig] : Show WindowConfig

    defaultWindow : WindowConfig


postulate
    createWindow : ⦃ MonadIO M ⦄ → Text → WindowConfig → M Window
    destroyWindow : ⦃ MonadIO M ⦄ → Window → M ⊤
    hideWindow : ⦃ MonadIO M ⦄ → Window → M ⊤
    raiseWindow : ⦃ MonadIO M ⦄ → Window → M ⊤
    showWindow : ⦃ MonadIO M ⦄ → Window → M ⊤

    windowMinimumSize : Window → StateVar (V2 CInt)
    windowMaximumSize : Window → StateVar (V2 CInt)
    windowSize : Window → StateVar (V2 CInt)
    windowBordered : Window → StateVar Bool
    windowBrightness : Window → StateVar Float
    windowGammaRamp : Window → StateVar (V3 (Vector Word16))
    windowGrab : Window → StateVar Bool
    setWindowMode : ⦃ MonadIO M ⦄ → Window → WindowMode → M ⊤
    getWindowAbsolutePosition : ⦃ MonadIO M ⦄ → Window → M (V2 CInt)
    getWindowBordersSize : ⦃ MonadIO M ⦄ → Window → M (Maybe (V4 CInt))
    setWindowIcon : ⦃ MonadIO M ⦄ → Window → Surface → M ⊤
    setWindowPosition : ⦃ MonadIO M ⦄ → Window → WindowPosition → M ⊤
    windowTitle : Window → StateVar Text
    windowData : Window → CString → StateVar (Ptr (⊤ {lzero}))
    getWindowConfig : ⦃ MonadIO M ⦄ → Window → M WindowConfig
    getWindowPixelFormat : ⦃ MonadIO M ⦄ → Window → M PixelFormat


postulate
    createRenderer : ⦃ MonadIO M ⦄ → Window → CInt → RendererConfig → M Renderer
    createSoftwareRenderer : ⦃ MonadIO M ⦄ → Surface → M Renderer
    destroyRenderer : ⦃ MonadIO M ⦄ → Renderer → M ⊤


postulate
    getClipboardText : ⦃ MonadIO M ⦄ → M Text
    hasClipboardText : ⦃ MonadIO M ⦄ → M Bool
    setClipboardText : ⦃ MonadIO M ⦄ → Text → M ⊤


record DisplayMode : Set where
    constructor mkDisplayMode
    field
        displayModeFormat      : PixelFormat
        displayModeSize        : V2 CInt
        displayModeRefreshRate : CInt

postulate
    Eq[DisplayMode]   : Eq DisplayMode
    Ord[DisplayMode]  : Ord DisplayMode
    Read[DisplayMode] : Read DisplayMode
    Show[DisplayMode] : Show DisplayMode


record Display : Set where
    constructor mkDisplay
    field
        displayName           : List Char
        displayBoundsPosition : Point V2 CInt
        displayBoundsSize     : V2 CInt
        displayModes          : List DisplayMode

postulate
    Eq[Display]   : Eq Display
    Ord[Display]  : Ord Display
    Read[Display] : Read Display
    Show[Display] : Show Display


record VideoDriver : Set where
    constructor mkVideoDriver
    field
        videoDriverName : List Char

postulate
    Eq[VideoDriver]   : Eq VideoDriver
    Data[VideoDriver] : Data VideoDriver
    Ord[VideoDriver]  : Ord VideoDriver
    Read[VideoDriver] : Read VideoDriver
    Show[VideoDriver] : Show VideoDriver


postulate
    screenSaverEnabled : StateVar Bool


data MessageKind : Set where
    Error       : MessageKind
    Warning     : MessageKind
    Information : MessageKind

postulate
    Bounded[MessageKind] : Bounded MessageKind
    Enum[MessageKind]    : Enum MessageKind
    Eq[MessageKind]      : Eq MessageKind
    Data[MessageKind]    : Data MessageKind
    Ord[MessageKind]     : Ord MessageKind
    Read[MessageKind]    : Read MessageKind
    Show[MessageKind]    : Show MessageKind

    showSimpleMessageBox : ⦃ MonadIO M ⦄ → Maybe Window → MessageKind → Text → Text → M ⊤
