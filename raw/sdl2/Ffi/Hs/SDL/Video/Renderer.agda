{-# OPTIONS --without-K #-}

module Ffi.Hs.SDL.Video.Renderer where

open import Agda.Builtin.Bool                   using (Bool)
open import Agda.Builtin.Char                   using (Char)
open import Agda.Builtin.List                   using (List)
open import Agda.Builtin.Maybe                  using (Maybe)
open import Agda.Builtin.String                 using () renaming (String to Text)
open import Agda.Primitive
open import Ffi.Hs.-base.Class
open import Ffi.Hs.-base.Unit                   using (⊤; ⊤′)
open import Ffi.Hs.Data.ByteString              using (ByteString)
open import Ffi.Hs.Data.StateVar                using (StateVar)
open import Ffi.Hs.Data.Tuple                   using (Tuple2)
open import Ffi.Hs.Data.Vector.Storable         using (Vector)
open import Ffi.Hs.Data.Vector.Storable.Mutable using (IOVector)
open import Ffi.Hs.Data.Word                    using (Word8; Word32)
open import Ffi.Hs.Foreign.C.Types              using (CInt; CFloat; CDouble)
open import Ffi.Hs.Foreign.Ptr                  using (Ptr)
open import Ffi.Hs.SDL.Internal.Types           using (Window)
open import Ffi.Hs.SDL.Raw.Types as Raw         using ()
open import Ffi.Hs.SDL.Vect                     using (V2; V3; V4; Point)

-- todo: compile

private
    variable
        aℓ : Level
        A : Set aℓ
        M : Set aℓ → Set aℓ

open import Ffi.Hs.SDL.Internal.Types public
    using
    ( Renderer
    ; Eq[Renderer]
    ; Data[Renderer]
    ; Ord[Renderer]
    ; Show[Renderer]
    )


data RendererType : Set where
    UnacceleratedRenderer    : RendererType
    AcceleratedRenderer      : RendererType
    AcceleratedVSyncRenderer : RendererType
    SoftwareRenderer         : RendererType

postulate
    Bounded[RendererType] : Bounded RendererType
    Enum[RendererType]    : Enum RendererType
    Eq[RendererType]      : Eq RendererType
    Data[RendererType]    : Data RendererType
    Ord[RendererType]     : Ord RendererType
    Read[RendererType]    : Read RendererType
    Show[RendererType]    : Show RendererType


record RendererConfig : Set where
    constructor mkRendererConfig
    field
        rendererType          : RendererType
        rendererTargetTexture : Bool

postulate
    Eq[RendererConfig]   : Eq RendererConfig
    Data[RendererConfig] : Data RendererConfig
    Ord[RendererConfig]  : Ord RendererConfig
    Read[RendererConfig] : Read RendererConfig
    Show[RendererConfig] : Show RendererConfig

postulate
    defaultRenderer : RendererConfig


data Rectangle (A : Set aℓ) : Set aℓ where
    mkRectangle : Point V2 A → V2 A → Rectangle A

postulate
    Functor[Rectangle]     : Functor {aℓ} Rectangle
    Eq[Rectangle[A]]       : ⦃ Eq A ⦄ → Eq (Rectangle A)
    Ord[Rectangle[A]]      : ⦃ Ord A ⦄ → Ord (Rectangle A)
    Read[Rectangle[A]]     : ⦃ Read A ⦄ → Read (Rectangle A)
    Show[Rectangle[A]]     : ⦃ Show A ⦄ → Show (Rectangle A)
    Storable[Rectangle[A]] : ⦃ Storable A ⦄ → Storable (Rectangle A)


postulate
    Texture : Set
    Eq[Texture] : Eq Texture


postulate
    clear      : ⦃ MonadIO M ⦄ → Renderer → M ⊤′
    copy       : ⦃ MonadIO M ⦄ → Renderer → Texture → Maybe (Rectangle CInt) → Maybe (Rectangle CInt) → M ⊤′
    copyEx     : ⦃ MonadIO M ⦄ → Renderer → Texture → Maybe (Rectangle CInt) → Maybe (Rectangle CInt) → CDouble → Maybe (Point V2 CInt) → V2 Bool → M ⊤′
    drawLine   : ⦃ MonadIO M ⦄ → Renderer → Point V2 CInt → Point V2 CInt → M ⊤′
    drawLines  : ⦃ MonadIO M ⦄ → Renderer → Vector (Point V2 CInt) → M ⊤′
    drawPoint  : ⦃ MonadIO M ⦄ → Renderer → Point V2 CInt → M ⊤′
    drawPoints : ⦃ MonadIO M ⦄ → Renderer → Vector (Point V2 CInt) → M ⊤′
    drawRect   : ⦃ MonadIO M ⦄ → Renderer → Maybe (Rectangle CInt) → M ⊤′
    drawRects  : ⦃ MonadIO M ⦄ → Renderer → Vector (Rectangle CInt) → M ⊤′
    fillRect   : ⦃ MonadIO M ⦄ → Renderer → Maybe (Rectangle CInt) → M ⊤′
    fillRects  : ⦃ MonadIO M ⦄ → Renderer → Vector (Rectangle CInt) → M ⊤′
    present    : ⦃ MonadIO M ⦄ → Renderer → M ⊤′


data BlendMode : Set where
    BlendNone       : BlendMode
    BlendAlphaBlend : BlendMode
    BlendAdditive   : BlendMode
    BlendMod        : BlendMode

postulate
    Bounded[BlendMode] : Bounded BlendMode
    Enum[BlendMode]    : Enum BlendMode
    Eq[BlendMode]      : Eq BlendMode
    Data[BlendMode]    : Data BlendMode
    Ord[BlendMode]     : Ord BlendMode
    Read[BlendMode]    : Read BlendMode
    Show[BlendMode]    : Show BlendMode


postulate
    rendererDrawBlendMode : Renderer → StateVar BlendMode
    rendererDrawColor     : Renderer → StateVar (V4 Word8)
    rendererRenderTarget  : Renderer → StateVar (Maybe Texture)
    rendererClipRect      : Renderer → StateVar (Maybe (Rectangle CInt))
    rendererLogicalSize   : Renderer → StateVar (Maybe (V2 CInt))
    rendererScale         : Renderer → StateVar (V2 CFloat)
    rendererViewport      : Renderer → StateVar (Maybe (Rectangle CInt))
    renderTargetSupported : ⦃ MonadIO M ⦄ → Renderer → M Bool


data SurfacePixelFormat : Set where
    mkSurfacePixelFormat : Ptr Raw.PixelFormat → SurfacePixelFormat

postulate
    Eq[SurfacePixelFormat] : Eq SurfacePixelFormat


data PixelFormat : Set where
    Unknown     : Word32 → PixelFormat
    Index1LSB   : PixelFormat
    Index1MSB   : PixelFormat
    Index4LSB   : PixelFormat
    Index4MSB   : PixelFormat
    Index8      : PixelFormat
    RGB332      : PixelFormat
    RGB444      : PixelFormat
    RGB555      : PixelFormat
    BGR555      : PixelFormat
    ARGB4444    : PixelFormat
    RGBA4444    : PixelFormat
    ABGR4444    : PixelFormat
    BGRA4444    : PixelFormat
    ARGB1555    : PixelFormat
    RGBA5551    : PixelFormat
    ABGR1555    : PixelFormat
    BGRA5551    : PixelFormat
    RGB565      : PixelFormat
    BGR565      : PixelFormat
    RGB24       : PixelFormat
    BGR24       : PixelFormat
    RGB888      : PixelFormat
    RGBX8888    : PixelFormat
    BGR888      : PixelFormat
    BGRX8888    : PixelFormat
    ARGB8888    : PixelFormat
    RGBA8888    : PixelFormat
    ABGR8888    : PixelFormat
    BGRA8888    : PixelFormat
    ARGB2101010 : PixelFormat
    YV12        : PixelFormat
    IYUV        : PixelFormat
    YUY2        : PixelFormat
    UYVY        : PixelFormat
    YVYU        : PixelFormat

postulate
    Eq[PixelFormat]   : Eq PixelFormat
    Data[PixelFormat] : Data PixelFormat
    Ord[PixelFormat]  : Ord PixelFormat
    Read[PixelFormat] : Read PixelFormat
    Show[PixelFormat] : Show PixelFormat


data Surface : Set where
    mkSurface : Ptr Raw.Surface → Maybe (IOVector Word8) → Surface

postulate
    updateWindowSurface : ⦃ MonadIO M ⦄ → Window → M ⊤′
    surfaceBlit         : ⦃ MonadIO M ⦄ → Surface → Maybe (Rectangle CInt) → Surface → Maybe (Point V2 CInt) → M (Maybe (Rectangle CInt))
    surfaceBlitScaled   : ⦃ MonadIO M ⦄ → Surface → Maybe (Rectangle CInt) → Surface → Maybe (Rectangle CInt) → M ⊤′
    surfaceFillRect     : ⦃ MonadIO M ⦄ → Surface → Maybe (Rectangle CInt) → V4 Word8 → M ⊤′
    surfaceFillRects    : ⦃ MonadIO M ⦄ → Surface → Vector (Rectangle CInt) → V4 Word8 → M ⊤′

    convertSurface       : ⦃ MonadIO M ⦄ → Surface → SurfacePixelFormat → M Surface
    createRGBSurface     : ⦃ MonadIO M ⦄ → V2 CInt → PixelFormat → M Surface
    createRGBSurfaceFrom : ⦃ MonadIO M ⦄ → IOVector Word8 → V2 CInt → CInt → PixelFormat → M Surface
    freeSurface          : ⦃ MonadIO M ⦄ → Surface → M ⊤′
    getWindowSurface     : ⦃ MonadIO M ⦄ → Window → M Surface
    loadBMP              : ⦃ MonadIO M ⦄ → List Char → M Surface

    surfaceColorKey   : Surface → StateVar (Maybe (V4 Word8))
    surfaceBlendMode  : Surface → StateVar BlendMode
    surfaceDimensions : ⦃ MonadIO M ⦄ → Surface → M (V2 CInt)
    surfaceFormat     : ⦃ MonadIO M ⦄ → Surface → M SurfacePixelFormat
    surfacePixels     : ⦃ MonadIO M ⦄ → Surface → M (Ptr ⊤)

    lookSurface   : ⦃ MonadIO M ⦄ → Surface → M ⊤′
    unlockSurface : ⦃ MonadIO M ⦄ → Surface → M ⊤′


postulate
    Palette : Set
    Eq[Palette] : Eq Palette

    paletteNColors : ⦃ MonadIO M ⦄ → Palette → M CInt
    paletteColors  : ⦃ MonadIO M ⦄ → Palette → M (Maybe (Vector (V4 Word8)))
    paletteColor   : ⦃ MonadIO M ⦄ → Palette → CInt → M (Maybe (V4 Word8))

    formatPalette      : ⦃ MonadIO M ⦄ → SurfacePixelFormat → M (Maybe Palette)
    setPaletteColors   : ⦃ MonadIO M ⦄ → Palette → Vector (V4 Word8) → CInt → M ⊤′
    pixelFormatToMasks : ⦃ MonadIO M ⦄ → PixelFormat → M (Tuple2 CInt (V4 Word32))
    masksToPixelFormat : ⦃ MonadIO M ⦄ → CInt → V4 Word32 → M PixelFormat


data TextureAccess : Set where
    TextureAccessStatic    : TextureAccess
    TextureAccessStreaming : TextureAccess
    TextureAccessTarget    : TextureAccess

postulate
    Bounded[TextureAccess] : Bounded TextureAccess
    Enum[TextureAccess]    : Enum TextureAccess
    Eq[TextureAccess]      : Eq TextureAccess
    Data[TextureAccess]    : Data TextureAccess
    Ord[TextureAccess]     : Ord TextureAccess
    Read[TextureAccess]    : Read TextureAccess
    Show[TextureAccess]    : Show TextureAccess


record TextureInfo : Set where
    constructor mkTextureInfo
    field
        texturePixelFormat : PixelFormat
        textureAccess      : TextureAccess
        textureWidth       : CInt
        textureHeight      : CInt

postulate
    Eq[TextureInfo]   : Eq TextureInfo
    Ord[TextureInfo]  : Ord TextureInfo
    Read[TextureInfo] : Read TextureInfo
    Show[TextureInfo] : Show TextureInfo


postulate
    createTexture            : ⦃ MonadIO M ⦄ → Renderer → PixelFormat → TextureAccess → V2 CInt → M Texture
    createTextureFromSurface : ⦃ MonadIO M ⦄ → Renderer → Surface → M Texture
    updateTexture            : ⦃ MonadIO M ⦄ → Texture → Maybe (Rectangle CInt) → ByteString → CInt → M ⊤′
    destroyTexture           : ⦃ MonadIO M ⦄ → Texture → M ⊤′
    glBindTexture            : ⦃ MonadIO M ⦄ → Texture → M ⊤′
    glUnbindTexture          : ⦃ MonadIO M ⦄ → Texture → M ⊤′

    textureAlphaMod  : Texture → StateVar Word8
    textureBlendMode : Texture → StateVar Word8
    textureColorMod  : Texture → StateVar (V3 Word8)
    lockTexture      : ⦃ MonadIO M ⦄ → Texture → Maybe (Rectangle CInt) → M (Tuple2 (Ptr ⊤) CInt)
    unlockTexture    : ⦃ MonadIO M ⦄ → Texture → M ⊤′
    queryTexture     : ⦃ MonadIO M ⦄ → Texture → M TextureInfo


record RendererInfo : Set where
    constructor mkRendererInfo
    field
        rendererInfoName              : Text
        rendererInfoFlags             : RendererConfig
        rendererInfoNumTextureFormats : Word32
        rendererInfoTextureFormats    : List PixelFormat
        rendererInfoMaxTextureWidth   : CInt
        rendererInfoMaxTextureHeight  : CInt

postulate
    Eq[RendererInfo]   : Eq RendererInfo
    Ord[RendererInfo]  : Ord RendererInfo
    Read[RendererInfo] : Read RendererInfo
    Show[RendererInfo] : Show RendererInfo

    getRendererInfo     : ⦃ MonadIO M ⦄ → Renderer → M RendererInfo
    getRenderDriverInfo : ⦃ MonadIO M ⦄ → M (List RendererInfo)
 