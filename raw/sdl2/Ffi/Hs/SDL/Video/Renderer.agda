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

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified SDL.Video.Renderer
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

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

{-# COMPILE GHC RendererType = data SDL.Video.Renderer.RendererType
    ( SDL.Video.Renderer.UnacceleratedRenderer
    | SDL.Video.Renderer.AcceleratedRenderer
    | SDL.Video.Renderer.AcceleratedVSyncRenderer
    | SDL.Video.Renderer.SoftwareRenderer
    ) #-}

postulate
    Bounded[RendererType] : Bounded RendererType
    Enum[RendererType]    : Enum RendererType
    Eq[RendererType]      : Eq RendererType
    Data[RendererType]    : Data RendererType
    Ord[RendererType]     : Ord RendererType
    Read[RendererType]    : Read RendererType
    Show[RendererType]    : Show RendererType

{-# COMPILE GHC Bounded[RendererType] = AgdaBounded #-}
{-# COMPILE GHC Enum[RendererType]    = AgdaEnum    #-}
{-# COMPILE GHC Eq[RendererType]      = AgdaEq      #-}
{-# COMPILE GHC Data[RendererType]    = AgdaData    #-}
{-# COMPILE GHC Ord[RendererType]     = AgdaOrd     #-}
{-# COMPILE GHC Read[RendererType]    = AgdaRead    #-}
{-# COMPILE GHC Show[RendererType]    = AgdaShow    #-}


record RendererConfig : Set where
    constructor mkRendererConfig
    field
        rendererType          : RendererType
        rendererTargetTexture : Bool

{-# COMPILE GHC RendererConfig = data SDL.Video.Renderer.RendererConfig (SDL.Video.Renderer.RendererConfig) #-}

postulate
    Eq[RendererConfig]   : Eq RendererConfig
    Data[RendererConfig] : Data RendererConfig
    Ord[RendererConfig]  : Ord RendererConfig
    Read[RendererConfig] : Read RendererConfig
    Show[RendererConfig] : Show RendererConfig

{-# COMPILE GHC Eq[RendererConfig]   = AgdaEq   #-}
{-# COMPILE GHC Data[RendererConfig] = AgdaData #-}
{-# COMPILE GHC Ord[RendererConfig]  = AgdaOrd  #-}
{-# COMPILE GHC Read[RendererConfig] = AgdaRead #-}
{-# COMPILE GHC Show[RendererConfig] = AgdaShow #-}

postulate
    defaultRenderer : RendererConfig

{-# COMPILE GHC defaultRenderer = SDL.Video.Renderer.defaultRenderer #-}


data Rectangle (A : Set aℓ) : Set aℓ where
    mkRectangle : Point V2 A → V2 A → Rectangle A

{-# COMPILE GHC Rectangle = data SDL.Video.Renderer.Rectangle (SDL.Video.Renderer.Rectangle) #-}

postulate
    Functor[Rectangle]     : Functor {aℓ} Rectangle
    Eq[Rectangle[A]]       : ⦃ Eq A ⦄ → Eq (Rectangle A)
    Ord[Rectangle[A]]      : ⦃ Ord A ⦄ → Ord (Rectangle A)
    Read[Rectangle[A]]     : ⦃ Read A ⦄ → Read (Rectangle A)
    Show[Rectangle[A]]     : ⦃ Show A ⦄ → Show (Rectangle A)
    Storable[Rectangle[A]] : ⦃ Storable A ⦄ → Storable (Rectangle A)

{-# COMPILE GHC Functor[Rectangle]     = AgdaFunctor  #-}
{-# COMPILE GHC Eq[Rectangle[A]]       = AgdaEq       #-}
{-# COMPILE GHC Ord[Rectangle[A]]      = AgdaOrd      #-}
{-# COMPILE GHC Read[Rectangle[A]]     = AgdaRead     #-}
{-# COMPILE GHC Show[Rectangle[A]]     = AgdaShow     #-}
{-# COMPILE GHC Storable[Rectangle[A]] = AgdaStorable #-}


postulate
    Texture : Set
    Eq[Texture] : Eq Texture

{-# COMPILE GHC Texture = type SDL.Video.Renderer.Texture #-}
{-# COMPILE GHC Eq[Texture] = AgdaEq #-}


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

{-# COMPILE GHC clear      = \ mℓ m AgdaMonadIO -> SDL.Video.Renderer.clear      #-}
{-# COMPILE GHC copy       = \ mℓ m AgdaMonadIO -> SDL.Video.Renderer.copy       #-}
{-# COMPILE GHC copyEx     = \ mℓ m AgdaMonadIO -> SDL.Video.Renderer.copyEx     #-}
{-# COMPILE GHC drawLine   = \ mℓ m AgdaMonadIO -> SDL.Video.Renderer.drawLine   #-}
{-# COMPILE GHC drawLines  = \ mℓ m AgdaMonadIO -> SDL.Video.Renderer.drawLines  #-}
{-# COMPILE GHC drawPoint  = \ mℓ m AgdaMonadIO -> SDL.Video.Renderer.drawPoint  #-}
{-# COMPILE GHC drawPoints = \ mℓ m AgdaMonadIO -> SDL.Video.Renderer.drawPoints #-}
{-# COMPILE GHC drawRect   = \ mℓ m AgdaMonadIO -> SDL.Video.Renderer.drawRect   #-}
{-# COMPILE GHC drawRects  = \ mℓ m AgdaMonadIO -> SDL.Video.Renderer.drawRects  #-}
{-# COMPILE GHC fillRect   = \ mℓ m AgdaMonadIO -> SDL.Video.Renderer.fillRect   #-}
{-# COMPILE GHC fillRects  = \ mℓ m AgdaMonadIO -> SDL.Video.Renderer.fillRects  #-}
{-# COMPILE GHC present    = \ mℓ m AgdaMonadIO -> SDL.Video.Renderer.present    #-}


data BlendMode : Set where
    BlendNone       : BlendMode
    BlendAlphaBlend : BlendMode
    BlendAdditive   : BlendMode
    BlendMod        : BlendMode

{-# COMPILE GHC BlendMode = data SDL.Video.Renderer.BlendMode
    ( SDL.Video.Renderer.BlendNone
    | SDL.Video.Renderer.BlendAlphaBlend
    | SDL.Video.Renderer.BlendAdditive
    | SDL.Video.Renderer.BlendMod
    ) #-}

postulate
    Bounded[BlendMode] : Bounded BlendMode
    Enum[BlendMode]    : Enum BlendMode
    Eq[BlendMode]      : Eq BlendMode
    Data[BlendMode]    : Data BlendMode
    Ord[BlendMode]     : Ord BlendMode
    Read[BlendMode]    : Read BlendMode
    Show[BlendMode]    : Show BlendMode

{-# COMPILE GHC Bounded[BlendMode] = AgdaBounded #-}
{-# COMPILE GHC Enum[BlendMode]    = AgdaEnum    #-}
{-# COMPILE GHC Eq[BlendMode]      = AgdaEq      #-}
{-# COMPILE GHC Data[BlendMode]    = AgdaData    #-}
{-# COMPILE GHC Ord[BlendMode]     = AgdaOrd     #-}
{-# COMPILE GHC Read[BlendMode]    = AgdaRead    #-}
{-# COMPILE GHC Show[BlendMode]    = AgdaShow    #-}


postulate
    rendererDrawBlendMode : Renderer → StateVar BlendMode
    rendererDrawColor     : Renderer → StateVar (V4 Word8)
    rendererRenderTarget  : Renderer → StateVar (Maybe Texture)
    rendererClipRect      : Renderer → StateVar (Maybe (Rectangle CInt))
    rendererLogicalSize   : Renderer → StateVar (Maybe (V2 CInt))
    rendererScale         : Renderer → StateVar (V2 CFloat)
    rendererViewport      : Renderer → StateVar (Maybe (Rectangle CInt))
    renderTargetSupported : ⦃ MonadIO M ⦄ → Renderer → M Bool

{-# COMPILE GHC rendererDrawBlendMode =                       SDL.Video.Renderer.rendererDrawBlendMode #-}
{-# COMPILE GHC rendererDrawColor     =                       SDL.Video.Renderer.rendererDrawColor     #-}
{-# COMPILE GHC rendererRenderTarget  =                       SDL.Video.Renderer.rendererRenderTarget  #-}
{-# COMPILE GHC rendererClipRect      =                       SDL.Video.Renderer.rendererClipRect      #-}
{-# COMPILE GHC rendererLogicalSize   =                       SDL.Video.Renderer.rendererLogicalSize   #-}
{-# COMPILE GHC rendererScale         =                       SDL.Video.Renderer.rendererScale         #-}
{-# COMPILE GHC rendererViewport      =                       SDL.Video.Renderer.rendererViewport      #-}
{-# COMPILE GHC renderTargetSupported = \ mℓ m AgdaMonadIO -> SDL.Video.Renderer.renderTargetSupported #-}


data SurfacePixelFormat : Set where
    mkSurfacePixelFormat : Ptr Raw.PixelFormat → SurfacePixelFormat

{-# COMPILE GHC SurfacePixelFormat = data SDL.Video.Renderer.SurfacePixelFormat (SDL.Video.Renderer.SurfacePixelFormat) #-}

postulate
    Eq[SurfacePixelFormat] : Eq SurfacePixelFormat

{-# COMPILE GHC Eq[SurfacePixelFormat] = AgdaEq #-}


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

{-# COMPILE GHC PixelFormat = data SDL.Video.Renderer.PixelFormat
    ( SDL.Video.Renderer.Unknown
    | SDL.Video.Renderer.Index1LSB
    | SDL.Video.Renderer.Index1MSB
    | SDL.Video.Renderer.Index4LSB
    | SDL.Video.Renderer.Index4MSB
    | SDL.Video.Renderer.Index8
    | SDL.Video.Renderer.RGB332
    | SDL.Video.Renderer.RGB444
    | SDL.Video.Renderer.RGB555
    | SDL.Video.Renderer.BGR555
    | SDL.Video.Renderer.ARGB4444
    | SDL.Video.Renderer.RGBA4444
    | SDL.Video.Renderer.ABGR4444
    | SDL.Video.Renderer.BGRA4444
    | SDL.Video.Renderer.ARGB1555
    | SDL.Video.Renderer.RGBA5551
    | SDL.Video.Renderer.ABGR1555
    | SDL.Video.Renderer.BGRA5551
    | SDL.Video.Renderer.RGB565
    | SDL.Video.Renderer.BGR565
    | SDL.Video.Renderer.RGB24
    | SDL.Video.Renderer.BGR24
    | SDL.Video.Renderer.RGB888
    | SDL.Video.Renderer.RGBX8888
    | SDL.Video.Renderer.BGR888
    | SDL.Video.Renderer.BGRX8888
    | SDL.Video.Renderer.ARGB8888
    | SDL.Video.Renderer.RGBA8888
    | SDL.Video.Renderer.ABGR8888
    | SDL.Video.Renderer.BGRA8888
    | SDL.Video.Renderer.ARGB2101010
    | SDL.Video.Renderer.YV12
    | SDL.Video.Renderer.IYUV
    | SDL.Video.Renderer.YUY2
    | SDL.Video.Renderer.UYVY
    | SDL.Video.Renderer.YVYU
    ) #-}

postulate
    Eq[PixelFormat]   : Eq PixelFormat
    Data[PixelFormat] : Data PixelFormat
    Ord[PixelFormat]  : Ord PixelFormat
    Read[PixelFormat] : Read PixelFormat
    Show[PixelFormat] : Show PixelFormat

{-# COMPILE GHC Eq[PixelFormat]   = AgdaEq   #-}
{-# COMPILE GHC Data[PixelFormat] = AgdaData #-}
{-# COMPILE GHC Ord[PixelFormat]  = AgdaOrd  #-}
{-# COMPILE GHC Read[PixelFormat] = AgdaRead #-}
{-# COMPILE GHC Show[PixelFormat] = AgdaShow #-}


data Surface : Set where
    mkSurface : Ptr Raw.Surface → Maybe (IOVector Word8) → Surface

{-# COMPILE GHC Surface = data SDL.Video.Renderer.Surface (SDL.Video.Renderer.Surface) #-}

postulate
    updateWindowSurface : ⦃ MonadIO M ⦄ → Window → M ⊤′
    surfaceBlit         : ⦃ MonadIO M ⦄ → Surface → Maybe (Rectangle CInt) → Surface → Maybe (Point V2 CInt) → M (Liftℓ _ (Maybe (Rectangle CInt)))
    surfaceBlitScaled   : ⦃ MonadIO M ⦄ → Surface → Maybe (Rectangle CInt) → Surface → Maybe (Rectangle CInt) → M ⊤′
    surfaceFillRect     : ⦃ MonadIO M ⦄ → Surface → Maybe (Rectangle CInt) → V4 Word8 → M ⊤′
    surfaceFillRects    : ⦃ MonadIO M ⦄ → Surface → Vector (Rectangle CInt) → V4 Word8 → M ⊤′

    convertSurface       : ⦃ MonadIO M ⦄ → Surface → SurfacePixelFormat → M (Liftℓ _ Surface)
    createRGBSurface     : ⦃ MonadIO M ⦄ → V2 CInt → PixelFormat → M (Liftℓ _ Surface)
    createRGBSurfaceFrom : ⦃ MonadIO M ⦄ → IOVector Word8 → V2 CInt → CInt → PixelFormat → M (Liftℓ _ Surface)
    freeSurface          : ⦃ MonadIO M ⦄ → Surface → M ⊤′
    getWindowSurface     : ⦃ MonadIO M ⦄ → Window → M (Liftℓ _ Surface)
    loadBMP              : ⦃ MonadIO M ⦄ → List Char → M (Liftℓ _ Surface)

    surfaceColorKey   : Surface → StateVar (Maybe (V4 Word8))
    surfaceBlendMode  : Surface → StateVar BlendMode
    surfaceDimensions : ⦃ MonadIO M ⦄ → Surface → M (Liftℓ _ (V2 CInt))
    surfaceFormat     : ⦃ MonadIO M ⦄ → Surface → M (Liftℓ _ SurfacePixelFormat)
    surfacePixels     : ⦃ MonadIO M ⦄ → Surface → M (Liftℓ _ (Ptr ⊤))

    lookSurface   : ⦃ MonadIO M ⦄ → Surface → M ⊤′
    unlockSurface : ⦃ MonadIO M ⦄ → Surface → M ⊤′

{-# COMPILE GHC updateWindowSurface = \ mℓ m AgdaMonadIO -> SDL.Video.Renderer.updateWindowSurface #-}
{-# COMPILE GHC surfaceBlit         = \ mℓ m AgdaMonadIO -> SDL.Video.Renderer.surfaceBlit         #-}
{-# COMPILE GHC surfaceBlitScaled   = \ mℓ m AgdaMonadIO -> SDL.Video.Renderer.surfaceBlitScaled   #-}
{-# COMPILE GHC surfaceFillRect     = \ mℓ m AgdaMonadIO -> SDL.Video.Renderer.surfaceFillRect     #-}
{-# COMPILE GHC surfaceFillRects    = \ mℓ m AgdaMonadIO -> SDL.Video.Renderer.surfaceFillRects    #-}

{-# COMPILE GHC convertSurface       = \ mℓ m AgdaMonadIO -> SDL.Video.Renderer.convertSurface       #-}
{-# COMPILE GHC createRGBSurface     = \ mℓ m AgdaMonadIO -> SDL.Video.Renderer.createRGBSurface     #-}
{-# COMPILE GHC createRGBSurfaceFrom = \ mℓ m AgdaMonadIO -> SDL.Video.Renderer.createRGBSurfaceFrom #-}
{-# COMPILE GHC freeSurface          = \ mℓ m AgdaMonadIO -> SDL.Video.Renderer.freeSurface          #-}
{-# COMPILE GHC getWindowSurface     = \ mℓ m AgdaMonadIO -> SDL.Video.Renderer.getWindowSurface     #-}
{-# COMPILE GHC loadBMP              = \ mℓ m AgdaMonadIO -> SDL.Video.Renderer.loadBMP              #-}

{-# COMPILE GHC surfaceColorKey   = SDL.Video.Renderer.surfaceColorKey   #-}
{-# COMPILE GHC surfaceBlendMode  = SDL.Video.Renderer.surfaceBlendMode  #-}
{-# COMPILE GHC surfaceDimensions = \ mℓ m AgdaMonadIO -> SDL.Video.Renderer.surfaceDimensions #-}
{-# COMPILE GHC surfaceFormat     = \ mℓ m AgdaMonadIO -> SDL.Video.Renderer.surfaceFormat     #-}
{-# COMPILE GHC surfacePixels     = \ mℓ m AgdaMonadIO -> SDL.Video.Renderer.surfacePixels     #-}

{-# COMPILE GHC lookSurface   = \ mℓ m AgdaMonadIO -> SDL.Video.Renderer.lookSurface   #-}
{-# COMPILE GHC unlockSurface = \ mℓ m AgdaMonadIO -> SDL.Video.Renderer.unlockSurface #-}


postulate
    Palette : Set
    Eq[Palette] : Eq Palette

    paletteNColors : ⦃ MonadIO M ⦄ → Palette → M (Liftℓ _ CInt)
    paletteColors  : ⦃ MonadIO M ⦄ → Palette → M (Liftℓ _ (Maybe (Vector (V4 Word8))))
    paletteColor   : ⦃ MonadIO M ⦄ → Palette → CInt → M (Liftℓ _ (Maybe (V4 Word8)))

    formatPalette      : ⦃ MonadIO M ⦄ → SurfacePixelFormat → M (Liftℓ _ (Maybe Palette))
    setPaletteColors   : ⦃ MonadIO M ⦄ → Palette → Vector (V4 Word8) → CInt → M ⊤′
    pixelFormatToMasks : ⦃ MonadIO M ⦄ → PixelFormat → M (Liftℓ _ (Tuple2 CInt (V4 Word32)))
    masksToPixelFormat : ⦃ MonadIO M ⦄ → CInt → V4 Word32 → M (Liftℓ _ PixelFormat)

{-# COMPILE GHC Palette = type SDL.Video.Renderer.Palette #-}
{-# COMPILE GHC Eq[Palette] = AgdaEq #-}

{-# COMPILE GHC paletteNColors = \ mℓ m AgdaMonadIO -> SDL.Video.Renderer.paletteNColors #-}
{-# COMPILE GHC paletteColors  = \ mℓ m AgdaMonadIO -> SDL.Video.Renderer.paletteColors  #-}
{-# COMPILE GHC paletteColor   = \ mℓ m AgdaMonadIO -> SDL.Video.Renderer.paletteColor   #-}

{-# COMPILE GHC formatPalette      = \ mℓ m AgdaMonadIO -> SDL.Video.Renderer.formatPalette      #-}
{-# COMPILE GHC setPaletteColors   = \ mℓ m AgdaMonadIO -> SDL.Video.Renderer.setPaletteColors   #-}
{-# COMPILE GHC pixelFormatToMasks = \ mℓ m AgdaMonadIO -> SDL.Video.Renderer.pixelFormatToMasks #-}
{-# COMPILE GHC masksToPixelFormat = \ mℓ m AgdaMonadIO -> SDL.Video.Renderer.masksToPixelFormat #-}


data TextureAccess : Set where
    TextureAccessStatic    : TextureAccess
    TextureAccessStreaming : TextureAccess
    TextureAccessTarget    : TextureAccess

{-# COMPILE GHC TextureAccess = type SDL.Video.Renderer.TextureAccess
    ( SDL.Video.Renderer.TextureAccessStatic
    | SDL.Video.Renderer.TextureAccessStreaming
    | SDL.Video.Renderer.TextureAccessTarget
    ) #-}

postulate
    Bounded[TextureAccess] : Bounded TextureAccess
    Enum[TextureAccess]    : Enum TextureAccess
    Eq[TextureAccess]      : Eq TextureAccess
    Data[TextureAccess]    : Data TextureAccess
    Ord[TextureAccess]     : Ord TextureAccess
    Read[TextureAccess]    : Read TextureAccess
    Show[TextureAccess]    : Show TextureAccess

{-# COMPILE GHC Bounded[TextureAccess] = AgdaBounded #-}
{-# COMPILE GHC Enum[TextureAccess]    = AgdaEnum    #-}
{-# COMPILE GHC Eq[TextureAccess]      = AgdaEq      #-}
{-# COMPILE GHC Data[TextureAccess]    = AgdaData    #-}
{-# COMPILE GHC Ord[TextureAccess]     = AgdaOrd     #-}
{-# COMPILE GHC Read[TextureAccess]    = AgdaRead    #-}
{-# COMPILE GHC Show[TextureAccess]    = AgdaShow    #-}


record TextureInfo : Set where
    constructor mkTextureInfo
    field
        texturePixelFormat : PixelFormat
        textureAccess      : TextureAccess
        textureWidth       : CInt
        textureHeight      : CInt

{-# COMPILE GHC TextureInfo = data SDL.Video.Renderer.TextureInfo (SDL.Video.Renderer.TextureInfo) #-}

postulate
    Eq[TextureInfo]   : Eq TextureInfo
    Ord[TextureInfo]  : Ord TextureInfo
    Read[TextureInfo] : Read TextureInfo
    Show[TextureInfo] : Show TextureInfo

{-# COMPILE GHC Eq[TextureInfo]   = AgdaEq   #-}
{-# COMPILE GHC Ord[TextureInfo]  = AgdaOrd  #-}
{-# COMPILE GHC Read[TextureInfo] = AgdaRead #-}
{-# COMPILE GHC Show[TextureInfo] = AgdaShow #-}


postulate
    createTexture            : ⦃ MonadIO M ⦄ → Renderer → PixelFormat → TextureAccess → V2 CInt → M (Liftℓ _ Texture)
    createTextureFromSurface : ⦃ MonadIO M ⦄ → Renderer → Surface → M (Liftℓ _ Texture)
    updateTexture            : ⦃ MonadIO M ⦄ → Texture → Maybe (Rectangle CInt) → ByteString → CInt → M ⊤′
    destroyTexture           : ⦃ MonadIO M ⦄ → Texture → M ⊤′
    glBindTexture            : ⦃ MonadIO M ⦄ → Texture → M ⊤′
    glUnbindTexture          : ⦃ MonadIO M ⦄ → Texture → M ⊤′

    textureAlphaMod  : Texture → StateVar Word8
    textureBlendMode : Texture → StateVar Word8
    textureColorMod  : Texture → StateVar (V3 Word8)
    lockTexture      : ⦃ MonadIO M ⦄ → Texture → Maybe (Rectangle CInt) → M (Liftℓ _ (Tuple2 (Ptr ⊤) CInt))
    unlockTexture    : ⦃ MonadIO M ⦄ → Texture → M ⊤′
    queryTexture     : ⦃ MonadIO M ⦄ → Texture → M (Liftℓ _ TextureInfo)

{-# COMPILE GHC createTexture            = \ mℓ m AgdaMonadIO -> SDL.Video.Renderer.createTexture            #-}
{-# COMPILE GHC createTextureFromSurface = \ mℓ m AgdaMonadIO -> SDL.Video.Renderer.createTextureFromSurface #-}
{-# COMPILE GHC updateTexture            = \ mℓ m AgdaMonadIO -> SDL.Video.Renderer.updateTexture            #-}
{-# COMPILE GHC destroyTexture           = \ mℓ m AgdaMonadIO -> SDL.Video.Renderer.destroyTexture           #-}
{-# COMPILE GHC glBindTexture            = \ mℓ m AgdaMonadIO -> SDL.Video.Renderer.glBindTexture            #-}
{-# COMPILE GHC glUnbindTexture          = \ mℓ m AgdaMonadIO -> SDL.Video.Renderer.glUnbindTexture          #-}

{-# COMPILE GHC textureAlphaMod  =                       SDL.Video.Renderer.textureAlphaMod  #-}
{-# COMPILE GHC textureBlendMode =                       SDL.Video.Renderer.textureBlendMode #-}
{-# COMPILE GHC textureColorMod  =                       SDL.Video.Renderer.textureColorMod  #-}
{-# COMPILE GHC lockTexture      = \ mℓ m AgdaMonadIO -> SDL.Video.Renderer.lockTexture      #-}
{-# COMPILE GHC unlockTexture    = \ mℓ m AgdaMonadIO -> SDL.Video.Renderer.unlockTexture    #-}
{-# COMPILE GHC queryTexture     = \ mℓ m AgdaMonadIO -> SDL.Video.Renderer.queryTexture     #-}


record RendererInfo : Set where
    constructor mkRendererInfo
    field
        rendererInfoName              : Text
        rendererInfoFlags             : RendererConfig
        rendererInfoNumTextureFormats : Word32
        rendererInfoTextureFormats    : List PixelFormat
        rendererInfoMaxTextureWidth   : CInt
        rendererInfoMaxTextureHeight  : CInt

{-# COMPILE GHC RendererInfo = data SDL.Video.Renderer.RendererInfo (SDL.Video.Renderer.RendererInfo) #-}

postulate
    Eq[RendererInfo]   : Eq RendererInfo
    Ord[RendererInfo]  : Ord RendererInfo
    Read[RendererInfo] : Read RendererInfo
    Show[RendererInfo] : Show RendererInfo

    getRendererInfo     : ⦃ MonadIO M ⦄ → Renderer → M (Liftℓ _ RendererInfo)
    getRenderDriverInfo : ⦃ MonadIO M ⦄ → M (Liftℓ _ (List RendererInfo))

{-# COMPILE GHC Eq[RendererInfo]   = AgdaEq   #-}
{-# COMPILE GHC Ord[RendererInfo]  = AgdaOrd  #-}
{-# COMPILE GHC Read[RendererInfo] = AgdaRead #-}
{-# COMPILE GHC Show[RendererInfo] = AgdaShow #-}

{-# COMPILE GHC getRendererInfo     = \ mℓ m AgdaMonadIO -> SDL.Video.Renderer.getRendererInfo     #-}
{-# COMPILE GHC getRenderDriverInfo = \ mℓ m AgdaMonadIO -> SDL.Video.Renderer.getRenderDriverInfo #-}
