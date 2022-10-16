{-# OPTIONS --without-K #-}

module Ffi.Hs.Graphics.Rendering.OpenGL.GL.Texturing.Specification where

open import Agda.Builtin.IO          using (IO)
open import Agda.Builtin.List        using (List)
open import Agda.Builtin.Unit        using (⊤)
open import Agda.Primitive           using ()
open import Ffi.Hs.-base.Class       using (Eq; Ord; Show)
open import Ffi.Hs.Data.StateVar     using (GettableStateVar)
open import Ffi.Hs.Foreign.Ptr       using (Ptr)
open import Ffi.Hs.Graphics.GL.Types using (GLint; GLenum; GLsizei)

open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.CoordTrans                             using (Position)
open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.FramebufferObjects.RenderbufferObjects using (Samples)
open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable             using (Proxy; PixelInternalFormat)
open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.PixelRectangles.Rasterization          using (PixelData)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Graphics.Rendering.OpenGL.GL.Texturing.Specification
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        -- Level is already defined in this module (mipmap level)
        aℓ : Agda.Primitive.Level
        A : Set aℓ
        T : Set


data TextureTarget1D : Set where
    Texture1D : TextureTarget1D

{-# COMPILE GHC TextureTarget1D = data Graphics.Rendering.OpenGL.GL.Texturing.Specification.TextureTarget1D
    ( Graphics.Rendering.OpenGL.GL.Texturing.Specification.Texture1D
    ) #-}

postulate
    Eq[TextureTarget1D]   : Eq TextureTarget1D
    Ord[TextureTarget1D]  : Ord TextureTarget1D
    Show[TextureTarget1D] : Show TextureTarget1D

{-# COMPILE GHC Eq[TextureTarget1D]   = AgdaEq   #-}
{-# COMPILE GHC Ord[TextureTarget1D]  = AgdaOrd  #-}
{-# COMPILE GHC Show[TextureTarget1D] = AgdaShow #-}


data TextureTarget2D : Set where
    Texture2D        : TextureTarget2D
    Texture1DArray   : TextureTarget2D
    TextureRectangle : TextureTarget2D

{-# COMPILE GHC TextureTarget2D = data Graphics.Rendering.OpenGL.GL.Texturing.Specification.TextureTarget2D
    ( Graphics.Rendering.OpenGL.GL.Texturing.Specification.Texture2D
    | Graphics.Rendering.OpenGL.GL.Texturing.Specification.Texture1DArray
    | Graphics.Rendering.OpenGL.GL.Texturing.Specification.TextureRectangle
    ) #-}

postulate
    Eq[TextureTarget2D]   : Eq TextureTarget2D
    Ord[TextureTarget2D]  : Ord TextureTarget2D
    Show[TextureTarget2D] : Show TextureTarget2D

{-# COMPILE GHC Eq[TextureTarget2D]   = AgdaEq   #-}
{-# COMPILE GHC Ord[TextureTarget2D]  = AgdaOrd  #-}
{-# COMPILE GHC Show[TextureTarget2D] = AgdaShow #-}


data TextureTarget2DMultisample : Set where
    Texture2DMultisample : TextureTarget2DMultisample

{-# COMPILE GHC TextureTarget2DMultisample = data Graphics.Rendering.OpenGL.GL.Texturing.Specification.TextureTarget2DMultisample
    ( Graphics.Rendering.OpenGL.GL.Texturing.Specification.Texture2DMultisample
    ) #-}

postulate
    Eq[TextureTarget2DMultisample]   : Eq TextureTarget2DMultisample
    Ord[TextureTarget2DMultisample]  : Ord TextureTarget2DMultisample
    Show[TextureTarget2DMultisample] : Show TextureTarget2DMultisample

{-# COMPILE GHC Eq[TextureTarget2DMultisample]   = AgdaEq   #-}
{-# COMPILE GHC Ord[TextureTarget2DMultisample]  = AgdaOrd  #-}
{-# COMPILE GHC Show[TextureTarget2DMultisample] = AgdaShow #-}


data TextureTargetCubeMap : Set where
    TextureCubeMap : TextureTargetCubeMap

{-# COMPILE GHC TextureTargetCubeMap = data Graphics.Rendering.OpenGL.GL.Texturing.Specification.TextureTargetCubeMap
    ( Graphics.Rendering.OpenGL.GL.Texturing.Specification.TextureCubeMap
    ) #-}

postulate
    Eq[TextureTargetCubeMap]   : Eq TextureTargetCubeMap
    Ord[TextureTargetCubeMap]  : Ord TextureTargetCubeMap
    Show[TextureTargetCubeMap] : Show TextureTargetCubeMap

{-# COMPILE GHC Eq[TextureTargetCubeMap]   = AgdaEq   #-}
{-# COMPILE GHC Ord[TextureTargetCubeMap]  = AgdaOrd  #-}
{-# COMPILE GHC Show[TextureTargetCubeMap] = AgdaShow #-}


data TextureTargetCubeMapFace : Set where
    TextureCubeMapPositiveX : TextureTargetCubeMapFace
    TextureCubeMapNegativeX : TextureTargetCubeMapFace
    TextureCubeMapPositiveY : TextureTargetCubeMapFace
    TextureCubeMapNegativeY : TextureTargetCubeMapFace
    TextureCubeMapPositiveZ : TextureTargetCubeMapFace
    TextureCubeMapNegativeZ : TextureTargetCubeMapFace

{-# COMPILE GHC TextureTargetCubeMapFace = data Graphics.Rendering.OpenGL.GL.Texturing.Specification.TextureTargetCubeMapFace
    ( Graphics.Rendering.OpenGL.GL.Texturing.Specification.TextureCubeMapPositiveX
    | Graphics.Rendering.OpenGL.GL.Texturing.Specification.TextureCubeMapNegativeX
    | Graphics.Rendering.OpenGL.GL.Texturing.Specification.TextureCubeMapPositiveY
    | Graphics.Rendering.OpenGL.GL.Texturing.Specification.TextureCubeMapNegativeY
    | Graphics.Rendering.OpenGL.GL.Texturing.Specification.TextureCubeMapPositiveZ
    | Graphics.Rendering.OpenGL.GL.Texturing.Specification.TextureCubeMapNegativeZ
    ) #-}

postulate
    Eq[TextureTargetCubeMapFace]   : Eq TextureTargetCubeMapFace
    Ord[TextureTargetCubeMapFace]  : Ord TextureTargetCubeMapFace
    Show[TextureTargetCubeMapFace] : Show TextureTargetCubeMapFace

{-# COMPILE GHC Eq[TextureTargetCubeMapFace]   = AgdaEq   #-}
{-# COMPILE GHC Ord[TextureTargetCubeMapFace]  = AgdaOrd  #-}
{-# COMPILE GHC Show[TextureTargetCubeMapFace] = AgdaShow #-}


data TextureTarget3D : Set where
    Texture3D           : TextureTarget3D
    Texture2DArray      : TextureTarget3D
    TextureCubeMapArray : TextureTarget3D

{-# COMPILE GHC TextureTarget3D = data Graphics.Rendering.OpenGL.GL.Texturing.Specification.TextureTarget3D
    ( Graphics.Rendering.OpenGL.GL.Texturing.Specification.Texture3D
    | Graphics.Rendering.OpenGL.GL.Texturing.Specification.Texture2DArray
    | Graphics.Rendering.OpenGL.GL.Texturing.Specification.TextureCubeMapArray
    ) #-}

postulate
    Eq[TextureTarget3D]   : Eq TextureTarget3D
    Ord[TextureTarget3D]  : Ord TextureTarget3D
    Show[TextureTarget3D] : Show TextureTarget3D

{-# COMPILE GHC Eq[TextureTarget3D]   = AgdaEq   #-}
{-# COMPILE GHC Ord[TextureTarget3D]  = AgdaOrd  #-}
{-# COMPILE GHC Show[TextureTarget3D] = AgdaShow #-}


data TextureTarget2DMultisampleArray : Set where
    Texture2DMultisampleArray : TextureTarget2DMultisampleArray

{-# COMPILE GHC TextureTarget2DMultisampleArray = data Graphics.Rendering.OpenGL.GL.Texturing.Specification.TextureTarget2DMultisampleArray
    ( Graphics.Rendering.OpenGL.GL.Texturing.Specification.Texture2DMultisampleArray
    ) #-}

postulate
    Eq[TextureTarget2DMultisampleArray]   : Eq TextureTarget2DMultisampleArray
    Ord[TextureTarget2DMultisampleArray]  : Ord TextureTarget2DMultisampleArray
    Show[TextureTarget2DMultisampleArray] : Show TextureTarget2DMultisampleArray

{-# COMPILE GHC Eq[TextureTarget2DMultisampleArray]   = AgdaEq   #-}
{-# COMPILE GHC Ord[TextureTarget2DMultisampleArray]  = AgdaOrd  #-}
{-# COMPILE GHC Show[TextureTarget2DMultisampleArray] = AgdaShow #-}


data TextureTargetBuffer : Set where
    TextureBuffer' : TextureTargetBuffer

{-# COMPILE GHC TextureTargetBuffer = data Graphics.Rendering.OpenGL.GL.Texturing.Specification.TextureTargetBuffer
    ( Graphics.Rendering.OpenGL.GL.Texturing.Specification.TextureBuffer'
    ) #-}

postulate
    Eq[TextureTargetBuffer]   : Eq TextureTargetBuffer
    Ord[TextureTargetBuffer]  : Ord TextureTargetBuffer
    Show[TextureTargetBuffer] : Show TextureTargetBuffer

{-# COMPILE GHC Eq[TextureTargetBuffer]   = AgdaEq   #-}
{-# COMPILE GHC Ord[TextureTargetBuffer]  = AgdaOrd  #-}
{-# COMPILE GHC Show[TextureTargetBuffer] = AgdaShow #-}


postulate
    BindableTextureTarget : Set → Set

    BindableTextureTarget[TextureTargetBuffer]             : BindableTextureTarget TextureTargetBuffer
    BindableTextureTarget[TextureTarget2DMultisampleArray] : BindableTextureTarget TextureTarget2DMultisampleArray
    BindableTextureTarget[TextureTarget3D]                 : BindableTextureTarget TextureTarget3D
    BindableTextureTarget[TextureTargetCubeMap]            : BindableTextureTarget TextureTargetCubeMap
    BindableTextureTarget[TextureTarget2DMultisample]      : BindableTextureTarget TextureTarget2DMultisample
    BindableTextureTarget[TextureTarget2D]                 : BindableTextureTarget TextureTarget2D
    BindableTextureTarget[TextureTarget1D]                 : BindableTextureTarget TextureTarget1D

{-# FOREIGN GHC data AgdaBindableTextureTarget a = Graphics.Rendering.OpenGL.GL.Texturing.Specification.BindableTextureTarget a => AgdaBindableTextureTarget #-}
{-# COMPILE GHC BindableTextureTarget = type(0) AgdaBindableTextureTarget #-}

{-# COMPILE GHC BindableTextureTarget[TextureTargetBuffer]             = AgdaBindableTextureTarget #-}
{-# COMPILE GHC BindableTextureTarget[TextureTarget2DMultisampleArray] = AgdaBindableTextureTarget #-}
{-# COMPILE GHC BindableTextureTarget[TextureTarget3D]                 = AgdaBindableTextureTarget #-}
{-# COMPILE GHC BindableTextureTarget[TextureTargetCubeMap]            = AgdaBindableTextureTarget #-}
{-# COMPILE GHC BindableTextureTarget[TextureTarget2DMultisample]      = AgdaBindableTextureTarget #-}
{-# COMPILE GHC BindableTextureTarget[TextureTarget2D]                 = AgdaBindableTextureTarget #-}
{-# COMPILE GHC BindableTextureTarget[TextureTarget1D]                 = AgdaBindableTextureTarget #-}


postulate
    ParameterizedTextureTarget : Set → Set

    ParameterizedTextureTarget[TextureTarget2DMultisampleArray] : ParameterizedTextureTarget TextureTarget2DMultisampleArray
    ParameterizedTextureTarget[TextureTarget3D]                 : ParameterizedTextureTarget TextureTarget3D
    ParameterizedTextureTarget[TextureTargetCubeMap]            : ParameterizedTextureTarget TextureTargetCubeMap
    ParameterizedTextureTarget[TextureTarget2DMultisample]      : ParameterizedTextureTarget TextureTarget2DMultisample
    ParameterizedTextureTarget[TextureTarget2D]                 : ParameterizedTextureTarget TextureTarget2D
    ParameterizedTextureTarget[TextureTarget1D]                 : ParameterizedTextureTarget TextureTarget1D

{-# FOREIGN GHC data AgdaParameterizedTextureTarget a = Graphics.Rendering.OpenGL.GL.Texturing.Specification.ParameterizedTextureTarget a => AgdaParameterizedTextureTarget #-}
{-# COMPILE GHC ParameterizedTextureTarget = type(0) AgdaParameterizedTextureTarget #-}

{-# COMPILE GHC ParameterizedTextureTarget[TextureTarget2DMultisampleArray] = AgdaParameterizedTextureTarget #-}
{-# COMPILE GHC ParameterizedTextureTarget[TextureTarget3D]                 = AgdaParameterizedTextureTarget #-}
{-# COMPILE GHC ParameterizedTextureTarget[TextureTargetCubeMap]            = AgdaParameterizedTextureTarget #-}
{-# COMPILE GHC ParameterizedTextureTarget[TextureTarget2DMultisample]      = AgdaParameterizedTextureTarget #-}
{-# COMPILE GHC ParameterizedTextureTarget[TextureTarget2D]                 = AgdaParameterizedTextureTarget #-}
{-# COMPILE GHC ParameterizedTextureTarget[TextureTarget1D]                 = AgdaParameterizedTextureTarget #-}


postulate
    OneDimensionalTextureTarget : Set → Set

    OneDimensionalTextureTarget[TextureTarget1D] : OneDimensionalTextureTarget TextureTarget1D

{-# FOREIGN GHC data AgdaOneDimensionalTextureTarget a = Graphics.Rendering.OpenGL.GL.Texturing.Specification.OneDimensionalTextureTarget a => AgdaOneDimensionalTextureTarget #-}
{-# COMPILE GHC OneDimensionalTextureTarget = type(0) AgdaOneDimensionalTextureTarget #-}

{-# COMPILE GHC OneDimensionalTextureTarget[TextureTarget1D] = AgdaOneDimensionalTextureTarget #-}


postulate
    TwoDimensionalTextureTarget : Set → Set

    TwoDimensionalTextureTarget[TextureTargetCubeMapFace] : TwoDimensionalTextureTarget TextureTargetCubeMapFace
    TwoDimensionalTextureTarget[TextureTargetCubeMap]     : TwoDimensionalTextureTarget TextureTargetCubeMap
    TwoDimensionalTextureTarget[TextureTarget2D]          : TwoDimensionalTextureTarget TextureTarget2D

{-# FOREIGN GHC data AgdaTwoDimensionalTextureTarget a = Graphics.Rendering.OpenGL.GL.Texturing.Specification.TwoDimensionalTextureTarget a => AgdaTwoDimensionalTextureTarget #-}
{-# COMPILE GHC TwoDimensionalTextureTarget = type(0) AgdaTwoDimensionalTextureTarget #-}

{-# COMPILE GHC TwoDimensionalTextureTarget[TextureTargetCubeMapFace] = AgdaTwoDimensionalTextureTarget #-}
{-# COMPILE GHC TwoDimensionalTextureTarget[TextureTargetCubeMap]     = AgdaTwoDimensionalTextureTarget #-}
{-# COMPILE GHC TwoDimensionalTextureTarget[TextureTarget2D]          = AgdaTwoDimensionalTextureTarget #-}


postulate
    ThreeDimensionalTextureTarget : Set → Set

    ThreeDimensionalTextureTarget[TextureTarget3D] : ThreeDimensionalTextureTarget TextureTarget3D

{-# FOREIGN GHC data AgdaThreeDimensionalTextureTarget a = Graphics.Rendering.OpenGL.GL.Texturing.Specification.ThreeDimensionalTextureTarget a => AgdaThreeDimensionalTextureTarget #-}
{-# COMPILE GHC ThreeDimensionalTextureTarget = type(0) AgdaThreeDimensionalTextureTarget #-}

{-# COMPILE GHC ThreeDimensionalTextureTarget[TextureTarget3D] = AgdaThreeDimensionalTextureTarget #-}


postulate
    QueryableTextureTarget : Set → Set

    QueryableTextureTarget[TextureTarget2DMultisampleArray] : QueryableTextureTarget TextureTarget2DMultisampleArray
    QueryableTextureTarget[TextureTarget3D]                 : QueryableTextureTarget TextureTarget3D
    QueryableTextureTarget[TextureTargetCubeMapFace]        : QueryableTextureTarget TextureTargetCubeMapFace
    QueryableTextureTarget[TextureTarget2DMultisample]      : QueryableTextureTarget TextureTarget2DMultisample
    QueryableTextureTarget[TextureTarget2D]                 : QueryableTextureTarget TextureTarget2D
    QueryableTextureTarget[TextureTarget1D]                 : QueryableTextureTarget TextureTarget1D

{-# FOREIGN GHC data AgdaQueryableTextureTarget a = Graphics.Rendering.OpenGL.GL.Texturing.Specification.QueryableTextureTarget a => AgdaQueryableTextureTarget #-}
{-# COMPILE GHC QueryableTextureTarget = type(0) AgdaQueryableTextureTarget #-}

{-# COMPILE GHC QueryableTextureTarget[TextureTarget2DMultisampleArray] = AgdaQueryableTextureTarget #-}
{-# COMPILE GHC QueryableTextureTarget[TextureTarget3D]                 = AgdaQueryableTextureTarget #-}
{-# COMPILE GHC QueryableTextureTarget[TextureTargetCubeMapFace]        = AgdaQueryableTextureTarget #-}
{-# COMPILE GHC QueryableTextureTarget[TextureTarget2DMultisample]      = AgdaQueryableTextureTarget #-}
{-# COMPILE GHC QueryableTextureTarget[TextureTarget2D]                 = AgdaQueryableTextureTarget #-}
{-# COMPILE GHC QueryableTextureTarget[TextureTarget1D]                 = AgdaQueryableTextureTarget #-}


postulate
    GettableTextureTarget : Set → Set

    GettableTextureTarget[TextureTarget3D]          : GettableTextureTarget TextureTarget3D
    GettableTextureTarget[TextureTargetCubeMapFace] : GettableTextureTarget TextureTargetCubeMapFace
    GettableTextureTarget[TextureTarget2D]          : GettableTextureTarget TextureTarget2D
    GettableTextureTarget[TextureTarget1D]          : GettableTextureTarget TextureTarget1D

{-# FOREIGN GHC data AgdaGettableTextureTarget a = Graphics.Rendering.OpenGL.GL.Texturing.Specification.GettableTextureTarget a => AgdaGettableTextureTarget #-}
{-# COMPILE GHC GettableTextureTarget = type(0) AgdaGettableTextureTarget #-}

{-# COMPILE GHC GettableTextureTarget[TextureTarget3D]          = AgdaGettableTextureTarget #-}
{-# COMPILE GHC GettableTextureTarget[TextureTargetCubeMapFace] = AgdaGettableTextureTarget #-}
{-# COMPILE GHC GettableTextureTarget[TextureTarget2D]          = AgdaGettableTextureTarget #-}
{-# COMPILE GHC GettableTextureTarget[TextureTarget1D]          = AgdaGettableTextureTarget #-}


Level : Set
Level = GLint

Border : Set
Border = GLint


data TexturePosition1D : Set where
    mkTexturePosition1D : GLint → TexturePosition1D

{-# COMPILE GHC TexturePosition1D = data Graphics.Rendering.OpenGL.GL.Texturing.Specification.TexturePosition1D
    ( Graphics.Rendering.OpenGL.GL.Texturing.Specification.TexturePosition1D
    ) #-}

postulate
    Eq[TexturePosition1D]   : Eq TexturePosition1D
    Ord[TexturePosition1D]  : Ord TexturePosition1D
    Show[TexturePosition1D] : Show TexturePosition1D

{-# COMPILE GHC Eq[TexturePosition1D]   = AgdaEq   #-}
{-# COMPILE GHC Ord[TexturePosition1D]  = AgdaOrd  #-}
{-# COMPILE GHC Show[TexturePosition1D] = AgdaShow #-}


data TexturePosition2D : Set where
    mkTexturePosition2D : GLint → GLint → TexturePosition2D

{-# COMPILE GHC TexturePosition2D = data Graphics.Rendering.OpenGL.GL.Texturing.Specification.TexturePosition2D
    ( Graphics.Rendering.OpenGL.GL.Texturing.Specification.TexturePosition2D
    ) #-}

postulate
    Eq[TexturePosition2D]   : Eq TexturePosition2D
    Ord[TexturePosition2D]  : Ord TexturePosition2D
    Show[TexturePosition2D] : Show TexturePosition2D

{-# COMPILE GHC Eq[TexturePosition2D]   = AgdaEq   #-}
{-# COMPILE GHC Ord[TexturePosition2D]  = AgdaOrd  #-}
{-# COMPILE GHC Show[TexturePosition2D] = AgdaShow #-}


data TexturePosition3D : Set where
    mkTexturePosition3D : GLint → GLint → GLint → TexturePosition3D

{-# COMPILE GHC TexturePosition3D = data Graphics.Rendering.OpenGL.GL.Texturing.Specification.TexturePosition3D
    ( Graphics.Rendering.OpenGL.GL.Texturing.Specification.TexturePosition3D
    ) #-}

postulate
    Eq[TexturePosition3D]   : Eq TexturePosition3D
    Ord[TexturePosition3D]  : Ord TexturePosition3D
    Show[TexturePosition3D] : Show TexturePosition3D

{-# COMPILE GHC Eq[TexturePosition3D]   = AgdaEq   #-}
{-# COMPILE GHC Ord[TexturePosition3D]  = AgdaOrd  #-}
{-# COMPILE GHC Show[TexturePosition3D] = AgdaShow #-}


data TextureSize1D : Set where
    mkTextureSize1D : GLint → TextureSize1D

{-# COMPILE GHC TextureSize1D = data Graphics.Rendering.OpenGL.GL.Texturing.Specification.TextureSize1D
    ( Graphics.Rendering.OpenGL.GL.Texturing.Specification.TextureSize1D
    ) #-}

postulate
    Eq[TextureSize1D]   : Eq TextureSize1D
    Ord[TextureSize1D]  : Ord TextureSize1D
    Show[TextureSize1D] : Show TextureSize1D

{-# COMPILE GHC Eq[TextureSize1D]   = AgdaEq   #-}
{-# COMPILE GHC Ord[TextureSize1D]  = AgdaOrd  #-}
{-# COMPILE GHC Show[TextureSize1D] = AgdaShow #-}


data TextureSize2D : Set where
    mkTextureSize2D : GLint → GLint → TextureSize2D

{-# COMPILE GHC TextureSize2D = data Graphics.Rendering.OpenGL.GL.Texturing.Specification.TextureSize2D
    ( Graphics.Rendering.OpenGL.GL.Texturing.Specification.TextureSize2D
    ) #-}

postulate
    Eq[TextureSize2D]   : Eq TextureSize2D
    Ord[TextureSize2D]  : Ord TextureSize2D
    Show[TextureSize2D] : Show TextureSize2D

{-# COMPILE GHC Eq[TextureSize2D]   = AgdaEq   #-}
{-# COMPILE GHC Ord[TextureSize2D]  = AgdaOrd  #-}
{-# COMPILE GHC Show[TextureSize2D] = AgdaShow #-}


data TextureSize3D : Set where
    mkTextureSize3D : GLint → GLint → GLint → TextureSize3D

{-# COMPILE GHC TextureSize3D = data Graphics.Rendering.OpenGL.GL.Texturing.Specification.TextureSize3D
    ( Graphics.Rendering.OpenGL.GL.Texturing.Specification.TextureSize3D
    ) #-}

postulate
    Eq[TextureSize3D]   : Eq TextureSize3D
    Ord[TextureSize3D]  : Ord TextureSize3D
    Show[TextureSize3D] : Show TextureSize3D

{-# COMPILE GHC Eq[TextureSize3D]   = AgdaEq   #-}
{-# COMPILE GHC Ord[TextureSize3D]  = AgdaOrd  #-}
{-# COMPILE GHC Show[TextureSize3D] = AgdaShow #-}


postulate
    texImage1D     : ⦃ OneDimensionalTextureTarget T ⦄ → T → Proxy → Level → PixelInternalFormat → TextureSize1D → Border → PixelData A → IO ⊤
    texImage2D     : ⦃ TwoDimensionalTextureTarget T ⦄ → T → Proxy → Level → PixelInternalFormat → TextureSize2D → Border → PixelData A → IO ⊤
    texImage3D     : ⦃ ThreeDimensionalTextureTarget T ⦄ → T → Proxy → Level → PixelInternalFormat → TextureSize3D → Border → PixelData A → IO ⊤
    copyTexImage1D : ⦃ OneDimensionalTextureTarget T ⦄ → T → Level → PixelInternalFormat → Position → TextureSize1D → Border → IO ⊤
    copyTexImage2D : ⦃ TwoDimensionalTextureTarget T ⦄ → T → Level → PixelInternalFormat → Position → TextureSize2D → Border → IO ⊤
    texSubImage1D  : ⦃ OneDimensionalTextureTarget T ⦄ → T → Level → TexturePosition1D → TextureSize1D → PixelData A → IO ⊤
    texSubImage2D  : ⦃ TwoDimensionalTextureTarget T ⦄ → T → Level → TexturePosition2D → TextureSize2D → PixelData A → IO ⊤
    texSubImage3D  : ⦃ ThreeDimensionalTextureTarget T ⦄ → T → Level → TexturePosition3D → TextureSize3D → PixelData A → IO ⊤
    getTexImage    : ⦃ GettableTextureTarget T ⦄ → T → Level → PixelData A → IO ⊤

    copyTexSubImage1D : ⦃ OneDimensionalTextureTarget T ⦄ → T → Level → TexturePosition1D → Position → TextureSize1D → IO ⊤
    copyTexSubImage2D : ⦃ TwoDimensionalTextureTarget T ⦄ → T → Level → TexturePosition2D → Position → TextureSize2D → IO ⊤
    copyTexSubImage3D : ⦃ ThreeDimensionalTextureTarget T ⦄ → T → Level → TexturePosition3D → Position → TextureSize2D → IO ⊤

{-# COMPILE GHC texImage1D     = \ t aℓ a AgdaOneDimensionalTextureTarget   -> Graphics.Rendering.OpenGL.GL.Texturing.Specification.texImage1D     #-}
{-# COMPILE GHC texImage2D     = \ t aℓ a AgdaTwoDimensionalTextureTarget   -> Graphics.Rendering.OpenGL.GL.Texturing.Specification.texImage2D     #-}
{-# COMPILE GHC texImage3D     = \ t aℓ a AgdaThreeDimensionalTextureTarget -> Graphics.Rendering.OpenGL.GL.Texturing.Specification.texImage3D     #-}
{-# COMPILE GHC copyTexImage1D = \ t      AgdaOneDimensionalTextureTarget   -> Graphics.Rendering.OpenGL.GL.Texturing.Specification.copyTexImage1D #-}
{-# COMPILE GHC copyTexImage2D = \ t      AgdaTwoDimensionalTextureTarget   -> Graphics.Rendering.OpenGL.GL.Texturing.Specification.copyTexImage2D #-}
{-# COMPILE GHC texSubImage1D  = \ t aℓ a AgdaOneDimensionalTextureTarget   -> Graphics.Rendering.OpenGL.GL.Texturing.Specification.texSubImage1D  #-}
{-# COMPILE GHC texSubImage2D  = \ t aℓ a AgdaTwoDimensionalTextureTarget   -> Graphics.Rendering.OpenGL.GL.Texturing.Specification.texSubImage2D  #-}
{-# COMPILE GHC texSubImage3D  = \ t aℓ a AgdaThreeDimensionalTextureTarget -> Graphics.Rendering.OpenGL.GL.Texturing.Specification.texSubImage3D  #-}
{-# COMPILE GHC getTexImage    = \ t aℓ a AgdaGettableTextureTarget         -> Graphics.Rendering.OpenGL.GL.Texturing.Specification.getTexImage    #-}

{-# COMPILE GHC copyTexSubImage1D = \ t AgdaOneDimensionalTextureTarget   -> Graphics.Rendering.OpenGL.GL.Texturing.Specification.copyTexSubImage1D #-}
{-# COMPILE GHC copyTexSubImage2D = \ t AgdaTwoDimensionalTextureTarget   -> Graphics.Rendering.OpenGL.GL.Texturing.Specification.copyTexSubImage2D #-}
{-# COMPILE GHC copyTexSubImage3D = \ t AgdaThreeDimensionalTextureTarget -> Graphics.Rendering.OpenGL.GL.Texturing.Specification.copyTexSubImage3D #-}


data CompressedTextureFormat : Set where
    mkCompressedTextureFormat : GLenum → CompressedTextureFormat

{-# COMPILE GHC CompressedTextureFormat = data Graphics.Rendering.OpenGL.GL.Texturing.Specification.CompressedTextureFormat
    ( Graphics.Rendering.OpenGL.GL.Texturing.Specification.CompressedTextureFormat
    ) #-}

postulate
    Eq[CompressedTextureFormat]   : Eq CompressedTextureFormat
    Ord[CompressedTextureFormat]  : Ord CompressedTextureFormat
    Show[CompressedTextureFormat] : Show CompressedTextureFormat

{-# COMPILE GHC Eq[CompressedTextureFormat]   = AgdaEq   #-}
{-# COMPILE GHC Ord[CompressedTextureFormat]  = AgdaOrd  #-}
{-# COMPILE GHC Show[CompressedTextureFormat] = AgdaShow #-}

postulate
    compressedTextureFormats : GettableStateVar (List CompressedTextureFormat)

{-# COMPILE GHC compressedTextureFormats = Graphics.Rendering.OpenGL.GL.Texturing.Specification.compressedTextureFormats #-}


data CompressedPixelData (A : Set aℓ) : Set aℓ where
    mkCompressedPixelData : CompressedTextureFormat → GLsizei → Ptr A → CompressedPixelData A

{-# FOREIGN GHC type AgdaCompressedPixelData aℓ = Graphics.Rendering.OpenGL.GL.Texturing.Specification.CompressedPixelData #-}
{-# COMPILE GHC CompressedPixelData = data(1) AgdaCompressedPixelData
    ( Graphics.Rendering.OpenGL.GL.Texturing.Specification.CompressedPixelData
    ) #-}

postulate
    Eq[CompressedPixelData[A]]   : Eq (CompressedPixelData A)
    Ord[CompressedPixelData[A]]  : Ord (CompressedPixelData A)
    Show[CompressedPixelData[A]] : Show (CompressedPixelData A)

{-# COMPILE GHC Eq[CompressedPixelData[A]]   = \ aℓ a -> AgdaEq   #-}
{-# COMPILE GHC Ord[CompressedPixelData[A]]  = \ aℓ a -> AgdaOrd  #-}
{-# COMPILE GHC Show[CompressedPixelData[A]] = \ aℓ a -> AgdaShow #-}


postulate
    compressedTexImage1D    : ⦃ OneDimensionalTextureTarget T ⦄ → T → Proxy → Level → TextureSize1D → Border → CompressedPixelData A → IO ⊤
    compressedTexImage2D    : ⦃ TwoDimensionalTextureTarget T ⦄ → T → Proxy → Level → TextureSize2D → Border → CompressedPixelData A → IO ⊤
    compressedTexImage3D    : ⦃ ThreeDimensionalTextureTarget T ⦄ → T → Proxy → Level → TextureSize3D → Border → CompressedPixelData A → IO ⊤
    compressedTexSubImage1D : ⦃ OneDimensionalTextureTarget T ⦄ → T → Level → TexturePosition1D → TextureSize1D → CompressedPixelData A → IO ⊤
    compressedTexSubImage2D : ⦃ TwoDimensionalTextureTarget T ⦄ → T → Level → TexturePosition2D → TextureSize2D → CompressedPixelData A → IO ⊤
    compressedTexSubImage3D : ⦃ ThreeDimensionalTextureTarget T ⦄ → T → Level → TexturePosition3D → TextureSize3D → CompressedPixelData A → IO ⊤
    getCompressedTexImage   : ⦃ GettableTextureTarget T ⦄ → T → Level → Ptr A → IO ⊤

{-# COMPILE GHC compressedTexImage1D    = \ t aℓ a AgdaOneDimensionalTextureTarget   -> Graphics.Rendering.OpenGL.GL.Texturing.Specification.compressedTexImage1D    #-}
{-# COMPILE GHC compressedTexImage2D    = \ t aℓ a AgdaTwoDimensionalTextureTarget   -> Graphics.Rendering.OpenGL.GL.Texturing.Specification.compressedTexImage2D    #-}
{-# COMPILE GHC compressedTexImage3D    = \ t aℓ a AgdaThreeDimensionalTextureTarget -> Graphics.Rendering.OpenGL.GL.Texturing.Specification.compressedTexImage3D    #-}
{-# COMPILE GHC compressedTexSubImage1D = \ t aℓ a AgdaOneDimensionalTextureTarget   -> Graphics.Rendering.OpenGL.GL.Texturing.Specification.compressedTexSubImage1D #-}
{-# COMPILE GHC compressedTexSubImage2D = \ t aℓ a AgdaTwoDimensionalTextureTarget   -> Graphics.Rendering.OpenGL.GL.Texturing.Specification.compressedTexSubImage2D #-}
{-# COMPILE GHC compressedTexSubImage3D = \ t aℓ a AgdaThreeDimensionalTextureTarget -> Graphics.Rendering.OpenGL.GL.Texturing.Specification.compressedTexSubImage3D #-}
{-# COMPILE GHC getCompressedTexImage   = \ t aℓ a AgdaGettableTextureTarget         -> Graphics.Rendering.OpenGL.GL.Texturing.Specification.getCompressedTexImage   #-}


data SampleLocations : Set where
    FlexibleSampleLocations : SampleLocations
    FixedSampleLocations    : SampleLocations

{-# COMPILE GHC SampleLocations = data Graphics.Rendering.OpenGL.GL.Texturing.Specification.SampleLocations
    ( Graphics.Rendering.OpenGL.GL.Texturing.Specification.FlexibleSampleLocations
    | Graphics.Rendering.OpenGL.GL.Texturing.Specification.FixedSampleLocations
    ) #-}

postulate
    Eq[SampleLocations]   : Eq SampleLocations
    Ord[SampleLocations]  : Ord SampleLocations
    Show[SampleLocations] : Show SampleLocations

{-# COMPILE GHC Eq[SampleLocations]   = AgdaEq   #-}
{-# COMPILE GHC Ord[SampleLocations]  = AgdaOrd  #-}
{-# COMPILE GHC Show[SampleLocations] = AgdaShow #-}

postulate
    texImage2DMultisample : TextureTarget2DMultisample → Proxy → Samples → PixelInternalFormat → TextureSize2D → SampleLocations → IO ⊤
    texImage3DMultisample : TextureTarget2DMultisampleArray → Proxy → Samples → PixelInternalFormat → TextureSize3D → SampleLocations → IO ⊤

{-# COMPILE GHC texImage2DMultisample = Graphics.Rendering.OpenGL.GL.Texturing.Specification.texImage2DMultisample #-}
{-# COMPILE GHC texImage3DMultisample = Graphics.Rendering.OpenGL.GL.Texturing.Specification.texImage3DMultisample #-}

postulate
    maxTextureSize          : GettableStateVar GLsizei
    maxCubeMapTextureSize   : GettableStateVar GLsizei
    maxRectangleTextureSize : GettableStateVar GLsizei
    max3DTextureSize        : GettableStateVar GLsizei
    maxArrayTextureLayers   : GettableStateVar GLsizei
    maxSampleMaskWords      : GettableStateVar GLsizei
    maxColorTextureSamples  : GettableStateVar GLsizei
    maxDepthTextureSamples  : GettableStateVar GLsizei
    maxIntegerSamples       : GettableStateVar GLsizei

{-# COMPILE GHC maxTextureSize          = Graphics.Rendering.OpenGL.GL.Texturing.Specification.maxTextureSize          #-}
{-# COMPILE GHC maxCubeMapTextureSize   = Graphics.Rendering.OpenGL.GL.Texturing.Specification.maxCubeMapTextureSize   #-}
{-# COMPILE GHC maxRectangleTextureSize = Graphics.Rendering.OpenGL.GL.Texturing.Specification.maxRectangleTextureSize #-}
{-# COMPILE GHC max3DTextureSize        = Graphics.Rendering.OpenGL.GL.Texturing.Specification.max3DTextureSize        #-}
{-# COMPILE GHC maxArrayTextureLayers   = Graphics.Rendering.OpenGL.GL.Texturing.Specification.maxArrayTextureLayers   #-}
{-# COMPILE GHC maxSampleMaskWords      = Graphics.Rendering.OpenGL.GL.Texturing.Specification.maxSampleMaskWords      #-}
{-# COMPILE GHC maxColorTextureSamples  = Graphics.Rendering.OpenGL.GL.Texturing.Specification.maxColorTextureSamples  #-}
{-# COMPILE GHC maxDepthTextureSamples  = Graphics.Rendering.OpenGL.GL.Texturing.Specification.maxDepthTextureSamples  #-}
{-# COMPILE GHC maxIntegerSamples       = Graphics.Rendering.OpenGL.GL.Texturing.Specification.maxIntegerSamples       #-}
