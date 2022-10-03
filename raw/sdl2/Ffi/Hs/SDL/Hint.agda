{-# OPTIONS --without-K #-}

module Ffi.Hs.SDL.Hint where

open import Agda.Builtin.Bool    using (Bool)
open import Agda.Primitive       using (Level)
open import Ffi.Hs.-base.Class
open import Ffi.Hs.-base.Level   using (Liftℓ)
open import Ffi.Hs.-base.Unit    using (⊤′)
open import Ffi.Hs.Data.StateVar using (HasGetter; HasSetter)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC {-# LANGUAGE GADTs #-} #-}
{-# FOREIGN GHC
import qualified SDL.Hint
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
import MAlonzo.Code.Ffi.Hs.Data.StateVar (AgdaHasGetter(AgdaHasGetter), AgdaHasSetter(AgdaHasSetter))
#-}

private
    variable
        aℓ : Level
        M : Set aℓ → Set aℓ
        V : Set


data AccelerometerJoystickOptions : Set where
    AccelerometerNotJoystick : AccelerometerJoystickOptions
    AccelerometerIsJoystick  : AccelerometerJoystickOptions

{-# COMPILE GHC AccelerometerJoystickOptions = data SDL.Hint.AccelerometerJoystickOptions
    ( SDL.Hint.AccelerometerNotJoystick
    | SDL.Hint.AccelerometerIsJoystick
    ) #-}

postulate
    Bounded[AccelerometerJoystickOptions] : Bounded AccelerometerJoystickOptions
    Enum[AccelerometerJoystickOptions]    : Enum AccelerometerJoystickOptions
    Eq[AccelerometerJoystickOptions]      : Eq AccelerometerJoystickOptions
    Data[AccelerometerJoystickOptions]    : Data AccelerometerJoystickOptions
    Ord[AccelerometerJoystickOptions]     : Ord AccelerometerJoystickOptions
    Read[AccelerometerJoystickOptions]    : Read AccelerometerJoystickOptions
    Show[AccelerometerJoystickOptions]    : Show AccelerometerJoystickOptions

{-# COMPILE GHC Bounded[AccelerometerJoystickOptions] = AgdaBounded #-}
{-# COMPILE GHC Enum[AccelerometerJoystickOptions]    = AgdaEnum    #-}
{-# COMPILE GHC Eq[AccelerometerJoystickOptions]      = AgdaEq      #-}
{-# COMPILE GHC Data[AccelerometerJoystickOptions]    = AgdaData    #-}
{-# COMPILE GHC Ord[AccelerometerJoystickOptions]     = AgdaOrd     #-}
{-# COMPILE GHC Read[AccelerometerJoystickOptions]    = AgdaRead    #-}
{-# COMPILE GHC Show[AccelerometerJoystickOptions]    = AgdaShow    #-}


data FramebufferAccelerationOptions : Set where
    Disable3D         : FramebufferAccelerationOptions
    Enable3DDefault   : FramebufferAccelerationOptions
    Enable3DDirect3D  : FramebufferAccelerationOptions
    Enable3DOpenGL    : FramebufferAccelerationOptions
    Enable3DOpenGLES  : FramebufferAccelerationOptions
    Enable3DOpenGLES2 : FramebufferAccelerationOptions
    Enable3DSoftware  : FramebufferAccelerationOptions

{-# COMPILE GHC FramebufferAccelerationOptions = data SDL.Hint.FramebufferAccelerationOptions
    ( SDL.Hint.Disable3D
    | SDL.Hint.Enable3DDefault
    | SDL.Hint.Enable3DDirect3D
    | SDL.Hint.Enable3DOpenGL
    | SDL.Hint.Enable3DOpenGLES
    | SDL.Hint.Enable3DOpenGLES2
    | SDL.Hint.Enable3DSoftware
    ) #-}

postulate
    Bounded[FramebufferAccelerationOptions] : Bounded FramebufferAccelerationOptions
    Enum[FramebufferAccelerationOptions]    : Enum FramebufferAccelerationOptions
    Eq[FramebufferAccelerationOptions]      : Eq FramebufferAccelerationOptions
    Data[FramebufferAccelerationOptions]    : Data FramebufferAccelerationOptions
    Ord[FramebufferAccelerationOptions]     : Ord FramebufferAccelerationOptions
    Read[FramebufferAccelerationOptions]    : Read FramebufferAccelerationOptions
    Show[FramebufferAccelerationOptions]    : Show FramebufferAccelerationOptions

{-# COMPILE GHC Bounded[FramebufferAccelerationOptions] = AgdaBounded #-}
{-# COMPILE GHC Enum[FramebufferAccelerationOptions]    = AgdaEnum    #-}
{-# COMPILE GHC Eq[FramebufferAccelerationOptions]      = AgdaEq      #-}
{-# COMPILE GHC Data[FramebufferAccelerationOptions]    = AgdaData    #-}
{-# COMPILE GHC Ord[FramebufferAccelerationOptions]     = AgdaOrd     #-}
{-# COMPILE GHC Read[FramebufferAccelerationOptions]    = AgdaRead    #-}
{-# COMPILE GHC Show[FramebufferAccelerationOptions]    = AgdaShow    #-}


data MacCTRLClickOptions : Set where
    NoRightClick      : MacCTRLClickOptions
    EmulateRightClick : MacCTRLClickOptions

{-# COMPILE GHC MacCTRLClickOptions = data SDL.Hint.MacCTRLClickOptions
    ( SDL.Hint.NoRightClick
    | SDL.Hint.EmulateRightClick
    ) #-}

postulate
    Bounded[MacCTRLClickOptions] : Bounded MacCTRLClickOptions
    Enum[MacCTRLClickOptions]    : Enum MacCTRLClickOptions
    Eq[MacCTRLClickOptions]      : Eq MacCTRLClickOptions
    Data[MacCTRLClickOptions]    : Data MacCTRLClickOptions
    Ord[MacCTRLClickOptions]     : Ord MacCTRLClickOptions
    Read[MacCTRLClickOptions]    : Read MacCTRLClickOptions
    Show[MacCTRLClickOptions]    : Show MacCTRLClickOptions

{-# COMPILE GHC Bounded[MacCTRLClickOptions] = AgdaBounded #-}
{-# COMPILE GHC Enum[MacCTRLClickOptions]    = AgdaEnum    #-}
{-# COMPILE GHC Eq[MacCTRLClickOptions]      = AgdaEq      #-}
{-# COMPILE GHC Data[MacCTRLClickOptions]    = AgdaData    #-}
{-# COMPILE GHC Ord[MacCTRLClickOptions]     = AgdaOrd     #-}
{-# COMPILE GHC Read[MacCTRLClickOptions]    = AgdaRead    #-}
{-# COMPILE GHC Show[MacCTRLClickOptions]    = AgdaShow    #-}


data MouseModeWarpOptions : Set where
    MouseRawInput : MouseModeWarpOptions
    MouseWarping  : MouseModeWarpOptions

{-# COMPILE GHC MouseModeWarpOptions = data SDL.Hint.MouseModeWarpOptions
    ( SDL.Hint.MouseRawInput
    | SDL.Hint.MouseWarping
    ) #-}

postulate
    Bounded[MouseModeWarpOptions] : Bounded MouseModeWarpOptions
    Enum[MouseModeWarpOptions]    : Enum MouseModeWarpOptions
    Eq[MouseModeWarpOptions]      : Eq MouseModeWarpOptions
    Data[MouseModeWarpOptions]    : Data MouseModeWarpOptions
    Ord[MouseModeWarpOptions]     : Ord MouseModeWarpOptions
    Read[MouseModeWarpOptions]    : Read MouseModeWarpOptions
    Show[MouseModeWarpOptions]    : Show MouseModeWarpOptions

{-# COMPILE GHC Bounded[MouseModeWarpOptions] = AgdaBounded #-}
{-# COMPILE GHC Enum[MouseModeWarpOptions]    = AgdaEnum    #-}
{-# COMPILE GHC Eq[MouseModeWarpOptions]      = AgdaEq      #-}
{-# COMPILE GHC Data[MouseModeWarpOptions]    = AgdaData    #-}
{-# COMPILE GHC Ord[MouseModeWarpOptions]     = AgdaOrd     #-}
{-# COMPILE GHC Read[MouseModeWarpOptions]    = AgdaRead    #-}
{-# COMPILE GHC Show[MouseModeWarpOptions]    = AgdaShow    #-}


data RenderDrivers : Set where
    Direct3D  : RenderDrivers
    OpenGL    : RenderDrivers
    OpenGLES  : RenderDrivers
    OpenGLES2 : RenderDrivers
    Software  : RenderDrivers

{-# COMPILE GHC RenderDrivers = data SDL.Hint.RenderDrivers
    ( SDL.Hint.Direct3D
    | SDL.Hint.OpenGL
    | SDL.Hint.OpenGLES
    | SDL.Hint.OpenGLES2
    | SDL.Hint.Software
    ) #-}

postulate
    Bounded[RenderDrivers] : Bounded RenderDrivers
    Enum[RenderDrivers]    : Enum RenderDrivers
    Eq[RenderDrivers]      : Eq RenderDrivers
    Data[RenderDrivers]    : Data RenderDrivers
    Ord[RenderDrivers]     : Ord RenderDrivers
    Read[RenderDrivers]    : Read RenderDrivers
    Show[RenderDrivers]    : Show RenderDrivers

{-# COMPILE GHC Bounded[RenderDrivers] = AgdaBounded #-}
{-# COMPILE GHC Enum[RenderDrivers]    = AgdaEnum    #-}
{-# COMPILE GHC Eq[RenderDrivers]      = AgdaEq      #-}
{-# COMPILE GHC Data[RenderDrivers]    = AgdaData    #-}
{-# COMPILE GHC Ord[RenderDrivers]     = AgdaOrd     #-}
{-# COMPILE GHC Read[RenderDrivers]    = AgdaRead    #-}
{-# COMPILE GHC Show[RenderDrivers]    = AgdaShow    #-}


data RenderOpenGLShaderOptions : Set where
    DisableShaders : RenderOpenGLShaderOptions
    EnableShaders  : RenderOpenGLShaderOptions

{-# COMPILE GHC RenderOpenGLShaderOptions = data SDL.Hint.RenderOpenGLShaderOptions
    ( SDL.Hint.DisableShaders
    | SDL.Hint.EnableShaders
    ) #-}

postulate
    Bounded[RenderOpenGLShaderOptions] : Bounded RenderOpenGLShaderOptions
    Enum[RenderOpenGLShaderOptions]    : Enum RenderOpenGLShaderOptions
    Eq[RenderOpenGLShaderOptions]      : Eq RenderOpenGLShaderOptions
    Data[RenderOpenGLShaderOptions]    : Data RenderOpenGLShaderOptions
    Ord[RenderOpenGLShaderOptions]     : Ord RenderOpenGLShaderOptions
    Read[RenderOpenGLShaderOptions]    : Read RenderOpenGLShaderOptions
    Show[RenderOpenGLShaderOptions]    : Show RenderOpenGLShaderOptions

{-# COMPILE GHC Bounded[RenderOpenGLShaderOptions] = AgdaBounded #-}
{-# COMPILE GHC Enum[RenderOpenGLShaderOptions]    = AgdaEnum    #-}
{-# COMPILE GHC Eq[RenderOpenGLShaderOptions]      = AgdaEq      #-}
{-# COMPILE GHC Data[RenderOpenGLShaderOptions]    = AgdaData    #-}
{-# COMPILE GHC Ord[RenderOpenGLShaderOptions]     = AgdaOrd     #-}
{-# COMPILE GHC Read[RenderOpenGLShaderOptions]    = AgdaRead    #-}
{-# COMPILE GHC Show[RenderOpenGLShaderOptions]    = AgdaShow    #-}


data RenderScaleQuality : Set where
    ScaleNearest : RenderScaleQuality
    ScaleLinear  : RenderScaleQuality
    ScaleBest    : RenderScaleQuality

{-# COMPILE GHC RenderScaleQuality = data SDL.Hint.RenderScaleQuality
    ( SDL.Hint.ScaleNearest
    | SDL.Hint.ScaleLinear
    | SDL.Hint.ScaleBest
    ) #-}

postulate
    Bounded[RenderScaleQuality] : Bounded RenderScaleQuality
    Enum[RenderScaleQuality]    : Enum RenderScaleQuality
    Eq[RenderScaleQuality]      : Eq RenderScaleQuality
    Data[RenderScaleQuality]    : Data RenderScaleQuality
    Ord[RenderScaleQuality]     : Ord RenderScaleQuality
    Read[RenderScaleQuality]    : Read RenderScaleQuality
    Show[RenderScaleQuality]    : Show RenderScaleQuality

{-# COMPILE GHC Bounded[RenderScaleQuality] = AgdaBounded #-}
{-# COMPILE GHC Enum[RenderScaleQuality]    = AgdaEnum    #-}
{-# COMPILE GHC Eq[RenderScaleQuality]      = AgdaEq      #-}
{-# COMPILE GHC Data[RenderScaleQuality]    = AgdaData    #-}
{-# COMPILE GHC Ord[RenderScaleQuality]     = AgdaOrd     #-}
{-# COMPILE GHC Read[RenderScaleQuality]    = AgdaRead    #-}
{-# COMPILE GHC Show[RenderScaleQuality]    = AgdaShow    #-}


data RenderVSyncOptions : Set where
    DisableVSync : RenderVSyncOptions
    EnableVSync  : RenderVSyncOptions

{-# COMPILE GHC RenderVSyncOptions = data SDL.Hint.RenderVSyncOptions
    ( SDL.Hint.DisableVSync
    | SDL.Hint.EnableVSync
    ) #-}

postulate
    Bounded[RenderVSyncOptions] : Bounded RenderVSyncOptions
    Enum[RenderVSyncOptions]    : Enum RenderVSyncOptions
    Eq[RenderVSyncOptions]      : Eq RenderVSyncOptions
    Data[RenderVSyncOptions]    : Data RenderVSyncOptions
    Ord[RenderVSyncOptions]     : Ord RenderVSyncOptions
    Read[RenderVSyncOptions]    : Read RenderVSyncOptions
    Show[RenderVSyncOptions]    : Show RenderVSyncOptions

{-# COMPILE GHC Bounded[RenderVSyncOptions] = AgdaBounded #-}
{-# COMPILE GHC Enum[RenderVSyncOptions]    = AgdaEnum    #-}
{-# COMPILE GHC Eq[RenderVSyncOptions]      = AgdaEq      #-}
{-# COMPILE GHC Data[RenderVSyncOptions]    = AgdaData    #-}
{-# COMPILE GHC Ord[RenderVSyncOptions]     = AgdaOrd     #-}
{-# COMPILE GHC Read[RenderVSyncOptions]    = AgdaRead    #-}
{-# COMPILE GHC Show[RenderVSyncOptions]    = AgdaShow    #-}


data VideoWinD3DCompilerOptions : Set where
    D3DVistaOrLater : VideoWinD3DCompilerOptions
    D3DXPSupport    : VideoWinD3DCompilerOptions
    D3DNone         : VideoWinD3DCompilerOptions

{-# COMPILE GHC VideoWinD3DCompilerOptions = data SDL.Hint.VideoWinD3DCompilerOptions
    ( SDL.Hint.D3DVistaOrLater
    | SDL.Hint.D3DXPSupport
    | SDL.Hint.D3DNone
    ) #-}

postulate
    Bounded[VideoWinD3DCompilerOptions] : Bounded VideoWinD3DCompilerOptions
    Enum[VideoWinD3DCompilerOptions]    : Enum VideoWinD3DCompilerOptions
    Eq[VideoWinD3DCompilerOptions]      : Eq VideoWinD3DCompilerOptions
    Data[VideoWinD3DCompilerOptions]    : Data VideoWinD3DCompilerOptions
    Ord[VideoWinD3DCompilerOptions]     : Ord VideoWinD3DCompilerOptions
    Read[VideoWinD3DCompilerOptions]    : Read VideoWinD3DCompilerOptions
    Show[VideoWinD3DCompilerOptions]    : Show VideoWinD3DCompilerOptions

{-# COMPILE GHC Bounded[VideoWinD3DCompilerOptions] = AgdaBounded #-}
{-# COMPILE GHC Enum[VideoWinD3DCompilerOptions]    = AgdaEnum    #-}
{-# COMPILE GHC Eq[VideoWinD3DCompilerOptions]      = AgdaEq      #-}
{-# COMPILE GHC Data[VideoWinD3DCompilerOptions]    = AgdaData    #-}
{-# COMPILE GHC Ord[VideoWinD3DCompilerOptions]     = AgdaOrd     #-}
{-# COMPILE GHC Read[VideoWinD3DCompilerOptions]    = AgdaRead    #-}
{-# COMPILE GHC Show[VideoWinD3DCompilerOptions]    = AgdaShow    #-}


data Hint : Set → Set₁ where
    HintAccelerometerAsJoystick : Hint AccelerometerJoystickOptions
    HintFramebufferAcceleration : Hint FramebufferAccelerationOptions
    HintMacCTRLClick            : Hint MacCTRLClickOptions
    HintMouseRelativeModeWarp   : Hint MouseModeWarpOptions
    HintRenderDriver            : Hint RenderDrivers
    HintRenderOpenGLShaders     : Hint RenderOpenGLShaderOptions
    HintRenderScaleQuality      : Hint RenderScaleQuality
    HintRenderVSync             : Hint RenderVSyncOptions
    HintVideoWinD3DCompiler     : Hint VideoWinD3DCompilerOptions

{-# COMPILE GHC Hint = data SDL.Hint.Hint
    ( SDL.Hint.HintAccelerometerAsJoystick
    | SDL.Hint.HintFramebufferAcceleration
    | SDL.Hint.HintMacCTRLClick
    | SDL.Hint.HintMouseRelativeModeWarp
    | SDL.Hint.HintRenderDriver
    | SDL.Hint.HintRenderOpenGLShaders
    | SDL.Hint.HintRenderScaleQuality
    | SDL.Hint.HintRenderVSync
    | SDL.Hint.HintVideoWinD3DCompiler
    ) #-}

postulate
    HasGetter[Hint[V],V] : HasGetter (Hint V) V
    HasSetter[Hint[V],V] : HasSetter (Hint V) V

{-# COMPILE GHC HasGetter[Hint[V],V] = \ v -> AgdaHasGetter #-}
{-# COMPILE GHC HasSetter[Hint[V],V] = \ v -> AgdaHasSetter #-}


data HintPriority : Set where
    DefaultPriority  : HintPriority
    NormalPriority   : HintPriority
    OverridePriority : HintPriority

{-# COMPILE GHC HintPriority = data SDL.Hint.HintPriority
    ( SDL.Hint.DefaultPriority
    | SDL.Hint.NormalPriority
    | SDL.Hint.OverridePriority
    )  #-}

postulate
    Bounded[HintPriority] : Bounded HintPriority
    Enum[HintPriority]    : Enum HintPriority
    Eq[HintPriority]      : Eq HintPriority
    Data[HintPriority]    : Data HintPriority
    Ord[HintPriority]     : Ord HintPriority
    Read[HintPriority]    : Read HintPriority
    Show[HintPriority]    : Show HintPriority

{-# COMPILE GHC Bounded[HintPriority] = AgdaBounded #-}
{-# COMPILE GHC Enum[HintPriority]    = AgdaEnum    #-}
{-# COMPILE GHC Eq[HintPriority]      = AgdaEq      #-}
{-# COMPILE GHC Data[HintPriority]    = AgdaData    #-}
{-# COMPILE GHC Ord[HintPriority]     = AgdaOrd     #-}
{-# COMPILE GHC Read[HintPriority]    = AgdaRead    #-}
{-# COMPILE GHC Show[HintPriority]    = AgdaShow    #-}


postulate
    setHintWithPriority : ⦃ MonadIO M ⦄ → HintPriority → Hint V → V → M (Liftℓ _ Bool)
    clearHints          : ⦃ MonadIO M ⦄ → M ⊤′

{-# COMPILE GHC setHintWithPriority = \ mℓ m v AgdaMonadIO -> SDL.Hint.setHintWithPriority #-}
{-# COMPILE GHC clearHints          = \ mℓ m AgdaMonadIO   -> SDL.Hint.clearHints          #-}
