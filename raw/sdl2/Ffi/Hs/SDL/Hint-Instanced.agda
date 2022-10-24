{-# OPTIONS --without-K #-}

module Ffi.Hs.SDL.Hint-Instanced where

open import Ffi.Hs.SDL.Hint

instance
    inst:Bounded[AccelerometerJoystickOptions] = Bounded[AccelerometerJoystickOptions]
    inst:Enum[AccelerometerJoystickOptions]    = Enum[AccelerometerJoystickOptions]
    inst:Eq[AccelerometerJoystickOptions]      = Eq[AccelerometerJoystickOptions]
    inst:Data[AccelerometerJoystickOptions]    = Data[AccelerometerJoystickOptions]
    inst:Ord[AccelerometerJoystickOptions]     = Ord[AccelerometerJoystickOptions]
    inst:Read[AccelerometerJoystickOptions]    = Read[AccelerometerJoystickOptions]
    inst:Show[AccelerometerJoystickOptions]    = Show[AccelerometerJoystickOptions]

    inst:Bounded[FramebufferAccelerationOptions] = Bounded[FramebufferAccelerationOptions]
    inst:Enum[FramebufferAccelerationOptions]    = Enum[FramebufferAccelerationOptions]
    inst:Eq[FramebufferAccelerationOptions]      = Eq[FramebufferAccelerationOptions]
    inst:Data[FramebufferAccelerationOptions]    = Data[FramebufferAccelerationOptions]
    inst:Ord[FramebufferAccelerationOptions]     = Ord[FramebufferAccelerationOptions]
    inst:Read[FramebufferAccelerationOptions]    = Read[FramebufferAccelerationOptions]
    inst:Show[FramebufferAccelerationOptions]    = Show[FramebufferAccelerationOptions]

    inst:Bounded[MacCTRLClickOptions] = Bounded[MacCTRLClickOptions]
    inst:Enum[MacCTRLClickOptions]    = Enum[MacCTRLClickOptions]
    inst:Eq[MacCTRLClickOptions]      = Eq[MacCTRLClickOptions]
    inst:Data[MacCTRLClickOptions]    = Data[MacCTRLClickOptions]
    inst:Ord[MacCTRLClickOptions]     = Ord[MacCTRLClickOptions]
    inst:Read[MacCTRLClickOptions]    = Read[MacCTRLClickOptions]
    inst:Show[MacCTRLClickOptions]    = Show[MacCTRLClickOptions]

    inst:Bounded[MouseModeWarpOptions] = Bounded[MouseModeWarpOptions]
    inst:Enum[MouseModeWarpOptions]    = Enum[MouseModeWarpOptions]
    inst:Eq[MouseModeWarpOptions]      = Eq[MouseModeWarpOptions]
    inst:Data[MouseModeWarpOptions]    = Data[MouseModeWarpOptions]
    inst:Ord[MouseModeWarpOptions]     = Ord[MouseModeWarpOptions]
    inst:Read[MouseModeWarpOptions]    = Read[MouseModeWarpOptions]
    inst:Show[MouseModeWarpOptions]    = Show[MouseModeWarpOptions]

    inst:Bounded[RenderDrivers] = Bounded[RenderDrivers]
    inst:Enum[RenderDrivers]    = Enum[RenderDrivers]
    inst:Eq[RenderDrivers]      = Eq[RenderDrivers]
    inst:Data[RenderDrivers]    = Data[RenderDrivers]
    inst:Ord[RenderDrivers]     = Ord[RenderDrivers]
    inst:Read[RenderDrivers]    = Read[RenderDrivers]
    inst:Show[RenderDrivers]    = Show[RenderDrivers]

    inst:Bounded[RenderOpenGLShaderOptions] = Bounded[RenderOpenGLShaderOptions]
    inst:Enum[RenderOpenGLShaderOptions]    = Enum[RenderOpenGLShaderOptions]
    inst:Eq[RenderOpenGLShaderOptions]      = Eq[RenderOpenGLShaderOptions]
    inst:Data[RenderOpenGLShaderOptions]    = Data[RenderOpenGLShaderOptions]
    inst:Ord[RenderOpenGLShaderOptions]     = Ord[RenderOpenGLShaderOptions]
    inst:Read[RenderOpenGLShaderOptions]    = Read[RenderOpenGLShaderOptions]
    inst:Show[RenderOpenGLShaderOptions]    = Show[RenderOpenGLShaderOptions]

    inst:Bounded[RenderScaleQuality] = Bounded[RenderScaleQuality]
    inst:Enum[RenderScaleQuality]    = Enum[RenderScaleQuality]
    inst:Eq[RenderScaleQuality]      = Eq[RenderScaleQuality]
    inst:Data[RenderScaleQuality]    = Data[RenderScaleQuality]
    inst:Ord[RenderScaleQuality]     = Ord[RenderScaleQuality]
    inst:Read[RenderScaleQuality]    = Read[RenderScaleQuality]
    inst:Show[RenderScaleQuality]    = Show[RenderScaleQuality]

    inst:Bounded[RenderVSyncOptions] = Bounded[RenderVSyncOptions]
    inst:Enum[RenderVSyncOptions]    = Enum[RenderVSyncOptions]
    inst:Eq[RenderVSyncOptions]      = Eq[RenderVSyncOptions]
    inst:Data[RenderVSyncOptions]    = Data[RenderVSyncOptions]
    inst:Ord[RenderVSyncOptions]     = Ord[RenderVSyncOptions]
    inst:Read[RenderVSyncOptions]    = Read[RenderVSyncOptions]
    inst:Show[RenderVSyncOptions]    = Show[RenderVSyncOptions]

    inst:Bounded[VideoWinD3DCompilerOptions] = Bounded[VideoWinD3DCompilerOptions]
    inst:Enum[VideoWinD3DCompilerOptions]    = Enum[VideoWinD3DCompilerOptions]
    inst:Eq[VideoWinD3DCompilerOptions]      = Eq[VideoWinD3DCompilerOptions]
    inst:Data[VideoWinD3DCompilerOptions]    = Data[VideoWinD3DCompilerOptions]
    inst:Ord[VideoWinD3DCompilerOptions]     = Ord[VideoWinD3DCompilerOptions]
    inst:Read[VideoWinD3DCompilerOptions]    = Read[VideoWinD3DCompilerOptions]
    inst:Show[VideoWinD3DCompilerOptions]    = Show[VideoWinD3DCompilerOptions]

    inst:HasGetter[Hint[V],V] = HasGetter[Hint[V],V]
    inst:HasSetter[Hint[V],V] = HasSetter[Hint[V],V]

    inst:Bounded[HintPriority] = Bounded[HintPriority]
    inst:Enum[HintPriority]    = Enum[HintPriority]
    inst:Eq[HintPriority]      = Eq[HintPriority]
    inst:Data[HintPriority]    = Data[HintPriority]
    inst:Ord[HintPriority]     = Ord[HintPriority]
    inst:Read[HintPriority]    = Read[HintPriority]
    inst:Show[HintPriority]    = Show[HintPriority]
