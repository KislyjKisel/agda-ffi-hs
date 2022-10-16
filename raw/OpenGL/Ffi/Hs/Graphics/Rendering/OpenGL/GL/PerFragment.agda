{-# OPTIONS --without-K #-}

module Ffi.Hs.Graphics.Rendering.OpenGL.GL.PerFragment where

open import Agda.Builtin.Bool        using (Bool)
open import Agda.Builtin.IO          using (IO)
open import Agda.Builtin.Maybe       using (Maybe)
open import Agda.Primitive
open import Ffi.Hs.-base.Class       using (Eq; Ord; Show)
open import Ffi.Hs.Data.StateVar     using (StateVar)
open import Ffi.Hs.Data.Tuple        using (Tuple2; Tuple3)
open import Ffi.Hs.Graphics.GL.Types

open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.Colors       using (Face)
open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.CoordTrans   using (Position; Size)
open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.Framebuffer  using (DrawBufferIndex)
open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.VertexArrays using (Capability)
open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.VertexSpec   using (Color4)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Graphics.Rendering.OpenGL.GL.PerFragment
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ : Level
        A : Set aℓ


data ComparisonFunction : Set where
    Never    : ComparisonFunction
    Less     : ComparisonFunction
    Equal    : ComparisonFunction
    Lequal   : ComparisonFunction
    Greater  : ComparisonFunction
    Notequal : ComparisonFunction
    Gequal   : ComparisonFunction
    Always   : ComparisonFunction

{-# COMPILE GHC ComparisonFunction = data Graphics.Rendering.OpenGL.GL.PerFragment.ComparisonFunction
    ( Graphics.Rendering.OpenGL.GL.PerFragment.Never
    | Graphics.Rendering.OpenGL.GL.PerFragment.Less
    | Graphics.Rendering.OpenGL.GL.PerFragment.Equal
    | Graphics.Rendering.OpenGL.GL.PerFragment.Lequal
    | Graphics.Rendering.OpenGL.GL.PerFragment.Greater
    | Graphics.Rendering.OpenGL.GL.PerFragment.Notequal
    | Graphics.Rendering.OpenGL.GL.PerFragment.Gequal
    | Graphics.Rendering.OpenGL.GL.PerFragment.Always
    ) #-}

postulate
    Eq[ComparisonFunction]   : Eq ComparisonFunction
    Ord[ComparisonFunction]  : Ord ComparisonFunction
    Show[ComparisonFunction] : Show ComparisonFunction

{-# COMPILE GHC Eq[ComparisonFunction]   = AgdaEq   #-}
{-# COMPILE GHC Ord[ComparisonFunction]  = AgdaOrd  #-}
{-# COMPILE GHC Show[ComparisonFunction] = AgdaShow #-}


data StencilOp : Set where
    OpZero     : StencilOp
    OpKeep     : StencilOp
    OpReplace  : StencilOp
    OpIncr     : StencilOp
    OpIncrWrap : StencilOp
    OpDecr     : StencilOp
    OpDecrWrap : StencilOp
    OpInvert   : StencilOp

{-# COMPILE GHC StencilOp = data Graphics.Rendering.OpenGL.GL.PerFragment.StencilOp
    ( Graphics.Rendering.OpenGL.GL.PerFragment.OpZero
    | Graphics.Rendering.OpenGL.GL.PerFragment.OpKeep
    | Graphics.Rendering.OpenGL.GL.PerFragment.OpReplace
    | Graphics.Rendering.OpenGL.GL.PerFragment.OpIncr
    | Graphics.Rendering.OpenGL.GL.PerFragment.OpIncrWrap
    | Graphics.Rendering.OpenGL.GL.PerFragment.OpDecr
    | Graphics.Rendering.OpenGL.GL.PerFragment.OpDecrWrap
    | Graphics.Rendering.OpenGL.GL.PerFragment.OpInvert
    ) #-}

postulate
    Eq[StencilOp]   : Eq StencilOp
    Ord[StencilOp]  : Ord StencilOp
    Show[StencilOp] : Show StencilOp

{-# COMPILE GHC Eq[StencilOp]   = AgdaEq   #-}
{-# COMPILE GHC Ord[StencilOp]  = AgdaOrd  #-}
{-# COMPILE GHC Show[StencilOp] = AgdaShow #-}


data BlendEquation : Set where
    FuncAdd             : BlendEquation
    FuncSubtract        : BlendEquation
    FuncReverseSubtract : BlendEquation
    Min                 : BlendEquation
    Max                 : BlendEquation
    mkLogicOp           : BlendEquation

{-# COMPILE GHC BlendEquation = data Graphics.Rendering.OpenGL.GL.PerFragment.BlendEquation
    ( Graphics.Rendering.OpenGL.GL.PerFragment.FuncAdd
    | Graphics.Rendering.OpenGL.GL.PerFragment.FuncSubtract
    | Graphics.Rendering.OpenGL.GL.PerFragment.FuncReverseSubtract
    | Graphics.Rendering.OpenGL.GL.PerFragment.Min
    | Graphics.Rendering.OpenGL.GL.PerFragment.Max
    | Graphics.Rendering.OpenGL.GL.PerFragment.LogicOp
    ) #-}

postulate
    Eq[BlendEquation]   : Eq BlendEquation
    Ord[BlendEquation]  : Ord BlendEquation
    Show[BlendEquation] : Show BlendEquation

{-# COMPILE GHC Eq[BlendEquation]   = AgdaEq   #-}
{-# COMPILE GHC Ord[BlendEquation]  = AgdaOrd  #-}
{-# COMPILE GHC Show[BlendEquation] = AgdaShow #-}


data BlendingFactor : Set where
    Zero                  : BlendingFactor
    One                   : BlendingFactor
    SrcColor              : BlendingFactor
    OneMinusSrcColor      : BlendingFactor
    DstColor              : BlendingFactor
    OneMinusDstColor      : BlendingFactor
    SrcAlpha              : BlendingFactor
    OneMinusSrcAlpha      : BlendingFactor
    DstAlpha              : BlendingFactor
    OneMinusDstAlpha      : BlendingFactor
    ConstantColor         : BlendingFactor
    OneMinusConstantColor : BlendingFactor
    ConstantAlpha         : BlendingFactor
    OneMinusConstantAlpha : BlendingFactor
    SrcAlphaSaturate      : BlendingFactor

{-# COMPILE GHC BlendingFactor = data Graphics.Rendering.OpenGL.GL.PerFragment.BlendingFactor
    ( Graphics.Rendering.OpenGL.GL.PerFragment.Zero
    | Graphics.Rendering.OpenGL.GL.PerFragment.One
    | Graphics.Rendering.OpenGL.GL.PerFragment.SrcColor
    | Graphics.Rendering.OpenGL.GL.PerFragment.OneMinusSrcColor
    | Graphics.Rendering.OpenGL.GL.PerFragment.DstColor
    | Graphics.Rendering.OpenGL.GL.PerFragment.OneMinusDstColor
    | Graphics.Rendering.OpenGL.GL.PerFragment.SrcAlpha
    | Graphics.Rendering.OpenGL.GL.PerFragment.OneMinusSrcAlpha
    | Graphics.Rendering.OpenGL.GL.PerFragment.DstAlpha
    | Graphics.Rendering.OpenGL.GL.PerFragment.OneMinusDstAlpha
    | Graphics.Rendering.OpenGL.GL.PerFragment.ConstantColor
    | Graphics.Rendering.OpenGL.GL.PerFragment.OneMinusConstantColor
    | Graphics.Rendering.OpenGL.GL.PerFragment.ConstantAlpha
    | Graphics.Rendering.OpenGL.GL.PerFragment.OneMinusConstantAlpha
    | Graphics.Rendering.OpenGL.GL.PerFragment.SrcAlphaSaturate
    ) #-}

postulate
    Eq[BlendingFactor]   : Eq BlendingFactor
    Ord[BlendingFactor]  : Ord BlendingFactor
    Show[BlendingFactor] : Show BlendingFactor

{-# COMPILE GHC Eq[BlendingFactor]   = AgdaEq   #-}
{-# COMPILE GHC Ord[BlendingFactor]  = AgdaOrd  #-}
{-# COMPILE GHC Show[BlendingFactor] = AgdaShow #-}


data LogicOp : Set where
    Clear        : LogicOp
    And          : LogicOp
    AndReverse   : LogicOp
    Copy         : LogicOp
    AndInverted  : LogicOp
    Noop         : LogicOp
    Xor          : LogicOp
    Or           : LogicOp
    Nor          : LogicOp
    Equiv        : LogicOp
    Invert       : LogicOp
    OrReverse    : LogicOp
    CopyInverted : LogicOp
    OrInverted   : LogicOp
    Nand         : LogicOp
    SetOp        : LogicOp

{-# COMPILE GHC LogicOp = data Graphics.Rendering.OpenGL.GL.PerFragment.LogicOp
    ( Graphics.Rendering.OpenGL.GL.PerFragment.Clear
    | Graphics.Rendering.OpenGL.GL.PerFragment.And
    | Graphics.Rendering.OpenGL.GL.PerFragment.AndReverse
    | Graphics.Rendering.OpenGL.GL.PerFragment.Copy
    | Graphics.Rendering.OpenGL.GL.PerFragment.AndInverted
    | Graphics.Rendering.OpenGL.GL.PerFragment.Noop
    | Graphics.Rendering.OpenGL.GL.PerFragment.Xor
    | Graphics.Rendering.OpenGL.GL.PerFragment.Or
    | Graphics.Rendering.OpenGL.GL.PerFragment.Nor
    | Graphics.Rendering.OpenGL.GL.PerFragment.Equiv
    | Graphics.Rendering.OpenGL.GL.PerFragment.Invert
    | Graphics.Rendering.OpenGL.GL.PerFragment.OrReverse
    | Graphics.Rendering.OpenGL.GL.PerFragment.CopyInverted
    | Graphics.Rendering.OpenGL.GL.PerFragment.OrInverted
    | Graphics.Rendering.OpenGL.GL.PerFragment.Nand
    | Graphics.Rendering.OpenGL.GL.PerFragment.Set
    ) #-}

postulate
    Eq[LogicOp]   : Eq LogicOp
    Ord[LogicOp]  : Ord LogicOp
    Show[LogicOp] : Show LogicOp

{-# COMPILE GHC Eq[LogicOp]   = AgdaEq   #-}
{-# COMPILE GHC Ord[LogicOp]  = AgdaOrd  #-}
{-# COMPILE GHC Show[LogicOp] = AgdaShow #-}


postulate
    discardingRasterizer  : IO A → IO A

    rasterizerDiscard     : StateVar Capability
    scissor               : StateVar (Maybe (Tuple2 Position Size))
    sampleAlphaToCoverage : StateVar Capability
    sampleAlphaToOne      : StateVar Capability
    sampleCoverage        : StateVar (Maybe (Tuple2 GLclampf Bool))
    depthBounds           : StateVar (Maybe (Tuple2 GLclampd GLclampd))
    alphaFunc             : StateVar (Maybe (Tuple2 ComparisonFunction GLclampf))
    stencilTest           : StateVar Capability
    stencilFunc           : StateVar (Tuple3 ComparisonFunction GLint GLuint)
    stencilFuncSeparate   : Face → StateVar (Tuple3 ComparisonFunction GLint GLuint)
    stencilOp             : StateVar (Tuple3 StencilOp StencilOp StencilOp)
    stencilOpSeparate     : Face → StateVar (Tuple3 StencilOp StencilOp StencilOp)
    activeStencilFace     : StateVar (Maybe Face)
    depthFunc             : StateVar (Maybe ComparisonFunction)
    blend                 : StateVar Capability
    blendBuffer           : DrawBufferIndex → StateVar Capability
    blendEquation         : StateVar BlendEquation
    blendEquationSeparate : StateVar (Tuple2 BlendEquation BlendEquation)
    blendFuncSeparate     : StateVar (Tuple2 (Tuple2 BlendingFactor BlendingFactor) (Tuple2 BlendingFactor BlendingFactor))
    blendFunc             : StateVar (Tuple2 BlendingFactor BlendingFactor)
    blendColor            : StateVar (Color4 GLclampf)
    dither                : StateVar Capability
    logicOp               : StateVar (Maybe LogicOp)

{-# COMPILE GHC discardingRasterizer  = \ aℓ a -> Graphics.Rendering.OpenGL.GL.PerFragment.discardingRasterizer #-}

{-# COMPILE GHC rasterizerDiscard     = Graphics.Rendering.OpenGL.GL.PerFragment.rasterizerDiscard     #-}
{-# COMPILE GHC scissor               = Graphics.Rendering.OpenGL.GL.PerFragment.scissor               #-}
{-# COMPILE GHC sampleAlphaToCoverage = Graphics.Rendering.OpenGL.GL.PerFragment.sampleAlphaToCoverage #-}
{-# COMPILE GHC sampleAlphaToOne      = Graphics.Rendering.OpenGL.GL.PerFragment.sampleAlphaToOne      #-}
{-# COMPILE GHC sampleCoverage        = Graphics.Rendering.OpenGL.GL.PerFragment.sampleCoverage        #-}
{-# COMPILE GHC depthBounds           = Graphics.Rendering.OpenGL.GL.PerFragment.depthBounds           #-}
{-# COMPILE GHC alphaFunc             = Graphics.Rendering.OpenGL.GL.PerFragment.alphaFunc             #-}
{-# COMPILE GHC stencilTest           = Graphics.Rendering.OpenGL.GL.PerFragment.stencilTest           #-}
{-# COMPILE GHC stencilFunc           = Graphics.Rendering.OpenGL.GL.PerFragment.stencilFunc           #-}
{-# COMPILE GHC stencilFuncSeparate   = Graphics.Rendering.OpenGL.GL.PerFragment.stencilFuncSeparate   #-}
{-# COMPILE GHC stencilOp             = Graphics.Rendering.OpenGL.GL.PerFragment.stencilOp             #-}
{-# COMPILE GHC stencilOpSeparate     = Graphics.Rendering.OpenGL.GL.PerFragment.stencilOpSeparate     #-}
{-# COMPILE GHC activeStencilFace     = Graphics.Rendering.OpenGL.GL.PerFragment.activeStencilFace     #-}
{-# COMPILE GHC depthFunc             = Graphics.Rendering.OpenGL.GL.PerFragment.depthFunc             #-}
{-# COMPILE GHC blend                 = Graphics.Rendering.OpenGL.GL.PerFragment.blend                 #-}
{-# COMPILE GHC blendBuffer           = Graphics.Rendering.OpenGL.GL.PerFragment.blendBuffer           #-}
{-# COMPILE GHC blendEquation         = Graphics.Rendering.OpenGL.GL.PerFragment.blendEquation         #-}
{-# COMPILE GHC blendEquationSeparate = Graphics.Rendering.OpenGL.GL.PerFragment.blendEquationSeparate #-}
{-# COMPILE GHC blendFuncSeparate     = Graphics.Rendering.OpenGL.GL.PerFragment.blendFuncSeparate     #-}
{-# COMPILE GHC blendFunc             = Graphics.Rendering.OpenGL.GL.PerFragment.blendFunc             #-}
{-# COMPILE GHC blendColor            = Graphics.Rendering.OpenGL.GL.PerFragment.blendColor            #-}
{-# COMPILE GHC dither                = Graphics.Rendering.OpenGL.GL.PerFragment.dither                #-}
{-# COMPILE GHC logicOp               = Graphics.Rendering.OpenGL.GL.PerFragment.logicOp               #-}
