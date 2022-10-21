{-# OPTIONS --without-K #-}

module Ffi.Hs.Graphics.Rendering.OpenGL.GL.VertexArrays where

open import Agda.Builtin.Maybe       using (Maybe)
open import Agda.Primitive
open import Ffi.Hs.-base.Class       using (Eq; Ord; Show)
open import Ffi.Hs.Data.Tuple        using (Tuple2)
open import Ffi.Hs.Foreign.Ptr       using (Ptr)
open import Ffi.Hs.Graphics.GL.Types using (GLint; GLsizei; GLuint)
open import Ffi.Hs.Data.StateVar     using (StateVar; GettableStateVar)
open import Agda.Builtin.IO          using (IO)
open import Agda.Builtin.Unit        using (⊤)

open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.PrimitiveMode using (PrimitiveMode)
open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.VertexSpec    using (TextureUnit; AttribLocation; IntegerHandling)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Graphics.Rendering.OpenGL.GL.VertexArrays
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ : Level
        A : Set aℓ


NumComponents : Set
NumComponents = GLint

Stride : Set
Stride = GLsizei

data DataType : Set where
    UnsignedByte             : DataType
    Byte                     : DataType
    UnsignedShort            : DataType
    Short                    : DataType
    UnsignedInt              : DataType
    Int                      : DataType
    HalfFloat                : DataType
    Float                    : DataType
    UnsignedByte332          : DataType
    UnsignedByte233Rev       : DataType
    UnsignedShort565         : DataType
    UnsignedShort565Rev      : DataType
    UnsignedShort4444        : DataType
    UnsignedShort4444Rev     : DataType
    UnsignedShort5551        : DataType
    UnsignedShort1555Rev     : DataType
    UnsignedInt8888          : DataType
    UnsignedInt8888Rev       : DataType
    UnsignedInt1010102       : DataType
    UnsignedInt2101010Rev    : DataType
    UnsignedInt248           : DataType
    UnsignedInt10f11f11fRev  : DataType
    UnsignedInt5999Rev       : DataType
    Float32UnsignedInt248Rev : DataType
    Bitmap                   : DataType
    UnsignedShort88          : DataType
    UnsignedShort88Rev       : DataType
    Double                   : DataType
    TwoBytes                 : DataType
    ThreeBytes               : DataType
    FourBytes                : DataType

{-# COMPILE GHC DataType = data Graphics.Rendering.OpenGL.GL.VertexArrays.DataType
    ( Graphics.Rendering.OpenGL.GL.VertexArrays.UnsignedByte
    | Graphics.Rendering.OpenGL.GL.VertexArrays.Byte
    | Graphics.Rendering.OpenGL.GL.VertexArrays.UnsignedShort
    | Graphics.Rendering.OpenGL.GL.VertexArrays.Short
    | Graphics.Rendering.OpenGL.GL.VertexArrays.UnsignedInt
    | Graphics.Rendering.OpenGL.GL.VertexArrays.Int
    | Graphics.Rendering.OpenGL.GL.VertexArrays.HalfFloat
    | Graphics.Rendering.OpenGL.GL.VertexArrays.Float
    | Graphics.Rendering.OpenGL.GL.VertexArrays.UnsignedByte332
    | Graphics.Rendering.OpenGL.GL.VertexArrays.UnsignedByte233Rev
    | Graphics.Rendering.OpenGL.GL.VertexArrays.UnsignedShort565
    | Graphics.Rendering.OpenGL.GL.VertexArrays.UnsignedShort565Rev
    | Graphics.Rendering.OpenGL.GL.VertexArrays.UnsignedShort4444
    | Graphics.Rendering.OpenGL.GL.VertexArrays.UnsignedShort4444Rev
    | Graphics.Rendering.OpenGL.GL.VertexArrays.UnsignedShort5551
    | Graphics.Rendering.OpenGL.GL.VertexArrays.UnsignedShort1555Rev
    | Graphics.Rendering.OpenGL.GL.VertexArrays.UnsignedInt8888
    | Graphics.Rendering.OpenGL.GL.VertexArrays.UnsignedInt8888Rev
    | Graphics.Rendering.OpenGL.GL.VertexArrays.UnsignedInt1010102
    | Graphics.Rendering.OpenGL.GL.VertexArrays.UnsignedInt2101010Rev
    | Graphics.Rendering.OpenGL.GL.VertexArrays.UnsignedInt248
    | Graphics.Rendering.OpenGL.GL.VertexArrays.UnsignedInt10f11f11fRev
    | Graphics.Rendering.OpenGL.GL.VertexArrays.UnsignedInt5999Rev
    | Graphics.Rendering.OpenGL.GL.VertexArrays.Float32UnsignedInt248Rev
    | Graphics.Rendering.OpenGL.GL.VertexArrays.Bitmap
    | Graphics.Rendering.OpenGL.GL.VertexArrays.UnsignedShort88
    | Graphics.Rendering.OpenGL.GL.VertexArrays.UnsignedShort88Rev
    | Graphics.Rendering.OpenGL.GL.VertexArrays.Double
    | Graphics.Rendering.OpenGL.GL.VertexArrays.TwoBytes
    | Graphics.Rendering.OpenGL.GL.VertexArrays.ThreeBytes
    | Graphics.Rendering.OpenGL.GL.VertexArrays.FourBytes
    ) #-}

postulate
    Eq[DataType]   : Eq DataType
    Ord[DataType]  : Ord DataType
    Show[DataType] : Show DataType

{-# COMPILE GHC Eq[DataType]   = AgdaEq   #-}
{-# COMPILE GHC Ord[DataType]  = AgdaOrd  #-}
{-# COMPILE GHC Show[DataType] = AgdaShow #-}


data VertexArrayDescriptor (A : Set aℓ) : Set aℓ where
    mkVertexArrayDescriptor : NumComponents → DataType → Stride → Ptr A → VertexArrayDescriptor A

{-# FOREIGN GHC type AgdaVertexArrayDescriptor aℓ = Graphics.Rendering.OpenGL.GL.VertexArrays.VertexArrayDescriptor #-}
{-# COMPILE GHC VertexArrayDescriptor = data(1) AgdaVertexArrayDescriptor (Graphics.Rendering.OpenGL.GL.VertexArrays.VertexArrayDescriptor) #-}

postulate
    Eq[VertexArrayDescriptor[A]]   : Eq (VertexArrayDescriptor A)
    Ord[VertexArrayDescriptor[A]]  : Ord (VertexArrayDescriptor A)
    Show[VertexArrayDescriptor[A]] : Show (VertexArrayDescriptor A)

{-# COMPILE GHC Eq[VertexArrayDescriptor[A]]   = \ aℓ a -> AgdaEq   #-}
{-# COMPILE GHC Ord[VertexArrayDescriptor[A]]  = \ aℓ a -> AgdaOrd  #-}
{-# COMPILE GHC Show[VertexArrayDescriptor[A]] = \ aℓ a -> AgdaShow #-}


data Capability : Set where
    Disabled : Capability
    Enabled  : Capability

{-# COMPILE GHC Capability = data Graphics.Rendering.OpenGL.GL.VertexArrays.Capability
    ( Graphics.Rendering.OpenGL.GL.VertexArrays.Disabled
    | Graphics.Rendering.OpenGL.GL.VertexArrays.Enabled
    ) #-}

postulate
    Eq[Capability]   : Eq Capability
    Ord[Capability]  : Ord Capability
    Show[Capability] : Show Capability

{-# COMPILE GHC Eq[Capability]   = AgdaEq   #-}
{-# COMPILE GHC Ord[Capability]  = AgdaOrd  #-}
{-# COMPILE GHC Show[Capability] = AgdaShow #-}


data ClientArrayType : Set where
    VertexArray         : ClientArrayType
    NormalArray         : ClientArrayType
    ColorArray          : ClientArrayType
    IndexArray          : ClientArrayType
    TextureCoordArray   : ClientArrayType
    EdgeFlagArray       : ClientArrayType
    FogCoordArray       : ClientArrayType
    SecondaryColorArray : ClientArrayType
    MatrixIndexArray    : ClientArrayType

{-# COMPILE GHC ClientArrayType = data Graphics.Rendering.OpenGL.GL.VertexArrays.ClientArrayType
    ( Graphics.Rendering.OpenGL.GL.VertexArrays.VertexArray
    | Graphics.Rendering.OpenGL.GL.VertexArrays.NormalArray
    | Graphics.Rendering.OpenGL.GL.VertexArrays.ColorArray
    | Graphics.Rendering.OpenGL.GL.VertexArrays.IndexArray
    | Graphics.Rendering.OpenGL.GL.VertexArrays.TextureCoordArray
    | Graphics.Rendering.OpenGL.GL.VertexArrays.EdgeFlagArray
    | Graphics.Rendering.OpenGL.GL.VertexArrays.FogCoordArray
    | Graphics.Rendering.OpenGL.GL.VertexArrays.SecondaryColorArray
    | Graphics.Rendering.OpenGL.GL.VertexArrays.MatrixIndexArray
    ) #-}

postulate
    Eq[ClientArrayType]   : Eq ClientArrayType
    Ord[ClientArrayType]  : Ord ClientArrayType
    Show[ClientArrayType] : Show ClientArrayType

{-# COMPILE GHC Eq[ClientArrayType]   = AgdaEq   #-}
{-# COMPILE GHC Ord[ClientArrayType]  = AgdaOrd  #-}
{-# COMPILE GHC Show[ClientArrayType] = AgdaShow #-}


postulate
    arrayPointer : ClientArrayType → StateVar (VertexArrayDescriptor A)

{-# COMPILE GHC arrayPointer = \ aℓ a -> Graphics.Rendering.OpenGL.GL.VertexArrays.arrayPointer #-}


data InterleavedArrays : Set where
    V2f          : InterleavedArrays
    V3f          : InterleavedArrays
    C4ubV2f      : InterleavedArrays
    C4ubV3f      : InterleavedArrays
    C3fV3f       : InterleavedArrays
    N3fV3f       : InterleavedArrays
    C4fN3fV3f    : InterleavedArrays
    T2fV3f       : InterleavedArrays
    T4fV4f       : InterleavedArrays
    T2fC4ubV3f   : InterleavedArrays
    T2fC3fV3f    : InterleavedArrays
    T2fN3fV3f    : InterleavedArrays
    T2fC4fN3fV3f : InterleavedArrays
    T4fC4fN3fV4f : InterleavedArrays

{-# COMPILE GHC InterleavedArrays = data Graphics.Rendering.OpenGL.GL.VertexArrays.InterleavedArrays
    ( Graphics.Rendering.OpenGL.GL.VertexArrays.V2f
    | Graphics.Rendering.OpenGL.GL.VertexArrays.V3f
    | Graphics.Rendering.OpenGL.GL.VertexArrays.C4ubV2f
    | Graphics.Rendering.OpenGL.GL.VertexArrays.C4ubV3f
    | Graphics.Rendering.OpenGL.GL.VertexArrays.C3fV3f
    | Graphics.Rendering.OpenGL.GL.VertexArrays.N3fV3f
    | Graphics.Rendering.OpenGL.GL.VertexArrays.C4fN3fV3f
    | Graphics.Rendering.OpenGL.GL.VertexArrays.T2fV3f
    | Graphics.Rendering.OpenGL.GL.VertexArrays.T4fV4f
    | Graphics.Rendering.OpenGL.GL.VertexArrays.T2fC4ubV3f
    | Graphics.Rendering.OpenGL.GL.VertexArrays.T2fC3fV3f
    | Graphics.Rendering.OpenGL.GL.VertexArrays.T2fN3fV3f
    | Graphics.Rendering.OpenGL.GL.VertexArrays.T2fC4fN3fV3f
    | Graphics.Rendering.OpenGL.GL.VertexArrays.T4fC4fN3fV4f
    ) #-}

postulate
    Eq[InterleavedArrays]   : Eq InterleavedArrays
    Ord[InterleavedArrays]  : Ord InterleavedArrays
    Show[InterleavedArrays] : Show InterleavedArrays

{-# COMPILE GHC Eq[InterleavedArrays]   = AgdaEq   #-}
{-# COMPILE GHC Ord[InterleavedArrays]  = AgdaOrd  #-}
{-# COMPILE GHC Show[InterleavedArrays] = AgdaShow #-}


postulate
    interleavedArrays : InterleavedArrays → Stride → Ptr A → IO ⊤

{-# COMPILE GHC interleavedArrays = \ aℓ a -> Graphics.Rendering.OpenGL.GL.VertexArrays.interleavedArrays #-}


ArrayIndex : Set
ArrayIndex = GLint

NumArrayIndices : Set
NumArrayIndices = GLsizei

NumIndexBlocks : Set
NumIndexBlocks = GLsizei

NumInstances : Set
NumInstances = GLsizei

BaseInstance : Set
BaseInstance = GLuint

BaseVertex : Set
BaseVertex = GLint


postulate
    clientState         : ClientArrayType → StateVar Capability
    clientActiveTexture : StateVar TextureUnit

    arrayElement                                : ArrayIndex → IO ⊤
    drawArrays                                  : PrimitiveMode → ArrayIndex → NumArrayIndices → IO ⊤
    drawArraysInstancedBaseInstance             : PrimitiveMode → ArrayIndex → NumArrayIndices → NumInstances → BaseInstance → IO ⊤
    drawArraysInstanced                         : PrimitiveMode → ArrayIndex → NumArrayIndices → NumInstances → IO ⊤
    multiDrawArrays                             : PrimitiveMode → Ptr ArrayIndex → Ptr NumArrayIndices → NumIndexBlocks → IO ⊤
    drawElements                                : PrimitiveMode → NumArrayIndices → DataType → Ptr A → IO ⊤
    drawElementsInstancedBaseInstance           : PrimitiveMode → NumArrayIndices → DataType → Ptr A → NumInstances → BaseInstance → IO ⊤
    drawElementsInstanced                       : PrimitiveMode → NumArrayIndices → DataType → Ptr A → NumInstances → IO ⊤
    multiDrawElements                           : PrimitiveMode → Ptr NumArrayIndices → DataType → Ptr (Ptr A) → NumIndexBlocks → IO ⊤
    drawRangeElements                           : PrimitiveMode → Tuple2 ArrayIndex ArrayIndex → NumArrayIndices → DataType → Ptr A → IO ⊤
    drawElementsBaseVertex                      : PrimitiveMode → NumArrayIndices → DataType → Ptr A → BaseVertex → IO ⊤
    drawRangeElementsBaseVertex                 : PrimitiveMode → Tuple2 ArrayIndex ArrayIndex → NumArrayIndices → DataType → Ptr A → BaseVertex → IO ⊤
    drawElementsInstancedBaseVertex             : PrimitiveMode → NumArrayIndices → DataType → Ptr A → NumInstances → BaseVertex → IO ⊤
    drawElementsInstancedBaseVertexBaseInstance : PrimitiveMode → NumArrayIndices → DataType → Ptr A → NumInstances → BaseVertex → BaseInstance → IO ⊤
    multiDrawElementsBaseVertex                 : PrimitiveMode → Ptr NumArrayIndices → DataType → Ptr (Ptr A) → NumIndexBlocks → Ptr BaseVertex → IO ⊤
    maxElementsVertices                         : GettableStateVar NumArrayIndices
    maxElementsIndices                          : GettableStateVar NumArrayIndices
    lockArrays                                  : StateVar (Maybe (Tuple2 ArrayIndex NumArrayIndices))
    primitiveRestartIndex                       : StateVar (Maybe ArrayIndex)
    primitiveRestartIndexNV                     : StateVar (Maybe ArrayIndex)

    vertexAttribPointer : AttribLocation → StateVar (Tuple2 IntegerHandling (VertexArrayDescriptor A))
    vertexAttribArray   : AttribLocation → StateVar Capability

{-# COMPILE GHC clientState         = Graphics.Rendering.OpenGL.GL.VertexArrays.clientState         #-}
{-# COMPILE GHC clientActiveTexture = Graphics.Rendering.OpenGL.GL.VertexArrays.clientActiveTexture #-}

{-# COMPILE GHC arrayElement                                =           Graphics.Rendering.OpenGL.GL.VertexArrays.arrayElement                                #-}
{-# COMPILE GHC drawArrays                                  =           Graphics.Rendering.OpenGL.GL.VertexArrays.drawArrays                                  #-}
{-# COMPILE GHC drawArraysInstancedBaseInstance             =           Graphics.Rendering.OpenGL.GL.VertexArrays.drawArraysInstancedBaseInstance             #-}
{-# COMPILE GHC drawArraysInstanced                         =           Graphics.Rendering.OpenGL.GL.VertexArrays.drawArraysInstanced                         #-}
{-# COMPILE GHC multiDrawArrays                             =           Graphics.Rendering.OpenGL.GL.VertexArrays.multiDrawArrays                             #-}
{-# COMPILE GHC drawElements                                = \ aℓ a -> Graphics.Rendering.OpenGL.GL.VertexArrays.drawElements                                #-}
{-# COMPILE GHC drawElementsInstancedBaseInstance           = \ aℓ a -> Graphics.Rendering.OpenGL.GL.VertexArrays.drawElementsInstancedBaseInstance           #-}
{-# COMPILE GHC drawElementsInstanced                       = \ aℓ a -> Graphics.Rendering.OpenGL.GL.VertexArrays.drawElementsInstanced                       #-}
{-# COMPILE GHC multiDrawElements                           = \ aℓ a -> Graphics.Rendering.OpenGL.GL.VertexArrays.multiDrawElements                           #-}
{-# COMPILE GHC drawRangeElements                           = \ aℓ a -> Graphics.Rendering.OpenGL.GL.VertexArrays.drawRangeElements                           #-}
{-# COMPILE GHC drawElementsBaseVertex                      = \ aℓ a -> Graphics.Rendering.OpenGL.GL.VertexArrays.drawElementsBaseVertex                      #-}
{-# COMPILE GHC drawRangeElementsBaseVertex                 = \ aℓ a -> Graphics.Rendering.OpenGL.GL.VertexArrays.drawRangeElementsBaseVertex                 #-}
{-# COMPILE GHC drawElementsInstancedBaseVertex             = \ aℓ a -> Graphics.Rendering.OpenGL.GL.VertexArrays.drawElementsInstancedBaseVertex             #-}
{-# COMPILE GHC drawElementsInstancedBaseVertexBaseInstance = \ aℓ a -> Graphics.Rendering.OpenGL.GL.VertexArrays.drawElementsInstancedBaseVertexBaseInstance #-}
{-# COMPILE GHC multiDrawElementsBaseVertex                 = \ aℓ a -> Graphics.Rendering.OpenGL.GL.VertexArrays.multiDrawElementsBaseVertex                 #-}
{-# COMPILE GHC maxElementsVertices                         =           Graphics.Rendering.OpenGL.GL.VertexArrays.maxElementsVertices                         #-}
{-# COMPILE GHC maxElementsIndices                          =           Graphics.Rendering.OpenGL.GL.VertexArrays.maxElementsIndices                          #-}
{-# COMPILE GHC lockArrays                                  =           Graphics.Rendering.OpenGL.GL.VertexArrays.lockArrays                                  #-}
{-# COMPILE GHC primitiveRestartIndex                       =           Graphics.Rendering.OpenGL.GL.VertexArrays.primitiveRestartIndex                       #-}
{-# COMPILE GHC primitiveRestartIndexNV                     =           Graphics.Rendering.OpenGL.GL.VertexArrays.primitiveRestartIndexNV                     #-}

{-# COMPILE GHC vertexAttribPointer = \ aℓ a -> Graphics.Rendering.OpenGL.GL.VertexArrays.vertexAttribPointer #-}
{-# COMPILE GHC vertexAttribArray   =           Graphics.Rendering.OpenGL.GL.VertexArrays.vertexAttribArray   #-}
