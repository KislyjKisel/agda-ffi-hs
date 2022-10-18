{-# OPTIONS --without-K #-}

module Ffi.Hs.Graphics.Rendering.OpenGL.GL.BufferObjects where

open import Agda.Builtin.Bool        using (Bool)
open import Agda.Builtin.IO          using (IO)
open import Agda.Builtin.List        using (List)
open import Agda.Builtin.Maybe       using (Maybe)
open import Agda.Builtin.Unit        using (⊤)
open import Agda.Primitive           using (Level)
open import Ffi.Hs.-base.Class       using (Eq; Ord; Show)
open import Ffi.Hs.Data.ObjectName   using (ObjectName; GeneratableObjectName)
open import Ffi.Hs.Data.StateVar     using (GettableStateVar; StateVar)
open import Ffi.Hs.Data.Tuple        using (Tuple3)
open import Ffi.Hs.Foreign.Ptr       using (Ptr)
open import Ffi.Hs.Graphics.GL.Types using (GLsizeiptr; GLintptr; GLuint)

open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.DebugOutput  using (CanBeLabeled)
open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.VertexArrays using (ClientArrayType)
open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.VertexSpec   using (AttribLocation)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Graphics.Rendering.OpenGL.GL.BufferObjects
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
import MAlonzo.Code.Ffi.Hs.Graphics.Rendering.OpenGL.GL.DebugOutput (AgdaCanBeLabeled(AgdaCanBeLabeled))
import MAlonzo.Code.Ffi.Hs.Data.ObjectName
    (AgdaObjectName(AgdaObjectName), AgdaGeneratableObjectName(AgdaGeneratableObjectName))
#-}

private
    variable
        aℓ : Level
        A B : Set aℓ


postulate
    BufferObject : Set

    Eq[BufferObject]                    : Eq BufferObject
    Ord[BufferObject]                   : Ord BufferObject
    Show[BufferObject]                  : Show BufferObject
    ObjectName[BufferObject]            : ObjectName BufferObject
    GeneratableObjectName[BufferObject] : GeneratableObjectName BufferObject
    CanBeLabeled[BufferObject]          : CanBeLabeled BufferObject

{-# COMPILE GHC BufferObject = type Graphics.Rendering.OpenGL.GL.BufferObjects.BufferObject #-}

{-# COMPILE GHC Eq[BufferObject]                    = AgdaEq                    #-}
{-# COMPILE GHC Ord[BufferObject]                   = AgdaOrd                   #-}
{-# COMPILE GHC Show[BufferObject]                  = AgdaShow                  #-}
{-# COMPILE GHC ObjectName[BufferObject]            = AgdaObjectName            #-}
{-# COMPILE GHC GeneratableObjectName[BufferObject] = AgdaGeneratableObjectName #-}
{-# COMPILE GHC CanBeLabeled[BufferObject]          = AgdaCanBeLabeled          #-}


data BufferTarget : Set where
    ArrayBuffer             : BufferTarget
    AtomicCounterBuffer     : BufferTarget
    CopyReadBuffer          : BufferTarget
    CopyWriteBuffer         : BufferTarget
    DispatchIndirectBuffer  : BufferTarget
    DrawIndirectBuffer      : BufferTarget
    ElementArrayBuffer      : BufferTarget
    PixelPackBuffer         : BufferTarget
    PixelUnpackBuffer       : BufferTarget
    QueryBuffer             : BufferTarget
    ShaderStorageBuffer     : BufferTarget
    TextureBuffer           : BufferTarget
    TransformFeedbackBuffer : BufferTarget
    UniformBuffer           : BufferTarget

{-# COMPILE GHC BufferTarget = data Graphics.Rendering.OpenGL.GL.BufferObjects.BufferTarget
    ( Graphics.Rendering.OpenGL.GL.BufferObjects.ArrayBuffer
    | Graphics.Rendering.OpenGL.GL.BufferObjects.AtomicCounterBuffer
    | Graphics.Rendering.OpenGL.GL.BufferObjects.CopyReadBuffer
    | Graphics.Rendering.OpenGL.GL.BufferObjects.CopyWriteBuffer
    | Graphics.Rendering.OpenGL.GL.BufferObjects.DispatchIndirectBuffer
    | Graphics.Rendering.OpenGL.GL.BufferObjects.DrawIndirectBuffer
    | Graphics.Rendering.OpenGL.GL.BufferObjects.ElementArrayBuffer
    | Graphics.Rendering.OpenGL.GL.BufferObjects.PixelPackBuffer
    | Graphics.Rendering.OpenGL.GL.BufferObjects.PixelUnpackBuffer
    | Graphics.Rendering.OpenGL.GL.BufferObjects.QueryBuffer
    | Graphics.Rendering.OpenGL.GL.BufferObjects.ShaderStorageBuffer
    | Graphics.Rendering.OpenGL.GL.BufferObjects.TextureBuffer
    | Graphics.Rendering.OpenGL.GL.BufferObjects.TransformFeedbackBuffer
    | Graphics.Rendering.OpenGL.GL.BufferObjects.UniformBuffer
    ) #-}

postulate
    Eq[BufferTarget]   : Eq BufferTarget
    Ord[BufferTarget]  : Ord BufferTarget
    Show[BufferTarget] : Show BufferTarget

{-# COMPILE GHC Eq[BufferTarget]   = AgdaEq   #-}
{-# COMPILE GHC Ord[BufferTarget]  = AgdaOrd  #-}
{-# COMPILE GHC Show[BufferTarget] = AgdaShow #-}

postulate
    bindBuffer                     : BufferTarget → StateVar (Maybe BufferObject)
    arrayBufferBinding             : ClientArrayType → GettableStateVar (Maybe BufferObject)
    vertexAttribArrayBufferBinding : AttribLocation → GettableStateVar (Maybe BufferObject)

{-# COMPILE GHC bindBuffer                     = Graphics.Rendering.OpenGL.GL.BufferObjects.bindBuffer                     #-}
{-# COMPILE GHC arrayBufferBinding             = Graphics.Rendering.OpenGL.GL.BufferObjects.arrayBufferBinding             #-}
{-# COMPILE GHC vertexAttribArrayBufferBinding = Graphics.Rendering.OpenGL.GL.BufferObjects.vertexAttribArrayBufferBinding #-}


data BufferUsage : Set where
    StreamDraw  : BufferUsage
    StreamRead  : BufferUsage
    StreamCopy  : BufferUsage
    StaticDraw  : BufferUsage
    StaticRead  : BufferUsage
    StaticCopy  : BufferUsage
    DynamicDraw : BufferUsage
    DynamicRead : BufferUsage
    DynamicCopy : BufferUsage

{-# COMPILE GHC BufferUsage = data Graphics.Rendering.OpenGL.GL.BufferObjects.BufferUsage
    ( Graphics.Rendering.OpenGL.GL.BufferObjects.StreamDraw
    | Graphics.Rendering.OpenGL.GL.BufferObjects.StreamRead
    | Graphics.Rendering.OpenGL.GL.BufferObjects.StreamCopy
    | Graphics.Rendering.OpenGL.GL.BufferObjects.StaticDraw
    | Graphics.Rendering.OpenGL.GL.BufferObjects.StaticRead
    | Graphics.Rendering.OpenGL.GL.BufferObjects.StaticCopy
    | Graphics.Rendering.OpenGL.GL.BufferObjects.DynamicDraw
    | Graphics.Rendering.OpenGL.GL.BufferObjects.DynamicRead
    | Graphics.Rendering.OpenGL.GL.BufferObjects.DynamicCopy
    ) #-}

postulate
    Eq[BufferUsage]   : Eq BufferUsage
    Ord[BufferUsage]  : Ord BufferUsage
    Show[BufferUsage] : Show BufferUsage

{-# COMPILE GHC Eq[BufferUsage]   = AgdaEq   #-}
{-# COMPILE GHC Ord[BufferUsage]  = AgdaOrd  #-}
{-# COMPILE GHC Show[BufferUsage] = AgdaShow #-}

postulate
    bufferData : BufferTarget → StateVar (Tuple3 GLsizeiptr (Ptr A) BufferUsage)

{-# COMPILE GHC bufferData = \ aℓ a -> Graphics.Rendering.OpenGL.GL.BufferObjects.bufferData #-}


data TransferDirection : Set where
    ReadFromBuffer : TransferDirection
    WriteToBuffer  : TransferDirection

{-# COMPILE GHC TransferDirection = data Graphics.Rendering.OpenGL.GL.BufferObjects.TransferDirection
    ( Graphics.Rendering.OpenGL.GL.BufferObjects.ReadFromBuffer
    | Graphics.Rendering.OpenGL.GL.BufferObjects.WriteToBuffer
    ) #-}

postulate
    Eq[TransferDirection]   : Eq TransferDirection
    Ord[TransferDirection]  : Ord TransferDirection
    Show[TransferDirection] : Show TransferDirection

{-# COMPILE GHC Eq[TransferDirection]   = AgdaEq   #-}
{-# COMPILE GHC Ord[TransferDirection]  = AgdaOrd  #-}
{-# COMPILE GHC Show[TransferDirection] = AgdaShow #-}

postulate
    bufferSubData : BufferTarget → TransferDirection → GLintptr → GLsizeiptr → Ptr A → IO ⊤

{-# COMPILE GHC bufferSubData = \ aℓ a -> Graphics.Rendering.OpenGL.GL.BufferObjects.bufferSubData #-}


data BufferAccess : Set where
    ReadOnly  : BufferAccess
    WriteOnly : BufferAccess
    ReadWrite : BufferAccess

{-# COMPILE GHC BufferAccess = data Graphics.Rendering.OpenGL.GL.BufferObjects.BufferAccess
    ( Graphics.Rendering.OpenGL.GL.BufferObjects.ReadOnly
    | Graphics.Rendering.OpenGL.GL.BufferObjects.WriteOnly
    | Graphics.Rendering.OpenGL.GL.BufferObjects.ReadWrite
    ) #-}

postulate
    Eq[BufferAccess]   : Eq BufferAccess
    Ord[BufferAccess]  : Ord BufferAccess
    Show[BufferAccess] : Show BufferAccess

{-# COMPILE GHC Eq[BufferAccess]   = AgdaEq   #-}
{-# COMPILE GHC Ord[BufferAccess]  = AgdaOrd  #-}
{-# COMPILE GHC Show[BufferAccess] = AgdaShow #-}


data MappingFailure : Set where
    MappingFailed   : MappingFailure
    UnmappingFailed : MappingFailure

{-# COMPILE GHC MappingFailure = data Graphics.Rendering.OpenGL.GL.BufferObjects.MappingFailure
    ( Graphics.Rendering.OpenGL.GL.BufferObjects.MappingFailed
    | Graphics.Rendering.OpenGL.GL.BufferObjects.UnmappingFailed
    ) #-}

postulate
    Eq[MappingFailure]   : Eq MappingFailure
    Ord[MappingFailure]  : Ord MappingFailure
    Show[MappingFailure] : Show MappingFailure

{-# COMPILE GHC Eq[MappingFailure]   = AgdaEq   #-}
{-# COMPILE GHC Ord[MappingFailure]  = AgdaOrd  #-}
{-# COMPILE GHC Show[MappingFailure] = AgdaShow #-}


data MapBufferUsage : Set where
    Read             : MapBufferUsage
    Write            : MapBufferUsage
    InvalidateRange  : MapBufferUsage
    InvalidateBuffer : MapBufferUsage
    FlushExplicit    : MapBufferUsage
    Unsychronized    : MapBufferUsage

{-# COMPILE GHC MapBufferUsage = data Graphics.Rendering.OpenGL.GL.BufferObjects.MapBufferUsage
    ( Graphics.Rendering.OpenGL.GL.BufferObjects.Read
    | Graphics.Rendering.OpenGL.GL.BufferObjects.Write
    | Graphics.Rendering.OpenGL.GL.BufferObjects.InvalidateRange
    | Graphics.Rendering.OpenGL.GL.BufferObjects.InvalidateBuffer
    | Graphics.Rendering.OpenGL.GL.BufferObjects.FlushExplicit
    | Graphics.Rendering.OpenGL.GL.BufferObjects.Unsychronized
    ) #-}

postulate
    Eq[MapBufferUsage]   : Eq MapBufferUsage
    Ord[MapBufferUsage]  : Ord MapBufferUsage
    Show[MapBufferUsage] : Show MapBufferUsage

{-# COMPILE GHC Eq[MapBufferUsage]   = AgdaEq   #-}
{-# COMPILE GHC Ord[MapBufferUsage]  = AgdaOrd  #-}
{-# COMPILE GHC Show[MapBufferUsage] = AgdaShow #-}


Offset : Set
Offset = GLintptr

Length : Set
Length = GLsizeiptr


postulate
    withMappedBuffer       : BufferTarget → BufferAccess → (Ptr A → IO B) → (MappingFailure → IO B) → IO B
    mapBuffer              : BufferTarget → BufferAccess → IO (Maybe (Ptr A))
    unmapBuffer            : BufferTarget → IO Bool
    bufferAccess           : BufferTarget → GettableStateVar BufferAccess
    bufferMapped           : BufferTarget → GettableStateVar Bool
    mapBufferRange         : BufferTarget → Offset → Length → List MapBufferUsage → IO (Maybe (Ptr A))
    flushMappedBufferRange : BufferTarget → Offset → Length → IO ⊤

{-# COMPILE GHC withMappedBuffer       = \ aℓ a bℓ b -> Graphics.Rendering.OpenGL.GL.BufferObjects.withMappedBuffer       #-}
{-# COMPILE GHC mapBuffer              = \ aℓ a      -> Graphics.Rendering.OpenGL.GL.BufferObjects.mapBuffer              #-}
{-# COMPILE GHC unmapBuffer            =                Graphics.Rendering.OpenGL.GL.BufferObjects.unmapBuffer            #-}
{-# COMPILE GHC bufferAccess           =                Graphics.Rendering.OpenGL.GL.BufferObjects.bufferAccess           #-}
{-# COMPILE GHC bufferMapped           =                Graphics.Rendering.OpenGL.GL.BufferObjects.bufferMapped           #-}
{-# COMPILE GHC mapBufferRange         = \ aℓ a      -> Graphics.Rendering.OpenGL.GL.BufferObjects.mapBufferRange         #-}
{-# COMPILE GHC flushMappedBufferRange =                Graphics.Rendering.OpenGL.GL.BufferObjects.flushMappedBufferRange #-}


BufferIndex : Set
BufferIndex = GLuint

RangeStartIndex : Set
RangeStartIndex = GLintptr

RangeSize : Set
RangeSize = GLsizeiptr

BufferRange : Set
BufferRange = Tuple3 BufferObject RangeStartIndex RangeSize


data IndexedBufferTarget : Set where
    IndexedAtomicCounterBuffer : IndexedBufferTarget
    IndexedShaderStorageBuffer : IndexedBufferTarget
    IndexedTransformFeedbackBuffer : IndexedBufferTarget
    IndexedUniformBuffer : IndexedBufferTarget

{-# COMPILE GHC IndexedBufferTarget = data Graphics.Rendering.OpenGL.GL.BufferObjects.IndexedBufferTarget
    ( Graphics.Rendering.OpenGL.GL.BufferObjects.IndexedAtomicCounterBuffer
    | Graphics.Rendering.OpenGL.GL.BufferObjects.IndexedShaderStorageBuffer
    | Graphics.Rendering.OpenGL.GL.BufferObjects.IndexedTransformFeedbackBuffer
    | Graphics.Rendering.OpenGL.GL.BufferObjects.IndexedUniformBuffer
    ) #-}

postulate
    Eq[IndexedBufferTarget]   : Eq IndexedBufferTarget
    Ord[IndexedBufferTarget]  : Ord IndexedBufferTarget
    Show[IndexedBufferTarget] : Show IndexedBufferTarget

{-# COMPILE GHC Eq[IndexedBufferTarget]   = AgdaEq   #-}
{-# COMPILE GHC Ord[IndexedBufferTarget]  = AgdaOrd  #-}
{-# COMPILE GHC Show[IndexedBufferTarget] = AgdaShow #-}


postulate
    bindBufferBase     : IndexedBufferTarget → BufferIndex → StateVar (Maybe BufferObject)
    bindBufferRange    : IndexedBufferTarget → BufferIndex → StateVar (Maybe BufferRange)
    indexedBufferStart : IndexedBufferTarget → BufferIndex → GettableStateVar RangeStartIndex
    indexedBufferSize  : IndexedBufferTarget → BufferIndex → GettableStateVar RangeSize

{-# COMPILE GHC bindBufferBase     = Graphics.Rendering.OpenGL.GL.BufferObjects.bindBufferBase     #-}
{-# COMPILE GHC bindBufferRange    = Graphics.Rendering.OpenGL.GL.BufferObjects.bindBufferRange    #-}
{-# COMPILE GHC indexedBufferStart = Graphics.Rendering.OpenGL.GL.BufferObjects.indexedBufferStart #-}
{-# COMPILE GHC indexedBufferSize  = Graphics.Rendering.OpenGL.GL.BufferObjects.indexedBufferSize  #-}
