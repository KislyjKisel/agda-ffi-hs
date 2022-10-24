{-# OPTIONS --without-K #-}

module Ffi.Hs.Graphics.Rendering.OpenGL.GL.BufferObjects-Instanced where

open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.BufferObjects

instance
    inst:Eq[BufferObject]                    = Eq[BufferObject]
    inst:Ord[BufferObject]                   = Ord[BufferObject]
    inst:Show[BufferObject]                  = Show[BufferObject]
    inst:ObjectName[BufferObject]            = ObjectName[BufferObject]
    inst:GeneratableObjectName[BufferObject] = GeneratableObjectName[BufferObject]
    inst:CanBeLabeled[BufferObject]          = CanBeLabeled[BufferObject]

    inst:Eq[BufferTarget]   = Eq[BufferTarget]
    inst:Ord[BufferTarget]  = Ord[BufferTarget]
    inst:Show[BufferTarget] = Show[BufferTarget]

    inst:Eq[BufferUsage]   = Eq[BufferUsage]
    inst:Ord[BufferUsage]  = Ord[BufferUsage]
    inst:Show[BufferUsage] = Show[BufferUsage]

    inst:Eq[TransferDirection]   = Eq[TransferDirection]
    inst:Ord[TransferDirection]  = Ord[TransferDirection]
    inst:Show[TransferDirection] = Show[TransferDirection]

    inst:Eq[BufferAccess]   = Eq[BufferAccess]
    inst:Ord[BufferAccess]  = Ord[BufferAccess]
    inst:Show[BufferAccess] = Show[BufferAccess]

    inst:Eq[MappingFailure]   = Eq[MappingFailure]
    inst:Ord[MappingFailure]  = Ord[MappingFailure]
    inst:Show[MappingFailure] = Show[MappingFailure]

    inst:Eq[MapBufferUsage]   = Eq[MapBufferUsage]
    inst:Ord[MapBufferUsage]  = Ord[MapBufferUsage]
    inst:Show[MapBufferUsage] = Show[MapBufferUsage]

    inst:Eq[IndexedBufferTarget]   = Eq[IndexedBufferTarget]
    inst:Ord[IndexedBufferTarget]  = Ord[IndexedBufferTarget]
    inst:Show[IndexedBufferTarget] = Show[IndexedBufferTarget]
