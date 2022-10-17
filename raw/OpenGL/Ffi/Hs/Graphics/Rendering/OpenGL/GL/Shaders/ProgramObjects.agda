{-# OPTIONS --without-K #-}

module Ffi.Hs.Graphics.Rendering.OpenGL.GL.Shaders.ProgramObjects where

open import Agda.Builtin.Bool      using (Bool)
open import Agda.Builtin.Char      using (Char)
open import Agda.Builtin.IO        using (IO)
open import Agda.Builtin.List      using (List)
open import Agda.Builtin.Maybe     using (Maybe)
open import Agda.Builtin.Unit      using (⊤)
open import Ffi.Hs.-base.Class     using (Eq; Ord; Show)
open import Ffi.Hs.Data.ObjectName using (ObjectName)
open import Ffi.Hs.Data.StateVar   using (StateVar; GettableStateVar; SettableStateVar)

open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.DebugOutput           using (CanBeLabeled)
open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.Framebuffer           using (DrawBufferIndex)
open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.Shaders.ShaderObjects using (Shader)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Graphics.Rendering.OpenGL.GL.Shaders.ProgramObjects
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
import MAlonzo.Code.Ffi.Hs.Graphics.Rendering.OpenGL.GL.DebugOutput (AgdaCanBeLabeled(AgdaCanBeLabeled))
import MAlonzo.Code.Ffi.Hs.Data.ObjectName (AgdaObjectName(AgdaObjectName))
#-}


postulate
    Program : Set

{-# COMPILE GHC Program = type Graphics.Rendering.OpenGL.GL.Shaders.ProgramObjects.Program #-}

postulate
    Eq[Program]           : Eq Program
    Ord[Program]          : Ord Program
    Show[Program]         : Show Program
    ObjectName[Program]   : ObjectName Program
    CanBeLabeled[Program] : CanBeLabeled Program

{-# COMPILE GHC Eq[Program]           = AgdaEq           #-}
{-# COMPILE GHC Ord[Program]          = AgdaOrd          #-}
{-# COMPILE GHC Show[Program]         = AgdaShow         #-}
{-# COMPILE GHC ObjectName[Program]   = AgdaObjectName   #-}
{-# COMPILE GHC CanBeLabeled[Program] = AgdaCanBeLabeled #-}


postulate
    createProgram                : IO Program
    programDeleteStatus          : Program → GettableStateVar Bool
    attachShader                 : Program → Shader → IO ⊤
    detachShader                 : Program → Shader → IO ⊤
    attachedShaders              : Program → StateVar (List Shader)
    linkProgram                  : Program → IO ⊤
    linkStatus                   : Program → GettableStateVar Bool
    validateProgram              : Program → IO ⊤
    validateStatus               : Program → GettableStateVar Bool
    programInfoLog               : Program → GettableStateVar (List Char)
    currentProgram               : StateVar (Maybe Program)
    programSeparable             : Program → StateVar Bool
    programBinaryRetrievableHint : Program → StateVar Bool
    bindFragDataLocation         : Program → List Char → SettableStateVar DrawBufferIndex
    getFragDataLocation          : Program → List Char → IO (Maybe DrawBufferIndex)

{-# COMPILE GHC createProgram                = Graphics.Rendering.OpenGL.GL.Shaders.ProgramObjects.createProgram                #-}
{-# COMPILE GHC programDeleteStatus          = Graphics.Rendering.OpenGL.GL.Shaders.ProgramObjects.programDeleteStatus          #-}
{-# COMPILE GHC attachShader                 = Graphics.Rendering.OpenGL.GL.Shaders.ProgramObjects.attachShader                 #-}
{-# COMPILE GHC detachShader                 = Graphics.Rendering.OpenGL.GL.Shaders.ProgramObjects.detachShader                 #-}
{-# COMPILE GHC attachedShaders              = Graphics.Rendering.OpenGL.GL.Shaders.ProgramObjects.attachedShaders              #-}
{-# COMPILE GHC linkProgram                  = Graphics.Rendering.OpenGL.GL.Shaders.ProgramObjects.linkProgram                  #-}
{-# COMPILE GHC linkStatus                   = Graphics.Rendering.OpenGL.GL.Shaders.ProgramObjects.linkStatus                   #-}
{-# COMPILE GHC validateProgram              = Graphics.Rendering.OpenGL.GL.Shaders.ProgramObjects.validateProgram              #-}
{-# COMPILE GHC validateStatus               = Graphics.Rendering.OpenGL.GL.Shaders.ProgramObjects.validateStatus               #-}
{-# COMPILE GHC programInfoLog               = Graphics.Rendering.OpenGL.GL.Shaders.ProgramObjects.programInfoLog               #-}
{-# COMPILE GHC currentProgram               = Graphics.Rendering.OpenGL.GL.Shaders.ProgramObjects.currentProgram               #-}
{-# COMPILE GHC programSeparable             = Graphics.Rendering.OpenGL.GL.Shaders.ProgramObjects.programSeparable             #-}
{-# COMPILE GHC programBinaryRetrievableHint = Graphics.Rendering.OpenGL.GL.Shaders.ProgramObjects.programBinaryRetrievableHint #-}
{-# COMPILE GHC bindFragDataLocation         = Graphics.Rendering.OpenGL.GL.Shaders.ProgramObjects.bindFragDataLocation         #-}
{-# COMPILE GHC getFragDataLocation          = Graphics.Rendering.OpenGL.GL.Shaders.ProgramObjects.getFragDataLocation          #-}
