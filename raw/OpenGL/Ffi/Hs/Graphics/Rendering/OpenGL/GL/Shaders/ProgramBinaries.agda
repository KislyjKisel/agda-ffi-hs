{-# OPTIONS --without-K #-}

module Ffi.Hs.Graphics.Rendering.OpenGL.GL.Shaders.ProgramBinaries where

open import Agda.Builtin.List        using (List)
open import Ffi.Hs.-base.Class       using (Eq; Ord; Show)
open import Ffi.Hs.Data.ByteString   using (ByteString)
open import Ffi.Hs.Data.StateVar     using (StateVar; GettableStateVar)
open import Ffi.Hs.Graphics.GL.Types using (GLenum)

open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.Shaders.ProgramObjects using (Program)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Graphics.Rendering.OpenGL.GL.Shaders.ProgramBinaries
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}


data ProgramBinaryFormat : Set where
    mkProgramBinaryFormat : GLenum → ProgramBinaryFormat

{-# COMPILE GHC ProgramBinaryFormat = data Graphics.Rendering.OpenGL.GL.Shaders.ProgramBinaries.ProgramBinaryFormat
    ( Graphics.Rendering.OpenGL.GL.Shaders.ProgramBinaries.ProgramBinaryFormat
    ) #-}

postulate
    Eq[ProgramBinaryFormat]   : Eq ProgramBinaryFormat
    Ord[ProgramBinaryFormat]  : Ord ProgramBinaryFormat
    Show[ProgramBinaryFormat] : Show ProgramBinaryFormat

{-# COMPILE GHC Eq[ProgramBinaryFormat]   = AgdaEq   #-}
{-# COMPILE GHC Ord[ProgramBinaryFormat]  = AgdaOrd  #-}
{-# COMPILE GHC Show[ProgramBinaryFormat] = AgdaShow #-}

postulate
    programBinaryFormats : GettableStateVar (List ProgramBinaryFormat)

{-# COMPILE GHC programBinaryFormats = Graphics.Rendering.OpenGL.GL.Shaders.ProgramBinaries.programBinaryFormats #-}


data ProgramBinary : Set where
    mkProgramBinary : ProgramBinaryFormat → ByteString → ProgramBinary

{-# COMPILE GHC ProgramBinary = data Graphics.Rendering.OpenGL.GL.Shaders.ProgramBinaries.ProgramBinary
    ( Graphics.Rendering.OpenGL.GL.Shaders.ProgramBinaries.ProgramBinary
    ) #-}

postulate
    Eq[ProgramBinary]   : Eq ProgramBinary
    Ord[ProgramBinary]  : Ord ProgramBinary
    Show[ProgramBinary] : Show ProgramBinary

{-# COMPILE GHC Eq[ProgramBinary]   = AgdaEq   #-}
{-# COMPILE GHC Ord[ProgramBinary]  = AgdaOrd  #-}
{-# COMPILE GHC Show[ProgramBinary] = AgdaShow #-}

postulate
    programBinary : Program → StateVar ProgramBinary

{-# COMPILE GHC programBinary = Graphics.Rendering.OpenGL.GL.Shaders.ProgramBinaries.programBinary #-}
