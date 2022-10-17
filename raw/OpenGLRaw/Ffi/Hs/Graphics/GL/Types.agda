{-# OPTIONS --without-K #-}

module Ffi.Hs.Graphics.GL.Types where

open import Agda.Builtin.IO        using (IO)
open import Agda.Builtin.Unit      using (⊤)
open import Ffi.Hs.Data.Int        using (Int8; Int16; Int32; Int64)
open import Ffi.Hs.Data.Word       using (Word8; Word16; Word32; Word64)
open import Ffi.Hs.Foreign.C.Types using (CChar; CPtrdiff)
open import Ffi.Hs.Foreign.Ptr     using (Ptr; FunPtr)
open import Ffi.Hs.GHC.Float       using (Float; Double)
open import Ffi.Hs.Numeric.Fixed   using (Fixed)
open import Ffi.Hs.Numeric.Half    using (Half)

{-# FOREIGN GHC
import qualified Graphics.GL.Types
#-}

GLboolean : Set
GLboolean = Word8

GLbyte : Set
GLbyte = Int8

GLubyte : Set
GLubyte = Word8

GLchar : Set
GLchar = CChar

GLshort : Set
GLshort = Int16

GLushort : Set
GLushort = Word16

GLint : Set
GLint = Int32

GLuint : Set
GLuint = Word32

GLfixed : Set
GLfixed = Fixed

GLint64 : Set
GLint64 = Int64

GLuint64 : Set
GLuint64 = Word64

GLsizei : Set
GLsizei = Int32

GLenum : Set
GLenum = Word32

GLintptr : Set
GLintptr = CPtrdiff

GLsizeiptr : Set
GLsizeiptr = CPtrdiff

GLsync : Set
GLsync = Ptr ⊤

GLbitfield : Set
GLbitfield = Word32

GLhalf : Set
GLhalf = Half

GLfloat : Set
GLfloat = Float

GLclampf : Set
GLclampf = Float

GLdouble : Set
GLdouble = Double

GLclampd : Set
GLclampd = Double

GLDEBUGPROCFunc : Set
GLDEBUGPROCFunc = GLenum → GLenum → GLuint → GLenum → GLsizei → Ptr GLchar → Ptr ⊤ → IO ⊤

GLDEBUGPROC : Set
GLDEBUGPROC = FunPtr GLDEBUGPROCFunc

postulate
    makeGLDEBUGPROC : GLDEBUGPROCFunc → IO (FunPtr GLDEBUGPROCFunc)

{-# COMPILE GHC makeGLDEBUGPROC = Graphics.GL.Types.makeGLDEBUGPROC #-}

GLvoid : Set
GLvoid = ⊤


GLcharARB : Set
GLcharARB = CChar

GLint64EXT : Set
GLint64EXT = Int64

GLuint64EXT : Set
GLuint64EXT = Word64

GLintptrARB : Set
GLintptrARB = CPtrdiff

GLsizeiptrARB : Set
GLsizeiptrARB = CPtrdiff

GLhalfARB : Set
GLhalfARB = Half

GLhalfNV : Set
GLhalfNV = Half

GLDEBUGPROCAMDFunc : Set
GLDEBUGPROCAMDFunc = GLuint → GLenum → GLenum → GLsizei → Ptr GLchar → Ptr ⊤ → IO ⊤

GLDEBUGPROCAMD : Set
GLDEBUGPROCAMD = FunPtr GLDEBUGPROCAMDFunc

postulate
    makeGLDEBUGPROCAMD : GLDEBUGPROCAMDFunc → IO (FunPtr GLDEBUGPROCAMDFunc)

{-# COMPILE GHC makeGLDEBUGPROCAMD = Graphics.GL.Types.makeGLDEBUGPROCAMD #-}

GLDEBUGPROCARB : Set
GLDEBUGPROCARB = GLDEBUGPROC

GLDEBUGPROCARBFunc : Set
GLDEBUGPROCARBFunc = GLDEBUGPROCFunc

postulate
    makeGLDEBUGPROCARB : GLDEBUGPROCARBFunc → IO (FunPtr GLDEBUGPROCARBFunc)

{-# COMPILE GHC makeGLDEBUGPROCARB = Graphics.GL.Types.makeGLDEBUGPROCARB #-}

GLDEBUGPROCKHR : Set
GLDEBUGPROCKHR = GLDEBUGPROC

GLDEBUGPROCKHRFunc : Set
GLDEBUGPROCKHRFunc = GLDEBUGPROCFunc

postulate
    makeGLDEBUGPROCKHR : GLDEBUGPROCKHRFunc → IO (FunPtr GLDEBUGPROCKHRFunc)

{-# COMPILE GHC makeGLDEBUGPROCKHR = Graphics.GL.Types.makeGLDEBUGPROCKHR #-}
