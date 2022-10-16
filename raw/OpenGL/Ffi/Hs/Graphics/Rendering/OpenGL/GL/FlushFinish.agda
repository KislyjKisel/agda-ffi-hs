{-# OPTIONS --without-K #-}

module Ffi.Hs.Graphics.Rendering.OpenGL.GL.FlushFinish where

open import Agda.Builtin.IO   using (IO)
open import Agda.Builtin.Unit using (⊤)

{-# FOREIGN GHC
import qualified Graphics.Rendering.OpenGL.GL.FlushFinish
#-}

postulate
    flush  : IO ⊤
    finish : IO ⊤

{-# COMPILE GHC flush  = Graphics.Rendering.OpenGL.GL.FlushFinish.flush  #-}
{-# COMPILE GHC finish = Graphics.Rendering.OpenGL.GL.FlushFinish.finish #-}
