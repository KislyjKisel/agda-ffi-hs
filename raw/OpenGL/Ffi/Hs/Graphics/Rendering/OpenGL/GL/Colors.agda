{-# OPTIONS --without-K #-}

module Ffi.Hs.Graphics.Rendering.OpenGL.GL.Colors where

open import Ffi.Hs.-base.Class using (Eq; Ord; Show)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Graphics.Rendering.OpenGL.GL.Colors
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

-- ...

data Face : Set where
    Front        : Face
    Back         : Face
    FrontAndBack : Face

{-# COMPILE GHC Face = data Graphics.Rendering.OpenGL.GL.Colors.Face
    ( Graphics.Rendering.OpenGL.GL.Colors.Front
    | Graphics.Rendering.OpenGL.GL.Colors.Back
    | Graphics.Rendering.OpenGL.GL.Colors.FrontAndBack
    ) #-}

postulate
    Eq[Face]   : Eq Face
    Ord[Face]  : Ord Face
    Show[Face] : Show Face

{-# COMPILE GHC Eq[Face]   = AgdaEq   #-}
{-# COMPILE GHC Ord[Face]  = AgdaOrd  #-}
{-# COMPILE GHC Show[Face] = AgdaShow #-}

-- ...
