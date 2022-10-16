{-# OPTIONS --without-K #-}

module Ffi.Hs.Graphics.Rendering.OpenGL.GL.BeginEnd where

open import Agda.Builtin.IO      using (IO)
open import Agda.Builtin.Unit    using (⊤)
open import Agda.Primitive       using (Level)
open import Ffi.Hs.-base.Class   using (Eq; Ord; Show)
open import Ffi.Hs.Data.StateVar using (StateVar)

open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.PrimitiveMode using (PrimitiveMode)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Graphics.Rendering.OpenGL.GL.BeginEnd
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ : Level
        A : Set aℓ


postulate
    renderPrimitive       : PrimitiveMode → IO A → IO A
    unsafeRenderPrimitive : PrimitiveMode → IO A → IO A
    primitiveRestart      : IO ⊤

{-# COMPILE GHC renderPrimitive       = \ aℓ a -> Graphics.Rendering.OpenGL.GL.BeginEnd.renderPrimitive       #-}
{-# COMPILE GHC unsafeRenderPrimitive = \ aℓ a -> Graphics.Rendering.OpenGL.GL.BeginEnd.unsafeRenderPrimitive #-}
{-# COMPILE GHC primitiveRestart      =           Graphics.Rendering.OpenGL.GL.BeginEnd.primitiveRestart      #-}

data EdgeFlag : Set where
    BeginsInteriorEdge : EdgeFlag
    BeginsBoundaryEdge : EdgeFlag

{-# COMPILE GHC EdgeFlag = data Graphics.Rendering.OpenGL.GL.BeginEnd.EdgeFlag
    ( Graphics.Rendering.OpenGL.GL.BeginEnd.BeginsInteriorEdge
    | Graphics.Rendering.OpenGL.GL.BeginEnd.BeginsBoundaryEdge
    ) #-}

postulate
    Eq[EdgeFlag]   : Eq EdgeFlag
    Ord[EdgeFlag]  : Ord EdgeFlag
    Show[EdgeFlag] : Show EdgeFlag

{-# COMPILE GHC Eq[EdgeFlag]   = AgdaEq   #-}
{-# COMPILE GHC Ord[EdgeFlag]  = AgdaOrd  #-}
{-# COMPILE GHC Show[EdgeFlag] = AgdaShow #-}

postulate
    edgeFlag : StateVar EdgeFlag

{-# COMPILE GHC edgeFlag = Graphics.Rendering.OpenGL.GL.BeginEnd.edgeFlag #-}
