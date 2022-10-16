{-# OPTIONS --without-K #-}

module Ffi.Hs.Graphics.Rendering.OpenGL.GL.CoordTrans where

open import Agda.Builtin.IO          using (IO)
open import Agda.Builtin.List        using (List)
open import Agda.Builtin.Maybe       using (Maybe)
open import Agda.Builtin.Unit        using (⊤)
open import Agda.Primitive           using (Level)
open import Ffi.Hs.-base.Class       using (Eq; Ord; Show; Storable)
open import Ffi.Hs.Data.StateVar     using (StateVar; GettableStateVar)
open import Ffi.Hs.Data.Tuple        using (Tuple2)
open import Ffi.Hs.Foreign.Ptr       using (Ptr)
open import Ffi.Hs.Graphics.GL.Types

open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.Tensor       using (Vector3)
open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.VertexArrays using (Capability)
open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.VertexSpec   using (TextureUnit)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Graphics.Rendering.OpenGL.GL.CoordTrans
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ : Level
        A : Set aℓ
        C : Set
        M : Set → Set


postulate
    depthRange : StateVar (Tuple2 GLclampd GLclampd)

{-# COMPILE GHC depthRange = Graphics.Rendering.OpenGL.GL.CoordTrans.depthRange #-}


data Position : Set where
    mkPosition : GLint → GLint → Position

{-# COMPILE GHC Position = data Graphics.Rendering.OpenGL.GL.CoordTrans.Position
    ( Graphics.Rendering.OpenGL.GL.CoordTrans.Position
    ) #-}

postulate
    Eq[Position]   : Eq Position
    Ord[Position]  : Ord Position
    Show[Position] : Show Position

{-# COMPILE GHC Eq[Position]   = AgdaEq   #-}
{-# COMPILE GHC Ord[Position]  = AgdaOrd  #-}
{-# COMPILE GHC Show[Position] = AgdaShow #-}


data Size : Set where
    mkSize : GLsizei → GLsizei → Size

{-# COMPILE GHC Size = data Graphics.Rendering.OpenGL.GL.CoordTrans.Size
    ( Graphics.Rendering.OpenGL.GL.CoordTrans.Size
    ) #-}

postulate
    Eq[Size]   : Eq Size
    Ord[Size]  : Ord Size
    Show[Size] : Show Size

{-# COMPILE GHC Eq[Size]   = AgdaEq   #-}
{-# COMPILE GHC Ord[Size]  = AgdaOrd  #-}
{-# COMPILE GHC Show[Size] = AgdaShow #-}


postulate
    viewport        : StateVar (Tuple2 Position Size)
    maxViewportDims : GettableStateVar Size

{-# COMPILE GHC viewport        = Graphics.Rendering.OpenGL.GL.CoordTrans.viewport        #-}
{-# COMPILE GHC maxViewportDims = Graphics.Rendering.OpenGL.GL.CoordTrans.maxViewportDims #-}


data MatrixMode : Set where
    Modelview     : GLsizei → MatrixMode
    Projection    : MatrixMode
    Texture       : MatrixMode
    Color         : MatrixMode
    MatrixPalette : MatrixMode

{-# COMPILE GHC MatrixMode = data Graphics.Rendering.OpenGL.GL.CoordTrans.MatrixMode
    ( Graphics.Rendering.OpenGL.GL.CoordTrans.Modelview
    | Graphics.Rendering.OpenGL.GL.CoordTrans.Projection
    | Graphics.Rendering.OpenGL.GL.CoordTrans.Texture
    | Graphics.Rendering.OpenGL.GL.CoordTrans.Color
    | Graphics.Rendering.OpenGL.GL.CoordTrans.MatrixPalette
    ) #-}

postulate
    Eq[MatrixMode]   : Eq MatrixMode
    Ord[MatrixMode]  : Ord MatrixMode
    Show[MatrixMode] : Show MatrixMode

{-# COMPILE GHC Eq[MatrixMode]   = AgdaEq   #-}
{-# COMPILE GHC Ord[MatrixMode]  = AgdaOrd  #-}
{-# COMPILE GHC Show[MatrixMode] = AgdaShow #-}

postulate
    matrixMode : StateVar MatrixMode

{-# COMPILE GHC matrixMode = Graphics.Rendering.OpenGL.GL.CoordTrans.matrixMode #-}


data MatrixOrder : Set where
    ColumnMajor : MatrixOrder
    RowMajor    : MatrixOrder

{-# COMPILE GHC MatrixOrder = data Graphics.Rendering.OpenGL.GL.CoordTrans.MatrixOrder
    ( Graphics.Rendering.OpenGL.GL.CoordTrans.ColumnMajor
    | Graphics.Rendering.OpenGL.GL.CoordTrans.RowMajor
    ) #-}

postulate
    Eq[MatrixOrder]   : Eq MatrixOrder
    Ord[MatrixOrder]  : Ord MatrixOrder
    Show[MatrixOrder] : Show MatrixOrder

{-# COMPILE GHC Eq[MatrixOrder]   = AgdaEq   #-}
{-# COMPILE GHC Ord[MatrixOrder]  = AgdaOrd  #-}
{-# COMPILE GHC Show[MatrixOrder] = AgdaShow #-}


postulate
    MatrixComponent : Set → Set
    MatrixComponent[C]⇒Storable[C] : ⦃ MatrixComponent C ⦄ → Storable C

    rotate    : ⦃ MatrixComponent C ⦄ → Vector3 C → IO ⊤
    translate : ⦃ MatrixComponent C ⦄ → Vector3 C → IO ⊤
    scale     : ⦃ MatrixComponent C ⦄ → C → C → C → IO ⊤

    MatrixComponent[GLfloat]  : MatrixComponent GLfloat
    MatrixComponent[GLdouble] : MatrixComponent GLdouble


postulate
    Matrix : (Set → Set) → Set

    withNewMatrix       : ⦃ Matrix M ⦄ → ⦃ MatrixComponent C ⦄ → MatrixOrder → (Ptr C → IO ⊤) → IO (M C)
    withMatrix          : ⦃ Matrix M ⦄ → ⦃ MatrixComponent C ⦄ → M C → (MatrixOrder → Ptr C → IO A) → IO A
    newMatrix           : ⦃ Matrix M ⦄ → ⦃ MatrixComponent C ⦄ → MatrixOrder → List C → IO (M C)
    getMatrixComponents : ⦃ Matrix M ⦄ → ⦃ MatrixComponent C ⦄ → MatrixOrder → M C → IO (List C)

    matrix     : ⦃ Matrix M ⦄ → ⦃ MatrixComponent C ⦄ → Maybe MatrixMode → StateVar (M C)
    multMatrix : ⦃ Matrix M ⦄ → ⦃ MatrixComponent C ⦄ → M C → IO ⊤


postulate
    GLmatrix : Set → Set

    Matrix[GLmatrix]  : Matrix GLmatrix
    Eq[GLmatrix[A]]   : Eq (GLmatrix A)
    Ord[GLmatrix[A]]  : Ord (GLmatrix A)
    Show[GLmatrix[A]] : Show (GLmatrix A)

{-# COMPILE GHC Matrix[GLmatrix]  =        AgdaMatrix #-}
{-# COMPILE GHC Eq[GLmatrix[A]]   = \ a -> AgdaEq     #-}
{-# COMPILE GHC Ord[GLmatrix[A]]  = \ a -> AgdaOrd    #-}
{-# COMPILE GHC Show[GLmatrix[A]] = \ a -> AgdaShow   #-}


postulate
    loadIdentity           : IO ⊤
    ortho                  : GLdouble → GLdouble → GLdouble → GLdouble → GLdouble → GLdouble → IO ⊤
    frustum                : GLdouble → GLdouble → GLdouble → GLdouble → GLdouble → GLdouble → IO ⊤
    depthClamp             : StateVar Capability
    activeTexture          : StateVar TextureUnit
    preservingMatrix       : IO A → IO A
    unsafePreservingMatrix : IO A → IO A
    stackDepth             : Maybe MatrixMode → GettableStateVar GLsizei
    maxStackDepth          : MatrixMode → GettableStateVar GLsizei

    rescaleNormal : StateVar Capability
    normalize     : StateVar Capability


data Plane (A : Set aℓ) : Set aℓ where
    mkPlane : A → A → A → A → Plane A

{-# FOREIGN GHC type AgdaPlane aℓ = Graphics.Rendering.OpenGL.GL.CoordTrans.Plane #-}
{-# COMPILE GHC Plane = data(1) AgdaPlane (Graphics.Rendering.OpenGL.GL.CoordTrans.Plane) #-}

postulate
    Eq[Plane[A]]       : Eq (Plane A)
    Ord[Plane[A]]      : Ord (Plane A)
    Show[Plane[A]]     : Show (Plane A)
    Storable[Plane[A]] : Storable (Plane A)

{-# COMPILE GHC Eq[Plane[A]]       = \ aℓ a -> AgdaEq       #-}
{-# COMPILE GHC Ord[Plane[A]]      = \ aℓ a -> AgdaOrd      #-}
{-# COMPILE GHC Show[Plane[A]]     = \ aℓ a -> AgdaShow     #-}
{-# COMPILE GHC Storable[Plane[A]] = \ aℓ a -> AgdaStorable #-}


data TextureCoordName : Set where
    S T R Q : TextureCoordName

{-# COMPILE GHC TextureCoordName = data Graphics.Rendering.OpenGL.GL.CoordTrans.TextureCoordName
    ( Graphics.Rendering.OpenGL.GL.CoordTrans.S
    | Graphics.Rendering.OpenGL.GL.CoordTrans.T
    | Graphics.Rendering.OpenGL.GL.CoordTrans.R
    | Graphics.Rendering.OpenGL.GL.CoordTrans.Q
    ) #-}

postulate
    Eq[TextureCoordName]   : Eq TextureCoordName
    Ord[TextureCoordName]  : Ord TextureCoordName
    Show[TextureCoordName] : Show TextureCoordName

{-# COMPILE GHC Eq[TextureCoordName]   = AgdaEq   #-}
{-# COMPILE GHC Ord[TextureCoordName]  = AgdaOrd  #-}
{-# COMPILE GHC Show[TextureCoordName] = AgdaShow #-}


data TextureGenMode : Set where
    EyeLinear     : (Plane GLdouble) → TextureGenMode
    ObjectLinear  : (Plane GLdouble) → TextureGenMode
    SphereMap     : TextureGenMode
    NormalMap     : TextureGenMode
    ReflectionMap : TextureGenMode

{-# COMPILE GHC TextureGenMode = data Graphics.Rendering.OpenGL.GL.CoordTrans.TextureGenMode
    ( Graphics.Rendering.OpenGL.GL.CoordTrans.EyeLinear
    | Graphics.Rendering.OpenGL.GL.CoordTrans.ObjectLinear
    | Graphics.Rendering.OpenGL.GL.CoordTrans.SphereMap
    | Graphics.Rendering.OpenGL.GL.CoordTrans.NormalMap
    | Graphics.Rendering.OpenGL.GL.CoordTrans.ReflectionMap
    ) #-}

postulate
    Eq[TextureGenMode]   : Eq TextureGenMode
    Ord[TextureGenMode]  : Ord TextureGenMode
    Show[TextureGenMode] : Show TextureGenMode

{-# COMPILE GHC Eq[TextureGenMode]   = AgdaEq   #-}
{-# COMPILE GHC Ord[TextureGenMode]  = AgdaOrd  #-}
{-# COMPILE GHC Show[TextureGenMode] = AgdaShow #-}

postulate
    textureGenMode : TextureCoordName → StateVar (Maybe TextureGenMode)

{-# COMPILE GHC textureGenMode = Graphics.Rendering.OpenGL.GL.CoordTrans.textureGenMode #-}
