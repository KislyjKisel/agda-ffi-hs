{-# OPTIONS --without-K #-}

module Ffi.Hs.Graphics.Rendering.OpenGL.GL.VertexSpec where

open import Agda.Builtin.Bool        using (Bool)
open import Agda.Builtin.IO          using (IO)
open import Agda.Builtin.Unit        using (⊤)
open import Ffi.Hs.-base.Class
open import Ffi.Hs.Data.StateVar     using (StateVar; GettableStateVar)
open import Ffi.Hs.Foreign.Ptr       using (Ptr)
open import Ffi.Hs.Graphics.GL.Types

open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.Tensor

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Graphics.Rendering.OpenGL.GL.VertexSpec
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        A : Set


postulate
    VertexComponent : Set → Set

    VertexComponent[GLshort]  : VertexComponent GLshort
    VertexComponent[GLint]    : VertexComponent GLint
    VertexComponent[GLfloat]  : VertexComponent GLfloat
    VertexComponent[GLdouble] : VertexComponent GLdouble

{-# FOREIGN GHC data AgdaVertexComponent a = Graphics.Rendering.OpenGL.GL.VertexSpec.VertexComponent a => AgdaVertexComponent #-}
{-# COMPILE GHC VertexComponent = type(0) AgdaVertexComponent #-}

{-# COMPILE GHC VertexComponent[GLshort]  = AgdaVertexComponent #-}
{-# COMPILE GHC VertexComponent[GLint]    = AgdaVertexComponent #-}
{-# COMPILE GHC VertexComponent[GLfloat]  = AgdaVertexComponent #-}
{-# COMPILE GHC VertexComponent[GLdouble] = AgdaVertexComponent #-}


postulate
    Vertex : Set → Set

    vertex  : ⦃ Vertex A ⦄ → A → IO ⊤
    vertexv : ⦃ Vertex A ⦄ → Ptr A → IO ⊤

    Vertex[Vertex2[A]] : ⦃ VertexComponent A ⦄ → Vertex (Vertex2 A)
    Vertex[Vertex3[A]] : ⦃ VertexComponent A ⦄ → Vertex (Vertex3 A)
    Vertex[Vertex4[A]] : ⦃ VertexComponent A ⦄ → Vertex (Vertex4 A)

{-# FOREIGN GHC data AgdaVertex a = Graphics.Rendering.OpenGL.GL.VertexSpec.Vertex a => AgdaVertex #-}
{-# COMPILE GHC Vertex = type(0) AgdaVertex #-}

{-# COMPILE GHC vertex  = \ a AgdaVertex -> Graphics.Rendering.OpenGL.GL.VertexSpec.vertex  #-}
{-# COMPILE GHC vertexv = \ a AgdaVertex -> Graphics.Rendering.OpenGL.GL.VertexSpec.vertexv #-}

{-# COMPILE GHC Vertex[Vertex2[A]] = \ a AgdaVertexComponent -> AgdaVertex #-}
{-# COMPILE GHC Vertex[Vertex3[A]] = \ a AgdaVertexComponent -> AgdaVertex #-}
{-# COMPILE GHC Vertex[Vertex4[A]] = \ a AgdaVertexComponent -> AgdaVertex #-}


data TextureUnit : Set where
    mkTextureUnit : GLuint → TextureUnit

{-# COMPILE GHC TextureUnit = data Graphics.Rendering.OpenGL.GL.VertexSpec.TextureUnit (Graphics.Rendering.OpenGL.GL.VertexSpec.TextureUnit) #-}

postulate
    Eq[TextureUnit]       : Eq TextureUnit
    Ord[TextureUnit]      : Ord TextureUnit
    Show[TextureUnit]     : Show TextureUnit
    Storable[TextureUnit] : Storable TextureUnit

{-# COMPILE GHC Eq[TextureUnit]       = AgdaEq       #-}
{-# COMPILE GHC Ord[TextureUnit]      = AgdaOrd      #-}
{-# COMPILE GHC Show[TextureUnit]     = AgdaShow     #-}
{-# COMPILE GHC Storable[TextureUnit] = AgdaStorable #-}

postulate
    maxTextureUnit : GettableStateVar TextureUnit

{-# COMPILE GHC maxTextureUnit = Graphics.Rendering.OpenGL.GL.VertexSpec.maxTextureUnit #-}


postulate
    TexCoordComponent : Set → Set

    TexCoordComponent[GLshort]  : TexCoordComponent GLshort
    TexCoordComponent[GLint]    : TexCoordComponent GLint
    TexCoordComponent[GLfloat]  : TexCoordComponent GLfloat
    TexCoordComponent[GLdouble] : TexCoordComponent GLdouble

{-# FOREIGN GHC data AgdaTexCoordComponent a = Graphics.Rendering.OpenGL.GL.VertexSpec.TexCoordComponent a => AgdaTexCoordComponent #-}
{-# COMPILE GHC TexCoordComponent = type(0) AgdaTexCoordComponent #-}

{-# COMPILE GHC TexCoordComponent[GLshort]  = AgdaTexCoordComponent #-}
{-# COMPILE GHC TexCoordComponent[GLint]    = AgdaTexCoordComponent #-}
{-# COMPILE GHC TexCoordComponent[GLfloat]  = AgdaTexCoordComponent #-}
{-# COMPILE GHC TexCoordComponent[GLdouble] = AgdaTexCoordComponent #-}


data TexCoord1 (A : Set) : Set where
    mkTexCoord1 : A → TexCoord1 A

{-# COMPILE GHC TexCoord1 = data(0) Graphics.Rendering.OpenGL.GL.VertexSpec.TexCoord1
    ( Graphics.Rendering.OpenGL.GL.VertexSpec.TexCoord1
    ) #-}

postulate
    Functor[TexCoord1]     : Functor TexCoord1
    Applicative[TexCoord1] : Applicative TexCoord1
    Foldable[TexCoord1]    : Foldable TexCoord1
    Traversable[TexCoord1] : Traversable TexCoord1
    Bounded[TexCoord1[A]]  : ⦃ Bounded A ⦄ → Bounded (TexCoord1 A)
    Eq[TexCoord1[A]]       : ⦃ Eq A ⦄ → Eq (TexCoord1 A)
    Ord[TexCoord1[A]]      : ⦃ Ord A ⦄ → Ord (TexCoord1 A)
    Read[TexCoord1[A]]     : ⦃ Read A ⦄ → Read (TexCoord1 A)
    Show[TexCoord1[A]]     : ⦃ Show A ⦄ → Show (TexCoord1 A)
    Ix[TexCoord1[A]]       : ⦃ Ix A ⦄ → Ix (TexCoord1 A)
    Storable[TexCoord1[A]] : ⦃ Storable A ⦄ → Storable (TexCoord1 A)

{-# COMPILE GHC Functor[TexCoord1]     =                     AgdaFunctor     #-}
{-# COMPILE GHC Applicative[TexCoord1] =                     AgdaApplicative #-}
{-# COMPILE GHC Foldable[TexCoord1]    =                     AgdaFoldable    #-}
{-# COMPILE GHC Traversable[TexCoord1] =                     AgdaTraversable #-}
{-# COMPILE GHC Bounded[TexCoord1[A]]  = \ a AgdaBounded  -> AgdaBounded     #-}
{-# COMPILE GHC Eq[TexCoord1[A]]       = \ a AgdaEq       -> AgdaEq          #-}
{-# COMPILE GHC Ord[TexCoord1[A]]      = \ a AgdaOrd      -> AgdaOrd         #-}
{-# COMPILE GHC Read[TexCoord1[A]]     = \ a AgdaRead     -> AgdaRead        #-}
{-# COMPILE GHC Show[TexCoord1[A]]     = \ a AgdaShow     -> AgdaShow        #-}
{-# COMPILE GHC Ix[TexCoord1[A]]       = \ a AgdaIx       -> AgdaIx          #-}
{-# COMPILE GHC Storable[TexCoord1[A]] = \ a AgdaStorable -> AgdaStorable    #-}


data TexCoord2 (A : Set) : Set where
    mkTexCoord2 : A → A → TexCoord2 A

{-# COMPILE GHC TexCoord2 = data(0) Graphics.Rendering.OpenGL.GL.VertexSpec.TexCoord2
    ( Graphics.Rendering.OpenGL.GL.VertexSpec.TexCoord2
    ) #-}

postulate
    Functor[TexCoord2]     : Functor TexCoord2
    Applicative[TexCoord2] : Applicative TexCoord2
    Foldable[TexCoord2]    : Foldable TexCoord2
    Traversable[TexCoord2] : Traversable TexCoord2
    Bounded[TexCoord2[A]]  : ⦃ Bounded A ⦄ → Bounded (TexCoord2 A)
    Eq[TexCoord2[A]]       : ⦃ Eq A ⦄ → Eq (TexCoord2 A)
    Ord[TexCoord2[A]]      : ⦃ Ord A ⦄ → Ord (TexCoord2 A)
    Read[TexCoord2[A]]     : ⦃ Read A ⦄ → Read (TexCoord2 A)
    Show[TexCoord2[A]]     : ⦃ Show A ⦄ → Show (TexCoord2 A)
    Ix[TexCoord2[A]]       : ⦃ Ix A ⦄ → Ix (TexCoord2 A)
    Storable[TexCoord2[A]] : ⦃ Storable A ⦄ → Storable (TexCoord2 A)

{-# COMPILE GHC Functor[TexCoord2]     =                     AgdaFunctor     #-}
{-# COMPILE GHC Applicative[TexCoord2] =                     AgdaApplicative #-}
{-# COMPILE GHC Foldable[TexCoord2]    =                     AgdaFoldable    #-}
{-# COMPILE GHC Traversable[TexCoord2] =                     AgdaTraversable #-}
{-# COMPILE GHC Bounded[TexCoord2[A]]  = \ a AgdaBounded  -> AgdaBounded     #-}
{-# COMPILE GHC Eq[TexCoord2[A]]       = \ a AgdaEq       -> AgdaEq          #-}
{-# COMPILE GHC Ord[TexCoord2[A]]      = \ a AgdaOrd      -> AgdaOrd         #-}
{-# COMPILE GHC Read[TexCoord2[A]]     = \ a AgdaRead     -> AgdaRead        #-}
{-# COMPILE GHC Show[TexCoord2[A]]     = \ a AgdaShow     -> AgdaShow        #-}
{-# COMPILE GHC Ix[TexCoord2[A]]       = \ a AgdaIx       -> AgdaIx          #-}
{-# COMPILE GHC Storable[TexCoord2[A]] = \ a AgdaStorable -> AgdaStorable    #-}


data TexCoord3 (A : Set) : Set where
    mkTexCoord3 : A → A → A → TexCoord3 A

{-# COMPILE GHC TexCoord3 = data(0) Graphics.Rendering.OpenGL.GL.VertexSpec.TexCoord3
    ( Graphics.Rendering.OpenGL.GL.VertexSpec.TexCoord3
    ) #-}

postulate
    Functor[TexCoord3]     : Functor TexCoord3
    Applicative[TexCoord3] : Applicative TexCoord3
    Foldable[TexCoord3]    : Foldable TexCoord3
    Traversable[TexCoord3] : Traversable TexCoord3
    Bounded[TexCoord3[A]]  : ⦃ Bounded A ⦄ → Bounded (TexCoord3 A)
    Eq[TexCoord3[A]]       : ⦃ Eq A ⦄ → Eq (TexCoord3 A)
    Ord[TexCoord3[A]]      : ⦃ Ord A ⦄ → Ord (TexCoord3 A)
    Read[TexCoord3[A]]     : ⦃ Read A ⦄ → Read (TexCoord3 A)
    Show[TexCoord3[A]]     : ⦃ Show A ⦄ → Show (TexCoord3 A)
    Ix[TexCoord3[A]]       : ⦃ Ix A ⦄ → Ix (TexCoord3 A)
    Storable[TexCoord3[A]] : ⦃ Storable A ⦄ → Storable (TexCoord3 A)

{-# COMPILE GHC Functor[TexCoord3]     =                     AgdaFunctor     #-}
{-# COMPILE GHC Applicative[TexCoord3] =                     AgdaApplicative #-}
{-# COMPILE GHC Foldable[TexCoord3]    =                     AgdaFoldable    #-}
{-# COMPILE GHC Traversable[TexCoord3] =                     AgdaTraversable #-}
{-# COMPILE GHC Bounded[TexCoord3[A]]  = \ a AgdaBounded  -> AgdaBounded     #-}
{-# COMPILE GHC Eq[TexCoord3[A]]       = \ a AgdaEq       -> AgdaEq          #-}
{-# COMPILE GHC Ord[TexCoord3[A]]      = \ a AgdaOrd      -> AgdaOrd         #-}
{-# COMPILE GHC Read[TexCoord3[A]]     = \ a AgdaRead     -> AgdaRead        #-}
{-# COMPILE GHC Show[TexCoord3[A]]     = \ a AgdaShow     -> AgdaShow        #-}
{-# COMPILE GHC Ix[TexCoord3[A]]       = \ a AgdaIx       -> AgdaIx          #-}
{-# COMPILE GHC Storable[TexCoord3[A]] = \ a AgdaStorable -> AgdaStorable    #-}


data TexCoord4 (A : Set) : Set where
    mkTexCoord4 : A → A → A → A → TexCoord4 A

{-# COMPILE GHC TexCoord4 = data(0) Graphics.Rendering.OpenGL.GL.VertexSpec.TexCoord4
    ( Graphics.Rendering.OpenGL.GL.VertexSpec.TexCoord4
    ) #-}

postulate
    Functor[TexCoord4]     : Functor TexCoord4
    Applicative[TexCoord4] : Applicative TexCoord4
    Foldable[TexCoord4]    : Foldable TexCoord4
    Traversable[TexCoord4] : Traversable TexCoord4
    Bounded[TexCoord4[A]]  : ⦃ Bounded A ⦄ → Bounded (TexCoord4 A)
    Eq[TexCoord4[A]]       : ⦃ Eq A ⦄ → Eq (TexCoord4 A)
    Ord[TexCoord4[A]]      : ⦃ Ord A ⦄ → Ord (TexCoord4 A)
    Read[TexCoord4[A]]     : ⦃ Read A ⦄ → Read (TexCoord4 A)
    Show[TexCoord4[A]]     : ⦃ Show A ⦄ → Show (TexCoord4 A)
    Ix[TexCoord4[A]]       : ⦃ Ix A ⦄ → Ix (TexCoord4 A)
    Storable[TexCoord4[A]] : ⦃ Storable A ⦄ → Storable (TexCoord4 A)

{-# COMPILE GHC Functor[TexCoord4]     =                     AgdaFunctor     #-}
{-# COMPILE GHC Applicative[TexCoord4] =                     AgdaApplicative #-}
{-# COMPILE GHC Foldable[TexCoord4]    =                     AgdaFoldable    #-}
{-# COMPILE GHC Traversable[TexCoord4] =                     AgdaTraversable #-}
{-# COMPILE GHC Bounded[TexCoord4[A]]  = \ a AgdaBounded  -> AgdaBounded     #-}
{-# COMPILE GHC Eq[TexCoord4[A]]       = \ a AgdaEq       -> AgdaEq          #-}
{-# COMPILE GHC Ord[TexCoord4[A]]      = \ a AgdaOrd      -> AgdaOrd         #-}
{-# COMPILE GHC Read[TexCoord4[A]]     = \ a AgdaRead     -> AgdaRead        #-}
{-# COMPILE GHC Show[TexCoord4[A]]     = \ a AgdaShow     -> AgdaShow        #-}
{-# COMPILE GHC Ix[TexCoord4[A]]       = \ a AgdaIx       -> AgdaIx          #-}
{-# COMPILE GHC Storable[TexCoord4[A]] = \ a AgdaStorable -> AgdaStorable    #-}


postulate
    TexCoord : Set → Set

    texCoord       : ⦃ TexCoord A ⦄ → A → IO ⊤
    texCoordv      : ⦃ TexCoord A ⦄ → Ptr A → IO ⊤
    multiTexCoord  : ⦃ TexCoord A ⦄ → TextureUnit → A → IO ⊤
    multiTexCoordv : ⦃ TexCoord A ⦄ → TextureUnit → Ptr A → IO ⊤

    TexCoord[TexCoord1[A]] : ⦃ TexCoordComponent A ⦄ → TexCoord (TexCoord1 A)
    TexCoord[TexCoord2[A]] : ⦃ TexCoordComponent A ⦄ → TexCoord (TexCoord2 A)
    TexCoord[TexCoord3[A]] : ⦃ TexCoordComponent A ⦄ → TexCoord (TexCoord3 A)
    TexCoord[TexCoord4[A]] : ⦃ TexCoordComponent A ⦄ → TexCoord (TexCoord4 A)

{-# FOREIGN GHC data AgdaTexCoord a = Graphics.Rendering.OpenGL.GL.VertexSpec.TexCoord a => AgdaTexCoord #-}
{-# COMPILE GHC TexCoord = type(0) AgdaTexCoord #-}

{-# COMPILE GHC texCoord       = \ a AgdaTexCoord -> Graphics.Rendering.OpenGL.GL.VertexSpec.texCoord       #-}
{-# COMPILE GHC texCoordv      = \ a AgdaTexCoord -> Graphics.Rendering.OpenGL.GL.VertexSpec.texCoordv      #-}
{-# COMPILE GHC multiTexCoord  = \ a AgdaTexCoord -> Graphics.Rendering.OpenGL.GL.VertexSpec.multiTexCoord  #-}
{-# COMPILE GHC multiTexCoordv = \ a AgdaTexCoord -> Graphics.Rendering.OpenGL.GL.VertexSpec.multiTexCoordv #-}

{-# COMPILE GHC TexCoord[TexCoord1[A]] = \ a AgdaTexCoordComponent -> AgdaTexCoord #-}
{-# COMPILE GHC TexCoord[TexCoord2[A]] = \ a AgdaTexCoordComponent -> AgdaTexCoord #-}
{-# COMPILE GHC TexCoord[TexCoord3[A]] = \ a AgdaTexCoordComponent -> AgdaTexCoord #-}
{-# COMPILE GHC TexCoord[TexCoord4[A]] = \ a AgdaTexCoordComponent -> AgdaTexCoord #-}


data Normal3 (A : Set) : Set where
    mkNormal3 : A → A → A → Normal3 A

{-# COMPILE GHC Normal3 = data(0) Graphics.Rendering.OpenGL.GL.VertexSpec.Normal3
    ( Graphics.Rendering.OpenGL.GL.VertexSpec.Normal3
    ) #-}

postulate
    Functor[Normal3]     : Functor Normal3
    Applicative[Normal3] : Applicative Normal3
    Foldable[Normal3]    : Foldable Normal3
    Traversable[Normal3] : Traversable Normal3
    Bounded[Normal3[A]]  : ⦃ Bounded A ⦄ → Bounded (Normal3 A)
    Eq[Normal3[A]]       : ⦃ Eq A ⦄ → Eq (Normal3 A)
    Ord[Normal3[A]]      : ⦃ Ord A ⦄ → Ord (Normal3 A)
    Read[Normal3[A]]     : ⦃ Read A ⦄ → Read (Normal3 A)
    Show[Normal3[A]]     : ⦃ Show A ⦄ → Show (Normal3 A)
    Ix[Normal3[A]]       : ⦃ Ix A ⦄ → Ix (Normal3 A)
    Storable[Normal3[A]] : ⦃ Storable A ⦄ → Storable (Normal3 A)

{-# COMPILE GHC Functor[Normal3]     =                     AgdaFunctor     #-}
{-# COMPILE GHC Applicative[Normal3] =                     AgdaApplicative #-}
{-# COMPILE GHC Foldable[Normal3]    =                     AgdaFoldable    #-}
{-# COMPILE GHC Traversable[Normal3] =                     AgdaTraversable #-}
{-# COMPILE GHC Bounded[Normal3[A]]  = \ a AgdaBounded  -> AgdaBounded     #-}
{-# COMPILE GHC Eq[Normal3[A]]       = \ a AgdaEq       -> AgdaEq          #-}
{-# COMPILE GHC Ord[Normal3[A]]      = \ a AgdaOrd      -> AgdaOrd         #-}
{-# COMPILE GHC Read[Normal3[A]]     = \ a AgdaRead     -> AgdaRead        #-}
{-# COMPILE GHC Show[Normal3[A]]     = \ a AgdaShow     -> AgdaShow        #-}
{-# COMPILE GHC Ix[Normal3[A]]       = \ a AgdaIx       -> AgdaIx          #-}
{-# COMPILE GHC Storable[Normal3[A]] = \ a AgdaStorable -> AgdaStorable    #-}

postulate
    currentNormal : StateVar (Normal3 GLfloat)

{-# COMPILE GHC currentNormal = Graphics.Rendering.OpenGL.GL.VertexSpec.currentNormal #-}


postulate
    NormalComponent : Set → Set

    NormalComponent[GLbyte]   : NormalComponent GLbyte
    NormalComponent[GLshort]  : NormalComponent GLshort
    NormalComponent[GLint]    : NormalComponent GLint
    NormalComponent[GLfloat]  : NormalComponent GLfloat
    NormalComponent[GLdouble] : NormalComponent GLdouble

{-# FOREIGN GHC data AgdaNormalComponent a = Graphics.Rendering.OpenGL.GL.VertexSpec.NormalComponent a => AgdaNormalComponent #-}
{-# COMPILE GHC NormalComponent = type(0) AgdaNormalComponent #-}

{-# COMPILE GHC NormalComponent[GLbyte]   = AgdaNormalComponent #-}
{-# COMPILE GHC NormalComponent[GLshort]  = AgdaNormalComponent #-}
{-# COMPILE GHC NormalComponent[GLint]    = AgdaNormalComponent #-}
{-# COMPILE GHC NormalComponent[GLfloat]  = AgdaNormalComponent #-}
{-# COMPILE GHC NormalComponent[GLdouble] = AgdaNormalComponent #-}

postulate
    Normal : Set → Set

    normal  : ⦃ Normal A ⦄ → A → IO ⊤
    normalv : ⦃ Normal A ⦄ → Ptr A → IO ⊤

    Normal[Normal3[A]] : ⦃ NormalComponent A ⦄ → Normal (Normal3 A)

{-# FOREIGN GHC data AgdaNormal a = Graphics.Rendering.OpenGL.GL.VertexSpec.Normal a => AgdaNormal #-}
{-# COMPILE GHC Normal = type(0) AgdaNormal #-}

{-# COMPILE GHC normal  = \ a AgdaNormal -> Graphics.Rendering.OpenGL.GL.VertexSpec.normal  #-}
{-# COMPILE GHC normalv = \ a AgdaNormal -> Graphics.Rendering.OpenGL.GL.VertexSpec.normalv #-}

{-# COMPILE GHC Normal[Normal3[A]] = \ a AgdaNormalComponent -> AgdaNormal #-}


data FogCoord1 (A : Set) : Set where
    mkFogCoord1 : A → FogCoord1 A

{-# COMPILE GHC FogCoord1 = data(0) Graphics.Rendering.OpenGL.GL.VertexSpec.FogCoord1
    ( Graphics.Rendering.OpenGL.GL.VertexSpec.FogCoord1
    ) #-}

postulate
    Functor[FogCoord1]     : Functor FogCoord1
    Applicative[FogCoord1] : Applicative FogCoord1
    Foldable[FogCoord1]    : Foldable FogCoord1
    Traversable[FogCoord1] : Traversable FogCoord1
    Bounded[FogCoord1[A]]  : ⦃ Bounded A ⦄ → Bounded (FogCoord1 A)
    Eq[FogCoord1[A]]       : ⦃ Eq A ⦄ → Eq (FogCoord1 A)
    Ord[FogCoord1[A]]      : ⦃ Ord A ⦄ → Ord (FogCoord1 A)
    Read[FogCoord1[A]]     : ⦃ Read A ⦄ → Read (FogCoord1 A)
    Show[FogCoord1[A]]     : ⦃ Show A ⦄ → Show (FogCoord1 A)
    Ix[FogCoord1[A]]       : ⦃ Ix A ⦄ → Ix (FogCoord1 A)
    Storable[FogCoord1[A]] : ⦃ Storable A ⦄ → Storable (FogCoord1 A)

{-# COMPILE GHC Functor[FogCoord1]     =                     AgdaFunctor     #-}
{-# COMPILE GHC Applicative[FogCoord1] =                     AgdaApplicative #-}
{-# COMPILE GHC Foldable[FogCoord1]    =                     AgdaFoldable    #-}
{-# COMPILE GHC Traversable[FogCoord1] =                     AgdaTraversable #-}
{-# COMPILE GHC Bounded[FogCoord1[A]]  = \ a AgdaBounded  -> AgdaBounded     #-}
{-# COMPILE GHC Eq[FogCoord1[A]]       = \ a AgdaEq       -> AgdaEq          #-}
{-# COMPILE GHC Ord[FogCoord1[A]]      = \ a AgdaOrd      -> AgdaOrd         #-}
{-# COMPILE GHC Read[FogCoord1[A]]     = \ a AgdaRead     -> AgdaRead        #-}
{-# COMPILE GHC Show[FogCoord1[A]]     = \ a AgdaShow     -> AgdaShow        #-}
{-# COMPILE GHC Ix[FogCoord1[A]]       = \ a AgdaIx       -> AgdaIx          #-}
{-# COMPILE GHC Storable[FogCoord1[A]] = \ a AgdaStorable -> AgdaStorable    #-}

postulate
    currentFogCoord : StateVar (FogCoord1 GLfloat)

{-# COMPILE GHC currentFogCoord = Graphics.Rendering.OpenGL.GL.VertexSpec.currentFogCoord #-}


postulate
    FogCoordComponent : Set → Set

    FogCoordComponent[GLfloat]  : FogCoordComponent GLfloat
    FogCoordComponent[GLdouble] : FogCoordComponent GLdouble

{-# FOREIGN GHC data AgdaFogCoordComponent a = Graphics.Rendering.OpenGL.GL.VertexSpec.FogCoordComponent a => AgdaFogCoordComponent #-}
{-# COMPILE GHC FogCoordComponent = type(0) AgdaFogCoordComponent #-}

{-# COMPILE GHC FogCoordComponent[GLfloat]  = AgdaFogCoordComponent #-}
{-# COMPILE GHC FogCoordComponent[GLdouble] = AgdaFogCoordComponent #-}

postulate
    FogCoord : Set → Set

    fogCoord  : ⦃ FogCoord A ⦄ → A → IO ⊤
    fogCoordv : ⦃ FogCoord A ⦄ → Ptr A → IO ⊤

    FogCoord[FogCoord1[A]] : ⦃ FogCoordComponent A ⦄ → FogCoord (FogCoord1 A)

{-# FOREIGN GHC data AgdaFogCoord a = Graphics.Rendering.OpenGL.GL.VertexSpec.FogCoord a => AgdaFogCoord #-}
{-# COMPILE GHC FogCoord = type(0) AgdaFogCoord #-}

{-# COMPILE GHC fogCoord  = \ a AgdaFogCoord -> Graphics.Rendering.OpenGL.GL.VertexSpec.fogCoord  #-}
{-# COMPILE GHC fogCoordv = \ a AgdaFogCoord -> Graphics.Rendering.OpenGL.GL.VertexSpec.fogCoordv #-}

{-# COMPILE GHC FogCoord[FogCoord1[A]] = \ a AgdaFogCoordComponent -> AgdaFogCoord #-}


data Color3 (A : Set) : Set where
    mkColor3 : A → A → A → Color3 A

{-# COMPILE GHC Color3 = data(0) Graphics.Rendering.OpenGL.GL.VertexSpec.Color3
    ( Graphics.Rendering.OpenGL.GL.VertexSpec.Color3
    ) #-}

postulate
    Functor[Color3]     : Functor Color3
    Applicative[Color3] : Applicative Color3
    Foldable[Color3]    : Foldable Color3
    Traversable[Color3] : Traversable Color3
    Bounded[Color3[A]]  : ⦃ Bounded A ⦄ → Bounded (Color3 A)
    Eq[Color3[A]]       : ⦃ Eq A ⦄ → Eq (Color3 A)
    Ord[Color3[A]]      : ⦃ Ord A ⦄ → Ord (Color3 A)
    Read[Color3[A]]     : ⦃ Read A ⦄ → Read (Color3 A)
    Show[Color3[A]]     : ⦃ Show A ⦄ → Show (Color3 A)
    Ix[Color3[A]]       : ⦃ Ix A ⦄ → Ix (Color3 A)
    Storable[Color3[A]] : ⦃ Storable A ⦄ → Storable (Color3 A)

{-# COMPILE GHC Functor[Color3]     =                     AgdaFunctor     #-}
{-# COMPILE GHC Applicative[Color3] =                     AgdaApplicative #-}
{-# COMPILE GHC Foldable[Color3]    =                     AgdaFoldable    #-}
{-# COMPILE GHC Traversable[Color3] =                     AgdaTraversable #-}
{-# COMPILE GHC Bounded[Color3[A]]  = \ a AgdaBounded  -> AgdaBounded     #-}
{-# COMPILE GHC Eq[Color3[A]]       = \ a AgdaEq       -> AgdaEq          #-}
{-# COMPILE GHC Ord[Color3[A]]      = \ a AgdaOrd      -> AgdaOrd         #-}
{-# COMPILE GHC Read[Color3[A]]     = \ a AgdaRead     -> AgdaRead        #-}
{-# COMPILE GHC Show[Color3[A]]     = \ a AgdaShow     -> AgdaShow        #-}
{-# COMPILE GHC Ix[Color3[A]]       = \ a AgdaIx       -> AgdaIx          #-}
{-# COMPILE GHC Storable[Color3[A]] = \ a AgdaStorable -> AgdaStorable    #-}


data Color4 (A : Set) : Set where
    mkColor4 : A → A → A → A → Color4 A

{-# COMPILE GHC Color4 = data(0) Graphics.Rendering.OpenGL.GL.VertexSpec.Color4
    ( Graphics.Rendering.OpenGL.GL.VertexSpec.Color4
    ) #-}

postulate
    Functor[Color4]     : Functor Color4
    Applicative[Color4] : Applicative Color4
    Foldable[Color4]    : Foldable Color4
    Traversable[Color4] : Traversable Color4
    Bounded[Color4[A]]  : ⦃ Bounded A ⦄ → Bounded (Color4 A)
    Eq[Color4[A]]       : ⦃ Eq A ⦄ → Eq (Color4 A)
    Ord[Color4[A]]      : ⦃ Ord A ⦄ → Ord (Color4 A)
    Read[Color4[A]]     : ⦃ Read A ⦄ → Read (Color4 A)
    Show[Color4[A]]     : ⦃ Show A ⦄ → Show (Color4 A)
    Ix[Color4[A]]       : ⦃ Ix A ⦄ → Ix (Color4 A)
    Storable[Color4[A]] : ⦃ Storable A ⦄ → Storable (Color4 A)

{-# COMPILE GHC Functor[Color4]     =                     AgdaFunctor     #-}
{-# COMPILE GHC Applicative[Color4] =                     AgdaApplicative #-}
{-# COMPILE GHC Foldable[Color4]    =                     AgdaFoldable    #-}
{-# COMPILE GHC Traversable[Color4] =                     AgdaTraversable #-}
{-# COMPILE GHC Bounded[Color4[A]]  = \ a AgdaBounded  -> AgdaBounded     #-}
{-# COMPILE GHC Eq[Color4[A]]       = \ a AgdaEq       -> AgdaEq          #-}
{-# COMPILE GHC Ord[Color4[A]]      = \ a AgdaOrd      -> AgdaOrd         #-}
{-# COMPILE GHC Read[Color4[A]]     = \ a AgdaRead     -> AgdaRead        #-}
{-# COMPILE GHC Show[Color4[A]]     = \ a AgdaShow     -> AgdaShow        #-}
{-# COMPILE GHC Ix[Color4[A]]       = \ a AgdaIx       -> AgdaIx          #-}
{-# COMPILE GHC Storable[Color4[A]] = \ a AgdaStorable -> AgdaStorable    #-}


postulate
    ColorComponent : Set → Set

    ColorComponent[GLbyte]   : ColorComponent GLbyte
    ColorComponent[GLubyte]  : ColorComponent GLubyte
    ColorComponent[GLshort]  : ColorComponent GLshort
    ColorComponent[GLushort] : ColorComponent GLushort
    ColorComponent[GLint]    : ColorComponent GLint
    ColorComponent[GLuint]   : ColorComponent GLuint
    ColorComponent[GLfloat]  : ColorComponent GLfloat
    ColorComponent[GLdouble] : ColorComponent GLdouble

{-# FOREIGN GHC data AgdaColorComponent a = Graphics.Rendering.OpenGL.GL.VertexSpec.ColorComponent a => AgdaColorComponent #-}
{-# COMPILE GHC ColorComponent = type(0) AgdaColorComponent #-}

{-# COMPILE GHC ColorComponent[GLbyte]   = AgdaColorComponent #-}
{-# COMPILE GHC ColorComponent[GLubyte]  = AgdaColorComponent #-}
{-# COMPILE GHC ColorComponent[GLshort]  = AgdaColorComponent #-}
{-# COMPILE GHC ColorComponent[GLushort] = AgdaColorComponent #-}
{-# COMPILE GHC ColorComponent[GLint]    = AgdaColorComponent #-}
{-# COMPILE GHC ColorComponent[GLuint]   = AgdaColorComponent #-}
{-# COMPILE GHC ColorComponent[GLfloat]  = AgdaColorComponent #-}
{-# COMPILE GHC ColorComponent[GLdouble] = AgdaColorComponent #-}


postulate
    rgbaMode              : GettableStateVar Bool
    currentColor          : StateVar (Color4 GLfloat)
    currentSecondaryColor : StateVar (Color3 GLfloat)

{-# COMPILE GHC rgbaMode              = Graphics.Rendering.OpenGL.GL.VertexSpec.rgbaMode              #-}
{-# COMPILE GHC currentColor          = Graphics.Rendering.OpenGL.GL.VertexSpec.currentColor          #-}
{-# COMPILE GHC currentSecondaryColor = Graphics.Rendering.OpenGL.GL.VertexSpec.currentSecondaryColor #-}


postulate
    Color : Set → Set

    color  : ⦃ Color A ⦄ → A → IO ⊤
    colorv : ⦃ Color A ⦄ → Ptr A → IO ⊤

    Color[Color3[A]] : ⦃ ColorComponent A ⦄ → Color (Color3 A)
    Color[Color4[A]] : ⦃ ColorComponent A ⦄ → Color (Color4 A)

{-# FOREIGN GHC data AgdaColor a = Graphics.Rendering.OpenGL.GL.VertexSpec.Color a => AgdaColor #-}
{-# COMPILE GHC Color = type(0) AgdaColor #-}

{-# COMPILE GHC color  = \ a AgdaColor -> Graphics.Rendering.OpenGL.GL.VertexSpec.color  #-}
{-# COMPILE GHC colorv = \ a AgdaColor -> Graphics.Rendering.OpenGL.GL.VertexSpec.colorv #-}

{-# COMPILE GHC Color[Color3[A]] = \ a AgdaColorComponent -> AgdaColor #-}
{-# COMPILE GHC Color[Color4[A]] = \ a AgdaColorComponent -> AgdaColor #-}


postulate
    SecondaryColor : Set → Set

    secondaryColor  : ⦃ SecondaryColor A ⦄ → A → IO ⊤
    secondaryColorv : ⦃ SecondaryColor A ⦄ → Ptr A → IO ⊤

    SecondaryColor[Color3[A]] : ⦃ ColorComponent A ⦄ → SecondaryColor (Color3 A)

{-# FOREIGN GHC data AgdaSecondaryColor a = Graphics.Rendering.OpenGL.GL.VertexSpec.SecondaryColor a => AgdaSecondaryColor #-}
{-# COMPILE GHC SecondaryColor = type(0) AgdaSecondaryColor #-}

{-# COMPILE GHC secondaryColor  = \ a AgdaSecondaryColor -> Graphics.Rendering.OpenGL.GL.VertexSpec.secondaryColor  #-}
{-# COMPILE GHC secondaryColorv = \ a AgdaSecondaryColor -> Graphics.Rendering.OpenGL.GL.VertexSpec.secondaryColorv #-}

{-# COMPILE GHC SecondaryColor[Color3[A]] = \ a AgdaColorComponent -> AgdaSecondaryColor #-}


data Index1 (A : Set) : Set where
    mkIndex1 : A → Index1 A

{-# COMPILE GHC Index1 = data(0) Graphics.Rendering.OpenGL.GL.VertexSpec.Index1
    ( Graphics.Rendering.OpenGL.GL.VertexSpec.Index1
    ) #-}

postulate
    Functor[Index1]     : Functor Index1
    Applicative[Index1] : Applicative Index1
    Foldable[Index1]    : Foldable Index1
    Traversable[Index1] : Traversable Index1
    Bounded[Index1[A]]  : ⦃ Bounded A ⦄ → Bounded (Index1 A)
    Eq[Index1[A]]       : ⦃ Eq A ⦄ → Eq (Index1 A)
    Ord[Index1[A]]      : ⦃ Ord A ⦄ → Ord (Index1 A)
    Read[Index1[A]]     : ⦃ Read A ⦄ → Read (Index1 A)
    Show[Index1[A]]     : ⦃ Show A ⦄ → Show (Index1 A)
    Ix[Index1[A]]       : ⦃ Ix A ⦄ → Ix (Index1 A)
    Storable[Index1[A]] : ⦃ Storable A ⦄ → Storable (Index1 A)

{-# COMPILE GHC Functor[Index1]     =                     AgdaFunctor     #-}
{-# COMPILE GHC Applicative[Index1] =                     AgdaApplicative #-}
{-# COMPILE GHC Foldable[Index1]    =                     AgdaFoldable    #-}
{-# COMPILE GHC Traversable[Index1] =                     AgdaTraversable #-}
{-# COMPILE GHC Bounded[Index1[A]]  = \ a AgdaBounded  -> AgdaBounded     #-}
{-# COMPILE GHC Eq[Index1[A]]       = \ a AgdaEq       -> AgdaEq          #-}
{-# COMPILE GHC Ord[Index1[A]]      = \ a AgdaOrd      -> AgdaOrd         #-}
{-# COMPILE GHC Read[Index1[A]]     = \ a AgdaRead     -> AgdaRead        #-}
{-# COMPILE GHC Show[Index1[A]]     = \ a AgdaShow     -> AgdaShow        #-}
{-# COMPILE GHC Ix[Index1[A]]       = \ a AgdaIx       -> AgdaIx          #-}
{-# COMPILE GHC Storable[Index1[A]] = \ a AgdaStorable -> AgdaStorable    #-}

postulate
    currentIndex : StateVar (Index1 GLint)

{-# COMPILE GHC currentIndex = Graphics.Rendering.OpenGL.GL.VertexSpec.currentIndex #-}


postulate
    IndexComponent : Set → Set

    IndexComponent[GLubyte]  : IndexComponent GLubyte
    IndexComponent[GLshort]  : IndexComponent GLshort
    IndexComponent[GLint]    : IndexComponent GLint
    IndexComponent[GLfloat]  : IndexComponent GLfloat
    IndexComponent[GLdouble] : IndexComponent GLdouble

{-# FOREIGN GHC data AgdaIndexComponent a = Graphics.Rendering.OpenGL.GL.VertexSpec.IndexComponent a => AgdaIndexComponent #-}
{-# COMPILE GHC IndexComponent = type(0) AgdaIndexComponent #-}

{-# COMPILE GHC IndexComponent[GLubyte]  = AgdaIndexComponent #-}
{-# COMPILE GHC IndexComponent[GLshort]  = AgdaIndexComponent #-}
{-# COMPILE GHC IndexComponent[GLint]    = AgdaIndexComponent #-}
{-# COMPILE GHC IndexComponent[GLfloat]  = AgdaIndexComponent #-}
{-# COMPILE GHC IndexComponent[GLdouble] = AgdaIndexComponent #-}

postulate
    Index : Set → Set

    index  : ⦃ Index A ⦄ → A → IO ⊤
    indexv : ⦃ Index A ⦄ → Ptr A → IO ⊤

    Index[Index1[A]] : ⦃ IndexComponent A ⦄ → Index (Index1 A)

{-# FOREIGN GHC data AgdaIndex a = Graphics.Rendering.OpenGL.GL.VertexSpec.Index a => AgdaIndex #-}
{-# COMPILE GHC Index = type(0) AgdaIndex #-}

{-# COMPILE GHC index  = \ a AgdaIndex -> Graphics.Rendering.OpenGL.GL.VertexSpec.index  #-}
{-# COMPILE GHC indexv = \ a AgdaIndex -> Graphics.Rendering.OpenGL.GL.VertexSpec.indexv #-}

{-# COMPILE GHC Index[Index1[A]] = \ a AgdaIndexComponent -> AgdaIndex #-}


data IntegerHandling : Set where
    ToFloat           : IntegerHandling
    ToNormalizedFloat : IntegerHandling
    KeepIntegral      : IntegerHandling

{-# COMPILE GHC IntegerHandling = data Graphics.Rendering.OpenGL.GL.VertexSpec.IntegerHandling
    ( Graphics.Rendering.OpenGL.GL.VertexSpec.ToFloat
    | Graphics.Rendering.OpenGL.GL.VertexSpec.ToNormalizedFloat
    | Graphics.Rendering.OpenGL.GL.VertexSpec.KeepIntegral
    ) #-}

postulate
    Eq[IntegerHandling]   : Eq IntegerHandling
    Ord[IntegerHandling]  : Ord IntegerHandling
    Show[IntegerHandling] : Show IntegerHandling

{-# COMPILE GHC Eq[IntegerHandling]   = AgdaEq   #-}
{-# COMPILE GHC Ord[IntegerHandling]  = AgdaOrd  #-}
{-# COMPILE GHC Show[IntegerHandling] = AgdaShow #-}


data AttribLocation : Set where
    mkAttribLocation : GLuint → AttribLocation

{-# COMPILE GHC AttribLocation = data Graphics.Rendering.OpenGL.GL.VertexSpec.AttribLocation
    ( Graphics.Rendering.OpenGL.GL.VertexSpec.AttribLocation
    ) #-}

postulate
    Eq[AttribLocation]   : Eq AttribLocation
    Ord[AttribLocation]  : Ord AttribLocation
    Show[AttribLocation] : Show AttribLocation

{-# COMPILE GHC Eq[AttribLocation]   = AgdaEq   #-}
{-# COMPILE GHC Ord[AttribLocation]  = AgdaOrd  #-}
{-# COMPILE GHC Show[AttribLocation] = AgdaShow #-}

postulate
    currentVertexAttrib   : AttribLocation → StateVar (Vertex4 GLfloat)
    currentVertexAttribI  : AttribLocation → StateVar (Vertex4 GLint)
    currentVertexAttribIu : AttribLocation → StateVar (Vertex4 GLuint)

{-# COMPILE GHC currentVertexAttrib   = Graphics.Rendering.OpenGL.GL.VertexSpec.currentVertexAttrib   #-}
{-# COMPILE GHC currentVertexAttribI  = Graphics.Rendering.OpenGL.GL.VertexSpec.currentVertexAttribI  #-}
{-# COMPILE GHC currentVertexAttribIu = Graphics.Rendering.OpenGL.GL.VertexSpec.currentVertexAttribIu #-}


postulate
    VertexAttribComponent : Set → Set

    VertexAttribComponent[A]⇒Storable[A] : ⦃ VertexAttribComponent A ⦄ → Storable A
    VertexAttribComponent[A]⇒Num[A]      : ⦃ VertexAttribComponent A ⦄ → Num A

    VertexAttribComponent[GLbyte]   : VertexAttribComponent GLbyte
    VertexAttribComponent[GLubyte]  : VertexAttribComponent GLubyte
    VertexAttribComponent[GLshort]  : VertexAttribComponent GLshort
    VertexAttribComponent[GLushort] : VertexAttribComponent GLushort
    VertexAttribComponent[GLint]    : VertexAttribComponent GLint
    VertexAttribComponent[GLuint]   : VertexAttribComponent GLuint
    VertexAttribComponent[GLfloat]  : VertexAttribComponent GLfloat
    VertexAttribComponent[GLdouble] : VertexAttribComponent GLdouble

{-# FOREIGN GHC data AgdaVertexAttribComponent a = Graphics.Rendering.OpenGL.GL.VertexSpec.VertexAttribComponent a => AgdaVertexAttribComponent #-}
{-# COMPILE GHC VertexAttribComponent = type(0) AgdaVertexAttribComponent #-}

{-# COMPILE GHC VertexAttribComponent[A]⇒Storable[A] = \ a AgdaVertexAttribComponent -> AgdaStorable #-}
{-# COMPILE GHC VertexAttribComponent[A]⇒Num[A]      = \ a AgdaVertexAttribComponent -> AgdaNum      #-}

{-# COMPILE GHC VertexAttribComponent[GLbyte]   = AgdaVertexAttribComponent #-}
{-# COMPILE GHC VertexAttribComponent[GLubyte]  = AgdaVertexAttribComponent #-}
{-# COMPILE GHC VertexAttribComponent[GLshort]  = AgdaVertexAttribComponent #-}
{-# COMPILE GHC VertexAttribComponent[GLushort] = AgdaVertexAttribComponent #-}
{-# COMPILE GHC VertexAttribComponent[GLint]    = AgdaVertexAttribComponent #-}
{-# COMPILE GHC VertexAttribComponent[GLuint]   = AgdaVertexAttribComponent #-}
{-# COMPILE GHC VertexAttribComponent[GLfloat]  = AgdaVertexAttribComponent #-}
{-# COMPILE GHC VertexAttribComponent[GLdouble] = AgdaVertexAttribComponent #-}


postulate
    VertexAttrib : Set → Set

    vertexAttrib  : ⦃ VertexAttrib A ⦄ → IntegerHandling → AttribLocation → A → IO ⊤
    vertexAttribv : ⦃ VertexAttrib A ⦄ → IntegerHandling → AttribLocation → Ptr A → IO ⊤

    VertexAttrib[Vector4[A]]   : ⦃ VertexAttribComponent A ⦄ → VertexAttrib (Vector4 A)
    VertexAttrib[Vector3[A]]   : ⦃ VertexAttribComponent A ⦄ → VertexAttrib (Vector3 A)
    VertexAttrib[Vector2[A]]   : ⦃ VertexAttribComponent A ⦄ → VertexAttrib (Vector2 A)
    VertexAttrib[Vector1[A]]   : ⦃ VertexAttribComponent A ⦄ → VertexAttrib (Vector1 A)
    VertexAttrib[Vertex4[A]]   : ⦃ VertexAttribComponent A ⦄ → VertexAttrib (Vertex4 A)
    VertexAttrib[Vertex3[A]]   : ⦃ VertexAttribComponent A ⦄ → VertexAttrib (Vertex3 A)
    VertexAttrib[Vertex2[A]]   : ⦃ VertexAttribComponent A ⦄ → VertexAttrib (Vertex2 A)
    VertexAttrib[Vertex1[A]]   : ⦃ VertexAttribComponent A ⦄ → VertexAttrib (Vertex1 A)
    VertexAttrib[Index1[A]]    : ⦃ VertexAttribComponent A ⦄ → VertexAttrib (Index1 A)
    VertexAttrib[Color4[A]]    : ⦃ VertexAttribComponent A ⦄ → VertexAttrib (Color4 A)
    VertexAttrib[Color3[A]]    : ⦃ VertexAttribComponent A ⦄ → VertexAttrib (Color3 A)
    VertexAttrib[FogCoord1[A]] : ⦃ VertexAttribComponent A ⦄ → VertexAttrib (FogCoord1 A)
    VertexAttrib[Normal3[A]]   : ⦃ VertexAttribComponent A ⦄ → VertexAttrib (Normal3 A)
    VertexAttrib[TexCoord4[A]] : ⦃ VertexAttribComponent A ⦄ → VertexAttrib (TexCoord4 A)
    VertexAttrib[TexCoord3[A]] : ⦃ VertexAttribComponent A ⦄ → VertexAttrib (TexCoord3 A)
    VertexAttrib[TexCoord2[A]] : ⦃ VertexAttribComponent A ⦄ → VertexAttrib (TexCoord2 A)
    VertexAttrib[TexCoord1[A]] : ⦃ VertexAttribComponent A ⦄ → VertexAttrib (TexCoord1 A)

{-# FOREIGN GHC data AgdaVertexAttrib a = Graphics.Rendering.OpenGL.GL.VertexSpec.VertexAttrib a => AgdaVertexAttrib #-}
{-# COMPILE GHC VertexAttrib = type(0) AgdaVertexAttrib #-}

{-# COMPILE GHC vertexAttrib  = \ a AgdaVertexAttrib -> Graphics.Rendering.OpenGL.GL.VertexSpec.vertexAttrib  #-}
{-# COMPILE GHC vertexAttribv = \ a AgdaVertexAttrib -> Graphics.Rendering.OpenGL.GL.VertexSpec.vertexAttribv #-}

{-# COMPILE GHC VertexAttrib[Vector4[A]]   = \ a AgdaVertexAttribComponent -> AgdaVertexAttrib #-}
{-# COMPILE GHC VertexAttrib[Vector3[A]]   = \ a AgdaVertexAttribComponent -> AgdaVertexAttrib #-}
{-# COMPILE GHC VertexAttrib[Vector2[A]]   = \ a AgdaVertexAttribComponent -> AgdaVertexAttrib #-}
{-# COMPILE GHC VertexAttrib[Vector1[A]]   = \ a AgdaVertexAttribComponent -> AgdaVertexAttrib #-}
{-# COMPILE GHC VertexAttrib[Vertex4[A]]   = \ a AgdaVertexAttribComponent -> AgdaVertexAttrib #-}
{-# COMPILE GHC VertexAttrib[Vertex3[A]]   = \ a AgdaVertexAttribComponent -> AgdaVertexAttrib #-}
{-# COMPILE GHC VertexAttrib[Vertex2[A]]   = \ a AgdaVertexAttribComponent -> AgdaVertexAttrib #-}
{-# COMPILE GHC VertexAttrib[Vertex1[A]]   = \ a AgdaVertexAttribComponent -> AgdaVertexAttrib #-}
{-# COMPILE GHC VertexAttrib[Index1[A]]    = \ a AgdaVertexAttribComponent -> AgdaVertexAttrib #-}
{-# COMPILE GHC VertexAttrib[Color4[A]]    = \ a AgdaVertexAttribComponent -> AgdaVertexAttrib #-}
{-# COMPILE GHC VertexAttrib[Color3[A]]    = \ a AgdaVertexAttribComponent -> AgdaVertexAttrib #-}
{-# COMPILE GHC VertexAttrib[FogCoord1[A]] = \ a AgdaVertexAttribComponent -> AgdaVertexAttrib #-}
{-# COMPILE GHC VertexAttrib[Normal3[A]]   = \ a AgdaVertexAttribComponent -> AgdaVertexAttrib #-}
{-# COMPILE GHC VertexAttrib[TexCoord4[A]] = \ a AgdaVertexAttribComponent -> AgdaVertexAttrib #-}
{-# COMPILE GHC VertexAttrib[TexCoord3[A]] = \ a AgdaVertexAttribComponent -> AgdaVertexAttrib #-}
{-# COMPILE GHC VertexAttrib[TexCoord2[A]] = \ a AgdaVertexAttribComponent -> AgdaVertexAttrib #-}
{-# COMPILE GHC VertexAttrib[TexCoord1[A]] = \ a AgdaVertexAttribComponent -> AgdaVertexAttrib #-}
