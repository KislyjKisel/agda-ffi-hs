{-# OPTIONS --without-K #-}

module Ffi.Hs.Graphics.Rendering.OpenGL.GL.Tensor where

open import Ffi.Hs.-base.Class

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Graphics.Rendering.OpenGL.GL.Tensor
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        A : Set


data Vertex1 (A : Set) : Set where
    mkVertex1 : A → Vertex1 A

{-# COMPILE GHC Vertex1 = data(0) Graphics.Rendering.OpenGL.GL.Tensor.Vertex1
    ( Graphics.Rendering.OpenGL.GL.Tensor.Vertex1
    ) #-}

postulate
    Functor[Vertex1]     : Functor Vertex1
    Applicative[Vertex1] : Applicative Vertex1
    Foldable[Vertex1]    : Foldable Vertex1
    Traversable[Vertex1] : Traversable Vertex1
    Bounded[Vertex1[A]]  : ⦃ Bounded A ⦄ → Bounded (Vertex1 A)
    Eq[Vertex1[A]]       : ⦃ Eq A ⦄ → Eq (Vertex1 A)
    Ord[Vertex1[A]]      : ⦃ Ord A ⦄ → Ord (Vertex1 A)
    Read[Vertex1[A]]     : ⦃ Read A ⦄ → Read (Vertex1 A)
    Show[Vertex1[A]]     : ⦃ Show A ⦄ → Show (Vertex1 A)
    Ix[Vertex1[A]]       : ⦃ Ix A ⦄ → Ix (Vertex1 A)
    Storable[Vertex1[A]] : ⦃ Storable A ⦄ → Storable (Vertex1 A)

{-# COMPILE GHC Functor[Vertex1]     =                     AgdaFunctor     #-}
{-# COMPILE GHC Applicative[Vertex1] =                     AgdaApplicative #-}
{-# COMPILE GHC Foldable[Vertex1]    =                     AgdaFoldable    #-}
{-# COMPILE GHC Traversable[Vertex1] =                     AgdaTraversable #-}
{-# COMPILE GHC Bounded[Vertex1[A]]  = \ a AgdaBounded  -> AgdaBounded     #-}
{-# COMPILE GHC Eq[Vertex1[A]]       = \ a AgdaEq       -> AgdaEq          #-}
{-# COMPILE GHC Ord[Vertex1[A]]      = \ a AgdaOrd      -> AgdaOrd         #-}
{-# COMPILE GHC Read[Vertex1[A]]     = \ a AgdaRead     -> AgdaRead        #-}
{-# COMPILE GHC Show[Vertex1[A]]     = \ a AgdaShow     -> AgdaShow        #-}
{-# COMPILE GHC Ix[Vertex1[A]]       = \ a AgdaIx       -> AgdaIx          #-}
{-# COMPILE GHC Storable[Vertex1[A]] = \ a AgdaStorable -> AgdaStorable    #-}


data Vertex2 (A : Set) : Set where
    mkVertex2 : A → A → Vertex2 A

{-# COMPILE GHC Vertex2 = data(0) Graphics.Rendering.OpenGL.GL.Tensor.Vertex2
    ( Graphics.Rendering.OpenGL.GL.Tensor.Vertex2
    ) #-}

postulate
    Functor[Vertex2]     : Functor Vertex2
    Applicative[Vertex2] : Applicative Vertex2
    Foldable[Vertex2]    : Foldable Vertex2
    Traversable[Vertex2] : Traversable Vertex2
    Bounded[Vertex2[A]]  : ⦃ Bounded A ⦄ → Bounded (Vertex2 A)
    Eq[Vertex2[A]]       : ⦃ Eq A ⦄ → Eq (Vertex2 A)
    Ord[Vertex2[A]]      : ⦃ Ord A ⦄ → Ord (Vertex2 A)
    Read[Vertex2[A]]     : ⦃ Read A ⦄ → Read (Vertex2 A)
    Show[Vertex2[A]]     : ⦃ Show A ⦄ → Show (Vertex2 A)
    Ix[Vertex2[A]]       : ⦃ Ix A ⦄ → Ix (Vertex2 A)
    Storable[Vertex2[A]] : ⦃ Storable A ⦄ → Storable (Vertex2 A)

{-# COMPILE GHC Functor[Vertex2]     =                     AgdaFunctor     #-}
{-# COMPILE GHC Applicative[Vertex2] =                     AgdaApplicative #-}
{-# COMPILE GHC Foldable[Vertex2]    =                     AgdaFoldable    #-}
{-# COMPILE GHC Traversable[Vertex2] =                     AgdaTraversable #-}
{-# COMPILE GHC Bounded[Vertex2[A]]  = \ a AgdaBounded  -> AgdaBounded     #-}
{-# COMPILE GHC Eq[Vertex2[A]]       = \ a AgdaEq       -> AgdaEq          #-}
{-# COMPILE GHC Ord[Vertex2[A]]      = \ a AgdaOrd      -> AgdaOrd         #-}
{-# COMPILE GHC Read[Vertex2[A]]     = \ a AgdaRead     -> AgdaRead        #-}
{-# COMPILE GHC Show[Vertex2[A]]     = \ a AgdaShow     -> AgdaShow        #-}
{-# COMPILE GHC Ix[Vertex2[A]]       = \ a AgdaIx       -> AgdaIx          #-}
{-# COMPILE GHC Storable[Vertex2[A]] = \ a AgdaStorable -> AgdaStorable    #-}


data Vertex3 (A : Set) : Set where
    mkVertex3 : A → A → A → Vertex3 A

{-# COMPILE GHC Vertex3 = data(0) Graphics.Rendering.OpenGL.GL.Tensor.Vertex3
    ( Graphics.Rendering.OpenGL.GL.Tensor.Vertex3
    ) #-}

postulate
    Functor[Vertex3]     : Functor Vertex3
    Applicative[Vertex3] : Applicative Vertex3
    Foldable[Vertex3]    : Foldable Vertex3
    Traversable[Vertex3] : Traversable Vertex3
    Bounded[Vertex3[A]]  : ⦃ Bounded A ⦄ → Bounded (Vertex3 A)
    Eq[Vertex3[A]]       : ⦃ Eq A ⦄ → Eq (Vertex3 A)
    Ord[Vertex3[A]]      : ⦃ Ord A ⦄ → Ord (Vertex3 A)
    Read[Vertex3[A]]     : ⦃ Read A ⦄ → Read (Vertex3 A)
    Show[Vertex3[A]]     : ⦃ Show A ⦄ → Show (Vertex3 A)
    Ix[Vertex3[A]]       : ⦃ Ix A ⦄ → Ix (Vertex3 A)
    Storable[Vertex3[A]] : ⦃ Storable A ⦄ → Storable (Vertex3 A)

{-# COMPILE GHC Functor[Vertex3]     =                     AgdaFunctor     #-}
{-# COMPILE GHC Applicative[Vertex3] =                     AgdaApplicative #-}
{-# COMPILE GHC Foldable[Vertex3]    =                     AgdaFoldable    #-}
{-# COMPILE GHC Traversable[Vertex3] =                     AgdaTraversable #-}
{-# COMPILE GHC Bounded[Vertex3[A]]  = \ a AgdaBounded  -> AgdaBounded     #-}
{-# COMPILE GHC Eq[Vertex3[A]]       = \ a AgdaEq       -> AgdaEq          #-}
{-# COMPILE GHC Ord[Vertex3[A]]      = \ a AgdaOrd      -> AgdaOrd         #-}
{-# COMPILE GHC Read[Vertex3[A]]     = \ a AgdaRead     -> AgdaRead        #-}
{-# COMPILE GHC Show[Vertex3[A]]     = \ a AgdaShow     -> AgdaShow        #-}
{-# COMPILE GHC Ix[Vertex3[A]]       = \ a AgdaIx       -> AgdaIx          #-}
{-# COMPILE GHC Storable[Vertex3[A]] = \ a AgdaStorable -> AgdaStorable    #-}


data Vertex4 (A : Set) : Set where
    mkVertex4 : A → A → A → A → Vertex4 A

{-# COMPILE GHC Vertex4 = data(0) Graphics.Rendering.OpenGL.GL.Tensor.Vertex4
    ( Graphics.Rendering.OpenGL.GL.Tensor.Vertex4
    ) #-}

postulate
    Functor[Vertex4]     : Functor Vertex4
    Applicative[Vertex4] : Applicative Vertex4
    Foldable[Vertex4]    : Foldable Vertex4
    Traversable[Vertex4] : Traversable Vertex4
    Bounded[Vertex4[A]]  : ⦃ Bounded A ⦄ → Bounded (Vertex4 A)
    Eq[Vertex4[A]]       : ⦃ Eq A ⦄ → Eq (Vertex4 A)
    Ord[Vertex4[A]]      : ⦃ Ord A ⦄ → Ord (Vertex4 A)
    Read[Vertex4[A]]     : ⦃ Read A ⦄ → Read (Vertex4 A)
    Show[Vertex4[A]]     : ⦃ Show A ⦄ → Show (Vertex4 A)
    Ix[Vertex4[A]]       : ⦃ Ix A ⦄ → Ix (Vertex4 A)
    Storable[Vertex4[A]] : ⦃ Storable A ⦄ → Storable (Vertex4 A)

{-# COMPILE GHC Functor[Vertex4]     =                     AgdaFunctor     #-}
{-# COMPILE GHC Applicative[Vertex4] =                     AgdaApplicative #-}
{-# COMPILE GHC Foldable[Vertex4]    =                     AgdaFoldable    #-}
{-# COMPILE GHC Traversable[Vertex4] =                     AgdaTraversable #-}
{-# COMPILE GHC Bounded[Vertex4[A]]  = \ a AgdaBounded  -> AgdaBounded     #-}
{-# COMPILE GHC Eq[Vertex4[A]]       = \ a AgdaEq       -> AgdaEq          #-}
{-# COMPILE GHC Ord[Vertex4[A]]      = \ a AgdaOrd      -> AgdaOrd         #-}
{-# COMPILE GHC Read[Vertex4[A]]     = \ a AgdaRead     -> AgdaRead        #-}
{-# COMPILE GHC Show[Vertex4[A]]     = \ a AgdaShow     -> AgdaShow        #-}
{-# COMPILE GHC Ix[Vertex4[A]]       = \ a AgdaIx       -> AgdaIx          #-}
{-# COMPILE GHC Storable[Vertex4[A]] = \ a AgdaStorable -> AgdaStorable    #-}


data Vector1 (A : Set) : Set where
    mkVector1 : A → Vector1 A

{-# COMPILE GHC Vector1 = data(0) Graphics.Rendering.OpenGL.GL.Tensor.Vector1
    ( Graphics.Rendering.OpenGL.GL.Tensor.Vector1
    ) #-}

postulate
    Functor[Vector1]     : Functor Vector1
    Applicative[Vector1] : Applicative Vector1
    Foldable[Vector1]    : Foldable Vector1
    Traversable[Vector1] : Traversable Vector1
    Bounded[Vector1[A]]  : ⦃ Bounded A ⦄ → Bounded (Vector1 A)
    Eq[Vector1[A]]       : ⦃ Eq A ⦄ → Eq (Vector1 A)
    Ord[Vector1[A]]      : ⦃ Ord A ⦄ → Ord (Vector1 A)
    Read[Vector1[A]]     : ⦃ Read A ⦄ → Read (Vector1 A)
    Show[Vector1[A]]     : ⦃ Show A ⦄ → Show (Vector1 A)
    Ix[Vector1[A]]       : ⦃ Ix A ⦄ → Ix (Vector1 A)
    Storable[Vector1[A]] : ⦃ Storable A ⦄ → Storable (Vector1 A)

{-# COMPILE GHC Functor[Vector1]     =                     AgdaFunctor     #-}
{-# COMPILE GHC Applicative[Vector1] =                     AgdaApplicative #-}
{-# COMPILE GHC Foldable[Vector1]    =                     AgdaFoldable    #-}
{-# COMPILE GHC Traversable[Vector1] =                     AgdaTraversable #-}
{-# COMPILE GHC Bounded[Vector1[A]]  = \ a AgdaBounded  -> AgdaBounded     #-}
{-# COMPILE GHC Eq[Vector1[A]]       = \ a AgdaEq       -> AgdaEq          #-}
{-# COMPILE GHC Ord[Vector1[A]]      = \ a AgdaOrd      -> AgdaOrd         #-}
{-# COMPILE GHC Read[Vector1[A]]     = \ a AgdaRead     -> AgdaRead        #-}
{-# COMPILE GHC Show[Vector1[A]]     = \ a AgdaShow     -> AgdaShow        #-}
{-# COMPILE GHC Ix[Vector1[A]]       = \ a AgdaIx       -> AgdaIx          #-}
{-# COMPILE GHC Storable[Vector1[A]] = \ a AgdaStorable -> AgdaStorable    #-}


data Vector2 (A : Set) : Set where
    mkVector2 : A → A → Vector2 A

{-# COMPILE GHC Vector2 = data(0) Graphics.Rendering.OpenGL.GL.Tensor.Vector2
    ( Graphics.Rendering.OpenGL.GL.Tensor.Vector2
    ) #-}

postulate
    Functor[Vector2]     : Functor Vector2
    Applicative[Vector2] : Applicative Vector2
    Foldable[Vector2]    : Foldable Vector2
    Traversable[Vector2] : Traversable Vector2
    Bounded[Vector2[A]]  : ⦃ Bounded A ⦄ → Bounded (Vector2 A)
    Eq[Vector2[A]]       : ⦃ Eq A ⦄ → Eq (Vector2 A)
    Ord[Vector2[A]]      : ⦃ Ord A ⦄ → Ord (Vector2 A)
    Read[Vector2[A]]     : ⦃ Read A ⦄ → Read (Vector2 A)
    Show[Vector2[A]]     : ⦃ Show A ⦄ → Show (Vector2 A)
    Ix[Vector2[A]]       : ⦃ Ix A ⦄ → Ix (Vector2 A)
    Storable[Vector2[A]] : ⦃ Storable A ⦄ → Storable (Vector2 A)

{-# COMPILE GHC Functor[Vector2]     =                     AgdaFunctor     #-}
{-# COMPILE GHC Applicative[Vector2] =                     AgdaApplicative #-}
{-# COMPILE GHC Foldable[Vector2]    =                     AgdaFoldable    #-}
{-# COMPILE GHC Traversable[Vector2] =                     AgdaTraversable #-}
{-# COMPILE GHC Bounded[Vector2[A]]  = \ a AgdaBounded  -> AgdaBounded     #-}
{-# COMPILE GHC Eq[Vector2[A]]       = \ a AgdaEq       -> AgdaEq          #-}
{-# COMPILE GHC Ord[Vector2[A]]      = \ a AgdaOrd      -> AgdaOrd         #-}
{-# COMPILE GHC Read[Vector2[A]]     = \ a AgdaRead     -> AgdaRead        #-}
{-# COMPILE GHC Show[Vector2[A]]     = \ a AgdaShow     -> AgdaShow        #-}
{-# COMPILE GHC Ix[Vector2[A]]       = \ a AgdaIx       -> AgdaIx          #-}
{-# COMPILE GHC Storable[Vector2[A]] = \ a AgdaStorable -> AgdaStorable    #-}


data Vector3 (A : Set) : Set where
    mkVector3 : A → A → A → Vector3 A

{-# COMPILE GHC Vector3 = data(0) Graphics.Rendering.OpenGL.GL.Tensor.Vector3
    ( Graphics.Rendering.OpenGL.GL.Tensor.Vector3
    ) #-}

postulate
    Functor[Vector3]     : Functor Vector3
    Applicative[Vector3] : Applicative Vector3
    Foldable[Vector3]    : Foldable Vector3
    Traversable[Vector3] : Traversable Vector3
    Bounded[Vector3[A]]  : ⦃ Bounded A ⦄ → Bounded (Vector3 A)
    Eq[Vector3[A]]       : ⦃ Eq A ⦄ → Eq (Vector3 A)
    Ord[Vector3[A]]      : ⦃ Ord A ⦄ → Ord (Vector3 A)
    Read[Vector3[A]]     : ⦃ Read A ⦄ → Read (Vector3 A)
    Show[Vector3[A]]     : ⦃ Show A ⦄ → Show (Vector3 A)
    Ix[Vector3[A]]       : ⦃ Ix A ⦄ → Ix (Vector3 A)
    Storable[Vector3[A]] : ⦃ Storable A ⦄ → Storable (Vector3 A)

{-# COMPILE GHC Functor[Vector3]     =                     AgdaFunctor     #-}
{-# COMPILE GHC Applicative[Vector3] =                     AgdaApplicative #-}
{-# COMPILE GHC Foldable[Vector3]    =                     AgdaFoldable    #-}
{-# COMPILE GHC Traversable[Vector3] =                     AgdaTraversable #-}
{-# COMPILE GHC Bounded[Vector3[A]]  = \ a AgdaBounded  -> AgdaBounded     #-}
{-# COMPILE GHC Eq[Vector3[A]]       = \ a AgdaEq       -> AgdaEq          #-}
{-# COMPILE GHC Ord[Vector3[A]]      = \ a AgdaOrd      -> AgdaOrd         #-}
{-# COMPILE GHC Read[Vector3[A]]     = \ a AgdaRead     -> AgdaRead        #-}
{-# COMPILE GHC Show[Vector3[A]]     = \ a AgdaShow     -> AgdaShow        #-}
{-# COMPILE GHC Ix[Vector3[A]]       = \ a AgdaIx       -> AgdaIx          #-}
{-# COMPILE GHC Storable[Vector3[A]] = \ a AgdaStorable -> AgdaStorable    #-}


data Vector4 (A : Set) : Set where
    mkVector4 : A → A → A → A → Vector4 A

{-# COMPILE GHC Vector4 = data(0) Graphics.Rendering.OpenGL.GL.Tensor.Vector4
    ( Graphics.Rendering.OpenGL.GL.Tensor.Vector4
    ) #-}

postulate
    Functor[Vector4]     : Functor Vector4
    Applicative[Vector4] : Applicative Vector4
    Foldable[Vector4]    : Foldable Vector4
    Traversable[Vector4] : Traversable Vector4
    Bounded[Vector4[A]]  : ⦃ Bounded A ⦄ → Bounded (Vector4 A)
    Eq[Vector4[A]]       : ⦃ Eq A ⦄ → Eq (Vector4 A)
    Ord[Vector4[A]]      : ⦃ Ord A ⦄ → Ord (Vector4 A)
    Read[Vector4[A]]     : ⦃ Read A ⦄ → Read (Vector4 A)
    Show[Vector4[A]]     : ⦃ Show A ⦄ → Show (Vector4 A)
    Ix[Vector4[A]]       : ⦃ Ix A ⦄ → Ix (Vector4 A)
    Storable[Vector4[A]] : ⦃ Storable A ⦄ → Storable (Vector4 A)

{-# COMPILE GHC Functor[Vector4]     =                     AgdaFunctor     #-}
{-# COMPILE GHC Applicative[Vector4] =                     AgdaApplicative #-}
{-# COMPILE GHC Foldable[Vector4]    =                     AgdaFoldable    #-}
{-# COMPILE GHC Traversable[Vector4] =                     AgdaTraversable #-}
{-# COMPILE GHC Bounded[Vector4[A]]  = \ a AgdaBounded  -> AgdaBounded     #-}
{-# COMPILE GHC Eq[Vector4[A]]       = \ a AgdaEq       -> AgdaEq          #-}
{-# COMPILE GHC Ord[Vector4[A]]      = \ a AgdaOrd      -> AgdaOrd         #-}
{-# COMPILE GHC Read[Vector4[A]]     = \ a AgdaRead     -> AgdaRead        #-}
{-# COMPILE GHC Show[Vector4[A]]     = \ a AgdaShow     -> AgdaShow        #-}
{-# COMPILE GHC Ix[Vector4[A]]       = \ a AgdaIx       -> AgdaIx          #-}
{-# COMPILE GHC Storable[Vector4[A]] = \ a AgdaStorable -> AgdaStorable    #-}
