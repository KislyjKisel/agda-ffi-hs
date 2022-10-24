{-# OPTIONS --without-K #-}

module Ffi.Hs.Graphics.Rendering.OpenGL.GL.Tensor-Instanced where

open import Ffi.Hs.Graphics.Rendering.OpenGL.GL.Tensor

instance
    inst:Functor[Vertex1]     = Functor[Vertex1]
    inst:Applicative[Vertex1] = Applicative[Vertex1]
    inst:Foldable[Vertex1]    = Foldable[Vertex1]
    inst:Traversable[Vertex1] = Traversable[Vertex1]
    inst:Bounded[Vertex1[A]]  = Bounded[Vertex1[A]]
    inst:Eq[Vertex1[A]]       = Eq[Vertex1[A]]
    inst:Ord[Vertex1[A]]      = Ord[Vertex1[A]]
    inst:Read[Vertex1[A]]     = Read[Vertex1[A]]
    inst:Show[Vertex1[A]]     = Show[Vertex1[A]]
    inst:Ix[Vertex1[A]]       = Ix[Vertex1[A]]
    inst:Storable[Vertex1[A]] = Storable[Vertex1[A]]

    inst:Functor[Vertex2]     = Functor[Vertex2]
    inst:Applicative[Vertex2] = Applicative[Vertex2]
    inst:Foldable[Vertex2]    = Foldable[Vertex2]
    inst:Traversable[Vertex2] = Traversable[Vertex2]
    inst:Bounded[Vertex2[A]]  = Bounded[Vertex2[A]]
    inst:Eq[Vertex2[A]]       = Eq[Vertex2[A]]
    inst:Ord[Vertex2[A]]      = Ord[Vertex2[A]]
    inst:Read[Vertex2[A]]     = Read[Vertex2[A]]
    inst:Show[Vertex2[A]]     = Show[Vertex2[A]]
    inst:Ix[Vertex2[A]]       = Ix[Vertex2[A]]
    inst:Storable[Vertex2[A]] = Storable[Vertex2[A]]

    inst:Functor[Vertex3]     = Functor[Vertex3]
    inst:Applicative[Vertex3] = Applicative[Vertex3]
    inst:Foldable[Vertex3]    = Foldable[Vertex3]
    inst:Traversable[Vertex3] = Traversable[Vertex3]
    inst:Bounded[Vertex3[A]]  = Bounded[Vertex3[A]]
    inst:Eq[Vertex3[A]]       = Eq[Vertex3[A]]
    inst:Ord[Vertex3[A]]      = Ord[Vertex3[A]]
    inst:Read[Vertex3[A]]     = Read[Vertex3[A]]
    inst:Show[Vertex3[A]]     = Show[Vertex3[A]]
    inst:Ix[Vertex3[A]]       = Ix[Vertex3[A]]
    inst:Storable[Vertex3[A]] = Storable[Vertex3[A]]

    inst:Functor[Vertex4]     = Functor[Vertex4]
    inst:Applicative[Vertex4] = Applicative[Vertex4]
    inst:Foldable[Vertex4]    = Foldable[Vertex4]
    inst:Traversable[Vertex4] = Traversable[Vertex4]
    inst:Bounded[Vertex4[A]]  = Bounded[Vertex4[A]]
    inst:Eq[Vertex4[A]]       = Eq[Vertex4[A]]
    inst:Ord[Vertex4[A]]      = Ord[Vertex4[A]]
    inst:Read[Vertex4[A]]     = Read[Vertex4[A]]
    inst:Show[Vertex4[A]]     = Show[Vertex4[A]]
    inst:Ix[Vertex4[A]]       = Ix[Vertex4[A]]
    inst:Storable[Vertex4[A]] = Storable[Vertex4[A]]

    inst:Functor[Vector1]     = Functor[Vector1]
    inst:Applicative[Vector1] = Applicative[Vector1]
    inst:Foldable[Vector1]    = Foldable[Vector1]
    inst:Traversable[Vector1] = Traversable[Vector1]
    inst:Bounded[Vector1[A]]  = Bounded[Vector1[A]]
    inst:Eq[Vector1[A]]       = Eq[Vector1[A]]
    inst:Ord[Vector1[A]]      = Ord[Vector1[A]]
    inst:Read[Vector1[A]]     = Read[Vector1[A]]
    inst:Show[Vector1[A]]     = Show[Vector1[A]]
    inst:Ix[Vector1[A]]       = Ix[Vector1[A]]
    inst:Storable[Vector1[A]] = Storable[Vector1[A]]

    inst:Functor[Vector2]     = Functor[Vector2]
    inst:Applicative[Vector2] = Applicative[Vector2]
    inst:Foldable[Vector2]    = Foldable[Vector2]
    inst:Traversable[Vector2] = Traversable[Vector2]
    inst:Bounded[Vector2[A]]  = Bounded[Vector2[A]]
    inst:Eq[Vector2[A]]       = Eq[Vector2[A]]
    inst:Ord[Vector2[A]]      = Ord[Vector2[A]]
    inst:Read[Vector2[A]]     = Read[Vector2[A]]
    inst:Show[Vector2[A]]     = Show[Vector2[A]]
    inst:Ix[Vector2[A]]       = Ix[Vector2[A]]
    inst:Storable[Vector2[A]] = Storable[Vector2[A]]

    inst:Functor[Vector3]     = Functor[Vector3]
    inst:Applicative[Vector3] = Applicative[Vector3]
    inst:Foldable[Vector3]    = Foldable[Vector3]
    inst:Traversable[Vector3] = Traversable[Vector3]
    inst:Bounded[Vector3[A]]  = Bounded[Vector3[A]]
    inst:Eq[Vector3[A]]       = Eq[Vector3[A]]
    inst:Ord[Vector3[A]]      = Ord[Vector3[A]]
    inst:Read[Vector3[A]]     = Read[Vector3[A]]
    inst:Show[Vector3[A]]     = Show[Vector3[A]]
    inst:Ix[Vector3[A]]       = Ix[Vector3[A]]
    inst:Storable[Vector3[A]] = Storable[Vector3[A]]

    inst:Functor[Vector4]     = Functor[Vector4]
    inst:Applicative[Vector4] = Applicative[Vector4]
    inst:Foldable[Vector4]    = Foldable[Vector4]
    inst:Traversable[Vector4] = Traversable[Vector4]
    inst:Bounded[Vector4[A]]  = Bounded[Vector4[A]]
    inst:Eq[Vector4[A]]       = Eq[Vector4[A]]
    inst:Ord[Vector4[A]]      = Ord[Vector4[A]]
    inst:Read[Vector4[A]]     = Read[Vector4[A]]
    inst:Show[Vector4[A]]     = Show[Vector4[A]]
    inst:Ix[Vector4[A]]       = Ix[Vector4[A]]
    inst:Storable[Vector4[A]] = Storable[Vector4[A]]
