{-# OPTIONS --without-K #-}

module Ffi.Hs.Codec.Picture.Metadata-Instanced where

open import Ffi.Hs.Codec.Picture.Metadata

instance
    inst:Show[Metadatas]      = Show[Metadatas]
    inst:Semigroup[Metadatas] = Semigroup[Metadatas]
    inst:Monoid[Metadatas]    = Monoid[Metadatas]
    inst:NFData[Metadatas]    = NFData[Metadatas]

    inst:Eq[Value]     = Eq[Value]
    inst:Show[Value]   = Show[Value]
    inst:NFData[Value] = NFData[Value]

    inst:Eq[SourceFormat]     = Eq[SourceFormat]
    inst:Show[SourceFormat]   = Show[SourceFormat]
    inst:NFData[SourceFormat] = NFData[SourceFormat]

    inst:Eq[ColorSpace]     = Eq[ColorSpace]
    inst:Show[ColorSpace]   = Show[ColorSpace]
    inst:NFData[ColorSpace] = NFData[ColorSpace]

    inst:Eq[Keys[A]]   = Eq[Keys[A]]
    inst:Show[Keys[A]] = Show[Keys[A]]

    inst:Show[Elem[Keys]]   = Show[Elem[Keys]]
    inst:NFData[Elem[Keys]] = NFData[Elem[Keys]]
