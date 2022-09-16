{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Vector.Storable where

postulate
    Vector : Set aℓ → Set aℓ

    -- todo: instances

data MVector (S : Set)