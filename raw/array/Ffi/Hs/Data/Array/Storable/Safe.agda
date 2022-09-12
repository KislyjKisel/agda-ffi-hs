{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Array.Storable.Safe where

open import Ffi.Hs.Data.Array.Storable public
    using
    ( StorableArray
    ; MArray[StorableArray,E,IO]
    ; withStorableArray
    ; touchStorableArray
    )

open import Ffi.Hs.Data.Array.MArray.Safe public
