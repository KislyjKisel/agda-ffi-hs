{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Array.ST.Safe where

open import Ffi.Hs.Data.Array.MArray.Safe public

open import Ffi.Hs.Data.Array.ST public
    using
    ( STArray
    ; runSTArray
    ; MArray[STArray[S],E,SST[S]]
    ; MArray[STArray[S],E,LST[S]]
    ; Eq[STArray[S,I,E]]
    ; STUArray
    ; runSTUArray
    ; MArray[STUArray[S],Int,ST[S]]
    ; MArray[STUArray[S],Int8,ST[S]]
    ; MArray[STUArray[S],Int16,ST[S]]
    ; MArray[STUArray[S],Int32,ST[S]]
    ; MArray[STUArray[S],Int64,ST[S]]
    ; MArray[STUArray[S],Word,ST[S]]
    ; MArray[STUArray[S],Word8,ST[S]]
    ; MArray[STUArray[S],Word16,ST[S]]
    ; MArray[STUArray[S],Word32,ST[S]]
    ; MArray[STUArray[S],Word64,ST[S]]
    ; MArray[STUArray[S],Bool,ST[S]]
    ; MArray[STUArray[S],Char,ST[S]]
    ; MArray[STUArray[S],Float,ST[S]]
    ; MArray[STUArray[S],Double,ST[S]]
    ; MArray[STUArray[S],StablePtr[A],ST[S]]
    ; MArray[STUArray[S],FunPtr[A],ST[S]]
    ; MArray[STUArray[S],Ptr[A],ST[S]]
    ; Eq[STUArray[S,I,E]]
    )
