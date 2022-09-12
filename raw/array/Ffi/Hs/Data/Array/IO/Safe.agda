{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Array.IO.Safe where

open import Ffi.Hs.Data.Array.MArray.Safe public

open import Ffi.Hs.Data.Array.IO public
    using
    ( IOArray
    ; MArray[IOArray,E,IO]
    ; Eq[IOArray[I,E]]
    ; IOUArray
    ; MArray[IOUArray,Int,IO]
    ; MArray[IOUArray,Int8,IO]
    ; MArray[IOUArray,Int16,IO]
    ; MArray[IOUArray,Int32,IO]
    ; MArray[IOUArray,Int64,IO]
    ; MArray[IOUArray,Word,IO]
    ; MArray[IOUArray,Word8,IO]
    ; MArray[IOUArray,Word16,IO]
    ; MArray[IOUArray,Word32,IO]
    ; MArray[IOUArray,Word64,IO]
    ; MArray[IOUArray,Bool,IO]
    ; MArray[IOUArray,Char,IO]
    ; MArray[IOUArray,Float,IO]
    ; MArray[IOUArray,Double,IO]
    ; MArray[IOUArray,Ptr[A],IO]
    ; MArray[IOUArray,FunPtr[A],IO]
    ; MArray[IOUArray,StablePtr[A],IO]
    ; Eq[IOUArray[I,E]]
    ; hGetArray
    ; hPutArray
    )
