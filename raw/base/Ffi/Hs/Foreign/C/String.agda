{-# OPTIONS --without-K #-}

module Ffi.Hs.Foreign.C.String where

open import Agda.Builtin.Bool      using (Bool)
open import Agda.Builtin.Char      using (Char)
open import Agda.Builtin.IO        using (IO)
open import Agda.Builtin.List      using (List)
open import Agda.Primitive
open import Ffi.Hs.Data.Int        using (Int)
open import Ffi.Hs.Data.Tuple      using (Tuple2)
open import Ffi.Hs.Foreign.C.Types using (CChar; CUChar; CSChar; CWchar)
open import Ffi.Hs.Foreign.Ptr     using (Ptr)

{-# FOREIGN GHC
import qualified Foreign.C.String
#-}

private
    variable
        aℓ : Level
        A : Set aℓ

CString : Set
CString = Ptr CChar

CStringLen : Set
CStringLen = Tuple2 (Ptr CChar) Int

postulate
    peekCString         : CString → IO (List Char)
    peekCStringLen      : CStringLen → IO (List Char)
    newCString          : List Char → IO CString
    newCStringLen       : List Char → IO CStringLen
    withCString         : List Char → (CString → IO A) → IO A
    withCStringLen      : List Char → (CStringLen → IO A) → IO A
    charIsRepresentable : Char → IO Bool

{-# COMPILE GHC peekCString         =           Foreign.C.String.peekCString         #-}
{-# COMPILE GHC peekCStringLen      =           Foreign.C.String.peekCStringLen      #-}
{-# COMPILE GHC newCString          =           Foreign.C.String.newCString          #-}
{-# COMPILE GHC newCStringLen       =           Foreign.C.String.newCStringLen       #-}
{-# COMPILE GHC withCString         = \ aℓ a -> Foreign.C.String.withCString         #-}
{-# COMPILE GHC withCStringLen      = \ aℓ a -> Foreign.C.String.withCStringLen      #-}
{-# COMPILE GHC charIsRepresentable =           Foreign.C.String.charIsRepresentable #-}

postulate
    castCharToCChar  : Char → CChar
    castCCharToChar  : CChar → Char
    castCharToCUChar : Char → CUChar
    castCUCharToChar : CUChar → Char
    castCharToCSChar : Char → CSChar
    castCSCharToChar : CSChar → Char
    peekCAString     : CString → IO (List Char)
    peekCAStringLen  : CStringLen → IO (List Char)
    newCAString      : List Char → IO CString
    newCAStringLen   : List Char → IO CStringLen
    withCAString     : List Char → (CString → IO A) → IO A
    withCAStringLen  : List Char → (CStringLen → IO A) → IO A

{-# COMPILE GHC castCharToCChar  =           Foreign.C.String.castCharToCChar  #-}
{-# COMPILE GHC castCCharToChar  =           Foreign.C.String.castCCharToChar  #-}
{-# COMPILE GHC castCharToCUChar =           Foreign.C.String.castCharToCUChar #-}
{-# COMPILE GHC castCUCharToChar =           Foreign.C.String.castCUCharToChar #-}
{-# COMPILE GHC castCharToCSChar =           Foreign.C.String.castCharToCSChar #-}
{-# COMPILE GHC castCSCharToChar =           Foreign.C.String.castCSCharToChar #-}
{-# COMPILE GHC peekCAString     =           Foreign.C.String.peekCAString     #-}
{-# COMPILE GHC peekCAStringLen  =           Foreign.C.String.peekCAStringLen  #-}
{-# COMPILE GHC newCAString      =           Foreign.C.String.newCAString      #-}
{-# COMPILE GHC newCAStringLen   =           Foreign.C.String.newCAStringLen   #-}
{-# COMPILE GHC withCAString     = \ aℓ a -> Foreign.C.String.withCAString     #-}
{-# COMPILE GHC withCAStringLen  = \ aℓ a -> Foreign.C.String.withCAStringLen  #-}

CWString : Set
CWString = Ptr CWchar

CWStringLen : Set
CWStringLen = Tuple2 (Ptr CWchar) Int

postulate
    peekCWString    : CWString → IO (List Char)
    peekCWStringLen : CWStringLen → IO (List Char)
    newCWString     : List Char → IO CWString
    newCWStringLen  : List Char → IO CWStringLen
    withCWString    : List Char → (CWString → IO A) → IO A
    withCWStringLen : List Char → (CWStringLen → IO A) → IO A

{-# COMPILE GHC peekCWString    =           Foreign.C.String.peekCWString    #-}
{-# COMPILE GHC peekCWStringLen =           Foreign.C.String.peekCWStringLen #-}
{-# COMPILE GHC newCWString     =           Foreign.C.String.newCWString     #-}
{-# COMPILE GHC newCWStringLen  =           Foreign.C.String.newCWStringLen  #-}
{-# COMPILE GHC withCWString    = \ aℓ a -> Foreign.C.String.withCWString    #-}
{-# COMPILE GHC withCWStringLen = \ aℓ a -> Foreign.C.String.withCWStringLen #-}
