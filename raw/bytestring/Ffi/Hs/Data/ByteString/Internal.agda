{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.ByteString.Internal where

open import Agda.Builtin.Bool         using (Bool)
open import Agda.Builtin.Char         using (Char)
open import Agda.Builtin.IO           using (IO)
open import Agda.Builtin.List         using (List)
open import Agda.Primitive            using (Level)
open import Ffi.Hs.-base.Class
open import Ffi.Hs.-base.Unit         using (⊤)
open import Ffi.Hs.Control.DeepSeq    using (NFData)
open import Ffi.Hs.Data.Int           using (Int)
open import Ffi.Hs.Data.Tuple         using (Tuple2; Tuple3)
open import Ffi.Hs.Data.Word          using (Word8)
open import Ffi.Hs.Foreign.C.String   using (CString)
open import Ffi.Hs.Foreign.C.Types    using (CSize; CInt)
open import Ffi.Hs.Foreign.ForeignPtr using (ForeignPtr)
open import Ffi.Hs.Foreign.Ptr        using (Ptr; FunPtr)
open import Ffi.Hs.GHC.Exts           using (Addr#)
open import Ffi.Hs.GHC.IsList         using (IsList)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Data.ByteString.Internal
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
import MAlonzo.Code.Ffi.Hs.GHC.IsList (AgdaIsList(AgdaIsList))
import MAlonzo.Code.Ffi.Hs.Control.DeepSeq (AgdaNFData(AgdaNFData))
#-}

private
    variable
        aℓ : Level
        A B : Set aℓ

data ByteString : Set where
    BS : ForeignPtr Word8 → Int → ByteString

{-# COMPILE GHC ByteString = data Data.ByteString.Internal.ByteString (Data.ByteString.Internal.BS) #-}

postulate
    IsList[ByteString]    : IsList ByteString
    Eq[ByteString]        : Eq ByteString
    Data[ByteString]      : Data ByteString
    Ord[ByteString]       : Ord ByteString
    Read[ByteString]      : Read ByteString
    Show[ByteString]      : Show ByteString
    IsString[ByteString]  : IsString ByteString
    Semigroup[ByteString] : Semigroup ByteString
    Monoid[ByteString]    : Monoid ByteString
    NFData[ByteString]    : NFData ByteString

{-# COMPILE GHC IsList[ByteString]    = AgdaIsList    #-}
{-# COMPILE GHC Eq[ByteString]        = AgdaEq        #-}
{-# COMPILE GHC Data[ByteString]      = AgdaData      #-}
{-# COMPILE GHC Ord[ByteString]       = AgdaOrd       #-}
{-# COMPILE GHC Read[ByteString]      = AgdaRead      #-}
{-# COMPILE GHC Show[ByteString]      = AgdaShow      #-}
{-# COMPILE GHC IsString[ByteString]  = AgdaIsString  #-}
{-# COMPILE GHC Semigroup[ByteString] = AgdaSemigroup #-}
{-# COMPILE GHC Monoid[ByteString]    = AgdaMonoid    #-}
{-# COMPILE GHC NFData[ByteString]    = AgdaNFData    #-}

StrictByteString : Set
StrictByteString = ByteString

postulate
    findIndexOrLength : (Word8 → Bool) → ByteString → Int

    packBytes               : List Word8 → ByteString
    packUptoLenBytes        : Int → List Word8 → Tuple2 ByteString (List Word8)
    unsafePackLenBytes      : Int → List Word8 → ByteString
    packChars               : List Char → ByteString
    packUptoLenChars        : Int → List Char → Tuple2 ByteString (List Char)
    unsafePackLenChars      : Int → List Char → ByteString
    unpackBytes             : ByteString → List Word8
    unpackAppendBytesLazy   : ByteString → List Word8 → List Word8
    unpackAppendBytesStrict : ByteString → List Word8 → List Word8
    unpackChars             : ByteString → List Char
    unpackAppendCharsLazy   : ByteString → List Char → List Char
    unpackAppendCharsStrict : ByteString → List Char → List Char
    unsafePackAddress       : Addr# → IO ByteString
    unsafePackLenAddress    : Int → Addr# → IO ByteString
    unsafePackLiteral       : Addr# → ByteString
    unsafePackLenLiteral    : Int → Addr# → ByteString

    empty              : ByteString
    create             : Int → (Ptr Word8 → IO ⊤) → IO ByteString
    createUptoN        : Int → (Ptr Word8 → IO Int) → IO ByteString
    createUptoN'       : Int → (Ptr Word8 → IO (Tuple2 Int A)) → IO (Tuple2 ByteString A)
    createAndTrim      : Int → (Ptr Word8 → IO Int) → IO ByteString
    createAndTrim'     : Int → (Ptr Word8 → IO (Tuple3 Int Int A)) → IO (Tuple2 ByteString A)
    unsafeCreate       : Int → (Ptr Word8 → IO ⊤) → ByteString
    unsafeCreateUptoN  : Int → (Ptr Word8 → IO Int) → ByteString
    unsafeCreateUptoN' : Int → (Ptr Word8 → IO (Tuple2 Int A)) → Tuple2 ByteString A
    mallocByteString   : Int → IO (ForeignPtr A)

    fromForeignPtr  : ForeignPtr Word8 → Int → Int → ByteString
    toForeignPtr    : ByteString → Tuple3 (ForeignPtr Word8) Int Int
    fromForeignPtr0 : ForeignPtr Word8 → Int → ByteString
    toForeignPtr0   : ByteString → Tuple2 (ForeignPtr Word8) Int

    nullForeignPtr : ForeignPtr Word8
    checkedAdd     : List Char → Int → Int → Int

    c-strlen         : CString → IO CSize
    c-free-finalizer : FunPtr (Ptr Word8 → IO ⊤)
    memchr           : Ptr Word8 → Word8 → CSize → IO (Ptr Word8)
    memcmp           : Ptr Word8 → Ptr Word8 → Int → IO CInt
    memcpy           : Ptr Word8 → Ptr Word8 → Int → IO ⊤
    memset           : Ptr Word8 → Word8 → CSize → IO (Ptr Word8)

    c-reverse     : Ptr Word8 → Ptr Word8 → CSize → IO ⊤
    c-intersperse : Ptr Word8 → Ptr Word8 → CSize → Word8 → IO ⊤
    c-maximum     : Ptr Word8 → CSize → IO Word8
    c-minimum     : Ptr Word8 → CSize → IO Word8
    c-count       : Ptr Word8 → CSize → Word8 → IO CSize
    c-sort        : Ptr Word8 → CSize → IO ⊤

    w2c          : Word8 → Char
    c2w          : Char → Word8
    isSpaceWord8 : Word8 → Bool
    isSpaceChar8 : Char → Bool

    plusForeignPtr       : ForeignPtr A → Int → ForeignPtr B
    unsafeWithForeignPtr : ForeignPtr A → (Ptr A → IO B) → IO B


{-# COMPILE GHC findIndexOrLength = Data.ByteString.Internal.findIndexOrLength #-}

{-# COMPILE GHC packBytes               = Data.ByteString.Internal.packBytes               #-}
{-# COMPILE GHC packUptoLenBytes        = Data.ByteString.Internal.packUptoLenBytes        #-}
{-# COMPILE GHC unsafePackLenBytes      = Data.ByteString.Internal.unsafePackLenBytes      #-}
{-# COMPILE GHC packChars               = Data.ByteString.Internal.packChars               #-}
{-# COMPILE GHC packUptoLenChars        = Data.ByteString.Internal.packUptoLenChars        #-}
{-# COMPILE GHC unsafePackLenChars      = Data.ByteString.Internal.unsafePackLenChars      #-}
{-# COMPILE GHC unpackBytes             = Data.ByteString.Internal.unpackBytes             #-}
{-# COMPILE GHC unpackAppendBytesLazy   = Data.ByteString.Internal.unpackAppendBytesLazy   #-}
{-# COMPILE GHC unpackAppendBytesStrict = Data.ByteString.Internal.unpackAppendBytesStrict #-}
{-# COMPILE GHC unpackChars             = Data.ByteString.Internal.unpackChars             #-}
{-# COMPILE GHC unpackAppendCharsLazy   = Data.ByteString.Internal.unpackAppendCharsLazy   #-}
{-# COMPILE GHC unpackAppendCharsStrict = Data.ByteString.Internal.unpackAppendCharsStrict #-}
{-# COMPILE GHC unsafePackAddress       = Data.ByteString.Internal.unsafePackAddress       #-}
{-# COMPILE GHC unsafePackLenAddress    = Data.ByteString.Internal.unsafePackLenAddress    #-}
{-# COMPILE GHC unsafePackLiteral       = Data.ByteString.Internal.unsafePackLiteral       #-}
{-# COMPILE GHC unsafePackLenLiteral    = Data.ByteString.Internal.unsafePackLenLiteral    #-}

{-# COMPILE GHC empty              =           Data.ByteString.Internal.empty              #-}
{-# COMPILE GHC create             =           Data.ByteString.Internal.create             #-}
{-# COMPILE GHC createUptoN        =           Data.ByteString.Internal.createUptoN        #-}
{-# COMPILE GHC createUptoN'       = \ aℓ a -> Data.ByteString.Internal.createUptoN'       #-}
{-# COMPILE GHC createAndTrim      =           Data.ByteString.Internal.createAndTrim      #-}
{-# COMPILE GHC createAndTrim'     = \ aℓ a -> Data.ByteString.Internal.createAndTrim'     #-}
{-# COMPILE GHC unsafeCreate       =           Data.ByteString.Internal.unsafeCreate       #-}
{-# COMPILE GHC unsafeCreateUptoN  =           Data.ByteString.Internal.unsafeCreateUptoN  #-}
{-# COMPILE GHC unsafeCreateUptoN' = \ aℓ a -> Data.ByteString.Internal.unsafeCreateUptoN' #-}
{-# COMPILE GHC mallocByteString   = \ aℓ a -> Data.ByteString.Internal.mallocByteString   #-}

{-# COMPILE GHC fromForeignPtr  = Data.ByteString.Internal.fromForeignPtr  #-}
{-# COMPILE GHC toForeignPtr    = Data.ByteString.Internal.toForeignPtr    #-}
{-# COMPILE GHC fromForeignPtr0 = Data.ByteString.Internal.fromForeignPtr0 #-}
{-# COMPILE GHC toForeignPtr0   = Data.ByteString.Internal.toForeignPtr0   #-}

{-# COMPILE GHC nullForeignPtr = Data.ByteString.Internal.nullForeignPtr #-}
{-# COMPILE GHC checkedAdd     = Data.ByteString.Internal.checkedAdd     #-}

{-# COMPILE GHC c-strlen         = Data.ByteString.Internal.c_strlen         #-}
{-# COMPILE GHC c-free-finalizer = Data.ByteString.Internal.c_free_finalizer #-}
{-# COMPILE GHC memchr           = Data.ByteString.Internal.memchr           #-}
{-# COMPILE GHC memcmp           = Data.ByteString.Internal.memcmp           #-}
{-# COMPILE GHC memcpy           = Data.ByteString.Internal.memcpy           #-}
{-# COMPILE GHC memset           = Data.ByteString.Internal.memset           #-}

{-# COMPILE GHC c-reverse     = Data.ByteString.Internal.c_reverse     #-}
{-# COMPILE GHC c-intersperse = Data.ByteString.Internal.c_intersperse #-}
{-# COMPILE GHC c-maximum     = Data.ByteString.Internal.c_maximum     #-}
{-# COMPILE GHC c-minimum     = Data.ByteString.Internal.c_minimum     #-}
{-# COMPILE GHC c-count       = Data.ByteString.Internal.c_count       #-}
{-# COMPILE GHC c-sort        = Data.ByteString.Internal.c_sort        #-}

{-# COMPILE GHC w2c          = Data.ByteString.Internal.w2c          #-}
{-# COMPILE GHC c2w          = Data.ByteString.Internal.c2w          #-}
{-# COMPILE GHC isSpaceWord8 = Data.ByteString.Internal.isSpaceWord8 #-}
{-# COMPILE GHC isSpaceChar8 = Data.ByteString.Internal.isSpaceChar8 #-}

{-# COMPILE GHC plusForeignPtr       = \ aℓ a bℓ b -> Data.ByteString.Internal.plusForeignPtr       #-}
{-# COMPILE GHC unsafeWithForeignPtr = \ aℓ a bℓ b -> Data.ByteString.Internal.unsafeWithForeignPtr #-}
