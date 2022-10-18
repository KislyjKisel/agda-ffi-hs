{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.ByteString.Short.Internal where

open import Agda.Builtin.Bool       using (Bool)
open import Agda.Builtin.Equality   using (_≡_)
open import Agda.Builtin.IO         using (IO)
open import Agda.Builtin.List       using (List)
open import Agda.Builtin.Maybe      using (Maybe)
open import Agda.Builtin.Unit       using (⊤)
open import Agda.Primitive          using (Level)
open import Ffi.Hs.-base.Class
open import Ffi.Hs.Control.DeepSeq  using (NFData)
open import Ffi.Hs.Data.ByteString  using (StrictByteString)
open import Ffi.Hs.Data.Int         using (Int)
open import Ffi.Hs.Data.Tuple       using (Tuple2)
open import Ffi.Hs.Data.Word        using (Word8)
open import Ffi.Hs.Foreign.C.String using (CString; CStringLen)
open import Ffi.Hs.Foreign.Ptr      using (Ptr)
open import Ffi.Hs.GHC.Exts         using (ByteArray#)
open import Ffi.Hs.GHC.IsList       using (IsList; Item)
open import Ffi.Hs.GHC.Stack        using (HasCallStack)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Data.ByteString.Short.Internal
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
import MAlonzo.Code.Ffi.Hs.Control.DeepSeq (AgdaNFData (AgdaNFData))
import MAlonzo.Code.Ffi.Hs.GHC.IsList (AgdaIsList (AgdaIsList))
import MAlonzo.Code.Ffi.Hs.GHC.Stack (AgdaHasCallStack (AgdaHasCallStack))
#-}

private
    variable
        aℓ : Level
        A : Set aℓ


data ShortByteString : Set where
    SBS : ByteArray# → ShortByteString

{-# COMPILE GHC ShortByteString = data Data.ByteString.Short.Internal.ShortByteString
    ( Data.ByteString.Short.Internal.SBS
    ) #-}

postulate
    IsList[ShortByteString]    : IsList ShortByteString
    Eq[ShortByteString]        : Eq ShortByteString
    Data[ShortByteString]      : Data ShortByteString
    Ord[ShortByteString]       : Ord ShortByteString
    Read[ShortByteString]      : Read ShortByteString
    Show[ShortByteString]      : Show ShortByteString
    IsString[ShortByteString]  : IsString ShortByteString
    Semigroup[ShortByteString] : Semigroup ShortByteString
    Monoid[ShortByteString]    : Monoid ShortByteString
    NFData[ShortByteString]    : NFData ShortByteString

    Item[ShortByteString] : Item ShortByteString ⦃ IsList[ShortByteString] ⦄ ≡ Word8

{-# COMPILE GHC IsList[ShortByteString]    = AgdaIsList    #-}
{-# COMPILE GHC Eq[ShortByteString]        = AgdaEq        #-}
{-# COMPILE GHC Data[ShortByteString]      = AgdaData      #-}
{-# COMPILE GHC Ord[ShortByteString]       = AgdaOrd       #-}
{-# COMPILE GHC Read[ShortByteString]      = AgdaRead      #-}
{-# COMPILE GHC Show[ShortByteString]      = AgdaShow      #-}
{-# COMPILE GHC IsString[ShortByteString]  = AgdaIsString  #-}
{-# COMPILE GHC Semigroup[ShortByteString] = AgdaSemigroup #-}
{-# COMPILE GHC Monoid[ShortByteString]    = AgdaMonoid    #-}
{-# COMPILE GHC NFData[ShortByteString]    = AgdaNFData    #-}


postulate
    empty           : ShortByteString
    singleton       : Word8 → ShortByteString
    pack            : List Word8 → ShortByteString
    unpack          : ShortByteString → List Word8
    fromShort       : ShortByteString → StrictByteString
    toShort         : StrictByteString → ShortByteString
    snoc            : ShortByteString → Word8 → ShortByteString
    cons            : Word8 → ShortByteString → ShortByteString
    append          : ShortByteString → ShortByteString → ShortByteString
    last            : ⦃ HasCallStack ⦄ → ShortByteString → Word8
    tail            : ⦃ HasCallStack ⦄ → ShortByteString → ShortByteString
    uncons          : ShortByteString → Maybe (Tuple2 Word8 ShortByteString)
    head            : ⦃ HasCallStack ⦄ → ShortByteString → Word8
    init            : ⦃ HasCallStack ⦄ → ShortByteString → ShortByteString
    unsnoc          : ShortByteString → Maybe (Tuple2 ShortByteString Word8)
    null            : ShortByteString → Bool
    length          : ShortByteString → Int
    map             : (Word8 → Word8) → ShortByteString → ShortByteString
    reverse         : ShortByteString → ShortByteString
    intercalate     : ShortByteString → List ShortByteString → ShortByteString
    foldl           : (A → Word8 → A) → A → ShortByteString → A
    foldl'          : (A → Word8 → A) → A → ShortByteString → A
    foldl1          : ⦃ HasCallStack ⦄ → (Word8 → Word8 → Word8) → ShortByteString → Word8
    foldl1'         : ⦃ HasCallStack ⦄ → (Word8 → Word8 → Word8) → ShortByteString → Word8
    foldr           : (Word8 → A → A) → A → ShortByteString → A
    foldr'          : (Word8 → A → A) → A → ShortByteString → A
    foldr1          : ⦃ HasCallStack ⦄ → (Word8 → Word8 → Word8) → ShortByteString → Word8
    foldr1'         : ⦃ HasCallStack ⦄ → (Word8 → Word8 → Word8) → ShortByteString → Word8
    all             : (Word8 → Bool) → ShortByteString → Bool
    any             : (Word8 → Bool) → ShortByteString → Bool
    concat          : List ShortByteString → ShortByteString
    replicate       : Int → Word8 → ShortByteString
    unfoldr         : (A → Maybe (Tuple2 Word8 A)) → A → ShortByteString
    unfoldrN        : Int → (A → Maybe (Tuple2 Word8 A)) → A → Tuple2 ShortByteString (Maybe A)
    take            : Int → ShortByteString → ShortByteString
    takeEnd         : Int → ShortByteString → ShortByteString
    takeWhileEnd    : (Word8 → Bool) → ShortByteString → ShortByteString
    takeWhile       : (Word8 → Bool) → ShortByteString → ShortByteString
    drop            : Int → ShortByteString → ShortByteString
    dropEnd         : Int → ShortByteString → ShortByteString
    dropWhile       : (Word8 → Bool) → ShortByteString → ShortByteString
    dropWhileEnd    : (Word8 → Bool) → ShortByteString → ShortByteString
    breakEnd        : (Word8 → Bool) → ShortByteString → Tuple2 ShortByteString ShortByteString
    break           : (Word8 → Bool) → ShortByteString → Tuple2 ShortByteString ShortByteString
    span            : (Word8 → Bool) → ShortByteString → Tuple2 ShortByteString ShortByteString
    spanEnd         : (Word8 → Bool) → ShortByteString → Tuple2 ShortByteString ShortByteString
    splitAt         : Int → ShortByteString → Tuple2 ShortByteString ShortByteString
    split           : Word8 → ShortByteString → List ShortByteString
    splitWith       : (Word8 → Bool) → ShortByteString → List ShortByteString
    stripSuffix     : ShortByteString → ShortByteString → Maybe ShortByteString
    stripPrefix     : ShortByteString → ShortByteString → Maybe ShortByteString
    isInfixOf       : ShortByteString → ShortByteString → Bool
    isPrefixOf      : ShortByteString → ShortByteString → Bool
    isSuffixOf      : ShortByteString → ShortByteString → Bool
    breakSubstring  : ShortByteString → ShortByteString → Tuple2 ShortByteString ShortByteString
    elem            : Word8 → ShortByteString → Bool
    find            : (Word8 → Bool) → ShortByteString → Maybe Word8
    filter          : (Word8 → Bool) → ShortByteString → ShortByteString
    partition       : (Word8 → Bool) → ShortByteString → Tuple2 ShortByteString ShortByteString
    index           : ⦃ HasCallStack ⦄ → ShortByteString → Int → Word8
    indexMaybe      : ShortByteString → Int → Maybe Word8
    _!?_            : ShortByteString → Int → Maybe Word8
    elemIndex       : Word8 → ShortByteString → Maybe Int
    elemIndices     : Word8 → ShortByteString → List Int
    count           : Word8 → ShortByteString → Int
    findIndex       : (Word8 → Bool) → ShortByteString → Maybe Int
    findIndices     : (Word8 → Bool) → ShortByteString → List Int
    createFromPtr   : Ptr A → Int → IO ShortByteString
    copyToPtr       : ShortByteString → Int → Ptr A → Int → IO ⊤
    isValidUtf8     : ShortByteString → Bool
    packCString     : CString → IO ShortByteString
    packCStringLen  : CStringLen → IO ShortByteString
    useAsCString    : ShortByteString → (CString → IO A) → IO A
    useAsCStringLen : ShortByteString → (CStringLen → IO A) → IO A

{-# COMPILE GHC empty           =                       Data.ByteString.Short.Internal.empty           #-}
{-# COMPILE GHC singleton       =                       Data.ByteString.Short.Internal.singleton       #-}
{-# COMPILE GHC pack            =                       Data.ByteString.Short.Internal.pack            #-}
{-# COMPILE GHC unpack          =                       Data.ByteString.Short.Internal.unpack          #-}
{-# COMPILE GHC fromShort       =                       Data.ByteString.Short.Internal.fromShort       #-}
{-# COMPILE GHC toShort         =                       Data.ByteString.Short.Internal.toShort         #-}
{-# COMPILE GHC snoc            =                       Data.ByteString.Short.Internal.snoc            #-}
{-# COMPILE GHC cons            =                       Data.ByteString.Short.Internal.cons            #-}
{-# COMPILE GHC append          =                       Data.ByteString.Short.Internal.append          #-}
{-# COMPILE GHC last            = \ AgdaHasCallStack -> Data.ByteString.Short.Internal.last            #-}
{-# COMPILE GHC tail            = \ AgdaHasCallStack -> Data.ByteString.Short.Internal.tail            #-}
{-# COMPILE GHC uncons          =                       Data.ByteString.Short.Internal.uncons          #-}
{-# COMPILE GHC head            = \ AgdaHasCallStack -> Data.ByteString.Short.Internal.head            #-}
{-# COMPILE GHC init            = \ AgdaHasCallStack -> Data.ByteString.Short.Internal.init            #-}
{-# COMPILE GHC unsnoc          =                       Data.ByteString.Short.Internal.unsnoc          #-}
{-# COMPILE GHC null            =                       Data.ByteString.Short.Internal.null            #-}
{-# COMPILE GHC length          =                       Data.ByteString.Short.Internal.length          #-}
{-# COMPILE GHC map             =                       Data.ByteString.Short.Internal.map             #-}
{-# COMPILE GHC reverse         =                       Data.ByteString.Short.Internal.reverse         #-}
{-# COMPILE GHC intercalate     =                       Data.ByteString.Short.Internal.intercalate     #-}
{-# COMPILE GHC foldl           = \ aℓ a             -> Data.ByteString.Short.Internal.foldl           #-}
{-# COMPILE GHC foldl'          = \ aℓ a             -> Data.ByteString.Short.Internal.foldl'          #-}
{-# COMPILE GHC foldl1          = \ AgdaHasCallStack -> Data.ByteString.Short.Internal.foldl1          #-}
{-# COMPILE GHC foldl1'         = \ AgdaHasCallStack -> Data.ByteString.Short.Internal.foldl1'         #-}
{-# COMPILE GHC foldr           = \ aℓ a             -> Data.ByteString.Short.Internal.foldr           #-}
{-# COMPILE GHC foldr'          = \ aℓ a             -> Data.ByteString.Short.Internal.foldr'          #-}
{-# COMPILE GHC foldr1          = \ AgdaHasCallStack -> Data.ByteString.Short.Internal.foldr1          #-}
{-# COMPILE GHC foldr1'         = \ AgdaHasCallStack -> Data.ByteString.Short.Internal.foldr1'         #-}
{-# COMPILE GHC all             =                       Data.ByteString.Short.Internal.all             #-}
{-# COMPILE GHC any             =                       Data.ByteString.Short.Internal.any             #-}
{-# COMPILE GHC concat          =                       Data.ByteString.Short.Internal.concat          #-}
{-# COMPILE GHC replicate       =                       Data.ByteString.Short.Internal.replicate       #-}
{-# COMPILE GHC unfoldr         = \ aℓ a             -> Data.ByteString.Short.Internal.unfoldr         #-}
{-# COMPILE GHC unfoldrN        = \ aℓ a             -> Data.ByteString.Short.Internal.unfoldrN        #-}
{-# COMPILE GHC take            =                       Data.ByteString.Short.Internal.take            #-}
{-# COMPILE GHC takeEnd         =                       Data.ByteString.Short.Internal.takeEnd         #-}
{-# COMPILE GHC takeWhileEnd    =                       Data.ByteString.Short.Internal.takeWhileEnd    #-}
{-# COMPILE GHC takeWhile       =                       Data.ByteString.Short.Internal.takeWhile       #-}
{-# COMPILE GHC drop            =                       Data.ByteString.Short.Internal.drop            #-}
{-# COMPILE GHC dropEnd         =                       Data.ByteString.Short.Internal.dropEnd         #-}
{-# COMPILE GHC dropWhile       =                       Data.ByteString.Short.Internal.dropWhile       #-}
{-# COMPILE GHC dropWhileEnd    =                       Data.ByteString.Short.Internal.dropWhileEnd    #-}
{-# COMPILE GHC breakEnd        =                       Data.ByteString.Short.Internal.breakEnd        #-}
{-# COMPILE GHC break           =                       Data.ByteString.Short.Internal.break           #-}
{-# COMPILE GHC span            =                       Data.ByteString.Short.Internal.span            #-}
{-# COMPILE GHC spanEnd         =                       Data.ByteString.Short.Internal.spanEnd         #-}
{-# COMPILE GHC splitAt         =                       Data.ByteString.Short.Internal.splitAt         #-}
{-# COMPILE GHC split           =                       Data.ByteString.Short.Internal.split           #-}
{-# COMPILE GHC splitWith       =                       Data.ByteString.Short.Internal.splitWith       #-}
{-# COMPILE GHC stripSuffix     =                       Data.ByteString.Short.Internal.stripSuffix     #-}
{-# COMPILE GHC stripPrefix     =                       Data.ByteString.Short.Internal.stripPrefix     #-}
{-# COMPILE GHC isInfixOf       =                       Data.ByteString.Short.Internal.isInfixOf       #-}
{-# COMPILE GHC isPrefixOf      =                       Data.ByteString.Short.Internal.isPrefixOf      #-}
{-# COMPILE GHC isSuffixOf      =                       Data.ByteString.Short.Internal.isSuffixOf      #-}
{-# COMPILE GHC breakSubstring  =                       Data.ByteString.Short.Internal.breakSubstring  #-}
{-# COMPILE GHC elem            =                       Data.ByteString.Short.Internal.elem            #-}
{-# COMPILE GHC find            =                       Data.ByteString.Short.Internal.find            #-}
{-# COMPILE GHC filter          =                       Data.ByteString.Short.Internal.filter          #-}
{-# COMPILE GHC partition       =                       Data.ByteString.Short.Internal.partition       #-}
{-# COMPILE GHC index           = \ AgdaHasCallStack -> Data.ByteString.Short.Internal.index           #-}
{-# COMPILE GHC indexMaybe      =                       Data.ByteString.Short.Internal.indexMaybe      #-}
{-# COMPILE GHC _!?_            =                      (Data.ByteString.Short.Internal.!?)             #-}
{-# COMPILE GHC elemIndex       =                       Data.ByteString.Short.Internal.elemIndex       #-}
{-# COMPILE GHC elemIndices     =                       Data.ByteString.Short.Internal.elemIndices     #-}
{-# COMPILE GHC count           =                       Data.ByteString.Short.Internal.count           #-}
{-# COMPILE GHC findIndex       =                       Data.ByteString.Short.Internal.findIndex       #-}
{-# COMPILE GHC findIndices     =                       Data.ByteString.Short.Internal.findIndices     #-}
{-# COMPILE GHC createFromPtr   = \ aℓ a             -> Data.ByteString.Short.Internal.createFromPtr   #-}
{-# COMPILE GHC copyToPtr       = \ aℓ a             -> Data.ByteString.Short.Internal.copyToPtr       #-}
{-# COMPILE GHC isValidUtf8     =                       Data.ByteString.Short.Internal.isValidUtf8     #-}
{-# COMPILE GHC packCString     =                       Data.ByteString.Short.Internal.packCString     #-}
{-# COMPILE GHC packCStringLen  =                       Data.ByteString.Short.Internal.packCStringLen  #-}
{-# COMPILE GHC useAsCString    = \ aℓ a             -> Data.ByteString.Short.Internal.useAsCString    #-}
{-# COMPILE GHC useAsCStringLen = \ aℓ a             -> Data.ByteString.Short.Internal.useAsCStringLen #-}
