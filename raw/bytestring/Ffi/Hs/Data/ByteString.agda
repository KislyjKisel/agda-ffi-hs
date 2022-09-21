{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.ByteString where

open import Agda.Builtin.Bool       using (Bool)
open import Agda.Builtin.List       using (List)
open import Agda.Builtin.Maybe      using (Maybe)
open import Agda.Primitive
open import Ffi.Hs.-base.Unit       using (⊤; ⊤′)
open import Ffi.Hs.Data.Int         using (Int)
open import Ffi.Hs.Data.Tuple       using (Tuple2)
open import Ffi.Hs.Data.Word        using (Word8)
open import Ffi.Hs.Foreign.C.String using (CString; CStringLen)
open import Ffi.Hs.GHC.Stack        using (HasCallStack)
open import Ffi.Hs.System.IO        using (IO; Handle; FilePath)

private
    variable
        aℓ : Level
        A : Set aℓ

{-# FOREIGN GHC
import qualified Data.ByteString
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
import MAlonzo.Code.Ffi.Hs.GHC.Stack (AgdaHasCallStack(AgdaHasCallStack))
#-}

postulate
    ByteString : Set

{-# COMPILE GHC ByteString = type Data.ByteString.ByteString #-}

StrictByteString : Set
StrictByteString = ByteString

infixl 9 _!?_

postulate
    empty        : ByteString
    singleton    : Word8 → ByteString
    pack         : List Word8 → ByteString
    unpack       : ByteString → List Word8
    -- todo: (req: lazy bytestring) fromStrict   : ByteString → ByteString
    -- todo: (req: lazy bytestring) toStrict     : ByteString → ByteString
    fromFilePath : FilePath → IO ByteString
    toFilePath   : ByteString → IO FilePath

    cons   : Word8 → ByteString → ByteString
    snoc   : ByteString → Word8 → ByteString
    append : ByteString → ByteString → ByteString
    head   : ⦃ HasCallStack ⦄ → ByteString → Word8
    uncons : ByteString → Maybe (Tuple2 Word8 ByteString)
    last   : ⦃ HasCallStack ⦄ → ByteString → Word8
    tail   : ⦃ HasCallStack ⦄ → ByteString → ByteString
    init   : ⦃ HasCallStack ⦄ → ByteString → ByteString
    null   : ByteString → Bool
    length : ByteString → Int

    map         : (Word8 → Word8) → ByteString → ByteString
    reverse     : ByteString → ByteString
    intersperse : Word8 → ByteString → ByteString
    intercalate : ByteString → List ByteString → ByteString
    transpose   : List ByteString → List ByteString

    foldl   : (A → Word8 → A) → A → ByteString → A
    foldl'  : (A → Word8 → A) → A → ByteString → A
    foldl1  : ⦃ HasCallStack ⦄ → (Word8 → Word8 → Word8) → ByteString → Word8
    foldl1' : ⦃ HasCallStack ⦄ → (Word8 → Word8 → Word8) → ByteString → Word8
    foldr   : (Word8 → A → A) → A → ByteString → A
    foldr'  : (Word8 → A → A) → A → ByteString → A
    foldr1  : ⦃ HasCallStack ⦄ → (Word8 → Word8 → Word8) → ByteString → Word8
    foldr1' : ⦃ HasCallStack ⦄ → (Word8 → Word8 → Word8) → ByteString → Word8

    concat    : List ByteString → ByteString
    concatMap : (Word8 → ByteString) → ByteString → ByteString
    any       : (Word8 → Bool) → ByteString → Bool
    all       : (Word8 → Bool) → ByteString → Bool
    maximum   : ⦃ HasCallStack ⦄ → ByteString → Word8
    minimum   : ⦃ HasCallStack ⦄ → ByteString → Word8

    scanl  : (Word8 → Word8 → Word8) → Word8 → ByteString → ByteString
    scanl1 : (Word8 → Word8 → Word8) → ByteString → ByteString
    scanr  : (Word8 → Word8 → Word8) → Word8 → ByteString → ByteString
    scanr1 : (Word8 → Word8 → Word8) → ByteString → ByteString

    mapAccumL : (A → Word8 → Tuple2 A Word8) → A → ByteString → Tuple2 A ByteString
    mapAccumR : (A → Word8 → Tuple2 A Word8) → A → ByteString → Tuple2 A ByteString

    replicate : Int → Word8 → ByteString
    unfoldr   : (A → Maybe (Tuple2 Word8 A)) → A → ByteString
    unfoldrN  : Int → (A → Maybe (Tuple2 Word8 A)) → A → Tuple2 ByteString (Maybe A)

    take         : Int → ByteString → ByteString
    takeEnd      : Int → ByteString → ByteString
    drop         : Int → ByteString → ByteString
    dropEnd      : Int → ByteString → ByteString
    splitAt      : Int → ByteString → Tuple2 ByteString ByteString
    takeWhile    : (Word8 → Bool) → ByteString → ByteString
    takeWhileEnd : (Word8 → Bool) → ByteString → ByteString
    dropWhile    : (Word8 → Bool) → ByteString → ByteString
    dropWhileEnd : (Word8 → Bool) → ByteString → ByteString
    span         : (Word8 → Bool) → ByteString → Tuple2 ByteString ByteString
    spanEnd      : (Word8 → Bool) → ByteString → Tuple2 ByteString ByteString
    break        : (Word8 → Bool) → ByteString → Tuple2 ByteString ByteString
    breakEnd     : (Word8 → Bool) → ByteString → Tuple2 ByteString ByteString
    group        : ByteString → List ByteString
    groupBy      : (Word8 → Word8 → Bool) → ByteString → List ByteString
    inits        : ByteString → List ByteString
    tails        : ByteString → List ByteString
    stripPrefix  : ByteString → ByteString → Maybe ByteString
    stripSuffix  : ByteString → ByteString → Maybe ByteString

    split     : Word8 → ByteString → List ByteString
    splitWith : (Word8 → Bool) → ByteString → List ByteString

    isPrefixOf : ByteString → ByteString → Bool
    isSuffixOf : ByteString → ByteString → Bool
    isInfixOf  : ByteString → ByteString → Bool

    isValidUtf8 : ByteString → Bool

    breakSubstring : ByteString → ByteString → Tuple2 ByteString ByteString

    elem    : Word8 → ByteString → Bool
    notElem : Word8 → ByteString → Bool

    find      : (Word8 → Bool) → ByteString → Maybe Word8
    filter    : (Word8 → Bool) → ByteString → ByteString
    partition : (Word8 → Bool) → ByteString → Tuple2 ByteString ByteString

    index        : ⦃ HasCallStack ⦄ → ByteString → Int → Word8
    indexMaybe   : ByteString → Int → Maybe Word8
    _!?_         : ByteString → Int → Maybe Word8
    elemIndex    : Word8 → ByteString → Maybe Int
    elemIndices  : Word8 → ByteString → List Int
    elemIndexEnd : Word8 → ByteString → Maybe Int
    findIndex    : (Word8 → Bool) → ByteString → Maybe Int
    findIndices  : (Word8 → Bool) → ByteString → List Int
    findIndexEnd : (Word8 → Bool) → ByteString → Maybe Int
    count        : Word8 → ByteString → Int

    zip         : ByteString → ByteString → List (Tuple2 Word8 Word8)
    zipWith     : (Word8 → Word8 → A) → ByteString → ByteString → List A
    packZipWith : (Word8 → Word8 → Word8) → ByteString → ByteString → ByteString
    unzip       : List (Tuple2 Word8 Word8) → Tuple2 ByteString ByteString

    sort : ByteString → ByteString

    copy : ByteString → ByteString

    packCString    : CString → IO ByteString
    packCStringLen : CStringLen → IO ByteString

    useAsCString    : ByteString → (CString → IO A) → IO A
    useAsCStringLen : ByteString → (CStringLen → IO A) → IO A

    getLine     : IO ByteString
    getContents : IO ByteString
    putStr      : ByteString → IO ⊤
    interact    : (ByteString → ByteString) → IO ⊤

    readFile   : FilePath → IO ByteString
    writeFile  : FilePath → ByteString → IO ⊤
    appendFile : FilePath → ByteString → IO ⊤

    hGetLine        : Handle → IO ByteString
    hGetContents    : Handle → IO ByteString
    hGet            : Handle → Int → IO ByteString
    hGetSome        : Handle → Int → IO ByteString
    hGetNonBlocking : Handle → Int → IO ByteString
    hPut            : Handle → ByteString → IO ⊤
    hPutNonBlocking : Handle → ByteString → IO ByteString
    hPutStr         : Handle → ByteString → IO ⊤

{-# COMPILE GHC empty        = Data.ByteString.empty        #-}
{-# COMPILE GHC singleton    = Data.ByteString.singleton    #-}
{-# COMPILE GHC pack         = Data.ByteString.pack         #-}
{-# COMPILE GHC unpack       = Data.ByteString.unpack       #-}
-- {-# COMPILE GHC fromStrict   = Data.ByteString.fromStrict   #-}
-- {-# COMPILE GHC toStrict     = Data.ByteString.toStrict     #-}
{-# COMPILE GHC fromFilePath = Data.ByteString.fromFilePath #-}
{-# COMPILE GHC toFilePath   = Data.ByteString.toFilePath   #-}

{-# COMPILE GHC cons   =                       Data.ByteString.cons   #-}
{-# COMPILE GHC snoc   =                       Data.ByteString.snoc   #-}
{-# COMPILE GHC append =                       Data.ByteString.append #-}
{-# COMPILE GHC head   = \ AgdaHasCallStack -> Data.ByteString.head   #-}
{-# COMPILE GHC uncons =                       Data.ByteString.uncons #-}
{-# COMPILE GHC last   = \ AgdaHasCallStack -> Data.ByteString.last   #-}
{-# COMPILE GHC tail   = \ AgdaHasCallStack -> Data.ByteString.tail   #-}
{-# COMPILE GHC init   = \ AgdaHasCallStack -> Data.ByteString.init   #-}
{-# COMPILE GHC null   =                       Data.ByteString.null   #-}
{-# COMPILE GHC length =                       Data.ByteString.length #-}

{-# COMPILE GHC map         = Data.ByteString.map         #-}
{-# COMPILE GHC reverse     = Data.ByteString.reverse     #-}
{-# COMPILE GHC intersperse = Data.ByteString.intersperse #-}
{-# COMPILE GHC intercalate = Data.ByteString.intercalate #-}
{-# COMPILE GHC transpose   = Data.ByteString.transpose   #-}

{-# COMPILE GHC foldl   = \ aℓ a             -> Data.ByteString.foldl   #-}
{-# COMPILE GHC foldl'  = \ aℓ a             -> Data.ByteString.foldl'  #-}
{-# COMPILE GHC foldl1  = \ AgdaHasCallStack -> Data.ByteString.foldl1  #-}
{-# COMPILE GHC foldl1' = \ AgdaHasCallStack -> Data.ByteString.foldl1' #-}
{-# COMPILE GHC foldr   = \ aℓ a             -> Data.ByteString.foldr   #-}
{-# COMPILE GHC foldr'  = \ aℓ a             -> Data.ByteString.foldr'  #-}
{-# COMPILE GHC foldr1  = \ AgdaHasCallStack -> Data.ByteString.foldr1  #-}
{-# COMPILE GHC foldr1' = \ AgdaHasCallStack -> Data.ByteString.foldr1' #-}

{-# COMPILE GHC concat    = Data.ByteString.concat                        #-}
{-# COMPILE GHC concatMap = Data.ByteString.concatMap                     #-}
{-# COMPILE GHC any       = Data.ByteString.any                           #-}
{-# COMPILE GHC all       = Data.ByteString.all                           #-}
{-# COMPILE GHC maximum   = \ AgdaHasCallStack -> Data.ByteString.maximum #-}
{-# COMPILE GHC minimum   = \ AgdaHasCallStack -> Data.ByteString.minimum #-}

{-# COMPILE GHC scanl  = Data.ByteString.scanl  #-}
{-# COMPILE GHC scanl1 = Data.ByteString.scanl1 #-}
{-# COMPILE GHC scanr  = Data.ByteString.scanr  #-}
{-# COMPILE GHC scanr1 = Data.ByteString.scanr1 #-}

{-# COMPILE GHC mapAccumL = \ aℓ a -> Data.ByteString.mapAccumL #-}
{-# COMPILE GHC mapAccumR = \ aℓ a -> Data.ByteString.mapAccumR #-}

{-# COMPILE GHC replicate =           Data.ByteString.replicate #-}
{-# COMPILE GHC unfoldr   = \ aℓ a -> Data.ByteString.unfoldr   #-}
{-# COMPILE GHC unfoldrN  = \ aℓ a -> Data.ByteString.unfoldrN  #-}

{-# COMPILE GHC take         = Data.ByteString.take #-}
{-# COMPILE GHC takeEnd      = Data.ByteString.takeEnd #-}
{-# COMPILE GHC drop         = Data.ByteString.drop #-}
{-# COMPILE GHC dropEnd      = Data.ByteString.dropEnd #-}
{-# COMPILE GHC splitAt      = Data.ByteString.splitAt #-}
{-# COMPILE GHC takeWhile    = Data.ByteString.takeWhile #-}
{-# COMPILE GHC takeWhileEnd = Data.ByteString.takeWhileEnd #-}
{-# COMPILE GHC dropWhile    = Data.ByteString.dropWhile #-}
{-# COMPILE GHC dropWhileEnd = Data.ByteString.dropWhileEnd #-}
{-# COMPILE GHC span         = Data.ByteString.span #-}
{-# COMPILE GHC spanEnd      = Data.ByteString.spanEnd #-}
{-# COMPILE GHC break        = Data.ByteString.break #-}
{-# COMPILE GHC breakEnd     = Data.ByteString.breakEnd #-}
{-# COMPILE GHC group        = Data.ByteString.group #-}
{-# COMPILE GHC groupBy      = Data.ByteString.groupBy #-}
{-# COMPILE GHC inits        = Data.ByteString.inits #-}
{-# COMPILE GHC tails        = Data.ByteString.tails #-}
{-# COMPILE GHC stripPrefix  = Data.ByteString.stripPrefix #-}
{-# COMPILE GHC stripSuffix  = Data.ByteString.stripSuffix #-}

{-# COMPILE GHC split     = Data.ByteString.split     #-}
{-# COMPILE GHC splitWith = Data.ByteString.splitWith #-}

{-# COMPILE GHC isPrefixOf = Data.ByteString.isPrefixOf #-}
{-# COMPILE GHC isSuffixOf = Data.ByteString.isSuffixOf #-}
{-# COMPILE GHC isInfixOf  = Data.ByteString.isInfixOf  #-}

{-# COMPILE GHC isValidUtf8 = Data.ByteString.isValidUtf8 #-}

{-# COMPILE GHC breakSubstring = Data.ByteString.breakSubstring #-}

{-# COMPILE GHC elem    = Data.ByteString.elem #-}
{-# COMPILE GHC notElem = Data.ByteString.notElem #-}

{-# COMPILE GHC find      = Data.ByteString.find      #-}
{-# COMPILE GHC filter    = Data.ByteString.filter    #-}
{-# COMPILE GHC partition = Data.ByteString.partition #-}

{-# COMPILE GHC index        = \ AgdaHasCallStack -> Data.ByteString.index        #-}
{-# COMPILE GHC indexMaybe   =                       Data.ByteString.indexMaybe   #-}
{-# COMPILE GHC _!?_         =                       (Data.ByteString.!?)         #-}
{-# COMPILE GHC elemIndex    =                       Data.ByteString.elemIndex    #-}
{-# COMPILE GHC elemIndices  =                       Data.ByteString.elemIndices  #-}
{-# COMPILE GHC elemIndexEnd =                       Data.ByteString.elemIndexEnd #-}
{-# COMPILE GHC findIndex    =                       Data.ByteString.findIndex    #-}
{-# COMPILE GHC findIndices  =                       Data.ByteString.findIndices  #-}
{-# COMPILE GHC findIndexEnd =                       Data.ByteString.findIndexEnd #-}
{-# COMPILE GHC count        =                       Data.ByteString.count        #-}

{-# COMPILE GHC zip         =           Data.ByteString.zip         #-}
{-# COMPILE GHC zipWith     = \ aℓ a -> Data.ByteString.zipWith     #-}
{-# COMPILE GHC packZipWith =           Data.ByteString.packZipWith #-}
{-# COMPILE GHC unzip       =           Data.ByteString.unzip       #-}

{-# COMPILE GHC sort = Data.ByteString.sort #-}

{-# COMPILE GHC copy = Data.ByteString.copy #-}

{-# COMPILE GHC packCString    = Data.ByteString.packCString    #-}
{-# COMPILE GHC packCStringLen = Data.ByteString.packCStringLen #-}

{-# COMPILE GHC useAsCString    = \ aℓ a -> Data.ByteString.useAsCString    #-}
{-# COMPILE GHC useAsCStringLen = \ aℓ a -> Data.ByteString.useAsCStringLen #-}

{-# COMPILE GHC getLine     = Data.ByteString.getLine     #-}
{-# COMPILE GHC getContents = Data.ByteString.getContents #-}
{-# COMPILE GHC putStr      = Data.ByteString.putStr      #-}
{-# COMPILE GHC interact    = Data.ByteString.interact    #-}

{-# COMPILE GHC readFile   = Data.ByteString.readFile   #-}
{-# COMPILE GHC writeFile  = Data.ByteString.writeFile  #-}
{-# COMPILE GHC appendFile = Data.ByteString.appendFile #-}

{-# COMPILE GHC hGetLine        = Data.ByteString.hGetLine        #-}
{-# COMPILE GHC hGetContents    = Data.ByteString.hGetContents    #-}
{-# COMPILE GHC hGet            = Data.ByteString.hGet            #-}
{-# COMPILE GHC hGetSome        = Data.ByteString.hGetSome        #-}
{-# COMPILE GHC hGetNonBlocking = Data.ByteString.hGetNonBlocking #-}
{-# COMPILE GHC hPut            = Data.ByteString.hPut            #-}
{-# COMPILE GHC hPutNonBlocking = Data.ByteString.hPutNonBlocking #-}
{-# COMPILE GHC hPutStr         = Data.ByteString.hPutStr         #-}
