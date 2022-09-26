{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.ByteString.Lazy where

open import Agda.Builtin.Bool      using (Bool)
open import Agda.Builtin.List      using (List)
open import Agda.Builtin.Maybe     using (Maybe)
open import Agda.Primitive
open import Ffi.Hs.-base.Unit      using (⊤)
open import Ffi.Hs.Control.DeepSeq using (NFData)
open import Ffi.Hs.Data.Int        using (Int; Int64)
open import Ffi.Hs.Data.Ord        using (Ordering)
open import Ffi.Hs.Data.Tuple      using (Tuple2)
open import Ffi.Hs.Data.Word       using (Word8)
open import Ffi.Hs.GHC.Stack       using (HasCallStack)
open import Ffi.Hs.System.IO       using (IO; Handle; FilePath)

{-# FOREIGN GHC
import qualified Data.ByteString.Lazy
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
import MAlonzo.Code.Ffi.Hs.GHC.Stack (AgdaHasCallStack(AgdaHasCallStack))
#-}

open import Ffi.Hs.Data.ByteString.Lazy.Internal public
    using
    ( ByteString
    ; LazyByteString
    ; IsList[ByteString]
    ; Eq[ByteString]
    ; Data[ByteString]
    ; Ord[ByteString]
    ; Read[ByteString]
    ; Show[ByteString]
    ; IsString[ByteString]
    ; Semigroup[ByteString]
    ; Monoid[ByteString]
    ; NFData[ByteString]
    ; foldrChunks
    ; foldlChunks
    ; toStrict
    ; fromStrict
    )
    renaming
    ( packBytes to pack
    ; unpackBytes to unpack
    )

private
    variable
        aℓ : Level
        A : Set aℓ

{-# FOREIGN GHC
import qualified Data.ByteString
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
import MAlonzo.Code.Ffi.Hs.GHC.Stack (AgdaHasCallStack(AgdaHasCallStack))
#-}

infixl 9 _!?_

postulate
    empty           : ByteString
    singleton       : Word8 → ByteString
    fromChunks      : List ByteString → ByteString
    toChunks        : ByteString → List ByteString
    cons            : Word8 → ByteString → ByteString
    cons'           : Word8 → ByteString → ByteString
    snoc            : ByteString → Word8 → ByteString
    append          : ByteString → ByteString → ByteString
    head            : ⦃ HasCallStack ⦄ → ByteString → Word8
    uncons          : ByteString → Maybe (Tuple2 Word8 ByteString)
    unsnoc          : ByteString → Maybe (Tuple2 ByteString Word8)
    last            : ⦃ HasCallStack ⦄ → ByteString → Word8
    tail            : ⦃ HasCallStack ⦄ → ByteString → ByteString
    init            : ⦃ HasCallStack ⦄ → ByteString → ByteString
    null            : ByteString → Bool
    length          : ByteString → Int64
    map             : (Word8 → Word8) → ByteString → ByteString
    reverse         : ByteString → ByteString
    intersperse     : Word8 → ByteString → ByteString
    intercalate     : ByteString → List ByteString → ByteString
    transpose       : List ByteString → List ByteString
    foldl           : (A → Word8 → A) → A → ByteString → A
    foldl'          : (A → Word8 → A) → A → ByteString → A
    foldl1          : ⦃ HasCallStack ⦄ → (Word8 → Word8 → Word8) → ByteString → Word8
    foldl1'         : ⦃ HasCallStack ⦄ → (Word8 → Word8 → Word8) → ByteString → Word8
    foldr           : (Word8 → A → A) → A → ByteString → A
    foldr'          : (Word8 → A → A) → A → ByteString → A
    foldr1          : ⦃ HasCallStack ⦄ → (Word8 → Word8 → Word8) → ByteString → Word8
    foldr1'         : ⦃ HasCallStack ⦄ → (Word8 → Word8 → Word8) → ByteString → Word8
    concat          : List ByteString → ByteString
    concatMap       : (Word8 → ByteString) → ByteString → ByteString
    any             : (Word8 → Bool) → ByteString → Bool
    all             : (Word8 → Bool) → ByteString → Bool
    maximum         : ⦃ HasCallStack ⦄ → ByteString → Word8
    minimum         : ⦃ HasCallStack ⦄ → ByteString → Word8
    compareLength   : ByteString → Int64 → Ordering
    scanl           : (Word8 → Word8 → Word8) → Word8 → ByteString → ByteString
    scanl1          : (Word8 → Word8 → Word8) → ByteString → ByteString
    scanr           : (Word8 → Word8 → Word8) → Word8 → ByteString → ByteString
    scanr1          : (Word8 → Word8 → Word8) → ByteString → ByteString
    mapAccumL       : (A → Word8 → Tuple2 A Word8) → A → ByteString → Tuple2 A ByteString
    mapAccumR       : (A → Word8 → Tuple2 A Word8) → A → ByteString → Tuple2 A ByteString
    repeat          : Word8 → ByteString
    replicate       : Int64 → Word8 → ByteString
    cycle           : ⦃ HasCallStack ⦄ → ByteString → ByteString
    iterate         : (Word8 → Word8) → Word8 → ByteString
    unfoldr         : (A → Maybe (Tuple2 Word8 A)) → A → ByteString
    take            : Int64 → ByteString → ByteString
    takeEnd         : Int64 → ByteString → ByteString
    drop            : Int64 → ByteString → ByteString
    dropEnd         : Int64 → ByteString → ByteString
    splitAt         : Int64 → ByteString → Tuple2 ByteString ByteString
    takeWhile       : (Word8 → Bool) → ByteString → ByteString
    takeWhileEnd    : (Word8 → Bool) → ByteString → ByteString
    dropWhile       : (Word8 → Bool) → ByteString → ByteString
    dropWhileEnd    : (Word8 → Bool) → ByteString → ByteString
    span            : (Word8 → Bool) → ByteString → Tuple2 ByteString ByteString
    spanEnd         : (Word8 → Bool) → ByteString → Tuple2 ByteString ByteString
    break           : (Word8 → Bool) → ByteString → Tuple2 ByteString ByteString
    breakEnd        : (Word8 → Bool) → ByteString → Tuple2 ByteString ByteString
    group           : ByteString → List ByteString
    groupBy         : (Word8 → Word8 → Bool) → ByteString → List ByteString
    inits           : ByteString → List ByteString
    tails           : ByteString → List ByteString
    stripPrefix     : ByteString → ByteString → Maybe ByteString
    stripSuffix     : ByteString → ByteString → Maybe ByteString
    split           : Word8 → ByteString → List ByteString
    splitWith       : (Word8 → Bool) → ByteString → List ByteString
    isPrefixOf      : ByteString → ByteString → Bool
    isSuffixOf      : ByteString → ByteString → Bool
    elem            : Word8 → ByteString → Bool
    notElem         : Word8 → ByteString → Bool
    find            : (Word8 → Bool) → ByteString → Maybe Word8
    filter          : (Word8 → Bool) → ByteString → ByteString
    partition       : (Word8 → Bool) → ByteString → Tuple2 ByteString ByteString
    index           : ⦃ HasCallStack ⦄ → ByteString → Int64 → Word8
    indexMaybe      : ByteString → Int64 → Maybe Word8
    _!?_            : ByteString → Int64 → Maybe Word8
    elemIndex       : Word8 → ByteString → Maybe Int64
    elemIndexEnd    : Word8 → ByteString → Maybe Int64
    elemIndices     : Word8 → ByteString → List Int64
    findIndex       : (Word8 → Bool) → ByteString → Maybe Int64
    findIndexEnd    : (Word8 → Bool) → ByteString → Maybe Int64
    findIndices     : (Word8 → Bool) → ByteString → List Int64
    count           : Word8 → ByteString → Int64
    zip             : ByteString → ByteString → List (Tuple2 Word8 Word8)
    zipWith         : (Word8 → Word8 → A) → ByteString → ByteString → List A
    packZipWith     : (Word8 → Word8 → Word8) → ByteString → ByteString → ByteString
    unzip           : List (Tuple2 Word8 Word8) → Tuple2 ByteString ByteString
    copy            : ByteString → ByteString
    getContents     : IO ByteString
    putStr          : ByteString → IO ⊤
    interact        : (ByteString → ByteString) → IO ⊤
    readFile        : FilePath → IO ByteString
    writeFile       : FilePath → ByteString → IO ⊤
    appendFile      : FilePath → ByteString → IO ⊤
    hGetContents    : Handle → IO ByteString
    hGet            : Handle → Int → IO ByteString
    hGetNonBlocking : Handle → Int → IO ByteString
    hPut            : Handle → ByteString → IO ⊤
    hPutNonBlocking : Handle → ByteString → IO ByteString
    hPutStr         : Handle → ByteString → IO ⊤

{-# COMPILE GHC empty           =                       Data.ByteString.Lazy.empty           #-}
{-# COMPILE GHC singleton       =                       Data.ByteString.Lazy.singleton       #-}
{-# COMPILE GHC fromChunks      =                       Data.ByteString.Lazy.fromChunks      #-}
{-# COMPILE GHC toChunks        =                       Data.ByteString.Lazy.toChunks        #-}
{-# COMPILE GHC cons            =                       Data.ByteString.Lazy.cons            #-}
{-# COMPILE GHC cons'           =                       Data.ByteString.Lazy.cons'           #-}
{-# COMPILE GHC snoc            =                       Data.ByteString.Lazy.snoc            #-}
{-# COMPILE GHC append          =                       Data.ByteString.Lazy.append          #-}
{-# COMPILE GHC head            = \ AgdaHasCallStack -> Data.ByteString.Lazy.head            #-}
{-# COMPILE GHC uncons          =                       Data.ByteString.Lazy.uncons          #-}
{-# COMPILE GHC unsnoc          =                       Data.ByteString.Lazy.unsnoc          #-}
{-# COMPILE GHC last            =                       Data.ByteString.Lazy.last            #-}
{-# COMPILE GHC tail            = \ AgdaHasCallStack -> Data.ByteString.Lazy.tail            #-}
{-# COMPILE GHC init            = \ AgdaHasCallStack -> Data.ByteString.Lazy.init            #-}
{-# COMPILE GHC null            = \ AgdaHasCallStack -> Data.ByteString.Lazy.null            #-}
{-# COMPILE GHC length          =                       Data.ByteString.Lazy.length          #-}
{-# COMPILE GHC map             =                       Data.ByteString.Lazy.map             #-}
{-# COMPILE GHC reverse         =                       Data.ByteString.Lazy.reverse         #-}
{-# COMPILE GHC intersperse     =                       Data.ByteString.Lazy.intersperse     #-}
{-# COMPILE GHC intercalate     =                       Data.ByteString.Lazy.intercalate     #-}
{-# COMPILE GHC transpose       =                       Data.ByteString.Lazy.transpose       #-}
{-# COMPILE GHC foldl           = \ aℓ a ->             Data.ByteString.Lazy.foldl           #-}
{-# COMPILE GHC foldl'          = \ aℓ a ->             Data.ByteString.Lazy.foldl'          #-}
{-# COMPILE GHC foldl1          = \ AgdaHasCallStack -> Data.ByteString.Lazy.foldl1          #-}
{-# COMPILE GHC foldl1'         = \ AgdaHasCallStack -> Data.ByteString.Lazy.foldl1'         #-}
{-# COMPILE GHC foldr           = \ aℓ a ->             Data.ByteString.Lazy.foldr           #-}
{-# COMPILE GHC foldr'          = \ aℓ a ->             Data.ByteString.Lazy.foldr'          #-}
{-# COMPILE GHC foldr1          = \ AgdaHasCallStack -> Data.ByteString.Lazy.foldr1          #-}
{-# COMPILE GHC foldr1'         = \ AgdaHasCallStack -> Data.ByteString.Lazy.foldr1'         #-}
{-# COMPILE GHC concat          =                       Data.ByteString.Lazy.concat          #-}
{-# COMPILE GHC concatMap       =                       Data.ByteString.Lazy.concatMap       #-}
{-# COMPILE GHC any             =                       Data.ByteString.Lazy.any             #-}
{-# COMPILE GHC all             =                       Data.ByteString.Lazy.all             #-}
{-# COMPILE GHC maximum         = \ AgdaHasCallStack -> Data.ByteString.Lazy.maximum         #-}
{-# COMPILE GHC minimum         = \ AgdaHasCallStack -> Data.ByteString.Lazy.minimum         #-}
{-# COMPILE GHC compareLength   =                       Data.ByteString.Lazy.compareLength   #-}
{-# COMPILE GHC scanl           =                       Data.ByteString.Lazy.scanl           #-}
{-# COMPILE GHC scanl1          =                       Data.ByteString.Lazy.scanl1          #-}
{-# COMPILE GHC scanr           =                       Data.ByteString.Lazy.scanr           #-}
{-# COMPILE GHC scanr1          =                       Data.ByteString.Lazy.scanr1          #-}
{-# COMPILE GHC mapAccumL       = \ aℓ a ->             Data.ByteString.Lazy.mapAccumL       #-}
{-# COMPILE GHC mapAccumR       = \ aℓ a ->             Data.ByteString.Lazy.mapAccumR       #-}
{-# COMPILE GHC repeat          =                       Data.ByteString.Lazy.repeat          #-}
{-# COMPILE GHC replicate       =                       Data.ByteString.Lazy.replicate       #-}
{-# COMPILE GHC cycle           = \ AgdaHasCallStack -> Data.ByteString.Lazy.cycle           #-}
{-# COMPILE GHC iterate         =                       Data.ByteString.Lazy.iterate         #-}
{-# COMPILE GHC unfoldr         = \ aℓ a ->             Data.ByteString.Lazy.unfoldr         #-}
{-# COMPILE GHC take            =                       Data.ByteString.Lazy.take            #-}
{-# COMPILE GHC takeEnd         =                       Data.ByteString.Lazy.takeEnd         #-}
{-# COMPILE GHC drop            =                       Data.ByteString.Lazy.drop            #-}
{-# COMPILE GHC dropEnd         =                       Data.ByteString.Lazy.dropEnd         #-}
{-# COMPILE GHC splitAt         =                       Data.ByteString.Lazy.splitAt         #-}
{-# COMPILE GHC takeWhile       =                       Data.ByteString.Lazy.takeWhile       #-}
{-# COMPILE GHC takeWhileEnd    =                       Data.ByteString.Lazy.takeWhileEnd    #-}
{-# COMPILE GHC dropWhile       =                       Data.ByteString.Lazy.dropWhile       #-}
{-# COMPILE GHC dropWhileEnd    =                       Data.ByteString.Lazy.dropWhileEnd    #-}
{-# COMPILE GHC span            =                       Data.ByteString.Lazy.span            #-}
{-# COMPILE GHC spanEnd         =                       Data.ByteString.Lazy.spanEnd         #-}
{-# COMPILE GHC break           =                       Data.ByteString.Lazy.break           #-}
{-# COMPILE GHC breakEnd        =                       Data.ByteString.Lazy.breakEnd        #-}
{-# COMPILE GHC group           =                       Data.ByteString.Lazy.group           #-}
{-# COMPILE GHC groupBy         =                       Data.ByteString.Lazy.groupBy         #-}
{-# COMPILE GHC inits           =                       Data.ByteString.Lazy.inits           #-}
{-# COMPILE GHC tails           =                       Data.ByteString.Lazy.tails           #-}
{-# COMPILE GHC stripPrefix     =                       Data.ByteString.Lazy.stripPrefix     #-}
{-# COMPILE GHC stripSuffix     =                       Data.ByteString.Lazy.stripSuffix     #-}
{-# COMPILE GHC split           =                       Data.ByteString.Lazy.split           #-}
{-# COMPILE GHC splitWith       =                       Data.ByteString.Lazy.splitWith       #-}
{-# COMPILE GHC isPrefixOf      =                       Data.ByteString.Lazy.isPrefixOf      #-}
{-# COMPILE GHC isSuffixOf      =                       Data.ByteString.Lazy.isSuffixOf      #-}
{-# COMPILE GHC elem            =                       Data.ByteString.Lazy.elem            #-}
{-# COMPILE GHC notElem         =                       Data.ByteString.Lazy.notElem         #-}
{-# COMPILE GHC find            =                       Data.ByteString.Lazy.find            #-}
{-# COMPILE GHC filter          =                       Data.ByteString.Lazy.filter          #-}
{-# COMPILE GHC partition       =                       Data.ByteString.Lazy.partition       #-}
{-# COMPILE GHC index           = \ AgdaHasCallStack -> Data.ByteString.Lazy.index           #-}
{-# COMPILE GHC indexMaybe      =                       Data.ByteString.Lazy.indexMaybe      #-}
{-# COMPILE GHC _!?_            =                       (Data.ByteString.Lazy.!?)            #-}
{-# COMPILE GHC elemIndex       =                       Data.ByteString.Lazy.elemIndex       #-}
{-# COMPILE GHC elemIndexEnd    =                       Data.ByteString.Lazy.elemIndexEnd    #-}
{-# COMPILE GHC elemIndices     =                       Data.ByteString.Lazy.elemIndices     #-}
{-# COMPILE GHC findIndex       =                       Data.ByteString.Lazy.findIndex       #-}
{-# COMPILE GHC findIndexEnd    =                       Data.ByteString.Lazy.findIndexEnd    #-}
{-# COMPILE GHC findIndices     =                       Data.ByteString.Lazy.findIndices     #-}
{-# COMPILE GHC count           =                       Data.ByteString.Lazy.count           #-}
{-# COMPILE GHC zip             =                       Data.ByteString.Lazy.zip             #-}
{-# COMPILE GHC zipWith         = \ aℓ a ->             Data.ByteString.Lazy.zipWith         #-}
{-# COMPILE GHC packZipWith     =                       Data.ByteString.Lazy.packZipWith     #-}
{-# COMPILE GHC unzip           =                       Data.ByteString.Lazy.unzip           #-}
{-# COMPILE GHC copy            =                       Data.ByteString.Lazy.copy            #-}
{-# COMPILE GHC getContents     =                       Data.ByteString.Lazy.getContents     #-}
{-# COMPILE GHC putStr          =                       Data.ByteString.Lazy.putStr          #-}
{-# COMPILE GHC interact        =                       Data.ByteString.Lazy.interact        #-}
{-# COMPILE GHC readFile        =                       Data.ByteString.Lazy.readFile        #-}
{-# COMPILE GHC writeFile       =                       Data.ByteString.Lazy.writeFile       #-}
{-# COMPILE GHC appendFile      =                       Data.ByteString.Lazy.appendFile      #-}
{-# COMPILE GHC hGetContents    =                       Data.ByteString.Lazy.hGetContents    #-}
{-# COMPILE GHC hGet            =                       Data.ByteString.Lazy.hGet            #-}
{-# COMPILE GHC hGetNonBlocking =                       Data.ByteString.Lazy.hGetNonBlocking #-}
{-# COMPILE GHC hPut            =                       Data.ByteString.Lazy.hPut            #-}
{-# COMPILE GHC hPutNonBlocking =                       Data.ByteString.Lazy.hPutNonBlocking #-}
{-# COMPILE GHC hPutStr         =                       Data.ByteString.Lazy.hPutStr         #-}
