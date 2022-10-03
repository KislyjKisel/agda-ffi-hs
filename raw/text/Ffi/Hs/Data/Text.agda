{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Text where

open import Agda.Builtin.Bool      using (Bool)
open import Agda.Builtin.Char      using (Char)
open import Agda.Builtin.List      using (List)
open import Agda.Builtin.Maybe     using (Maybe)
open import Agda.Primitive
open import Ffi.Hs.-base.Class
open import Ffi.Hs.-base.Level     using (Liftℓ)
open import Ffi.Hs.Control.DeepSeq using (NFData)
open import Ffi.Hs.Data.Int        using (Int)
open import Ffi.Hs.Data.Ord        using (Ordering)
open import Ffi.Hs.Data.Tuple      using (Tuple2; Tuple3)
open import Ffi.Hs.GHC.Exts        using (Addr#)
open import Ffi.Hs.GHC.IsList      using (IsList)
open import Ffi.Hs.GHC.Stack       using (HasCallStack)
open import Ffi.Hs.Text.Printf     using (PrintfArg)

open import Agda.Builtin.String public
    using ()
    renaming (String to Text)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC {-# LANGUAGE MagicHash #-} #-}
{-# FOREIGN GHC
import qualified Data.Text
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
import MAlonzo.Code.Ffi.Hs.GHC.IsList (AgdaIsList(AgdaIsList))
import MAlonzo.Code.Ffi.Hs.Control.DeepSeq (AgdaNFData(AgdaNFData))
import MAlonzo.Code.Ffi.Hs.Text.Printf (AgdaPrintfArg(AgdaPrintfArg))
import MAlonzo.Code.Ffi.Hs.GHC.Stack (AgdaHasCallStack(AgdaHasCallStack))
#-}

private
    variable
        aℓ : Level
        A : Set aℓ
        M : Set aℓ → Set aℓ

postulate
    IsList[Text]    : IsList Text
    Eq[Text]        : Eq Text
    Data[Text]      : Data Text
    Ord[Text]       : Ord Text
    Read[Text]      : Read Text
    Show[Text]      : Show Text
    IsString[Text]  : IsString Text
    Semigroup[Text] : Semigroup Text
    Monoid[Text]    : Monoid Text
    PrintfArg[Text] : PrintfArg Text
    -- todo: (binary) Binary[Text] : Binary Text
    NFData[Text]    : NFData Text
    -- todo: (TH?) Lift

{-# COMPILE GHC IsList[Text]    = AgdaIsList    #-}
{-# COMPILE GHC Eq[Text]        = AgdaEq        #-}
{-# COMPILE GHC Data[Text]      = AgdaData      #-}
{-# COMPILE GHC Ord[Text]       = AgdaOrd       #-}
{-# COMPILE GHC Read[Text]      = AgdaRead      #-}
{-# COMPILE GHC Show[Text]      = AgdaShow      #-}
{-# COMPILE GHC IsString[Text]  = AgdaIsString  #-}
{-# COMPILE GHC Semigroup[Text] = AgdaSemigroup #-}
{-# COMPILE GHC Monoid[Text]    = AgdaMonoid    #-}
{-# COMPILE GHC PrintfArg[Text] = AgdaPrintfArg #-}
{-# COMPILE GHC NFData[Text]    = AgdaNFData    #-}

postulate
    pack                : List Char → Text
    unpack              : Text → List Char
    singleton           : Char → Text
    empty               : Text
    cons                : Char → Text → Text
    snoc                : Text → Char → Text
    append              : Text → Text → Text
    uncons              : Text → Maybe (Tuple2 Char Text)
    unsnoc              : Text → Maybe (Tuple2 Text Char)
    head                : ⦃ HasCallStack ⦄ → Text → Char
    last                : ⦃ HasCallStack ⦄ → Text → Char
    tail                : ⦃ HasCallStack ⦄ → Text → Text
    init                : ⦃ HasCallStack ⦄ → Text → Text
    null                : Text → Bool
    length              : Text → Int
    compareLength       : Text → Int → Ordering
    map                 : (Char → Char) → Text → Text
    intercalate         : Text → List Text → Text
    intersperse         : Char → Text → Text
    transpose           : List Text → List Text
    reverse             : Text → Text
    replace             : ⦃ HasCallStack ⦄ → Text → Text → Text → Text
    toCaseFold          : Text → Text
    toLower             : Text → Text
    toUpper             : Text → Text
    toTitle             : Text → Text
    justifyLeft         : Int → Char → Text → Text
    justifyRight        : Int → Char → Text → Text
    center              : Int → Char → Text → Text
    foldl               : (A → Char → A) → A → Text → A
    foldl'              : (A → Char → A) → A → Text → A
    foldl1              : ⦃ HasCallStack ⦄ → (Char → Char → Char) → Text → Char
    foldl1'             : ⦃ HasCallStack ⦄ → (Char → Char → Char) → Text → Char
    foldr               : (Char → A → A) → A → Text → A
    foldr'              : (Char → A → A) → A → Text → A
    foldr1              : ⦃ HasCallStack ⦄ → (Char → Char → Char) → Text → Char
    concat              : List Text → Text
    concatMap           : (Char → Text) → Text → Text
    any                 : (Char → Bool) → Text → Bool
    all                 : (Char → Bool) → Text → Bool
    maximum             : ⦃ HasCallStack ⦄ → Text → Char
    minimum             : ⦃ HasCallStack ⦄ → Text → Char
    scanl               : (Char → Char → Char) → Char → Text → Text
    scanl1              : (Char → Char → Char) → Text → Text
    scanr               : (Char → Char → Char) → Char → Text → Text
    scanr1              : (Char → Char → Char) → Text → Text
    mapAccumL           : (A → Char → Tuple2 A Char) → A → Text → Tuple2 A Text
    mapAccumR           : (A → Char → Tuple2 A Char) → A → Text → Tuple2 A Text
    replicate           : Int → Text → Text
    unfoldr             : (A → Maybe (Tuple2 Char A)) → A → Text
    unfoldrN            : Int → (A → Maybe (Tuple2 Char A)) → A → Text
    take                : Int → Text → Text
    takeEnd             : Int → Text → Text
    drop                : Int → Text → Text
    dropEnd             : Int → Text → Text
    takeWhile           : (Char → Bool) → Text → Text
    takeWhileEnd        : (Char → Bool) → Text → Text
    dropWhile           : (Char → Bool) → Text → Text
    dropWhileEnd        : (Char → Bool) → Text → Text
    dropAround          : (Char → Bool) → Text → Text
    strip               : Text → Text
    stripStart          : Text → Text
    stripEnd            : Text → Text
    splitAt             : Int → Text → (Tuple2 Text Text)
    breakOn             : ⦃ HasCallStack ⦄ → Text → Text → (Tuple2 Text Text)
    breakOnEnd          : ⦃ HasCallStack ⦄ → Text → Text → (Tuple2 Text Text)
    break               : (Char → Bool) → Text → (Tuple2 Text Text)
    span                : (Char → Bool) → Text → (Tuple2 Text Text)
    spanM               : ⦃ Monad M ⦄ → (Char → M (Liftℓ _ Bool)) → Text → M (Liftℓ _ (Tuple2 Text Text))
    spanEndM            : ⦃ Monad M ⦄ → (Char → M (Liftℓ _ Bool)) → Text → M (Liftℓ _ (Tuple2 Text Text))
    group               : Text → List Text
    groupBy             : (Char → Char → Bool) → Text → List Text
    inits               : Text → List Text
    tails               : Text → List Text
    splitOn             : ⦃ HasCallStack ⦄ → Text → Text → List Text
    split               : (Char → Bool) → Text → List Text
    chunksOf            : Int → Text → List Text
    lines               : Text → List Text
    words               : Text → List Text
    unlines             : List Text → Text
    unwords             : List Text → Text
    isPrefixOf          : Text → Text → Bool
    isSuffixOf          : Text → Text → Bool
    isInfixOf           : Text → Text → Bool
    stripPrefix         : Text → Text → Maybe Text
    stripSuffix         : Text → Text → Maybe Text
    commonPrefixes      : Text → Text → Maybe (Tuple3 Text Text Text)
    filter              : (Char → Bool) → Text → Text
    breakOnAll          : ⦃ HasCallStack ⦄ → Text → Text → List (Tuple2 Text Text)
    find                : (Char → Bool) → Text → Maybe Char
    elem                : Char → Text → Bool
    partition           : (Char → Bool) → Text → (Tuple2 Text Text)
    index               : ⦃ HasCallStack ⦄ → Text → Int → Char
    findIndex           : (Char → Bool) → Text → Maybe Int
    count               : ⦃ HasCallStack ⦄ → Text → Text → Int
    zip                 : Text → Text → List (Tuple2 Char Char)
    zipWith             : (Char → Char → Char) → Text → Text → Text
    copy                : Text → Text
    unpackCString#      : Addr# → Text
    unpackCStringAscii# : Addr# → Text
    measureOff          : Int → Text → Int

{-# COMPILE GHC pack                =                       Data.Text.pack                #-}
{-# COMPILE GHC unpack              =                       Data.Text.unpack              #-}
{-# COMPILE GHC singleton           =                       Data.Text.singleton           #-}
{-# COMPILE GHC empty               =                       Data.Text.empty               #-}
{-# COMPILE GHC cons                =                       Data.Text.cons                #-}
{-# COMPILE GHC snoc                =                       Data.Text.snoc                #-}
{-# COMPILE GHC append              =                       Data.Text.append              #-}
{-# COMPILE GHC uncons              =                       Data.Text.uncons              #-}
{-# COMPILE GHC unsnoc              =                       Data.Text.unsnoc              #-}
{-# COMPILE GHC head                = \ AgdaHasCallStack -> Data.Text.head                #-}
{-# COMPILE GHC last                = \ AgdaHasCallStack -> Data.Text.last                #-}
{-# COMPILE GHC tail                = \ AgdaHasCallStack -> Data.Text.tail                #-}
{-# COMPILE GHC init                = \ AgdaHasCallStack -> Data.Text.init                #-}
{-# COMPILE GHC null                =                       Data.Text.null                #-}
{-# COMPILE GHC length              =                       Data.Text.length              #-}
{-# COMPILE GHC compareLength       =                       Data.Text.compareLength       #-}
{-# COMPILE GHC map                 =                       Data.Text.map                 #-}
{-# COMPILE GHC intercalate         =                       Data.Text.intercalate         #-}
{-# COMPILE GHC intersperse         =                       Data.Text.intersperse         #-}
{-# COMPILE GHC transpose           =                       Data.Text.transpose           #-}
{-# COMPILE GHC reverse             =                       Data.Text.reverse             #-}
{-# COMPILE GHC replace             = \ AgdaHasCallStack -> Data.Text.replace             #-}
{-# COMPILE GHC toCaseFold          =                       Data.Text.toCaseFold          #-}
{-# COMPILE GHC toLower             =                       Data.Text.toLower             #-}
{-# COMPILE GHC toUpper             =                       Data.Text.toUpper             #-}
{-# COMPILE GHC toTitle             =                       Data.Text.toTitle             #-}
{-# COMPILE GHC justifyLeft         =                       Data.Text.justifyLeft         #-}
{-# COMPILE GHC justifyRight        =                       Data.Text.justifyRight        #-}
{-# COMPILE GHC center              =                       Data.Text.center              #-}
{-# COMPILE GHC foldl               = \ aℓ a             -> Data.Text.foldl               #-}
{-# COMPILE GHC foldl'              = \ aℓ a             -> Data.Text.foldl'              #-}
{-# COMPILE GHC foldl1              = \ AgdaHasCallStack -> Data.Text.foldl1              #-}
{-# COMPILE GHC foldl1'             = \ AgdaHasCallStack -> Data.Text.foldl1'             #-}
{-# COMPILE GHC foldr               = \ aℓ a             -> Data.Text.foldr               #-}
{-# COMPILE GHC foldr'              = \ aℓ a             -> Data.Text.foldr'              #-}
{-# COMPILE GHC foldr1              = \ AgdaHasCallStack -> Data.Text.foldr1              #-}
{-# COMPILE GHC concat              =                       Data.Text.concat              #-}
{-# COMPILE GHC concatMap           =                       Data.Text.concatMap           #-}
{-# COMPILE GHC any                 =                       Data.Text.any                 #-}
{-# COMPILE GHC all                 =                       Data.Text.all                 #-}
{-# COMPILE GHC maximum             = \ AgdaHasCallStack -> Data.Text.maximum             #-}
{-# COMPILE GHC minimum             = \ AgdaHasCallStack -> Data.Text.minimum             #-}
{-# COMPILE GHC scanl               =                       Data.Text.scanl               #-}
{-# COMPILE GHC scanl1              =                       Data.Text.scanl1              #-}
{-# COMPILE GHC scanr               =                       Data.Text.scanr               #-}
{-# COMPILE GHC scanr1              =                       Data.Text.scanr1              #-}
{-# COMPILE GHC mapAccumL           = \ aℓ a             -> Data.Text.mapAccumL           #-}
{-# COMPILE GHC mapAccumR           = \ aℓ a             -> Data.Text.mapAccumR           #-}
{-# COMPILE GHC replicate           =                       Data.Text.replicate           #-}
{-# COMPILE GHC unfoldr             = \ aℓ a             -> Data.Text.unfoldr             #-}
{-# COMPILE GHC unfoldrN            = \ aℓ a             -> Data.Text.unfoldrN            #-}
{-# COMPILE GHC take                =                       Data.Text.take                #-}
{-# COMPILE GHC takeEnd             =                       Data.Text.takeEnd             #-}
{-# COMPILE GHC drop                =                       Data.Text.drop                #-}
{-# COMPILE GHC dropEnd             =                       Data.Text.dropEnd             #-}
{-# COMPILE GHC takeWhile           =                       Data.Text.takeWhile           #-}
{-# COMPILE GHC takeWhileEnd        =                       Data.Text.takeWhileEnd        #-}
{-# COMPILE GHC dropWhile           =                       Data.Text.dropWhile           #-}
{-# COMPILE GHC dropWhileEnd        =                       Data.Text.dropWhileEnd        #-}
{-# COMPILE GHC dropAround          =                       Data.Text.dropAround          #-}
{-# COMPILE GHC strip               =                       Data.Text.strip               #-}
{-# COMPILE GHC stripStart          =                       Data.Text.stripStart          #-}
{-# COMPILE GHC stripEnd            =                       Data.Text.stripEnd            #-}
{-# COMPILE GHC splitAt             =                       Data.Text.splitAt             #-}
{-# COMPILE GHC breakOn             = \ AgdaHasCallStack -> Data.Text.breakOn             #-}
{-# COMPILE GHC breakOnEnd          = \ AgdaHasCallStack -> Data.Text.breakOnEnd          #-}
{-# COMPILE GHC break               =                       Data.Text.break               #-}
{-# COMPILE GHC span                =                       Data.Text.span                #-}
{-# COMPILE GHC spanM               = \ mℓ m AgdaMonad   -> Data.Text.spanM               #-}
{-# COMPILE GHC spanEndM            = \ mℓ m AgdaMonad   -> Data.Text.spanEndM            #-}
{-# COMPILE GHC group               =                       Data.Text.group               #-}
{-# COMPILE GHC groupBy             =                       Data.Text.groupBy             #-}
{-# COMPILE GHC inits               =                       Data.Text.inits               #-}
{-# COMPILE GHC tails               =                       Data.Text.tails               #-}
{-# COMPILE GHC splitOn             = \ AgdaHasCallStack -> Data.Text.splitOn             #-}
{-# COMPILE GHC split               =                       Data.Text.split               #-}
{-# COMPILE GHC chunksOf            =                       Data.Text.chunksOf            #-}
{-# COMPILE GHC lines               =                       Data.Text.lines               #-}
{-# COMPILE GHC words               =                       Data.Text.words               #-}
{-# COMPILE GHC unlines             =                       Data.Text.unlines             #-}
{-# COMPILE GHC unwords             =                       Data.Text.unwords             #-}
{-# COMPILE GHC isPrefixOf          =                       Data.Text.isPrefixOf          #-}
{-# COMPILE GHC isSuffixOf          =                       Data.Text.isSuffixOf          #-}
{-# COMPILE GHC isInfixOf           =                       Data.Text.isInfixOf           #-}
{-# COMPILE GHC stripPrefix         =                       Data.Text.stripPrefix         #-}
{-# COMPILE GHC stripSuffix         =                       Data.Text.stripSuffix         #-}
{-# COMPILE GHC commonPrefixes      =                       Data.Text.commonPrefixes      #-}
{-# COMPILE GHC filter              =                       Data.Text.filter              #-}
{-# COMPILE GHC breakOnAll          = \ AgdaHasCallStack -> Data.Text.breakOnAll          #-}
{-# COMPILE GHC find                =                       Data.Text.find                #-}
{-# COMPILE GHC elem                =                       Data.Text.elem                #-}
{-# COMPILE GHC partition           =                       Data.Text.partition           #-}
{-# COMPILE GHC index               = \ AgdaHasCallStack -> Data.Text.index               #-}
{-# COMPILE GHC findIndex           =                       Data.Text.findIndex           #-}
{-# COMPILE GHC count               = \ AgdaHasCallStack -> Data.Text.count               #-}
{-# COMPILE GHC zip                 =                       Data.Text.zip                 #-}
{-# COMPILE GHC zipWith             =                       Data.Text.zipWith             #-}
{-# COMPILE GHC copy                =                       Data.Text.copy                #-}
{-# COMPILE GHC unpackCString#      =                       Data.Text.unpackCString#      #-}
{-# COMPILE GHC unpackCStringAscii# =                       Data.Text.unpackCStringAscii# #-}
{-# COMPILE GHC measureOff          =                       Data.Text.measureOff          #-}
