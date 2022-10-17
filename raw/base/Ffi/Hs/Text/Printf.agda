{-# OPTIONS --without-K #-}

module Ffi.Hs.Text.Printf where

open import Agda.Builtin.Bool      using (Bool)
open import Agda.Builtin.Char      using (Char)
open import Agda.Builtin.Int       using () renaming (Int to Integer)
open import Agda.Builtin.List      using (List)
open import Agda.Builtin.Maybe     using (Maybe)
open import Agda.Primitive
open import Ffi.Hs.-base.Class     using (Integral; Bounded; RealFloat)
open import Ffi.Hs.-base.Unit      using (⊤; ⊤′)
open import Ffi.Hs.Data.Int        using (Int; Int8; Int16; Int32; Int64)
open import Ffi.Hs.Data.Word       using (Word; Word8; Word16; Word32; Word64)
open import Ffi.Hs.GHC.Float       using (Float; Double)
open import Ffi.Hs.Numeric.Natural using (Natural)
open import Ffi.Hs.System.IO       using (Handle; IO)
open import Ffi.Hs.Text.Show       using (ShowS)

{-# FOREIGN GHC
import qualified Text.Printf
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ bℓ : Level
        A : Set aℓ
        B : Set bℓ

postulate
    PrintfType  : Set aℓ → Set aℓ
    HPrintfType : Set aℓ → Set aℓ

    printf  : ⦃ PrintfType A ⦄ → List Char → A
    hPrintf : ⦃ HPrintfType A ⦄ → Handle → List Char → A

{-# FOREIGN GHC
data AgdaPrintfType  aℓ a = Text.Printf.PrintfType a  => AgdaPrintfType
data AgdaHPrintfType aℓ a = Text.Printf.HPrintfType a => AgdaHPrintfType
#-}
{-# COMPILE GHC PrintfType  = type(0) AgdaPrintfType  #-}
{-# COMPILE GHC HPrintfType = type(0) AgdaHPrintfType #-}

{-# COMPILE GHC printf  = \ aℓ a AgdaPrintfType  -> Text.Printf.printf  #-}
{-# COMPILE GHC hPrintf = \ aℓ a AgdaHPrintfType -> Text.Printf.hPrintf #-}

data FormatAdjustment : Set where
    LeftAdjust ZeroPad : FormatAdjustment

{-# COMPILE GHC FormatAdjustment = data Text.Printf.FormatAdjustment (Text.Printf.LeftAdjust | Text.Printf.ZeroPad) #-}

data FormatSign : Set where
    SignPlus SignSpace : FormatSign

{-# COMPILE GHC FormatSign = data Text.Printf.FormatSign (Text.Printf.SignPlus | Text.Printf.SignSpace) #-}

record FieldFormat : Set where
    constructor mkFieldFormat
    field
        fmtWidth     : Maybe Int
        fmtPrecision : Maybe Int
        fmtAdjust    : Maybe FormatAdjustment
        fmtSign      : Maybe FormatSign
        fmtAlternate : Bool
        fmtModifiers : List Char
        fmtChar      : Char

{-# COMPILE GHC FieldFormat = data Text.Printf.FieldFormat (Text.Printf.FieldFormat) #-}

FieldFormatter : Set
FieldFormatter = FieldFormat → ShowS

record FormatParse : Set where
    constructor mkFormatParse
    field
        fpModifiers : List Char
        fpChar      : Char
        fpRest      : List Char

{-# COMPILE GHC FormatParse = data Text.Printf.FormatParse (Text.Printf.FormatParse) #-}

ModifierParser : Set
ModifierParser = List Char → FormatParse

postulate
    PrintfArg : Set aℓ → Set aℓ
    formatArg   : ⦃ PrintfArg A ⦄ → A → FieldFormatter
    parseFormat : ⦃ PrintfArg A ⦄ → A → ModifierParser

    vFmt : Char → FieldFormat → FieldFormat

{-# FOREIGN GHC data AgdaPrintfArg aℓ a = Text.Printf.PrintfArg a => AgdaPrintfArg #-}
{-# COMPILE GHC PrintfArg = type(0) AgdaPrintfArg #-}
{-# COMPILE GHC formatArg   = \ aℓ a AgdaPrintfArg -> Text.Printf.formatArg   #-}
{-# COMPILE GHC parseFormat = \ aℓ a AgdaPrintfArg -> Text.Printf.parseFormat #-}
{-# COMPILE GHC vFmt        =                         Text.Printf.vFmt        #-}

postulate
    IsChar   : Set aℓ → Set aℓ
    toChar   : ⦃ IsChar A ⦄ → A → Char
    fromChar : ⦃ IsChar A ⦄ → Char → A

{-# FOREIGN GHC data AgdaIsChar aℓ a = Text.Printf.IsChar a => AgdaIsChar #-}
{-# COMPILE GHC IsChar   = type(0) AgdaIsChar   #-}
{-# COMPILE GHC toChar   = \ aℓ a AgdaIsChar -> Text.Printf.toChar   #-}
{-# COMPILE GHC fromChar = \ aℓ a AgdaIsChar -> Text.Printf.fromChar #-}

postulate
    formatString : List Char → FieldFormatter
    formatChar : Char → FieldFormatter
    formatInt : ⦃ Integral A ⦄ → ⦃ Bounded A ⦄ → A → FieldFormatter
    formatInteger : Integer → FieldFormatter
    formatRealFloat : ⦃ RealFloat A ⦄ → A → FieldFormatter

    errorBadFormat : Char → A
    errorShortFormat : A
    errorMissingArgument : A
    errorBadArgument : A
    perror : List Char → A

{-# COMPILE GHC formatString    =                                    Text.Printf.formatString    #-}
{-# COMPILE GHC formatChar      =                                    Text.Printf.formatChar      #-}
{-# COMPILE GHC formatInt       = \ aℓ a AgdaIntegral AgdaBounded -> Text.Printf.formatInt       #-}
{-# COMPILE GHC formatInteger   =                                    Text.Printf.formatInteger   #-}
{-# COMPILE GHC formatRealFloat = \ aℓ a AgdaRealFloat            -> Text.Printf.formatRealFloat #-}

{-# COMPILE GHC errorBadFormat       = \ aℓ a -> Text.Printf.errorBadFormat       #-}
{-# COMPILE GHC errorShortFormat     = \ aℓ a -> Text.Printf.errorShortFormat     #-}
{-# COMPILE GHC errorMissingArgument = \ aℓ a -> Text.Printf.errorMissingArgument #-}
{-# COMPILE GHC errorBadArgument     = \ aℓ a -> Text.Printf.errorBadArgument     #-}
{-# COMPILE GHC perror               = \ aℓ a -> Text.Printf.perror               #-}

postulate
    IsChar[Char] : IsChar Char

    PrintfType[IO[⊤]]      : PrintfType (IO (⊤′ {aℓ}))
    PrintfType[List[Char]] : PrintfType (List Char)
    PrintfType[A⟶B]        : ⦃ PrintfArg A ⦄ → ⦃ PrintfType B ⦄ → PrintfType (A → B)

    HPrintfType[IO[⊤]] : HPrintfType (IO (⊤′ {aℓ}))
    HPrintfType[A⟶B]   : ⦃ PrintfArg A ⦄ → ⦃ HPrintfType B ⦄ → HPrintfType (A → B)

    PrintfArg[Int]        : PrintfArg Int
    PrintfArg[Int8]       : PrintfArg Int8
    PrintfArg[Int16]      : PrintfArg Int16
    PrintfArg[Int32]      : PrintfArg Int32
    PrintfArg[Int64]      : PrintfArg Int64
    PrintfArg[Word]       : PrintfArg Word
    PrintfArg[Word8]      : PrintfArg Word8
    PrintfArg[Word16]     : PrintfArg Word16
    PrintfArg[Word32]     : PrintfArg Word32
    PrintfArg[Word64]     : PrintfArg Word64
    PrintfArg[Integer]    : PrintfArg Integer
    PrintfArg[Natural]    : PrintfArg Natural
    PrintfArg[Char]       : PrintfArg Char
    PrintfArg[List[Char]] : PrintfArg (List Char)
    PrintfArg[Float]      : PrintfArg Float
    PrintfArg[Double]     : PrintfArg Double

{-# COMPILE GHC IsChar[Char] = AgdaIsChar #-}

{-# COMPILE GHC PrintfType[IO[⊤]]      = \ aℓ                                     -> AgdaPrintfType #-}
{-# COMPILE GHC PrintfType[List[Char]] =                                             AgdaPrintfType #-}
{-# COMPILE GHC PrintfType[A⟶B]        = \ aℓ a bℓ b AgdaPrintfArg AgdaPrintfType -> AgdaPrintfType #-}

{-# COMPILE GHC HPrintfType[IO[⊤]] = \ aℓ                                      -> AgdaHPrintfType #-}
{-# COMPILE GHC HPrintfType[A⟶B]   = \ aℓ a bℓ b AgdaPrintfArg AgdaHPrintfType -> AgdaHPrintfType #-}

{-# COMPILE GHC PrintfArg[Int]        = AgdaPrintfArg #-}
{-# COMPILE GHC PrintfArg[Int8]       = AgdaPrintfArg #-}
{-# COMPILE GHC PrintfArg[Int16]      = AgdaPrintfArg #-}
{-# COMPILE GHC PrintfArg[Int32]      = AgdaPrintfArg #-}
{-# COMPILE GHC PrintfArg[Int64]      = AgdaPrintfArg #-}
{-# COMPILE GHC PrintfArg[Word]       = AgdaPrintfArg #-}
{-# COMPILE GHC PrintfArg[Word8]      = AgdaPrintfArg #-}
{-# COMPILE GHC PrintfArg[Word16]     = AgdaPrintfArg #-}
{-# COMPILE GHC PrintfArg[Word32]     = AgdaPrintfArg #-}
{-# COMPILE GHC PrintfArg[Word64]     = AgdaPrintfArg #-}
{-# COMPILE GHC PrintfArg[Integer]    = AgdaPrintfArg #-}
{-# COMPILE GHC PrintfArg[Natural]    = AgdaPrintfArg #-}
{-# COMPILE GHC PrintfArg[Char]       = AgdaPrintfArg #-}
{-# COMPILE GHC PrintfArg[List[Char]] = AgdaPrintfArg #-}
{-# COMPILE GHC PrintfArg[Float]      = AgdaPrintfArg #-}
{-# COMPILE GHC PrintfArg[Double]     = AgdaPrintfArg #-}
