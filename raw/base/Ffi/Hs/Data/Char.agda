{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Char where

open import Agda.Builtin.Bool                   using (Bool)
open import Agda.Builtin.List                   using (List)
open import Ffi.Hs.-base.Class
open import Ffi.Hs.Data.Int                     using (Int)
open import Ffi.Hs.Text.ParserCombinators.ReadP using (ReadS)
open import Ffi.Hs.Text.Show                    using (ShowS)

open import Agda.Builtin.Char public
    using (Char)

{-# FOREIGN GHC
import qualified Data.Char
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

postulate
    Data[Char]     : Data Char
    Storable[Char] : Storable Char
    Bounded[Char]  : Bounded Char
    Enum[Char]     : Enum Char
    Ix[Char]       : Ix Char
    Read[Char]     : Read Char
    Show[Char]     : Show Char
    Eq[Char]       : Eq Char
    Ord[Char]      : Ord Char

{-# COMPILE GHC Data[Char]     = AgdaData     #-}
{-# COMPILE GHC Storable[Char] = AgdaStorable #-}
{-# COMPILE GHC Bounded[Char]  = AgdaBounded  #-}
{-# COMPILE GHC Enum[Char]     = AgdaEnum     #-}
{-# COMPILE GHC Ix[Char]       = AgdaIx       #-}
{-# COMPILE GHC Read[Char]     = AgdaRead     #-}
{-# COMPILE GHC Show[Char]     = AgdaShow     #-}
{-# COMPILE GHC Eq[Char]       = AgdaEq       #-}
{-# COMPILE GHC Ord[Char]      = AgdaOrd      #-}

postulate
    isControl     : Char → Bool
    isSpace       : Char → Bool
    isLower       : Char → Bool
    isUpper       : Char → Bool
    isAlpha       : Char → Bool
    isAlphaNum    : Char → Bool
    isPrint       : Char → Bool
    isDigit       : Char → Bool
    isOctDigit    : Char → Bool
    isHexDigit    : Char → Bool
    isLetter      : Char → Bool
    isMark        : Char → Bool
    isNumber      : Char → Bool
    isPunctuation : Char → Bool
    isSymbol      : Char → Bool
    isSeparator   : Char → Bool
    isAscii       : Char → Bool
    isLatin1      : Char → Bool
    isAsciiUpper  : Char → Bool
    isAsciiLower  : Char → Bool

{-# COMPILE GHC isControl     = Data.Char.isControl     #-}
{-# COMPILE GHC isSpace       = Data.Char.isSpace       #-}
{-# COMPILE GHC isLower       = Data.Char.isLower       #-}
{-# COMPILE GHC isUpper       = Data.Char.isUpper       #-}
{-# COMPILE GHC isAlpha       = Data.Char.isAlpha       #-}
{-# COMPILE GHC isAlphaNum    = Data.Char.isAlphaNum    #-}
{-# COMPILE GHC isPrint       = Data.Char.isPrint       #-}
{-# COMPILE GHC isDigit       = Data.Char.isDigit       #-}
{-# COMPILE GHC isOctDigit    = Data.Char.isOctDigit    #-}
{-# COMPILE GHC isHexDigit    = Data.Char.isHexDigit    #-}
{-# COMPILE GHC isLetter      = Data.Char.isLetter      #-}
{-# COMPILE GHC isMark        = Data.Char.isMark        #-}
{-# COMPILE GHC isNumber      = Data.Char.isNumber      #-}
{-# COMPILE GHC isPunctuation = Data.Char.isPunctuation #-}
{-# COMPILE GHC isSymbol      = Data.Char.isSymbol      #-}
{-# COMPILE GHC isSeparator   = Data.Char.isSeparator   #-}
{-# COMPILE GHC isAscii       = Data.Char.isAscii       #-}
{-# COMPILE GHC isLatin1      = Data.Char.isLatin1      #-}
{-# COMPILE GHC isAsciiUpper  = Data.Char.isAsciiUpper  #-}
{-# COMPILE GHC isAsciiLower  = Data.Char.isAsciiLower  #-}

data GeneralCategory : Set where
    UppercaseLetter      : GeneralCategory
    LowercaseLetter      : GeneralCategory
    TitlecaseLetter      : GeneralCategory
    ModifierLetter       : GeneralCategory
    OtherLetter          : GeneralCategory
    NonSpacingMark       : GeneralCategory
    SpacingCombiningMark : GeneralCategory
    EnclosingMark        : GeneralCategory
    DecimalNumber        : GeneralCategory
    LetterNumber         : GeneralCategory
    OtherNumber          : GeneralCategory
    ConnectorPunctuation : GeneralCategory
    DashPunctuation      : GeneralCategory
    OpenPunctuation      : GeneralCategory
    ClosePunctuation     : GeneralCategory
    InitialQuote         : GeneralCategory
    FinalQuote           : GeneralCategory
    OtherPunctuation     : GeneralCategory
    MathSymbol           : GeneralCategory
    CurrencySymbol       : GeneralCategory
    ModifierSymbol       : GeneralCategory
    OtherSymbol          : GeneralCategory
    Space                : GeneralCategory
    LineSeparator        : GeneralCategory
    ParagraphSeparator   : GeneralCategory
    Control              : GeneralCategory
    Format               : GeneralCategory
    Surrogate            : GeneralCategory
    PrivateUse           : GeneralCategory
    NotAssigned          : GeneralCategory

{-# COMPILE GHC GeneralCategory = data Data.Char.GeneralCategory
    ( UppercaseLetter      | LowercaseLetter | TitlecaseLetter
    | ModifierLetter       | OtherLetter     | NonSpacingMark
    | SpacingCombiningMark | EnclosingMark   | DecimalNumber
    | LetterNumber         | OtherNumber     | ConnectorPunctuation
    | DashPunctuation      | OpenPunctuation | ClosePunctuation
    | InitialQuote         | FinalQuote      | OtherPunctuation
    | MathSymbol           | CurrencySymbol  | ModifierSymbol
    | OtherSymbol          | Space           | LineSeparator
    | ParagraphSeparator   | Control         | Format
    | Surrogate            | PrivateUse      | NotAssigned
    )
#-}

postulate
    generalCategory : Char → GeneralCategory

{-# COMPILE GHC generalCategory = Data.Char.generalCategory #-}

postulate
    Bounded[GeneralCategory] : Bounded GeneralCategory
    Enum[GeneralCategory]    : Enum GeneralCategory
    Ix[GeneralCategory]      : Ix GeneralCategory
    Read[GeneralCategory]    : Read GeneralCategory
    Show[GeneralCategory]    : Show GeneralCategory
    Eq[GeneralCategory]      : Eq GeneralCategory
    Ord[GeneralCategory]     : Ord GeneralCategory

{-# COMPILE GHC Bounded[GeneralCategory] = AgdaBounded #-}
{-# COMPILE GHC Enum[GeneralCategory]    = AgdaEnum    #-}
{-# COMPILE GHC Ix[GeneralCategory]      = AgdaIx      #-}
{-# COMPILE GHC Read[GeneralCategory]    = AgdaRead    #-}
{-# COMPILE GHC Show[GeneralCategory]    = AgdaShow    #-}
{-# COMPILE GHC Eq[GeneralCategory]      = AgdaEq      #-}
{-# COMPILE GHC Ord[GeneralCategory]     = AgdaOrd     #-}

postulate
    toUpper : Char → Char
    toLower : Char → Char
    toTitle : Char → Char
    digitToInt : Char → Int
    intToDigit : Int → Char
    ord : Char → Int
    chr : Int → Char
    showLitChar : Char → ShowS
    lexLitChar  : ReadS (List Char)
    readLitChar : ReadS Char

{-# COMPILE GHC toUpper     = Data.Char.toUpper     #-}
{-# COMPILE GHC toLower     = Data.Char.toLower     #-}
{-# COMPILE GHC toTitle     = Data.Char.toTitle     #-}
{-# COMPILE GHC digitToInt  = Data.Char.digitToInt  #-}
{-# COMPILE GHC intToDigit  = Data.Char.intToDigit  #-}
{-# COMPILE GHC ord         = Data.Char.ord         #-}
{-# COMPILE GHC chr         = Data.Char.chr         #-}
{-# COMPILE GHC showLitChar = Data.Char.showLitChar #-}
{-# COMPILE GHC lexLitChar  = Data.Char.lexLitChar  #-}
{-# COMPILE GHC readLitChar = Data.Char.readLitChar #-}
