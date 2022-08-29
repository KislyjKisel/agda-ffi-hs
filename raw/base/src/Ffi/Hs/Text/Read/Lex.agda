{-# OPTIONS --without-K #-}

module Ffi.Hs.Text.Read.Lex where

open import Agda.Builtin.Bool  using (Bool)
open import Agda.Builtin.Char  using (Char)
open import Agda.Builtin.Int   using () renaming (Int to Integer)
open import Agda.Builtin.List  using (List)
open import Agda.Builtin.Maybe using (Maybe)
open import Agda.Primitive
open import Ffi.Hs.-base.Class using (Read; Show; Num)
open import Ffi.Hs.-base.Real  using (Rational)
open import Ffi.Hs.-base.Unit  using (⊤)
open import Ffi.Hs.Data.Eq     using (Eq)
open import Ffi.Hs.Data.Int    using (Int)
open import Ffi.Hs.Data.Tuple  using (Tuple2)
open import Ffi.Hs.Text.ParserCombinators.ReadP using (ReadP)

{-# FOREIGN GHC import qualified Text.Read.Lex #-}
{-# FOREIGN GHC import MAlonzo.Code.Ffi.Hs.-base.Class (AgdaShow, AgdaRead, AgdaEq, AgdaNum) #-}

private
    variable
        aℓ : Level
        A : Set aℓ

postulate
    Number : Set
    numberToInteger        : Number → Maybe Integer
    numberToFixed          : Integer → Number → Maybe (Tuple2 Integer Integer)
    numberToRational       : Number → Rational
    numberToRangedRational : Tuple2 Int Int → Number → Maybe Rational

{-# COMPILE GHC Number                 = Text.Read.Lex.Number                 #-}
{-# COMPILE GHC numberToInteger        = Text.Read.Lex.numberToInteger        #-}
{-# COMPILE GHC numberToFixed          = Text.Read.Lex.numberToFixed          #-}
{-# COMPILE GHC numberToRational       = Text.Read.Lex.numberToRational       #-}
{-# COMPILE GHC numberToRangedRational = Text.Read.Lex.numberToRangedRational #-}

data Lexeme : Set where
    char   : Char → Lexeme
    string : List Char → Lexeme
    punc   : List Char → Lexeme
    ident  : List Char → Lexeme
    symbol : List Char → Lexeme
    number : Number → Lexeme
    eof    : Lexeme

{-# COMPILE GHC Lexeme = data Text.Read.Lex.Lexeme
    ( Text.Read.Lex.Char  | Text.Read.Lex.String | Text.Read.Lex.Punc
    | Text.Read.Lex.Ident | Text.Read.Lex.Symbol | Text.Read.Lex.Number
    | Text.Read.Lex.EOF
    ) #-}

module Instances where
    postulate
        Eq[Lexeme]   : Eq Lexeme
        Read[Lexeme] : Read Lexeme
        Show[Lexeme] : Show Lexeme

        Eq[Number]   : Eq Number
        Show[Number] : Show Number

{-# COMPILE GHC Instances.Eq[Lexeme]   = AgdaEq   #-}
{-# COMPILE GHC Instances.Read[Lexeme] = AgdaRead #-}
{-# COMPILE GHC Instances.Show[Lexeme] = AgdaShow #-}

{-# COMPILE GHC Instances.Eq[Number]   = AgdaEq   #-}
{-# COMPILE GHC Instances.Show[Number] = AgdaShow #-}

postulate
    lex          : ReadP Lexeme
    expect       : Lexeme → ReadP (⊤ {lzero})
    hsLex        : ReadP (List Char)
    lexChar      : ReadP Char
    isSymbolChar : Char → Bool

    readBinP     : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → ReadP A
    readIntP     : ⦃ Num A ⦄ → A → (Char → Bool) → (Char → Int) → ReadP A
    readOctP     : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → ReadP A
    readDecP     : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → ReadP A
    readHexP     : ⦃ Eq A ⦄ → ⦃ Num A ⦄ → ReadP A

{-# COMPILE GHC lex          = Text.Read.Lex.lex          #-}
{-# COMPILE GHC expect       = Text.Read.Lex.expect       #-}
{-# COMPILE GHC hsLex        = Text.Read.Lex.hsLex        #-}
{-# COMPILE GHC lexChar      = Text.Read.Lex.lexChar      #-}
{-# COMPILE GHC isSymbolChar = Text.Read.Lex.isSymbolChar #-}

{-# COMPILE GHC readBinP = \ aℓ a AgdaEq AgdaNum -> Text.Read.Lex.readBinP #-}
{-# COMPILE GHC readIntP = \ aℓ a        AgdaNum -> Text.Read.Lex.readIntP #-}
{-# COMPILE GHC readOctP = \ aℓ a AgdaEq AgdaNum -> Text.Read.Lex.readOctP #-}
{-# COMPILE GHC readDecP = \ aℓ a AgdaEq AgdaNum -> Text.Read.Lex.readDecP #-}
{-# COMPILE GHC readHexP = \ aℓ a AgdaEq AgdaNum -> Text.Read.Lex.readHexP #-}
