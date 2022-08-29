{-# OPTIONS --without-K #-}

module Ffi.Hs.Text.ParserCombinators.ReadP where

open import Agda.Builtin.Bool           using (Bool)
open import Agda.Builtin.Char           using (Char)
open import Agda.Builtin.List           using (List)
open import Agda.Primitive
open import Ffi.Hs.Data.Tuple using (Tuple2)
open import Ffi.Hs.-base.Unit  using (⊤)
open import Ffi.Hs.Data.Int             using (Int)

private
    variable
        aℓ bℓ cℓ : Level
        A : Set aℓ
        B : Set bℓ
        C : Set cℓ

ReadS : Set aℓ → Set aℓ
ReadS A = List Char → List (Tuple2 A (List Char))

postulate
    ReadP : Set aℓ → Set aℓ

    get    : ReadP Char
    look   : ReadP (List Char)
    _+++_  : ReadP A → ReadP A → ReadP A
    _<++_  : ReadP A → ReadP A → ReadP A
    gather : ReadP A → ReadP (Tuple2 (List Char) A)

    pfail      : ReadP A
    eof        : ∀{ℓ} → ReadP (⊤ {ℓ})
    satisfy    : (Char → Bool) → ReadP Char
    char       : Char → ReadP Char
    string     : List Char → ReadP (List Char)
    munch      : (Char → Bool) → ReadP (List Char)
    munch1     : (Char → Bool) → ReadP (List Char)
    skipSpaces : ReadP (⊤ {lzero})
    choice     : List (ReadP A) → ReadP A
    count      : Int → ReadP A → ReadP (List A)
    between    : ReadP A → ReadP B → ReadP C → ReadP C
    option     : A → ReadP A → ReadP A
    optional   : ∀{ℓ} → ReadP A → ReadP (⊤ {ℓ})
    many       : ReadP A → ReadP (List A)
    many1      : ReadP A → ReadP (List A)
    skipMany   : ∀{ℓ} → ReadP A → ReadP (⊤ {ℓ})
    skipMany1  : ∀{ℓ} → ReadP A → ReadP (⊤ {ℓ})
    sepBy      : ReadP A → ReadP B → ReadP (List A)
    sepBy1     : ReadP A → ReadP B → ReadP (List A)
    endBy      : ReadP A → ReadP B → ReadP (List A)
    endBy1     : ReadP A → ReadP B → ReadP (List A)
    chainr     : ReadP A → ReadP (A → A → A) → A → ReadP A
    chainl     : ReadP A → ReadP (A → A → A) → A → ReadP A
    chainl1    : ReadP A → ReadP (A → A → A) → ReadP A
    chainr1    : ReadP A → ReadP (A → A → A) → ReadP A
    manyTill   : ReadP A → ReadP B → ReadP (List A)

    readP-to-S : ReadP A → ReadS A
    readS-to-P : ReadS A → ReadP A

    -- todo: properties?