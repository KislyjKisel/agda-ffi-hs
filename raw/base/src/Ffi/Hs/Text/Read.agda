{-# OPTIONS --without-K #-}

module Ffi.Hs.Text.Read where

open import Agda.Builtin.Bool  using (Bool)
open import Agda.Builtin.Char  using (Char)
open import Agda.Builtin.List  using (List)
open import Agda.Builtin.Maybe using (Maybe)
open import Agda.Primitive
open import Ffi.Hs.-base.Class using (Integral) 
open import Ffi.Hs.Data.Either using (Either)
open import Ffi.Hs.Data.Int    using (Int)
open import Ffi.Hs.Text.ParserCombinators.ReadPrec using (ReadPrec)

open Ffi.Hs.-base.Class public
    using (Read)

open import Ffi.Hs.Text.ParserCombinators.ReadP public
    using (ReadS)

open import Ffi.Hs.Text.Read.Lex public
    using (Lexeme)

private
    variable
        aℓ : Level
        A : Set aℓ

postulate
    readsPrec    : ⦃ Read A ⦄ → Int → ReadS A
    readList     : ⦃ Read A ⦄ → ReadS (List A)
    readPrec     : ⦃ Read A ⦄ → ReadPrec A
    readListPrec : ⦃ Read A ⦄ → ReadPrec (List A)

    reads     : ⦃ Read A ⦄ → ReadS A
    read      : ⦃ Read A ⦄ → List Char → A
    readParen : Bool → ReadS A → ReadS A
    lex       : ReadS (List Char)

    lexP                : ReadPrec Lexeme
    parens              : ReadPrec A → ReadPrec A
    readListDefault     : ⦃ Read A ⦄ → ReadS (List A)
    readListPrecDefault : ⦃ Read A ⦄ → ReadPrec (List A)
    readEither          : ⦃ Read A ⦄ → List Char → Either (List Char) A
    readMaybe           : ⦃ Read A ⦄ → List Char → Maybe A

{-# FOREIGN GHC import qualified Text.Read #-}
{-# FOREIGN GHC import MAlonzo.Code.Ffi.Hs.-base.Class (AgdaRead) #-}

{-# COMPILE GHC readsPrec    = \ aℓ a AgdaRead -> Text.Read.readsPrec    #-}
{-# COMPILE GHC readList     = \ aℓ a AgdaRead -> Text.Read.readList     #-}
{-# COMPILE GHC readPrec     = \ aℓ a AgdaRead -> Text.Read.readPrec     #-}
{-# COMPILE GHC readListPrec = \ aℓ a AgdaRead -> Text.Read.readListPrec #-}

{-# COMPILE GHC reads     = \ aℓ a AgdaRead -> Text.Read.reads     #-}
{-# COMPILE GHC read      = \ aℓ a AgdaRead -> Text.Read.read      #-}
{-# COMPILE GHC readParen = \ aℓ a          -> Text.Read.readParen #-}
{-# COMPILE GHC lex       =                    Text.Read.lex       #-}

{-# COMPILE GHC lexP                =                    Text.Read.lexP                #-}
{-# COMPILE GHC parens              = \ aℓ a ->          Text.Read.parens              #-}
{-# COMPILE GHC readListDefault     = \ aℓ a AgdaRead -> Text.Read.readListDefault     #-}
{-# COMPILE GHC readListPrecDefault = \ aℓ a AgdaRead -> Text.Read.readListPrecDefault #-}
{-# COMPILE GHC readEither          = \ aℓ a AgdaRead -> Text.Read.readEither          #-}
{-# COMPILE GHC readMaybe           = \ aℓ a AgdaRead -> Text.Read.readMaybe           #-}
