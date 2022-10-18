{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.CaseInsensitive where

open import Agda.Builtin.Char           using (Char)
open import Agda.Builtin.List           using (List)
open import Agda.Builtin.String         using () renaming (String to Text)
open import Agda.Primitive              using (Level)
open import Ffi.Hs.-base.Class
open import Ffi.Hs.Control.DeepSeq      using (NFData)
open import Ffi.Hs.Data.ByteString      using (ByteString)
open import Ffi.Hs.Data.ByteString.Lazy using (LazyByteString)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Data.CaseInsensitive
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
import MAlonzo.Code.Ffi.Hs.Control.DeepSeq (AgdaNFData (AgdaNFData))
#-}

private
    variable
        sℓ : Level
        A S S₁ S₂ : Set sℓ
        F : Set sℓ → Set sℓ


postulate
    CI : Set sℓ → Set sℓ
    FoldCase : Set sℓ → Set sℓ

    Eq[CI[S]]        : ⦃ Eq S ⦄ → Eq (CI S)
    Data[CI[S]]      : ⦃ Data S ⦄ → Data (CI S)
    Ord[CI[S]]       : ⦃ Ord S ⦄ → Ord (CI S)
    Read[CI[S]]      : ⦃ Read S ⦄ → ⦃ FoldCase S ⦄ → Read (CI S)
    Show[CI[S]]      : ⦃ Show S ⦄ → Show (CI S)
    IsString[CI[S]]  : ⦃ IsString S ⦄ → ⦃ FoldCase S ⦄ → IsString (CI S)
    Semigroup[CI[S]] : ⦃ Semigroup S ⦄ → Semigroup (CI S)
    Monoid[CI[S]]    : ⦃ Monoid S ⦄ → Monoid (CI S)
    NFData[CI[S]]    : ⦃ NFData S ⦄ → NFData (CI S)
    -- todo: Hashable instance for CI
    FoldCase[CI[S]]          : FoldCase (CI S)
    FoldCase[Char]           : FoldCase Char
    FoldCase[ByteString]     : FoldCase ByteString
    FoldCase[LazyByteString] : FoldCase LazyByteString
    FoldCase[Text]           : FoldCase Text
    FoldCase[List[A]]        : ⦃ FoldCase A ⦄ → FoldCase (List A)
    -- todo: FoldCase instance for LazyText

    mk         : ⦃ FoldCase S ⦄ → S → CI S
    original   : CI S → S
    foldedCase : CI S → S
    map        : ⦃ FoldCase S₂ ⦄ → (S₁ → S₂) → CI S₁ → CI S₂
    traverse   : ⦃ FoldCase S₂ ⦄ → ⦃ Applicative F ⦄ → (S₁ → F S₂) → CI S₁ → F (CI S₂)
    foldCase   : ⦃ FoldCase S ⦄ → S → S

{-# FOREIGN GHC type AgdaCI aℓ = Data.CaseInsensitive.CI #-}
{-# COMPILE GHC CI = type(1) AgdaCI #-}

{-# FOREIGN GHC data AgdaFoldCase aℓ a = Data.CaseInsensitive.FoldCase a => AgdaFoldCase #-}
{-# COMPILE GHC FoldCase = type(0) AgdaFoldCase #-}

{-# COMPILE GHC Eq[CI[S]]                = \ sℓ s AgdaEq                    -> AgdaEq        #-}
{-# COMPILE GHC Data[CI[S]]              = \ sℓ s AgdaData                  -> AgdaData      #-}
{-# COMPILE GHC Ord[CI[S]]               = \ sℓ s AgdaOrd                   -> AgdaOrd       #-}
{-# COMPILE GHC Read[CI[S]]              = \ sℓ s AgdaRead AgdaFoldCase     -> AgdaRead      #-}
{-# COMPILE GHC Show[CI[S]]              = \ sℓ s AgdaShow                  -> AgdaShow      #-}
{-# COMPILE GHC IsString[CI[S]]          = \ sℓ s AgdaIsString AgdaFoldCase -> AgdaIsString  #-}
{-# COMPILE GHC Semigroup[CI[S]]         = \ sℓ s AgdaSemigroup             -> AgdaSemigroup #-}
{-# COMPILE GHC Monoid[CI[S]]            = \ sℓ s AgdaMonoid                -> AgdaMonoid    #-}
{-# COMPILE GHC NFData[CI[S]]            = \ sℓ s AgdaNFData                -> AgdaNFData    #-}
{-# COMPILE GHC FoldCase[CI[S]]          = \ sℓ s                           -> AgdaFoldCase  #-}
{-# COMPILE GHC FoldCase[Char]           =                                     AgdaFoldCase  #-}
{-# COMPILE GHC FoldCase[ByteString]     =                                     AgdaFoldCase  #-}
{-# COMPILE GHC FoldCase[LazyByteString] =                                     AgdaFoldCase  #-}
{-# COMPILE GHC FoldCase[Text]           =                                     AgdaFoldCase  #-}
{-# COMPILE GHC FoldCase[List[A]]        = \ aℓ a AgdaFoldCase              -> AgdaFoldCase  #-}

{-# COMPILE GHC mk         = \ sℓ s AgdaFoldCase                            -> Data.CaseInsensitive.mk         #-}
{-# COMPILE GHC original   = \ sℓ s                                         -> Data.CaseInsensitive.original   #-}
{-# COMPILE GHC foldedCase = \ sℓ s                                         -> Data.CaseInsensitive.foldedCase #-}
{-# COMPILE GHC map        = \ s2ℓ s2 s1ℓ s1 AgdaFoldCase                   -> Data.CaseInsensitive.map        #-}
{-# COMPILE GHC traverse   = \ s2ℓ s2 f s1ℓ s1 AgdaFoldCase AgdaApplicative -> Data.CaseInsensitive.traverse   #-}
{-# COMPILE GHC foldCase   = \ sℓ s AgdaFoldCase                            -> Data.CaseInsensitive.foldCase   #-}
