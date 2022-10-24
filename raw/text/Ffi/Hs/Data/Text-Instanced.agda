{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Text-Instanced where

open import Ffi.Hs.Data.Text

instance
    inst:Lit-FromText[Text] = Lit-FromText[Text]
    inst:IsList[Text]       = IsList[Text]
    inst:Eq[Text]           = Eq[Text]
    inst:Data[Text]         = Data[Text]
    inst:Ord[Text]          = Ord[Text]
    inst:Read[Text]         = Read[Text]
    inst:Show[Text]         = Show[Text]
    inst:IsString[Text]     = IsString[Text]
    inst:Semigroup[Text]    = Semigroup[Text]
    inst:Monoid[Text]       = Monoid[Text]
    inst:PrintfArg[Text]    = PrintfArg[Text]
    inst:Binary[Text]       = Binary[Text]
    inst:NFData[Text]       = NFData[Text]
