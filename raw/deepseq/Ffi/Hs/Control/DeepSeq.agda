{-# OPTIONS --without-K #-}

module Ffi.Hs.Control.DeepSeq where

open import Ffi.Hs.-base.Class using (Monad)
open import Agda.Primitive
open import Ffi.Hs.-base.Unit using (⊤)

{-# FOREIGN GHC
import qualified Control.DeepSeq
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ bℓ cℓ : Level
        A B : Set aℓ
        M : Set aℓ → Set aℓ

infixl 4 _<$!!>_
infixr 0 _$!!_

postulate
    NFData : Set aℓ → Set aℓ
    rnf     : ⦃ NFData A ⦄ → A → ⊤ {lzero}
    deepseq : ⦃ NFData A ⦄ → A → B → B
    force   : ⦃ NFData A ⦄ → A → A
    _$!!_   : ⦃ NFData A ⦄ → (A → B) → A → B
    _<$!!>_ : ⦃ Monad M ⦄ → ⦃ NFData B ⦄ → (A → B) → M A → M B
    rwhnf   : A → ⊤ {lzero}

{-# FOREIGN GHC data AgdaNFData aℓ a = Control.DeepSeq.NFData a => AgdaNFData #-}
{-# COMPILE GHC NFData = type(0) AgdaNFData #-}

{-# COMPILE GHC rnf     = \ aℓ a AgdaNFData               -> Control.DeepSeq.rnf     #-}
{-# COMPILE GHC deepseq = \ aℓ a bℓ b AgdaNFData          -> Control.DeepSeq.deepseq #-}
{-# COMPILE GHC force   = \ aℓ a AgdaNFData               -> Control.DeepSeq.force   #-}
{-# COMPILE GHC _$!!_   = \ aℓ a bℓ b AgdaNFData          -> (Control.DeepSeq.$!!)   #-}
{-# COMPILE GHC _<$!!>_ = \ mℓ m b a AgdaMonad AgdaNFData -> (Control.DeepSeq.<$!!>) #-}
{-# COMPILE GHC rwhnf   = \ aℓ a                          -> Control.DeepSeq.rwhnf   #-}

module _ where
    private variable F : Set aℓ → Set bℓ
    postulate
        NFData1 : (Set aℓ → Set bℓ) → Set (aℓ ⊔ bℓ)
        liftRnf : ⦃ NFData1 F ⦄ → (A → ⊤ {lzero}) → F A → ⊤ {lzero}
        rnf1    : ⦃ NFData1 F ⦄ → ⦃ NFData A ⦄ → F A → ⊤ {lzero}

module _ where
    private variable F : Set aℓ → Set bℓ → Set cℓ
    postulate
        NFData2 : (Set aℓ → Set bℓ → Set cℓ) → Set (aℓ ⊔ bℓ ⊔ cℓ)
        liftRnf2 : ⦃ NFData2 F ⦄ → (A → ⊤ {lzero}) → (B → ⊤ {lzero}) → F A B → ⊤ {lzero}
        rnf2     : ⦃ NFData2 F ⦄ → ⦃ NFData A ⦄ → ⦃ NFData B ⦄ → F A B → ⊤ {lzero}

{-# FOREIGN GHC data AgdaNFData1 aℓ bℓ f = Control.DeepSeq.NFData1 f => AgdaNFData1 #-}
{-# COMPILE GHC NFData1 = type(0) AgdaNFData1 #-}

{-# COMPILE GHC liftRnf = \ aℓ bℓ f a AgdaNFData1            -> Control.DeepSeq.liftRnf #-}
{-# COMPILE GHC rnf1    = \ aℓ bℓ f a AgdaNFData1 AgdaNFData -> Control.DeepSeq.rnf1    #-}

{-# FOREIGN GHC data AgdaNFData2 aℓ bℓ f = Control.DeepSeq.NFData2 f => AgdaNFData2 #-}
{-# COMPILE GHC NFData2 = type(0) AgdaNFData2 #-}

{-# COMPILE GHC liftRnf2 = \ aℓ bℓ cℓ f a b AgdaNFData2                       -> Control.DeepSeq.liftRnf2 #-}
{-# COMPILE GHC rnf2     = \ aℓ bℓ cℓ f a b AgdaNFData2 AgdaNFData AgdaNFData -> Control.DeepSeq.rnf2     #-}

-- todo: instances
