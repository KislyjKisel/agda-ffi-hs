{-# OPTIONS --without-K #-}

module Ffi.Hs.Control.Applicative where

open import Agda.Builtin.Bool           using (Bool)
open import Agda.Builtin.List           using (List)
open import Agda.Builtin.Maybe          using (Maybe)
open import Agda.Primitive
open import Ffi.Hs.Data.Int             using (Int)
open import Ffi.Hs.Data.Tuple using (Tuple2)
open import Ffi.Hs.-base.Unit  using (⊤)

private
    variable
        aℓ bℓ cℓ dℓ fℓ : Level
        A : Set aℓ
        B : Set bℓ
        C : Set cℓ
        D : Set dℓ
        F T : Set fℓ → Set fℓ

infixl 4 _<*>_ _*>_ _<*_ _<**>_
infixl 3 _<|>_

postulate
    Applicative : (Set aℓ → Set aℓ) → Set aℓ
    pure   : ⦃ Applicative F ⦄ → A → F A
    _<*>_  : ⦃ Applicative F ⦄ → F (A → B) → F A → F B
    liftA2 : ⦃ Applicative F ⦄ → (A → B → C) → F A → F B → F C
    _*>_   : ⦃ Applicative F ⦄ → F A → F B → F B
    _<*_   : ⦃ Applicative F ⦄ → F A → F B → F A

    Alternative : (Set aℓ → Set aℓ) → Set aℓ
    empty : ⦃ Alternative F ⦄ → F A
    _<|>_ : ⦃ Alternative F ⦄ → F A → F A → F A
    some  : ⦃ Alternative F ⦄ → F A → F (List A)
    many  : ⦃ Alternative F ⦄ → F A → F (List A)

    WrappedArrow : (Set aℓ → Set bℓ → Set bℓ) → Set aℓ → Set bℓ → Set bℓ
    mkWrappedArrow : {A : Set aℓ → Set bℓ → Set bℓ} → A B C → WrappedArrow A B C
    unwrapArrow    : {A : Set aℓ → Set bℓ → Set bℓ} → WrappedArrow A B C → A B C

    ZipList : Set aℓ → Set aℓ
    mkZipList  : List A → ZipList A
    getZipList : ZipList A → List A

    _<**>_   : ⦃ Applicative F ⦄ → F A → F (A → B) → F B
    liftA    : ⦃ Applicative F ⦄ → (A → B) → F A → F B
    liftA3   : ⦃ Applicative F ⦄ → (A → B → C → D) → F A → F B → F C → F D
    optional : ⦃ Alternative F ⦄ → F A → F (Maybe A)
 
    filterM      : ⦃ Applicative F ⦄ → (A → F Bool) → List A → F (List A)
    mapAndUnzipM : ⦃ Applicative F ⦄ → (A → F (Tuple2 B C)) → List A → F (Tuple2 (List B) (List C))
    zipWithM     : ⦃ Applicative F ⦄ → (A → B → F C) → List A → List B → F (List C)
    zipWithM_    : ⦃ Applicative F ⦄ → (A → B → F C) → List A → List B → F ⊤
    replicateM   : ⦃ Applicative F ⦄ → Int → F A → F (List A)
    replicateM_  : ⦃ Applicative F ⦄ → Int → F A → F ⊤
    
    guard  : ⦃ Alternative F ⦄ → Bool → F ⊤
    when   : ⦃ Applicative F ⦄ → Bool → F ⊤ → F ⊤
    unless : ⦃ Applicative F ⦄ → Bool → F ⊤ → F ⊤ 

{-# FOREIGN GHC import qualified Control.Applicative               #-}

{-# FOREIGN GHC data AgdaApplicative aℓ f = Control.Applicative.Applicative f => AgdaApplicative #-}
{-# COMPILE GHC Applicative = type(0) AgdaApplicative #-}
{-# COMPILE GHC pure   = \ ℓ f a     AgdaApplicative -> Control.Applicative.pure   #-}
{-# COMPILE GHC _<*>_  = \ ℓ f a b   AgdaApplicative -> (Control.Applicative.<*>)  #-}
{-# COMPILE GHC liftA2 = \ ℓ f a b c AgdaApplicative -> Control.Applicative.liftA2 #-}
{-# COMPILE GHC _*>_   = \ ℓ f a b   AgdaApplicative -> (Control.Applicative.*>)   #-}
{-# COMPILE GHC _<*_   = \ ℓ f a b   AgdaApplicative -> (Control.Applicative.<*)   #-}

{-# FOREIGN GHC data AgdaAlternative aℓ f = Control.Applicative.Alternative f => AgdaAlternative #-}
{-# COMPILE GHC Alternative = type(0) AgdaAlternative                     #-}
{-# COMPILE GHC empty = \ ℓ f a AgdaAlternative -> Control.Applicative.empty #-}
{-# COMPILE GHC _<|>_ = \ ℓ f a AgdaAlternative -> (Control.Applicative.<|>) #-}
{-# COMPILE GHC some  = \ ℓ f a AgdaAlternative -> Control.Applicative.some  #-}
{-# COMPILE GHC many  = \ ℓ f a AgdaAlternative -> Control.Applicative.many  #-}

{-# FOREIGN GHC type AgdaWrappedArrow aℓ bℓ = WrappedArrow #-}
{-# COMPILE GHC WrappedArrow = type(2) AgdaWrappedArrow    #-}
{-# COMPILE GHC mkWrappedArrow = \ aℓ bℓ a b c -> Control.Applicative.WrappedArrow #-}
{-# COMPILE GHC unwrapArrow    = \ aℓ bℓ a b c -> Control.Applicative.unwrapArrow  #-}

{-# FOREIGN GHC type AgdaZipList aℓ = Control.Applicative.ZipList #-}
{-# COMPILE GHC ZipList = type(1) AgdaZipList                     #-}
{-# COMPILE GHC mkZipList  = \ aℓ a -> Control.Applicative.ZipList    #-}
{-# COMPILE GHC getZipList = \ aℓ a -> Control.Applicative.getZipList #-}

{-# COMPILE GHC _<**>_   = \ ℓ f a b     AgdaApplicative              -> (Control.Applicative.<**>)   #-}
{-# COMPILE GHC liftA    = \ ℓ f a b     AgdaApplicative              -> Control.Applicative.liftA    #-}
{-# COMPILE GHC liftA3   = \ ℓ f a b c d AgdaApplicative              -> Control.Applicative.liftA3   #-}
{-# COMPILE GHC optional = \ ℓ f a       AgdaAlternative              -> Control.Applicative.optional #-}

{-# COMPILE GHC filterM      = \ ℓ f a AgdaApplicative            -> Control.Monad.filterM      #-}
{-# COMPILE GHC mapAndUnzipM = \ fℓ aℓ f a b c AgdaApplicative    -> Control.Monad.mapAndUnzipM #-}
{-# COMPILE GHC zipWithM     = \ fℓ aℓ bℓ f a b c AgdaApplicative -> Control.Monad.zipWithM     #-}
{-# COMPILE GHC zipWithM_    = \ fℓ aℓ bℓ f a b c AgdaApplicative -> Control.Monad.zipWithM_    #-}
{-# COMPILE GHC replicateM   = \ ℓ f a b AgdaApplicative          -> Control.Monad.replicateM   #-}
{-# COMPILE GHC replicateM_  = \ ℓ f a AgdaApplicative            -> Control.Monad.replicateM_  #-}

{-# COMPILE GHC guard  = \ ℓ f AgdaAlternative -> Control.Monad.guard  #-}
{-# COMPILE GHC when   = \ ℓ f AgdaApplicative -> Control.Monad.when   #-}
{-# COMPILE GHC unless = \ ℓ f AgdaApplicative -> Control.Monad.unless #-}
