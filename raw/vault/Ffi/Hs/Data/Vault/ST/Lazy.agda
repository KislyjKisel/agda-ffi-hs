{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Vault.ST.Lazy where

open import Agda.Builtin.Maybe      using (Maybe)
open import Agda.Primitive          using (Level)
open import Ffi.Hs.-base.Class      using (Semigroup; Monoid)
open import Ffi.Hs.Control.Monad.ST using (ST)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Data.Vault.ST.Lazy
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ : Level
        S : Set
        A : Set aℓ


postulate
    Vault : Set → Set aℓ

    Semigroup[Vault[S]] : Semigroup (Vault {aℓ} S)
    Monoid[Vault[S]]    : Monoid (Vault {aℓ} S)

    Key : Set → Set aℓ → Set

    empty  : Vault {aℓ} S
    newKey : ST S (Key S A)
    lookup : {A : Set aℓ} → Key S A → Vault {aℓ} S → Maybe A
    insert : {A : Set aℓ} → Key S A → A → Vault {aℓ} S → Vault {aℓ} S
    adjust : {A : Set aℓ} → (A → A) → Key S A → Vault {aℓ} S → Vault {aℓ} S
    delete : {A : Set aℓ} → Key S A → Vault {aℓ} S → Vault {aℓ} S
    union  : Vault {aℓ} S → Vault {aℓ} S → Vault {aℓ} S 

    Locker : Set → Set aℓ

    lock   : {A : Set aℓ} → Key S A → A → Locker {aℓ} S
    unlock : {A : Set aℓ} → Key S A → Locker {aℓ} S → Maybe A

{-# FOREIGN GHC type AgdaVault ℓ = Data.Vault.ST.Lazy.Vault #-}
{-# COMPILE GHC Vault = type(1) AgdaVault #-}

{-# COMPILE GHC Semigroup[Vault[S]] = \ ℓ s -> AgdaSemigroup #-}
{-# COMPILE GHC Monoid[Vault[S]]    = \ ℓ s -> AgdaMonoid    #-}

{-# FOREIGN GHC type AgdaKey aℓ = Data.Vault.ST.Lazy.Key #-}
{-# COMPILE GHC Key = type(1) AgdaKey #-}

{-# COMPILE GHC empty  = \ ℓ s    -> Data.Vault.ST.Lazy.empty  #-}
{-# COMPILE GHC newKey = \ aℓ s a -> Data.Vault.ST.Lazy.newKey #-}
{-# COMPILE GHC lookup = \ s aℓ a -> Data.Vault.ST.Lazy.lookup #-}
{-# COMPILE GHC insert = \ s aℓ a -> Data.Vault.ST.Lazy.insert #-}
{-# COMPILE GHC adjust = \ aℓ s a -> Data.Vault.ST.Lazy.adjust #-}
{-# COMPILE GHC delete = \ aℓ s a -> Data.Vault.ST.Lazy.delete #-}
{-# COMPILE GHC union  = \ aℓ s   -> Data.Vault.ST.Lazy.union  #-} 

{-# FOREIGN GHC type AgdaLocker ℓ = Data.Vault.ST.Lazy.Locker #-}
{-# COMPILE GHC Locker = type(1) AgdaLocker #-}

{-# COMPILE GHC lock   = \ aℓ s a -> Data.Vault.ST.Lazy.lock   #-}
{-# COMPILE GHC unlock = \ aℓ s a -> Data.Vault.ST.Lazy.unlock #-}
