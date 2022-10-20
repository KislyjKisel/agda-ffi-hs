{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Vault.Lazy where

open import Agda.Builtin.IO                 using (IO)
open import Agda.Builtin.Maybe              using (Maybe)
open import Agda.Primitive
open import Ffi.Hs.Data.Vault.ST.Lazy as ST using ()
open import Ffi.Hs.GHC.Exts                 using (RealWorld)

{-# FOREIGN GHC
import qualified Data.Vault.Lazy
#-}

private
    variable
        aℓ : Level
        A : Set aℓ


Vault : ∀{ℓ} → Set ℓ
Vault {ℓ} = ST.Vault {ℓ} RealWorld

Key : Set aℓ → Set 
Key = ST.Key RealWorld

Locker : ∀{ℓ} → Set ℓ
Locker {ℓ} = ST.Locker {ℓ} RealWorld

postulate
    empty  : Vault {aℓ}
    newKey : IO (Key A)
    lookup : {A : Set aℓ} → Key A → Vault {aℓ} → Maybe A
    insert : {A : Set aℓ} → Key A → A → Vault {aℓ} → Vault {aℓ}
    adjust : {A : Set aℓ} → (A → A) → Key A → Vault {aℓ} → Vault {aℓ}
    delete : {A : Set aℓ} → Key A → Vault {aℓ} → Vault {aℓ}
    union  : {A : Set aℓ} → Vault {aℓ} → Vault {aℓ} → Vault {aℓ}
    lock   : {A : Set aℓ} → Key A → A → Locker {aℓ}
    unlock : {A : Set aℓ} → Key A → Locker {aℓ} → Maybe A

{-# COMPILE GHC empty  = \ aℓ   -> Data.Vault.Lazy.empty  #-}
{-# COMPILE GHC newKey = \ aℓ a -> Data.Vault.Lazy.newKey #-}
{-# COMPILE GHC lookup = \ aℓ a -> Data.Vault.Lazy.lookup #-}
{-# COMPILE GHC insert = \ aℓ a -> Data.Vault.Lazy.insert #-}
{-# COMPILE GHC adjust = \ aℓ a -> Data.Vault.Lazy.adjust #-}
{-# COMPILE GHC delete = \ aℓ a -> Data.Vault.Lazy.delete #-}
{-# COMPILE GHC union  = \ aℓ a -> Data.Vault.Lazy.union  #-}
{-# COMPILE GHC lock   = \ aℓ a -> Data.Vault.Lazy.lock   #-}
{-# COMPILE GHC unlock = \ aℓ a -> Data.Vault.Lazy.unlock #-}
