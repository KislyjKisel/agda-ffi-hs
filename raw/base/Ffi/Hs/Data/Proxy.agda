{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Proxy where

open import Ffi.Hs.-base.Class
open import Ffi.Hs.-base.Kind  using (IsKind)
open import Agda.Primitive

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Data.Proxy
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ : Level
        A : Set aℓ
        K : Set aℓ

data Proxy (A : Set aℓ) : Set aℓ where
    mkProxy : Proxy A

{-# FOREIGN GHC type AgdaProxy aℓ = Data.Proxy.Proxy #-}
{-# COMPILE GHC Proxy = data(1) AgdaProxy (Data.Proxy.Proxy) #-}

asProxyTypeOf : A → Proxy A → A
asProxyTypeOf x _ = x

data KProxy (K : Set aℓ) ⦃ _ : IsKind K ⦄ : Set aℓ where
    mkKProxy : KProxy K

{-# FOREIGN GHC type AgdaKProxy aℓ k isk = Data.Proxy.KProxy k #-}
{-# COMPILE GHC KProxy = data(3) AgdaKProxy (Data.Proxy.KProxy) #-}

postulate
    Foldable[Proxy]      : Foldable {aℓ} Proxy
    Traversable[Proxy]   : Traversable {aℓ} Proxy
    Functor[Proxy]       : Functor {aℓ} Proxy
    Contravariant[Proxy] : Contravariant {aℓ} Proxy
    Applicative[Proxy]   : Applicative {aℓ} Proxy
    Alternative[Proxy]   : Alternative {aℓ} Proxy
    Monad[Proxy]         : Monad {aℓ} Proxy
    MonadPlus[Proxy]     : MonadPlus {aℓ} Proxy
    Semigroup[Proxy[A]] : Semigroup (Proxy A)
    Monoid[Proxy[A]]    : Monoid (Proxy A)
    Bounded[Proxy[A]]   : Bounded (Proxy A)
    Enum[Proxy[A]]      : Enum (Proxy A)
    Ix[Proxy[A]]        : Ix (Proxy A)
    Read[Proxy[A]]      : Read (Proxy A)
    Show[Proxy[A]]      : Show (Proxy A)
    Eq[Proxy[A]]        : Eq (Proxy A)
    Ord[Proxy[A]]       : Ord (Proxy A)

{-# COMPILE GHC Foldable[Proxy]      = \ aℓ -> AgdaFoldable      #-}
{-# COMPILE GHC Traversable[Proxy]   = \ aℓ -> AgdaTraversable   #-}
{-# COMPILE GHC Functor[Proxy]       = \ aℓ -> AgdaFunctor       #-}
{-# COMPILE GHC Contravariant[Proxy] = \ aℓ -> AgdaContravariant #-}
{-# COMPILE GHC Applicative[Proxy]   = \ aℓ -> AgdaApplicative   #-}
{-# COMPILE GHC Alternative[Proxy]   = \ aℓ -> AgdaAlternative   #-}
{-# COMPILE GHC Monad[Proxy]         = \ aℓ -> AgdaMonad         #-}
{-# COMPILE GHC MonadPlus[Proxy]     = \ aℓ -> AgdaMonadPlus     #-}
{-# COMPILE GHC Semigroup[Proxy[A]] = \ aℓ a -> AgdaSemigroup #-}
{-# COMPILE GHC Monoid[Proxy[A]]    = \ aℓ a -> AgdaMonoid    #-}
{-# COMPILE GHC Bounded[Proxy[A]]   = \ aℓ a -> AgdaBounded   #-}
{-# COMPILE GHC Enum[Proxy[A]]      = \ aℓ a -> AgdaEnum      #-}
{-# COMPILE GHC Ix[Proxy[A]]        = \ aℓ a -> AgdaIx        #-}
{-# COMPILE GHC Read[Proxy[A]]      = \ aℓ a -> AgdaRead      #-}
{-# COMPILE GHC Show[Proxy[A]]      = \ aℓ a -> AgdaShow      #-}
{-# COMPILE GHC Eq[Proxy[A]]        = \ aℓ a -> AgdaEq        #-}
{-# COMPILE GHC Ord[Proxy[A]]       = \ aℓ a -> AgdaOrd       #-}
