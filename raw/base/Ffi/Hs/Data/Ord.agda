{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Ord where

open import Agda.Builtin.Bool  using (Bool)
open import Agda.Primitive
open import Ffi.Hs.-base.Class
open import Ffi.Hs.-base.Kind  using (IsKind)
open import Ffi.Hs.Data.Tuple  using (Tuple2)

open Ffi.Hs.-base.Class public
    using (Ord)

{-# FOREIGN GHC {-# LANGUAGE DataKinds #-} #-}
{-# FOREIGN GHC
import qualified Data.Ord
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Kind (AgdaIsKind(AgdaIsKind))
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ bℓ : Level
        A : Set aℓ
        B : Set bℓ

data Ordering : Set where
    LT EQ GT : Ordering

{-# COMPILE GHC Ordering = data Data.Ord.Ordering
    ( Data.Ord.LT
    | Data.Ord.EQ
    | Data.Ord.GT
    ) #-}

postulate
    Data[Ordering]      : Data Ordering
    Monoid[Ordering]    : Monoid Ordering
    Semigroup[Ordering] : Semigroup Ordering
    Bounded[Ordering]   : Bounded Ordering
    Enum[Ordering]      : Enum Ordering
    Ix[Ordering]        : Ix Ordering
    Read[Ordering]      : Read Ordering
    Show[Ordering]      : Show Ordering
    Eq[Ordering]        : Eq Ordering
    Ord[Ordering]       : Ord Ordering

{-# COMPILE GHC Data[Ordering]      = AgdaData      #-}
{-# COMPILE GHC Monoid[Ordering]    = AgdaMonoid    #-}
{-# COMPILE GHC Semigroup[Ordering] = AgdaSemigroup #-}
{-# COMPILE GHC Bounded[Ordering]   = AgdaBounded   #-}
{-# COMPILE GHC Enum[Ordering]      = AgdaEnum      #-}
{-# COMPILE GHC Ix[Ordering]        = AgdaIx        #-}
{-# COMPILE GHC Read[Ordering]      = AgdaRead      #-}
{-# COMPILE GHC Show[Ordering]      = AgdaShow      #-}
{-# COMPILE GHC Eq[Ordering]        = AgdaEq        #-}
{-# COMPILE GHC Ord[Ordering]       = AgdaOrd       #-}

infix 4 _<_ _<=_ _>_ _>=_

postulate
    _<_  : ⦃ Ord A ⦄ → A → A → Bool
    _<=_ : ⦃ Ord A ⦄ → A → A → Bool
    _>_  : ⦃ Ord A ⦄ → A → A → Bool
    _>=_ : ⦃ Ord A ⦄ → A → A → Bool

    compare : ⦃ Ord A ⦄ → A → A → Ordering
    min     : ⦃ Ord A ⦄ → A → A → A
    max     : ⦃ Ord A ⦄ → A → A → A

    comparing : ⦃ Ord A ⦄ → (B → A) → B → B → Ordering
    clamp     : ⦃ Ord A ⦄ → Tuple2 A A → A → A

{-# COMPILE GHC _<_  = \ aℓ a AgdaOrd -> (Data.Ord.<)  #-}
{-# COMPILE GHC _<=_ = \ aℓ a AgdaOrd -> (Data.Ord.<=) #-}
{-# COMPILE GHC _>_  = \ aℓ a AgdaOrd -> (Data.Ord.>)  #-}
{-# COMPILE GHC _>=_ = \ aℓ a AgdaOrd -> (Data.Ord.>=) #-}

{-# COMPILE GHC compare = \ aℓ a AgdaOrd -> Data.Ord.compare #-}
{-# COMPILE GHC min     = \ aℓ a AgdaOrd -> Data.Ord.min     #-}
{-# COMPILE GHC max     = \ aℓ a AgdaOrd -> Data.Ord.max     #-}

{-# COMPILE GHC comparing = \ aℓ bℓ a b AgdaOrd -> Data.Ord.comparing #-}
{-# COMPILE GHC clamp     = \ aℓ a AgdaOrd      -> Data.Ord.clamp     #-}

postulate
    Ord[A]⇒Eq[A] : ⦃ Ord A ⦄ → Eq A

{-# COMPILE GHC Ord[A]⇒Eq[A] = \ aℓ a AgdaOrd -> AgdaEq #-}

postulate
    `Ordering : Set₁
    `LT `EQ `GT : `Ordering
    IsKind[`Ordering] : IsKind `Ordering

{-# COMPILE GHC `Ordering = type Data.Ord.Ordering #-}
{-# COMPILE GHC `LT = type 'Data.Ord.LT #-}
{-# COMPILE GHC `EQ = type 'Data.Ord.EQ #-}
{-# COMPILE GHC `GT = type 'Data.Ord.GT #-}
{-# COMPILE GHC IsKind[`Ordering] = AgdaIsKind #-}

postulate
    Down    : Set aℓ → Set aℓ
    mkDown  : A → Down A
    getDown : Down A → A

    MonadFix[Down]    : MonadFix {aℓ} Down
    MonadZip[Down]    : MonadZip {aℓ} Down
    Foldable[Down]    : Foldable {aℓ} Down
    Traversable[Down] : Traversable {aℓ} Down
    Applicative[Down] : Applicative {aℓ} Down
    Functor[Down]     : Functor {aℓ} Down
    Monad[Down]       : Monad {aℓ} Down
    Data[Down[A]]        : ⦃ Data A ⦄ → Data (Down A)
    Storable[Down[A]]    : ⦃ Storable A ⦄ → Storable (Down A)
    Monoid[Down[A]]      : ⦃ Monoid A ⦄ → Monoid (Down A)
    Semigroup[Down[A]]   : ⦃ Semigroup A ⦄ → Semigroup (Down A)
    Bits[Down[A]]        : ⦃ Bits A ⦄ → Bits (Down A)
    FiniteBits[Down[A]]  : ⦃ FiniteBits A ⦄ → FiniteBits (Down A)
    Bounded[Down[A]]     : ⦃ Bounded A ⦄ → Bounded (Down A)
    Floating[Down[A]]    : ⦃ Floating A ⦄ → Floating (Down A)
    RealFloat[Down[A]]   : ⦃ RealFloat A ⦄ → RealFloat (Down A)
    Ix[Down[A]]          : ⦃ Ix A ⦄ → Ix (Down A)
    Num[Down[A]]         : ⦃ Num A ⦄ → Num (Down A)
    Read[Down[A]]        : ⦃ Read A ⦄ → Read (Down A)
    Fractional[Down[A]]  : ⦃ Fractional A ⦄ → Fractional (Down A)
    Real[Down[A]]        : ⦃ Real A ⦄ → Real (Down A)
    RealFrac[Down[A]]    : ⦃ RealFrac A ⦄ → RealFrac (Down A)
    Show[Down[A]]        : ⦃ Show A ⦄ → Show (Down A)
    Eq[Down[A]]          : ⦃ Eq A ⦄ → Eq (Down A)
    Ord[Down[A]]         : ⦃ Ord A ⦄ → Ord (Down A)

{-# FOREIGN GHC type AgdaDown aℓ = Data.Ord.Down #-}
{-# COMPILE GHC Down = type(1) AgdaDown #-}
{-# COMPILE GHC mkDown    = \ aℓ a         -> Data.Ord.Down    #-}
{-# COMPILE GHC getDown   = \ aℓ a         -> Data.Ord.getDown #-}

{-# COMPILE GHC MonadFix[Down]    = \ aℓ -> AgdaMonadFix    #-}
{-# COMPILE GHC MonadZip[Down]    = \ aℓ -> AgdaMonadZip    #-}
{-# COMPILE GHC Foldable[Down]    = \ aℓ -> AgdaFoldable    #-}
{-# COMPILE GHC Traversable[Down] = \ aℓ -> AgdaTraversable #-}
{-# COMPILE GHC Applicative[Down] = \ aℓ -> AgdaApplicative #-}
{-# COMPILE GHC Functor[Down]     = \ aℓ -> AgdaFunctor     #-}
{-# COMPILE GHC Monad[Down]       = \ aℓ -> AgdaMonad       #-}
{-# COMPILE GHC Data[Down[A]]       = \ aℓ a AgdaData       -> AgdaData       #-}
{-# COMPILE GHC Storable[Down[A]]   = \ aℓ a AgdaStorable   -> AgdaStorable   #-}
{-# COMPILE GHC Monoid[Down[A]]     = \ aℓ a AgdaMonoid     -> AgdaMonoid     #-}
{-# COMPILE GHC Semigroup[Down[A]]  = \ aℓ a AgdaSemigroup  -> AgdaSemigroup  #-}
{-# COMPILE GHC Bits[Down[A]]       = \ aℓ a AgdaBits       -> AgdaBits       #-}
{-# COMPILE GHC FiniteBits[Down[A]] = \ aℓ a AgdaFiniteBits -> AgdaFiniteBits #-}
{-# COMPILE GHC Bounded[Down[A]]    = \ aℓ a AgdaBounded    -> AgdaBounded    #-}
{-# COMPILE GHC Floating[Down[A]]   = \ aℓ a AgdaFloating   -> AgdaFloating   #-}
{-# COMPILE GHC RealFloat[Down[A]]  = \ aℓ a AgdaRealFloat  -> AgdaRealFloat  #-}
{-# COMPILE GHC Ix[Down[A]]         = \ aℓ a AgdaIx         -> AgdaIx         #-}
{-# COMPILE GHC Num[Down[A]]        = \ aℓ a AgdaNum        -> AgdaNum        #-}
{-# COMPILE GHC Read[Down[A]]       = \ aℓ a AgdaRead       -> AgdaRead       #-}
{-# COMPILE GHC Fractional[Down[A]] = \ aℓ a AgdaFractional -> AgdaFractional #-}
{-# COMPILE GHC Real[Down[A]]       = \ aℓ a AgdaReal       -> AgdaReal       #-}
{-# COMPILE GHC RealFrac[Down[A]]   = \ aℓ a AgdaRealFrac   -> AgdaRealFrac   #-}
{-# COMPILE GHC Show[Down[A]]       = \ aℓ a AgdaShow       -> AgdaShow       #-}
{-# COMPILE GHC Eq[Down[A]]         = \ aℓ a AgdaEq         -> AgdaEq         #-}
{-# COMPILE GHC Ord[Down[A]]        = \ aℓ a AgdaOrd        -> AgdaOrd        #-}
