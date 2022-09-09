{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Tuple where

open import Agda.Primitive
open import Ffi.Hs.-base.Class

{-# FOREIGN GHC
import qualified Data.Tuple
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Class
    ( AgdaData, AgdaMonoid, AgdaSemigroup
    , AgdaShow, AgdaRead, AgdaEq, AgdaOrd
    , AgdaBounded, AgdaEnum, AgdaIx
    )
#-}

private
    variable
        aℓ bℓ cℓ dℓ eℓ : Level
        A : Set aℓ
        B : Set bℓ
        C : Set cℓ
        D : Set dℓ
        E : Set eℓ

record Tuple2 (A : Set aℓ) (B : Set bℓ) : Set (aℓ ⊔ bℓ) where
    constructor tuple2
    field
        fst : A
        snd : B

open Tuple2 public

data Tuple3 (A : Set aℓ) (B : Set bℓ) (C : Set cℓ) : Set (aℓ ⊔ bℓ ⊔ cℓ) where
    tuple3 : A → B → C → Tuple3 A B C

data Tuple4 (A : Set aℓ) (B : Set bℓ) (C : Set cℓ) (D : Set dℓ) : Set (aℓ ⊔ bℓ ⊔ cℓ ⊔ dℓ) where
    tuple4 : A → B → C → D → Tuple4 A B C D

data Tuple5 (A : Set aℓ) (B : Set bℓ) (C : Set cℓ) (D : Set dℓ) (E : Set eℓ) : Set (aℓ ⊔ bℓ ⊔ cℓ ⊔ dℓ ⊔ eℓ) where
    tuple5 : A → B → C → D → E → Tuple5 A B C D E

{-# FOREIGN GHC type AgdaTuple2 aℓ bℓ          = (,)    #-}
{-# FOREIGN GHC type AgdaTuple3 aℓ bℓ cℓ       = (,,)   #-}
{-# FOREIGN GHC type AgdaTuple4 aℓ bℓ cℓ dℓ    = (,,,)  #-}
{-# FOREIGN GHC type AgdaTuple5 aℓ bℓ cℓ dℓ eℓ = (,,,,) #-}

{-# COMPILE GHC Tuple2 = data(2) AgdaTuple2 ((,))    #-}
{-# COMPILE GHC Tuple3 = data(3) AgdaTuple3 ((,,))   #-}
{-# COMPILE GHC Tuple4 = data(4) AgdaTuple4 ((,,,))  #-}
{-# COMPILE GHC Tuple5 = data(5) AgdaTuple5 ((,,,,)) #-}

data Solo (A : Set aℓ) : Set aℓ where
    mkSolo : A → Solo A

{-# FOREIGN GHC type AgdaSolo aℓ = Data.Tuple.Solo #-}
{-# COMPILE GHC Solo = data(1) AgdaSolo (Data.Tuple.Solo) #-}

postulate
    curry   : (Tuple2 A B → C) → A → B → C
    uncurry : (A → B → C) → Tuple2 A B → C
    swap    : Tuple2 A B → Tuple2 B A

{-# COMPILE GHC curry   = \ aℓ bℓ cℓ a b c -> Data.Tuple.curry   #-}
{-# COMPILE GHC uncurry = \ aℓ bℓ cℓ a b c -> Data.Tuple.uncurry #-}
{-# COMPILE GHC swap    = \ aℓ bℓ a b      -> Data.Tuple.swap    #-}

postulate
    MonadFix[Solo]    : MonadFix {aℓ} Solo
    MonadZip[Solo]    : MonadZip {aℓ} Solo
    Foldable[Solo]    : Foldable {aℓ} Solo
    Traversable[Solo] : Traversable {aℓ} Solo
    Applicative[Solo] : Applicative {aℓ} Solo
    Functor[Solo]     : Functor {aℓ} Solo
    Monad[Solo]       : Monad {aℓ} Solo

    Data[Solo[A]]      : ⦃ Data A ⦄ → Data (Solo A)
    Monoid[Solo[A]]    : ⦃ Monoid A ⦄ → Monoid (Solo A)
    Semigroup[Solo[A]] : ⦃ Semigroup A ⦄ → Semigroup (Solo A)
    Bounded[Solo[A]]   : ⦃ Bounded A ⦄ → Bounded (Solo A)
    Enum[Solo[A]]      : ⦃ Enum A ⦄ → Enum (Solo A)
    Ix[Solo[A]]        : ⦃ Ix A ⦄ → Ix (Solo A)
    Read[Solo[A]]      : ⦃ Read A ⦄ → Read (Solo A)
    Show[Solo[A]]      : ⦃ Show A ⦄ → Show (Solo A)
    Eq[Solo[A]]        : ⦃ Eq A ⦄ → Eq (Solo A)
    Ord[Solo[A]]       : ⦃ Ord A ⦄ → Ord (Solo A)

-- # *(\w+)\[(\w+(?:\[A\])?)\] *: .*
-- #{-# COMPILE GHC $1[$2] = Agda$1 #-}

{-# COMPILE GHC MonadFix[Solo]    = \ aℓ -> AgdaMonadFix    #-}
{-# COMPILE GHC MonadZip[Solo]    = \ aℓ -> AgdaMonadZip    #-}
{-# COMPILE GHC Foldable[Solo]    = \ aℓ -> AgdaFoldable    #-}
{-# COMPILE GHC Traversable[Solo] = \ aℓ -> AgdaTraversable #-}
{-# COMPILE GHC Applicative[Solo] = \ aℓ -> AgdaApplicative #-}
{-# COMPILE GHC Functor[Solo]     = \ aℓ -> AgdaFunctor     #-}
{-# COMPILE GHC Monad[Solo]       = \ aℓ -> AgdaMonad       #-}

{-# COMPILE GHC Data[Solo[A]]      = \ aℓ a AgdaData      -> AgdaData      #-}
{-# COMPILE GHC Monoid[Solo[A]]    = \ aℓ a AgdaMonoid    -> AgdaMonoid    #-}
{-# COMPILE GHC Semigroup[Solo[A]] = \ aℓ a AgdaSemigroup -> AgdaSemigroup #-}
{-# COMPILE GHC Bounded[Solo[A]]   = \ aℓ a AgdaBounded   -> AgdaBounded   #-}
{-# COMPILE GHC Enum[Solo[A]]      = \ aℓ a AgdaEnum      -> AgdaEnum      #-}
{-# COMPILE GHC Ix[Solo[A]]        = \ aℓ a AgdaIx        -> AgdaIx        #-}
{-# COMPILE GHC Read[Solo[A]]      = \ aℓ a AgdaRead      -> AgdaRead      #-}
{-# COMPILE GHC Show[Solo[A]]      = \ aℓ a AgdaShow      -> AgdaShow      #-}
{-# COMPILE GHC Eq[Solo[A]]        = \ aℓ a AgdaEq        -> AgdaEq        #-}
{-# COMPILE GHC Ord[Solo[A]]       = \ aℓ a AgdaOrd       -> AgdaOrd       #-}

-- todo: instances for Tuple2/3/4/5
