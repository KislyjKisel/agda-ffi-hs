{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Tuple where

open import Agda.Primitive
open import Ffi.Hs.-base.Class

{-# FOREIGN GHC
import qualified Data.Tuple
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
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


postulate
    Show[Tuple2[A,B]]      : ⦃ Show A ⦄ → ⦃ Show B ⦄ → Show (Tuple2 A B)
    Read[Tuple2[A,B]]      : ⦃ Read A ⦄ → ⦃ Read B ⦄ → Read (Tuple2 A B)
    Eq[Tuple2[A,B]]        : ⦃ Eq A ⦄ → ⦃ Eq B ⦄ → Eq (Tuple2 A B)
    Ord[Tuple2[A,B]]       : ⦃ Ord A ⦄ → ⦃ Ord B ⦄ → Ord (Tuple2 A B)
    Foldable[Tuple2[A]]    : Foldable (Tuple2 {bℓ = bℓ} A)
    Traversable[Tuple2[A]] : Traversable (Tuple2 {aℓ} {bℓ = aℓ ⊔ bℓ} A)
    Functor[Tuple2[A]]     : Functor (Tuple2 {aℓ} {bℓ = aℓ ⊔ bℓ} A)
    Bifunctor[Tuple2]      : Bifunctor {aℓ} {bℓ} Tuple2
    Bifoldable[Tuple2]     : Bifoldable {aℓ} {bℓ} Tuple2
    Bitraversable[Tuple2]  : Bitraversable {aℓ} Tuple2

    Show[Tuple3[A,B,C]]      : ⦃ Show A ⦄ → ⦃ Show B ⦄ → ⦃ Show C ⦄ → Show (Tuple3 A B C)
    Read[Tuple3[A,B,C]]      : ⦃ Read A ⦄ → ⦃ Read B ⦄ → ⦃ Read C ⦄ → Read (Tuple3 A B C)
    Eq[Tuple3[A,B,C]]        : ⦃ Eq A ⦄ → ⦃ Eq B ⦄ → ⦃ Eq C ⦄ → Eq (Tuple3 A B C)
    Ord[Tuple3[A,B,C]]       : ⦃ Ord A ⦄ → ⦃ Ord B ⦄ → ⦃ Ord C ⦄ → Ord (Tuple3 A B C)
    Functor[Tuple3[A,B]]     : Functor (Tuple3 {aℓ} {bℓ} {cℓ = aℓ ⊔ bℓ ⊔ cℓ} A B)
    Bifunctor[Tuple3[A]]     : Bifunctor {bℓ} {cℓ} (Tuple3 A)
    Bifoldable[Tuple3[A]]    : Bifoldable {aℓ = bℓ} {bℓ = cℓ} (Tuple3 A)
    Bitraversable[Tuple3[A]] : Bitraversable (Tuple3 {aℓ} {aℓ} A)

    Show[Tuple4[A,B,C,D]]      : ⦃ Show A ⦄ → ⦃ Show B ⦄ → ⦃ Show C ⦄ → ⦃ Show D ⦄ → Show (Tuple4 A B C D)
    Read[Tuple4[A,B,C,D]]      : ⦃ Read A ⦄ → ⦃ Read B ⦄ → ⦃ Read C ⦄ → ⦃ Read D ⦄ → Read (Tuple4 A B C D)
    Eq[Tuple4[A,B,C,D]]        : ⦃ Eq A ⦄ → ⦃ Eq B ⦄ → ⦃ Eq C ⦄ → ⦃ Eq D ⦄ → Eq (Tuple4 A B C D)
    Ord[Tuple4[A,B,C,D]]       : ⦃ Ord A ⦄ → ⦃ Ord B ⦄ → ⦃ Ord C ⦄ → ⦃ Ord D ⦄ → Ord (Tuple4 A B C D)
    Functor[Tuple4[A,B,C]]     : Functor (Tuple4 {aℓ} {bℓ} {cℓ} {dℓ = aℓ ⊔ bℓ ⊔ cℓ ⊔ dℓ} A B C)
    Bifunctor[Tuple4[A,B]]     : Bifunctor {cℓ} {dℓ} (Tuple4 A B)
    Bifoldable[Tuple4[A,B]]    : Bifoldable {aℓ = cℓ} {bℓ = dℓ} (Tuple4 A B)
    Bitraversable[Tuple4[A,B]] : Bitraversable (Tuple4 {aℓ} {aℓ} {aℓ} A B)

    Show[Tuple5[A,B,C,D,E]]      : ⦃ Show A ⦄ → ⦃ Show B ⦄ → ⦃ Show C ⦄ → ⦃ Show E ⦄ → ⦃ Show D ⦄ → Show (Tuple5 A B C D E)
    Read[Tuple5[A,B,C,D,E]]      : ⦃ Read A ⦄ → ⦃ Read B ⦄ → ⦃ Read C ⦄ → ⦃ Read E ⦄ → ⦃ Read D ⦄ → Read (Tuple5 A B C D E)
    Eq[Tuple5[A,B,C,D,E]]        : ⦃ Eq A ⦄ → ⦃ Eq B ⦄ → ⦃ Eq C ⦄ → ⦃ Eq D ⦄ → ⦃ Eq E ⦄ → Eq (Tuple5 A B C D E)
    Ord[Tuple5[A,B,C,D,E]]       : ⦃ Ord A ⦄ → ⦃ Ord B ⦄ → ⦃ Ord C ⦄ → ⦃ Ord D ⦄ → ⦃ Ord E ⦄ → Ord (Tuple5 A B C D E)
    Bifunctor[Tuple5[A,B,C]]     : Bifunctor {dℓ} {eℓ} (Tuple5 A B C)
    Bifoldable[Tuple5[A,B,C]]    : Bifoldable {aℓ = dℓ} {bℓ = eℓ} (Tuple5 A B C)
    Bitraversable[Tuple4[A,B,C]] : Bitraversable (Tuple5 {aℓ} {aℓ} {aℓ} {aℓ} A B C)

{-# COMPILE GHC Show[Tuple2[A,B]]      = \ aℓ a bℓ b AgdaShow AgdaShow -> AgdaShow          #-}
{-# COMPILE GHC Read[Tuple2[A,B]]      = \ aℓ a bℓ b AgdaRead AgdaRead -> AgdaRead          #-}
{-# COMPILE GHC Eq[Tuple2[A,B]]        = \ aℓ a bℓ b AgdaEq AgdaEq     -> AgdaEq            #-}
{-# COMPILE GHC Ord[Tuple2[A,B]]       = \ aℓ a bℓ b AgdaOrd AgdaOrd   -> AgdaOrd           #-}
{-# COMPILE GHC Foldable[Tuple2[A]]    = \ bℓ aℓ a                     -> AgdaFoldable      #-}
{-# COMPILE GHC Traversable[Tuple2[A]] = \ aℓ bℓ a                     -> AgdaTraversable   #-}
{-# COMPILE GHC Functor[Tuple2[A]]     = \ aℓ bℓ a                     -> AgdaFunctor       #-}
{-# COMPILE GHC Bifunctor[Tuple2]      = \ aℓ bℓ                       -> AgdaBifunctor     #-}
{-# COMPILE GHC Bifoldable[Tuple2]     = \ aℓ bℓ                       -> AgdaBifoldable    #-}
{-# COMPILE GHC Bitraversable[Tuple2]  = \ aℓ                          -> AgdaBitraversable #-}

{-# COMPILE GHC Show[Tuple3[A,B,C]]      = \ aℓ a bℓ b cℓ c AgdaShow AgdaShow AgdaShow -> AgdaShow          #-}
{-# COMPILE GHC Read[Tuple3[A,B,C]]      = \ aℓ a bℓ b cℓ c AgdaRead AgdaRead AgdaRead -> AgdaRead          #-}
{-# COMPILE GHC Eq[Tuple3[A,B,C]]        = \ aℓ a bℓ b cℓ c AgdaEq AgdaEq AgdaEq       -> AgdaEq            #-}
{-# COMPILE GHC Ord[Tuple3[A,B,C]]       = \ aℓ a bℓ b cℓ c AgdaOrd AgdaOrd AgdaOrd    -> AgdaOrd           #-}
{-# COMPILE GHC Functor[Tuple3[A,B]]     = \ aℓ bℓ cℓ a b                              -> AgdaFunctor       #-}
{-# COMPILE GHC Bifunctor[Tuple3[A]]     = \ bℓ cℓ aℓ a                                -> AgdaBifunctor     #-}
{-# COMPILE GHC Bifoldable[Tuple3[A]]    = \ bℓ cℓ aℓ a                                -> AgdaBifoldable    #-}
{-# COMPILE GHC Bitraversable[Tuple3[A]] = \ aℓ a                                      -> AgdaBitraversable #-}

{-# COMPILE GHC Show[Tuple4[A,B,C,D]]      = \ aℓ a bℓ b cℓ c dℓ d AgdaShow AgdaShow AgdaShow AgdaShow -> AgdaShow          #-}
{-# COMPILE GHC Read[Tuple4[A,B,C,D]]      = \ aℓ a bℓ b cℓ c dℓ d AgdaRead AgdaRead AgdaRead AgdaRead -> AgdaRead          #-}
{-# COMPILE GHC Eq[Tuple4[A,B,C,D]]        = \ aℓ a bℓ b cℓ c dℓ d AgdaEq AgdaEq AgdaEq AgdaEq         -> AgdaEq            #-}
{-# COMPILE GHC Ord[Tuple4[A,B,C,D]]       = \ aℓ a bℓ b cℓ c dℓ d AgdaOrd AgdaOrd AgdaOrd AgdaOrd     -> AgdaOrd           #-}
{-# COMPILE GHC Functor[Tuple4[A,B,C]]     = \ aℓ bℓ cℓ dℓ a b c                                       -> AgdaFunctor       #-}
{-# COMPILE GHC Bifunctor[Tuple4[A,B]]     = \ cℓ dℓ aℓ a bℓ b                                         -> AgdaBifunctor     #-}
{-# COMPILE GHC Bifoldable[Tuple4[A,B]]    = \ cℓ dℓ aℓ a bℓ b                                         -> AgdaBifoldable    #-}
{-# COMPILE GHC Bitraversable[Tuple4[A,B]] = \ aℓ a b                                                  -> AgdaBitraversable #-}

{-# COMPILE GHC Show[Tuple5[A,B,C,D,E]]      = \ aℓ a bℓ b cℓ c dℓ d eℓ e AgdaShow AgdaShow AgdaShow AgdaShow AgdaShow -> AgdaShow          #-}
{-# COMPILE GHC Read[Tuple5[A,B,C,D,E]]      = \ aℓ a bℓ b cℓ c dℓ d eℓ e AgdaRead AgdaRead AgdaRead AgdaRead AgdaRead -> AgdaRead          #-}
{-# COMPILE GHC Eq[Tuple5[A,B,C,D,E]]        = \ aℓ a bℓ b cℓ c dℓ d eℓ e AgdaEq AgdaEq AgdaEq AgdaEq AgdaEq           -> AgdaEq            #-}
{-# COMPILE GHC Ord[Tuple5[A,B,C,D,E]]       = \ aℓ a bℓ b cℓ c dℓ d eℓ e AgdaOrd AgdaOrd AgdaOrd AgdaOrd AgdaOrd      -> AgdaOrd           #-}
{-# COMPILE GHC Bifunctor[Tuple5[A,B,C]]     = \ dℓ eℓ aℓ a bℓ b cℓ c                                                  -> AgdaBifunctor     #-}
{-# COMPILE GHC Bifoldable[Tuple5[A,B,C]]    = \ dℓ eℓ aℓ a bℓ b cℓ c                                                  -> AgdaBifoldable    #-}
{-# COMPILE GHC Bitraversable[Tuple4[A,B,C]] = \ aℓ a b c                                                              -> AgdaBitraversable #-}
