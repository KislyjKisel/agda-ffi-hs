{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Either where

open import Agda.Primitive
open import Ffi.Hs.-base.Class
open import Ffi.Hs.Data.Bool   using (Bool)
open import Ffi.Hs.Data.List   using (List)
open import Ffi.Hs.Data.Tuple  using (Tuple2)

{-# FOREIGN GHC
import qualified Data.Either
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ bℓ cℓ dℓ : Level
        A : Set aℓ
        B : Set bℓ
        C : Set cℓ
        D : Set dℓ

data Either (A : Set aℓ) (B : Set bℓ) : Set (aℓ ⊔ bℓ) where
    Left  : A → Either A B
    Right : B → Either A B

postulate
    Bifoldable[Either]     : Bifoldable {aℓ} {bℓ} Either
    Bifunctor[Either]      : Bifunctor {aℓ} {bℓ} Either
    Bitraversable[Either]  : Bitraversable {aℓ} {bℓ} Either
    Eq2[Either]            : Eq2 {aℓ} {bℓ} Either
    Ord2[Either]           : Ord2 {aℓ} {bℓ} Either
    Read2[Either]          : Read2 {aℓ} {bℓ} Either
    Show2[Either]          : Show2 {aℓ} {bℓ} Either
    MonadFix[Either[A]]    : MonadFix (Either {aℓ} {aℓ} A)
    Foldable[Either[A]]    : Foldable (Either {bℓ = bℓ} A)
    Eq1[Either[A]]         : ⦃ Eq A ⦄ → Eq1 (Either {bℓ = bℓ} A)
    Ord1[Either[A]]        : ⦃ Ord A ⦄ → Ord1 (Either {bℓ = bℓ} A)
    Read1[Either[A]]       : ⦃ Read A ⦄ → Read1 (Either {bℓ = bℓ} A)
    Show1[Either[A]]       : ⦃ Show A ⦄ → Show1 (Either {bℓ = bℓ} A)
    Traversable[Either[A]] : Traversable (Either {bℓ = bℓ} A)
    Applicative[Either[A]] : Applicative (Either {aℓ} {aℓ} A)
    Functor[Either[A]]     : Functor (Either {aℓ} {aℓ} A)
    Monad[Either[A]]       : Monad (Either {aℓ} {aℓ} A)
    Data[Either[A,B]]      : ⦃ Data A ⦄ → ⦃ Data B ⦄ → Data (Either A B)
    Semigroup[Either[A,B]] : Semigroup (Either A B)
    Read[Either[A,B]]      : ⦃ Read A ⦄ → ⦃ Read B ⦄ → Read (Either A B)
    Show[Either[A,B]]      : ⦃ Show A ⦄ → ⦃ Show B ⦄ → Show (Either A B)
    Eq[Either[A,B]]        : ⦃ Eq A ⦄ → ⦃ Eq B ⦄ → Eq (Either A B)
    Ord[Either[A,B]]       : ⦃ Ord A ⦄ → ⦃ Ord B ⦄ → Ord (Either A B)

{-# COMPILE GHC Bifoldable[Either]     = \ aℓ bℓ                       -> AgdaBifoldable    #-}
{-# COMPILE GHC Bifunctor[Either]      = \ aℓ bℓ                       -> AgdaBifunctor     #-}
{-# COMPILE GHC Bitraversable[Either]  = \ aℓ bℓ                       -> AgdaBitraversable #-}
{-# COMPILE GHC Eq2[Either]            = \ aℓ bℓ                       -> AgdaEq2           #-}
{-# COMPILE GHC Ord2[Either]           = \ aℓ bℓ                       -> AgdaOrd2          #-}
{-# COMPILE GHC Show2[Either]          = \ aℓ bℓ                       -> AgdaShow2         #-}
{-# COMPILE GHC Read2[Either]          = \ aℓ bℓ                       -> AgdaRead2         #-}
{-# COMPILE GHC MonadFix[Either[A]]    = \ aℓ a                        -> AgdaMonadFix      #-}
{-# COMPILE GHC Foldable[Either[A]]    = \ bℓ aℓ a                     -> AgdaFoldable      #-}
{-# COMPILE GHC Eq1[Either[A]]         = \ aℓ a bℓ AgdaEq              -> AgdaEq1           #-}
{-# COMPILE GHC Ord1[Either[A]]        = \ aℓ a bℓ AgdaOrd             -> AgdaOrd1          #-}
{-# COMPILE GHC Read1[Either[A]]       = \ aℓ a bℓ AgdaRead            -> AgdaRead1         #-}
{-# COMPILE GHC Show1[Either[A]]       = \ aℓ a bℓ AgdaShow            -> AgdaShow1         #-}
{-# COMPILE GHC Traversable[Either[A]] = \ bℓ aℓ a                     -> AgdaTraversable   #-}
{-# COMPILE GHC Applicative[Either[A]] = \ aℓ a                        -> AgdaApplicative   #-}
{-# COMPILE GHC Functor[Either[A]]     = \ aℓ a                        -> AgdaFunctor       #-}
{-# COMPILE GHC Monad[Either[A]]       = \ aℓ a                        -> AgdaMonad         #-}
{-# COMPILE GHC Data[Either[A,B]]      = \ aℓ a bℓ b AgdaData AgdaData -> AgdaData          #-}
{-# COMPILE GHC Semigroup[Either[A,B]] = \ aℓ a bℓ b                   -> AgdaSemigroup     #-}
{-# COMPILE GHC Read[Either[A,B]]      = \ aℓ a bℓ b AgdaRead AgdaRead -> AgdaRead          #-}
{-# COMPILE GHC Show[Either[A,B]]      = \ aℓ a bℓ b AgdaShow AgdaShow -> AgdaShow          #-}
{-# COMPILE GHC Eq[Either[A,B]]        = \ aℓ a bℓ b AgdaEq AgdaEq     -> AgdaEq            #-}
{-# COMPILE GHC Ord[Either[A,B]]       = \ aℓ a bℓ b AgdaOrd AgdaOrd   -> AgdaOrd           #-}

postulate
    either           : (A → C) → (B → C) → Either A B → C
    lefts            : List (Either A B) → List A
    rights           : List (Either A B) → List B
    isLeft           : Either A B → Bool
    isRight          : Either A B → Bool
    fromLeft         : A → Either A B → A
    fromRight        : B → Either A B → B
    partitionEithers : List (Either A B) → Tuple2 (List A) (List B)

{-# FOREIGN GHC type AgdaEither aℓ bℓ = Data.Either.Either #-}
{-# COMPILE GHC Either = data(2) AgdaEither (Left | Right) #-}

{-# COMPILE GHC either           = \ aℓ bℓ cℓ a b c -> Data.Either.either           #-}
{-# COMPILE GHC lefts            = \ aℓ bℓ    a b   -> Data.Either.lefts            #-}
{-# COMPILE GHC rights           = \ aℓ bℓ    a b   -> Data.Either.rights           #-}
{-# COMPILE GHC isLeft           = \ aℓ bℓ    a b   -> Data.Either.isLeft           #-}
{-# COMPILE GHC isRight          = \ aℓ bℓ    a b   -> Data.Either.isRight          #-}
{-# COMPILE GHC fromLeft         = \ aℓ bℓ    a b   -> Data.Either.fromLeft         #-}
{-# COMPILE GHC fromRight        = \ aℓ bℓ    a b   -> Data.Either.fromRight        #-}
{-# COMPILE GHC partitionEithers = \ aℓ bℓ    a b   -> Data.Either.partitionEithers #-}
