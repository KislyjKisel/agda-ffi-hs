{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Either where

open import Agda.Primitive
open import Ffi.Hs.Data.Bool  using (Bool)
open import Ffi.Hs.Data.List  using (List)
open import Ffi.Hs.Data.Tuple using (Tuple2)

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
    either           : (A → C) → (B → C) → Either A B → C
    lefts            : List (Either A B) → List A
    rights           : List (Either A B) → List B
    isLeft           : Either A B → Bool
    isRight          : Either A B → Bool
    fromLeft         : A → Either A B → A
    fromRight        : B → Either A B → B
    partitionEithers : List (Either A B) → Tuple2 (List A) (List B)

{-# FOREIGN GHC import qualified Data.Either #-}
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
