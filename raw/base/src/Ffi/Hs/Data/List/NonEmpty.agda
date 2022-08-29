{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.List.NonEmpty where

open import Agda.Builtin.Bool           using (Bool)
open import Agda.Builtin.List           using (List)
open import Agda.Builtin.Maybe          using (Maybe)
open import Agda.Primitive
open import Ffi.Hs.Data.Tuple using (Tuple2)
open import Ffi.Hs.Control.Applicative  using (Alternative)
open import Ffi.Hs.Data.Eq              using (Eq)
open import Ffi.Hs.Data.Foldable        using (Foldable)
open import Ffi.Hs.Data.Functor         using (Functor)
open import Ffi.Hs.Data.Int             using (Int)
open import Ffi.Hs.Data.Ord             using (Ord; Ordering)

private
    variable
        aℓ bℓ cℓ fℓ : Level
        A : Set aℓ
        B : Set bℓ
        C : Set cℓ
        F : Set fℓ → Set fℓ

data NonEmpty (A : Set aℓ) : Set aℓ where
    _:|_ : A → List A → NonEmpty A

{-# FOREIGN GHC import qualified Data.List.NonEmpty                       #-}
{-# FOREIGN GHC type AgdaNonEmpty aℓ = Data.List.NonEmpty.NonEmpty        #-}
{-# COMPILE GHC NonEmpty = data(1) AgdaNonEmpty ((Data.List.NonEmpty.:|)) #-}

infixl 9 _!!_
infixr 5 _<|_

postulate
    map           : (A → B) → NonEmpty A → NonEmpty B
    intersperse   : A → NonEmpty A → NonEmpty A
    scanl         : ⦃ Foldable F ⦄ → (B → A → B) → B → F A → NonEmpty B
    scanr         : ⦃ Foldable F ⦄ → (A → B → B) → B → F A → NonEmpty B
    scanl1        : (A → A → A) → NonEmpty A → NonEmpty A
    scanr1        : (A → A → A) → NonEmpty A → NonEmpty A
    transpose     : NonEmpty (NonEmpty A) → NonEmpty (NonEmpty A)
    sortBy        : (A → A → Ordering) → NonEmpty A → NonEmpty A
    sortWith      : ⦃ Ord B ⦄ → (A → B) → NonEmpty A → NonEmpty A
    length        : NonEmpty A → Int
    head          : NonEmpty A → A
    tail          : NonEmpty A → List A
    last          : NonEmpty A → A
    init          : NonEmpty A → List A
    singleton     : A → NonEmpty A
    _<|_          : A → NonEmpty A → NonEmpty A  
    cons          : A → NonEmpty A → NonEmpty A
    uncons        : NonEmpty A → Tuple2 A (Maybe (NonEmpty A))
    unfoldr       : (A → Tuple2 B (Maybe A)) → A → NonEmpty B
    sort          : ⦃ Ord A ⦄ → NonEmpty A → NonEmpty A
    reverse       : NonEmpty A → NonEmpty A
    inits         : ⦃ Foldable F ⦄ → F A → NonEmpty (List A)
    tails         : ⦃ Foldable F ⦄ → F A → NonEmpty (List A)
    append        : NonEmpty A → NonEmpty A → NonEmpty A
    appendList    : NonEmpty A → List A → NonEmpty A
    prependList   : List A → NonEmpty A → NonEmpty A
    iterate       : (A → A) → A → NonEmpty A
    repeat        : A → NonEmpty A
    cycle         : NonEmpty A → NonEmpty A
    insert        : ⦃ Foldable F ⦄ → ⦃ Ord A ⦄ → A → F A → NonEmpty A
    some1         : ⦃ Alternative F ⦄ → F A → F (NonEmpty A)
    take          : Int → NonEmpty A → List A
    drop          : Int → NonEmpty A → List A
    splitAt       : Int → NonEmpty A → Tuple2 (List A) (List A)
    takeWhile     : (A → Bool) → NonEmpty A → List A
    dropWhile     : (A → Bool) → NonEmpty A → List A
    span          : (A → Bool) → NonEmpty A → Tuple2 (List A) (List A)
    break         : (A → Bool) → NonEmpty A → Tuple2 (List A) (List A)
    filter        : (A → Bool) → NonEmpty A → List A
    partition     : (A → Bool) → NonEmpty A → Tuple2 (List A) (List A)
    group         : ⦃ Foldable F ⦄ → ⦃ Eq A ⦄ → F A → List (NonEmpty A)
    groupBy       : ⦃ Foldable F ⦄ → (A → A → Bool) → F A → List (NonEmpty A)
    groupWith     : ⦃ Foldable F ⦄ → ⦃ Eq B ⦄ → (A → B) → F A → List (NonEmpty A)
    groupAllWith  : ⦃ Ord B ⦄ → (A → B) → List A → List (NonEmpty A)
    group1        : ⦃ Eq A ⦄ → NonEmpty A → NonEmpty (NonEmpty A)
    groupBy1      : (A → A → Bool) → NonEmpty A → NonEmpty (NonEmpty A)
    groupWith1    : ⦃ Eq A ⦄ → (A → B) → NonEmpty A → NonEmpty (NonEmpty A)
    groupAllWith1 : ⦃ Ord B ⦄ → (A → B) → NonEmpty A → NonEmpty (NonEmpty A)
    nub           : ⦃ Eq A ⦄ → NonEmpty A → NonEmpty A
    nubBy         : (A → A → Bool) → NonEmpty A → NonEmpty A
    _!!_          : NonEmpty A → Int → A -- todo: `HasCallStack`
    zip           : NonEmpty A → NonEmpty B → NonEmpty (Tuple2 A B)
    zipWith       : (A → B → C) → NonEmpty A → NonEmpty B → NonEmpty C
    unzip         : ⦃ Functor F ⦄ → F (Tuple2 A B) → Tuple2 (F A) (F B)
    fromList      : List A → NonEmpty A
    toList        : NonEmpty A → List A
    nonEmpty      : List A → Maybe (NonEmpty A)
    xor           : NonEmpty Bool → Bool
