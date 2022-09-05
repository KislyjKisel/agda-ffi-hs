{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.List where

open import Agda.Builtin.Bool  using (Bool)
open import Agda.Builtin.Maybe using (Maybe)
open import Agda.Primitive
open import Ffi.Hs.-base.Class using (Num; Integral)
open import Ffi.Hs.Data.Eq     using (Eq)
open import Ffi.Hs.Data.Int    using (Int)
open import Ffi.Hs.Data.Ord    using (Ord; Ordering)
open import Ffi.Hs.Data.Tuple  using (Tuple2; Tuple3; Tuple4; Tuple5)

open import Agda.Builtin.List public
    using (List; []; _∷_)

open import Ffi.Hs.-base.Kind.List public
    using (`List; `[]; _`∷_; lift`List)

private
    variable
        aℓ bℓ cℓ dℓ eℓ : Level
        A : Set aℓ
        B : Set bℓ
        C : Set cℓ
        D : Set dℓ
        E : Set eℓ

-- todo: `HasCallStack`s

infixl 9 _!!_
infixr 5 _++_
infix  5 _\\_

postulate
    _++_      : List A → List A → List A
    head      : List A → A
    last      : List A → A
    tail      : List A → List A
    init      : List A → List A
    uncons    : List A → Maybe (Tuple2 A (List A))
    singleton : A → List A
    null      : List A → Bool
    length    : List A → Int

    map          : (A → B) → List A → List B
    reverse      : List A → List A
    intersperse  : A → List A → List A
    intercalate  : List A → List (List A) → List A
    transpose    : List (List A) → List (List A)
    subsequences : List A → List (List A)
    permutations : List A → List (List A)

    scanl  : (B → A → B) → B → List A → List B
    scanl' : (B → A → B) → B → List A → List B
    scanl1 : (A → A → A) → List A → List A
    scanr  : (A → B → B) → B → List A → List B
    scanr1 : (A → A → A) → List A → List A

    iterate   : (A → A) → A → List A
    iterate'  : (A → A) → A → List A
    repeat    : A → List A
    replicate : Int → A → List A
    cycle     : List A → List A -- todo `HasCallStack`
    unfoldr   : (B → Maybe (Tuple2 A B)) → B → List A

    take         : Int → List A → List A
    drop         : Int → List A → List A
    splitAt      : Int → List A → Tuple2 (List A) (List A)
    takeWhile    : (A → Bool) → List A → List A
    dropWhile    : (A → Bool) → List A → List A
    dropWhileEnd : (A → Bool) → List A → List A
    span         : (A → Bool) → List A → Tuple2 (List A) (List A)
    break        : (A → Bool) → List A → Tuple2 (List A) (List A)
    stripPrefix  : ⦃ Eq A ⦄ → List A → List A → Maybe (List A)
    group        : ⦃ Eq A ⦄ → List A → List (List A)
    inits        : List A → List (List A)
    tails        : List A → List (List A)
    
    isPrefixOf      : ⦃ Eq A ⦄ → List A → List A → Bool
    isSuffixOf      : ⦃ Eq A ⦄ → List A → List A → Bool
    isInfixOf       : ⦃ Eq A ⦄ → List A → List A → Bool
    isSubsequenceOf : ⦃ Eq A ⦄ → List A → List A → Bool

    lookup      : ⦃ Eq A ⦄ → A → List (Tuple2 A B) → Maybe B
    filter      : (A → Bool) → List A → List A
    partition   : (A → Bool) → List A → Tuple2 (List A) (List A)
    _!!_        : List A → Int → A -- todo: `HasCallStack`
    elemIndex   : ⦃ Eq A ⦄ → A → List A → Maybe Int
    elemIndices : ⦃ Eq A ⦄ → A → List A → List Int
    findIndex   : (A → Bool) → List A → Maybe Int
    findIndices : (A → Bool) → List A → List Int

    zip  : List A → List B → List (Tuple2 A B)
    zip3 : List A → List B → List C → List (Tuple3 A B C)
    zip4 : List A → List B → List C → List D → List (Tuple4 A B C D)
    zip5 : List A → List B → List C → List D → List E → List (Tuple5 A B C D E)
    zipWith  : (A → B → C) → List A → List B → List C
    zipWith3 : (A → B → C → D) → List A → List B → List C → List D
    zipWith4 : (A → B → C → D → E) → List A → List B → List C → List D → List E
    unzip  : List (Tuple2 A B) → Tuple2 (List A) (List B)
    unzip3 : List (Tuple3 A B C) → Tuple3 (List A) (List B) (List C)
    unzip4 : List (Tuple4 A B C D) → Tuple4 (List A) (List B) (List C) (List D)
    unzip5 : List (Tuple5 A B C D E) → Tuple5 (List A) (List B) (List C) (List D) (List E)

    nub       : ⦃ Eq A ⦄ → List A → List A
    delete    : ⦃ Eq A ⦄ → A → List A → List A
    _\\_      : ⦃ Eq A ⦄ → List A → List A → List A
    union     : ⦃ Eq A ⦄ → List A → List A → List A
    intersect : ⦃ Eq A ⦄ → List A → List A → List A

    sort   : ⦃ Ord A ⦄ → List A → List A
    sortOn : ⦃ Ord B ⦄ → (A → B) → List A → List A
    insert : ⦃ Ord A ⦄ → A → List A → List A

    nubBy          : (A → A → Bool) → List A → List A
    deleteBy       : (A → A → Bool) → A → List A → List A
    deleteFirstsBy : (A → A → Bool) → List A → List A → List A
    unionBy        : (A → A → Bool) → List A → List A → List A
    intersectBy    : (A → A → Bool) → List A → List A → List A
    groupBy        : (A → A → Bool) → List A → List (List A)

    sortBy   : (A → A → Ordering) → List A → List A
    insertBy : (A → A → Ordering) → A → List A → List A

    genericLength    : ⦃ Num B ⦄ → List A → B
    genericTake      : ⦃ Integral B ⦄ → B → List A → List A
    genericDrop      : ⦃ Integral B ⦄ → B → List A → List A
    genericSplitAt   : ⦃ Integral B ⦄ → B → List A → Tuple2 (List A) (List A)
    genericIndex     : ⦃ Integral B ⦄ → List A → B → A
    genericReplicate : ⦃ Integral B ⦄ → B → A → List A
