{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Vector.Storable where

open import Agda.Builtin.Bool              using (Bool)
open import Agda.Builtin.IO                using (IO)
open import Agda.Builtin.List              using (List)
open import Agda.Builtin.Maybe             using (Maybe)
open import Agda.Primitive
open import Ffi.Hs.-base.Class
open import Ffi.Hs.-base.Level             using (Liftℓ)
open import Ffi.Hs.-base.Unit              using (⊤; ⊤′)
open import Ffi.Hs.Control.DeepSeq         using (NFData; NFData1)
open import Ffi.Hs.Control.Monad.Primitive using (PrimMonad; PrimState)
open import Ffi.Hs.Control.Monad.ST        using (ST)
open import Ffi.Hs.Data.Either             using (Either)
open import Ffi.Hs.Data.Int                using (Int)
open import Ffi.Hs.Data.Ord                using (Ordering)
open import Ffi.Hs.Data.Tuple              using (Tuple2; Tuple3)
open import Ffi.Hs.Foreign.ForeignPtr      using (ForeignPtr)
open import Ffi.Hs.Foreign.Ptr             using (Ptr)
open import Ffi.Hs.GHC.IsList              using (IsList)

open Ffi.Hs.-base.Class public
    using (Storable)

open import Ffi.Hs.Data.Vector.Storable.Mutable public
    using (MVector)

{-# FOREIGN GHC
import qualified Data.Vector.Storable
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
import MAlonzo.Code.Ffi.Hs.Control.DeepSeq (AgdaNFData(AgdaNFData), AgdaNFData1(AgdaNFData1))
import MAlonzo.Code.Ffi.Hs.Control.Monad.Primitive (AgdaPrimMonad(AgdaPrimMonad))
import MAlonzo.Code.Ffi.Hs.GHC.IsList (AgdaIsList(AgdaIsList))
#-}

private
    variable
        aℓ bℓ : Level
        A B C D E F G : Set aℓ
        S : Set
        M : Set aℓ → Set aℓ

postulate
    Vector : Set aℓ → Set aℓ
    NFData1[Vector]      : NFData1 (Vector {aℓ})
    IsList[Vector[A]]    : ⦃ Storable A ⦄ → IsList (Vector A)
    Eq[Vector[A]]        : ⦃ Storable A ⦄ → ⦃ Eq A ⦄ → Eq (Vector A)
    Data[Vector[A]]      : ⦃ Storable A ⦄ → ⦃ Data A ⦄ → Data (Vector A)
    Ord[Vector[A]]       : ⦃ Storable A ⦄ → ⦃ Ord A ⦄ → Ord (Vector A)
    Read[Vector[A]]      : ⦃ Storable A ⦄ → ⦃ Read A ⦄ → Read (Vector A)
    Show[Vector[A]]      : ⦃ Storable A ⦄ → ⦃ Show A ⦄ → Show (Vector A)
    Semigroup[Vector[A]] : ⦃ Storable A ⦄ → Semigroup (Vector A)
    Monoid[Vector[A]]    : ⦃ Storable A ⦄ → Monoid (Vector A)
    NFData[Vector[A]]    : NFData (Vector A)

{-# FOREIGN GHC type AgdaVector aℓ = Data.Vector.Storable.Vector #-}
{-# COMPILE GHC Vector = type(1) AgdaVector #-}

{-# COMPILE GHC NFData1[Vector]      = \ aℓ                         -> AgdaNFData1   #-}
{-# COMPILE GHC IsList[Vector[A]]    = \ aℓ a AgdaStorable          -> AgdaIsList    #-}
{-# COMPILE GHC Eq[Vector[A]]        = \ aℓ a AgdaStorable AgdaEq   -> AgdaEq        #-}
{-# COMPILE GHC Data[Vector[A]]      = \ aℓ a AgdaStorable AgdaData -> AgdaData      #-}
{-# COMPILE GHC Ord[Vector[A]]       = \ aℓ a AgdaStorable AgdaOrd  -> AgdaOrd       #-}
{-# COMPILE GHC Read[Vector[A]]      = \ aℓ a AgdaStorable AgdaRead -> AgdaRead      #-}
{-# COMPILE GHC Show[Vector[A]]      = \ aℓ a AgdaStorable AgdaShow -> AgdaShow      #-}
{-# COMPILE GHC Semigroup[Vector[A]] = \ aℓ a AgdaStorable          -> AgdaSemigroup #-}
{-# COMPILE GHC Monoid[Vector[A]]    = \ aℓ a AgdaStorable          -> AgdaMonoid    #-}
{-# COMPILE GHC NFData[Vector[A]]    = \ aℓ a                       -> AgdaNFData    #-}

infixr 5 _++_

postulate
    length                : ⦃ Storable A ⦄ → Vector A → Int
    null                  : ⦃ Storable A ⦄ → Vector A → Bool
    _!_                   : ⦃ Storable A ⦄ → Vector A → Int → A
    _!?_                  : ⦃ Storable A ⦄ → Vector A → Int → Maybe A
    head                  : ⦃ Storable A ⦄ → Vector A → A
    last                  : ⦃ Storable A ⦄ → Vector A → A
    unsafeIndex           : ⦃ Storable A ⦄ → Vector A → Int → A
    unsafeHead            : ⦃ Storable A ⦄ → Vector A → A
    unsafeLast            : ⦃ Storable A ⦄ → Vector A → A
    indexM                : ⦃ Storable A ⦄ → ⦃ Monad M ⦄ → Vector A → Int → M A
    headM                 : ⦃ Storable A ⦄ → ⦃ Monad M ⦄ → Vector A → M A
    lastM                 : ⦃ Storable A ⦄ → ⦃ Monad M ⦄ → Vector A → M A
    unsafeIndexM          : ⦃ Storable A ⦄ → ⦃ Monad M ⦄ → Vector A → Int → M A
    unsafeHeadM           : ⦃ Storable A ⦄ → ⦃ Monad M ⦄ → Vector A → M A
    unsafeLastM           : ⦃ Storable A ⦄ → ⦃ Monad M ⦄ → Vector A → M A
    slice                 : ⦃ Storable A ⦄ → Int → Int → Vector A → Vector A
    init                  : ⦃ Storable A ⦄ → Vector A → Vector A
    tail                  : ⦃ Storable A ⦄ → Vector A → Vector A
    take                  : ⦃ Storable A ⦄ → Int → Vector A → Vector A
    drop                  : ⦃ Storable A ⦄ → Int → Vector A → Vector A
    splitAt               : ⦃ Storable A ⦄ → Int → Vector A → Tuple2 (Vector A) (Vector A)
    uncons                : ⦃ Storable A ⦄ → Vector A → Maybe (Tuple2 A (Vector A))
    unsnoc                : ⦃ Storable A ⦄ → Vector A → Maybe (Tuple2 (Vector A) A)
    unsafeSlice           : ⦃ Storable A ⦄ → Int → Int → Vector A → Vector A
    unsafeInit            : ⦃ Storable A ⦄ → Vector A → Vector A
    unsafeTail            : ⦃ Storable A ⦄ → Vector A → Vector A
    unsafeTake            : ⦃ Storable A ⦄ → Int → Vector A → Vector A
    unsafeDrop            : ⦃ Storable A ⦄ → Int → Vector A → Vector A
    empty                 : ⦃ Storable A ⦄ → Vector A
    singleton             : ⦃ Storable A ⦄ → A → Vector A
    replicate             : ⦃ Storable A ⦄ → Int → A → Vector A
    generate              : ⦃ Storable A ⦄ → Int → (Int → A) → Vector A
    iterateN              : ⦃ Storable A ⦄ → Int → (A → A) → A → Vector A
    replicateM            : ⦃ Monad M ⦄ → ⦃ Storable A ⦄ → Int → M A → M (Vector A)
    generateM             : ⦃ Monad M ⦄ → ⦃ Storable A ⦄ → Int → (Int → M A) → M (Vector A)
    iterateNM             : ⦃ Monad M ⦄ → ⦃ Storable A ⦄ → Int → (A → M A) → A → M (Vector A)
    create                : ⦃ Storable A ⦄ → (∀{S} → ST S (MVector S A)) → Vector A
    createT               : {F : Set aℓ → Set aℓ} → ⦃ Traversable F ⦄ → ⦃ Storable A ⦄ → (∀{S} → ST S (F (MVector S A))) → F (Vector A)
    unfoldr               : ⦃ Storable A ⦄ → (B → Maybe (Tuple2 A B)) → B → Vector A
    unfoldrN              : ⦃ Storable A ⦄ → Int → (B → Maybe (Tuple2 A B)) → B → Vector A
    unfoldrExactN         : ⦃ Storable A ⦄ → Int → (B → Tuple2 A B) → B → Vector A
    unfoldrM              : {A B : Set aℓ} → ⦃ Monad M ⦄ → ⦃ Storable A ⦄ → (B → M (Maybe (Tuple2 A B))) → B → M (Vector A)
    unfoldrNM             : {A B : Set aℓ} → ⦃ Monad M ⦄ → ⦃ Storable A ⦄ → Int → (B → M (Maybe (Tuple2 A B))) → B → M (Vector A)
    unfoldrExactNM        : {A B : Set aℓ} → ⦃ Monad M ⦄ → ⦃ Storable A ⦄ → Int → (B → M (Tuple2 A B)) → B → M (Vector A)
    constructN            : ⦃ Storable A ⦄ → Int → (Vector A → A) → Vector A
    constructrN           : ⦃ Storable A ⦄ → Int → (Vector A → A) → Vector A
    enumFromN             : ⦃ Storable A ⦄ → ⦃ Num A ⦄ → A → Int → Vector A
    enumFromStepN         : ⦃ Storable A ⦄ → ⦃ Num A ⦄ → A → A → Int → Vector A
    enumFromTo            : ⦃ Storable A ⦄ → ⦃ Enum A ⦄ → A → A → Vector A
    enumFromThenTo        : ⦃ Storable A ⦄ → ⦃ Enum A ⦄ → A → A → A → Vector A
    cons                  : ⦃ Storable A ⦄ → A → Vector A → Vector A
    snoc                  : ⦃ Storable A ⦄ → Vector A → A → Vector A
    _++_                  : ⦃ Storable A ⦄ → Vector A → Vector A → Vector A
    concat                : ⦃ Storable A ⦄ → List (Vector A) → Vector A
    force                 : ⦃ Storable A ⦄ → Vector A → Vector A
    _//_                  : ⦃ Storable A ⦄ → Vector A → List (Tuple2 Int A) → Vector A
    update-               : ⦃ Storable A ⦄ → Vector A → Vector Int → Vector A → Vector A
    unsafeUpd             : ⦃ Storable A ⦄ → Vector A → List (Tuple2 Int A) → Vector A
    unsafeUpdate-         : ⦃ Storable A ⦄ → Vector A → Vector Int → Vector A → Vector A
    accum                 : ⦃ Storable A ⦄ → (A → B → A) → Vector A → List (Tuple2 Int B) → Vector A
    accumulate-           : ⦃ Storable A ⦄ → ⦃ Storable B ⦄ → (A → B → A) → Vector A → Vector Int → Vector B → Vector A
    unsafeAccum           : ⦃ Storable A ⦄ → (A → B → A) → Vector A → List (Tuple2 Int B) → Vector A
    unsafeAccumulate-     : ⦃ Storable A ⦄ → ⦃ Storable B ⦄ → (A → B → A) → Vector A → Vector Int → Vector B → Vector A
    reverse               : ⦃ Storable A ⦄ → Vector A → Vector A
    backpermute           : ⦃ Storable A ⦄ → Vector A → Vector Int → Vector A
    unsafeBackpermute     : ⦃ Storable A ⦄ → Vector A → Vector Int → Vector A
    modify                : ⦃ Storable A ⦄ → (∀{S} → MVector S A → ST S ⊤) → Vector A → Vector A
    map                   : ⦃ Storable A ⦄ → ⦃ Storable B ⦄ → (A → B) → Vector A → Vector B
    imap                  : ⦃ Storable A ⦄ → ⦃ Storable B ⦄ → (Int → A → B) → Vector A → Vector B
    concatMap             : ⦃ Storable A ⦄ → ⦃ Storable B ⦄ → (A → Vector B) → Vector A → Vector B
    mapM                  : ⦃ Monad M ⦄ → ⦃ Storable A ⦄ → ⦃ Storable B ⦄ → (A → M B) → Vector A → M (Vector B)
    imapM                 : ⦃ Monad M ⦄ → ⦃ Storable A ⦄ → ⦃ Storable B ⦄ → (Int → A → M B) → Vector A → M (Vector B)
    mapM-                 : ⦃ Monad M ⦄ → ⦃ Storable A ⦄ → (A → M B) → Vector A → M ⊤′
    imapM-                : ⦃ Monad M ⦄ → ⦃ Storable A ⦄ → (Int → A → M B) → Vector A → M ⊤′
    forM                  : ⦃ Monad M ⦄ → ⦃ Storable A ⦄ → ⦃ Storable B ⦄ → Vector A → (A → M B) → M (Vector B)
    forM-                 : ⦃ Monad M ⦄ → ⦃ Storable A ⦄ → Vector A → (A → M B) → M ⊤′
    iforM                 : ⦃ Monad M ⦄ → ⦃ Storable A ⦄ → ⦃ Storable B ⦄ → Vector A → (Int → A → M B) → M (Vector B)
    iforM-                : ⦃ Monad M ⦄ → ⦃ Storable A ⦄ → Vector A → (Int → A → M B) → M ⊤′
    zipWith               : ⦃ Storable A ⦄ → ⦃ Storable B ⦄ → ⦃ Storable C ⦄ → (A → B → C) → Vector A → Vector B → Vector C
    zipWith3              : ⦃ Storable A ⦄ → ⦃ Storable B ⦄ → ⦃ Storable C ⦄ → ⦃ Storable D ⦄ → (A → B → C → D) → Vector A → Vector B → Vector C → Vector D
    zipWith4              : ⦃ Storable A ⦄ → ⦃ Storable B ⦄ → ⦃ Storable C ⦄ → ⦃ Storable D ⦄ → ⦃ Storable E ⦄ → (A → B → C → D → E) → Vector A → Vector B → Vector C → Vector D → Vector E
    zipWith5              : ⦃ Storable A ⦄ → ⦃ Storable B ⦄ → ⦃ Storable C ⦄ → ⦃ Storable D ⦄ → ⦃ Storable E ⦄ → ⦃ Storable F ⦄ → (A → B → C → D → E → F) → Vector A → Vector B → Vector C → Vector D → Vector E → Vector F
    zipWith6              : ⦃ Storable A ⦄ → ⦃ Storable B ⦄ → ⦃ Storable C ⦄ → ⦃ Storable D ⦄ → ⦃ Storable E ⦄ → ⦃ Storable F ⦄ → ⦃ Storable G ⦄ → (A → B → C → D → E → F → G) → Vector A → Vector B → Vector C → Vector D → Vector E → Vector F → Vector G
    izipWith              : ⦃ Storable A ⦄ → ⦃ Storable B ⦄ → ⦃ Storable C ⦄ → (Int → A → B → C) → Vector A → Vector B → Vector C
    izipWith3             : ⦃ Storable A ⦄ → ⦃ Storable B ⦄ → ⦃ Storable C ⦄ → ⦃ Storable D ⦄ → (Int → A → B → C → D) → Vector A → Vector B → Vector C → Vector D
    izipWith4             : ⦃ Storable A ⦄ → ⦃ Storable B ⦄ → ⦃ Storable C ⦄ → ⦃ Storable D ⦄ → ⦃ Storable E ⦄ → (Int → A → B → C → D → E) → Vector A → Vector B → Vector C → Vector D → Vector E
    izipWith5             : ⦃ Storable A ⦄ → ⦃ Storable B ⦄ → ⦃ Storable C ⦄ → ⦃ Storable D ⦄ → ⦃ Storable E ⦄ → ⦃ Storable F ⦄ → (Int → A → B → C → D → E → F) → Vector A → Vector B → Vector C → Vector D → Vector E → Vector F
    izipWith6             : ⦃ Storable A ⦄ → ⦃ Storable B ⦄ → ⦃ Storable C ⦄ → ⦃ Storable D ⦄ → ⦃ Storable E ⦄ → ⦃ Storable F ⦄ → ⦃ Storable G ⦄ → (Int → A → B → C → D → E → F → G) → Vector A → Vector B → Vector C → Vector D → Vector E → Vector F → Vector G
    zipWithM              : ⦃ Monad M ⦄ → ⦃ Storable A ⦄ → ⦃ Storable B ⦄ → ⦃ Storable C ⦄ → (A → B → M C) → Vector A → Vector B → M (Vector C)
    izipWithM             : ⦃ Monad M ⦄ → ⦃ Storable A ⦄ → ⦃ Storable B ⦄ → ⦃ Storable C ⦄ → (Int → A → B → M C) → Vector A → Vector B → M (Vector C)
    zipWithM-             : ⦃ Monad M ⦄ → ⦃ Storable A ⦄ → ⦃ Storable B ⦄ → (A → B → M C) → Vector A → Vector B → M ⊤′
    izipWithM-            : ⦃ Monad M ⦄ → ⦃ Storable A ⦄ → ⦃ Storable B ⦄ → (Int → A → B → M C) → Vector A → Vector B → M ⊤′
    filter                : ⦃ Storable A ⦄ → (A → Bool) → Vector A → Vector A
    ifilter               : ⦃ Storable A ⦄ → (Int → A → Bool) → Vector A → Vector A
    filterM               : ⦃ Monad M ⦄ → ⦃ Storable A ⦄ → (A → M (Liftℓ _ Bool)) → Vector A → M (Vector A)
    uniq                  : ⦃ Storable A ⦄ → ⦃ Eq A ⦄ → Vector A → Vector A
    mapMaybe              : ⦃ Storable A ⦄ → ⦃ Storable B ⦄ → (A → Maybe B) → Vector A → Vector B
    imapMaybe             : ⦃ Storable A ⦄ → ⦃ Storable B ⦄ → (Int → A → Maybe B) → Vector A → Vector B
    mapMaybeM             : ⦃ Monad M ⦄ → ⦃ Storable A ⦄ → ⦃ Storable B ⦄ → (A → M (Maybe B)) → Vector A → M (Vector B)
    imapMaybeM            : ⦃ Monad M ⦄ → ⦃ Storable A ⦄ → ⦃ Storable B ⦄ → (Int → A → M (Maybe B)) → Vector A → M (Vector B)
    takeWhile             : ⦃ Storable A ⦄ → (A → Bool) → Vector A → Vector A
    dropWhile             : ⦃ Storable A ⦄ → (A → Bool) → Vector A → Vector A
    partition             : ⦃ Storable A ⦄ → (A → Bool) → Vector A → Tuple2 (Vector A) (Vector A)
    unstablePartition     : ⦃ Storable A ⦄ → (A → Bool) → Vector A → Tuple2 (Vector A) (Vector A)
    partitionWith         : ⦃ Storable A ⦄ → ⦃ Storable B ⦄ → ⦃ Storable C ⦄ → (A → Either B C) → Vector A → Tuple2 (Vector B) (Vector C)
    span                  : ⦃ Storable A ⦄ → (A → Bool) → Vector A → Tuple2 (Vector A) (Vector A)
    break                 : ⦃ Storable A ⦄ → (A → Bool) → Vector A → Tuple2 (Vector A) (Vector A)
    -- todo: (req v13) groupBy               : ⦃ Storable A ⦄ → (A → A → Bool) → Vector A → List (Vector A)
    -- todo: (req v13) group                 : ⦃ Storable A ⦄ → ⦃ Eq A ⦄ → Vector A → List (Vector A)
    elem                  : ⦃ Storable A ⦄ → ⦃ Eq A ⦄ → A → Vector A → Bool
    notElem               : ⦃ Storable A ⦄ → ⦃ Eq A ⦄ → A → Vector A → Bool
    find                  : ⦃ Storable A ⦄ → (A → Bool) → Vector A → Maybe A
    findIndex             : ⦃ Storable A ⦄ → (A → Bool) → Vector A → Maybe Int
    findIndices           : ⦃ Storable A ⦄ → (A → Bool) → Vector A → Vector Int
    elemIndex             : ⦃ Storable A ⦄ → ⦃ Eq A ⦄ → A → Vector A → Maybe Int
    elemIndices           : ⦃ Storable A ⦄ → ⦃ Eq A ⦄ → A → Vector A → Vector Int
    foldl                 : ⦃ Storable B ⦄ → (A → B → A) → A → Vector B → A
    foldl1                : ⦃ Storable A ⦄ → (A → A → A) → Vector A → A
    foldl'                : ⦃ Storable B ⦄ → (A → B → A) → A → Vector B → A
    foldl1'               : ⦃ Storable A ⦄ → (A → A → A) → Vector A → A
    foldr                 : ⦃ Storable A ⦄ → (A → B → B) → B → Vector A → B
    foldr1                : ⦃ Storable A ⦄ → (A → A → A) → Vector A → A
    foldr'                : ⦃ Storable A ⦄ → (A → B → B) → B → Vector A → B
    foldr1'               : ⦃ Storable A ⦄ → (A → A → A) → Vector A → A
    ifoldl                : ⦃ Storable B ⦄ → (A → Int → B → A) → A → Vector B → A
    ifoldl'               : ⦃ Storable B ⦄ → (A → Int → B → A) → A → Vector B → A
    ifoldr                : ⦃ Storable A ⦄ → (Int → A → B → B) → B → Vector A → B
    ifoldr'               : ⦃ Storable A ⦄ → (Int → A → B → B) → B → Vector A → B
    foldMap               : ⦃ Monoid B ⦄ → ⦃ Storable A ⦄ → (A → B) → Vector A → B
    foldMap'              : ⦃ Monoid B ⦄ → ⦃ Storable A ⦄ → (A → B) → Vector A → B
    all                   : ⦃ Storable A ⦄ → (A → Bool) → Vector A → Bool
    any                   : ⦃ Storable A ⦄ → (A → Bool) → Vector A → Bool
    and                   : Vector Bool → Bool
    or                    : Vector Bool → Bool
    sum                   : ⦃ Storable A ⦄ → ⦃ Num A ⦄ → Vector A → A
    product               : ⦃ Storable A ⦄ → ⦃ Num A ⦄ → Vector A → A
    maximum               : ⦃ Storable A ⦄ → ⦃ Ord A ⦄ → Vector A → A
    maximumBy             : ⦃ Storable A ⦄ → (A → A → Ordering) → Vector A → A
    -- todo: (req v13) maximumOn             : ⦃ Ord B ⦄ → ⦃ Storable A ⦄ → (A → B) → Vector A → A
    minimum               : ⦃ Storable A ⦄ → ⦃ Ord A ⦄ → Vector A → A
    minimumBy             : ⦃ Storable A ⦄ → (A → A → Ordering) → Vector A → A
    -- todo: (req v13) minimumOn             : ⦃ Ord B ⦄ → ⦃ Storable A ⦄ → (A → B) → Vector A → A
    minIndex              : ⦃ Storable A ⦄ → ⦃ Ord A ⦄ → Vector A → Int
    minIndexBy            : ⦃ Storable A ⦄ → (A → A → Ordering) → Vector A → Int
    maxIndex              : ⦃ Storable A ⦄ → ⦃ Ord A ⦄ → Vector A → Int
    maxIndexBy            : ⦃ Storable A ⦄ → (A → A → Ordering) → Vector A → Int
    foldM                 : ⦃ Monad M ⦄ → ⦃ Storable B ⦄ → (A → B → M A) → A → Vector B → M A
    ifoldM                : ⦃ Monad M ⦄ → ⦃ Storable B ⦄ → (A → Int → B → M A) → A → Vector B → M A
    foldM'                : ⦃ Monad M ⦄ → ⦃ Storable B ⦄ → (A → B → M A) → A → Vector B → M A
    ifoldM'               : ⦃ Monad M ⦄ → ⦃ Storable B ⦄ → (A → Int → B → M A) → A → Vector B → M A
    fold1M                : ⦃ Monad M ⦄ → ⦃ Storable A ⦄ → (A → A → M A) → Vector A → M A
    fold1M'               : ⦃ Monad M ⦄ → ⦃ Storable A ⦄ → (A → A → M A) → Vector A → M A
    foldM-                : ⦃ Monad M ⦄ → ⦃ Storable B ⦄ → (A → B → M A) → A → Vector B → M ⊤′
    ifoldM-               : ⦃ Monad M ⦄ → ⦃ Storable B ⦄ → (A → Int → B → M A) → A → Vector B → M ⊤′
    foldM'-               : ⦃ Monad M ⦄ → ⦃ Storable B ⦄ → (A → B → M A) → A → Vector B → M ⊤′
    ifoldM'-              : ⦃ Monad M ⦄ → ⦃ Storable B ⦄ → (A → Int → B → M A) → A → Vector B → M ⊤′
    fold1M-               : ⦃ Monad M ⦄ → ⦃ Storable A ⦄ → (A → A → M A) → Vector A → M ⊤′
    fold1M'-              : ⦃ Monad M ⦄ → ⦃ Storable A ⦄ → (A → A → M A) → Vector A → M ⊤′
    prescanl              : ⦃ Storable A ⦄ → ⦃ Storable B ⦄ → (A → B → A) → A → Vector B → Vector A
    prescanl'             : ⦃ Storable A ⦄ → ⦃ Storable B ⦄ → (A → B → A) → A → Vector B → Vector A
    postscanl             : ⦃ Storable A ⦄ → ⦃ Storable B ⦄ → (A → B → A) → A → Vector B → Vector A
    postscanl'            : ⦃ Storable A ⦄ → ⦃ Storable B ⦄ → (A → B → A) → A → Vector B → Vector A
    scanl                 : ⦃ Storable A ⦄ → ⦃ Storable B ⦄ → (A → B → A) → A → Vector B → Vector A
    scanl'                : ⦃ Storable A ⦄ → ⦃ Storable B ⦄ → (A → B → A) → A → Vector B → Vector A
    scanl1                : ⦃ Storable A ⦄ → (A → A → A) → Vector A → Vector A
    scanl1'               : ⦃ Storable A ⦄ → (A → A → A) → Vector A → Vector A
    iscanl                : ⦃ Storable A ⦄ → ⦃ Storable B ⦄ → (Int → A → B → A) → A → Vector B → Vector A
    iscanl'               : ⦃ Storable A ⦄ → ⦃ Storable B ⦄ → (Int → A → B → A) → A → Vector B → Vector A
    prescanr              : ⦃ Storable A ⦄ → ⦃ Storable B ⦄ → (A → B → B) → B → Vector A → Vector B
    prescanr'             : ⦃ Storable A ⦄ → ⦃ Storable B ⦄ → (A → B → B) → B → Vector A → Vector B
    postscanr             : ⦃ Storable A ⦄ → ⦃ Storable B ⦄ → (A → B → B) → B → Vector A → Vector B
    postscanr'            : ⦃ Storable A ⦄ → ⦃ Storable B ⦄ → (A → B → B) → B → Vector A → Vector B
    scanr                 : ⦃ Storable A ⦄ → ⦃ Storable B ⦄ → (A → B → B) → B → Vector A → Vector B
    scanr'                : ⦃ Storable A ⦄ → ⦃ Storable B ⦄ → (A → B → B) → B → Vector A → Vector B
    scanr1                : ⦃ Storable A ⦄ → (A → A → A) → Vector A → Vector A
    scanr1'               : ⦃ Storable A ⦄ → (A → A → A) → Vector A → Vector A
    iscanr                : ⦃ Storable A ⦄ → ⦃ Storable B ⦄ → (Int → A → B → B) → B → Vector A → Vector B
    iscanr'               : ⦃ Storable A ⦄ → ⦃ Storable B ⦄ → (Int → A → B → B) → B → Vector A → Vector B
    eqBy                  : ⦃ Storable A ⦄ → ⦃ Storable B ⦄ → (A → B → Bool) → Vector A → Vector B → Bool
    cmpBy                 : ⦃ Storable A ⦄ → ⦃ Storable B ⦄ → (A → B → Ordering) → Vector A → Vector B → Ordering
    isSameVector          : ⦃ Storable A ⦄ → Vector A → Vector A → Bool
    toList                : ⦃ Storable A ⦄ → Vector A → List A
    fromList              : ⦃ Storable A ⦄ → List A → Vector A
    fromListN             : ⦃ Storable A ⦄ → Int → List A → Vector A
    unsafeCast            : ⦃ Storable A ⦄ → ⦃ Storable B ⦄ → Vector A → Vector B
    -- todo: (req v13) unsafeCoerceVector    : ⦃ Coercible A B ⦄ → Vector A → Vector B
    freeze                : ⦃ Storable A ⦄ → ⦃ _ : PrimMonad M ⦄ → MVector (PrimState M) A → M (Vector A)
    thaw                  : ⦃ Storable A ⦄ → ⦃ _ : PrimMonad M ⦄ → Vector A → M (MVector (PrimState M) A)
    copy                  : ⦃ Storable A ⦄ → ⦃ _ : PrimMonad M ⦄ → MVector (PrimState M) A → Vector A → M ⊤′
    unsafeFreeze          : ⦃ Storable A ⦄ → ⦃ _ : PrimMonad M ⦄ → MVector (PrimState M) A → M (Vector A)
    unsafeThaw            : ⦃ Storable A ⦄ → ⦃ _ : PrimMonad M ⦄ → Vector A → M (MVector (PrimState M) A)
    unsafeCopy            : ⦃ Storable A ⦄ → ⦃ _ : PrimMonad M ⦄ → MVector (PrimState M) A → Vector A → M ⊤′
    unsafeFromForeignPtr  : ⦃ Storable A ⦄ → ForeignPtr A → Int → Int → Vector A
    unsafeFromForeignPtr0 : ⦃ Storable A ⦄ → ForeignPtr A → Int → Vector A
    unsafeToForeignPtr    : ⦃ Storable A ⦄ → Vector A → Tuple3 (ForeignPtr A) Int Int
    unsafeToForeignPtr0   : ⦃ Storable A ⦄ → Vector A → Tuple2 (ForeignPtr A) Int
    unsafeWith            : ⦃ Storable A ⦄ → Vector A → (Ptr A → IO B) → IO B

{-# COMPILE GHC length                = \ aℓ a AgdaStorable                                                                                                             ->  Data.Vector.Storable.length                #-}
{-# COMPILE GHC null                  = \ aℓ a AgdaStorable                                                                                                             ->  Data.Vector.Storable.null                  #-}
{-# COMPILE GHC _!_                   = \ aℓ a AgdaStorable                                                                                                             -> (Data.Vector.Storable.!)                    #-}
{-# COMPILE GHC _!?_                  = \ aℓ a AgdaStorable                                                                                                             -> (Data.Vector.Storable.!?)                   #-}
{-# COMPILE GHC head                  = \ aℓ a AgdaStorable                                                                                                             ->  Data.Vector.Storable.head                  #-}
{-# COMPILE GHC last                  = \ aℓ a AgdaStorable                                                                                                             ->  Data.Vector.Storable.last                  #-}
{-# COMPILE GHC unsafeIndex           = \ aℓ a AgdaStorable                                                                                                             ->  Data.Vector.Storable.unsafeIndex           #-}
{-# COMPILE GHC unsafeHead            = \ aℓ a AgdaStorable                                                                                                             ->  Data.Vector.Storable.unsafeHead            #-}
{-# COMPILE GHC unsafeLast            = \ aℓ a AgdaStorable                                                                                                             ->  Data.Vector.Storable.unsafeLast            #-}
{-# COMPILE GHC indexM                = \ aℓ a m AgdaStorable AgdaMonad                                                                                                 ->  Data.Vector.Storable.indexM                #-}
{-# COMPILE GHC headM                 = \ aℓ a m AgdaStorable AgdaMonad                                                                                                 ->  Data.Vector.Storable.headM                 #-}
{-# COMPILE GHC lastM                 = \ aℓ a m AgdaStorable AgdaMonad                                                                                                 ->  Data.Vector.Storable.lastM                 #-}
{-# COMPILE GHC unsafeIndexM          = \ aℓ a m AgdaStorable AgdaMonad                                                                                                 ->  Data.Vector.Storable.unsafeIndexM          #-}
{-# COMPILE GHC unsafeHeadM           = \ aℓ a m AgdaStorable AgdaMonad                                                                                                 ->  Data.Vector.Storable.unsafeHeadM           #-}
{-# COMPILE GHC unsafeLastM           = \ aℓ a m AgdaStorable AgdaMonad                                                                                                 ->  Data.Vector.Storable.unsafeLastM           #-}
{-# COMPILE GHC slice                 = \ aℓ a AgdaStorable                                                                                                             ->  Data.Vector.Storable.slice                 #-}
{-# COMPILE GHC init                  = \ aℓ a AgdaStorable                                                                                                             ->  Data.Vector.Storable.init                  #-}
{-# COMPILE GHC tail                  = \ aℓ a AgdaStorable                                                                                                             ->  Data.Vector.Storable.tail                  #-}
{-# COMPILE GHC take                  = \ aℓ a AgdaStorable                                                                                                             ->  Data.Vector.Storable.take                  #-}
{-# COMPILE GHC drop                  = \ aℓ a AgdaStorable                                                                                                             ->  Data.Vector.Storable.drop                  #-}
{-# COMPILE GHC splitAt               = \ aℓ a AgdaStorable                                                                                                             ->  Data.Vector.Storable.splitAt               #-}
{-# COMPILE GHC uncons                = \ aℓ a AgdaStorable                                                                                                             ->  Data.Vector.Storable.uncons                #-}
{-# COMPILE GHC unsnoc                = \ aℓ a AgdaStorable                                                                                                             ->  Data.Vector.Storable.unsnoc                #-}
{-# COMPILE GHC unsafeSlice           = \ aℓ a AgdaStorable                                                                                                             ->  Data.Vector.Storable.unsafeSlice           #-}
{-# COMPILE GHC unsafeInit            = \ aℓ a AgdaStorable                                                                                                             ->  Data.Vector.Storable.unsafeInit            #-}
{-# COMPILE GHC unsafeTail            = \ aℓ a AgdaStorable                                                                                                             ->  Data.Vector.Storable.unsafeTail            #-}
{-# COMPILE GHC unsafeTake            = \ aℓ a AgdaStorable                                                                                                             ->  Data.Vector.Storable.unsafeTake            #-}
{-# COMPILE GHC unsafeDrop            = \ aℓ a AgdaStorable                                                                                                             ->  Data.Vector.Storable.unsafeDrop            #-}
{-# COMPILE GHC empty                 = \ aℓ a AgdaStorable                                                                                                             ->  Data.Vector.Storable.empty                 #-}
{-# COMPILE GHC singleton             = \ aℓ a AgdaStorable                                                                                                             ->  Data.Vector.Storable.singleton             #-}
{-# COMPILE GHC replicate             = \ aℓ a AgdaStorable                                                                                                             ->  Data.Vector.Storable.replicate             #-}
{-# COMPILE GHC generate              = \ aℓ a AgdaStorable                                                                                                             ->  Data.Vector.Storable.generate              #-}
{-# COMPILE GHC iterateN              = \ aℓ a AgdaStorable                                                                                                             ->  Data.Vector.Storable.iterateN              #-}
{-# COMPILE GHC replicateM            = \ mℓ m a AgdaMonad AgdaStorable                                                                                                 ->  Data.Vector.Storable.replicateM            #-}
{-# COMPILE GHC generateM             = \ mℓ m a AgdaMonad AgdaStorable                                                                                                 ->  Data.Vector.Storable.generateM             #-}
{-# COMPILE GHC iterateNM             = \ mℓ m a AgdaMonad AgdaStorable                                                                                                 ->  Data.Vector.Storable.iterateNM             #-}
{-# COMPILE GHC create                = \ aℓ a AgdaStorable f                                                                                                           ->  Data.Vector.Storable.create (f ())         #-}
{-# COMPILE GHC createT               = \ aℓ f a AgdaTraversable AgdaStorable g                                                                                         ->  Data.Vector.Storable.createT (g ())        #-}
{-# COMPILE GHC unfoldr               = \ aℓ a bℓ b AgdaStorable                                                                                                        ->  Data.Vector.Storable.unfoldr               #-}
{-# COMPILE GHC unfoldrN              = \ aℓ a bℓ b AgdaStorable                                                                                                        ->  Data.Vector.Storable.unfoldrN              #-}
{-# COMPILE GHC unfoldrExactN         = \ aℓ a bℓ b AgdaStorable                                                                                                        ->  Data.Vector.Storable.unfoldrExactN         #-}
{-# COMPILE GHC unfoldrM              = \ aℓ a b m AgdaMonad AgdaStorable                                                                                               ->  Data.Vector.Storable.unfoldrM              #-}
{-# COMPILE GHC unfoldrNM             = \ aℓ a b m AgdaMonad AgdaStorable                                                                                               ->  Data.Vector.Storable.unfoldrNM             #-}
{-# COMPILE GHC unfoldrExactNM        = \ aℓ a b m AgdaMonad AgdaStorable                                                                                               ->  Data.Vector.Storable.unfoldrExactNM        #-}
{-# COMPILE GHC constructN            = \ aℓ a AgdaStorable                                                                                                             ->  Data.Vector.Storable.constructN            #-}
{-# COMPILE GHC constructrN           = \ aℓ a AgdaStorable                                                                                                             ->  Data.Vector.Storable.constructrN           #-}
{-# COMPILE GHC enumFromN             = \ aℓ a AgdaStorable AgdaNum                                                                                                     ->  Data.Vector.Storable.enumFromN             #-}
{-# COMPILE GHC enumFromStepN         = \ aℓ a AgdaStorable AgdaNum                                                                                                     ->  Data.Vector.Storable.enumFromStepN         #-}
{-# COMPILE GHC enumFromTo            = \ aℓ a AgdaStorable AgdaEnum                                                                                                    ->  Data.Vector.Storable.enumFromTo            #-}
{-# COMPILE GHC enumFromThenTo        = \ aℓ a AgdaStorable AgdaEnum                                                                                                    ->  Data.Vector.Storable.enumFromThenTo        #-}
{-# COMPILE GHC cons                  = \ aℓ a AgdaStorable                                                                                                             ->  Data.Vector.Storable.cons                  #-}
{-# COMPILE GHC snoc                  = \ aℓ a AgdaStorable                                                                                                             ->  Data.Vector.Storable.snoc                  #-}
{-# COMPILE GHC _++_                  = \ aℓ a AgdaStorable                                                                                                             -> (Data.Vector.Storable.++)                   #-}
{-# COMPILE GHC concat                = \ aℓ a AgdaStorable                                                                                                             ->  Data.Vector.Storable.concat                #-}
{-# COMPILE GHC force                 = \ aℓ a AgdaStorable                                                                                                             ->  Data.Vector.Storable.force                 #-}
{-# COMPILE GHC _//_                  = \ aℓ a AgdaStorable                                                                                                             -> (Data.Vector.Storable.//)                   #-}
{-# COMPILE GHC update-               = \ aℓ a AgdaStorable                                                                                                             ->  Data.Vector.Storable.update_               #-}
{-# COMPILE GHC unsafeUpd             = \ aℓ a AgdaStorable                                                                                                             ->  Data.Vector.Storable.unsafeUpd             #-}
{-# COMPILE GHC unsafeUpdate-         = \ aℓ a AgdaStorable                                                                                                             ->  Data.Vector.Storable.unsafeUpdate_         #-}
{-# COMPILE GHC accum                 = \ aℓ a bℓ b AgdaStorable                                                                                                        ->  Data.Vector.Storable.accum                 #-}
{-# COMPILE GHC accumulate-           = \ aℓ a bℓ b AgdaStorable AgdaStorable                                                                                           ->  Data.Vector.Storable.accumulate_           #-}
{-# COMPILE GHC unsafeAccum           = \ aℓ a bℓ b AgdaStorable                                                                                                        ->  Data.Vector.Storable.unsafeAccum           #-}
{-# COMPILE GHC unsafeAccumulate-     = \ aℓ a bℓ b AgdaStorable AgdaStorable                                                                                           ->  Data.Vector.Storable.unsafeAccumulate_     #-}
{-# COMPILE GHC reverse               = \ aℓ a AgdaStorable                                                                                                             ->  Data.Vector.Storable.reverse               #-}
{-# COMPILE GHC backpermute           = \ aℓ a AgdaStorable                                                                                                             ->  Data.Vector.Storable.backpermute           #-}
{-# COMPILE GHC unsafeBackpermute     = \ aℓ a AgdaStorable                                                                                                             ->  Data.Vector.Storable.unsafeBackpermute     #-}
{-# COMPILE GHC modify                = \ aℓ a AgdaStorable f                                                                                                           ->  Data.Vector.Storable.modify (f ())         #-}
{-# COMPILE GHC map                   = \ aℓ a bℓ b AgdaStorable AgdaStorable                                                                                           ->  Data.Vector.Storable.map                   #-}
{-# COMPILE GHC imap                  = \ aℓ a bℓ b AgdaStorable AgdaStorable                                                                                           ->  Data.Vector.Storable.imap                  #-}
{-# COMPILE GHC concatMap             = \ aℓ a bℓ b AgdaStorable AgdaStorable                                                                                           ->  Data.Vector.Storable.concatMap             #-}
{-# COMPILE GHC mapM                  = \ mℓ m aℓ a b AgdaMonad AgdaStorable AgdaStorable                                                                               ->  Data.Vector.Storable.mapM                  #-}
{-# COMPILE GHC imapM                 = \ mℓ m aℓ a b AgdaMonad AgdaStorable AgdaStorable                                                                               ->  Data.Vector.Storable.imapM                 #-}
{-# COMPILE GHC mapM-                 = \ mℓ m aℓ a b AgdaMonad AgdaStorable                                                                                            ->  Data.Vector.Storable.mapM_                 #-}
{-# COMPILE GHC imapM-                = \ mℓ m aℓ a b AgdaMonad AgdaStorable                                                                                            ->  Data.Vector.Storable.imapM_                #-}
{-# COMPILE GHC forM                  = \ mℓ m aℓ a b AgdaMonad AgdaStorable AgdaStorable                                                                               ->  Data.Vector.Storable.forM                  #-}
{-# COMPILE GHC forM-                 = \ mℓ m aℓ a b AgdaMonad AgdaStorable                                                                                            ->  Data.Vector.Storable.forM_                 #-}
{-# COMPILE GHC iforM                 = \ mℓ m aℓ a b AgdaMonad AgdaStorable AgdaStorable                                                                               ->  Data.Vector.Storable.iforM                 #-}
{-# COMPILE GHC iforM-                = \ mℓ m aℓ a b AgdaMonad AgdaStorable                                                                                            ->  Data.Vector.Storable.iforM_                #-}
{-# COMPILE GHC zipWith               = \ aℓ a bℓ b cℓ c AgdaStorable AgdaStorable AgdaStorable                                                                         ->  Data.Vector.Storable.zipWith               #-}
{-# COMPILE GHC zipWith3              = \ aℓ a bℓ b cℓ c dℓ d AgdaStorable AgdaStorable AgdaStorable AgdaStorable                                                       ->  Data.Vector.Storable.zipWith3              #-}
{-# COMPILE GHC zipWith4              = \ aℓ a bℓ b cℓ c dℓ d eℓ e AgdaStorable AgdaStorable AgdaStorable AgdaStorable AgdaStorable                                     ->  Data.Vector.Storable.zipWith4              #-}
{-# COMPILE GHC zipWith5              = \ aℓ a bℓ b cℓ c dℓ d eℓ e fℓ f AgdaStorable AgdaStorable AgdaStorable AgdaStorable AgdaStorable AgdaStorable                   ->  Data.Vector.Storable.zipWith5              #-}
{-# COMPILE GHC zipWith6              = \ aℓ a bℓ b cℓ c dℓ d eℓ e fℓ f gℓ g AgdaStorable AgdaStorable AgdaStorable AgdaStorable AgdaStorable AgdaStorable AgdaStorable ->  Data.Vector.Storable.zipWith6              #-}
{-# COMPILE GHC izipWith              = \ aℓ a bℓ b cℓ c AgdaStorable AgdaStorable AgdaStorable                                                                         ->  Data.Vector.Storable.izipWith              #-}
{-# COMPILE GHC izipWith3             = \ aℓ a bℓ b cℓ c dℓ d AgdaStorable AgdaStorable AgdaStorable AgdaStorable                                                       ->  Data.Vector.Storable.izipWith3             #-}
{-# COMPILE GHC izipWith4             = \ aℓ a bℓ b cℓ c dℓ d eℓ e AgdaStorable AgdaStorable AgdaStorable AgdaStorable AgdaStorable                                     ->  Data.Vector.Storable.izipWith4             #-}
{-# COMPILE GHC izipWith5             = \ aℓ a bℓ b cℓ c dℓ d eℓ e fℓ f AgdaStorable AgdaStorable AgdaStorable AgdaStorable AgdaStorable AgdaStorable                   ->  Data.Vector.Storable.izipWith5             #-}
{-# COMPILE GHC izipWith6             = \ aℓ a bℓ b cℓ c dℓ d eℓ e fℓ f gℓ g AgdaStorable AgdaStorable AgdaStorable AgdaStorable AgdaStorable AgdaStorable AgdaStorable ->  Data.Vector.Storable.izipWith6             #-}
{-# COMPILE GHC zipWithM              = \ mℓ m aℓ a bℓ b c AgdaMonad AgdaStorable AgdaStorable AgdaStorable                                                             ->  Data.Vector.Storable.zipWithM              #-}
{-# COMPILE GHC izipWithM             = \ mℓ m aℓ a bℓ b c AgdaMonad AgdaStorable AgdaStorable AgdaStorable                                                             ->  Data.Vector.Storable.izipWithM             #-}
{-# COMPILE GHC zipWithM-             = \ mℓ m aℓ a bℓ b c AgdaMonad AgdaStorable AgdaStorable                                                                          ->  Data.Vector.Storable.zipWithM_             #-}
{-# COMPILE GHC izipWithM-            = \ mℓ m aℓ a bℓ b c AgdaMonad AgdaStorable AgdaStorable                                                                          ->  Data.Vector.Storable.izipWithM_            #-}
{-# COMPILE GHC filter                = \ aℓ a AgdaStorable                                                                                                             ->  Data.Vector.Storable.filter                #-}
{-# COMPILE GHC ifilter               = \ aℓ a AgdaStorable                                                                                                             ->  Data.Vector.Storable.ifilter               #-}
{-# COMPILE GHC filterM               = \ mℓ m a AgdaMonad AgdaStorable                                                                                                 ->  Data.Vector.Storable.filterM               #-}
{-# COMPILE GHC uniq                  = \ aℓ a AgdaStorable AgdaEq                                                                                                      ->  Data.Vector.Storable.uniq                  #-}
{-# COMPILE GHC mapMaybe              = \ aℓ a bℓ b AgdaStorable AgdaStorable                                                                                           ->  Data.Vector.Storable.mapMaybe              #-}
{-# COMPILE GHC imapMaybe             = \ aℓ a bℓ b AgdaStorable AgdaStorable                                                                                           ->  Data.Vector.Storable.imapMaybe             #-}
{-# COMPILE GHC mapMaybeM             = \ mℓ m aℓ a b AgdaMonad AgdaStorable AgdaStorable                                                                               ->  Data.Vector.Storable.mapMaybeM             #-}
{-# COMPILE GHC imapMaybeM            = \ mℓ m aℓ a b AgdaMonad AgdaStorable AgdaStorable                                                                               ->  Data.Vector.Storable.imapMaybeM            #-}
{-# COMPILE GHC takeWhile             = \ aℓ a AgdaStorable                                                                                                             ->  Data.Vector.Storable.takeWhile             #-}
{-# COMPILE GHC dropWhile             = \ aℓ a AgdaStorable                                                                                                             ->  Data.Vector.Storable.dropWhile             #-}
{-# COMPILE GHC partition             = \ aℓ a AgdaStorable                                                                                                             ->  Data.Vector.Storable.partition             #-}
{-# COMPILE GHC unstablePartition     = \ aℓ a AgdaStorable                                                                                                             ->  Data.Vector.Storable.unstablePartition     #-}
{-# COMPILE GHC partitionWith         = \ aℓ a bℓ b cℓ c AgdaStorable AgdaStorable AgdaStorable                                                                         ->  Data.Vector.Storable.partitionWith         #-}
{-# COMPILE GHC span                  = \ aℓ a AgdaStorable                                                                                                             ->  Data.Vector.Storable.span                  #-}
{-# COMPILE GHC break                 = \ aℓ a AgdaStorable                                                                                                             ->  Data.Vector.Storable.break                 #-}
-- {-# COMPILE GHC groupBy               = \ aℓ a AgdaStorable                                                                                                             ->  Data.Vector.Storable.groupBy               #-}
-- {-# COMPILE GHC group                 = \ aℓ a AgdaStorable AgdaEq                                                                                                      ->  Data.Vector.Storable.group                 #-}
{-# COMPILE GHC elem                  = \ aℓ a AgdaStorable AgdaEq                                                                                                      ->  Data.Vector.Storable.elem                  #-}
{-# COMPILE GHC notElem               = \ aℓ a AgdaStorable AgdaEq                                                                                                      ->  Data.Vector.Storable.notElem               #-}
{-# COMPILE GHC find                  = \ aℓ a AgdaStorable                                                                                                             ->  Data.Vector.Storable.find                  #-}
{-# COMPILE GHC findIndex             = \ aℓ a AgdaStorable                                                                                                             ->  Data.Vector.Storable.findIndex             #-}
{-# COMPILE GHC findIndices           = \ aℓ a AgdaStorable                                                                                                             ->  Data.Vector.Storable.findIndices           #-}
{-# COMPILE GHC elemIndex             = \ aℓ a AgdaStorable AgdaEq                                                                                                      ->  Data.Vector.Storable.elemIndex             #-}
{-# COMPILE GHC elemIndices           = \ aℓ a AgdaStorable AgdaEq                                                                                                      ->  Data.Vector.Storable.elemIndices           #-}
{-# COMPILE GHC foldl                 = \ aℓ a bℓ b AgdaStorable                                                                                                        ->  Data.Vector.Storable.foldl                 #-}
{-# COMPILE GHC foldl1                = \ aℓ a AgdaStorable                                                                                                             ->  Data.Vector.Storable.foldl1                #-}
{-# COMPILE GHC foldl'                = \ aℓ a bℓ b AgdaStorable                                                                                                        ->  Data.Vector.Storable.foldl'                #-}
{-# COMPILE GHC foldl1'               = \ aℓ a AgdaStorable                                                                                                             ->  Data.Vector.Storable.foldl1'               #-}
{-# COMPILE GHC foldr                 = \ aℓ a bℓ b AgdaStorable                                                                                                        ->  Data.Vector.Storable.foldr                 #-}
{-# COMPILE GHC foldr1                = \ aℓ a AgdaStorable                                                                                                             ->  Data.Vector.Storable.foldr1                #-}
{-# COMPILE GHC foldr'                = \ aℓ a bℓ b AgdaStorable                                                                                                        ->  Data.Vector.Storable.foldr'                #-}
{-# COMPILE GHC foldr1'               = \ aℓ a AgdaStorable                                                                                                             ->  Data.Vector.Storable.foldr1'               #-}
{-# COMPILE GHC ifoldl                = \ aℓ a bℓ b AgdaStorable                                                                                                        ->  Data.Vector.Storable.ifoldl                #-}
{-# COMPILE GHC ifoldl'               = \ aℓ a bℓ b AgdaStorable                                                                                                        ->  Data.Vector.Storable.ifoldl'               #-}
{-# COMPILE GHC ifoldr                = \ aℓ a bℓ b AgdaStorable                                                                                                        ->  Data.Vector.Storable.ifoldr                #-}
{-# COMPILE GHC ifoldr'               = \ aℓ a bℓ b AgdaStorable                                                                                                        ->  Data.Vector.Storable.ifoldr'               #-}
{-# COMPILE GHC foldMap               = \ aℓ a bℓ b AgdaMonoid AgdaStorable                                                                                             ->  Data.Vector.Storable.foldMap               #-}
{-# COMPILE GHC foldMap'              = \ aℓ a bℓ b AgdaMonoid AgdaStorable                                                                                             ->  Data.Vector.Storable.foldMap'              #-}
{-# COMPILE GHC all                   = \ aℓ a AgdaStorable                                                                                                             ->  Data.Vector.Storable.all                   #-}
{-# COMPILE GHC any                   = \ aℓ a AgdaStorable                                                                                                             ->  Data.Vector.Storable.any                   #-}
{-# COMPILE GHC and                   =                                                                                                                                     Data.Vector.Storable.and                   #-}
{-# COMPILE GHC or                    =                                                                                                                                     Data.Vector.Storable.or                    #-}
{-# COMPILE GHC sum                   = \ aℓ a AgdaStorable AgdaNum                                                                                                     ->  Data.Vector.Storable.sum                   #-}
{-# COMPILE GHC product               = \ aℓ a AgdaStorable AgdaNum                                                                                                     ->  Data.Vector.Storable.product               #-}
{-# COMPILE GHC maximum               = \ aℓ a AgdaStorable AgdaOrd                                                                                                     ->  Data.Vector.Storable.maximum               #-}
{-# COMPILE GHC maximumBy             = \ aℓ a AgdaStorable                                                                                                             ->  Data.Vector.Storable.maximumBy             #-}
-- {-# COMPILE GHC maximumOn             = \ bℓ b aℓ a AgdaOrd AgdaStorable                                                                                                ->  Data.Vector.Storable.maximumOn             #-}
{-# COMPILE GHC minimum               = \ aℓ a AgdaStorable AgdaOrd                                                                                                     ->  Data.Vector.Storable.minimum               #-}
{-# COMPILE GHC minimumBy             = \ aℓ a AgdaStorable                                                                                                             ->  Data.Vector.Storable.minimumBy             #-}
-- {-# COMPILE GHC minimumOn             = \ bℓ b aℓ a AgdaOrd AgdaStorable                                                                                                ->  Data.Vector.Storable.minimumOn             #-}
{-# COMPILE GHC minIndex              = \ aℓ a AgdaStorable AgdaOrd                                                                                                     ->  Data.Vector.Storable.minIndex              #-}
{-# COMPILE GHC minIndexBy            = \ aℓ a AgdaStorable                                                                                                             ->  Data.Vector.Storable.minIndexBy            #-}
{-# COMPILE GHC maxIndex              = \ aℓ a AgdaStorable AgdaOrd                                                                                                     ->  Data.Vector.Storable.maxIndex              #-}
{-# COMPILE GHC maxIndexBy            = \ aℓ a AgdaStorable                                                                                                             ->  Data.Vector.Storable.maxIndexBy            #-}
{-# COMPILE GHC foldM                 = \ mℓ m bℓ b a AgdaMonad AgdaStorable                                                                                            ->  Data.Vector.Storable.foldM                 #-}
{-# COMPILE GHC ifoldM                = \ mℓ m bℓ b a AgdaMonad AgdaStorable                                                                                            ->  Data.Vector.Storable.ifoldM                #-}
{-# COMPILE GHC foldM'                = \ mℓ m bℓ b a AgdaMonad AgdaStorable                                                                                            ->  Data.Vector.Storable.foldM'                #-}
{-# COMPILE GHC ifoldM'               = \ mℓ m bℓ b a AgdaMonad AgdaStorable                                                                                            ->  Data.Vector.Storable.ifoldM'               #-}
{-# COMPILE GHC fold1M                = \ mℓ m a AgdaMonad AgdaStorable                                                                                                 ->  Data.Vector.Storable.fold1M                #-}
{-# COMPILE GHC fold1M'               = \ mℓ m a AgdaMonad AgdaStorable                                                                                                 ->  Data.Vector.Storable.fold1M'               #-}
{-# COMPILE GHC foldM-                = \ mℓ m bℓ b a AgdaMonad AgdaStorable                                                                                            ->  Data.Vector.Storable.foldM_                #-}
{-# COMPILE GHC ifoldM-               = \ mℓ m bℓ b a AgdaMonad AgdaStorable                                                                                            ->  Data.Vector.Storable.ifoldM_               #-}
{-# COMPILE GHC foldM'-               = \ mℓ m bℓ b a AgdaMonad AgdaStorable                                                                                            ->  Data.Vector.Storable.foldM'_               #-}
{-# COMPILE GHC ifoldM'-              = \ mℓ m bℓ b a AgdaMonad AgdaStorable                                                                                            ->  Data.Vector.Storable.ifoldM'_              #-}
{-# COMPILE GHC fold1M-               = \ mℓ m a AgdaMonad AgdaStorable                                                                                                 ->  Data.Vector.Storable.fold1M_               #-}
{-# COMPILE GHC fold1M'-              = \ mℓ m a AgdaMonad AgdaStorable                                                                                                 ->  Data.Vector.Storable.fold1M'_              #-}
{-# COMPILE GHC prescanl              = \ aℓ a bℓ b AgdaStorable AgdaStorable                                                                                           ->  Data.Vector.Storable.prescanl              #-}
{-# COMPILE GHC prescanl'             = \ aℓ a bℓ b AgdaStorable AgdaStorable                                                                                           ->  Data.Vector.Storable.prescanl'             #-}
{-# COMPILE GHC postscanl             = \ aℓ a bℓ b AgdaStorable AgdaStorable                                                                                           ->  Data.Vector.Storable.postscanl             #-}
{-# COMPILE GHC postscanl'            = \ aℓ a bℓ b AgdaStorable AgdaStorable                                                                                           ->  Data.Vector.Storable.postscanl'            #-}
{-# COMPILE GHC scanl                 = \ aℓ a bℓ b AgdaStorable AgdaStorable                                                                                           ->  Data.Vector.Storable.scanl                 #-}
{-# COMPILE GHC scanl'                = \ aℓ a bℓ b AgdaStorable AgdaStorable                                                                                           ->  Data.Vector.Storable.scanl'                #-}
{-# COMPILE GHC scanl1                = \ aℓ a AgdaStorable                                                                                                             ->  Data.Vector.Storable.scanl1                #-}
{-# COMPILE GHC scanl1'               = \ aℓ a AgdaStorable                                                                                                             ->  Data.Vector.Storable.scanl1'               #-}
{-# COMPILE GHC iscanl                = \ aℓ a bℓ b AgdaStorable AgdaStorable                                                                                           ->  Data.Vector.Storable.iscanl                #-}
{-# COMPILE GHC iscanl'               = \ aℓ a bℓ b AgdaStorable AgdaStorable                                                                                           ->  Data.Vector.Storable.iscanl'               #-}
{-# COMPILE GHC prescanr              = \ aℓ a bℓ b AgdaStorable AgdaStorable                                                                                           ->  Data.Vector.Storable.prescanr              #-}
{-# COMPILE GHC prescanr'             = \ aℓ a bℓ b AgdaStorable AgdaStorable                                                                                           ->  Data.Vector.Storable.prescanr'             #-}
{-# COMPILE GHC postscanr             = \ aℓ a bℓ b AgdaStorable AgdaStorable                                                                                           ->  Data.Vector.Storable.postscanr             #-}
{-# COMPILE GHC postscanr'            = \ aℓ a bℓ b AgdaStorable AgdaStorable                                                                                           ->  Data.Vector.Storable.postscanr'            #-}
{-# COMPILE GHC scanr                 = \ aℓ a bℓ b AgdaStorable AgdaStorable                                                                                           ->  Data.Vector.Storable.scanr                 #-}
{-# COMPILE GHC scanr'                = \ aℓ a bℓ b AgdaStorable AgdaStorable                                                                                           ->  Data.Vector.Storable.scanr'                #-}
{-# COMPILE GHC scanr1                = \ aℓ a AgdaStorable                                                                                                             ->  Data.Vector.Storable.scanr1                #-}
{-# COMPILE GHC scanr1'               = \ aℓ a AgdaStorable                                                                                                             ->  Data.Vector.Storable.scanr1'               #-}
{-# COMPILE GHC iscanr                = \ aℓ a bℓ b AgdaStorable AgdaStorable                                                                                           ->  Data.Vector.Storable.iscanr                #-}
{-# COMPILE GHC iscanr'               = \ aℓ a bℓ b AgdaStorable AgdaStorable                                                                                           ->  Data.Vector.Storable.iscanr'               #-}
{-# COMPILE GHC eqBy                  = \ aℓ a bℓ b AgdaStorable AgdaStorable                                                                                           ->  Data.Vector.Storable.eqBy                  #-}
{-# COMPILE GHC cmpBy                 = \ aℓ a bℓ b AgdaStorable AgdaStorable                                                                                           ->  Data.Vector.Storable.cmpBy                 #-}
{-# COMPILE GHC isSameVector          = \ aℓ a AgdaStorable                                                                                                             ->  Data.Vector.Storable.isSameVector          #-}
{-# COMPILE GHC toList                = \ aℓ a AgdaStorable                                                                                                             ->  Data.Vector.Storable.toList                #-}
{-# COMPILE GHC fromList              = \ aℓ a AgdaStorable                                                                                                             ->  Data.Vector.Storable.fromList              #-}
{-# COMPILE GHC fromListN             = \ aℓ a AgdaStorable                                                                                                             ->  Data.Vector.Storable.fromListN             #-}
{-# COMPILE GHC unsafeCast            = \ aℓ a bℓ b AgdaStorable AgdaStorable                                                                                           ->  Data.Vector.Storable.unsafeCast            #-}
-- {-# COMPILE GHC unsafeCoerceVector    = \ aℓ a bℓ b AgdaStorable AgdaStorable                                                                                           ->  Data.Vector.Storable.unsafeCoerceVector    #-}
{-# COMPILE GHC freeze                = \ aℓ a m AgdaStorable AgdaPrimMonad                                                                                             ->  Data.Vector.Storable.freeze                #-}
{-# COMPILE GHC thaw                  = \ aℓ a m AgdaStorable AgdaPrimMonad                                                                                             ->  Data.Vector.Storable.thaw                  #-}
{-# COMPILE GHC copy                  = \ aℓ a mℓ m AgdaStorable AgdaPrimMonad                                                                                             ->  Data.Vector.Storable.copy                  #-}
{-# COMPILE GHC unsafeFreeze          = \ aℓ a m AgdaStorable AgdaPrimMonad                                                                                             ->  Data.Vector.Storable.unsafeFreeze          #-}
{-# COMPILE GHC unsafeThaw            = \ aℓ a m AgdaStorable AgdaPrimMonad                                                                                             ->  Data.Vector.Storable.unsafeThaw            #-}
{-# COMPILE GHC unsafeCopy            = \ aℓ a mℓ m AgdaStorable AgdaPrimMonad                                                                                             ->  Data.Vector.Storable.unsafeCopy            #-}
{-# COMPILE GHC unsafeFromForeignPtr  = \ aℓ a AgdaStorable                                                                                                             ->  Data.Vector.Storable.unsafeFromForeignPtr  #-}
{-# COMPILE GHC unsafeFromForeignPtr0 = \ aℓ a AgdaStorable                                                                                                             ->  Data.Vector.Storable.unsafeFromForeignPtr0 #-}
{-# COMPILE GHC unsafeToForeignPtr    = \ aℓ a AgdaStorable                                                                                                             ->  Data.Vector.Storable.unsafeToForeignPtr    #-}
{-# COMPILE GHC unsafeToForeignPtr0   = \ aℓ a AgdaStorable                                                                                                             ->  Data.Vector.Storable.unsafeToForeignPtr0   #-}
{-# COMPILE GHC unsafeWith            = \ aℓ a bℓ b AgdaStorable                                                                                                        ->  Data.Vector.Storable.unsafeWith            #-}
