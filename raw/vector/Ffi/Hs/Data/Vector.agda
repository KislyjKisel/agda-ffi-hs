{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Vector where

open import Agda.Builtin.Bool              using (Bool)
open import Agda.Builtin.List              using (List)
open import Agda.Builtin.Maybe             using (Maybe)
open import Agda.Primitive
open import Ffi.Hs.-base.Class
open import Ffi.Hs.-base.Unit              using (⊤; ⊤′)
open import Ffi.Hs.Control.DeepSeq         using (NFData; NFData1)
open import Ffi.Hs.Control.Monad.Primitive using (PrimMonad; PrimState)
open import Ffi.Hs.Control.Monad.ST        using (ST)
open import Ffi.Hs.Data.Either             using (Either)
open import Ffi.Hs.Data.Int                using (Int)
open import Ffi.Hs.Data.Ord                using (Ordering)
open import Ffi.Hs.Data.Tuple              using (Tuple2; Tuple3; Tuple4; Tuple5)
open import Ffi.Hs.GHC.IsList              using (IsList)

import Ffi.Hs.-base.Dictionaries

{-# FOREIGN GHC
import qualified Data.Vector
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ : Level
        A B C D E F : Set aℓ
        M : Set aℓ → Set aℓ

postulate
    Vector : Set aℓ → Set aℓ

    Monad[Vector]        : Monad {aℓ} Vector
    Functor[Vector]      : Functor {aℓ} Vector
    MonadFix[Vector]     : MonadFix {aℓ} Vector
    MonadFail[Vector]    : MonadFail {aℓ} Vector
    Applicative[Vector]  : Applicative {aℓ} Vector
    Foldable[Vector]     : Foldable {aℓ} Vector
    Traversable[Vector]  : Traversable {aℓ} Vector
    Eq1[Vector]          : Eq1 {aℓ} Vector
    Ord1[Vector]         : Ord1 {aℓ} Vector
    Read1[Vector]        : Read1 {aℓ} Vector
    Show1[Vector]        : Show1 {aℓ} Vector
    MonadZip[Vector]     : MonadZip {aℓ} Vector
    Alternative[Vector]  : Alternative {aℓ} Vector
    MonadPlus[Vector]    : MonadPlus {aℓ} Vector
    NFData1[Vector]      : NFData1 {aℓ} Vector
    IsList[Vector[A]]    : IsList (Vector A)
    Eq[Vector[A]]        : Eq (Vector A)
    Data[Vector[A]]      : Data (Vector A)
    Ord[Vector[A]]       : Ord (Vector A)
    Read[Vector[A]]      : Read (Vector A)
    Show[Vector[A]]      : Show (Vector A)
    Semigroup[Vector[A]] : Semigroup (Vector A)
    Monoid[Vector[A]]    : Monoid (Vector A)
    NFData[Vector[A]]    : ⦃ NFData A ⦄ → NFData (Vector A)

postulate
    MVector : Set → Set aℓ → Set aℓ

infixr 5 _++_
postulate
    length            : Vector A → Int
    null              : Vector A → Bool
    _!_               : Vector A → Int → A
    _!?_              : Vector A → Int → Maybe A
    head              : Vector A → A
    last              : Vector A → A
    unsafeIndex       : Vector A → Int → A
    unsafeHead        : Vector A → A
    unsafeLast        : Vector A → A
    indexM            : ⦃ Monad M ⦄ → Vector A → Int → M A
    headM             : ⦃ Monad M ⦄ → Vector A → M A
    lastM             : ⦃ Monad M ⦄ → Vector A → M A
    unsafeIndexM      : ⦃ Monad M ⦄ → Vector A → Int → M A
    unsafeHeadM       : ⦃ Monad M ⦄ → Vector A → M A
    unsafeLastM       : ⦃ Monad M ⦄ → Vector A → M A
    slice             : Int → Int → Vector A → Vector A
    init              : Vector A → Vector A
    tail              : Vector A → Vector A
    take              : Int → Vector A → Vector A
    drop              : Int → Vector A → Vector A
    splitAt           : Int → Vector A → Tuple2 (Vector A) (Vector A)
    uncons            : Vector A → Maybe (Tuple2 A (Vector A))
    unsnoc            : Vector A → Maybe (Tuple2 (Vector A) (A))
    unsafeSlice       : Int → Int → Vector A → Vector A
    unsafeInit        : Vector A → Vector A
    unsafeTail        : Vector A → Vector A
    unsafeTake        : Int → Vector A → Vector A
    unsafeDrop        : Int → Vector A → Vector A
    empty             : Vector A
    singleton         : A → Vector A
    replicate         : Int → A → Vector A
    generate          : Int → (Int → A) → Vector A
    iterateN          : Int → (A → A) → A → Vector A
    replicateM        : ⦃ Monad M ⦄ → Int → M A → M (Vector A)
    generateM         : ⦃ Monad M ⦄ → Int → (Int → M A) → M (Vector A)
    iterateNM         : ⦃ Monad M ⦄ → Int → (A → M A) → A → M (Vector A)
    create            : (∀{S} → ST S (MVector S A)) → Vector A
    createT           : ⦃ Traversable M ⦄ → (∀{S} → ST S (M (MVector S A))) → M (Vector A)
    unfoldr           : (B → Maybe (Tuple2 A B)) → B → Vector A
    unfoldrN          : Int → (B → Maybe (Tuple2 A B)) → B → Vector A
    unfoldrExactN     : Int → (B → Tuple2 A B) → B → Vector A
    unfoldrM          : {A B : Set aℓ} → ⦃ Monad M ⦄ → (B → M (Maybe (Tuple2 A B))) → B → M (Vector A)
    unfoldrNM         : {A B : Set aℓ} → ⦃ Monad M ⦄ → Int → (B → M (Maybe (Tuple2 A B))) → B → M (Vector A)
    unfoldrExactNM    : {A B : Set aℓ} → ⦃ Monad M ⦄ → Int → (B → M (Tuple2 A B)) → B → M (Vector A)
    constructN        : Int → (Vector A → A) → Vector A
    constructrN       : Int → (Vector A → A) → Vector A
    enumFromN         : ⦃ Num A ⦄ → A → Int → Vector A
    enumFromStepN     : ⦃ Num A ⦄ → A → A → Int → Vector A
    enumFromTo        : ⦃ Enum A ⦄ → A → A → Vector A
    enumFromThenTo    : ⦃ Enum A ⦄ → A → A → A → Vector A
    cons              : A → Vector A → Vector A
    snoc              : Vector A → A → Vector A
    _++_              : Vector A → Vector A → Vector A
    concat            : List (Vector A) → Vector A
    force             : Vector A → Vector A
    _//_              : Vector A → List (Tuple2 Int A) → Vector A
    update            : Vector A → Vector (Tuple2 Int A) → Vector A
    update-           : Vector A → Vector Int → Vector A → Vector A
    unsafeUpd         : Vector A → List (Tuple2 Int A) → Vector A
    unsafeUpdate      : Vector A → Vector (Tuple2 Int A) → Vector A
    unsafeUpdate-     : Vector A → Vector Int → Vector A → Vector A
    accum             : (A → B → A) → Vector A → List (Tuple2 Int B) → Vector A
    accumulate        : (A → B → A) → Vector A → Vector (Tuple2 Int B) → Vector A
    accumulate-       : (A → B → A) → Vector A → Vector Int → Vector B → Vector A
    unsafeAccum       : (A → B → A) → Vector A → List (Tuple2 Int B) → Vector A
    unsafeAccumulate  : (A → B → A) → Vector A → Vector (Tuple2 Int B) → Vector A
    unsafeAccumulate- : (A → B → A) → Vector A → Vector Int → Vector B → Vector A
    reverse           : Vector A → Vector A
    backpermute       : Vector A → Vector Int → Vector A
    unsafeBackpermute : Vector A → Vector Int → Vector A
    modify            : (∀{S} → MVector S A → ST S ⊤) → Vector A → Vector A
    indexed           : Vector A → Vector (Tuple2 Int A)
    map               : (A → B) → Vector A → Vector B
    imap              : (Int → A → B) → Vector A → Vector B
    concatMap         : (A → Vector B) → Vector A → Vector B
    mapM              : ⦃ Monad M ⦄ → (A → M B) → Vector A → M (Vector B)
    imapM             : ⦃ Monad M ⦄ → (Int → A → M B) → Vector A → M (Vector B)
    mapM-             : ⦃ Monad M ⦄ → (A → M B) → Vector A → M ⊤′
    imapM-            : ⦃ Monad M ⦄ → (Int → A → M B) → Vector A → M ⊤′
    forM              : ⦃ Monad M ⦄ → Vector A → (A → M B) → M (Vector B)
    forM-             : ⦃ Monad M ⦄ → Vector A → (A → M B) → M ⊤′
    iforM             : ⦃ Monad M ⦄ → Vector A → (Int → A → M B) → M (Vector B)
    iforM-            : ⦃ Monad M ⦄ → Vector A → (Int → A → M B) → M ⊤′
    zipWith           : (A → B → C) → Vector A → Vector B → Vector C
    zipWith3          : (A → B → C → D) → Vector A → Vector B → Vector C → Vector D
    zipWith4          : (A → B → C → D → E) → Vector A → Vector B → Vector C → Vector D → Vector E
    zipWith5          : (A → B → C → D → E → F) → Vector A → Vector B → Vector C → Vector D → Vector E → Vector F
    izipWith          : (Int → A → B → C) → Vector A → Vector B → Vector C
    izipWith3         : (Int → A → B → C → D) → Vector A → Vector B → Vector C → Vector D
    izipWith4         : (Int → A → B → C → D → E) → Vector A → Vector B → Vector C → Vector D → Vector E
    izipWith5         : (Int → A → B → C → D → E → F) → Vector A → Vector B → Vector C → Vector D → Vector E → Vector F
    zip               : Vector A → Vector B → Vector (Tuple2 A B)
    zip3              : Vector A → Vector B → Vector C → Vector (Tuple3 A B C)
    zip4              : Vector A → Vector B → Vector C → Vector D → Vector (Tuple4 A B C D)
    zip5              : Vector A → Vector B → Vector C → Vector D → Vector E → Vector (Tuple5 A B C D E)
    zipWithM          : ⦃ Monad M ⦄ → (A → B → M C) → Vector A → Vector B → M (Vector C)
    izipWithM         : ⦃ Monad M ⦄ → (Int → A → B → M C) → Vector A → Vector B → M (Vector C)
    zipWithM-         : ⦃ Monad M ⦄ → (A → B → M C) → Vector A → Vector B → M ⊤′
    izipWithM-        : ⦃ Monad M ⦄ → (Int → A → B → M C) → Vector A → Vector B → M ⊤′
    unzip             : Vector (Tuple2 A B) → Tuple2 (Vector A) (Vector B)
    unzip3            : Vector (Tuple3 A B C) → Tuple3 (Vector A) (Vector B) (Vector C)
    unzip4            : Vector (Tuple4 A B C D) → Tuple4 (Vector A) (Vector B) (Vector C) (Vector D)
    unzip5            : Vector (Tuple5 A B C D E) → Tuple5 (Vector A) (Vector B) (Vector C) (Vector D) (Vector E)
    filter            : (A → Bool) → Vector A → Vector A
    ifilter           : (Int → A → Bool) → Vector A → Vector A
    filterM           : ⦃ Monad M ⦄ → (A → M Bool) → Vector A → M (Vector A)
    uniq              : ⦃ Eq A ⦄ → Vector A → Vector A
    mapMaybe          : (A → Maybe B) → Vector A → Vector B
    imapMaybe         : (Int → A → Maybe B) → Vector A → Vector B
    mapMaybeM         : ⦃ Monad M ⦄ → (A → M (Maybe B)) → Vector A → M (Vector B)
    imapMaybeM        : ⦃ Monad M ⦄ → (Int → A → M (Maybe B)) → Vector A → M (Vector B)
    catMaybes         : Vector (Maybe A) → Vector A
    takeWhile         : (A → Bool) → Vector A → Vector A
    dropWhile         : (A → Bool) → Vector A → Vector A
    partition         : (A → Bool) → Vector A → Tuple2 (Vector A) (Vector A)
    unstablePartition : (A → Bool) → Vector A → Tuple2 (Vector A) (Vector A)
    partitionWith     : (A → Either B C) → Vector A → Tuple2 (Vector B) (Vector C)
    span              : (A → Bool) → Vector A → Tuple2 (Vector A) (Vector A)
    break             : (A → Bool) → Vector A → Tuple2 (Vector A) (Vector A)
    elem              : ⦃ Eq A ⦄ → A → Vector A → Bool
    notElem           : ⦃ Eq A ⦄ → A → Vector A → Bool
    find              : (A → Bool) → Vector A → Maybe A
    findIndex         : (A → Bool) → Vector A → Maybe Int
    findIndices       : (A → Bool) → Vector A → Vector Int
    elemIndex         : ⦃ Eq A ⦄ → A → Vector A → Maybe Int
    elemIndices       : ⦃ Eq A ⦄ → A → Vector A → Vector Int
    foldl             : (A → B → A) → A → Vector B → A
    foldl1            : (A → A → A) → Vector A → A
    foldl'            : (A → B → A) → A → Vector B → A
    foldl1'           : (A → A → A) → Vector A → A
    foldr             : (A → B → B) → B → Vector A → B
    foldr1            : (A → A → A) → Vector A → A
    foldr'            : (A → B → B) → B → Vector A → B
    foldr1'           : (A → A → A) → Vector A → A
    ifoldl            : (A → Int → B → A) → A → Vector B → A
    ifoldl'           : (A → Int → B → A) → A → Vector B → A
    ifoldr            : (Int → A → B → B) → B → Vector A → B
    ifoldr'           : (Int → A → B → B) → B → Vector A → B
    foldMap           : ⦃ Monoid B ⦄ → (A → B) → Vector A → B
    foldMap'          : ⦃ Monoid B ⦄ → (A → B) → Vector A → B
    all               : (A → Bool) → Vector A → Bool
    any               : (A → Bool) → Vector A → Bool
    and               : Vector Bool → Bool
    or                : Vector Bool → Bool
    sum               : ⦃ Num A ⦄ → Vector A → A
    product           : ⦃ Num A ⦄ → Vector A → A
    maximum           : ⦃ Ord A ⦄ → Vector A → A
    maximumBy         : (A → A → Ordering) → Vector A → A
    minimum           : ⦃ Ord A ⦄ → Vector A → A
    minimumBy         : (A → A → Ordering) → Vector A → A
    minIndex          : ⦃ Ord A ⦄ → Vector A → Int
    minIndexBy        : (A → A → Ordering) → Vector A → Int
    maxIndex          : ⦃ Ord A ⦄ → Vector A → Int
    maxIndexBy        : (A → A → Ordering) → Vector A → Int
    foldM             : ⦃ Monad M ⦄ → (A → B → M A) → A → Vector B → M A
    ifoldM            : ⦃ Monad M ⦄ → (A → Int → B → M A) → A → Vector B → M A
    foldM'            : ⦃ Monad M ⦄ → (A → B → M A) → A → Vector B → M A
    ifoldM'           : ⦃ Monad M ⦄ → (A → Int → B → M A) → A → Vector B → M A
    fold1M            : ⦃ Monad M ⦄ → (A → A → M A) → Vector A → M A
    fold1M'           : ⦃ Monad M ⦄ → (A → A → M A) → Vector A → M A
    foldM-            : ⦃ Monad M ⦄ → (A → B → M A) → A → Vector B → M ⊤′
    ifoldM-           : ⦃ Monad M ⦄ → (A → Int → B → M A) → A → Vector B → M ⊤′
    foldM'-           : ⦃ Monad M ⦄ → (A → B → M A) → A → Vector B → M ⊤′
    ifoldM'-          : ⦃ Monad M ⦄ → (A → Int → B → M A) → A → Vector B → M ⊤′
    fold1M-           : ⦃ Monad M ⦄ → (A → A → M A) → Vector A → M ⊤′
    fold1M'-          : ⦃ Monad M ⦄ → (A → A → M A) → Vector A → M ⊤′
    sequence          : ⦃ Monad M ⦄ → Vector (M A) → M (Vector A)
    sequence-         : ⦃ Monad M ⦄ → Vector (M A) → M ⊤′
    prescanl          : (A → B → A) → A → Vector B → Vector A
    prescanl'         : (A → B → A) → A → Vector B → Vector A
    postscanl         : (A → B → A) → A → Vector B → Vector A
    postscanl'        : (A → B → A) → A → Vector B → Vector A
    scanl             : (A → B → A) → A → Vector B → Vector A
    scanl'            : (A → B → A) → A → Vector B → Vector A
    scanl1            : (A → A → A) → Vector A → Vector A
    scanl1'           : (A → A → A) → Vector A → Vector A
    iscanl            : (Int → A → B → A) → A → Vector B → Vector A
    iscanl'           : (Int → A → B → A) → A → Vector B → Vector A
    prescanr          : (A → B → B) → B → Vector A → Vector B
    prescanr'         : (A → B → B) → B → Vector A → Vector B
    postscanr         : (A → B → B) → B → Vector A → Vector B
    postscanr'        : (A → B → B) → B → Vector A → Vector B
    scanr             : (A → B → B) → B → Vector A → Vector B
    scanr'            : (A → B → B) → B → Vector A → Vector B
    scanr1            : (A → A → A) → Vector A → Vector A
    scanr1'           : (A → A → A) → Vector A → Vector A
    iscanr            : (Int → A → B → B) → B → Vector A → Vector B
    iscanr'           : (Int → A → B → B) → B → Vector A → Vector B
    eqBy              : (A → B → Bool) → Vector A → Vector B → Bool
    cmpBy             : (A → B → Ordering) → Vector A → Vector B → Ordering
    toList            : Vector A → List A
    fromList          : List A → Vector A
    fromListN         : Int → List A → Vector A
    -- todo: (req prim arr) fromArray         : Array A → Vector A
    -- todo: (req prim arr) toArray           : Vector A → Array A
    -- todo: (req generic) convert           : ⦃ Vector V A ⦄ → ⦃ Vector W A ⦄ → V A → W A
    freeze            : ⦃ _ : PrimMonad M ⦄ → MVector (PrimState M) A → M (Vector A)
    thaw              : ⦃ _ : PrimMonad M ⦄ → Vector A → M (MVector (PrimState M) A)
    copy              : ⦃ _ : PrimMonad M ⦄ → MVector (PrimState M) A → Vector A → M ⊤′
    unsafeFreeze      : ⦃ _ : PrimMonad M ⦄ → MVector (PrimState M) A → M (Vector A)
    unsafeThaw        : ⦃ _ : PrimMonad M ⦄ → Vector A → M (MVector (PrimState M) A)
    unsafeCopy        : ⦃ _ : PrimMonad M ⦄ → MVector (PrimState M) A → Vector A → M ⊤′

{-# COMPILE GHC length            = Data.Vector.length            #-}
{-# COMPILE GHC null              = Data.Vector.null              #-}
{-# COMPILE GHC _!_               = (Data.Vector.!)               #-}
{-# COMPILE GHC _!?_              = (Data.Vector.!?)              #-}
{-# COMPILE GHC head              = Data.Vector.head              #-}
{-# COMPILE GHC last              = Data.Vector.last              #-}
{-# COMPILE GHC unsafeIndex       = Data.Vector.unsafeIndex       #-}
{-# COMPILE GHC unsafeHead        = Data.Vector.unsafeHead        #-}
{-# COMPILE GHC unsafeLast        = Data.Vector.unsafeLast        #-}
{-# COMPILE GHC indexM            = Data.Vector.indexM            #-}
{-# COMPILE GHC headM             = Data.Vector.headM             #-}
{-# COMPILE GHC lastM             = Data.Vector.lastM             #-}
{-# COMPILE GHC unsafeIndexM      = Data.Vector.unsafeIndexM      #-}
{-# COMPILE GHC unsafeHeadM       = Data.Vector.unsafeHeadM       #-}
{-# COMPILE GHC unsafeLastM       = Data.Vector.unsafeLastM       #-}
{-# COMPILE GHC slice             = Data.Vector.slice             #-}
{-# COMPILE GHC init              = Data.Vector.init              #-}
{-# COMPILE GHC tail              = Data.Vector.tail              #-}
{-# COMPILE GHC take              = Data.Vector.take              #-}
{-# COMPILE GHC drop              = Data.Vector.drop              #-}
{-# COMPILE GHC splitAt           = Data.Vector.splitAt           #-}
{-# COMPILE GHC uncons            = Data.Vector.uncons            #-}
{-# COMPILE GHC unsnoc            = Data.Vector.unsnoc            #-}
{-# COMPILE GHC unsafeSlice       = Data.Vector.unsafeSlice       #-}
{-# COMPILE GHC unsafeInit        = Data.Vector.unsafeInit        #-}
{-# COMPILE GHC unsafeTail        = Data.Vector.unsafeTail        #-}
{-# COMPILE GHC unsafeTake        = Data.Vector.unsafeTake        #-}
{-# COMPILE GHC unsafeDrop        = Data.Vector.unsafeDrop        #-}
{-# COMPILE GHC empty             = Data.Vector.empty             #-}
{-# COMPILE GHC singleton         = Data.Vector.singleton         #-}
{-# COMPILE GHC replicate         = Data.Vector.replicate         #-}
{-# COMPILE GHC generate          = Data.Vector.generate          #-}
{-# COMPILE GHC iterateN          = Data.Vector.iterateN          #-}
{-# COMPILE GHC replicateM        = Data.Vector.replicateM        #-}
{-# COMPILE GHC generateM         = Data.Vector.generateM         #-}
{-# COMPILE GHC iterateNM         = Data.Vector.iterateNM         #-}
{-# COMPILE GHC create            = Data.Vector.create            #-}
{-# COMPILE GHC createT           = Data.Vector.createT           #-}
{-# COMPILE GHC unfoldr           = Data.Vector.unfoldr           #-}
{-# COMPILE GHC unfoldrN          = Data.Vector.unfoldrN          #-}
{-# COMPILE GHC unfoldrExactN     = Data.Vector.unfoldrExactN     #-}
{-# COMPILE GHC unfoldrM          = Data.Vector.unfoldrM          #-}
{-# COMPILE GHC unfoldrNM         = Data.Vector.unfoldrNM         #-}
{-# COMPILE GHC unfoldrExactNM    = Data.Vector.unfoldrExactNM    #-}
{-# COMPILE GHC constructN        = Data.Vector.constructN        #-}
{-# COMPILE GHC constructrN       = Data.Vector.constructrN       #-}
{-# COMPILE GHC enumFromN         = Data.Vector.enumFromN         #-}
{-# COMPILE GHC enumFromStepN     = Data.Vector.enumFromStepN     #-}
{-# COMPILE GHC enumFromTo        = Data.Vector.enumFromTo        #-}
{-# COMPILE GHC enumFromThenTo    = Data.Vector.enumFromThenTo    #-}
{-# COMPILE GHC cons              = Data.Vector.cons              #-}
{-# COMPILE GHC snoc              = Data.Vector.snoc              #-}
{-# COMPILE GHC _++_              = (Data.Vector.++)              #-}
{-# COMPILE GHC concat            = Data.Vector.concat            #-}
{-# COMPILE GHC force             = Data.Vector.force             #-}
{-# COMPILE GHC _//_              = (Data.Vector.//)              #-}
{-# COMPILE GHC update            = Data.Vector.update            #-}
{-# COMPILE GHC update-           = Data.Vector.update_           #-}
{-# COMPILE GHC unsafeUpd         = Data.Vector.unsafeUpd         #-}
{-# COMPILE GHC unsafeUpdate      = Data.Vector.unsafeUpdate      #-}
{-# COMPILE GHC unsafeUpdate-     = Data.Vector.unsafeUpdate_     #-}
{-# COMPILE GHC accum             = Data.Vector.accum             #-}
{-# COMPILE GHC accumulate        = Data.Vector.accumulate        #-}
{-# COMPILE GHC accumulate-       = Data.Vector.accumulate_       #-}
{-# COMPILE GHC unsafeAccum       = Data.Vector.unsafeAccum       #-}
{-# COMPILE GHC unsafeAccumulate  = Data.Vector.unsafeAccumulate  #-}
{-# COMPILE GHC unsafeAccumulate- = Data.Vector.unsafeAccumulate_ #-}
{-# COMPILE GHC reverse           = Data.Vector.reverse           #-}
{-# COMPILE GHC backpermute       = Data.Vector.backpermute       #-}
{-# COMPILE GHC unsafeBackpermute = Data.Vector.unsafeBackpermute #-}
{-# COMPILE GHC modify            = Data.Vector.modify            #-}
{-# COMPILE GHC indexed           = Data.Vector.indexed           #-}
{-# COMPILE GHC map               = Data.Vector.map               #-}
{-# COMPILE GHC imap              = Data.Vector.imap              #-}
{-# COMPILE GHC concatMap         = Data.Vector.concatMap         #-}
{-# COMPILE GHC mapM              = Data.Vector.mapM              #-}
{-# COMPILE GHC imapM             = Data.Vector.imapM             #-}
{-# COMPILE GHC mapM-             = Data.Vector.mapM_             #-}
{-# COMPILE GHC imapM-            = Data.Vector.imapM_            #-}
{-# COMPILE GHC forM              = Data.Vector.forM              #-}
{-# COMPILE GHC forM-             = Data.Vector.forM_             #-}
{-# COMPILE GHC iforM             = Data.Vector.iforM             #-}
{-# COMPILE GHC iforM-            = Data.Vector.iforM_            #-}
{-# COMPILE GHC zipWith           = Data.Vector.zipWith           #-}
{-# COMPILE GHC zipWith3          = Data.Vector.zipWith3          #-}
{-# COMPILE GHC zipWith4          = Data.Vector.zipWith4          #-}
{-# COMPILE GHC zipWith5          = Data.Vector.zipWith5          #-}
{-# COMPILE GHC izipWith          = Data.Vector.izipWith          #-}
{-# COMPILE GHC izipWith3         = Data.Vector.izipWith3         #-}
{-# COMPILE GHC izipWith4         = Data.Vector.izipWith4         #-}
{-# COMPILE GHC izipWith5         = Data.Vector.izipWith5         #-}
{-# COMPILE GHC zip               = Data.Vector.zip               #-}
{-# COMPILE GHC zip3              = Data.Vector.zip3              #-}
{-# COMPILE GHC zip4              = Data.Vector.zip4              #-}
{-# COMPILE GHC zip5              = Data.Vector.zip5              #-}
{-# COMPILE GHC zipWithM          = Data.Vector.zipWithM          #-}
{-# COMPILE GHC izipWithM         = Data.Vector.izipWithM         #-}
{-# COMPILE GHC zipWithM-         = Data.Vector.zipWithM_         #-}
{-# COMPILE GHC izipWithM-        = Data.Vector.izipWithM_        #-}
{-# COMPILE GHC unzip             = Data.Vector.unzip             #-}
{-# COMPILE GHC unzip3            = Data.Vector.unzip3            #-}
{-# COMPILE GHC unzip4            = Data.Vector.unzip4            #-}
{-# COMPILE GHC unzip5            = Data.Vector.unzip5            #-}
{-# COMPILE GHC filter            = Data.Vector.filter            #-}
{-# COMPILE GHC ifilter           = Data.Vector.ifilter           #-}
{-# COMPILE GHC filterM           = Data.Vector.filterM           #-}
{-# COMPILE GHC uniq              = Data.Vector.uniq              #-}
{-# COMPILE GHC mapMaybe          = Data.Vector.mapMaybe          #-}
{-# COMPILE GHC imapMaybe         = Data.Vector.imapMaybe         #-}
{-# COMPILE GHC mapMaybeM         = Data.Vector.mapMaybeM         #-}
{-# COMPILE GHC imapMaybeM        = Data.Vector.imapMaybeM        #-}
{-# COMPILE GHC catMaybes         = Data.Vector.catMaybes         #-}
{-# COMPILE GHC takeWhile         = Data.Vector.takeWhile         #-}
{-# COMPILE GHC dropWhile         = Data.Vector.dropWhile         #-}
{-# COMPILE GHC partition         = Data.Vector.partition         #-}
{-# COMPILE GHC unstablePartition = Data.Vector.unstablePartition #-}
{-# COMPILE GHC partitionWith     = Data.Vector.partitionWith     #-}
{-# COMPILE GHC span              = Data.Vector.span              #-}
{-# COMPILE GHC break             = Data.Vector.break             #-}
{-# COMPILE GHC elem              = Data.Vector.elem              #-}
{-# COMPILE GHC notElem           = Data.Vector.notElem           #-}
{-# COMPILE GHC find              = Data.Vector.find              #-}
{-# COMPILE GHC findIndex         = Data.Vector.findIndex         #-}
{-# COMPILE GHC findIndices       = Data.Vector.findIndices       #-}
{-# COMPILE GHC elemIndex         = Data.Vector.elemIndex         #-}
{-# COMPILE GHC elemIndices       = Data.Vector.elemIndices       #-}
{-# COMPILE GHC foldl             = Data.Vector.foldl             #-}
{-# COMPILE GHC foldl1            = Data.Vector.foldl1            #-}
{-# COMPILE GHC foldl'            = Data.Vector.foldl'            #-}
{-# COMPILE GHC foldl1'           = Data.Vector.foldl1'           #-}
{-# COMPILE GHC foldr             = Data.Vector.foldr             #-}
{-# COMPILE GHC foldr1            = Data.Vector.foldr1            #-}
{-# COMPILE GHC foldr'            = Data.Vector.foldr'            #-}
{-# COMPILE GHC foldr1'           = Data.Vector.foldr1'           #-}
{-# COMPILE GHC ifoldl            = Data.Vector.ifoldl            #-}
{-# COMPILE GHC ifoldl'           = Data.Vector.ifoldl'           #-}
{-# COMPILE GHC ifoldr            = Data.Vector.ifoldr            #-}
{-# COMPILE GHC ifoldr'           = Data.Vector.ifoldr'           #-}
{-# COMPILE GHC foldMap           = Data.Vector.foldMap           #-}
{-# COMPILE GHC foldMap'          = Data.Vector.foldMap'          #-}
{-# COMPILE GHC all               = Data.Vector.all               #-}
{-# COMPILE GHC any               = Data.Vector.any               #-}
{-# COMPILE GHC and               = Data.Vector.and               #-}
{-# COMPILE GHC or                = Data.Vector.or                #-}
{-# COMPILE GHC sum               = Data.Vector.sum               #-}
{-# COMPILE GHC product           = Data.Vector.product           #-}
{-# COMPILE GHC maximum           = Data.Vector.maximum           #-}
{-# COMPILE GHC maximumBy         = Data.Vector.maximumBy         #-}
{-# COMPILE GHC minimum           = Data.Vector.minimum           #-}
{-# COMPILE GHC minimumBy         = Data.Vector.minimumBy         #-}
{-# COMPILE GHC minIndex          = Data.Vector.minIndex          #-}
{-# COMPILE GHC minIndexBy        = Data.Vector.minIndexBy        #-}
{-# COMPILE GHC maxIndex          = Data.Vector.maxIndex          #-}
{-# COMPILE GHC maxIndexBy        = Data.Vector.maxIndexBy        #-}
{-# COMPILE GHC foldM             = Data.Vector.foldM             #-}
{-# COMPILE GHC ifoldM            = Data.Vector.ifoldM            #-}
{-# COMPILE GHC foldM'            = Data.Vector.foldM'            #-}
{-# COMPILE GHC ifoldM'           = Data.Vector.ifoldM'           #-}
{-# COMPILE GHC fold1M            = Data.Vector.fold1M            #-}
{-# COMPILE GHC fold1M'           = Data.Vector.fold1M'           #-}
{-# COMPILE GHC foldM-            = Data.Vector.foldM_            #-}
{-# COMPILE GHC ifoldM-           = Data.Vector.ifoldM_           #-}
{-# COMPILE GHC foldM'-           = Data.Vector.foldM'_           #-}
{-# COMPILE GHC ifoldM'-          = Data.Vector.ifoldM'_          #-}
{-# COMPILE GHC fold1M-           = Data.Vector.fold1M_           #-}
{-# COMPILE GHC fold1M'-          = Data.Vector.fold1M'_          #-}
{-# COMPILE GHC sequence          = Data.Vector.sequence          #-}
{-# COMPILE GHC sequence-         = Data.Vector.sequence_         #-}
{-# COMPILE GHC prescanl          = Data.Vector.prescanl          #-}
{-# COMPILE GHC prescanl'         = Data.Vector.prescanl'         #-}
{-# COMPILE GHC postscanl         = Data.Vector.postscanl         #-}
{-# COMPILE GHC postscanl'        = Data.Vector.postscanl'        #-}
{-# COMPILE GHC scanl             = Data.Vector.scanl             #-}
{-# COMPILE GHC scanl'            = Data.Vector.scanl'            #-}
{-# COMPILE GHC scanl1            = Data.Vector.scanl1            #-}
{-# COMPILE GHC scanl1'           = Data.Vector.scanl1'           #-}
{-# COMPILE GHC iscanl            = Data.Vector.iscanl            #-}
{-# COMPILE GHC iscanl'           = Data.Vector.iscanl'           #-}
{-# COMPILE GHC prescanr          = Data.Vector.prescanr          #-}
{-# COMPILE GHC prescanr'         = Data.Vector.prescanr'         #-}
{-# COMPILE GHC postscanr         = Data.Vector.postscanr         #-}
{-# COMPILE GHC postscanr'        = Data.Vector.postscanr'        #-}
{-# COMPILE GHC scanr             = Data.Vector.scanr             #-}
{-# COMPILE GHC scanr'            = Data.Vector.scanr'            #-}
{-# COMPILE GHC scanr1            = Data.Vector.scanr1            #-}
{-# COMPILE GHC scanr1'           = Data.Vector.scanr1'           #-}
{-# COMPILE GHC iscanr            = Data.Vector.iscanr            #-}
{-# COMPILE GHC iscanr'           = Data.Vector.iscanr'           #-}
{-# COMPILE GHC eqBy              = Data.Vector.eqBy              #-}
{-# COMPILE GHC cmpBy             = Data.Vector.cmpBy             #-}
{-# COMPILE GHC toList            = Data.Vector.toList            #-}
{-# COMPILE GHC fromList          = Data.Vector.fromList          #-}
{-# COMPILE GHC fromListN         = Data.Vector.fromListN         #-}
{-# COMPILE GHC freeze            = Data.Vector.freeze            #-}
{-# COMPILE GHC thaw              = Data.Vector.thaw              #-}
{-# COMPILE GHC copy              = Data.Vector.copy              #-}
{-# COMPILE GHC unsafeFreeze      = Data.Vector.unsafeFreeze      #-}
{-# COMPILE GHC unsafeThaw        = Data.Vector.unsafeThaw        #-}
{-# COMPILE GHC unsafeCopy        = Data.Vector.unsafeCopy        #-}
