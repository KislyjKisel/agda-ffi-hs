{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Vector where

open import Agda.Builtin.Bool              using (Bool)
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
    Eq[Vector[A]]        : ⦃ Eq A ⦄ → Eq (Vector A)
    Data[Vector[A]]      : ⦃ Data A ⦄ → Data (Vector A)
    Ord[Vector[A]]       : ⦃ Ord A ⦄ → Ord (Vector A)
    Read[Vector[A]]      : ⦃ Read A ⦄ → Read (Vector A)
    Show[Vector[A]]      : ⦃ Show A ⦄ → Show (Vector A)
    Semigroup[Vector[A]] : Semigroup (Vector A)
    Monoid[Vector[A]]    : Monoid (Vector A)
    NFData[Vector[A]]    : ⦃ NFData A ⦄ → NFData (Vector A)

{-# FOREIGN GHC type AgdaVector aℓ = Data.Vector.Vector #-}
{-# COMPILE GHC Vector = type(1) AgdaVector #-}

{-# COMPILE GHC Monad[Vector]        = \ aℓ              -> AgdaMonad       #-}
{-# COMPILE GHC Functor[Vector]      = \ aℓ              -> AgdaFunctor     #-}
{-# COMPILE GHC MonadFix[Vector]     = \ aℓ              -> AgdaMonadFix    #-}
{-# COMPILE GHC MonadFail[Vector]    = \ aℓ              -> AgdaMonadFail   #-}
{-# COMPILE GHC Applicative[Vector]  = \ aℓ              -> AgdaApplicative #-}
{-# COMPILE GHC Foldable[Vector]     = \ aℓ              -> AgdaFoldable    #-}
{-# COMPILE GHC Traversable[Vector]  = \ aℓ              -> AgdaTraversable #-}
{-# COMPILE GHC Eq1[Vector]          = \ aℓ              -> AgdaEq1         #-}
{-# COMPILE GHC Ord1[Vector]         = \ aℓ              -> AgdaOrd1        #-}
{-# COMPILE GHC Read1[Vector]        = \ aℓ              -> AgdaRead1       #-}
{-# COMPILE GHC Show1[Vector]        = \ aℓ              -> AgdaShow1       #-}
{-# COMPILE GHC MonadZip[Vector]     = \ aℓ              -> AgdaMonadZip    #-}
{-# COMPILE GHC Alternative[Vector]  = \ aℓ              -> AgdaAlternative #-}
{-# COMPILE GHC MonadPlus[Vector]    = \ aℓ              -> AgdaMonadPlus   #-}
{-# COMPILE GHC NFData1[Vector]      = \ aℓ              -> AgdaNFData1     #-}
{-# COMPILE GHC IsList[Vector[A]]    = \ aℓ a            -> AgdaIsList      #-}
{-# COMPILE GHC Eq[Vector[A]]        = \ aℓ a AgdaEq     -> AgdaEq          #-}
{-# COMPILE GHC Data[Vector[A]]      = \ aℓ a AgdaData   -> AgdaData        #-}
{-# COMPILE GHC Ord[Vector[A]]       = \ aℓ a AgdaOrd    -> AgdaOrd         #-}
{-# COMPILE GHC Read[Vector[A]]      = \ aℓ a AgdaRead   -> AgdaRead        #-}
{-# COMPILE GHC Show[Vector[A]]      = \ aℓ a AgdaShow   -> AgdaShow        #-}
{-# COMPILE GHC Semigroup[Vector[A]] = \ aℓ a            -> AgdaSemigroup   #-}
{-# COMPILE GHC Monoid[Vector[A]]    = \ aℓ a            -> AgdaMonoid      #-}
{-# COMPILE GHC NFData[Vector[A]]    = \ aℓ a AgdaNFData -> AgdaNFData      #-}

postulate
    MVector : Set → Set aℓ → Set aℓ

{-# FOREIGN GHC type AgdaMVector aℓ = Data.Vector.MVector #-}
{-# COMPILE GHC MVector = type(1) AgdaMVector #-}

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
    filterM           : ⦃ Monad M ⦄ → (A → M (Liftℓ _ Bool)) → Vector A → M (Vector A)
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

{-# COMPILE GHC length            = \ aℓ a                          ->  Data.Vector.length            #-}
{-# COMPILE GHC null              = \ aℓ a                          ->  Data.Vector.null              #-}
{-# COMPILE GHC _!_               = \ aℓ a                          -> (Data.Vector.!)                #-}
{-# COMPILE GHC _!?_              = \ aℓ a                          -> (Data.Vector.!?)               #-}
{-# COMPILE GHC head              = \ aℓ a                          ->  Data.Vector.head              #-}
{-# COMPILE GHC last              = \ aℓ a                          ->  Data.Vector.last              #-}
{-# COMPILE GHC unsafeIndex       = \ aℓ a                          ->  Data.Vector.unsafeIndex       #-}
{-# COMPILE GHC unsafeHead        = \ aℓ a                          ->  Data.Vector.unsafeHead        #-}
{-# COMPILE GHC unsafeLast        = \ aℓ a                          ->  Data.Vector.unsafeLast        #-}
{-# COMPILE GHC indexM            = \ mℓ m a AgdaMonad              ->  Data.Vector.indexM            #-}
{-# COMPILE GHC headM             = \ mℓ m a AgdaMonad              ->  Data.Vector.headM             #-}
{-# COMPILE GHC lastM             = \ mℓ m a AgdaMonad              ->  Data.Vector.lastM             #-}
{-# COMPILE GHC unsafeIndexM      = \ mℓ m a AgdaMonad              ->  Data.Vector.unsafeIndexM      #-}
{-# COMPILE GHC unsafeHeadM       = \ mℓ m a AgdaMonad              ->  Data.Vector.unsafeHeadM       #-}
{-# COMPILE GHC unsafeLastM       = \ mℓ m a AgdaMonad              ->  Data.Vector.unsafeLastM       #-}
{-# COMPILE GHC slice             = \ aℓ a                          ->  Data.Vector.slice             #-}
{-# COMPILE GHC init              = \ aℓ a                          ->  Data.Vector.init              #-}
{-# COMPILE GHC tail              = \ aℓ a                          ->  Data.Vector.tail              #-}
{-# COMPILE GHC take              = \ aℓ a                          ->  Data.Vector.take              #-}
{-# COMPILE GHC drop              = \ aℓ a                          ->  Data.Vector.drop              #-}
{-# COMPILE GHC splitAt           = \ aℓ a                          ->  Data.Vector.splitAt           #-}
{-# COMPILE GHC uncons            = \ aℓ a                          ->  Data.Vector.uncons            #-}
{-# COMPILE GHC unsnoc            = \ aℓ a                          ->  Data.Vector.unsnoc            #-}
{-# COMPILE GHC unsafeSlice       = \ aℓ a                          ->  Data.Vector.unsafeSlice       #-}
{-# COMPILE GHC unsafeInit        = \ aℓ a                          ->  Data.Vector.unsafeInit        #-}
{-# COMPILE GHC unsafeTail        = \ aℓ a                          ->  Data.Vector.unsafeTail        #-}
{-# COMPILE GHC unsafeTake        = \ aℓ a                          ->  Data.Vector.unsafeTake        #-}
{-# COMPILE GHC unsafeDrop        = \ aℓ a                          ->  Data.Vector.unsafeDrop        #-}
{-# COMPILE GHC empty             = \ aℓ a                          ->  Data.Vector.empty             #-}
{-# COMPILE GHC singleton         = \ aℓ a                          ->  Data.Vector.singleton         #-}
{-# COMPILE GHC replicate         = \ aℓ a                          ->  Data.Vector.replicate         #-}
{-# COMPILE GHC generate          = \ aℓ a                          ->  Data.Vector.generate          #-}
{-# COMPILE GHC iterateN          = \ aℓ a                          ->  Data.Vector.iterateN          #-}
{-# COMPILE GHC replicateM        = \ mℓ m a AgdaMonad              ->  Data.Vector.replicateM        #-}
{-# COMPILE GHC generateM         = \ mℓ m a AgdaMonad              ->  Data.Vector.generateM         #-}
{-# COMPILE GHC iterateNM         = \ mℓ m a AgdaMonad              ->  Data.Vector.iterateNM         #-}
{-# COMPILE GHC create            = \ aℓ a f                        ->  Data.Vector.create (f ())     #-}
{-# COMPILE GHC createT           = \ mℓ m a AgdaTraversable f      ->  Data.Vector.createT (f ())    #-}
{-# COMPILE GHC unfoldr           = \ aℓ a bℓ b                     ->  Data.Vector.unfoldr           #-}
{-# COMPILE GHC unfoldrN          = \ aℓ a bℓ b                     ->  Data.Vector.unfoldrN          #-}
{-# COMPILE GHC unfoldrExactN     = \ aℓ a bℓ b                     ->  Data.Vector.unfoldrExactN     #-}
{-# COMPILE GHC unfoldrM          = \ aℓ m a b AgdaMonad            ->  Data.Vector.unfoldrM          #-}
{-# COMPILE GHC unfoldrNM         = \ aℓ m a b AgdaMonad            ->  Data.Vector.unfoldrNM         #-}
{-# COMPILE GHC unfoldrExactNM    = \ aℓ m a b AgdaMonad            ->  Data.Vector.unfoldrExactNM    #-}
{-# COMPILE GHC constructN        = \ aℓ a                          ->  Data.Vector.constructN        #-}
{-# COMPILE GHC constructrN       = \ aℓ a                          ->  Data.Vector.constructrN       #-}
{-# COMPILE GHC enumFromN         = \ aℓ a AgdaNum                  ->  Data.Vector.enumFromN         #-}
{-# COMPILE GHC enumFromStepN     = \ aℓ a AgdaNum                  ->  Data.Vector.enumFromStepN     #-}
{-# COMPILE GHC enumFromTo        = \ aℓ a AgdaEnum                 ->  Data.Vector.enumFromTo        #-}
{-# COMPILE GHC enumFromThenTo    = \ aℓ a AgdaEnum                 ->  Data.Vector.enumFromThenTo    #-}
{-# COMPILE GHC cons              = \ aℓ a                          ->  Data.Vector.cons              #-}
{-# COMPILE GHC snoc              = \ aℓ a                          ->  Data.Vector.snoc              #-}
{-# COMPILE GHC _++_              = \ aℓ a                          -> (Data.Vector.++)               #-}
{-# COMPILE GHC concat            = \ aℓ a                          ->  Data.Vector.concat            #-}
{-# COMPILE GHC force             = \ aℓ a                          ->  Data.Vector.force             #-}
{-# COMPILE GHC _//_              = \ aℓ a                          -> (Data.Vector.//)               #-}
{-# COMPILE GHC update            = \ aℓ a                          ->  Data.Vector.update            #-}
{-# COMPILE GHC update-           = \ aℓ a                          ->  Data.Vector.update_           #-}
{-# COMPILE GHC unsafeUpd         = \ aℓ a                          ->  Data.Vector.unsafeUpd         #-}
{-# COMPILE GHC unsafeUpdate      = \ aℓ a                          ->  Data.Vector.unsafeUpdate      #-}
{-# COMPILE GHC unsafeUpdate-     = \ aℓ a                          ->  Data.Vector.unsafeUpdate_     #-}
{-# COMPILE GHC accum             = \ aℓ a bℓ b                     ->  Data.Vector.accum             #-}
{-# COMPILE GHC accumulate        = \ aℓ a bℓ b                     ->  Data.Vector.accumulate        #-}
{-# COMPILE GHC accumulate-       = \ aℓ a bℓ b                     ->  Data.Vector.accumulate_       #-}
{-# COMPILE GHC unsafeAccum       = \ aℓ a bℓ b                     ->  Data.Vector.unsafeAccum       #-}
{-# COMPILE GHC unsafeAccumulate  = \ aℓ a bℓ b                     ->  Data.Vector.unsafeAccumulate  #-}
{-# COMPILE GHC unsafeAccumulate- = \ aℓ a bℓ b                     ->  Data.Vector.unsafeAccumulate_ #-}
{-# COMPILE GHC reverse           = \ aℓ a                          ->  Data.Vector.reverse           #-}
{-# COMPILE GHC backpermute       = \ aℓ a                          ->  Data.Vector.backpermute       #-}
{-# COMPILE GHC unsafeBackpermute = \ aℓ a                          ->  Data.Vector.unsafeBackpermute #-}
{-# COMPILE GHC modify            = \ aℓ a f                        ->  Data.Vector.modify (f ())     #-}
{-# COMPILE GHC indexed           = \ aℓ a                          ->  Data.Vector.indexed           #-}
{-# COMPILE GHC map               = \ aℓ a bℓ b                     ->  Data.Vector.map               #-}
{-# COMPILE GHC imap              = \ aℓ a bℓ b                     ->  Data.Vector.imap              #-}
{-# COMPILE GHC concatMap         = \ aℓ a bℓ b                     ->  Data.Vector.concatMap         #-}
{-# COMPILE GHC mapM              = \ mℓ m aℓ a b AgdaMonad         ->  Data.Vector.mapM              #-}
{-# COMPILE GHC imapM             = \ mℓ m aℓ a b AgdaMonad         ->  Data.Vector.imapM             #-}
{-# COMPILE GHC mapM-             = \ mℓ m aℓ a b AgdaMonad         ->  Data.Vector.mapM_             #-}
{-# COMPILE GHC imapM-            = \ mℓ m aℓ a b AgdaMonad         ->  Data.Vector.imapM_            #-}
{-# COMPILE GHC forM              = \ mℓ m aℓ a b AgdaMonad         ->  Data.Vector.forM              #-}
{-# COMPILE GHC forM-             = \ mℓ m aℓ a b AgdaMonad         ->  Data.Vector.forM_             #-}
{-# COMPILE GHC iforM             = \ mℓ m aℓ a b AgdaMonad         ->  Data.Vector.iforM             #-}
{-# COMPILE GHC iforM-            = \ mℓ m aℓ a b AgdaMonad         ->  Data.Vector.iforM_            #-}
{-# COMPILE GHC zipWith           = \ aℓ a bℓ b cℓ c                ->  Data.Vector.zipWith           #-}
{-# COMPILE GHC zipWith3          = \ aℓ a bℓ b cℓ c dℓ d           ->  Data.Vector.zipWith3          #-}
{-# COMPILE GHC zipWith4          = \ aℓ a bℓ b cℓ c dℓ d eℓ e      ->  Data.Vector.zipWith4          #-}
{-# COMPILE GHC zipWith5          = \ aℓ a bℓ b cℓ c dℓ d eℓ e fℓ f ->  Data.Vector.zipWith5          #-}
{-# COMPILE GHC izipWith          = \ aℓ a bℓ b cℓ c                ->  Data.Vector.izipWith          #-}
{-# COMPILE GHC izipWith3         = \ aℓ a bℓ b cℓ c dℓ d           ->  Data.Vector.izipWith3         #-}
{-# COMPILE GHC izipWith4         = \ aℓ a bℓ b cℓ c dℓ d eℓ e      ->  Data.Vector.izipWith4         #-}
{-# COMPILE GHC izipWith5         = \ aℓ a bℓ b cℓ c dℓ d eℓ e fℓ f ->  Data.Vector.izipWith5         #-}
{-# COMPILE GHC zip               = \ aℓ a bℓ b cℓ c                ->  Data.Vector.zip               #-}
{-# COMPILE GHC zip3              = \ aℓ a bℓ b cℓ c dℓ d           ->  Data.Vector.zip3              #-}
{-# COMPILE GHC zip4              = \ aℓ a bℓ b cℓ c dℓ d eℓ e      ->  Data.Vector.zip4              #-}
{-# COMPILE GHC zip5              = \ aℓ a bℓ b cℓ c dℓ d eℓ e fℓ f ->  Data.Vector.zip5              #-}
{-# COMPILE GHC zipWithM          = \ mℓ m aℓ a bℓ b c AgdaMonad    ->  Data.Vector.zipWithM          #-}
{-# COMPILE GHC izipWithM         = \ mℓ m aℓ a bℓ b c AgdaMonad    ->  Data.Vector.izipWithM         #-}
{-# COMPILE GHC zipWithM-         = \ mℓ m aℓ a bℓ b c AgdaMonad    ->  Data.Vector.zipWithM_         #-}
{-# COMPILE GHC izipWithM-        = \ mℓ m aℓ a bℓ b c AgdaMonad    ->  Data.Vector.izipWithM_        #-}
{-# COMPILE GHC unzip             = \ aℓ a bℓ b cℓ c                ->  Data.Vector.unzip             #-}
{-# COMPILE GHC unzip3            = \ aℓ a bℓ b cℓ c dℓ d           ->  Data.Vector.unzip3            #-}
{-# COMPILE GHC unzip4            = \ aℓ a bℓ b cℓ c dℓ d eℓ e      ->  Data.Vector.unzip4            #-}
{-# COMPILE GHC unzip5            = \ aℓ a bℓ b cℓ c dℓ d eℓ e fℓ f ->  Data.Vector.unzip5            #-}
{-# COMPILE GHC filter            = \ aℓ a                          ->  Data.Vector.filter            #-}
{-# COMPILE GHC ifilter           = \ aℓ a                          ->  Data.Vector.ifilter           #-}
{-# COMPILE GHC filterM           = \ mℓ m a AgdaMonad              ->  Data.Vector.filterM           #-}
{-# COMPILE GHC uniq              = \ aℓ a AgdaEq                   ->  Data.Vector.uniq              #-}
{-# COMPILE GHC mapMaybe          = \ aℓ a bℓ b                     ->  Data.Vector.mapMaybe          #-}
{-# COMPILE GHC imapMaybe         = \ aℓ a bℓ b                     ->  Data.Vector.imapMaybe         #-}
{-# COMPILE GHC mapMaybeM         = \ mℓ m aℓ a b AgdaMonad         ->  Data.Vector.mapMaybeM         #-}
{-# COMPILE GHC imapMaybeM        = \ mℓ m aℓ a b AgdaMonad         ->  Data.Vector.imapMaybeM        #-}
{-# COMPILE GHC catMaybes         = \ aℓ a                          ->  Data.Vector.catMaybes         #-}
{-# COMPILE GHC takeWhile         = \ aℓ a                          ->  Data.Vector.takeWhile         #-}
{-# COMPILE GHC dropWhile         = \ aℓ a                          ->  Data.Vector.dropWhile         #-}
{-# COMPILE GHC partition         = \ aℓ a                          ->  Data.Vector.partition         #-}
{-# COMPILE GHC unstablePartition = \ aℓ a                          ->  Data.Vector.unstablePartition #-}
{-# COMPILE GHC partitionWith     = \ aℓ a bℓ b cℓ c                ->  Data.Vector.partitionWith     #-}
{-# COMPILE GHC span              = \ aℓ a                          ->  Data.Vector.span              #-}
{-# COMPILE GHC break             = \ aℓ a                          ->  Data.Vector.break             #-}
{-# COMPILE GHC elem              = \ aℓ a AgdaEq                   ->  Data.Vector.elem              #-}
{-# COMPILE GHC notElem           = \ aℓ a AgdaEq                   ->  Data.Vector.notElem           #-}
{-# COMPILE GHC find              = \ aℓ a                          ->  Data.Vector.find              #-}
{-# COMPILE GHC findIndex         = \ aℓ a                          ->  Data.Vector.findIndex         #-}
{-# COMPILE GHC findIndices       = \ aℓ a                          ->  Data.Vector.findIndices       #-}
{-# COMPILE GHC elemIndex         = \ aℓ a AgdaEq                   ->  Data.Vector.elemIndex         #-}
{-# COMPILE GHC elemIndices       = \ aℓ a AgdaEq                   ->  Data.Vector.elemIndices       #-}
{-# COMPILE GHC foldl             = \ aℓ a bℓ b                     ->  Data.Vector.foldl             #-}
{-# COMPILE GHC foldl1            = \ aℓ a                          ->  Data.Vector.foldl1            #-}
{-# COMPILE GHC foldl'            = \ aℓ a bℓ b                     ->  Data.Vector.foldl'            #-}
{-# COMPILE GHC foldl1'           = \ aℓ a                          ->  Data.Vector.foldl1'           #-}
{-# COMPILE GHC foldr             = \ aℓ a bℓ b                     ->  Data.Vector.foldr             #-}
{-# COMPILE GHC foldr1            = \ aℓ a                          ->  Data.Vector.foldr1            #-}
{-# COMPILE GHC foldr'            = \ aℓ a bℓ b                     ->  Data.Vector.foldr'            #-}
{-# COMPILE GHC foldr1'           = \ aℓ a                          ->  Data.Vector.foldr1'           #-}
{-# COMPILE GHC ifoldl            = \ aℓ a bℓ b                     ->  Data.Vector.ifoldl            #-}
{-# COMPILE GHC ifoldl'           = \ aℓ a bℓ b                     ->  Data.Vector.ifoldl'           #-}
{-# COMPILE GHC ifoldr            = \ aℓ a bℓ b                     ->  Data.Vector.ifoldr            #-}
{-# COMPILE GHC ifoldr'           = \ aℓ a bℓ b                     ->  Data.Vector.ifoldr'           #-}
{-# COMPILE GHC foldMap           = \ aℓ a bℓ b AgdaMonoid          ->  Data.Vector.foldMap           #-}
{-# COMPILE GHC foldMap'          = \ aℓ a bℓ b AgdaMonoid          ->  Data.Vector.foldMap'          #-}
{-# COMPILE GHC all               = \ aℓ a                          ->  Data.Vector.all               #-}
{-# COMPILE GHC any               = \ aℓ a                          ->  Data.Vector.any               #-}
{-# COMPILE GHC and               =                                     Data.Vector.and               #-}
{-# COMPILE GHC or                =                                     Data.Vector.or                #-}
{-# COMPILE GHC sum               = \ aℓ a AgdaNum                  ->  Data.Vector.sum               #-}
{-# COMPILE GHC product           = \ aℓ a AgdaNum                  ->  Data.Vector.product           #-}
{-# COMPILE GHC maximum           = \ aℓ a AgdaOrd                  ->  Data.Vector.maximum           #-}
{-# COMPILE GHC maximumBy         = \ aℓ a                          ->  Data.Vector.maximumBy         #-}
{-# COMPILE GHC minimum           = \ aℓ a AgdaOrd                  ->  Data.Vector.minimum           #-}
{-# COMPILE GHC minimumBy         = \ aℓ a                          ->  Data.Vector.minimumBy         #-}
{-# COMPILE GHC minIndex          = \ aℓ a AgdaOrd                  ->  Data.Vector.minIndex          #-}
{-# COMPILE GHC minIndexBy        = \ aℓ a                          ->  Data.Vector.minIndexBy        #-}
{-# COMPILE GHC maxIndex          = \ aℓ a AgdaOrd                  ->  Data.Vector.maxIndex          #-}
{-# COMPILE GHC maxIndexBy        = \ aℓ a                          ->  Data.Vector.maxIndexBy        #-}
{-# COMPILE GHC foldM             = \ mℓ m a bℓ b AgdaMonad         ->  Data.Vector.foldM             #-}
{-# COMPILE GHC ifoldM            = \ mℓ m a bℓ b AgdaMonad         ->  Data.Vector.ifoldM            #-}
{-# COMPILE GHC foldM'            = \ mℓ m a bℓ b AgdaMonad         ->  Data.Vector.foldM'            #-}
{-# COMPILE GHC ifoldM'           = \ mℓ m a bℓ b AgdaMonad         ->  Data.Vector.ifoldM'           #-}
{-# COMPILE GHC fold1M            = \ mℓ m a AgdaMonad              ->  Data.Vector.fold1M            #-}
{-# COMPILE GHC fold1M'           = \ mℓ m a AgdaMonad              ->  Data.Vector.fold1M'           #-}
{-# COMPILE GHC foldM-            = \ mℓ m a bℓ b AgdaMonad         ->  Data.Vector.foldM_            #-}
{-# COMPILE GHC ifoldM-           = \ mℓ m a bℓ b AgdaMonad         ->  Data.Vector.ifoldM_           #-}
{-# COMPILE GHC foldM'-           = \ mℓ m a bℓ b AgdaMonad         ->  Data.Vector.foldM'_           #-}
{-# COMPILE GHC ifoldM'-          = \ mℓ m a bℓ b AgdaMonad         ->  Data.Vector.ifoldM'_          #-}
{-# COMPILE GHC fold1M-           = \ mℓ m a AgdaMonad              ->  Data.Vector.fold1M_           #-}
{-# COMPILE GHC fold1M'-          = \ mℓ m a AgdaMonad              ->  Data.Vector.fold1M'_          #-}
{-# COMPILE GHC sequence          = \ mℓ m a AgdaMonad              ->  Data.Vector.sequence          #-}
{-# COMPILE GHC sequence-         = \ mℓ m a AgdaMonad              ->  Data.Vector.sequence_         #-}
{-# COMPILE GHC prescanl          = \ aℓ a bℓ b                     ->  Data.Vector.prescanl          #-}
{-# COMPILE GHC prescanl'         = \ aℓ a bℓ b                     ->  Data.Vector.prescanl'         #-}
{-# COMPILE GHC postscanl         = \ aℓ a bℓ b                     ->  Data.Vector.postscanl         #-}
{-# COMPILE GHC postscanl'        = \ aℓ a bℓ b                     ->  Data.Vector.postscanl'        #-}
{-# COMPILE GHC scanl             = \ aℓ a bℓ b                     ->  Data.Vector.scanl             #-}
{-# COMPILE GHC scanl'            = \ aℓ a bℓ b                     ->  Data.Vector.scanl'            #-}
{-# COMPILE GHC scanl1            = \ aℓ a                          ->  Data.Vector.scanl1            #-}
{-# COMPILE GHC scanl1'           = \ aℓ a                          ->  Data.Vector.scanl1'           #-}
{-# COMPILE GHC iscanl            = \ aℓ a bℓ b                     ->  Data.Vector.iscanl            #-}
{-# COMPILE GHC iscanl'           = \ aℓ a bℓ b                     ->  Data.Vector.iscanl'           #-}
{-# COMPILE GHC prescanr          = \ aℓ a bℓ b                     ->  Data.Vector.prescanr          #-}
{-# COMPILE GHC prescanr'         = \ aℓ a bℓ b                     ->  Data.Vector.prescanr'         #-}
{-# COMPILE GHC postscanr         = \ aℓ a bℓ b                     ->  Data.Vector.postscanr         #-}
{-# COMPILE GHC postscanr'        = \ aℓ a bℓ b                     ->  Data.Vector.postscanr'        #-}
{-# COMPILE GHC scanr             = \ aℓ a bℓ b                     ->  Data.Vector.scanr             #-}
{-# COMPILE GHC scanr'            = \ aℓ a bℓ b                     ->  Data.Vector.scanr'            #-}
{-# COMPILE GHC scanr1            = \ aℓ a                          ->  Data.Vector.scanr1            #-}
{-# COMPILE GHC scanr1'           = \ aℓ a                          ->  Data.Vector.scanr1'           #-}
{-# COMPILE GHC iscanr            = \ aℓ a bℓ b                     ->  Data.Vector.iscanr            #-}
{-# COMPILE GHC iscanr'           = \ aℓ a bℓ b                     ->  Data.Vector.iscanr'           #-}
{-# COMPILE GHC eqBy              = \ aℓ a bℓ b                     ->  Data.Vector.eqBy              #-}
{-# COMPILE GHC cmpBy             = \ aℓ a bℓ b                     ->  Data.Vector.cmpBy             #-}
{-# COMPILE GHC toList            = \ aℓ a                          ->  Data.Vector.toList            #-}
{-# COMPILE GHC fromList          = \ aℓ a                          ->  Data.Vector.fromList          #-}
{-# COMPILE GHC fromListN         = \ aℓ a                          ->  Data.Vector.fromListN         #-}
{-# COMPILE GHC freeze            = \ mℓ m a AgdaPrimMonad          ->  Data.Vector.freeze            #-}
{-# COMPILE GHC thaw              = \ mℓ m a AgdaPrimMonad          ->  Data.Vector.thaw              #-}
{-# COMPILE GHC copy              = \ mℓ m aℓ a AgdaPrimMonad       ->  Data.Vector.copy              #-}
{-# COMPILE GHC unsafeFreeze      = \ mℓ m a AgdaPrimMonad          ->  Data.Vector.unsafeFreeze      #-}
{-# COMPILE GHC unsafeThaw        = \ mℓ m a AgdaPrimMonad          ->  Data.Vector.unsafeThaw        #-}
{-# COMPILE GHC unsafeCopy        = \ mℓ m aℓ a AgdaPrimMonad       ->  Data.Vector.unsafeCopy        #-}
