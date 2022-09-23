{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.List where

open import Agda.Builtin.Bool  using (Bool)
open import Agda.Builtin.Maybe using (Maybe)
open import Agda.Primitive
open import Ffi.Hs.-base.Class
open import Ffi.Hs.-base.Kind  using (IsKind)
open import Ffi.Hs.Data.Int    using (Int)
open import Ffi.Hs.Data.Ord    using (Ordering)
open import Ffi.Hs.Data.Tuple  using (Tuple2; Tuple3; Tuple4; Tuple5)
open import Ffi.Hs.GHC.Stack   using (HasCallStack)

open import Agda.Builtin.List public
    using (List; []; _∷_)

open import Ffi.Hs.-base.Kind.List public
    using (`List; `[]; _`∷_; lift`List)

{-# FOREIGN GHC {-# LANGUAGE DataKinds #-} #-}
{-# FOREIGN GHC
import qualified Data.List
import MAlonzo.Code.Ffi.Hs.GHC.Stack (AgdaHasCallStack(AgdaHasCallStack))
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

infixl 9 _!!_
infixr 5 _++_
infix  5 _\\_

postulate
    _++_      : List A → List A → List A
    head      : ⦃ HasCallStack ⦄ → List A → A
    last      : ⦃ HasCallStack ⦄ → List A → A
    tail      : ⦃ HasCallStack ⦄ → List A → List A
    init      : ⦃ HasCallStack ⦄ → List A → List A
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
    cycle     : ⦃ HasCallStack ⦄ → List A → List A
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
    _!!_        : ⦃ HasCallStack ⦄ → List A → Int → A
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

{-# COMPILE GHC _++_      = \ aℓ a                  -> (Data.List.++)      #-}
{-# COMPILE GHC head      = \ aℓ a AgdaHasCallStack -> Data.List.head      #-}
{-# COMPILE GHC last      = \ aℓ a AgdaHasCallStack -> Data.List.last      #-}
{-# COMPILE GHC tail      = \ aℓ a AgdaHasCallStack -> Data.List.tail      #-}
{-# COMPILE GHC init      = \ aℓ a AgdaHasCallStack -> Data.List.init      #-}
{-# COMPILE GHC uncons    = \ aℓ a                  -> Data.List.uncons    #-}
{-# COMPILE GHC singleton = \ aℓ a                  -> Data.List.singleton #-}
{-# COMPILE GHC null      = \ aℓ a                  -> Data.List.null      #-}
{-# COMPILE GHC length    = \ aℓ a                  -> Data.List.length    #-}

{-# COMPILE GHC map          = \ aℓ a bℓ b -> Data.List.map          #-}
{-# COMPILE GHC reverse      = \ aℓ a      -> Data.List.reverse      #-}
{-# COMPILE GHC intersperse  = \ aℓ a      -> Data.List.intersperse  #-}
{-# COMPILE GHC intercalate  = \ aℓ a      -> Data.List.intercalate  #-}
{-# COMPILE GHC transpose    = \ aℓ a      -> Data.List.transpose    #-}
{-# COMPILE GHC subsequences = \ aℓ a      -> Data.List.subsequences #-}
{-# COMPILE GHC permutations = \ aℓ a      -> Data.List.permutations #-}

{-# COMPILE GHC scanl  = \ aℓ a bℓ b -> Data.List.scanl  #-}
{-# COMPILE GHC scanl' = \ aℓ a bℓ b -> Data.List.scanl' #-}
{-# COMPILE GHC scanl1 = \ aℓ a      -> Data.List.scanl1 #-}
{-# COMPILE GHC scanr  = \ aℓ a bℓ b -> Data.List.scanr  #-}
{-# COMPILE GHC scanr1 = \ aℓ a      -> Data.List.scanr1 #-}

{-# COMPILE GHC iterate   = \ aℓ a                  -> Data.List.iterate   #-}
{-# COMPILE GHC iterate'  = \ aℓ a                  -> Data.List.iterate'  #-}
{-# COMPILE GHC repeat    = \ aℓ a                  -> Data.List.repeat    #-}
{-# COMPILE GHC replicate = \ aℓ a                  -> Data.List.replicate #-}
{-# COMPILE GHC cycle     = \ aℓ a AgdaHasCallStack -> Data.List.cycle     #-}
{-# COMPILE GHC unfoldr   = \ aℓ a bℓ b             -> Data.List.unfoldr   #-}

{-# COMPILE GHC take         = \ aℓ a        -> Data.List.take         #-}
{-# COMPILE GHC drop         = \ aℓ a        -> Data.List.drop         #-}
{-# COMPILE GHC splitAt      = \ aℓ a        -> Data.List.splitAt      #-}
{-# COMPILE GHC takeWhile    = \ aℓ a        -> Data.List.takeWhile    #-}
{-# COMPILE GHC dropWhile    = \ aℓ a        -> Data.List.dropWhile    #-}
{-# COMPILE GHC dropWhileEnd = \ aℓ a        -> Data.List.dropWhileEnd #-}
{-# COMPILE GHC span         = \ aℓ a        -> Data.List.span         #-}
{-# COMPILE GHC break        = \ aℓ a        -> Data.List.break        #-}
{-# COMPILE GHC stripPrefix  = \ aℓ a AgdaEq -> Data.List.stripPrefix  #-}
{-# COMPILE GHC group        = \ aℓ a AgdaEq -> Data.List.group        #-}
{-# COMPILE GHC inits        = \ aℓ a        -> Data.List.inits        #-}
{-# COMPILE GHC tails        = \ aℓ a        -> Data.List.tails        #-}

{-# COMPILE GHC isPrefixOf      = \ aℓ a AgdaEq -> Data.List.isPrefixOf #-}
{-# COMPILE GHC isSuffixOf      = \ aℓ a AgdaEq -> Data.List.isSuffixOf #-}
{-# COMPILE GHC isInfixOf       = \ aℓ a AgdaEq -> Data.List.isInfixOf #-}
{-# COMPILE GHC isSubsequenceOf = \ aℓ a AgdaEq -> Data.List.isSubsequenceOf #-}

{-# COMPILE GHC lookup      = \ aℓ a bℓ b AgdaEq      -> Data.List.lookup      #-}
{-# COMPILE GHC filter      = \ aℓ a                  -> Data.List.filter      #-}
{-# COMPILE GHC partition   = \ aℓ a                  -> Data.List.partition   #-}
{-# COMPILE GHC _!!_        = \ aℓ a AgdaHasCallStack -> (Data.List.!!)        #-}
{-# COMPILE GHC elemIndex   = \ aℓ a AgdaEq           -> Data.List.elemIndex   #-}
{-# COMPILE GHC elemIndices = \ aℓ a AgdaEq           -> Data.List.elemIndices #-}
{-# COMPILE GHC findIndex   = \ aℓ a                  -> Data.List.findIndex   #-}
{-# COMPILE GHC findIndices = \ aℓ a                  -> Data.List.findIndices #-}

{-# COMPILE GHC zip      = \ aℓ a bℓ b                -> Data.List.zip      #-}
{-# COMPILE GHC zip3     = \ aℓ a bℓ b cℓ c           -> Data.List.zip3     #-}
{-# COMPILE GHC zip4     = \ aℓ a bℓ b cℓ c dℓ d      -> Data.List.zip4     #-}
{-# COMPILE GHC zip5     = \ aℓ a bℓ b cℓ c dℓ d eℓ e -> Data.List.zip5     #-}
{-# COMPILE GHC zipWith  = \ aℓ a bℓ b cℓ c           -> Data.List.zipWith  #-}
{-# COMPILE GHC zipWith3 = \ aℓ a bℓ b cℓ c dℓ d      -> Data.List.zipWith3 #-}
{-# COMPILE GHC zipWith4 = \ aℓ a bℓ b cℓ c dℓ d eℓ e -> Data.List.zipWith4 #-}
{-# COMPILE GHC unzip    = \ aℓ a bℓ b                -> Data.List.unzip    #-}
{-# COMPILE GHC unzip3   = \ aℓ a bℓ b cℓ c           -> Data.List.unzip3   #-}
{-# COMPILE GHC unzip4   = \ aℓ a bℓ b cℓ c dℓ d      -> Data.List.unzip4   #-}
{-# COMPILE GHC unzip5   = \ aℓ a bℓ b cℓ c dℓ d eℓ e -> Data.List.unzip5   #-}

{-# COMPILE GHC nub       = \ aℓ a AgdaEq -> Data.List.nub       #-}
{-# COMPILE GHC delete    = \ aℓ a AgdaEq -> Data.List.delete    #-}
{-# COMPILE GHC _\\_      = \ aℓ a AgdaEq -> (Data.List.\\)      #-}
{-# COMPILE GHC union     = \ aℓ a AgdaEq -> Data.List.union     #-}
{-# COMPILE GHC intersect = \ aℓ a AgdaEq -> Data.List.intersect #-}

{-# COMPILE GHC sort   = \ aℓ a      AgdaOrd -> Data.List.sort   #-}
{-# COMPILE GHC sortOn = \ aℓ a bℓ b AgdaOrd -> Data.List.sortOn #-}
{-# COMPILE GHC insert = \ aℓ a      AgdaOrd -> Data.List.insert #-}

{-# COMPILE GHC nubBy          = \ aℓ a -> Data.List.nubBy          #-}
{-# COMPILE GHC deleteBy       = \ aℓ a -> Data.List.deleteBy       #-}
{-# COMPILE GHC deleteFirstsBy = \ aℓ a -> Data.List.deleteFirstsBy #-}
{-# COMPILE GHC unionBy        = \ aℓ a -> Data.List.unionBy        #-}
{-# COMPILE GHC intersectBy    = \ aℓ a -> Data.List.intersectBy    #-}
{-# COMPILE GHC groupBy        = \ aℓ a -> Data.List.groupBy        #-}

{-# COMPILE GHC sortBy   = \ aℓ a -> Data.List.sortBy   #-}
{-# COMPILE GHC insertBy = \ aℓ a -> Data.List.insertBy #-}

{-# COMPILE GHC genericLength    = \ aℓ a bℓ b AgdaNum      -> Data.List.genericLength    #-}
{-# COMPILE GHC genericTake      = \ aℓ a bℓ b AgdaIntegral -> Data.List.genericTake      #-}
{-# COMPILE GHC genericDrop      = \ aℓ a bℓ b AgdaIntegral -> Data.List.genericDrop      #-}
{-# COMPILE GHC genericSplitAt   = \ aℓ a bℓ b AgdaIntegral -> Data.List.genericSplitAt   #-}
{-# COMPILE GHC genericIndex     = \ aℓ a bℓ b AgdaIntegral -> Data.List.genericIndex     #-}
{-# COMPILE GHC genericReplicate = \ aℓ a bℓ b AgdaIntegral -> Data.List.genericReplicate #-}

postulate
    Functor[List]     : Functor {aℓ} List
    Applicative[List] : Applicative {aℓ} List
    Alternative[List] : Alternative {aℓ} List
    Monad[List]       : Monad {aℓ} List
    MonadPlus[List]   : MonadPlus {aℓ} List
    Foldable[List]    : Foldable {aℓ} List
    Traversable[List] : Traversable {aℓ} List
    Semigroup[List]   : Semigroup (List A)
    Monoid[List]      : Monoid (List A)
    Data[List[A]]     : ⦃ Data A ⦄ → Data (List A)
    Eq[List[A]]       : ⦃ Eq A ⦄ → Eq (List A)
    Ord[List[A]]      : ⦃ Ord A ⦄ → Ord (List A)
    Show[List[A]]     : ⦃ Show A ⦄ → Show (List A)
    Read[List[A]]     : ⦃ Read A ⦄ → Read (List A)
    Eq1[List]   : Eq1 {aℓ} List
    Ord1[List]  : Ord1 {aℓ} List
    Read1[List] : Read1 {aℓ} List
    Show1[List] : Show1 {aℓ} List

{-# COMPILE GHC Functor[List]     = \ aℓ            -> AgdaFunctor     #-}
{-# COMPILE GHC Applicative[List] = \ aℓ            -> AgdaApplicative #-}
{-# COMPILE GHC Alternative[List] = \ aℓ            -> AgdaAlternative #-}
{-# COMPILE GHC Monad[List]       = \ aℓ            -> AgdaMonad       #-}
{-# COMPILE GHC MonadPlus[List]   = \ aℓ            -> AgdaMonadPlus   #-}
{-# COMPILE GHC Foldable[List]    = \ aℓ            -> AgdaFoldable    #-}
{-# COMPILE GHC Traversable[List] = \ aℓ            -> AgdaTraversable #-}
{-# COMPILE GHC Semigroup[List]   = \ aℓ a          -> AgdaSemigroup   #-}
{-# COMPILE GHC Monoid[List]      = \ aℓ a          -> AgdaMonoid      #-}
{-# COMPILE GHC Data[List[A]]     = \ aℓ a AgdaData -> AgdaData        #-}
{-# COMPILE GHC Eq[List[A]]       = \ aℓ a AgdaEq   -> AgdaEq          #-}
{-# COMPILE GHC Ord[List[A]]      = \ aℓ a AgdaOrd  -> AgdaOrd         #-}
{-# COMPILE GHC Show[List[A]]     = \ aℓ a AgdaShow -> AgdaShow        #-}
{-# COMPILE GHC Read[List[A]]     = \ aℓ a AgdaRead -> AgdaRead        #-}
{-# COMPILE GHC Eq1[List]         = \ aℓ            -> AgdaEq1         #-}
{-# COMPILE GHC Ord1[List]        = \ aℓ            -> AgdaOrd1        #-}
{-# COMPILE GHC Read1[List]       = \ aℓ            -> AgdaRead1       #-}
{-# COMPILE GHC Show1[List]       = \ aℓ            -> AgdaShow1       #-}
