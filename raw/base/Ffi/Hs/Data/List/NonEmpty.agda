{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.List.NonEmpty where

open import Agda.Builtin.Bool   using (Bool)
open import Agda.Builtin.List   using (List)
open import Agda.Builtin.Maybe  using (Maybe)
open import Agda.Primitive
open import Ffi.Hs.-base.Class
open import Ffi.Hs.Data.Functor using (Functor)
open import Ffi.Hs.Data.Int     using (Int)
open import Ffi.Hs.Data.Ord     using (Ordering)
open import Ffi.Hs.Data.Tuple   using (Tuple2)
open import Ffi.Hs.GHC.IsList   using (IsList)
open import Ffi.Hs.GHC.Stack    using (HasCallStack)

{-# FOREIGN GHC
import qualified Data.List.NonEmpty
import MAlonzo.Code.Ffi.Hs.GHC.IsList (AgdaIsList(AgdaIsList))
import MAlonzo.Code.Ffi.Hs.GHC.Stack (AgdaHasCallStack(AgdaHasCallStack))
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ bℓ cℓ fℓ : Level
        A : Set aℓ
        B : Set bℓ
        C : Set cℓ
        F : Set fℓ → Set fℓ

data NonEmpty (A : Set aℓ) : Set aℓ where
    _:|_ : A → List A → NonEmpty A

{-# FOREIGN GHC type AgdaNonEmpty aℓ = Data.List.NonEmpty.NonEmpty        #-}
{-# COMPILE GHC NonEmpty = data(1) AgdaNonEmpty ((Data.List.NonEmpty.:|)) #-}

postulate
    MonadFix[NonEmpty]    : MonadFix {aℓ} NonEmpty
    MonadZip[NonEmpty]    : MonadZip {aℓ} NonEmpty
    Foldable[NonEmpty]    : Foldable {aℓ} NonEmpty
    Traversable[NonEmpty] : Traversable {aℓ} NonEmpty
    Applicative[NonEmpty] : Applicative {aℓ} NonEmpty
    Functor[NonEmpty]     : Functor {aℓ} NonEmpty
    Monad[NonEmpty]       : Monad {aℓ} NonEmpty
    Data[NonEmpty[A]]      : ⦃ Data A ⦄ → Data (NonEmpty A)
    Semigroup[NonEmpty[A]] : Semigroup (NonEmpty A)
    IsList[NonEmpty[A]]    : IsList (NonEmpty A)
    Read[NonEmpty[A]]      : ⦃ Read A ⦄ → Read (NonEmpty A)
    Show[NonEmpty[A]]      : ⦃ Show A ⦄ → Show (NonEmpty A)
    Eq[NonEmpty[A]]        : ⦃ Eq A ⦄ → Eq (NonEmpty A)
    Ord[NonEmpty[A]]       : ⦃ Ord A ⦄ → Ord (NonEmpty A)

{-# COMPILE GHC MonadFix[NonEmpty]    = \ aℓ -> AgdaMonadFix    #-}
{-# COMPILE GHC MonadZip[NonEmpty]    = \ aℓ -> AgdaMonadZip    #-}
{-# COMPILE GHC Foldable[NonEmpty]    = \ aℓ -> AgdaFoldable    #-}
{-# COMPILE GHC Traversable[NonEmpty] = \ aℓ -> AgdaTraversable #-}
{-# COMPILE GHC Applicative[NonEmpty] = \ aℓ -> AgdaApplicative #-}
{-# COMPILE GHC Functor[NonEmpty]     = \ aℓ -> AgdaFunctor     #-}
{-# COMPILE GHC Monad[NonEmpty]       = \ aℓ -> AgdaMonad       #-}
{-# COMPILE GHC Data[NonEmpty[A]]      = \ aℓ a AgdaData -> AgdaData      #-}
{-# COMPILE GHC Semigroup[NonEmpty[A]] = \ aℓ a          -> AgdaSemigroup #-}
{-# COMPILE GHC IsList[NonEmpty[A]]    = \ aℓ a          -> AgdaIsList    #-}
{-# COMPILE GHC Read[NonEmpty[A]]      = \ aℓ a AgdaRead -> AgdaRead      #-}
{-# COMPILE GHC Show[NonEmpty[A]]      = \ aℓ a AgdaShow -> AgdaShow      #-}
{-# COMPILE GHC Eq[NonEmpty[A]]        = \ aℓ a AgdaEq   -> AgdaEq        #-}
{-# COMPILE GHC Ord[NonEmpty[A]]       = \ aℓ a AgdaOrd  -> AgdaOrd       #-}

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
    groupWith1    : ⦃ Eq B ⦄ → (A → B) → NonEmpty A → NonEmpty (NonEmpty A)
    groupAllWith1 : ⦃ Ord B ⦄ → (A → B) → NonEmpty A → NonEmpty (NonEmpty A)
    nub           : ⦃ Eq A ⦄ → NonEmpty A → NonEmpty A
    nubBy         : (A → A → Bool) → NonEmpty A → NonEmpty A
    _!!_          : ⦃ HasCallStack ⦄ → NonEmpty A → Int → A
    zip           : NonEmpty A → NonEmpty B → NonEmpty (Tuple2 A B)
    zipWith       : (A → B → C) → NonEmpty A → NonEmpty B → NonEmpty C
    unzip         : ⦃ Functor F ⦄ → F (Tuple2 A B) → Tuple2 (F A) (F B)
    fromList      : List A → NonEmpty A
    toList        : NonEmpty A → List A
    nonEmpty      : List A → Maybe (NonEmpty A)
    xor           : NonEmpty Bool → Bool

{-# COMPILE GHC map           = \ aℓ a bℓ b                       -> Data.List.NonEmpty.map           #-}
{-# COMPILE GHC intersperse   = \ aℓ a                            -> Data.List.NonEmpty.intersperse   #-}
{-# COMPILE GHC scanl         = \ fℓ f a bℓ b AgdaFoldable        -> Data.List.NonEmpty.scanl         #-}
{-# COMPILE GHC scanr         = \ fℓ f a bℓ b AgdaFoldable        -> Data.List.NonEmpty.scanr         #-}
{-# COMPILE GHC scanl1        = \ aℓ a                            -> Data.List.NonEmpty.scanl1        #-}
{-# COMPILE GHC scanr1        = \ aℓ a                            -> Data.List.NonEmpty.scanr1        #-}
{-# COMPILE GHC transpose     = \ aℓ a                            -> Data.List.NonEmpty.transpose     #-}
{-# COMPILE GHC sortBy        = \ aℓ a                            -> Data.List.NonEmpty.sortBy        #-}
{-# COMPILE GHC sortWith      = \ aℓ a bℓ b AgdaOrd               -> Data.List.NonEmpty.sortWith      #-}
{-# COMPILE GHC length        = \ aℓ a                            -> Data.List.NonEmpty.length        #-}
{-# COMPILE GHC head          = \ aℓ a                            -> Data.List.NonEmpty.head          #-}
{-# COMPILE GHC tail          = \ aℓ a                            -> Data.List.NonEmpty.tail          #-}
{-# COMPILE GHC last          = \ aℓ a                            -> Data.List.NonEmpty.last          #-}
{-# COMPILE GHC init          = \ aℓ a                            -> Data.List.NonEmpty.init          #-}
{-# COMPILE GHC singleton     = \ aℓ a                            -> Data.List.NonEmpty.singleton     #-}
{-# COMPILE GHC _<|_          = \ aℓ a                            -> (Data.List.NonEmpty.<|)          #-}
{-# COMPILE GHC cons          = \ aℓ a                            -> Data.List.NonEmpty.cons          #-}
{-# COMPILE GHC uncons        = \ aℓ a                            -> Data.List.NonEmpty.uncons        #-}
{-# COMPILE GHC unfoldr       = \ aℓ a bℓ b                       -> Data.List.NonEmpty.unfoldr       #-}
{-# COMPILE GHC sort          = \ aℓ a AgdaOrd                    -> Data.List.NonEmpty.sort          #-}
{-# COMPILE GHC reverse       = \ aℓ a                            -> Data.List.NonEmpty.reverse       #-}
{-# COMPILE GHC inits         = \ fℓ f a AgdaFoldable             -> Data.List.NonEmpty.inits         #-}
{-# COMPILE GHC tails         = \ fℓ f a AgdaFoldable             -> Data.List.NonEmpty.tails         #-}
{-# COMPILE GHC append        = \ aℓ a                            -> Data.List.NonEmpty.append        #-}
{-# COMPILE GHC appendList    = \ aℓ a                            -> Data.List.NonEmpty.appendList    #-}
{-# COMPILE GHC prependList   = \ aℓ a                            -> Data.List.NonEmpty.prependList   #-}
{-# COMPILE GHC iterate       = \ aℓ a                            -> Data.List.NonEmpty.iterate       #-}
{-# COMPILE GHC repeat        = \ aℓ a                            -> Data.List.NonEmpty.repeat        #-}
{-# COMPILE GHC cycle         = \ aℓ a                            -> Data.List.NonEmpty.cycle         #-}
{-# COMPILE GHC insert        = \ fℓ f a AgdaFoldable AgdaOrd     -> Data.List.NonEmpty.insert        #-}
{-# COMPILE GHC some1         = \ fℓ f a AgdaAlternative          -> Data.List.NonEmpty.some1         #-}
{-# COMPILE GHC take          = \ aℓ a                            -> Data.List.NonEmpty.take          #-}
{-# COMPILE GHC drop          = \ aℓ a                            -> Data.List.NonEmpty.drop          #-}
{-# COMPILE GHC splitAt       = \ aℓ a                            -> Data.List.NonEmpty.splitAt       #-}
{-# COMPILE GHC takeWhile     = \ aℓ a                            -> Data.List.NonEmpty.takeWhile     #-}
{-# COMPILE GHC dropWhile     = \ aℓ a                            -> Data.List.NonEmpty.dropWhile     #-}
{-# COMPILE GHC span          = \ aℓ a                            -> Data.List.NonEmpty.span          #-}
{-# COMPILE GHC break         = \ aℓ a                            -> Data.List.NonEmpty.break         #-}
{-# COMPILE GHC filter        = \ aℓ a                            -> Data.List.NonEmpty.filter        #-}
{-# COMPILE GHC partition     = \ aℓ a                            -> Data.List.NonEmpty.partition     #-}
{-# COMPILE GHC group         = \ fℓ f a AgdaFoldable AgdaEq      -> Data.List.NonEmpty.group         #-}
{-# COMPILE GHC groupBy       = \ fℓ f a AgdaFoldable             -> Data.List.NonEmpty.groupBy       #-}
{-# COMPILE GHC groupWith     = \ fℓ f a bℓ b AgdaFoldable AgdaEq -> Data.List.NonEmpty.groupWith     #-}
{-# COMPILE GHC groupAllWith  = \ aℓ a bℓ b AgdaOrd               -> Data.List.NonEmpty.groupAllWith  #-}
{-# COMPILE GHC group1        = \ aℓ a AgdaEq                     -> Data.List.NonEmpty.group1        #-}
{-# COMPILE GHC groupBy1      = \ aℓ a                            -> Data.List.NonEmpty.groupBy1      #-}
{-# COMPILE GHC groupWith1    = \ aℓ a bℓ b AgdaEq                -> Data.List.NonEmpty.groupWith1    #-}
{-# COMPILE GHC groupAllWith1 = \ aℓ a bℓ b AgdaOrd               -> Data.List.NonEmpty.groupAllWith1 #-}
{-# COMPILE GHC nub           = \ aℓ a AgdaEq                     -> Data.List.NonEmpty.nub           #-}
{-# COMPILE GHC nubBy         = \ aℓ a                            -> Data.List.NonEmpty.nubBy         #-}
{-# COMPILE GHC _!!_          = \ aℓ a AgdaHasCallStack           -> (Data.List.NonEmpty.!!)          #-}
{-# COMPILE GHC zip           = \ aℓ a bℓ b                       -> Data.List.NonEmpty.zip           #-}
{-# COMPILE GHC zipWith       = \ aℓ a bℓ b cℓ c                  -> Data.List.NonEmpty.zipWith       #-}
{-# COMPILE GHC unzip         = \ fℓ f a b AgdaFunctor            -> Data.List.NonEmpty.unzip         #-}
{-# COMPILE GHC fromList      = \ aℓ a                            -> Data.List.NonEmpty.fromList      #-}
{-# COMPILE GHC toList        = \ aℓ a                            -> Data.List.NonEmpty.toList        #-}
{-# COMPILE GHC nonEmpty      = \ aℓ a                            -> Data.List.NonEmpty.nonEmpty      #-}
{-# COMPILE GHC xor           =                                      Data.List.NonEmpty.xor           #-}
