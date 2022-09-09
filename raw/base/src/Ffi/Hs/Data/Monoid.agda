{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Monoid where

open import Agda.Builtin.Bool  using (Bool)
open import Agda.Builtin.List  using (List)
open import Agda.Builtin.Maybe using (Maybe)
open import Agda.Primitive
open import Ffi.Hs.-base.Class

open Ffi.Hs.-base.Class public
    using (Monoid)

{-# FOREIGN GHC
import qualified Data.Monoid
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Class
    ( AgdaMonoid, AgdaSemigroup, AgdaEq, AgdaOrd, AgdaRead, AgdaShow
    , AgdaFoldable, AgdaTraversable, AgdaMonadFail, AgdaMonadFix
    , AgdaMonadZip, AgdaMonad, AgdaFunctor, AgdaApplicative, AgdaData
    , AgdaAlternative, AgdaTypeable, AgdaEnum, AgdaBounded
    )
#-}

private
    variable
        aℓ fℓ : Level
        A : Set aℓ
        F : Set fℓ → Set fℓ

postulate
    mempty  : ⦃ Monoid A ⦄ → A
    mconcat : ⦃ Monoid A ⦄ → List A → A
    
    Dual    : Set aℓ → Set aℓ
    mkDual  : A → Dual A
    getDual : Dual A → A

    Endo    : Set aℓ → Set aℓ
    mkEndo  : (A → A) → Endo A
    appEndo : Endo A → A → A

    All    : Set
    mkAll  : Bool → All
    getAll : All → Bool

    Any    : Set
    mkAny  : Bool → Any
    getAny : Any → Bool

    Sum    : Set aℓ → Set aℓ
    mkSum  : A → Sum A
    getSum : Sum A → A

    Product    : Set aℓ → Set aℓ
    mkProduct  : A → Product A
    getProduct : Product A → A

    First    : Set aℓ → Set aℓ
    mkFirst  : Maybe A → First A
    getFirst : First A → Maybe A

    Last    : Set aℓ → Set aℓ
    mkLast  : Maybe A → Last A
    getLast : Last A → Maybe A

    Alt    : (Set fℓ → Set fℓ) → Set fℓ → Set fℓ
    mkAlt  : F A → Alt F A
    getAlt : Alt F A → F A

    Ap    : (Set fℓ → Set fℓ) → Set fℓ → Set fℓ
    mkAp  : F A → Ap F A
    getAp : Ap F A → F A

{-# COMPILE GHC mempty  = \ aℓ a AgdaMonoid -> Data.Monoid.mempty  #-}
{-# COMPILE GHC mconcat = \ aℓ a AgdaMonoid -> Data.Monoid.mconcat #-}

{-# FOREIGN GHC type AgdaDual aℓ = Data.Monoid.Dual #-}
{-# COMPILE GHC Dual = type(1) AgdaDual #-}
{-# COMPILE GHC mkDual  = \ aℓ a -> Data.Monoid.mkDual  #-}
{-# COMPILE GHC getDual = \ aℓ a -> Data.Monoid.getDual #-}

{-# FOREIGN GHC type AgdaEndo aℓ = Data.Monoid.Endo #-}
{-# COMPILE GHC Endo = type(1) AgdaEndo #-}
{-# COMPILE GHC mkEndo  = \ aℓ a -> Data.Monoid.mkEndo  #-}
{-# COMPILE GHC appEndo = \ aℓ a -> Data.Monoid.appEndo #-}

{-# COMPILE GHC All = type Data.Monoid.All #-}
{-# COMPILE GHC mkAll  = \ aℓ a -> Data.Monoid.mkAll  #-}
{-# COMPILE GHC getAll = \ aℓ a -> Data.Monoid.getAll #-}

{-# COMPILE GHC Any = type Data.Monoid.Any #-}
{-# COMPILE GHC mkAny  = \ aℓ a -> Data.Monoid.mkAny  #-}
{-# COMPILE GHC getAny = \ aℓ a -> Data.Monoid.getAny #-}

{-# FOREIGN GHC type AgdaSum aℓ = Data.Monoid.Sum #-}
{-# COMPILE GHC Sum = type(1) AgdaSum #-}
{-# COMPILE GHC mkSum  = \ aℓ a -> Data.Monoid.mkSum  #-}
{-# COMPILE GHC getSum = \ aℓ a -> Data.Monoid.getSum #-}

{-# FOREIGN GHC type AgdaProduct aℓ = Data.Monoid.Product #-}
{-# COMPILE GHC Product = type(1) AgdaProduct #-}
{-# COMPILE GHC mkProduct  = \ aℓ a -> Data.Monoid.mkProduct  #-}
{-# COMPILE GHC getProduct = \ aℓ a -> Data.Monoid.getProduct #-}

{-# FOREIGN GHC type AgdaFirst aℓ = Data.Monoid.First #-}
{-# COMPILE GHC First = type(1) AgdaFirst #-}
{-# COMPILE GHC mkFirst  = \ aℓ a -> Data.Monoid.mkFirst  #-}
{-# COMPILE GHC getFirst = \ aℓ a -> Data.Monoid.getFirst #-}

{-# FOREIGN GHC type AgdaLast aℓ = Data.Monoid.Last #-}
{-# COMPILE GHC Last = type(1) AgdaLast #-}
{-# COMPILE GHC mkLast  = \ aℓ a -> Data.Monoid.mkLast  #-}
{-# COMPILE GHC getLast = \ aℓ a -> Data.Monoid.getLast #-}

{-# FOREIGN GHC type AgdaAlt fℓ = Data.Monoid.Alt #-}
{-# COMPILE GHC Alt = type(1) AgdaAlt #-}
{-# COMPILE GHC mkAlt  = \ fℓ f a -> Data.Monoid.mkAlt  #-}
{-# COMPILE GHC getAlt = \ fℓ f a -> Data.Monoid.getAlt #-}

{-# FOREIGN GHC type AgdaAp fℓ = Data.Monoid.Ap #-}
{-# COMPILE GHC Ap = type(1) AgdaAp #-}
{-# COMPILE GHC mkAp  = \ fℓ f a -> Data.Monoid.mkAp  #-}
{-# COMPILE GHC getAp = \ fℓ f a -> Data.Monoid.getAp #-}

postulate
    MonadFix[Dual]    : MonadFix {aℓ} Dual
    MonadZip[Dual]    : MonadZip {aℓ} Dual
    Foldable[Dual]    : Foldable {aℓ} Dual
    Traversable[Dual] : Traversable {aℓ} Dual
    Applicative[Dual] : Applicative {aℓ} Dual
    Functor[Dual]     : Functor {aℓ} Dual
    Monad[Dual]       : Monad {aℓ} Dual
    Data[Dual[A]]      : ⦃ Data A ⦄ → Data (Dual A)
    Monoid[Dual[A]]    : ⦃ Monoid A ⦄ → Monoid (Dual A)
    Semigroup[Dual[A]] : ⦃ Semigroup A ⦄ → Semigroup (Dual A)
    Bounded[Dual[A]]   : ⦃ Bounded A ⦄ → Bounded (Dual A)
    Read[Dual[A]]      : ⦃ Read A ⦄ → Read (Dual A)
    Show[Dual[A]]      : ⦃ Show A ⦄ → Show (Dual A)
    Eq[Dual[A]]        : ⦃ Eq A ⦄ → Eq (Dual A)
    Ord[Dual[A]]       : ⦃ Ord A ⦄ → Ord (Dual A)

    Monoid[Endo[A]]    : Monoid (Endo A)
    Semigroup[Endo[A]] : Semigroup (Endo A)

    Data[All]      : Data All
    Monoid[All]    : Monoid All
    Semigroup[All] : Semigroup All
    Bounded[All]   : Bounded All
    Read[All]      : Read All
    Show[All]      : Show All
    Eq[All]        : Eq All
    Ord[All]       : Ord All

    Data[Any]      : Data Any
    Monoid[Any]    : Monoid Any
    Semigroup[Any] : Semigroup Any
    Bounded[Any]   : Bounded Any
    Read[Any]      : Read Any
    Show[Any]      : Show Any
    Eq[Any]        : Eq Any
    Ord[Any]       : Ord Any

    MonadFix[Sum]    : MonadFix {aℓ} Sum
    MonadZip[Sum]    : MonadZip {aℓ} Sum
    Foldable[Sum]    : Foldable {aℓ} Sum
    Traversable[Sum] : Traversable {aℓ} Sum
    Applicative[Sum] : Applicative {aℓ} Sum
    Functor[Sum]     : Functor {aℓ} Sum
    Monad[Sum]       : Monad {aℓ} Sum
    Data[Sum[A]]      : ⦃ Data A ⦄ → Data (Sum A)
    Monoid[Sum[A]]    : ⦃ Monoid A ⦄ → Monoid (Sum A)
    Semigroup[Sum[A]] : ⦃ Semigroup A ⦄ → Semigroup (Sum A)
    Bounded[Sum[A]]   : ⦃ Bounded A ⦄ → Bounded (Sum A)
    Read[Sum[A]]      : ⦃ Read A ⦄ → Read (Sum A)
    Show[Sum[A]]      : ⦃ Show A ⦄ → Show (Sum A)
    Eq[Sum[A]]        : ⦃ Eq A ⦄ → Eq (Sum A)
    Ord[Sum[A]]       : ⦃ Ord A ⦄ → Ord (Sum A)

    MonadFix[Product]    : MonadFix {aℓ} Product
    MonadZip[Product]    : MonadZip {aℓ} Product
    Foldable[Product]    : Foldable {aℓ} Product
    Traversable[Product] : Traversable {aℓ} Product
    Applicative[Product] : Applicative {aℓ} Product
    Functor[Product]     : Functor {aℓ} Product
    Monad[Product]       : Monad {aℓ} Product
    Data[Product[A]]      : ⦃ Data A ⦄ → Data (Product A)
    Monoid[Product[A]]    : ⦃ Monoid A ⦄ → Monoid (Product A)
    Semigroup[Product[A]] : ⦃ Semigroup A ⦄ → Semigroup (Product A)
    Bounded[Product[A]]   : ⦃ Bounded A ⦄ → Bounded (Product A)
    Read[Product[A]]      : ⦃ Read A ⦄ → Read (Product A)
    Show[Product[A]]      : ⦃ Show A ⦄ → Show (Product A)
    Eq[Product[A]]        : ⦃ Eq A ⦄ → Eq (Product A)
    Ord[Product[A]]       : ⦃ Ord A ⦄ → Ord (Product A)

    MonadFix[First]    : MonadFix {aℓ} First
    MonadZip[First]    : MonadZip {aℓ} First
    Foldable[First]    : Foldable {aℓ} First
    Traversable[First] : Traversable {aℓ} First
    Applicative[First] : Applicative {aℓ} First
    Functor[First]     : Functor {aℓ} First
    Monad[First]       : Monad {aℓ} First
    Data[First[A]]      : ⦃ Data A ⦄ → Data (First A)
    Monoid[First[A]]    : ⦃ Monoid A ⦄ → Monoid (First A)
    Semigroup[First[A]] : ⦃ Semigroup A ⦄ → Semigroup (First A)
    Bounded[First[A]]   : ⦃ Bounded A ⦄ → Bounded (First A)
    Read[First[A]]      : ⦃ Read A ⦄ → Read (First A)
    Show[First[A]]      : ⦃ Show A ⦄ → Show (First A)
    Eq[First[A]]        : ⦃ Eq A ⦄ → Eq (First A)
    Ord[First[A]]       : ⦃ Ord A ⦄ → Ord (First A)

    MonadFix[Last]    : MonadFix {aℓ} Last
    MonadZip[Last]    : MonadZip {aℓ} Last
    Foldable[Last]    : Foldable {aℓ} Last
    Traversable[Last] : Traversable {aℓ} Last
    Applicative[Last] : Applicative {aℓ} Last
    Functor[Last]     : Functor {aℓ} Last
    Monad[Last]       : Monad {aℓ} Last
    Data[Last[A]]      : ⦃ Data A ⦄ → Data (Last A)
    Monoid[Last[A]]    : ⦃ Monoid A ⦄ → Monoid (Last A)
    Semigroup[Last[A]] : ⦃ Semigroup A ⦄ → Semigroup (Last A)
    Bounded[Last[A]]   : ⦃ Bounded A ⦄ → Bounded (Last A)
    Read[Last[A]]      : ⦃ Read A ⦄ → Read (Last A)
    Show[Last[A]]      : ⦃ Show A ⦄ → Show (Last A)
    Eq[Last[A]]        : ⦃ Eq A ⦄ → Eq (Last A)
    Ord[Last[A]]       : ⦃ Ord A ⦄ → Ord (Last A)

    MonadFix[Alt[F]]      : ⦃ MonadFix F ⦄ → MonadFix (Alt F)
    MonadZip[Alt[F]]      : ⦃ MonadZip F ⦄ → MonadZip (Alt F)
    Foldable[Alt[F]]      : ⦃ Foldable F ⦄ → Foldable (Alt F)
    Contravariant[Alt[F]] : ⦃ Contravariant F ⦄ → Contravariant (Alt F)
    Traversable[Alt[F]]   : ⦃ Traversable F ⦄ → Traversable (Alt F)
    Alternative[Alt[F]]   : ⦃ Alternative F ⦄ → Alternative (Alt F)
    Applicative[Alt[F]]   : ⦃ Applicative F ⦄ → Applicative (Alt F)
    Functor[Alt[F]]       : ⦃ Functor F ⦄ → Functor (Alt F)
    Monad[Alt[F]]         : ⦃ Monad F ⦄ → Monad (Alt F)
    MonadPlus[Alt[F]]     : ⦃ MonadPlus F ⦄ → MonadPlus (Alt F)
    Data[Alt[F,A]]        : ⦃ Data (F A) ⦄ → ⦃ Data A ⦄ → ⦃ Typeable F ⦄ → Data (Alt F A)
    Monoid[Alt[F,A]]      : ⦃ Alternative F ⦄ → Monoid (Alt F A)
    Semigroup[Alt[F,A]]   : ⦃ Alternative F ⦄ → Semigroup (Alt F A)
    Enum[Alt[F,A]]        : ⦃ Enum (F A) ⦄ → Enum (Alt F A)
    Num[Alt[F,A]]         : ⦃ Num (F A) ⦄ → Num (Alt F A)
    Read[Alt[F,A]]        : ⦃ Read (F A) ⦄ → Read (Alt F A)
    Show[Alt[F,A]]        : ⦃ Show (F A) ⦄ → Show (Alt F A)
    Eq[Alt[F,A]]          : ⦃ Eq (F A) ⦄ → Eq (Alt F A)
    Ord[Alt[F,A]]         : ⦃ Ord (F A) ⦄ → Ord (Alt F A)

    MonadFail[Ap[F]]   : ⦃ MonadFail F ⦄ → MonadFail (Ap F)
    MonadFix[Ap[F]]    : ⦃ MonadFix F ⦄ → MonadFix (Ap F)
    MonadZip[Ap[F]]    : ⦃ MonadZip F ⦄ → MonadZip (Ap F)
    Foldable[Ap[F]]    : ⦃ Foldable F ⦄ → Foldable (Ap F)
    Traversable[Ap[F]] : ⦃ Traversable F ⦄ → Traversable (Ap F)
    Alternative[Ap[F]] : ⦃ Alternative F ⦄ → Alternative (Ap F)
    Applicative[Ap[F]] : ⦃ Applicative F ⦄ → Applicative (Ap F)
    Functor[Ap[F]]     : ⦃ Functor F ⦄ → Functor (Ap F)
    Monad[Ap[F]]       : ⦃ Monad F ⦄ → Monad (Ap F)
    MonadPlus[Ap[F]]   : ⦃ MonadPlus F ⦄ → MonadPlus (Ap F)
    Data[Ap[F,A]]      : ⦃ Data (F A) ⦄ → ⦃ Data A ⦄ → ⦃ Typeable F ⦄ → Data (Ap F A)
    Monoid[Ap[F,A]]    : ⦃ Applicative F ⦄ → ⦃ Monoid A ⦄ → Monoid (Ap F A)
    Semigroup[Ap[F,A]] : ⦃ Applicative F ⦄ → ⦃ Semigroup A ⦄ → Semigroup (Ap F A)
    Bounded[Ap[F,A]    : ⦃ Applicative F ⦄ → ⦃ Bounded A ⦄ → Bounded (Ap F A)
    Enum[Ap[F,A]]      : ⦃ Enum (F A) ⦄ → Enum (Ap F A)
    Num[Ap[F,A]]       : ⦃ Applicative F ⦄ → ⦃ Num A ⦄ → Num (Ap F A)
    Read[Ap[F,A]]      : ⦃ Read (F A) ⦄ → Read (Ap F A)
    Show[Ap[F,A]]      : ⦃ Show (F A) ⦄ → Show (Ap F A)
    Eq[Ap[F,A]]        : ⦃ Eq (F A) ⦄ → Eq (Ap F A)
    Ord[Ap[F,A]]       : ⦃ Ord (F A) ⦄ → Ord (Ap F A)

{-# COMPILE GHC MonadFix[Dual]     = \ aℓ -> AgdaMonadFix    #-}
{-# COMPILE GHC MonadZip[Dual]     = \ aℓ -> AgdaMonadZip    #-}
{-# COMPILE GHC Foldable[Dual]     = \ aℓ -> AgdaFoldable    #-}
{-# COMPILE GHC Traversable[Dual]  = \ aℓ -> AgdaTraversable #-}
{-# COMPILE GHC Applicative[Dual]  = \ aℓ -> AgdaApplicative #-}
{-# COMPILE GHC Functor[Dual]      = \ aℓ -> AgdaFunctor     #-}
{-# COMPILE GHC Monad[Dual]        = \ aℓ -> AgdaMonad       #-}
{-# COMPILE GHC Data[Dual[A]]      = \ aℓ a AgdaData      -> AgdaData      #-}
{-# COMPILE GHC Monoid[Dual[A]]    = \ aℓ a AgdaMonoid    -> AgdaMonoid    #-}
{-# COMPILE GHC Semigroup[Dual[A]] = \ aℓ a AgdaSemigroup -> AgdaSemigroup #-}
{-# COMPILE GHC Bounded[Dual[A]]   = \ aℓ a AgdaBounded   -> AgdaBounded   #-}
{-# COMPILE GHC Read[Dual[A]]      = \ aℓ a AgdaRead      -> AgdaRead      #-}
{-# COMPILE GHC Show[Dual[A]]      = \ aℓ a AgdaShow      -> AgdaShow      #-}
{-# COMPILE GHC Eq[Dual[A]]        = \ aℓ a AgdaEq        -> AgdaEq        #-}
{-# COMPILE GHC Ord[Dual[A]]       = \ aℓ a AgdaOrd       -> AgdaOrd       #-}

{-# COMPILE GHC Monoid[Endo[A]]    = \ aℓ a -> AgdaMonoid    #-}
{-# COMPILE GHC Semigroup[Endo[A]] = \ aℓ a -> AgdaSemigroup #-}

{-# COMPILE GHC Data[All]      = AgdaData      #-}
{-# COMPILE GHC Monoid[All]    = AgdaMonoid    #-}
{-# COMPILE GHC Semigroup[All] = AgdaSemigroup #-}
{-# COMPILE GHC Bounded[All]   = AgdaBounded   #-}
{-# COMPILE GHC Read[All]      = AgdaRead      #-}
{-# COMPILE GHC Show[All]      = AgdaShow      #-}
{-# COMPILE GHC Eq[All]        = AgdaEq        #-}
{-# COMPILE GHC Ord[All]       = AgdaOrd       #-}

{-# COMPILE GHC Data[Any]      = AgdaData      #-}
{-# COMPILE GHC Monoid[Any]    = AgdaMonoid    #-}
{-# COMPILE GHC Semigroup[Any] = AgdaSemigroup #-}
{-# COMPILE GHC Bounded[Any]   = AgdaBounded   #-}
{-# COMPILE GHC Read[Any]      = AgdaRead      #-}
{-# COMPILE GHC Show[Any]      = AgdaShow      #-}
{-# COMPILE GHC Eq[Any]        = AgdaEq        #-}
{-# COMPILE GHC Ord[Any]       = AgdaOrd       #-}

{-# COMPILE GHC MonadFix[Sum]     = \ aℓ -> AgdaMonadFix    #-}
{-# COMPILE GHC MonadZip[Sum]     = \ aℓ -> AgdaMonadZip    #-}
{-# COMPILE GHC Foldable[Sum]     = \ aℓ -> AgdaFoldable    #-}
{-# COMPILE GHC Traversable[Sum]  = \ aℓ -> AgdaTraversable #-}
{-# COMPILE GHC Applicative[Sum]  = \ aℓ -> AgdaApplicative #-}
{-# COMPILE GHC Functor[Sum]      = \ aℓ -> AgdaFunctor     #-}
{-# COMPILE GHC Monad[Sum]        = \ aℓ -> AgdaMonad       #-}
{-# COMPILE GHC Data[Sum[A]]      = \ aℓ a AgdaData      -> AgdaData      #-}
{-# COMPILE GHC Monoid[Sum[A]]    = \ aℓ a AgdaMonoid    -> AgdaMonoid    #-}
{-# COMPILE GHC Semigroup[Sum[A]] = \ aℓ a AgdaSemigroup -> AgdaSemigroup #-}
{-# COMPILE GHC Bounded[Sum[A]]   = \ aℓ a AgdaBounded   -> AgdaBounded   #-}
{-# COMPILE GHC Read[Sum[A]]      = \ aℓ a AgdaRead      -> AgdaRead      #-}
{-# COMPILE GHC Show[Sum[A]]      = \ aℓ a AgdaShow      -> AgdaShow      #-}
{-# COMPILE GHC Eq[Sum[A]]        = \ aℓ a AgdaEq        -> AgdaEq        #-}
{-# COMPILE GHC Ord[Sum[A]]       = \ aℓ a AgdaOrd       -> AgdaOrd       #-}

{-# COMPILE GHC MonadFix[Product]     = \ aℓ -> AgdaMonadFix    #-}
{-# COMPILE GHC MonadZip[Product]     = \ aℓ -> AgdaMonadZip    #-}
{-# COMPILE GHC Foldable[Product]     = \ aℓ -> AgdaFoldable    #-}
{-# COMPILE GHC Traversable[Product]  = \ aℓ -> AgdaTraversable #-}
{-# COMPILE GHC Applicative[Product]  = \ aℓ -> AgdaApplicative #-}
{-# COMPILE GHC Functor[Product]      = \ aℓ -> AgdaFunctor     #-}
{-# COMPILE GHC Monad[Product]        = \ aℓ -> AgdaMonad       #-}
{-# COMPILE GHC Data[Product[A]]      = \ aℓ a AgdaData      -> AgdaData      #-}
{-# COMPILE GHC Monoid[Product[A]]    = \ aℓ a AgdaMonoid    -> AgdaMonoid    #-}
{-# COMPILE GHC Semigroup[Product[A]] = \ aℓ a AgdaSemigroup -> AgdaSemigroup #-}
{-# COMPILE GHC Bounded[Product[A]]   = \ aℓ a AgdaBounded   -> AgdaBounded   #-}
{-# COMPILE GHC Read[Product[A]]      = \ aℓ a AgdaRead      -> AgdaRead      #-}
{-# COMPILE GHC Show[Product[A]]      = \ aℓ a AgdaShow      -> AgdaShow      #-}
{-# COMPILE GHC Eq[Product[A]]        = \ aℓ a AgdaEq        -> AgdaEq        #-}
{-# COMPILE GHC Ord[Product[A]]       = \ aℓ a AgdaOrd       -> AgdaOrd       #-}

{-# COMPILE GHC MonadFix[First]     = \ aℓ -> AgdaMonadFix    #-}
{-# COMPILE GHC MonadZip[First]     = \ aℓ -> AgdaMonadZip    #-}
{-# COMPILE GHC Foldable[First]     = \ aℓ -> AgdaFoldable    #-}
{-# COMPILE GHC Traversable[First]  = \ aℓ -> AgdaTraversable #-}
{-# COMPILE GHC Applicative[First]  = \ aℓ -> AgdaApplicative #-}
{-# COMPILE GHC Functor[First]      = \ aℓ -> AgdaFunctor     #-}
{-# COMPILE GHC Monad[First]        = \ aℓ -> AgdaMonad       #-}
{-# COMPILE GHC Data[First[A]]      = \ aℓ a AgdaData      -> AgdaData      #-}
{-# COMPILE GHC Monoid[First[A]]    = \ aℓ a AgdaMonoid    -> AgdaMonoid    #-}
{-# COMPILE GHC Semigroup[First[A]] = \ aℓ a AgdaSemigroup -> AgdaSemigroup #-}
{-# COMPILE GHC Bounded[First[A]]   = \ aℓ a AgdaBounded   -> AgdaBounded   #-}
{-# COMPILE GHC Read[First[A]]      = \ aℓ a AgdaRead      -> AgdaRead      #-}
{-# COMPILE GHC Show[First[A]]      = \ aℓ a AgdaShow      -> AgdaShow      #-}
{-# COMPILE GHC Eq[First[A]]        = \ aℓ a AgdaEq        -> AgdaEq        #-}
{-# COMPILE GHC Ord[First[A]]       = \ aℓ a AgdaOrd       -> AgdaOrd       #-}

{-# COMPILE GHC MonadFix[Last]     = \ aℓ -> AgdaMonadFix    #-}
{-# COMPILE GHC MonadZip[Last]     = \ aℓ -> AgdaMonadZip    #-}
{-# COMPILE GHC Foldable[Last]     = \ aℓ -> AgdaFoldable    #-}
{-# COMPILE GHC Traversable[Last]  = \ aℓ -> AgdaTraversable #-}
{-# COMPILE GHC Applicative[Last]  = \ aℓ -> AgdaApplicative #-}
{-# COMPILE GHC Functor[Last]      = \ aℓ -> AgdaFunctor     #-}
{-# COMPILE GHC Monad[Last]        = \ aℓ -> AgdaMonad       #-}
{-# COMPILE GHC Data[Last[A]]      = \ aℓ a AgdaData      -> AgdaData      #-}
{-# COMPILE GHC Monoid[Last[A]]    = \ aℓ a AgdaMonoid    -> AgdaMonoid    #-}
{-# COMPILE GHC Semigroup[Last[A]] = \ aℓ a AgdaSemigroup -> AgdaSemigroup #-}
{-# COMPILE GHC Bounded[Last[A]]   = \ aℓ a AgdaBounded   -> AgdaBounded   #-}
{-# COMPILE GHC Read[Last[A]]      = \ aℓ a AgdaRead      -> AgdaRead      #-}
{-# COMPILE GHC Show[Last[A]]      = \ aℓ a AgdaShow      -> AgdaShow      #-}
{-# COMPILE GHC Eq[Last[A]]        = \ aℓ a AgdaEq        -> AgdaEq        #-}
{-# COMPILE GHC Ord[Last[A]]       = \ aℓ a AgdaOrd       -> AgdaOrd       #-}

{-# COMPILE GHC MonadFix[Alt[F]]      = \ fℓ f AgdaMonadFix      -> AgdaMonadFix      #-}
{-# COMPILE GHC MonadZip[Alt[F]]      = \ fℓ f AgdaMonadZip      -> AgdaMonadZip      #-}
{-# COMPILE GHC Foldable[Alt[F]]      = \ fℓ f AgdaFoldable      -> AgdaFoldable      #-}
{-# COMPILE GHC Contravariant[Alt[F]] = \ fℓ f AgdaContravariant -> AgdaContravariant #-}
{-# COMPILE GHC Traversable[Alt[F]]   = \ fℓ f AgdaTraversable   -> AgdaTraversable   #-}
{-# COMPILE GHC Alternative[Alt[F]]   = \ fℓ f AgdaAlternative   -> AgdaAlternative   #-}
{-# COMPILE GHC Applicative[Alt[F]]   = \ fℓ f AgdaApplicative   -> AgdaApplicative   #-}
{-# COMPILE GHC Functor[Alt[F]]       = \ fℓ f AgdaFunctor       -> AgdaFunctor       #-}
{-# COMPILE GHC Monad[Alt[F]]         = \ fℓ f AgdaMonad         -> AgdaMonad         #-}
{-# COMPILE GHC MonadPlus[Alt[F]]     = \ fℓ f AgdaMonadPlus     -> AgdaMonadPlus     #-}
{-# COMPILE GHC Monoid[Alt[F,A]]      = \ fℓ f a AgdaAlternative -> AgdaMonoid    #-}
{-# COMPILE GHC Semigroup[Alt[F,A]]   = \ fℓ f a AgdaAlternative -> AgdaSemigroup #-}
{-# COMPILE GHC Enum[Alt[F,A]]        = \ fℓ f a AgdaEnum        -> AgdaEnum      #-}
{-# COMPILE GHC Num[Alt[F,A]]         = \ fℓ f a AgdaNum         -> AgdaNum       #-}
{-# COMPILE GHC Read[Alt[F,A]]        = \ fℓ f a AgdaRead        -> AgdaRead      #-}
{-# COMPILE GHC Show[Alt[F,A]]        = \ fℓ f a AgdaShow        -> AgdaShow      #-}
{-# COMPILE GHC Eq[Alt[F,A]]          = \ fℓ f a AgdaEq          -> AgdaEq        #-}
{-# COMPILE GHC Ord[Alt[F,A]]         = \ fℓ f a AgdaOrd         -> AgdaOrd       #-}
{-# COMPILE GHC Data[Alt[F,A]]        = \ fℓ f a AgdaData AgdaData AgdaTypeable -> AgdaData #-}

{-# COMPILE GHC MonadFail[Ap[F]]   = \ fℓ f AgdaMonadFail   -> AgdaMonadFail   #-}
{-# COMPILE GHC MonadFix[Ap[F]]    = \ fℓ f AgdaMonadFix    -> AgdaMonadFix    #-}
{-# COMPILE GHC MonadZip[Ap[F]]    = \ fℓ f AgdaMonadZip    -> AgdaMonadZip    #-}
{-# COMPILE GHC Foldable[Ap[F]]    = \ fℓ f AgdaFoldable    -> AgdaFoldable    #-}
{-# COMPILE GHC Traversable[Ap[F]] = \ fℓ f AgdaTraversable -> AgdaTraversable #-}
{-# COMPILE GHC Alternative[Ap[F]] = \ fℓ f AgdaAlternative -> AgdaAlternative #-}
{-# COMPILE GHC Applicative[Ap[F]] = \ fℓ f AgdaApplicative -> AgdaApplicative #-}
{-# COMPILE GHC Functor[Ap[F]]     = \ fℓ f AgdaFunctor     -> AgdaFunctor     #-}
{-# COMPILE GHC Monad[Ap[F]]       = \ fℓ f AgdaMonad       -> AgdaMonad       #-}
{-# COMPILE GHC MonadPlus[Ap[F]]   = \ fℓ f AgdaMonadPlus   -> AgdaMonadPlus   #-}
{-# COMPILE GHC Data[Ap[F,A]]      = \ fℓ f a AgdaData AgdaData AgdaTypeable -> AgdaData      #-}
{-# COMPILE GHC Monoid[Ap[F,A]]    = \ fℓ f a AgdaApplicative AgdaMonoid     -> AgdaMonoid    #-}
{-# COMPILE GHC Semigroup[Ap[F,A]] = \ fℓ f a AgdaApplicative AgdaSemigroup  -> AgdaSemigroup #-}
{-# COMPILE GHC Bounded[Ap[F,A]    = \ fℓ f a AgdaApplicative AgdaBounded    -> AgdaBounded   #-}
{-# COMPILE GHC Enum[Ap[F,A]]      = \ fℓ f a AgdaEnum                       -> AgdaEnum      #-}
{-# COMPILE GHC Num[Ap[F,A]]       = \ fℓ f a AgdaApplicative AgdaNum        -> AgdaNum       #-}
{-# COMPILE GHC Read[Ap[F,A]]      = \ fℓ f a AgdaRead                       -> AgdaRead      #-}
{-# COMPILE GHC Show[Ap[F,A]]      = \ fℓ f a AgdaShow                       -> AgdaShow      #-}
{-# COMPILE GHC Eq[Ap[F,A]]        = \ fℓ f a AgdaEq                         -> AgdaEq        #-}
{-# COMPILE GHC Ord[Ap[F,A]]       = \ fℓ f a AgdaOrd                        -> AgdaOrd       #-}
