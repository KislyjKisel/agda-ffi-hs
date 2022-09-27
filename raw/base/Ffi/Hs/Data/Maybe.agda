{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Maybe where

open import Agda.Builtin.Bool  using (Bool)
open import Agda.Builtin.List  using (List)
open import Agda.Primitive
open import Ffi.Hs.-base.Class
open import Ffi.Hs.GHC.Stack   using (HasCallStack)

open import Agda.Builtin.Maybe public
    using (Maybe)
    renaming (just to Just; nothing to Nothing)

{-# FOREIGN GHC
import qualified Data.Maybe
import MAlonzo.Code.Ffi.Hs.GHC.Stack (AgdaHasCallStack(AgdaHasCallStack))
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ bℓ : Level
        A : Set aℓ
        B : Set bℓ

postulate
    maybe       : B → (A → B) → Maybe A → B
    isJust      : Maybe A → Bool
    isNothing   : Maybe A → Bool
    fromMaybe   : A → Maybe A → A
    listToMaybe : List A → Maybe A
    maybeToList : Maybe A → List A
    catMaybes   : List (Maybe A) → List A
    mapMaybe    : (A → Maybe B) → List A → List B
    fromJust    : ⦃ HasCallStack ⦄ → Maybe A → A

{-# COMPILE GHC maybe       = \ aℓ bℓ a b → Data.Maybe.maybe       #-}
{-# COMPILE GHC isJust      = \ aℓ a      → Data.Maybe.isJust      #-}
{-# COMPILE GHC isNothing   = \ aℓ a      → Data.Maybe.isNothing   #-}
{-# COMPILE GHC fromMaybe   = \ aℓ a      → Data.Maybe.fromMaybe   #-}
{-# COMPILE GHC listToMaybe = \ aℓ a      → Data.Maybe.listToMaybe #-}
{-# COMPILE GHC maybeToList = \ aℓ a      → Data.Maybe.maybeToList #-}
{-# COMPILE GHC catMaybes   = \ aℓ a      → Data.Maybe.catMaybes   #-}
{-# COMPILE GHC mapMaybe    = \ aℓ bℓ a b → Data.Maybe.mapMaybe    #-}
{-# COMPILE GHC fromJust =
    \ aℓ a AgdaHasCallStack → Data.Maybe.fromJust #-}

postulate
    MonadFail[Maybe]   : MonadFail {aℓ} Maybe
    MonadFix[Maybe]    : MonadFix {aℓ} Maybe
    MonadZip[Maybe]    : MonadZip {aℓ} Maybe
    Foldable[Maybe]    : Foldable {aℓ} Maybe
    Traversable[Maybe] : Traversable {aℓ} Maybe
    Alternative[Maybe] : Alternative {aℓ} Maybe
    Applicative[Maybe] : Applicative {aℓ} Maybe
    Functor[Maybe]     : Functor {aℓ} Maybe
    Monad[Maybe]       : Monad {aℓ} Maybe
    MonadPlus[Maybe]   : MonadPlus {aℓ} Maybe
    Data[Maybe[A]]      : ⦃ Data A ⦄ → Data (Maybe A)
    Monoid[Maybe[A]]    : ⦃ Semigroup A ⦄ → Monoid (Maybe A)
    Semigroup[Maybe[A]] : ⦃ Semigroup A ⦄ → Semigroup (Maybe A)
    Read[Maybe[A]]      : ⦃ Read A ⦄ → Read (Maybe A)
    Show[Maybe[A]]      : ⦃ Show A ⦄ → Show (Maybe A)
    Eq[Maybe[A]]        : ⦃ Eq A ⦄ → Eq (Maybe A)
    Ord[Maybe[A]]       : ⦃ Ord A ⦄ → Ord (Maybe A)
    Eq1[Maybe]          : Eq1 {aℓ} Maybe
    Ord1[Maybe]         : Ord1 {aℓ} Maybe
    Read1[Maybe]        : Read1 {aℓ} Maybe
    Show1[Maybe]        : Show1 {aℓ} Maybe

{-# COMPILE GHC MonadFail[Maybe]    = \ aℓ a               -> AgdaMonadFail   #-}
{-# COMPILE GHC MonadFix[Maybe]     = \ aℓ a               -> AgdaMonadFix    #-}
{-# COMPILE GHC MonadZip[Maybe]     = \ aℓ a               -> AgdaMonadZip    #-}
{-# COMPILE GHC Foldable[Maybe]     = \ aℓ a               -> AgdaFoldable    #-}
{-# COMPILE GHC Traversable[Maybe]  = \ aℓ a               -> AgdaTraversable #-}
{-# COMPILE GHC Alternative[Maybe]  = \ aℓ a               -> AgdaAlternative #-}
{-# COMPILE GHC Applicative[Maybe]  = \ aℓ a               -> AgdaApplicative #-}
{-# COMPILE GHC Functor[Maybe]      = \ aℓ a               -> AgdaFunctor     #-}
{-# COMPILE GHC Monad[Maybe]        = \ aℓ a               -> AgdaMonad       #-}
{-# COMPILE GHC MonadPlus[Maybe]    = \ aℓ a               -> AgdaMonadPlus   #-}
{-# COMPILE GHC Data[Maybe[A]]      = \ aℓ a AgdaData      -> AgdaData        #-}
{-# COMPILE GHC Monoid[Maybe[A]]    = \ aℓ a AgdaSemigroup -> AgdaMonoid      #-}
{-# COMPILE GHC Semigroup[Maybe[A]] = \ aℓ a AgdaSemigroup -> AgdaSemigroup   #-}
{-# COMPILE GHC Read[Maybe[A]]      = \ aℓ a AgdaRead      -> AgdaRead        #-}
{-# COMPILE GHC Show[Maybe[A]]      = \ aℓ a AgdaShow      -> AgdaShow        #-}
{-# COMPILE GHC Eq[Maybe[A]]        = \ aℓ a AgdaEq        -> AgdaEq          #-}
{-# COMPILE GHC Ord[Maybe[A]]       = \ aℓ a AgdaOrd       -> AgdaOrd         #-}
{-# COMPILE GHC Eq1[Maybe]          = \ aℓ                 -> AgdaEq1         #-}
{-# COMPILE GHC Ord1[Maybe]         = \ aℓ                 -> AgdaOrd1        #-}
{-# COMPILE GHC Read1[Maybe]        = \ aℓ                 -> AgdaRead1       #-}
{-# COMPILE GHC Show1[Maybe]        = \ aℓ                 -> AgdaShow1       #-}
