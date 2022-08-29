{-# OPTIONS --without-K                                #-}

module Ffi.Hs.Data.Maybe where

open import Agda.Builtin.Bool using (Bool)
open import Agda.Builtin.List using (List)
open import Agda.Primitive

open import Agda.Builtin.Maybe public
    using    (Maybe; just; nothing)

private
    variable
        aℓ bℓ : Level
        A : Set aℓ
        B : Set bℓ

postulate
    maybe       : B → (A → B) → Maybe A → B
    isJust      : Maybe A → Bool
    isNothing   : Maybe A → Bool
    fromJust    : Maybe A → A -- todo: `HasCallStack` ?
    fromMaybe   : A → Maybe A → A
    listToMaybe : List A → Maybe A
    maybeToList : Maybe A → List A
    catMaybes   : List (Maybe A) → List A
    mapMaybe    : (A → Maybe B) → List A → List B

{-# COMPILE GHC maybe       = \ aℓ bℓ a b → Data.Maybe.maybe       #-}
{-# COMPILE GHC isJust      = \ aℓ a      → Data.Maybe.isJust      #-}
{-# COMPILE GHC isNothing   = \ aℓ a      → Data.Maybe.isNothing   #-}
{-# COMPILE GHC fromJust    = \ aℓ a      → Data.Maybe.fromJust    #-}
{-# COMPILE GHC fromMaybe   = \ aℓ a      → Data.Maybe.fromMaybe   #-}
{-# COMPILE GHC listToMaybe = \ aℓ a      → Data.Maybe.listToMaybe #-}
{-# COMPILE GHC maybeToList = \ aℓ a      → Data.Maybe.maybeToList #-}
{-# COMPILE GHC catMaybes   = \ aℓ a      → Data.Maybe.catMaybes   #-}
{-# COMPILE GHC mapMaybe    = \ aℓ bℓ a b → Data.Maybe.mapMaybe    #-}
