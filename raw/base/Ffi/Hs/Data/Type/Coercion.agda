{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Type.Coercion where

open import Agda.Builtin.Maybe        using (Maybe)
open import Agda.Primitive
open import Ffi.Hs.-base.Class
open import Ffi.Hs.Data.Type.Equality using (_:~:_; _:~~:_)

open Ffi.Hs.-base.Class public
    using (TestCoercion)

{-# FOREIGN GHC
import qualified Data.Type.Coercion
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ bℓ : Level
        A B C K : Set aℓ
        F : Set aℓ → Set bℓ

postulate
    Coercion : Set aℓ → Set aℓ → Set aℓ
    mkCoercion : ⦃ Coercible A B ⦄ → Coercion A B
    getCoercion : Coercion A B → Coercible A B

{-# FOREIGN GHC
type AgdaCoercion aℓ = Data.Type.Coercion.Coercion
#-}
{-# COMPILE GHC Coercion = type(1) AgdaCoercion #-}

{-# COMPILE GHC mkCoercion  = \ aℓ a b AgdaCoercible -> Data.Type.Coercion.Coercion  #-}
{-# COMPILE GHC getCoercion = \ aℓ a b Coercion      -> AgdaCoercible                #-}

postulate
    coerceWith   : Coercion A B → A → B
    gcoerceWith  : Coercion A B → (⦃ Coercible A B ⦄ → C) → C
    sym          : Coercion A B → Coercion B A
    trans        : Coercion A B → Coercion B C → Coercion A C
    repr         : (A :~: B) → Coercion A B
    testCoercion : ⦃ TestCoercion F ⦄ → F A → F B → Maybe (Coercion A B)

{-# COMPILE GHC coerceWith   = \ aℓ a b                       -> Data.Type.Coercion.coerceWith                      #-}
{-# COMPILE GHC gcoerceWith  = \ aℓ a b c f                   -> Data.Type.Coercion.gcoerceWith c (f AgdaCoercible) #-}
{-# COMPILE GHC sym          = \ aℓ a b                       -> Data.Type.Coercion.sym                             #-}
{-# COMPILE GHC trans        = \ aℓ a b                       -> Data.Type.Coercion.trans                           #-}
{-# COMPILE GHC repr         = \ aℓ a b                       -> Data.Type.Coercion.repr                            #-}
{-# COMPILE GHC testCoercion = \ aℓ bℓ a b f AgdaTestCoercion -> Data.Type.Coercion.testCoercion                    #-}

postulate
    Category[Coercion]        : Category {aℓ} Coercion
    Data[Coercion[A,B]]       : ⦃ Coercible A B ⦄ → ⦃ Data A ⦄ → ⦃ Data B ⦄ → Data (Coercion A B)
    Bounded[Coercion[A,B]]    : ⦃ Coercible A B ⦄ → Bounded (Coercion A B)
    Enum[Coercion[A,B]]       : ⦃ Coercible A B ⦄ → Enum (Coercion A B)
    Read[Coercion[A,B]]       : ⦃ Coercible A B ⦄ → Read (Coercion A B)
    Show[Coercion[A,B]]       : Show (Coercion A B)
    Eq[Coercion[A,B]]         : Eq (Coercion A B)
    Ord[Coercion[A,B]]        : Ord (Coercion A B)
    TestCoercion[Coercion[A]] : TestCoercion (Coercion A)
    TestCoercion[A:~:]        : TestCoercion (A :~:_)
    TestCoercion[A:~~:]       : TestCoercion {aℓ} (_:~~:_ A {K₂ = K})

{-# COMPILE GHC Category[Coercion]        = \ aℓ                                     -> AgdaCategory     #-}
{-# COMPILE GHC Data[Coercion[A,B]]       = \ aℓ a b AgdaCoercible AgdaData AgdaData -> AgdaData         #-}
{-# COMPILE GHC Bounded[Coercion[A,B]]    = \ aℓ a b AgdaCoercible                   -> AgdaBounded      #-}
{-# COMPILE GHC Enum[Coercion[A,B]]       = \ aℓ a b AgdaCoercible                   -> AgdaEnum         #-}
{-# COMPILE GHC Read[Coercion[A,B]]       = \ aℓ a b AgdaCoercible                   -> AgdaRead         #-}
{-# COMPILE GHC Show[Coercion[A,B]]       = \ aℓ a b                                 -> AgdaShow         #-}
{-# COMPILE GHC Eq[Coercion[A,B]]         = \ aℓ a b                                 -> AgdaEq           #-}
{-# COMPILE GHC Ord[Coercion[A,B]]        = \ aℓ a b                                 -> AgdaOrd          #-}
{-# COMPILE GHC TestCoercion[Coercion[A]] = \ aℓ a                                   -> AgdaTestCoercion #-}
{-# COMPILE GHC TestCoercion[A:~:]        = \ aℓ a                                   -> AgdaTestCoercion #-}
{-# COMPILE GHC TestCoercion[A:~~:]       = \ aℓ k a                                 -> AgdaTestCoercion #-}
