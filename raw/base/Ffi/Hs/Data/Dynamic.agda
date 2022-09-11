{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Dynamic where

open import Agda.Builtin.Maybe     using (Maybe)
open import Agda.Primitive
open import Ffi.Hs.-base.Class     using (Exception; Show)
open import Ffi.Hs.Type.Reflection using (TypeRep; SomeTypeRep)

open Ffi.Hs.Type.Reflection public
    using (Typeable)

{-# FOREIGN GHC
import qualified Data.Dynamic
import MAlonzo.Code.Ffi.Hs.Type.Reflection (AgdaTypeRep)
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Class (AgdaException, AgdaShow)
#-}

private
    variable
        aℓ : Level
        A : Set aℓ

postulate
    Dynamic : Set
    toDyn       : ⦃ TypeRep A ⦄ → A → Dynamic
    fromDyn     : ⦃ TypeRep A ⦄ → Dynamic → A → A
    fromDynamic : ⦃ TypeRep A ⦄ → Dynamic → Maybe A

    dynApply   : Dynamic → Dynamic → Maybe Dynamic
    dynApp     : Dynamic → Dynamic → Dynamic
    dynTypeRep : Dynamic → SomeTypeRep {kℓ = lzero} -- todo: Dynamic's type rep 0ℓ ?

    Exception[Dynamic] : Exception Dynamic
    Show[Dynamic]      : Show Dynamic

{-# COMPILE GHC Dynamic = type Data.Dynamic.Dynamic #-}
{-# COMPILE GHC toDyn       = \ aℓ a AgdaTypeRep -> Data.Dynamic.toDyn       #-}
{-# COMPILE GHC fromDyn     = \ aℓ a AgdaTypeRep -> Data.Dynamic.fromDyn     #-}
{-# COMPILE GHC fromDynamic = \ aℓ a AgdaTypeRep -> Data.Dynamic.fromDynamic #-}

{-# COMPILE GHC dynApply   = Data.Dynamic.dynApply   #-}
{-# COMPILE GHC dynApp     = Data.Dynamic.dynApp     #-}
{-# COMPILE GHC dynTypeRep = Data.Dynamic.dynTypeRep #-}

{-# COMPILE GHC Exception[Dynamic] = AgdaException #-}
{-# COMPILE GHC Show[Dynamic]      = AgdaShow      #-}
