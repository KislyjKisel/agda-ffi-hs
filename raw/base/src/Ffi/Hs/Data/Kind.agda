{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Kind where

open import Ffi.Hs.-base.Kind using (IsKind)

open import Ffi.Hs.GHC.Exts public
    using    ()
    renaming (LiftedType to Type)

{-# FOREIGN GHC
import qualified Data.Kind
import MAlonzo.Code.QZ45Zbase.Kind (AgdaIsKind)
#-}

postulate
    Constraint : Set₁
    IsKind[Constraint] : IsKind Constraint

{-# COMPILE GHC Constraint = type(0) Data.Kind.Constraint #-}
{-# COMPILE GHC IsKind[Constraint] = AgdaIsKind #-}
