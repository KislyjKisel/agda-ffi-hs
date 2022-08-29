{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Kind where

open import Ffi.Hs.-base.Kind using (IsKind)

open import Ffi.Hs.GHC.Exts public
    using    ()
    renaming (LiftedType to Type)

postulate
    Constraint : Set
    IsKind[Constraint] : IsKind Constraint

{-# FOREIGN GHC import MAlonzo.Code.QZ45Zbase.Kind (AgdaIsKind(AgdaIsKind)) #-}
{-# FOREIGN GHC import qualified Data.Kind #-}
{-# COMPILE GHC Constraint = type(0) Data.Kind.Constraint #-}
{-# COMPILE GHC IsKind[Constraint] = AgdaIsKind #-}
