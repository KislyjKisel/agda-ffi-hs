{-# OPTIONS --without-K #-}

module Ffi.Hs.Data.Type.Bool where

open import Agda.Builtin.Bool using (Bool)
open import Ffi.Hs.-base.Kind using (IsKind)
open import Ffi.Hs.Data.Bool using (`Bool; `True; `False)
open import Agda.Primitive

{-# FOREIGN GHC
import qualified Data.Type.Bool
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Kind (AgdaIsKind)
#-}

private
    variable
        aℓ : Level

infixr 3 _&&_
infixr 2 _||_

postulate
    If   : {K : Set (lsuc aℓ)} → ⦃ IsKind K ⦄ → `Bool → K → K → K
    _&&_ : `Bool → `Bool → `Bool
    _||_ : `Bool → `Bool → `Bool
    Not  : `Bool → `Bool

{-# FOREIGN GHC type AgdaIf aℓ k isk cond tru fls = Data.Type.Bool.If cond tru fls #-}
{-# COMPILE GHC If = type(6) AgdaIf #-}

{-# COMPILE GHC _&&_ = type (Data.Type.Bool.&&) #-}
{-# COMPILE GHC _||_ = type (Data.Type.Bool.||) #-}
{-# COMPILE GHC Not  = type Data.Type.Bool.Not  #-}
