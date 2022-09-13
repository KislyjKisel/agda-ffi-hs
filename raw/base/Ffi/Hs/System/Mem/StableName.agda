{-# OPTIONS --without-K #-}

module Ffi.Hs.System.Mem.StableName where

open import Agda.Builtin.Bool  using (Bool)
open import Agda.Builtin.IO    using (IO)
open import Agda.Primitive
open import Ffi.Hs.-base.Class using (Eq)
open import Ffi.Hs.Data.Int    using (Int)

{-# FOREIGN GHC
import qualified System.Mem.StableName
import MAlonzo.Code.Ffi.Hs.QZ45Zbase.Dictionaries
#-}

private
    variable
        aℓ : Level
        A B : Set aℓ

postulate
    StableName        : Set aℓ → Set aℓ
    Eq[StableName[A]] : Eq (StableName A)
    makeStableName    : A → IO (StableName A)
    hashStableName    : StableName A → Int
    eqStableName      : StableName A → StableName B → Bool

{-# FOREIGN GHC type AgdaStableName aℓ = System.Mem.StableName.StableName #-}
{-# COMPILE GHC StableName = type(1) AgdaStableName #-}

{-# COMPILE GHC Eq[StableName[A]] = \ aℓ a      -> AgdaEq                               #-}
{-# COMPILE GHC makeStableName    = \ aℓ a      -> System.Mem.StableName.makeStableName #-}
{-# COMPILE GHC hashStableName    = \ aℓ a      -> System.Mem.StableName.hashStableName #-}
{-# COMPILE GHC eqStableName      = \ aℓ a bℓ b -> System.Mem.StableName.eqStableName   #-}
